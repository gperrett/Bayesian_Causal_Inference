library(tidyverse)
library(bartCause)
library(ranger)
library(arm)

modeler <- function(dat){
  if('X' %in% names(dat) ){
    true_ate <- mean(dat[dat$z ==1,]$y1 - dat[dat$z==1,]$y0)
    
    # linear model
    sample_lm <- summary(lm(y~ z + X, dat))
    est.z.lm <- as.vector(sample_lm$coefficients[2,1])
    est.z.lm.ub <- as.vector(est.z.lm + 1.96*sample_lm$coefficients[2,2])
    est.z.lm.lb <- as.vector(est.z.lm - 1.96*sample_lm$coefficients[2,2])
    lm.rmse <- as.vector(sqrt(((true_ate - est.z.lm)**2)))
    
    # save slope and intercept for plotting
    dat$slope = sample_lm$coefficients[3,1]
    dat$intercept = ifelse(
      dat$z == 1,
      sample_lm$coefficients[1,1] + sample_lm$coefficients[2,1],
      sample_lm$coefficients[1,1]
    )
    
    # IPTW model 
    iptw <- glm(z ~ X, family = "binomial", data = dat)
    pscores.iptw <- predict(iptw, type = "response")
    weights <- data.frame(pscores.iptw, z = dat$z)
    weights$wgt <- ifelse(weights$z ==1, 
                          1,
                          1/(exp(1 - pscores.iptw))) 
    dat$wgt <- weights$wgt
    
    sample_iptw <- summary(lm(y~z + X, weights = wgt, data = dat))
    est.z.iptw <- as.vector(sample_iptw$coefficients[2,1])
    est.z.iptw.lb <- as.vector(sample_iptw$coefficients[2,1] - (1.96*sample_iptw$coefficients[2,2]))
    est.z.iptw.ub <- as.vector(sample_iptw$coefficients[2,1] + (1.96*sample_iptw$coefficients[2,2]))
    iptw.rmse <- as.vector(sqrt(((true_ate - sample_iptw$coefficients[2,1])**2)))
    
    # Fit Prop Score
    rf <- ranger(z ~ X, data = dat, classification = T,min.node.size = 10, num.trees = 100, splitrule= 'gini')
    pscores <- rf$predictions
    match <- matching(dat$z, pscores, replace = T)
    match.wgt <- match$cnts
    dat$match <- match.wgt
    sample_rf <- summary(lm(y~z + X,weights = match.wgt, data= dat ))
    
    est.z.rf <- as.vector(sample_rf$coefficients[2,1])
    est.z.rf.ub <- as.vector(est.z.rf + 1.96*sample_rf$coefficients[2,2])
    est.z.rf.lb <- as.vector(est.z.rf - 1.96*sample_rf$coefficients[2,2])
    rf.rmse <- as.vector(sqrt(((true_ate - est.z.rf)**2)))
    
    # BART
    sample_bart <- bartc(y, z, confounders = X, data = dat, commonSup.rule = "sd",estimand = 'att', seed = 2)
    est.z.bart <- as.vector(summary(sample_bart)$estimates[1])
    est.z.bart.lb <- as.vector(summary(sample_bart)$estimates[3])
    est.z.bart.ub <- as.vector(summary(sample_bart)$estimates[4])
    bart_true_ate <- as.vector(dat %>% filter(z ==1, sample_bart$commonSup.sub == 1) %>% summarise(mean(y1 - y0)))
    bart.rmse <- as.vector(sqrt(((bart_true_ate-est.z.bart)**2)))
    
    # get ite's
    cf <- bartCause::extract(sample_bart, 'y.cf')
    cf.m <- apply(cf, 2, mean)
    cf.sd <- apply(cf, 2, sd) 
    cf.lb <- cf.m - 1.96 * cf.sd 
    cf.ub <- cf.m + 1.96 * cf.sd
    dat[dat$z==1,'cf.m'] <- cf.m
    dat[dat$z==1,'cf.lb'] <- cf.lb
    dat[dat$z==1,'cf.ub'] <- cf.ub
    
    # combine RMSE
    estimates <- tibble(rmse = rbind(lm.rmse, iptw.rmse,rf.rmse, bart.rmse), 
                            est.z = rbind(est.z.lm, est.z.iptw,est.z.rf, est.z.bart), 
                            est.lb = rbind(est.z.lm.lb, est.z.iptw.lb,est.z.rf.lb, est.z.bart.lb), 
                            est.up = rbind(est.z.lm.ub, est.z.iptw.ub,est.z.rf.ub, est.z.bart.ub), 
                            truth = rbind(true_ate, true_ate,true_ate, bart_true_ate), 
                            estimator = c('Linear Regression', "IPTW","Propensity Score Matching", 'BART'))
    names(estimates) <- c('rmse', 'estimate', 'li', 'ui', 'truth', 'estimator')
    
  }
  
  
  if('a' %in% names(dat)){
    # linear model
    sample_lm <- lm(y~ z + a + b + c + d + e + f, dat)
    est.z.lm <- sample_lm$coefficients['z']
    lm.rmse <- sqrt(((mean(dat$y1 - dat$y0) - est.z.lm)**2))
    dat$slope = sample_lm$coefficients['X']
    dat$intercept = ifelse(
      dat$z == 1,
      sample_lm$coefficients['(Intercept)'] + sample_lm$coefficients['z'],
      sample_lm$coefficients['(Intercept)']
    )
    
    # IPTW model 
    iptw <- glm(z ~ a + b + c + d + e + f , family = "binomial", data = dat)
    pscores.iptw <- predict(iptw, type = "response")
    weights <- data.frame(pscores.iptw, z = dat$z)
    weights$wgt <- ifelse(weights$z ==1, 
                          1/(exp(pscores.iptw)),
                          1/(exp(1 - pscores.iptw))) 
    dat$wgt <- weights$wgt
    
    sample_iptw <-lm(y~z + a + b + c + d + e + f, weights = wgt, data = dat)
    est.z.iptw <- sample_iptw$coefficients['z']
    iptw.rmse <- sqrt(((mean(dat$y1 - dat$y0) - sample_iptw$coefficients['z'])**2))
    
    
    # BART
    X <- as.matrix(dat[,c("a", "b","c","d", "e", "f")])
    sample_bart <- bartc(y, z, confounders = X, data = dat, commonSup.rule = "sd")
    est.z.bart <- summary(sample_bart)$estimates[1]
    bart.rmse <- sqrt(((mean(dat[sample_bart$commonSup.sub,]$y1 - dat[sample_bart$commonSup.sub,]$y0) - est.z.bart)**2))
    cf <- extract(sample_bart, 'mu.cf')
    cf.m <- apply(cf, 2, mean)
    cf.sd <- apply(cf, 2, sd) 
    cf.lb <- cf.m - 2 * cf.sd 
    cf.ub <- cf.m + 2 * cf.sd
    dat <- cbind(dat, cf.m, cf.sd, cf.lb, cf.ub)
    # combine RMSE
    est.rmse <- data.frame(lm.rmse,iptw.rmse ,bart.rmse)
    names(est.rmse) <- c("linear regression", "IPTW", "BART")
    rownames(est.rmse) <- 1:nrow(est.rmse)
    
  }
  out <- list(estimates, dat)
  return(out)
}
