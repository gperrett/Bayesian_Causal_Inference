library(MASS)
library(ranger)
library(bartCause)
library(arm)
library(parallel)
library(tidyverse)
simulation2 <- function(n=500){
  R <- matrix(c(1, 0.5,.5,
                .5,1,.5,
                .5, .5,1), 
              nrow = 3, ncol = 3, byrow = T)
  inv.log <- function(x){exp(.8*x)/(1 + exp(.8*x))} 
  mu <- c(a = 0, b = 0, c = 0)
  norms <- MASS::mvrnorm(n, mu = mu, Sigma = R)
  norms <- as.data.frame(norms)
  z <- rbinom(n, 1, inv.log(norms$a))
  d <- rbinom(n, 1, inv.log(z))
  e <- rbinom(n, 1, (1 - inv.log(z)))
  f <- rbinom(n, 1, .5)
  g <- rbinom(n, 1, .5)
  y1 <- -15 + 2*norms$a + 10*(norms$a^2)+  6*norms$b + 2*(norms$b^2)+ -3*norms$c + d + 5*f -6*g+ 5 + rnorm(n, 0, 1)
  y0 <- -15 + .8*norms$a + 6*norms$b+ -3*norms$c + d + 5*f -6*g + + rnorm(n, 0, 1)
  
 
  
  y <- ifelse(z==1, y1, y0)
  dat <- data.frame(d,e,f,g,z, y, y1, y0)
  dat <- cbind(dat, norms)
  
  ate <- mean(dat[dat$z==1,]$y1 - dat[dat$z==1,]$y0)
  
  # Fit Lm
  model_lm <- lm(y~a+b+d+e+f+g+z, dat)
  
  # Fit IPTW
  iptw <- glm(z ~ a + b + d + e + f + g , family = "binomial", data = dat)
  pscores.iptw <- predict(iptw, type = "response")
  weights <- data.frame(pscores.iptw, z = dat$z)
  weights$wgt <- ifelse(weights$z ==1, 
                        1,
                        1/(exp(1 - pscores.iptw))) 
  
  
  # pull weights 
  dat$wgt <- weights$wgt
  
  model_iptw <-lm(y~z + a + b + d + e + f + g, weights = wgt, data = dat)
  
  # Fit RF
  rf <- ranger(z ~ a + b + d + e + f + g, data = dat,mtry = 2, classification = T,min.node.size = 10, num.trees = 100, splitrule= 'gini')
  pscores <- rf$predictions
  match <- matching(dat$z, pscores, replace = T)
  match.wgt <- match$cnts
  model_rf <- lm(y~z + a + b + d + e + f + g,weights = match.wgt, data= dat )
  # Fit BART
  X <- as.matrix(dat[,c("a", "b","d", "e", "f", "g")])
  model_bart_non_adj <- bartc(y, z, confounders = X, data = dat, estimand = 'att')
  model_bart_adj <- bartc(y, z, confounders = X, data = dat, estimand = 'att', commonSup.rule = "sd")
  bart_ate <- dat %>% filter(z == 1, model_bart_adj$commonSup.sub ==1) %>% summarise(mean(y1 -y0))
  model_bart_non_adj <- summary(model_bart_non_adj)
  model_bart_adj <-summary(model_bart_adj)
  # collect RMSE
  lm.rmse <- sqrt(((ate - model_lm$coefficients['z'])**2))
  iptw.rmse <- sqrt(((ate - model_iptw$coefficients['z'])**2))
  rf.rmse <- sqrt(((ate - model_rf$coefficients['z'])**2))
  bart.unadj <- sqrt(((ate - model_bart_non_adj$estimates[1])**2))
  names(bart.unadj) <- 'bart.unadj'
  bart.adj <- sqrt(((bart_ate - model_bart_adj$estimates[1])**2))
  names(bart.adj) <- 'bart.adj'
  rmse <- data.frame(lm.rmse, iptw.rmse, rf.rmse, bart.unadj, bart.adj)
  return(rmse)
}

study.2 <- mclapply(1:1000, function(i){simulation2()}, mc.cores = 10)

df2 <- bind_rows(study.2)

write_csv(df2, "study2.csv")

