library(tidyverse)
library(bartCause)

dgp <- function(n=1000, tau = 5, lin = T, com.sup = "strong", high.dim =F){
  
  z <- rbinom(n, 1, .5)
  inv.log <- function(x){exp(x)/(1 + exp(x))} 
  
  if(lin==T & com.sup == "strong")
  {
    X <- rnorm(n, 40, 10)
    y1 <- .8*X + tau + rnorm(n, 0, 1) 
    y0 <- .8*X +  rnorm(n, 0, 1) 
    y <- ifelse(z==1, y1, y0)
    dat <- data.frame( X, y1, y0, y, z)
    
  }
  
  if(lin==T & com.sup == "mod")
  {
    X0 <- rnorm(n, 20, 10)
    X0 <- ifelse(X0<0, 0, X0)
    X1 <- rnorm(n, 30, 10)
    X <- ifelse(z==1, X1, X0)
    y1 <- .8*X + tau + rnorm(n, 0, 1)
    y0 <- .8*X +  rnorm(n, 0, 1) 
    y <- ifelse(z==1, y1, y0)
    dat <- data.frame(X1, X0, X, y1, y0, y, z)
    
  }
  
  
  if(lin==T & com.sup == "weak")
  {
    X0 <- rnorm(n, 20, 10)
    X0 <- ifelse(X0<0, 0, X0)
    X1 <- rnorm(n, 40, 10)
    X <- ifelse(z==1, X1, X0)
    y1 <- .8*X + tau + rnorm(n, 0, 1)
    y0 <- .8*X +  rnorm(n, 0, 1) 
    y <- ifelse(z==1, y1, y0)
    dat <- data.frame(X1, X0, X, y1, y0, y, z)
    
  }
  
  if(lin==F & com.sup == "weak")
  {
    X0 <- rnorm(n, 20, 10)
    X0 <- ifelse(X0<0, 0, X0)
    X1 <- rnorm(n, 40, 10)
    X1 <- ifelse(X1>60, 60, X1)
    X1 <- ifelse(X1 <0, 0, X1)
    X <- ifelse(z==1, X1, X0)
    y0 <- rnorm(n, 72 + 3*sqrt(X0), 1) 
    y1 <- rnorm(n, 90 + exp((.06*X1)), 1)
    y1 <- ifelse(y1>120, 120, y1)
    y <- ifelse(z==1, y1, y0)
    dat <- data.frame(X1, X0, X, y1, y0, y, z)
    
    
  }
  
  if(lin==F & com.sup == "strong")
  {
    X0 <- rnorm(n, 30, 10)
    X0 <- ifelse(X0<0, 0, X0)
    X1 <- rnorm(n, 30, 10)
    X1 <- ifelse(X1>60, 60, X1)
    X1 <- ifelse(X1 <0, 0, X1)
    X <- ifelse(z==1, X1, X0)
    y0 <- rnorm(n, 72 + 3*sqrt(X0), 1) 
    y1 <- rnorm(n, 90 + exp((.06*X1)), 1)
    y1 <- ifelse(y1>120, 120, y1)
    y <- ifelse(z==1, y1, y0)
    dat <- data.frame(X1, X0, X, y1, y0, y, z)
  }
  
  if(lin==F & com.sup == "mod")
  {
    X0 <- rnorm(n, 25, 10)
    X0 <- ifelse(X0<0, 0, X0)
    X0 <- ifelse(X0>60, 60, X0)
    X1 <- rnorm(n, 35, 10)
    X1 <- ifelse(X1>60, 60, X1)
    X1 <- ifelse(X1<0, 0, X1)
    X <- ifelse(z==1, X1, X0)
    y0 <- rnorm(n, 72 + 3*sqrt(X0), 1)
    y1 <- rnorm(n, 90 + exp((.06*X1)), 1) 
    y1 <- ifelse(y1>120, 120, y1)
    y <- ifelse(z==1, y1, y0)
    
    dat <- data.frame(X1, X0, X, y1, y0, y, z)
    
  }
  
  if(high.dim ==T)
  {
    a.1 <- rnorm(n, 40, 10)
    a.0 <- rnorm(n, 30, 10)
    b <- runif(n,0, 2)
    c <- rnorm(n, 30, 15)
    d <- rbinom(n, 1, .5)
    e <- rbinom(n, 1, .5)
    f <- rbinom(n, 1, inv.log(e))
    z <- rbinom(n, 1, inv.log(-2 + (b^2)))
    a <- ifelse(z == 1, a.1, a.0)
    y1 <- 40 + .5*c + 5*f + 2*b + 3*(b^2) + rnorm(n, 0, 1)
    y0 <- 40  + .5*c  + 5*f + .2*b + rnorm(n, 0, 1)
    y <- ifelse(z==1, y1, y0)
    dat <- data.frame(a.1, a.0, a, b, c, d,e,f, y1, y0, y, z)
  }
  return(dat)
}


modeler <- function(dat){
  if('X' %in% names(dat) ){
    # linear model
    sample_lm <- lm(y~ z + X, dat)
    est.z.lm <- sample_lm$coefficients['z']
    lm.rmse <- sqrt(((mean(dat$y1 - dat$y0) - est.z.lm)**2))
    dat$slope = sample_lm$coefficients['X']
    dat$intercept = ifelse(
      dat$z == 1,
      sample_lm$coefficients['(Intercept)'] + sample_lm$coefficients['z'],
      sample_lm$coefficients['(Intercept)']
    )
    
    # IPTW model 
    iptw <- glm(z ~ X, family = "binomial", data = dat)
    pscores.iptw <- predict(iptw, type = "response")
    weights <- data.frame(pscores.iptw, z = dat$z)
    weights$wgt <- ifelse(weights$z ==1, 
                          1/(exp(pscores.iptw)),
                          1/(exp(1 - pscores.iptw))) 
    dat$wgt <- weights$wgt
    
    sample_iptw <-lm(y~z + X, weights = wgt, data = dat)
    iptw.rmse <- sqrt(((mean(dat$y1 - dat$y0) - sample_iptw$coefficients['z'])**2))
    
    
    
    # BART
    sample_bart <- bartc(y, z, confounders = X, data = dat, commonSup.rule = "sd")
    est.z.bart <- summary(sample_bart)$estimates[1]
    bart.rmse <- sqrt(((mean(dat[sample_bart$commonSup.sub,]$y1 - dat[sample_bart$commonSup.sub,]$y0) - est.z.bart)**2))
    
    # get ite's
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
  out <- list(est.rmse, dat)
  return(out)
}


ploter <- function(out){
  dat <- output[[2]]
  bart.plt <- ggplot(dat, aes(X, y, col = factor(z))) + 
    geom_linerange(aes(ymin = cf.lb, ymax = cf.ub)) + 
    geom_point(alpha = .5) + 
    scale_color_manual(labels = c("Control", "Treatment"), values = c('coral3','steelblue')) + 
    labs(title = "BART", subtitle = paste0("RMSE = ", round(out[[1]][3], 2))) + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "bottom")
  
  iptw.plt <- ggplot(dat, aes(X, y, col = factor(z))) + 
    geom_point(alpha = .5, aes(size = wgt)) + 
    scale_color_manual(labels = c("Control", "Treatment"), values = c('coral3','steelblue')) + 
    labs(title = "IPTW", subtitle = paste0("RMSE = ", round(out[[1]][2], 2))) + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "bottom")
  
  
  slope.t <- dat[dat$z==1,]$slope[1]
  slope.c <- dat[dat$z==0,]$slope[1]
  intercept.t <- dat[dat$z==1,]$intercept[1]
  intercept.c <- dat[dat$z==0,]$intercept[1]
  
  lm.plt <- ggplot(dat, aes(X, y, col = factor(z))) +
    geom_point(alpha = .5) +
    geom_abline(
      intercept = intercept.c,
      slope = slope.c,
      col = 'coral3'
    ) +
    geom_abline(
      intercept = intercept.t,
      slope = slope.t,
      col = 'steelblue'
    ) +
    scale_color_manual(labels = c("Control", "Treatment"), values = c('coral3','steelblue')) + 
    labs(title = "Linear Regression", subtitle = paste0("RMSE = ", round(out[[1]][1], 2))) + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "bottom")
  
  plts <- list(bart.plt, iptw.plt, lm.plt)
  
}



dat<- dgp(n = 250, lin = F, com.sup = "mod")
output <- modeler(dat)
plts <- ploter(output)

plts[2]

dat <- output[[2]]
check <- output[[2]]
attributes(output)
sample_lm

summary(sample_bart)

plot_support(sample_bart)
