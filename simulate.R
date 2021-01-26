library(tidyverse)

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
    X0 <- rnorm(1000, 20, 10)
    X0 <- ifelse(X0<0, 0, X0)
    X1 <- rnorm(1000, 40, 10)
    X1 <- ifelse(X1>60, 60, X1)
    X1 <- ifelse(X1 <0, 0, X1)
    X <- ifelse(z==1, X1, X0)
    y0 <- rnorm(1000, 72 + 3*sqrt(X0), 1) 
    y1 <- rnorm(1000, 90 + exp((.06*X1)), 1)
    y1 <- ifelse(y1>120, 120, y1)
    y <- ifelse(z==1, y1, y0)
    dat <- data.frame(X1, X0, X, y1, y0, y, z)
    
    
  }
  
  if(lin==F & com.sup == "strong")
  {
    X0 <- rnorm(1000, 30, 10)
    X0 <- ifelse(X0<0, 0, X0)
    X1 <- rnorm(1000, 30, 10)
    X1 <- ifelse(X1>60, 60, X1)
    X1 <- ifelse(X1 <0, 0, X1)
    X <- ifelse(z==1, X1, X0)
    y0 <- rnorm(1000, 72 + 3*sqrt(X0), 1) 
    y1 <- rnorm(1000, 90 + exp((.06*X1)), 1)
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

dat<- dgp()
  modeler <- function(dat){
  if('X' %in% names(dat) ){
    # linear model
    sample_lm <- lm(y~ z + X, dat)
    est.z.lm <- sample_lm$coefficients['z']
    lm.rmse <- sqrt(((mean(dat$y1 - dat$y0) - est.z.lm)**2))
    
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
    
    
    # combine RMSE
    est.rmse <- data.frame(lm.rmse,iptw.rmse ,bart.rmse)
    names(est.rmse) <- c("linear regression", "IPTW", "BART")
    rownames(est.rmse) <- "rmse"
  }
  
  
  if('a' %in% names(dat)){
  # linear model
  sample_lm <- lm(y~ z + a + b + c + d + e + f, dat)
  est.z.lm <- sample_lm$coefficients['z']
  lm.rmse <- sqrt(((mean(dat$y1 - dat$y0) - est.z.lm)**2))
  
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
  
  
  # combine RMSE
  est.rmse <- data.frame(lm.rmse,iptw.rmse ,bart.rmse)
  names(est.rmse) <- c("linear regression", "IPTW", "BART")
  rownames(est.rmse) <- "rmse"
  }
    return(est.rmse)
}

sampler <- function(){modeler(dgp(n=200,lin = F, com.sup = "weak"))}
test.rmse <- mclapply(1:10000, function(i){sampler()})

test.rmse <- bind_rows(test.rmse) 
test.rmse %>% pivot_longer(1:3) %>% 
ggplot(aes(value, fill = name))+ 
  geom_boxplot()
  #geom_density(alpha = .5)

  ggplot(dat, aes(X, y, col = factor(z))) + 
  geom_point() + 
  geom_smooth(method = "lm")
  
  dat <- dgp(lin = F, com.sup = "strong")
ggplot(dat, aes(X, y, col = factor(z))) + 
  geom_point()
