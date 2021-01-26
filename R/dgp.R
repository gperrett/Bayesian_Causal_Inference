dgp <- function(n=1000, tau = 5, lin = T, com.sup = "strong", high.dim =F, seed = 1){
  
  z <- rbinom(n, 1, .5)
  inv.log <- function(x){exp(x)/(1 + exp(x))} 
  
  if(lin==T & com.sup == "strong")
  {
    X <- rnorm(n, 40, 10)
    X <- ifelse(X > 60 , 60, X)
    y1 <- 60 + .8*X + tau + rnorm(n, 0, 1) 
    y0 <- 60 + .8*X +  rnorm(n, 0, 1) 
    y <- ifelse(z==1, y1, y0)
    dat <- data.frame( X, y1, y0, y, z)
    
  }
  
  if(lin==T & com.sup == "mod")
  {
    X0 <- rnorm(n, 20, 10)
    X0 <- ifelse(X0<0, 0, X0)
    X1 <- rnorm(n, 30, 10)
    X <- ifelse(z==1, X1, X0)
    X <- ifelse(X > 60 , 60, X)
    y1 <- 60 + .8*X + tau + rnorm(n, 0, 1)
    y0 <- 60 + .8*X +  rnorm(n, 0, 1) 
    y <- ifelse(z==1, y1, y0)
    dat <- data.frame(X1, X0, X, y1, y0, y, z)
    
  }
  
  
  if(lin==T & com.sup == "weak")
  {
    X0 <- rnorm(n, 20, 10)
    X0 <- ifelse(X0<0, 0, X0)
    X1 <- rnorm(n, 40, 10)
    X <- ifelse(z==1, X1, X0)
    X <- ifelse(X > 60 , 60, X)
    y1 <- 60 + .8*X + tau + rnorm(n, 0, 1)
    y0 <- 60 + .8*X +  rnorm(n, 0, 1) 
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
    y0 <- rnorm(n, 72 + 3*sqrt(X), 1) 
    y1 <- rnorm(n, 90 + exp((.06*X)), 1)
    y1 <- ifelse(y1>120, 120, y1)
    y <- ifelse(z==1, y1, y0)
    dat <- data.frame(X1, X0, X, y1, y0, y, z)
    
    
  }
  
  if(lin==F & com.sup == "strong")
  {
    X0 <- rnorm(n, 30, 10)
    X0 <- ifelse(X0<0, 0, X0)
    X0 <- ifelse(X0>60, 60, X0)
    X1 <- rnorm(n, 30, 10)
    X1 <- ifelse(X1>60, 60, X1)
    X1 <- ifelse(X1 <0, 0, X1)
    X <- ifelse(z==1, X1, X0)
    y0 <- rnorm(n, 72 + 3*sqrt(X), 1)  + rnorm(n, 0, 1) + rnorm(n, 0, 1)
    y1 <- rnorm(n, 90 + exp((.06*X)), 1) + rnorm(n, 0, 1) + + rnorm(n, 0, 1)
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
    y0 <- rnorm(n, 72 + 3*sqrt(X), 1)
    y1 <- rnorm(n, 90 + exp((.06*X)), 1) 
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
