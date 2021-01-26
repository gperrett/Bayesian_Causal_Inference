library(tidyverse)
library(bartCause)
library(caret)
library(ranger)
library(arm)

n <- 500
inv.log <- function(x){exp(x)/(1 + exp(x))} 
a.1 <- rnorm(n, 40, 10)
a.0 <- rnorm(n, 30, 10)
b <- runif(n,0, 2)
c <- rnorm(n, 30, 15)
d <- rbinom(n, 1, .5)
e <- rbinom(n, 1, .5)
f <- rbinom(n, 1, inv.log(e))
g.1 <- rnorm(n, 17, 5)
g.0 <- rnorm(n, 20, 5)
h.1 <- rnorm(n, 10, 1)
h.0 <- rnorm(n, 12, 2)

z <- rbinom(n, 1, inv.log(-2 + (b^2)))
a <- ifelse(z == 1, a.1, a.0)
g <- ifelse(z == 1, g.1, g.0)
y1 <- 40 + -.2*a + .5*c + 5*f + .2*b +  -.2*g + rnorm(n, 0, 1) + 3*(b^2)
y0 <- 40 -.2*a + .5*c  + 5*f + .2*b + -.2*g +rnorm(n, 0, 1)
y <- ifelse(z==1, y1, y0)
dat <- data.frame(a.1, a.0, a, b, c, d,e,f,g,h,  y1, y0, y, z)
true_att <- mean(dat[dat$z==1,]$y1 - dat[dat$z==1,]$y0)

# linear model 
sample_lm <- summary(lm(y~a+ b+ c+ d+e+f+g+h+ z, dat))
est.z.lm <- sample_lm$coefficients['z',1]
lm.rmse <- sqrt(((true_att - est.z.lm)**2))

# IPTW
iptw <- glm(z ~ a + b + c + d + e + f + g + h , family = "binomial", data = dat)
pscores.iptw <- predict(iptw, type = "response")
weights <- data.frame(pscores.iptw, z = dat$z)
weights$wgt <- ifelse(weights$z ==1, 
                      1/(exp(pscores.iptw)),
                      1/(exp(1 - pscores.iptw))) 
dat$wgt <- weights$wgt

sample_iptw <-lm(y~z + a + b + c + d + e + f + g + h, weights = wgt, data = dat)
est.z.iptw <- sample_iptw$coefficients['z']
iptw.rmse <- sqrt(((true_att - sample_iptw$coefficients['z'])**2))

# RF
rf <- ranger(z ~ a + b + c + d + e + f + g + h, data = dat,mtry = 2, classification = T,min.node.size = 10, num.trees = 100, splitrule= 'gini')

# tgrid <- expand.grid(
#   .mtry = 2:6,
#   .splitrule = c("gini","extratrees"),
#   .min.node.size = c(2,10, 20)
# )
# 
# model_caret <- train(Species  ~ ., data = iris,
#                      method = "ranger",
#                      trControl = trainControl(method="cv", number = 5, verboseIter = T, classProbs = T),
#                      tuneGrid = tgrid,
#                      num.trees = 100)
# model_caret$bestTune

pscores <- rf$predictions
match <- matching(dat$z, pscores, replace = F)
match.wgt <- match$cnts
sample_rf.pscore <- lm(y~z + a + b + c + d + e + f + g + h,weights = match.wgt, data= dat )
est.z.rfpscore <- sample_rf.pscore$coefficients['z']
rmse.rf.pscore <- sqrt((true_att - est.z.rfpscore)**2)


# BART
X <- as.matrix(dat[,c("a", "b","c","d", "e", "f", "g", "h")])
sample_bart <- bartc(y, z, confounders = X, data = dat, commonSup.rule = "sd", estimand = 'att')
sample_bart
bart_att <- dat %>% filter(sample_bart$commonSup.sub) %>% summarise(mean(y1 - y0)) %>% as.vector()
summary(sample_bart)
est.z.bart <- as.vector(summary(sample_bart)$estimate[1])

bart_rmse <- sqrt(((bart_att - est.z.bart)**2))

rmse <- tibble(LM = lm.rmse, IPTW = iptw.rmse, RF = rmse.rf.pscore ,BART = bart_rmse)

bart_rmse

bart_rmse
rmse.rf.pscore
rmse

iptw.rmse





dat %>% pivot_longer(3:10) %>% 
  ggplot(aes(value, y, col = factor(z))) + 
  geom_point(alpha = .7) + 
  facet_wrap(~name, scales = 'free')





