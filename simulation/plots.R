dat <- dgp(n = 120, com.sup = "weak", lin =F)

sample_bart <- bartc(y, z, confounders = X, data = dat, commonSup.rule = "sd",estimand = 'att', seed = 2)
ite <- bartCause::extract(sample_bart, 'ite')
ite.m <- apply(ite, 2, mean)
ite.sd <- apply(ite, 2, sd) 
ite.lb <- ite.m - 1.96 * ite.sd 
ite.ub <- ite.m + 1.96 * ite.sd
dat[dat$z==1,'ite.m'] <- ite.m
dat[dat$z==1,'ite.lb'] <- ite.lb
dat[dat$z==1,'ite.ub'] <- ite.ub


dat$removed <- abs(1 - sample_bart$commonSup.sub)
dat$removed <- ifelse(dat$removed ==1, "Excluded","Included")
y.true <- vector()
for (X in 0:60) {
  y.true[X] <- (90 + exp((.06*X))) - (72 + 3*sqrt(X))
  
}
line <- tibble(y.true, X = 1:60)

p1 <- ggplot() + 
  geom_pointrange(data = dat,aes(x = X, y = ite.m,ymin = ite.lb, ymax = ite.ub, col = removed)) +
  geom_smooth(data = line, aes(X, y.true), col = 'black') + 
  scale_color_manual(values = c('coral3','steelblue')) + 
  labs(y = 'Predicted Individual Treatment Effect', title = "Weak Common Support") + 
  theme_bw()

p2 <- ggplot() + 
  geom_pointrange(data = dat,aes(x = X, y = ite.m,ymin = ite.lb, ymax = ite.ub, col = removed)) +
  geom_smooth(data = line, aes(X, y.true), col = 'black') + 
  scale_color_manual(values = c('coral3','steelblue')) + 
  labs(y = 'Predicted Individual Treatment Effect', title = "Moderate Common Support") + 
  theme_bw()

p3 <- ggplot() + 
  geom_pointrange(data = dat,aes(x = X, y = ite.m,ymin = ite.lb, ymax = ite.ub, col = removed)) +
  geom_smooth(data = line, aes(X, y.true), col = 'black') + 
  scale_color_manual(values = c('coral3','steelblue')) + 
  labs(y = 'Predicted Individual Treatment Effect', title = "Strong Common Support") + 
  theme_bw()

?egg::ggarrange
egg::ggarrange(p1, p2, p3, nrow = 1, l)

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

p1
p2
p3
