library(tidyverse)
library(ggnewscale)
ploter <- function(out){
  dat <- out[[2]]
  bart.plt <- ggplot(dat, aes(X, y, col = factor(z))) + 
    geom_linerange(aes(ymin = cf.lb, ymax = cf.ub)) + 
    geom_point(alpha = .8) + 
    scale_color_manual(labels = c("Control", "Treatment"), values = c('coral3','steelblue')) + 
    scale_y_continuous(limits = c(60, 120)) +
    labs(title = "BART", subtitle = paste0("Estimated ATT = ", round(out[[1]]$estimate[4,1], 2)," ", '(',round(out[[1]]$li[4,1], 2)," - ",round(out[[1]]$ui[4,1], 2),")" ,"\n", "True ATT = ", round(out[[1]]$truth[4,1], 2),"\n", "RMSE = ", round(out[[1]]$rmse[4,1], 2))) +  
    theme_bw() + 
    theme(legend.title = element_blank(), 
          legend.position = "bottom", 
          plot.title = element_text(size = 16, face = 'bold'), 
          plot.subtitle = element_text(size = 16, face = 'bold'))
  
  
  
  iptw.plt <-  ggplot() + 
    geom_point(data = dat[dat$z==0,],aes(X, y, col = wgt)) + 
    scale_color_gradient('Control', low = 'coral4', high = 'coral') +
    new_scale_color() + 
    geom_point(data = dat[dat$z==1,],aes(X, y, col = wgt)) + 
    scale_color_gradient('Treatment') + 
    scale_y_continuous(limits = c(60, 120)) +
    labs(title = "IPTW", subtitle = paste0("Estimated ATT = ", round(out[[1]]$estimate[2,1], 2)," ", '(',round(out[[1]]$li[2,1], 2)," - ",round(out[[1]]$ui[2,1], 2),")" ,"\n", "True ATT = ", round(out[[1]]$truth[2,1], 2),"\n", "RMSE = ", round(out[[1]]$rmse[2,1], 2))) +  
    theme_bw()+ 
    theme(
          plot.title = element_text(size = 16, face = 'bold'), 
          plot.subtitle = element_text(size = 16, face = 'bold'))
    # 

  
  lm.plt <- ggplot(dat, aes(X, y, col = factor(z))) +
    geom_point(alpha = .8) +
    scale_y_continuous(limits = c(60, 120)) +
    geom_abline(
      intercept = dat[dat$z==0,]$intercept[1],
      slope = dat[dat$z==0,]$slope[1],
      col = 'coral3'
    ) +
    geom_abline(
      intercept = dat[dat$z==1,]$intercept[1],
      slope = dat[dat$z==1,]$slope[1],
      col = 'steelblue'
    ) +
    scale_color_manual(labels = c("Control", "Treatment"), values = c('coral3','steelblue')) + 
    labs(title = "Linear Regression", subtitle = paste0("Estimated ATT = ", round(out[[1]]$estimate[1,1], 2)," ", '(',round(out[[1]]$li[1,1], 2)," - ",round(out[[1]]$ui[1,1], 2),")" ,"\n", "True ATT = ", round(out[[1]]$truth[1,1], 2),"\n", "RMSE = ", round(out[[1]]$rmse[1,1], 2))) +  
    theme_bw() + 
    theme(legend.title = element_blank(), 
          legend.position = "bottom", 
          plot.title = element_text(size = 16, face = 'bold'), 
          plot.subtitle = element_text(size = 16, face = 'bold'))
  
  rf.plt <- ggplot() + 
    geom_point(data = dat[dat$z==0,],aes(X, y, col = match)) + 
    scale_color_gradient('Control', low = 'coral4', high = 'coral') +
    new_scale_color() + 
    geom_point(data = dat[dat$z==1,],aes(X, y, col = match)) + 
    scale_color_gradient('Treatment') + 
    scale_y_continuous(limits = c(60, 120)) +
    labs(title = "Propensity Score Matching", subtitle = paste0("Estimated ATT = ", round(out[[1]]$estimate[3,1], 2)," ", '(',round(out[[1]]$li[3,1], 2)," - ",round(out[[1]]$ui[3,1], 2),")" ,"\n", "True ATT = ", round(out[[1]]$truth[3,1], 2),"\n", "RMSE = ", round(out[[1]]$rmse[3,1], 2))) +  
    theme_bw()+ 
    theme(
          plot.title = element_text(size = 16, face = 'bold'), 
          plot.subtitle = element_text(size = 16, face = 'bold'))
  
  plts <- list(bart.plt, iptw.plt, lm.plt, rf.plt)
  return(plts)
  
}
