
library(tidyverse)
library(ggsci)

df1 %>% # each column is a RMSE score from a simulation
  pivot_longer(1:5) %>% # pivot to long form 
  ggplot(aes(value, fill = name)) + 
  geom_boxplot() + 
  ggsci::scale_fill_nejm(name = "Estimator", labels = c("BART 1 SD rule", "BART", "IPTW", "Linear Regression", "Propensity Score Matching")) + 
  labs(title = "Study 2: 1000 Iterations", x = "RMSE", y = element_blank()) + 
  theme_bw() + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "left")

