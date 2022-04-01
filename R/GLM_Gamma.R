library(tidyverse)
library(nimble)

a <- rgamma(1e4, 2, 0.01)

tibble(dat = a, 
       link = 1 / a, 
       std_res = (a - mean(a)) / mean(a), 
       mod_res_dev = glm(a ~ 1, family = Gamma()) %>% residuals(type = "deviance"),
       mod_res_res = glm(a ~ 1, family = Gamma()) %>% residuals(type = "response"),
       mod_res_work = glm(a ~ 1, family = Gamma()) %>% residuals(type = "working"),
       mod_res_per = glm(a ~ 1, family = Gamma()) %>% residuals(type = "pearson")) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() + 
  facet_wrap(~ name, scales = "free")
