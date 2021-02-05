# ve is measured based on reduction in symptomatic infection
# ei_v is per covidm definition
# ed_vi is per covidm definition
# y is clinical fractiona, proportion symptomatic

library(data.table)
library(tidyverse)

exp_ve <- function(ve, ei_v, y){
  
  ed_vi <- (ve - ei_v)/(1 - ei_v)
  p_asym <- (1-ei_v)*(1-ed_vi)*(1-y) + (1-ei_v)*ed_vi
  vea <- 1 - p_asym/(1-y)
  r <- list(ed_vi = ed_vi, vea = vea)
  return(r)
  
}

CJ(ve = seq(0.5,0.99,0.01),
   ei_v = seq(0.5,0.99,0.01),
   y = seq(0.15, 0.85, 0.1)) %>% 
  filter(ve >= ei_v) %>% 
  mutate(ed_vi = exp_ve(ve = ve, ei_v = ei_v, y = y)[[1]],
         vea = exp_ve(ve = ve, ei_v = ei_v, y = y)[[2]]) -> tab

tab %>%
  mutate(y = paste0("CF = ", y)) %>% 
  ggplot(., aes(x = ve, y = ei_v, fill = vea)) +
  geom_tile() +
  facet_wrap(~y, nrow = 2) +
  labs(x = "VE against symptomatic infections",
       y = "VE against all infections",
       fill = "VE against asymptomatic infections",
       caption = "CF = clinical Fraction") +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90)) +
  scale_fill_gradient2()
 
