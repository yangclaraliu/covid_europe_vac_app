# with currently available data, stringency generally was paused between 22 and 81
# si[date == as.Date(si_stopdate)-1]$StringencyIndex %>% range
si_stopvalue <- si[date == as.Date(si_stopdate)-1]

# draw gradual changes
si_changes <- list()
for(i in 1:nrow(si_stopvalue)){
  asc(seq(from = 0, 
          to = 1, 
          length.out = recovery_period),
      si_stopvalue$StringencyIndex[i],
      si_post,
      -5,
      5) -> si_changes[[i]]
}

si_changes %>% 
  setNames(si_stopvalue$wb) %>% 
  map(data.table) %>%
  map(~.[, date := seq(as.Date(si$date %>% range %>% .[2]),
                       as.Date(si$date %>% range %>% .[2]) + recovery_period - 1,
                       1)]) %>% 
  bind_rows(.id = "wb") %>% 
  setNames(c("wb", "StringencyIndex", "date")) %>%
  bind_rows(si[,list(wb, StringencyIndex, date)]) %>% 
  right_join(CJ(date = seq(as.Date(sim_start), 
                           as.Date(sim_end), 
                           1),
                wb = unique(si$wb)), 
             by = c("wb","date")) %>% 
  mutate(StringencyIndex = if_else(is.na(StringencyIndex),
                                   si_post,
                                   StringencyIndex)) %>% 
  mutate(status = case_when(date <= si$date %>% range %>% .[2] ~ "empirical",
                            date %in% seq(as.Date(si$date %>% range %>% .[2]),
                                          as.Date(si$date %>% range %>% .[2]) + recovery_period - 1,
                                          1) ~ "transition",
                            TRUE ~ "post-pandemic")) %>% 
  mutate(d = as.numeric(date)) -> si_imputed

ggplot(si_imputed, aes(x = date,
                       color = status,
                       y = StringencyIndex)) +
  geom_point() +
  facet_wrap(~wb) +
  theme_bw() -> p  

# ggsave(plot = p,
#          filename = "figs/si_imputed.png")




