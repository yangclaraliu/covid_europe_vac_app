
# source("0_LoadData.R")
params_baseline <- gen_country_basics("United Kingdom of Great Britain") %>% 
  update_vac_char(ve = 0.9) 
res_baseline <- cm_simulate(params_baseline)
# params_baseline$schedule$mobility$values %<>% 
#   map(~c(0,0,0,0))

params <- gen_country_basics("United Kingdom of Great Britain") %>% 
  update_vac_char(ve = 0) %>% 
  vac_policy(milestone_date = c("2021-01-01", # start from 0
                                "2022-12-31"), # 0.6
             milestone_cov = c(0,
                               0.6),
             priority = c(NA, NA, NA, NA,
                          2,  2,  2,  2,
                          2,  2,  2,  2,
                          1,  1,  1,  1),
             cov_max = 0.7)

# params$param$pop[[1]]$ev <- rep(0,16)
params$param$pop[[1]]$ev <- rep(1,16)
params$param$pop[[1]]$ei_v <- rep(1, 16)
params$param$pop[[1]]$ei_v2 <- rep(1, 16)
params$param$pop[[1]]$ed_vi <- rep(0, 16)
params$param$pop[[1]]$ed_vi2 <- rep(0, 16)

# params$param$pop[[1]]$ev <- rep(1,16)
res <- cm_simulate(params$param)

res$dynamics %>% 
  left_join(res_baseline$dynamics, by = c("t", "group", "run", "population","compartment")) %>% 
  filter(compartment == "death_o") %>%
  group_by(t) %>% 
  summarise(cases = sum(value.x),
            baseline = sum(value.y)) %>% 
  mutate(tot_cases = cumsum(cases),
         tot_baseline = cumsum(baseline)) %>% 
  pivot_longer(cols = c("cases", "baseline",
                        "tot_cases", "tot_baseline")) %>% 
  separate(name, into = c("metric", "var")) %>% 
  mutate(var = if_else(is.na(var), metric, var),
         metric = if_else(metric != "tot", "daily", metric)) %>% 
  ggplot(., aes(x = t, y = value, color = var)) +
  geom_line(size = 2) +
  facet_wrap(~metric, scales = "free")

