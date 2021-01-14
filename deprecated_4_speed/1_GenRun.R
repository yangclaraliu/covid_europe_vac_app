# function for generating baseline parameter ombinations
source("f_gen_country_basics.R")
# function for updating the baseline parameter set with vaccine characteristics
source("f_update_vac_char.R")
# function for updating the baseline parameter set with vaccine policy 
# characteristics
source("f_vac_policy.R")
# load all utility functions
source("f_utils.R")
# load healthcare system data
source("d_healthcare_parameters.R")
# generate health economics results
source("f_gen_econ.R")

# different priority settings
# priority_settings <- list(p1 = c(NA, NA, NA, NA,
#                                  5,  5,  5,  5,
#                                  5,  5,  5,  5,
#                                  4,  3,  2,  1),
#                           p2 = c(NA, NA, NA, NA,
#                                  2,  2,  2,  2,
#                                  2,  2,  2,  2,
#                                  1,  1,  1,  1),
#                           p3 = c(NA, NA, NA, NA,
#                                  1,  1,  1,  1,
#                                  1,  1,  1,  1,
#                                  1,  1,  1,  1))

# simulate end dates, determine the end of mobility imputation
sim_start = "2020-02-15"
sim_end = "2022-12-31"
# impute stringency index
si_post <- 10 # generally should be lower than si_post after examining the data
recovery_period <- 365 # recovery_period days to recover to si_post level of stringency
source("d_si_imputation.R")
source("d_mobility_imputation.R")

# res$dynamics %>% 
#   filter(compartment == "cases") %>% 
#   group_by(t) %>% 
#   summarise(value = sum(value)) %>% 
#   mutate(date = as.Date(date_start_tmp) + t,
#          iso = iso) %>% 
#   full_join(owid %>% 
#               filter(iso_code == iso) %>% 
#               dplyr::select(iso_code, date, new_cases_smoothed),
#             by = c("iso" = "iso_code",
#                    "date")) %>% 
#   arrange(date) %>% 
#   pivot_longer(cols = c(value, new_cases_smoothed)) %>% 
#   ggplot(., aes(x = date,
#                 y = value,
#                 color = name)) +
#   geom_point()

# we are going to leave this function here for now for debugging purposes
# gen_param <- function(strategy){
#   members$country_name %>% 
#     map(gen_country_basics) %>% 
#     map(~update_vac_char(para = .)) %>% 
#     map(~vac_policy(para = .,
#                     milestone_date = c("2021-01-01", # start from 0
#                                        "2021-06-01", # 0.03
#                                        "2021-12-31", # all population; 0.2
#                                        "2022-12-31"), # 0.6
#                     milestone_cov = c(0,
#                                       0.03,
#                                       0.2,
#                                       0.6),
#                     priority = priority_settings[[strategy]],
#                     cov_max = 0.8)) -> res
#   return(res)
# }

# c("p1", "p2", "p3") %>% 
#   map(gen_param) %>% 
#   setNames(c("p1", "p2", "p3")) -> param_list
# 
# run <- list()
# for(i in 1:3){
#   sapply(param_list[[i]], "[", "param") %>% 
#     map(~ cm_simulate(parameters = .,
#                       model_seed = 0,
#                       n = 1)) -> run[[i + 1]]
# }
# 
# members$country_name %>% 
#   map(gen_country_basics)  %>% 
#   map(~ cm_simulate(parameters = .,
#                     model_seed = 0,
#                     n = 1)) -> run[[1]]
# 
# run %>% 
#   map(~sapply(., "[", "dynamics")) %>% 
#   map(bind_rows) %>% 
#   setNames(c("p0","p1","p2","p3")) %>% 
#   bind_rows(.id = "policy") -> dyna
