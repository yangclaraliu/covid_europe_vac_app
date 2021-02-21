# f_gen_couuntry_basics.R
# Authors: Yang Liu, Frank Sandmann
# Date: 29/10/2020
# Date updated: 29/10/2020
# Objective: create baseline parameter sets for covidm

gen_country_basics <- function(country,
                               waning_nat = 45*7,
                               R0_assumed  = 2.7,
                               date_start = "2020-01-01",
                               date_end = "2022-12-31",
                               # s_A = 0.1,
                               deterministic = TRUE){
  
  wb_tmp = countrycode(country, "country.name", "wb")
  c_tmp = schedule_raw %>% 
    filter(wb == wb_tmp,
           date >= lubridate::ymd(date_start),
           date <= lubridate::ymd(date_end))

  t_run <- nrow(c_tmp)

  para = cm_parameters_SEI3R(dem_locations = country, 
                             date_start = date_start, 
                             date_end = date_end,
                             dE  = cm_delay_gamma(2.5, 2.5, t_max = 15, t_step = 0.25)$p,
                             dEa = cm_delay_gamma(2.5, 2.5, t_max = 15, t_step = 0.25)$p,
                             dIp = cm_delay_gamma(1.5, 4.0, t_max = 15, t_step = 0.25)$p,
                             dIs = cm_delay_gamma(3.5, 4.0, t_max = 15, t_step = 0.25)$p,
                             dIa = cm_delay_gamma(5.0, 4.0, t_max = 15, t_step = 0.25)$p,
                             deterministic = deterministic)
  
  n_age_groups <- length(para$pop[[1]]$size)
  
  for(i in 1:length(para$pop)){
    
    para$pop[[i]]$y <- cf
    para$pop[[i]]$u <- sus
    
    # scale u (susceptibility) to achieve desired R0
    current_R0 = cm_calc_R0(para, i); # calculate R0 in population i of params
    para$pop[[i]]$u = para$pop[[i]]$u * R0_assumed / current_R0
    
    # natural waning
    para$pop[[i]]$wn <- rep((1/waning_nat), n_age_groups)
    
    ## Set seeds to control start of outbreak
    para$pop[[i]]$dist_seed_ages = cm_age_coefficients(20, 50, 5 * (0:length(para$pop[[i]]$size))) # infections start in individuals aged 20-50
    
    # 5 new infections each day for 7 days every month
    # para$pop[[i]]$seed_times = sapply(30*0:(4* as.numeric(gsub("-.*$", "", para$time1)) - 
    #                                           as.numeric(gsub("-.*$", "", para$date0))),
    #                                   function(x) x+rep(0:6, each = 5)) %>% as.vector 
    para$pop[[i]]$seed_times <- c(1:14)
  }
  
  para$processes = burden_processes
  
  # para$schedule[["seasonality"]] <- list(
  #   parameter = "season_A",
  #   pops = 0,
  #   mode = "assign",
  #   values = list(s_A),
  #   times = as.numeric(lubridate::ymd("2020-09-30") - 
  #                        lubridate::ymd(date_start))
  # )
  
  para$schedule[["mobility"]] = list(
    parameter = "contact",
    pops = numeric(),
    mode = "assign",
    values = split(c_tmp[,4:7],
                   seq(nrow(c_tmp))) %>%
      map(unlist) %>%
      map(as.vector) %>%
      unname,
    times = 1:t_run)
  
  return(para)
}
