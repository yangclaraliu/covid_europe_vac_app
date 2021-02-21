make_params <- function(cn_tmp, 
                        t, 
                        # s_A,
                        R0_target){

  wb_tmp <- countrycode(cn_tmp, "country.name", "wb")
  mob_cn <- data.table(schedule_raw)[wb == wb_tmp] %>% arrange(date) 
  epi_cn <- epi[loc == wb_tmp] %>% arrange(date)
  ymd_start <- ymd("2019-12-01") + t 
  ymd_end <- min(# mob_cn[status == "empirical"]$date %>% max,
                 epi_cn$date %>% max)
  if(is.infinite(ymd_end)) ymd_end <- "2020-12-16"
  mob_cn <- mob_cn[date <= ymd_end & date >= ymd_start]
  
  para <- cm_parameters_SEI3R(dem_locations = cn_tmp,
                             date_start = ymd("2019-12-01")+t,
                             date_end = ymd_end,
                             dE  = cm_delay_gamma(2.5, 2.5, t_max = 15, t_step = 0.25)$p,
                             dEa = cm_delay_gamma(2.5, 2.5, t_max = 15, t_step = 0.25)$p,
                             dIp = cm_delay_gamma(1.5, 4.0, t_max = 15, t_step = 0.25)$p,
                             dIs = cm_delay_gamma(3.5, 4.0, t_max = 15, t_step = 0.25)$p,
                             dIa = cm_delay_gamma(5.0, 4.0, t_max = 15, t_step = 0.25)$p,
                             deterministic = T)
  
  n_age_groups <- para$pop[[1]]$size %>% length
  para$pop[[1]]$wn <- rep((1/(45*7)), n_age_groups)
  para$pop[[1]]$y <- cf
  para$pop[[1]]$u <- sus
  current_R0 = cm_calc_R0(para, 1) # calculate R0 in population i of para
  para$pop[[1]]$u = para$pop[[1]]$u * R0_target / current_R0
  para$processes = burden_processes
  
  # para$schedule[["seasonality"]] <- list(
  #   parameter = "season_A",
  #   pops = 0,
  #   mode = "assign",
  #   values = list(s_A),
  #   times = as.numeric(ymd("2020-09-15") - ymd(para$date0))
  # )
  
  para$schedule[["mobility"]] = list(
    parameter = "contact",
    pops = numeric(),
    mode = "assign",
    values = split(mob_cn[,4:7],
                   seq(nrow(mob_cn))) %>%
      map(unlist) %>%
      map(as.vector) %>%
      unname,
    times = 0:(nrow(mob_cn)-1))
  
  para$pop[[1]]$seed_times <- c(1:14)
    
  return(para)
}
