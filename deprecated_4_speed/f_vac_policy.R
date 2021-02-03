# f_vac_policy.R
# Authors: Yang Liu
# Date: 29/10/2020
# Date updated: 29/10/2020
# Objective: parameterise vaccine policies in covidm

vac_policy <- function(para,
                       milestone_date = c("2021-01-01", # start from 0
                                          "2021-06-01", # 0.03
                                          "2021-12-31", # all population; 0.2
                                          "2022-12-31"), # 0.6
                       milestone_cov = c(0,
                                         0.03,
                                         0.2,
                                         0.6),
                       priority = c(NA, NA, NA, NA,
                                    5,  5,  5,  5,
                                    5,  5,  5,  5,
                                    4,  3,  2,  1),
                       # date_start = date_start,
                       cov_max = 0.7 # maximum coverage objective in each population
                       
){
  # require(magrittr)
  
  #### variables for debugging ####
  # para <- gen_country_basics("Belgium") %>% update_vac_char(ve_i = 0.9, ve_d = 0)
  # milestone_date = c("2021-01-01", # start from 0
  #                    "2021-06-01", # 0.03
  #                    "2021-12-31", # all population; 0.2
  #                    "2022-12-31") # 0.6
  # milestone_cov = c(0,
  #                   0.03,
  #                   0.2,
  #                   0.6)
  # priority = priority_policy[[3]]
  # cov_max = 0.7
  # #######
  
  date_start = para$date0
  
  # error messages RE: parameters
  if((length(milestone_date)) != length(milestone_cov)) {
    stop("The length of milestones do not line up. Note that the date milestones 
         should have one more element compared to coverage milestones.")
  }
  
  # details of the vaccine rollout schedules
  tmp_schedule <- data.frame(milestone_date = c(para$date0, 
                                                milestone_date, 
                                                para$time1),
                             milestone_cov = c(NA, 
                                               milestone_cov, 
                                               NA)) %>% 
    group_by(milestone_date) %>% 
    summarise(milestone_cov = mean(milestone_cov, 
                                   na.rm = T),
              .groups = "drop") %>% 
    ungroup %>% 
    mutate(milestone_date = lubridate::ymd(milestone_date),
           t = milestone_date - as.Date(para$date0),
           cov = c(NA, diff(zoo::na.locf(milestone_cov, 
                                         na.rm = F))),
           cov = if_else(cov %in% c(0, NA, NaN), 
                         as.numeric(NA), 
                         cov),
           doses = cov * sum(para$pop[[1]]$size),
           t_diff = c(NA, diff(t)),
           doses_daily = dplyr::lead(doses/t_diff, 1),
           milestone_marker = !is.na(milestone_cov))
  
  # details of the target population
  tmp_pop <- data.frame(n_pop = para$pop[[1]]$size) %>% 
    mutate(n_tar = n_pop * cov_max,
           # licen = priority,
           # licen = if_else(licen > 1, 1, licen),
           # licen = if_else(is.na(licen), 0, licen),
           ve = para$pop[[1]]$ev,
           wv = para$pop[[1]]$wv)
  
  # details on age-specific prioritisation
  tmp_priorities <- priority %>% 
    enframe(name = "age_group") %>% 
    filter(!is.na(value)) %>% 
    arrange(value) %>% 
    group_by(value) %>% 
    group_split()
  
  # age group specific population size cap
  pop_cap <- tmp_priorities %>% 
    map(~.$age_group) %>% 
    map(~tmp_pop$n_tar[.]) 
  
  # population saturation marker
  pop_marker <- pop_cap %>% 
    map(sum) %>% 
    unlist() %>% 
    cumsum() 
  
  # create empty grid to score vaccination plans
  matrix(0, 
         ncol = length(para$pop[[1]]$size),
         nrow = max(as.numeric(tmp_schedule$t), na.rm = T)) %>% 
    as_tibble() %>% 
    setNames(paste0("Y",1:16)) %>% 
    rownames_to_column(var = "t") %>% 
    mutate(supply = NA) -> daily_vac
  
  # calculate the daily supply of vaccine doses
  daily_vac$supply[1:tmp_schedule$t[2]] <- 0
  for(i in 2:(nrow(tmp_schedule)-1)){
    daily_vac$supply[tmp_schedule$t[i]:tmp_schedule$t[i+1]] <-
      tmp_schedule$doses_daily[i]
  }

  # identify dates when population becomes saturated
  date_marker <- sapply(1:length(pop_marker), 
                        function(i) {
                          min(which(cumsum(daily_vac$supply) >= 
                                      pop_marker[i]))})
  
  # a warning message on having vaccinated everyone we think is necessary
  if(!is.infinite(tail(date_marker, 1))) 
  {warning("You have exhausted the population.")}
  
  # recycle back in when vaccintion programs start
  # remove options where populations are never saturated
  # not doing anything about this right now
  date_marker <- c(tmp_schedule %>% 
                     filter(milestone_cov == 0) %>% 
                     pull(t) %>% 
                     as.numeric, 
                   date_marker) %>% 
    .[!is.infinite(.)]
  
  # if there's inconsistenties between the last date_marker this code can fix 
  # it - using the end of the model running window.
  if(length(date_marker)-1 < length(tmp_priorities)) {
    date_marker <- c(date_marker, 
                     as.numeric(max(tmp_schedule$t)) + 1)
  }
  # allocate vaccines available each day to specific population age groups 
  # using pre-set schedules
  for(i in 1:(length(date_marker) - 1)){
    # vaccines available
    if(!is.na(date_marker[i]) & !is.na((date_marker[i+1] - 1))){
      supply_tmp <-  daily_vac[date_marker[i]:(date_marker[i+1] - 1), 
                               "supply"] %>% as.matrix()
      # weights assigned to each age group, aligned
      # transposed for matrix multiplication purpose only
      weights_tmp <- matrix(tmp_pop$n_pop[tmp_priorities[[i]]$age_group]/
                              sum(tmp_pop$n_pop[tmp_priorities[[i]]$age_group])) %>% 
        t
      
      daily_vac[date_marker[i]:(date_marker[i+1] - 1), 
                tmp_priorities[[i]]$age_group + 1] <- 
        # daily_vac[date_marker[i]:(date_marker[i+1] - 1), "supply"]/length(tmp_priorities[[i]]$age_group)
        supply_tmp %*% weights_tmp
    }
  }
  
  # putting all vaccine policy related raw parameters back together
  daily_vac %>% 
    group_by_at(vars(starts_with("Y"))) %>% 
    dplyr::summarise(t = min(t),
              .groups = "drop") %>% 
    ungroup() -> vac_para
  
  # then convert these parameters to a format that's friendly with `covidm`
  # allocation
  vac_para %>% 
    dplyr::select(starts_with("Y")) %>% 
    split(seq(nrow(vac_para))) %>% 
    map(unlist) %>% 
    map(as.vector) -> vacc_vals
  
  # timing
  vacc_times <- vac_para$t %>% as.numeric; vacc_times[1] <- 0
  
  para$schedule[["vaccination"]] = list(
    parameter = "v",
    pops = numeric(),
    mode = "assign",
    values = vacc_vals,
    times = vacc_times)
  
  daily_vac %<>% 
    mutate(doses_daily = supply,
           supply = cumsum(supply),
           date = as.Date(date_start) + as.numeric(t))
  
  return(list(param = para, 
              supply = tmp_schedule,
              vac_para = vac_para,
              daily_vac = daily_vac))
}