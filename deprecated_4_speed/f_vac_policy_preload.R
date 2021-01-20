# f_vac_policy.R
# Authors: Yang Liu
# Date: 29/10/2020
# Date updated: 29/10/2020
# Objective: parameterise vaccine policies in covidm

vac_policy_preload <- function(para,
                               pattern_tmp = "linear",
                               priority_tmp = NULL,
                               cov_max = NULL
){
  # debug
  # para <- gen_country_basics("Belgium") %>% update_vac_char()
  # pattern_tmp = "linear"
  # cov_max = 0.7
  # priority_tmp <- priority_policy[[4]]

  # details of the vaccine rollout schedules
  vac_progress[, ..pattern_tmp] %>% 
    setNames("value") %>% 
    mutate(date = seq(as.Date("2021-01-01"),
                      as.Date("2021-12-31"),
                      # as.Date("2022-12-31"),
                      1),
           pop = sum(para$pop[[1]]$size),
           doses_daily = pop * value,
           cov = cumsum(value),
           doses = cumsum(doses_daily)) -> tmp_schedule_preload
  
  # details of the target population
  tmp_pop <- data.frame(n_pop = para$pop[[1]]$size) %>% 
    mutate(n_tar = n_pop * cov_max,
           # licen = priority,
           # licen = if_else(licen > 1, 1, licen),
           # licen = if_else(is.na(licen), 0, licen),
           ve = para$pop[[1]]$ev,
           wv = para$pop[[1]]$wv)
  
  # details on age-specific prioritisation
  tmp_priorities <- priority_tmp %>% 
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
  seq(lubridate::ymd(para$date0),
      lubridate::ymd("2022-12-31"),
      # lubridate::ymd("2021-12-31"),
      1) %>%
    enframe(value = "date") %>%
    bind_cols(matrix(0,
                     ncol = length(para$pop[[1]]$size),
                     nrow = nrow(.)) %>%
                data.table %>%
                setNames(paste0("Y",1:16))
    )  %>% 
    left_join(tmp_schedule_preload, by = "date") %>% 
    mutate(pop = mean(pop, na.rm = T)) %>% 
    # mutate(pattern = zoo::na.locf(pattern, fromLast = T),
    #        pop = zoo::na.locf(pop, fromLast = T)) %>% 
    replace(., is.na(.), 0) %>% 
    mutate(supply = cumsum(doses_daily)) -> daily_vac
  
  # identify dates when population becomes saturated
  date_marker <- sapply(1:length(pop_marker), 
                        function(i) {
                          min(which(daily_vac$supply >= 
                                      pop_marker[i]))})
  
  # a warning message on having vaccinated everyone we think is necessary
  if(!is.infinite(tail(date_marker, 1))) 
  {warning("You have exhausted the population.")}
  
  date_marker %<>% 
    .[!is.infinite(.)] %>% 
    c(min(which(daily_vac$doses_daily>0)),
      .,
      nrow(daily_vac)) 
  
  
  # allocate vaccines available each day to specific population age groups 
  # using pre-set schedules
  for(i in 1:(length(date_marker) - 1)){
    # vaccines available
    if(!is.na(date_marker[i]) & !is.na((date_marker[i+1] - 1))){
      supply_tmp <-  daily_vac[date_marker[i]:(date_marker[i+1] - 1), 
                               "doses_daily"] %>% as.matrix()
      # weights assigned to each age group, aligned
      # transposed for matrix multiplication purpose only
      weights_tmp <- matrix(tmp_pop$n_pop[tmp_priorities[[i]]$age_group]/
                              sum(tmp_pop$n_pop[tmp_priorities[[i]]$age_group])) %>% 
        t
      
      daily_vac[date_marker[i]:(date_marker[i+1] - 1), 
                tmp_priorities[[i]]$age_group + 2] <- 
        # daily_vac[date_marker[i]:(date_marker[i+1] - 1), "supply"]/length(tmp_priorities[[i]]$age_group)
        supply_tmp %*% weights_tmp
    }
  }
  
  # number going into revaccination
  # re-vacciantion starts as soon as an age group is done vaccination
  daily_vac %>% 
    dplyr::select(starts_with("Y", ignore.case = F)) %>%
    mutate_at(vars(paste0("Y",1:16)), cumsum) %>% 
    apply(., 2, max) %>% 
    "*"(para$pop[[1]]$wv) -> n_revac
  
  daily_vac %>% 
    dplyr::select(starts_with("Y", ignore.case = F)) %>% 
    apply(., 2, which.max) %>% 
    "+"(1)-> start_revac

  for(i in 1:16){
    daily_vac[(start_revac[i]:nrow(daily_vac)),paste0("Y",i)] <- n_revac[i]
  }
  
  daily_vac[(which.max(daily_vac$cov)+1):nrow(daily_vac),"cov"] <-
    max(daily_vac$cov)
  
  daily_vac %<>% 
    mutate(doses_daily =rowSums(.[grep("Y[1-9]", names(.))]),
           doses = cumsum(doses_daily),
           supply = cumsum(doses_daily)) # %>%
    # filter(doses_daily <= 7500 & date >= "2021-03-15") %>% View()
    # filter(date <= "2021-09-01" & date >= "2021-05-01") %>% View()
    # ggplot(., aes(x = date, y = doses_daily)) +
    # geom_line()
    # 
   # putting all vaccine policy related raw parameters back together
  daily_vac %>% 
    group_by_at(vars(starts_with("Y"))) %>% 
    summarise(t = min(t),
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
  
  return(list(param = para, 
              supply = tmp_schedule_preload,
              vac_para = vac_para,
              daily_vac = daily_vac))
  }
