predict_deriv <- function(
                          cn, # country name
                          # S_A, # size of seasonal component
                          cov_tar = NULL, # target coverage  
                          type_ms = "Preload",
                          pattern_label = NULL,
                          ms_date = NULL, # milestone - dates
                          ms_cov = NULL, # milestones - coverage levels
                          # priority = NULL, # priority settings
                          date_start = NULL, # starting date of uncontrollable community transmission
                          date_end = "2022-12-31",
                          eff = NULL,#c(rep(0.9,10),rep(0.8,6)),
                          wane = NULL, # natural waning; vaccine waning
                          R = NULL
                          ){
  
  # debugging
  # cn = "Belgium"                  # country name
  # S_A = 0.2                       # size of seasonal component
  # cov_tar = 0.5                   # target coverage
  # ms_date <- c("2021-01-01",
  #              "2021-06-30",
  #              "2021-12-31",
  #              "2022-12-31")      # milestones for dates
  # ms_cov <- c(0, 0.03, 0.2, 0.6)  # milestones for coverage
  # date_start = "2020-02-15"       # starting date of uncontrollable community transmission
  # date_end = "2022-12-31"
  # eff = c(rep(0.9,10),
  #         rep(0.8,6))             # vaccine efficacy
  # wane = c(45, 1)
  # R = 2.7
  # pattern = "linear"
  # type_ms = "Preload"

  pattern <-   case_when(pattern_label == "Linear Increase" ~ "linear",
                         pattern_label == "Exponential Increase" ~ "exponential",
                         pattern_label == "Sigmoid Increase" ~ "sigmoid")


  # fcontent
  wb <- countrycode::countrycode(cn, "country.name", "wb")
  S_A <- model_selected %>% filter(WB == wb) %>% pull(S_A)
  # debugging version
  # date_start <- model_selected %>% filter(WB == wb) %>% pull(start_date) %>% as.Date
  # model version
  date_start <- date_start %>% 
    as.numeric %>%
    as.Date(., origin = "1970-01-01") %>% 
    as.character()
  
  gen_country_basics(country = cn,
                     waning_nat = wane[1]*7,
                     R0_assumed  = R ,
                     # infection introduction date
                     date_start = date_start ,
                     date_end = date_end,
                     # size of seasonal component
                     s_A = S_A,
                     deterministic = TRUE) %>% 
    update_vac_char(para = .,
                    waning_vac = wane[2]*365,
                    ve = eff) -> params_baseline
  
  if(type_ms == "Customised"){
    ms_date <- ms_date %>% 
      .[!is.na(.)] %>% 
      as.numeric %>%
      as.Date(., origin = "1970-01-01") %>% 
      as.character()
    
    ms_cov <- ms_cov %>% 
      .[!is.na(.)] %>% 
      as.numeric 
    
    priority_policy %>% 
      map(~vac_policy(para = params_baseline,
                      milestone_date = ms_date,
                      milestone_cov = ms_cov,
                      date_start = date_start,
                      priority = .,
                      cov_max = cov_tar)) -> params
  }
  
  if(type_ms == "Preload"){
    priority_policy %>% 
      map(~vac_policy_preload(para = params_baseline,
                              pattern_tmp = pattern, # linear, exponential, sigmoid
                              priority_tmp = .,
                              cov_max = cov_tar)) -> params
  }

  res <- dyna <- daily_vac <- vac_para <- list()
  for(i in seq_along(priority_policy)) {
    res[[i]] <- cm_simulate(params[[i]]$param)
    dyna[[i]] <- res[[i]]$dynamics
    vac_para[[i]] <- params[[i]]$vac_para
    daily_vac[[i]] <- params[[i]]$daily_vac
  }
  res_baseline <- cm_simulate(params_baseline)
   
  map2(params,
       dyna,
       function(x, y){
         gen_econ_app(para = x,
                      dyna = y,
                      dyna_baseline = res_baseline$dynamics,
                      cn_tmp = cn)
       }
  ) %>% 
    bind_rows(.id = "policy") %>% 
    data.table() -> econ
  
  size <- params[[1]]$param$pop[[1]]$size
  
  daily_vac %<>% 
    bind_rows(.id = "policy")
    
  vac_para %<>% bind_rows(.id = "policy")

  data.table(res_baseline$dynamics) %>% 
    mutate(policy = "0") %>% 
    bind_rows(bind_rows(dyna, .id = "policy")) %>% 
    group_by(compartment, t, policy, population, run) %>% 
    summarise(value = sum(value), .groups = "drop") %>% 
    pivot_wider(names_from = compartment,
                 values_from = value) %>% 
    mutate(date = lubridate::ymd(date_start) + t) %>% 
    full_join(daily_vac,
              by = c("policy",
                     "date")) -> main
  
  r <- list(main = main,
            supply = params[[1]]$supply,
            vac_para = vac_para,
            econ = econ,
            date_start = date_start,
            size = size)

  return(r)
}