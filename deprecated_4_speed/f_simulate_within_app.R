predict_deriv <- function(
                          cn, # country name
                          # S_A, # size of seasonal component
                          cov_tar = 0.8, # target coverage  
                          ms_date = NULL, # milestone - dates
                          ms_cov = NULL, # milestones - coverage levels
                          priority = NULL, # priority settings
                          date_start = NULL, # starting date of uncontrollable community transmission
                          date_end = "2022-12-31",
                          eff = NULL,#c(rep(0.9,10),rep(0.8,6)),
                          wane = NULL, # natural waning; vaccine waning
                          R = NULL
                          ){
  
  # debugging
  # cn = "Belgium"                  # country name
  # S_A = 0.2                       # size of seasonal component
  # cov_tar = 0.8                   # target coverage
  # ms_date <- c("2021-01-01",
  #              "2021-06-30",
  #              "2021-12-31",
  #              "2022-12-31")      # milestones for dates
  # ms_cov <- c(0, 0.03, 0.2, 0.6)  # milestones for coverage
  # priority = "Strategy 2"         # priority settings
  # date_start = "2020-02-15"       # starting date of uncontrollable community transmission
  # date_end = "2022-12-31"
  # eff = c(rep(0.9,10),
  #         rep(0.8,6))             # vaccine efficacy
  # wane = c(45, 52)
  # R = 2.7
  
  # fcontent
  wb <- countrycode::countrycode(cn, "country.name", "wb")
  S_A <- model_selected %>% filter(WB == wb) %>% pull(S_A)
  # debugging version
  # date_start <- model_selected %>% filter(WB == wb) %>% pull(start_date) %>% as.Date
  # model version
  date_start <- as.numeric(date_start) %>%
    as.Date(., origin = "1970-01-01") %>%
    as.character()
  
  
  # do not run during debug
  ms_date <- ms_date %>% 
    .[!is.na(.)] %>% 
    as.numeric %>%
    as.Date(., origin = "1970-01-01") %>% 
    as.character()

  
  #
  ms_cov <- ms_cov %>% 
    .[!is.na(.)] %>% 
    as.numeric 
  
  gen_country_basics(country = cn,
                     waning_nat = wane[1]*7,
                     R0_assumed  = R,
                     # infection introduction date
                     date_start = date_start,
                     date_end = date_end,
                     # size of seasonal component
                     s_A = S_A,
                     deterministic = TRUE) %>% 
    update_vac_char(para = .,
                    waning_vac = wane[2]*7,
                    ve = eff) -> params_baseline
  
  priority_policy %>% 
    map(~vac_policy(para = params_baseline,
                    milestone_date = ms_date,
                    milestone_cov = ms_cov,
                    priority = .,
                    cov_max = cov_tar)) -> params

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
  
  daily_vac %<>% bind_rows(.id = "policy")
  vac_para %<>% bind_rows(.id = "policy")
  
  r <- list(dynamics_baseline = data.table(res_baseline$dynamics),
            dynamics = bind_rows(dyna, .id = "policy"),
            supply = params[[1]]$supply,
            vac_para = vac_para,
            daily_vac = daily_vac,
            econ = econ,
            size = size)

  return(r)
  
}
