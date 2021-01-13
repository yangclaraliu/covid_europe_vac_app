predict_deriv <- function(
                          cn, # country name
                          # S_A, # size of seasonal component
                          cov_tar = 0.8, # target coverage  
                          ms_date = NULL, # milestone - dates
                          ms_cov = NULL, # milestones - coverage levels
                          priority = NULL, # priority settings
                          # date_start, # starting date of uncontrollable community transmission
                          date_end = "2022-12-31",
                          eff = c(rep(0.9,10),rep(0.8,6)),
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
  date_start <- model_selected %>% filter(WB == wb) %>% pull(start_date) %>% as.Date
  
  ms_date <- ms_date %>% 
    .[!is.na(.)] %>% 
    as.numeric %>%
    as.Date(., origin = "1970-01-01") %>% 
    as.character()

  ms_cov <- ms_cov %>% 
    .[!is.na(.)] %>% 
    as.numeric 
  
  ps <- case_when(
    # Strategy 1: All Adults
    priority == "Strategy 1" ~ c(rep(NA,4), rep(1,12)),
    # Strategy 2: All 60+, then all younger adults
    priority == "Strategy 2" ~ c(rep(NA,4), rep(2, 8), rep(1, 4)),
    # Strategy 3: All younger adults, then all elderly
    priority == "Strategy 3" ~ c(rep(NA,4), rep(1, 8), rep(2, 4)),
    # Strategy 4: All the oldest to the youngest adults
    priority == "Strategy 4" ~ c(rep(NA,4), rep(5, 8), 4, 3, 2, 1))
  
  gen_country_basics(country = cn,
                     waning_nat = wane[1]*7,
                     R0_assumed  = R,
                     # infection introduction date
                     date_start = date_start,
                     date_end = date_end,
                     # size of seasonal component
                     s_A = S_A,
                     deterministic = TRUE) -> params_baseline
  
  params_baseline %>% 
    update_vac_char(para = .,
                    waning_vac = wane[2]*7,
                    ve = eff)  %>%
    vac_policy(para = .,
               # milestone dates
               milestone_date = ms_date,
               # milestone coverage levels
               milestone_cov = ms_cov,# as.numeric(as.vector(ms[,2])),
               # priority settings
               priority = ps,#as.vector(ps),
               cov_max = cov_tar) -> params

  res <- cm_simulate(params$param)
  res_baseline <- cm_simulate(params_baseline)

  econ <- gen_econ_app(para = params, 
               dyna = res$dynamics,
               dyna_baseline = res_baseline$dynamics,
               cn_tmp = cn)
  
  size <- params$param$pop[[1]]$size
  
  r <- list(dynamics_baseline = data.table(res_baseline$dynamics),
            dynamics = data.table(res$dynamics),
            supply = params$supply,
            vac_para = params$vac_para,
            daily_vac = params$daily_vac,
            econ = econ,
            size = size)

  return(r)
  
}
