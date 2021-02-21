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
                          ve_i = NULL, # VE against infection
                          ve_d = NULL, # VE against disease
                          # eff = NULL,#c(rep(0.9,10),rep(0.8,6)),
                          wane = NULL, # natural waning; vaccine waning
                          R = NULL
                          ){
  
  # # debugging
  # cn = "Albania"                  # country name
  # cov_tar = 0.8                   # target coverage
  # ms_date <- c("2021-03-01",
  #              "2021-06-30",
  #              "2022-12-31")      # milestones for dates
  # ms_cov <- c(0, 0.5, 0.6)  # milestones for coverage
  # date_start = "2020-02-15"       # starting date of uncontrollable community transmission
  # date_end = "2022-12-31"
  # ve_i = 0
  # ve_d = 0
  # wane = c(45, 1)
  # R = 2.7
  # pattern = "linear"
  # type_ms = "Preload"
  # #

  wb <- countrycode::countrycode(cn, "country.name", "wb")
  pattern <-   case_when(pattern_label == "Linear Increase" ~ "linear",
                         pattern_label == "Exponential Increase" ~ "exponential",
                         pattern_label == "Sigmoid Increase" ~ "sigmoid")
  # S_A <- model_selected %>% filter(WB == wb) %>% pull(S_A)
  # debugging version
  # date_start <- model_selected %>% filter(WB == wb) %>% pull(start_date) %>% as.Date
  # model version
  # date_start <- date_start %>% 
  #   as.numeric %>%
  #   as.Date(., origin = "1970-01-01") %>% 
  #   as.character()
  
  gen_country_basics(country = cn,
                     waning_nat = wane[1]*7,
                     R0_assumed  = R ,
                     # infection introduction date
                     date_start = date_start,
                     date_end = date_end,
                     # size of seasonal component
                     # s_A = S_A,
                     deterministic = TRUE) %>% 
    update_vac_char(para = .,
                    waning_vac = wane[2]*365,
                    ve_i = ve_i,
                    ve_d = ve_d)  -> params_baseline
  
  if(type_ms == "Customised"){
    priority_policy %>% 
      map(~vac_policy(para = params_baseline,
                      milestone_date = ms_date,
                      milestone_cov = ms_cov,
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
  
  # identify the starting time of vaccination programs
  lapply(seq_along(params), function(x) params[[x]]$param$schedule$vaccination$times) %>% 
    map(~.[.!=0]) %>% 
    map(min) %>% 
    unlist %>% 
    unique %>% 
    "+"(lubridate::ymd(params[[1]]$param$date0)) %>% 
    enframe(value = "date_start_vac", name = "id") %>% 
    mutate(m1 = date_start_vac %m+% months(1),
           m4 = date_start_vac %m+% months(4),
           m7 = date_start_vac %m+% months(7)) -> date_start_vac
  
  # res <- dyna <- daily_vac <- vac_para <- list()
  res <- lapply(seq_along(priority_policy), function(x) cm_simulate(params[[x]]$param))
  res_baseline <- cm_simulate(params_baseline)
  dyna <- lapply(res, "[[", "dynamics")
 #  vac_para <- lapply(params, "[[", "vac_para")
  daily_vac <-  lapply(params, "[[", "daily_vac") %>% bind_rows(.id = "policy") %>% 
    mutate(policy = parse_number(policy) %>% as.character())

  # Aggregate health economics output  
  dyna %>% 
    bind_rows(.id = "policy") %>% 
    bind_rows(res_baseline$dynamics %>% mutate(policy = "0")) %>% 
    filter(compartment %in% c("death_o", "cases"))  %>%
    dcast(., policy + run + population + group + t ~ compartment) %>% 
    mutate(wb = countrycode::countrycode(population, "country.name", "wb"),
           baseline = if_else(policy == "0", "baseline", "current"),
           date = lubridate::ymd(params$p1$param$date0) + as.numeric(t)) %>% 
    left_join(LE_estimates, by = c("wb", "group")) %>% 
    left_join(df_VSL, by = "wb") %>% 
    mutate_at(vars(c("LE", "adjLE", "adjQALEdisc", "VSL_mlns")), ~.*death_o) %>% 
    rename(VSLmlns = VSL_mlns) %>% 
    mutate(QALYcases = cases*0.0307,
           QALYloss = adjQALEdisc + QALYcases) %>% 
    filter(date >= date_start_vac$date_start_vac) -> econ_full 
  
  date_start_vac[,c("m1","m4","m7")] %>% 
    mutate_all(as.character) %>% unlist %>% as.vector %>% c(., date_end) %>% sort %>% 
    map(~econ_full[econ_full$date <= .,]) %>% 
    bind_rows(.id = "w") %>% 
    mutate(w = factor(w, levels = 1:4,
                      labels = c("1 month",
                                 "4 months",
                                 "7 months",
                                 "Before 2023"))) %>%
    .[, lapply(.SD, sum, na.rm = T), 
      by = c("policy", "run", "population", "w"),
      .SDcols = c("cases", "death_o",
                  "LE", "adjLE", "adjQALEdisc", 
                  "VSLmlns", "QALYcases","QALYloss")] -> econ_w
  
  date_start_vac[,c("m1","m4","m7")] %>% 
    mutate_all(as.character) %>% unlist %>% as.vector %>% c(., date_end) %>% sort %>% 
    map(~daily_vac[daily_vac$date <= .,]) %>% 
    map(group_by, policy) %>% 
    map(summarise, doses =  max(supply)) %>% 
    bind_rows(.id = "w") %>% 
    mutate(w = factor(w, levels = 1:4,
                      labels = c("1 month",
                                 "4 months",
                                 "7 months",
                                 "Before 2023"))) %>% 
    data.table() -> doses_w
  
  econ_w %>% 
    left_join(doses_w,
              by = c("policy", "w")) %>%
    group_by(w) %>% group_split() %>% 
    map(mutate, 
        LE_pd = (max(LE) - LE)/doses,
        adjLE_pd = (max(adjLE) - adjLE)/doses,
        adjQALEdisc_pd = (max(adjQALEdisc) - adjQALEdisc)/doses,
        VSLmlns_pd = (max(VSLmlns) - VSLmlns)/doses,
        QALYloss_pd = (max(QALYloss) - QALYloss)/doses) %>% 
    bind_rows() %>% 
    reshape2::melt(., id.var = c("policy", "run", "population", "w")) %>% 
    mutate(value = if_else(is.infinite(value), as.numeric(NA), value)) %>% 
    data.table() -> econ
  
  # map2(params,
  #      dyna,
  #      function(x, y){
  #        gen_econ_app(para = x,
  #                     dyna = y,
  #                     dyna_baseline = res_baseline$dynamics,
  #                     cn_tmp = cn)
  #      }
  # ) %>% 
  #   bind_rows(.id = "policy") %>% 
  #   data.table() -> econ
  
  size <- params[[1]]$param$pop[[1]]$size
  # vac_para %<>% bind_rows(.id = "policy")

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
                     "date"))  -> main
  
  r <- list(main = main,
            supply = params[[1]]$supply,
            # vac_para = vac_para,
            econ = econ,
            date_start = date_start,
            date_start_vac = date_start_vac,
            size = size)

  return(r)
}
