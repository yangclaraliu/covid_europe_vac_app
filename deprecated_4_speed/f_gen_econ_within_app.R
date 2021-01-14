gen_econ_app <- function(para, 
                         dyna,
                         dyna_baseline,
                         cn_tmp){
  
  dyna %>% 
    mutate(policy = "current") %>% 
    bind_rows(dyna_baseline %>% 
                mutate(policy = "baseline")) %>% 
    filter(compartment == "death_o") %>% 
    group_by(policy, group) %>% 
    summarise(value = sum(value),
              .groups = "drop") %>% 
    mutate(wb = countrycode(cn_tmp, "country.name", "wb")) %>% 
    left_join(LE_estimates, by = c("wb", "group")) %>% 
    pivot_wider(names_from = policy,
                values_from = value) %>% 
    mutate(current_LE = (baseline - current) * LE,
           current_adjLE = (baseline - current) * adjLE,
           current_adjQALEdisc = (baseline - current) * adjQALEdisc) %>% 
    group_by(wb) %>% 
    summarise_at(vars(starts_with(c("current_"))), sum) -> impact
  
  para$supply  %>% 
    pull(doses) %>% 
    max(., na.rm = T) %>% 
    enframe() %>%
    rename(wb = name) %>% 
    mutate(wb = unique(impact$wb)) %>% 
    left_join(impact, by = "wb") %>% 
    mutate_at(vars(starts_with(c("current_"))), ~./value) %>% 
    dplyr::select(-value) %>% 
    setNames(c("wb",
               "current_LE_pd",
               "current_adjLE_pd",
               "current_adjQALEdisc_pd"))-> impact_pd
  
  dyna %>% 
    mutate(policy = "current") %>% 
    bind_rows(dyna_baseline %>% 
                mutate(policy = "baseline")) %>% 
    filter(compartment == "death_o") %>% 
    group_by(policy, group) %>% 
    summarise(value = sum(value),
              .groups = "drop") %>% 
    mutate(wb = countrycode(cn_tmp, "country.name", "wb")) %>% 
    left_join(df_VSL, by = c("wb")) %>% 
    pivot_wider(names_from = policy,
                values_from = value) %>% 
    mutate(current_VSLmlns = (baseline - current) * VSL_mlns) %>% 
    group_by(wb) %>% 
    summarise_at(vars(starts_with(c("current_"))), sum) -> impact_VSL
  
  para[["supply"]]  %>% 
    pull(doses) %>% 
    max(., na.rm = T) %>% 
    enframe() %>%
    rename(wb = name) %>% 
    mutate(wb = unique(impact$wb)) %>% 
    left_join(impact_VSL, by = "wb") %>% 
    mutate_at(vars(starts_with(c("current_"))), ~./value) %>% 
    dplyr::select(-value) %>% 
    setNames(c("wb",
               "current_VSLmlns_pd"))-> impact_VSL_pd
  
  dyna %>% 
    mutate(policy = "current") %>% 
    bind_rows(dyna_baseline %>% 
                mutate(policy = "baseline")) %>% 
    filter(compartment == "cases") %>% 
    group_by(policy, population, group) %>% 
    summarise(value = sum(value),
              .groups = "drop") %>% 
    mutate(wb = impact$wb) %>% 
    pivot_wider(names_from = policy,
                values_from = value) %>% 
    mutate(current_QALYcases = (baseline - current) * 0.0307) %>% 
    group_by(wb) %>% 
    summarise_at(vars(starts_with(c("current_"))), sum) -> impact_QALYmorb
  
  para[["supply"]]  %>% 
    pull(doses) %>% 
    max(., na.rm = T) %>% 
    enframe() %>%
    rename(wb = name) %>% 
    mutate(wb = unique(impact$wb)) %>% 
    left_join(impact, by = "wb") %>% 
    dplyr::select(-current_LE, -current_adjLE) %>%
    # add QALY loss from AEFI (assumed at 1 day with 10% chance) 
    mutate_at(vars(starts_with(c("current_"))), ~. + (1/365.25*0.1)*value) %>% 
    # add QALY loss from cases
    left_join(impact_QALYmorb, by = "wb") %>% 
    transmute(value = value,
              wb = wb,
              current_QALYloss = current_adjQALEdisc + current_QALYcases) -> impact_QALYtotal
  
  impact_QALYtotal %>% 
    mutate_at(vars(starts_with(c("current_"))), ~./value) %>% 
    dplyr::select(-value) %>% 
    setNames(c("wb",
               "current_QALYloss_pd")) -> impact_QALYtotal_pd
  
  impact_QALYtotal <- impact_QALYtotal %>% 
    dplyr::select(-value)
  
  r <- list(impact,
              impact_pd,
              impact_VSL,
              impact_VSL_pd,
              impact_QALYmorb,
              impact_QALYtotal,
              impact_QALYtotal_pd) %>% 
    bind_rows() %>% 
    pivot_longer(cols = starts_with("current")) %>% 
    filter(!is.na(value))

  return(r)
}
