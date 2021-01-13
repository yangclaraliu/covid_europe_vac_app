gen_econ <- function(params, dyna, cn_tmp){
  dyna[compartment == "death_o"] %>% 
    group_by(policy, group) %>% 
    summarise(value = sum(value)) %>% 
    mutate(wb = countrycode(cn_tmp, "country.name", "wb")) %>% 
    left_join(LE_estimates, by = c("wb", "group")) %>% 
    pivot_wider(names_from = policy,
                values_from = value) %>% 
    mutate(p1_LE = (p0 - p1) * LE,
           p1_adjLE = (p0 - p1) * adjLE,
           p1_adjQALEdisc = (p0 - p1) * adjQALEdisc) %>% 
    group_by(wb) %>% 
    summarise_at(vars(starts_with(c("p1_"))), sum) -> impact
  
  params[[3]]  %>% 
    pull(doses) %>% 
    max(., na.rm = T) %>% 
    enframe() %>%
    rename(wb = name) %>% 
    mutate(wb = unique(impact$wb)) %>% 
    left_join(impact, by = "wb") %>% 
    mutate_at(vars(starts_with(c("p1_"))), ~./value) %>% 
    dplyr::select(-value) -> impact_pd
  
  dyna[compartment == "death_o"] %>% 
    group_by(policy, group) %>% 
    summarise(value = sum(value)) %>% 
    mutate(wb = countrycode(cn_tmp, "country.name", "wb")) %>% 
    left_join(df_VSL, by = c("wb")) %>% 
    pivot_wider(names_from = policy,
                values_from = value) %>% 
    mutate(p1_VSLmlns = (p0 - p1) * VSL_mlns) %>% 
    group_by(wb) %>% 
    summarise_at(vars(starts_with(c("p1_"))), sum) -> impact_VSL
  
  params[[3]]  %>% 
    pull(doses) %>% 
    max(., na.rm = T) %>% 
    enframe() %>%
    rename(wb = name) %>% 
    mutate(wb = unique(impact$wb)) %>% 
    left_join(impact_VSL, by = "wb") %>% 
    mutate_at(vars(starts_with(c("p1_"))), ~./value) %>% 
    dplyr::select(-value) -> impact_VSL_pd
  
  dyna[compartment == "cases"] %>% 
    group_by(policy, population, group) %>% 
    summarise(value = sum(value)) %>% 
    mutate(wb = impact$wb) %>% 
    pivot_wider(names_from = policy,
                values_from = value) %>% 
    mutate(p1_QALYcases = (p0 - p1) * 0.0307) %>% 
    group_by(wb) %>% 
    summarise_at(vars(starts_with(c("p1_"))), sum) -> impact_QALYmorb
  
  params[[3]]  %>% 
    pull(doses) %>% 
    max(., na.rm = T) %>% 
    enframe() %>%
    rename(wb = name) %>% 
    mutate(wb = unique(impact$wb)) %>% 
    left_join(impact, by = "wb") %>% 
    dplyr::select(-p1_LE, -p1_adjLE) %>%
    # add QALY loss from AEFI (assumed at 1 day with 10% chance) 
    mutate_at(vars(starts_with(c("p1_"))), ~. + (1/365.25*0.1)*value ) %>% 
    # add QALY loss from cases
    left_join(impact_QALYmorb, by = "wb") %>% 
    transmute(value = value,
              wb = wb,
              p1_QALYloss = p1_adjQALEdisc + p1_QALYcases) -> impact_QALYtotal
  
  impact_QALYtotal %>% 
    mutate_at(vars(starts_with(c("p1_"))), ~./value) %>% 
    dplyr::select(-value) -> impact_QALYtotal_pd
  
  impact_QALYtotal <- impact_QALYtotal %>% 
    dplyr::select(-value)
  
  res <- list(impact,
              impact_pd,
              impact_VSL,
              impact_VSL_pd,
              impact_QALYmorb,
              impact_QALYtotal,
              impact_QALYtotal_pd)
  
  return(res)
}
