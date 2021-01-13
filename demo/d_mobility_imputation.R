require("mgcv")

gm %>% 
  melt(.,
       id.vars = c("country_name", "wb", "date"),
       measures.var = gm_type) %>% 
  .[, c("m",
        "dow",
        "doy",
        "d",
        "variable",
        "value") := list(factor(month(date), levels = 1:12),
                         factor(lubridate::wday(date)),
                         lubridate::yday(date),
                         as.numeric(date),
                         factor(variable),
                         (value + 100)/100)] %>% 
  left_join(., si[,list(d,wb,StringencyIndex)], 
            by = c("wb", "d")) %>% 
  filter(date < si_stopdate) %>% 
  filter(variable %in% c("retail",
                         "transit",
                         "grocery",
                         "work")) %>% 
  mutate(wb = factor(wb)) -> tab

fit <- gam(formula = value ~  dow + s(wb, bs = "re") + variable + 
             variable*dow + StringencyIndex + variable*m, 
           data = tab, 
           na.action = na.omit)

CJ(date = seq(range(tab$date)[1],
              as.Date(sim_end),
              by = 1),
   wb = unique(tab$wb),
   variable = c("retail",
                "transit",
                "grocery",
                "work")) %>% 
  .[, c("dow",
        "doy",
        "m",
        "d") := list(lubridate::wday(date) %>% factor,
                     lubridate::yday(date),
                     month(date),
                     as.numeric(date))]  %>% 
  .[, m := if_else(m == 12, 11, m)] %>%
  .[, m := if_else(m == 1, 2, m)] %>%
  .[, m := factor(m)] %>%
  left_join(si_imputed[,c("wb","d", "StringencyIndex","status")],
            by = c("wb", "d")) %>%
  split(by = "variable") %>% 
  map(arrange, date) %>% 
  bind_rows() %>% 
  mutate(d = if_else(d >= max(tab$d), max(tab$d), d),
         country_missing = if_else(is.na(status), T, F)) -> pre_tab

val <- predict(fit, pre_tab)
pre_tab %<>% mutate(predicted = val)
pre_tab %>% 
  left_join(tab[,c("wb", "date", "variable","value")],
            by = c("wb","date", "variable")) %>% 
  mutate(imputed = if_else(is.na(value),
                           predicted,
                           value)) -> gm_forecast

# imputation for countries with no data
members_neighbors <- spdep::poly2nb(members_shp)
members_missing <- which(!members$wb %in% gm_forecast$wb) %>% sort

to_merge <- list()
for(i in 1:length(members_missing)){
  gm_forecast %>% 
    filter(wb %in% members$wb[members_neighbors[[members_missing[i]]]]) %>% 
    group_by(variable, dow, doy, m, date) %>% 
    summarise(imputed = mean(imputed, na.rm = T),
              .groups = "drop") %>% 
    mutate(predicted = NA,
           value = NA,
           wb = members$wb[members_missing[i]],
           status = "averaged",
           country_missing = T,
           StringencyIndex = NA,
           d = as.numeric(date)) %>% 
    dplyr::select(colnames(gm_forecast)) -> to_merge[[i]]
}

to_merge %>% 
  bind_rows() %>% 
  bind_rows(gm_forecast) -> gm_forecast

# check for completness 
gm_forecast %>% 
  filter(is.na(imputed)) %>% 
  ungroup %>% 
  # group_by(wb, variable) %>% 
  dplyr::select(wb) %>%
  distinct() %>% 
  left_join(members, by = "wb") %>% 
  pull(country_index) %>% 
  sort -> members_missing

members_missing <- c(members_missing, 
                     which(!(members$wb %in% unique(gm_forecast$wb)))) %>% 
  unique %>% sort

to_merge <- list()
for(i in 1:length(members_missing)){
  gm_forecast %>% 
    filter(wb %in% members$wb[members_neighbors[[members_missing[i]]]]) %>% 
    group_by(variable, dow, doy, m, date) %>% 
    summarise(imputed = mean(imputed, na.rm = T),
              .groups = "drop") %>% 
    mutate(predicted = NA,
           value = NA,
           status = "averaged",
           wb = members$wb[members_missing[i]],
           StringencyIndex = NA,
           country_missing = TRUE,
           d = as.numeric(date)) %>% 
    dplyr::select(colnames(gm_forecast)) -> to_merge[[i]]
}

to_merge %>% 
  bind_rows() %>%
  # group_by(date, wb, variable) %>% tally %>% filter(n != 1)
  bind_rows(gm_forecast) %>% 
  # dplyr::select(-value, -predicted) %>% 
  mutate(status = if_else(is.na(status), "empirical", status)) -> gm_forecast

gm_forecast$status %>% table
status_col <- ggsci::pal_futurama()(4)
status_lvl <- unique(gm_forecast$status)

gm_forecast %>% 
  filter(wb %in% c("MLT","MKD")) %>% 
  arrange(date) %>% 
  distinct() %>% 
  dplyr::select(-value, -predicted) %>% 
  pivot_wider(names_from = status, 
              values_from = imputed) %>% 
  mutate(averaged = if_else(!is.na(empirical), as.numeric(NA), averaged)) %>% 
  pivot_longer(cols = c(averaged, empirical)) %>% 
  filter(!is.na(value)) %>% 
  mutate(imputed = value) %>% 
  bind_rows(gm_forecast %>% 
              filter(!wb %in% c("MLT","MKD"))) %>% 
  distinct -> gm_forecast

# gm_forecast %>%
#   group_by(date, wb) %>%
#   tally() %>%
#   filter(n != 4)

# # plot everything
# gm_forecast %>%
#   arrange(date) %>% 
#   ggplot(., aes(x = date,
#                 y = imputed,
#                 color = interaction(status, variable))) +
#   geom_point() +
#   facet_wrap(~wb)

# get projected travel based on gm data
# get_projected_travel_gm <- function(iso){
#   gm_forecast %>% 
#     mutate(status = factor(status)) %>% 
#     filter(wb == iso) %>% 
#     ggplot(., aes(x = date,
#                   color = status,
#                   group = variable)) +
#     geom_point(aes(y = imputed),
#               alpha = 0.5) +
#     facet_wrap(~variable) +
#     theme_bw() +
#     scale_color_manual(breaks = status_lvl, values = status_col, drop = F) + 
#     theme(axis.text.x = element_text(angle = 90)) +
#     labs(title = countrycode(iso, "wb","country.name")) -> p_tmp 
#   
#   ggsave(plot = p_tmp,
#          filename = paste0("figs/imputed_mobility/imputed_mobility_",iso,".png"))
# }

# members$wb %>%
#   map(get_projected_travel_gm)

gm_forecast %<>% 
  data.table() %>% 
  dplyr::select(date, wb, variable, status, imputed) %>% 
  distinct() %>% 
  pivot_wider(names_from = variable,
              values_from = imputed) %>% 
  arrange(date, wb)

gm_forecast %>% 
  mutate(work = if_else(work > 1.25, 1.25, work),
         othx = 0.345*retail + 0.445*transit + 0.21*grocery,
         othx = if_else(othx > 1.25, 1.25, othx),
         work = round(work, 2),
         othx = round(othx, 2)) %>% 
  left_join(curves[,c("perc","work_scaler")], by = c("work" = "perc")) %>% 
  left_join(curves[,c("perc", "other_scaler")], by = c("othx" = "perc")) %>% 
  dplyr::select(-c(grocery, retail, transit, work, othx)) %>% 
  rename(work = work_scaler,
         other = other_scaler) -> gm_scaled

schedule_raw <- gm_scaled %>% 
  mutate(home = 1,
         date = as.character(date)) %>% 
  left_join(oxcgrt %>% 
              
              mutate(C1 = if_else(is.na(C1), 1, C1),
                     Date = as.character(Date)) %>% 
              dplyr::select(Date, C1, wb) %>% 
              setNames(c("date", "school", "wb")), # %>% 
            # mutate(school = case_when(school == 0 ~ 1,
            #                           is.na(school) ~ 1,
            #                           school == 3 ~ 0,
            #                           TRUE ~ 0.5),
            by = c("date", "wb"))  %>% 
  mutate(school = if_else(is.na(school), 0, school)) %>% 
  mutate(school = case_when(school == 0 ~ 1,
                            school == 3 ~ 0,
                            TRUE ~ 0.5))

# mutate(school = if_else(is.na(school), 0, school)) 

CJ(date = seq(as.Date("2019-12-01"), as.Date("2020-02-14"),1),
   wb = members$wb) %>% 
  .[,status := "assumed"] %>% 
  .[,c("work",
       "other",
       "home",
       "school",
       "date") := 
      list(1,1,1,1,
           as.character(date))] -> schedule_pre

# school holidays
schedule_raw %<>%
  bind_rows(schedule_pre) %>% 
  arrange(date) %>% 
  mutate(date = lubridate::ymd(date),
         month = lubridate::month(date),
         day = lubridate::day(date),
         year = lubridate::year(date)) %>% 
  mutate(holiday = if_else(
    #winter holiday, 
    (year > 2020 & month == 12 & day >=  15) |
      (year > 2020 & month == 1 & day < 5) |
      # summer holiday
      month %in% c(7,8),
    T,
    F),
    school = if_else(holiday, 0, school)) %>% 
  dplyr::select(-holiday)

# schedule_raw %>% 
#   ggplot(., aes(x = date,
#                 y = school,
#                 group = wb)) +
#   geom_line() +
#   facet_wrap(~wb)

# schedule_raw %>% 
# filter(is.na(home))
# filter(is.na(school))
# filter(is.na(other))
# filter(is.na(work))

