library(sf)
library(spdep)

# load geography
suppressWarnings(
  members_shp <- st_read("data/CNTR_RG_60M_2020_4326.shp", quiet = T) %>%
    mutate(wb = countrycode(NAME_ENGL, "country.name", "wb")) %>%
    filter(wb %in% members$wb)
)


read_csv("data/gs_fitted.csv") -> members
nb <- poly2nb(members_shp, "B")
members_shp[c(10, 14, 27, 42),]
# Iceland is the only country that needs imputation
nb[[10]] <- which(members_shp$NAME_ENGL %in% c("United Kingdom", "Ireland", "Norway"))
nb[[38]]

which(members$Fit == F) %>% 
  members[.,] %>% 
  left_join(data.table(country_name = members_shp$NAME_ENGL) %>% 
              mutate(wb = countrycode(country_name, "country.name", "wb")) %>% 
              rownames_to_column(var = "index_shp"),
            by = "wb") %>% 
  pull(index_shp) %>% as.numeric %>% 
  map(~nb[[.]]) %>% 
  map(~members_shp[.,]) %>% 
  bind_rows(.id = "missing") %>%
  data.table %>% dplyr::select(missing, wb) %>% 
  left_join(members, by = "wb") %>% 
  group_by(missing) %>% 
  summarise(t_intro = min(t_intro, na.rm = T),
            R0 = mean(R0, na.rm = T)) -> to_fill

to_fill %<>% 
  mutate(t_intro = if_else(is.infinite(t_intro),
                           round(median(members$t_intro, na.rm = T)),
                           t_intro),
         R0 = if_else(is.nan(R0), 
                      median(members$R0, na.rm = T),
                      R0))

missing_all <- which(members$Fit == F)

for(i in seq(missing_all)) members[missing_all[i],c("t_intro", "R0")] <- to_fill[i, c("t_intro", "R0")]

write_csv(members, "gs_fitted_imputed.csv")
