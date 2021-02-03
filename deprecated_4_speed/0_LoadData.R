# loading packages
# proj_path <- "C:/Users/eideyliu/Documents/GitHub/covid_europe_vaccine"
# if(!require(pacman)) install.packages("pacman")
library(tidyverse)
library(sf)
library(countrycode) 
library(rnaturalearth)
library(magrittr)
library(data.table)
library(ggsflabel)
library(mgcv)
library(pspline)

#
scientific_10 <- function(x){
  scales::scientific_format()(x) %>%
    sub(pattern = "e\\+",
        x = .,
        replacement = "e") %>%
    sub(pattern = "e00", 
        x = ., 
        replacement = "") %>%
    gsub(pattern = "e",
         replacement = " %*% 10^",
         x = .) %>%
    sub(pattern = "1 %*% ",
        replacement = "",
        x = ., fixed = T) %>%
    parse(text = .)
}


# p_load(tidyverse, sf, countrycode, rnaturalearth, magrittr, data.table, ggsflabel,
#        mgcv, pspline)
# load data
# list of member states
"Albania, Andorra, Armenia, Austria, Azerbaijan, Belarus, Belgium, Bosnia and Herzegovina, Bulgaria, Croatia, Cyprus, Czech Republic, Denmark, Estonia, Finland,  France, Georgia, Germany, Greece, Hungary, Iceland, Ireland, Israel, Italy, Kazakhstan, Kyrgyzstan, Latvia, Lithuania, Luxembourg, Malta, Monaco, Montenegro, Netherlands, Norway, Poland, Portugal, Republic of Moldova, Romania, Russian Federation, San Marino, Serbia, Slovakia, Slovenia, Spain, Sweden, Switzerland, Tajikistan, North Macedonia, Turkey, Turkmenistan, Ukraine, United Kingdom of Great Britain, Uzbekistan" %>% 
  strsplit(., split = ",") %>% 
  .[[1]] %>% 
  trimws() %>% 
  enframe(value = "country_name",
          name = "country_index") %>% 
  mutate(wb = countrycode::countrycode(country_name, 
                                       origin = "country.name", 
                                       destination = "wb"))-> members

# load geography
suppressWarnings(
  members_shp <- st_read("CNTR_RG_60M_2020_4326.shp", quiet = T) %>%
    mutate(wb = countrycode(NAME_ENGL, "country.name", "wb")) %>%
    filter(wb %in% members$wb)
)

# st_centroid(members_shp) %>% 
#   as.data.frame() %>% 
#   mutate(geometry = as.character(geometry)) %>% 
#   separate(geometry, into = c("lon", "lat"), sep = ",") %>% 
#   mutate(lon = parse_number(lon),
#          lat = parse_number(lat))

#  age-dependent ratio of infection:cases (based on Davies et al, Nature paper)
cf <- c(0.2904047, 0.2904047, 0.2070468, 0.2070468, 0.2676134, 
        0.2676134, 0.3284704, 0.3284704, 0.3979398, 0.3979398,
        0.4863355, 0.4863355, 0.6306967, 0.6306967, 0.6906705, 0.6906705)

# susecptibility (based on Davies et al, Nature paper)
sus <- c(0.3956736, 0.3956736, 0.3815349, 0.3815349, 0.7859512,
         0.7859512, 0.8585759, 0.8585759, 0.7981468, 0.7981468,
         0.8166960, 0.8166960, 0.8784811, 0.8784811, 0.7383189, 0.7383189)

# load data needed from covidm
cm_path = "covidm_for_fitting/" # "~/GitHub/covidm_MTPs/covidm_for_fitting/"
cm_force_rebuild = F
cm_build_verbose = T
cm_version = 2
source(paste0(cm_path, "/R/covidm.R"))

# few fixes of country names
names(cm_matrices)[names(cm_matrices)=="TFYR of Macedonia"] <- "North Macedonia"
cm_populations$name <- fct_recode(cm_populations$name, "Czech Republic" = "Czechia")
cm_populations$name <- fct_recode(cm_populations$name, "United Kingdom of Great Britain" = "United Kingdom")

# proxy for contact matrices (currently unavailable ?)
cm_matrices[["Norway"]] <- cm_matrices$Denmark
cm_matrices[["Republic of Moldova"]] <- cm_matrices$Romania
cm_matrices[["Turkmenistan"]] <- cm_matrices$Uzbekistan
cm_matrices[["San Marino"]] <- cm_matrices$`Italy | Emilia-Romagna`

# fix population demography missing for micro states (so as to be able and claim modelling for all 53 Member States of WHO Europe)

# add demography of Andorra (from Eurostat, 2019; latest data: https://ec.europa.eu/eurostat/databrowser/view/DEMO_PJAN__custom_145232/default/table?lang=en)
pop_Andorra = cm_populations[name=="Italy",] %>%
  mutate(country_code = 020,
         name = "Andorra",
         f = c(1328, 1911, 1903, 1981, 1841, 2214, 2499, 3144, 3524, 3455, 
               3268, 2864, 2121, 1614, 1252, 953, 666, 512, 256, 70, 12)/1000,
         m = c(1391, 1912, 2124, 2096, 2164, 2356, 2563, 3125, 3511, 3628, 
               3524, 3062, 2322, 1719, 1416, 832, 524, 340, 137, 38, 5)/1000)

# add demography of Monacco (from Monaco Statistics (IMSEE), 2016; latest data: https://www.monacostatistics.mc/Population-and-employment/Population-census/Statistical-tables#eztoc4898551_1_1)
pop_Monaco = cm_populations[name=="Italy",] %>%
  mutate(country_code = 492,
         name = "Monaco",
         f = c(915, 915, 915, 651, 651, 889, 889, 1159, 1159, 1505,
               1505, 1335, 1335, 1277, 1277, 953, 776, 586, 277, 90, 10)/1000,
         m = c(938, 938, 938, 688, 688, 893, 893, 1046, 1046, 1516,
               1516, 1363, 1363, 1207, 1207, 856, 614, 364, 137, 28, 1)/1000)

# add demography of San Marino (from Eurostat, 2018; latest data: https://ec.europa.eu/eurostat/databrowser/view/DEMO_PJAN__custom_145232/default/table?lang=en)
pop_SanMarino = cm_populations[name=="Italy",] %>%
  mutate(country_code = 674,
         name = "San Marino",
         f = c(690, 860, 794, 827, 762, 818, 949, 1197, 1505, 1602, 
               1646, 1312, 1104, 935, 836, 647, 527, 398, 188, 61, 7)/1000,
         m = c(755, 884, 900, 914, 833, 820, 892, 1071, 1395, 1496,
               1532, 1288, 1005, 888, 773, 574, 412, 244, 92, 19, 1)/1000)

# add to cm_populations data and afterwards declutter environment
cm_populations = rbindlist(list(cm_populations, pop_Andorra, pop_Monaco, pop_SanMarino))
rm(pop_Andorra, pop_Monaco, pop_SanMarino)

# QALY estimates
load("LE_estimates.rda")
LE_estimates %<>% 
  mutate(wb = countrycode(country_name, "country.name", "wb")) %>% 
  rename(group = AgeGroup)
# Google mobility data
# download the data file from Google if it doesn't exist in your directory 
# if(!file.exists(paste0(proj_path, "/data/gm.csv"))){
#   download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
#                 paste0(proj_path, "/data/gm.csv"))
# }

# update the data file from google if the time difference is greater than a week
# if(as.numeric(abs(as.Date(file.info(paste0(proj_path, "/data/gm.csv"))$mtime) - 
#                   as.Date(Sys.time()))) > 7){
#   download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv",
#                 paste0(proj_path, "/data/gm.csv"))
# }

gm <- fread("gm.csv")
gm_type <- c("retail", "grocery", "parks",
             "transit", "work", "residential")
gm %<>% 
  .[sub_region_1 == "" & sub_region_2 == "" & metro_area == ""] %>% 
  .[, wb := countrycode::countrycode(country_region,
                                     "country.name",
                                     "wb")] %>% 
  .[wb %in% members$wb] %>% 
  .[,!c("sub_region_1", "sub_region_2", "metro_area", "iso_3166_2_code",
        "census_fips_code", "country_region_code")] %>% 
  setnames(., c("country_name", "date",
                gm_type, "wb"))

# mobility scalers
curves <- data.table(
  work_scaler = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                  0.008, 0.021, 0.033, 0.046, 0.058, 0.071, 0.083, 0.096, 0.108, 0.121, 0.133, 
                  0.146, 0.158, 0.171, 0.183, 0.196, 0.208, 0.221, 0.233, 0.246, 0.258, 0.271, 
                  0.283, 0.296, 0.308, 0.321, 0.334, 0.346, 0.359, 0.371, 0.384, 0.397, 0.41, 
                  0.422, 0.435, 0.448, 0.461, 0.474 , 0.487, 0.5, 0.513, 0.526, 0.539, 0.552, 
                  0.566, 0.579, 0.592, 0.606, 0.619, 0.633, 0.646, 0.66, 0.674, 0.687, 0.701, 
                  0.715, 0.729, 0.743, 0.757, 0.771, 0.785, 0.799, 0.813, 0.828, 0.842, 0.856, 
                  0.87, 0.885, 0.899, 0.914, 0.928, 0.942, 0.957, 0.971, 0.986, 1, 1.014, 1.029, 
                  1.043, 1.058, 1.072, 1.087, 1.101, 1.115, 1.13, 1.144, 1.159, 1.173, 1.188, 
                  1.202, 1.216, 1.231, 1.245, 1.26, 1.274, 1.289, 1.303, 1.317, 1.332, 1.346, 1.361),
  other_scaler = c(0.064, 0.066, 0.067, 0.068, 0.069, 0.071, 0.072, 0.073, 0.075, 0.076, 0.077, 0.078, 
                   0.08, 0.081, 0.082, 0.084, 0.085, 0.086, 0.087, 0.089, 0.09, 0.091, 0.092, 0.094, 
                   0.095, 0.096, 0.098, 0.099, 0.1, 0.101, 0.103, 0.104, 0.105, 0.106, 0.108, 0.109, 
                   0.11, 0.112, 0.113, 0.114, 0.116, 0.118, 0.119, 0.121, 0.123, 0.125, 0.128, 0.13, 
                   0.132, 0.135, 0.137, 0.14, 0.143, 0.146, 0.15, 0.154, 0.159, 0.164, 0.169, 0.175, 
                   0.182, 0.19, 0.198, 0.207, 0.217, 0.228, 0.24, 0.252, 0.266, 0.28, 0.295, 0.31, 
                   0.327, 0.344, 0.361, 0.379, 0.398, 0.418, 0.438, 0.459, 0.48, 0.502, 0.525, 0.549, 
                   0.572, 0.597, 0.621, 0.647, 0.672, 0.698, 0.725, 0.751, 0.778, 0.805, 0.833, 0.86, 
                   0.888, 0.916, 0.944, 0.972, 1, 1.028, 1.056, 1.084, 1.112, 1.14, 1.168, 1.196, 1.224, 
                   1.252, 1.28, 1.308, 1.337, 1.365, 1.393, 1.421, 1.449, 1.477, 1.505, 1.533, 1.561, 
                   1.589, 1.617, 1.645, 1.673, 1.701),
  perc = round(seq(0,1.25,0.01),2)) 

# OXCGRT
today <- Sys.Date()
# file_date <- format(file.info("oxcgrt.csv")$mtime, "%Y-%m-%d")
# if(abs(as.numeric(as.Date(file_date) - today)) > 7){
#   download.file("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv",
#                 paste0(proj_path, "/data/oxcgrt.csv"))
# }

oxcgrt <- read_csv("oxcgrt.csv",
                   col_types = cols(RegionName = col_character(), RegionCode = col_character())) %>%
  filter(is.na(RegionName)) %>% 
  dplyr::select(CountryName, Date, RegionName, `C1_School closing`)  %>%
  # filter(C1_Flag == 0) %>% 
  # group_by(CountryName, Date) %>% tally %>% arrange(desc(n))
  mutate( Date = lubridate::ymd(Date),
          wb = countrycode::countrycode(CountryName, 
                                        "country.name",
                                        "wb")) %>%
  filter( wb %in% members$wb) %>% 
  rename(C1 = `C1_School closing`) %>% 
  distinct() %>%
  # group_by(CountryName, Date, wb) %>% 
  # summarise(C1 = mean(`C1_School closing`)) %>% 
  pivot_wider(names_from = Date,
              values_from = C1) %>% 
  full_join(members[,"wb"], by = "wb") %>% 
  ungroup %>% 
  mutate(CountryName = if_else(is.na(CountryName),
                               countrycode::countrycode(wb, "wb", "country.name"),
                               CountryName)) %>% 
  pivot_longer(cols = starts_with("2020"),
               names_to = "Date",
               values_to = "C1") %>% 
  mutate(Date = lubridate::ymd(Date))

# stringency index
si <- read_csv("oxcgrt.csv",
               col_types = cols(RegionName = col_character(), RegionCode = col_character())) %>%
  filter(is.na(RegionName)) %>% 
  dplyr::select(CountryName, Date, StringencyIndex) %>% 
  mutate(date = lubridate::ymd(Date),
         wb = countrycode::countrycode(CountryName, 
                                       "country.name",
                                       "wb"),
         d = as.numeric(date)) %>% 
  dplyr::select(-Date,
                -CountryName) %>% 
  filter(wb %in% members$wb) %>% 
  distinct() %>% 
  as.data.table()

si %>% 
  filter(!is.na(StringencyIndex)) %>% 
  group_by(date) %>% 
  tally() %>% 
  mutate(n_max = max(n),
         missing = (n_max - n)/n_max) %>% 
  filter(missing > 0.1) %>% 
  pull(date) %>% 
  min -> si_stopdate

si %<>% 
  filter(date < si_stopdate) %>% 
  # filter(is.na(StringencyIndex))
  group_by(wb) %>%
  arrange(date) %>% 
  group_split() %>% 
  map(mutate, StringencyIndex = zoo::na.locf(StringencyIndex)) %>% 
  bind_rows()  %>%
  as.data.table()
# filter(is.na(StringencyIndex))

# si %<>% 

# some public health data as sanity checks
# download.file("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv",
#               paste0(proj_path, "/data/owid.csv"))

owid <- read.csv( "owid.csv") %>% 
  mutate(wb = countrycode(location, "country.name", "wb"))

# owid %>% 
#   right_join(members, by = c("wb")) %>% 
#   group_by(iso_code) %>%
#   group_split() %>% 
#   map(filter, !is.na(total_deaths)) %>% 
#   map(arrange, desc(total_deaths)) %>% 
#   map(~.[1,]) %>% 
#   bind_rows() %>% 
#   dplyr::select(iso_code, date, total_deaths) -> covid_death

# members_shp %>% 
#   left_join(covid_death, by = c("ISO3_CODE" = "iso_code")) %>% 
#   st_as_sf() %>% 
#   ggplot(.) +
#   geom_sf(aes(fill = log(total_deaths, base = 10)), color = "black") +
#   geom_sf_label_repel(aes(label = ISO3_CODE, 
#                           fill = log(total_deaths, base = 10)), 
#                       show.legend = F) +
#   lims(x = c(-30, 90),
#        y = c(30, 73)) +
#   theme_cowplot() +
#   theme(legend.position = "bottom") +
#   labs(title = "COVID-19 Deaths",
#        fill = "COVID-19 Deaths") +
#   viridis::scale_fill_viridis() -> p_tmp

# ggsave(plot = p_tmp,
#        filename = "figs/covid_deaths.pdf",
#        width = 15,
#        height = 10)

# owid %>% 
#   right_join(members, by = c("wb")) %>% 
#   dplyr::select(iso_code,
#                 country_name,
#                 date, 
#                 new_cases, 
#                 new_cases_smoothed) %>% 
#   group_by(iso_code) %>% 
#   group_split() %>% 
#   map(arrange, desc(new_cases)) %>% 
#   map(~mutate(.data = .,
#               rank_new_cases = 1:n())) %>% 
#   map(arrange, desc(new_cases_smoothed)) %>% 
#   map(~mutate(.data = .,
#               rank_new_cases_smoothed = 1:n())) %>% 
#   map(filter, rank_new_cases == 1 | rank_new_cases_smoothed == 1) %>% 
#   bind_rows() %>% 
#   mutate(raw_peak = if_else(rank_new_cases == 1,
#                             T,
#                             F),
#          smoothed_peak = if_else(rank_new_cases_smoothed == 1,
#                                  T,
#                                  F)) %>% 
#   dplyr::select(-new_cases,
#                 -new_cases_smoothed,
#                 -rank_new_cases,
#                 -rank_new_cases_smoothed)  -> covid_timing

# members_shp %>% 
#   left_join(covid_timing %>% filter(smoothed_peak == T), by = c("ISO3_CODE" = "iso_code")) %>% 
#   mutate(wk = date - as.Date("2020-01-01"),
#          wk = as.numeric(wk),
#          wk = round(wk/7),
#          wk = factor(wk, ordered = T)) %>% 
#   st_as_sf() %>% 
#   ggplot(.) +
#   geom_sf(aes(fill = wk), color = "black") +
#   geom_sf_label_repel(aes(label = ISO3_CODE, 
#                           fill = wk), 
#                       show.legend = F) +
#   lims(x = c(-30, 90),
#        y = c(30, 73)) +
#   theme_cowplot() +
#   theme(legend.position = "bottom") +
#   labs(title = "Peak Timing",
#        fill = "Peak timing since 2020-01-01") -> p_tmp
# 
# ggsave(plot = p_tmp,
#        filename = "figs/peak_time.pdf",
#        width = 15,
#        height = 10)

# owid %>% 
#   right_join(members, by = c("wb")) %>% 
#   dplyr::select(iso_code, country_name, date, new_cases) %>% 
#   filter(!is.na(new_cases) & new_cases != 0) %>% 
#   group_by(iso_code, country_name) %>% 
#   group_split() %>% 
#   map(arrange, date) %>% 
#   map(head, 1) %>% 
#   bind_rows() -> covid_intro
# 
# members_shp %>% 
#   left_join(covid_intro, by = c("ISO3_CODE" = "iso_code")) %>% 
#   mutate(wk = round(as.numeric(date - min(covid_intro$date))/7),
#          wk = factor(wk, ordered = T)) %>% 
#   st_as_sf() %>% 
#   ggplot(.) +
#   geom_sf(aes(fill = wk), color = "black") +
#   geom_sf_label_repel(aes(label = ISO3_CODE, 
#                           fill = wk), 
#                 show.legend = F) +
#   lims(x = c(-30, 90),
#        y = c(30, 73)) +
#   theme_cowplot() +
#   theme(legend.position = "bottom") +
#   labs(title = "Introduction Time",
#        fill = "Weeks since first introduction in the region") -> p_tmp
#   
# ggsave(plot = p_tmp,
#        filename = "figs/introduction_time.pdf",
#        width = 15,
#        height = 10)

# economic parameters
load("WB_GDP.rda")

######################
# add VSL estimate
######################
df_VSL <- read.delim("Viscusi-Masterman_2017_VSL-estimates-by-country.csv", 
                     sep=",", 
                     stringsAsFactors=FALSE, 
                     col.names = c("country_name", 
                                   "GNI_per_capita_tsds", "VSL_mlns"))


# proxy for Monaci and San Marino based on GDP ratio
df_VSL <- rbind(df_VSL, 
                data.frame("country_name" = "Monaco", 
                           "GNI_per_capita_tsds" = NA,
                           "VSL_mlns" = df_VSL[df_VSL$country_name  == "Italy", "VSL_mlns"] *
                             unlist(WB_GDP[WB_GDP$`Country Name`=="Monaco", "WTPvalue"])/
                             unlist(WB_GDP[WB_GDP$`Country Name`=="Italy", "WTPvalue"]))) 

df_VSL <- rbind(df_VSL, 
                data.frame("country_name" = "San Marino", 
                           "GNI_per_capita_tsds" = NA,
                           "VSL_mlns" = df_VSL[df_VSL$country_name  == "Italy", "VSL_mlns"] *
                             unlist(WB_GDP[WB_GDP$`Country Name`=="San Marino", "WTPvalue"])/
                             unlist(WB_GDP[WB_GDP$`Country Name`=="Italy", "WTPvalue"]))) %>% 
  mutate(wb = countrycode(country_name, "country.name", "wb"))

# model selected for first derivative fitting
model_selected <- read_rds("fit_derivative.rds") %>% 
  compact %>%
  map(mutate, tmp = min(S_A )) %>% 
  map(filter, tmp == S_A) %>% 
  bind_rows() %>% 
  dplyr::select(-tmp) %>% 
  mutate(WB = countrycode::countrycode(cn, "country.name","wb"))

# function for generating baseline parameter ombinations
source("f_gen_country_basics.R")
# function for updating the baseline parameter set with vaccine characteristics
source("f_update_vac_char.R")
# function for updating the baseline parameter set with vaccine policy 
# characteristics
source("f_vac_policy.R")
source("f_vac_policy_preload.R")
# load all utility functions
source("f_utils.R")
# load healthcare system data
source("d_healthcare_parameters2.R")
# generate health economics results
source("f_gen_econ.R")

# different priority settings
# priority_settings <- list(p1 = c(NA, NA, NA, NA,
#                                  5,  5,  5,  5,
#                                  5,  5,  5,  5,
#                                  4,  3,  2,  1),
#                           p2 = c(NA, NA, NA, NA,
#                                  2,  2,  2,  2,
#                                  2,  2,  2,  2,
#                                  1,  1,  1,  1),
#                           p3 = c(NA, NA, NA, NA,
#                                  1,  1,  1,  1,
#                                  1,  1,  1,  1,
#                                  1,  1,  1,  1))

# simulate end dates, determine the end of mobility imputation
sim_start = "2020-02-15"
sim_end = "2022-12-31"
# impute stringency index
si_post <- 10 # generally should be lower than si_post after examining the data
recovery_period <- 365 # recovery_period days to recover to si_post level of stringency
source("d_si_imputation.R")
source("d_mobility_imputation.R")

priority_policy <- list()
priority_policy[["p1"]] <- c(rep(NA, 4), rep(1, 12))
priority_policy[["p2"]] <- c(rep(NA, 4), rep(2, 8), rep(1, 4))
priority_policy[["p3"]] <- c(rep(NA, 4), rep(1, 8), rep(2, 4))
priority_policy[["p4"]]  <- c(rep(NA, 4), rep(5, 8), 4, 3, 2, 1)

source("f_gen_econ_within_app.R")
source("f_simulate_within_app.R")
source("d_vac_progress.R")
flags = sapply(c(members$country_name), function(i) {
  paste0("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/",
         tolower(countrycode::countrycode(i, "country.name", "iso2c")),
                                                           ".svg")
  }
)

# get lon and lat and area
wp_reg = qread("worldpop5yr.qs");
wp_reg[, tot_pop := rowSums(.SD), .SDcols = f_0:m_80] # Calculate total population
wp_reg = wp_reg[!is.na(lon) & !is.na(lat)] %>% 
  filter(type == "Country") %>% 
  # mutate(wb = countrycode::countrycode(name, "country.name", "wb")) %>% 
  filter(id %in% members$wb)


# Restrict to regions with a lon and lat; this removes places with 0 population as well.

rm("fit", "pre_tab","owid","val","p","tab","gm_forecast","gm_scaled","si_imputed","oxcgrt","to_merge","si","gm",
   "schedule_pre")
save.image("global.RData")
save.image("../demo/global.RData")
