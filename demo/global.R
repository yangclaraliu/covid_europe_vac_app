
library(tidyverse)
# library(sf)
library(countrycode) 
# library(rnaturalearth)
library(magrittr)
library(data.table)
library(shinyjs)
library(scales)
library(shinyWidgets)
# library(ggsflabel)
# library(mgcv)
# library(pspline)

# load data needed from covidm
# cm_path = "covidm_for_fitting/" # "~/GitHub/covidm_MTPs/covidm_for_fitting/"
# cm_force_rebuild = F
# cm_build_verbose = T
# cm_version = 2
load("global.RData")
source(paste0(cm_path, "/R/covidm.R"))
flags = sapply(c(members$country_name), function(i) paste0("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/",
                                                           tolower(countrycode::countrycode(i, "country.name", "iso2c")),
                                                           ".svg"))
# HTML(paste0(p("Strategy 1: All Adults"),
#             "</p>",
#             p("Strategy 2: All 60+, then all younger adults"),
#             "</p>",
#             p("Strategy 3: All younger adults, then all elderly"),
#             "</p>",
#             p("Strategy 4: From the oldest to the youngest adults"))),



# sort_input <- function(input, pattern){
#   input[startsWith(names(input),pattern)] %>% 
#     stack %>%
#     mutate(ind = as.character(ind),
#            ind = parse_number(ind)) %>% 
#     arrange(ind) %>% 
#     pull(values) -> tmp
#   return(tmp)
# }
