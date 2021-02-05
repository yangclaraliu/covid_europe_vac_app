
library(tidyverse)
# library(sf)
library(countrycode) 
# library(rnaturalearth)
library(magrittr)
library(data.table)
library(shinyjs)
library(scales)
library(shinyWidgets)
library(leaflet)
library(zoo)
# library(ggsflabel)
# library(mgcv)
# library(pspline)

# load data needed from covid
# cm_path = "covidm_for_fitting/" # "~/GitHub/covidm_MTPs/covidm_for_fitting/"
# cm_force_rebuild = F
# cm_build_verbose = T
# cm_version = 2
load("global.RData")
source(paste0(cm_path, "/R/covidm.R"))

