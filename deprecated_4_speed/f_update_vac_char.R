# f_update_vac_char.R
# Authors: Yang Liu, Frank Sandman
# Date: 29/10/2020
# Date updated: 29/10/2020
# Objective: parameterise vaccine characteristics in covidm

update_vac_char <- function(para,
                            waning_vac = 52*7,
                            ve_i = NULL,
                            ve_d = NULL){

  # debug
  # para <- gen_country_basics("Belgium")
  # ve_i <- 0.9
  # ve_d <- 0.5
  # waning_vac = 52*7

  # check input ve parameters
  n_age <- length(para$pop[[1]]$size)
  # if(length(ve) != 1 & length(ve) != n_age) stop("VE input is not of correct dimension.")
  # if(length(ve) == 1) ve <- rep(ve, n_age)

  # assigning values to different types of protection
  # para$pop[[1]]$ev <- 1 # no need to run as it is the default value
  # assume vaccines to be effective after one dose
  para$pop[[1]]$ev2 <- rep(0, n_age)
  # protection against infection
  para$pop[[1]]$ei_v <- rep(ve_i, n_age) # * n_age_gr_vacc_B
  # protection against disease
  para$pop[[1]]$ed_vi <- rep(ve_d, n_age)

  # waning vaccine-induced immunity
  para$pop[[1]]$wv = rep((1/waning_vac), n_age)

  # return results
  return(para)
}