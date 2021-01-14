# f_update_vac_char.R
# Authors: Yang Liu, Frank Sandman
# Date: 29/10/2020
# Date updated: 29/10/2020
# Objective: parameterise vaccine characteristics in covidm

update_vac_char <- function(para,
                            waning_vac = 52*1,
                            ve = c(rep(0.7,10),rep(0.5,6))){
  
  require(covidm)
  
  # check input ve parameters
  n_age <- length(para$pop[[1]]$size)
  if(length(ve) != 1 & length(ve) != n_age) stop("VE input is not of correct dimension.")
  if(length(ve) == 1) ve <- rep(ve, n_age)
  
  # ve by age
  para$pop[[1]]$ev <- ve # * n_age_gr_vacc_B
  
  # waning vaccine-induced immunity 
  para$pop[[1]]$wv = rep((1/waning_vac), n_age)
  
  # return results
  return(para)
}