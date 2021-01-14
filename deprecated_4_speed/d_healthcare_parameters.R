# include health burden process as in covidm for consistency
# Health burden processes
probs = fread(
  "Age,Prop_symptomatic,IFR,Prop_inf_hosp,Prop_inf_critical,Prop_critical_fatal,Prop_noncritical_fatal,Prop_symp_hospitalised,Prop_hospitalised_critical
  10,0.66,8.59E-05,0.002361009,6.44E-05,0.5,0,0,0.3
  20,0.66,0.000122561,0.003370421,9.19E-05,0.5,9.47E-04,0.007615301,0.3
  30,0.66,0.000382331,0.010514103,0.000286748,0.5,0.001005803,0.008086654,0.3
  40,0.66,0.000851765,0.023423527,0.000638823,0.5,0.001231579,0.009901895,0.3
  50,0.66,0.001489873,0.0394717,0.001117404,0.5,0.002305449,0.018535807,0.3
  60,0.66,0.006933589,0.098113786,0.005200192,0.5,0.006754596,0.054306954,0.3
  70,0.66,0.022120421,0.224965092,0.016590316,0.5,0.018720727,0.150514645,0.3
  80,0.66,0.059223786,0.362002579,0.04441784,0.5,0.041408882,0.332927412,0.3
  100,0.66,0.087585558,0.437927788,0.065689168,0.5,0.076818182,0.617618182,0.3")

reformat = function(P)
{
  # 70-74,3388.488  75-79,2442.147  80-84,1736.567  85-89,1077.555  90-94,490.577  95-99,130.083  100+,15.834
  x = c(P[1:7], weighted.mean(c(P[8], P[9]), 
                              c(3388.488 + 2442.147, 1736.567 + 1077.555 + 490.577 + 130.083 + 15.834)));

  return (rep(x, each = 2))
}
## alternative data for UK:
##Prop_critical_fatal assumed 32% (based on 20k pts https://www.bmj.com/content/369/bmj.m1985/)
##Prop_hospitalised_critical assumed 17% (based on 20k pts https://www.bmj.com/content/369/bmj.m1985/)
probs[, "Prop_critical_fatal"]        <- 0.32
probs[, "Prop_hospitalised_critical"] <- 0.17
# Ensemble estimates, Table S4, p. 13/14 https://www.medrxiv.org/content/10.1101/2020.08.24.20180851v1.full.pdf
P.death_nonicu <- c(0.00002,
                   # 0.0, 0.0,
                   0.00001, 0.00001,
                    0.00002, 0.00004,
                    0.00009, 0.00017, 0.00029, 0.00053,
                    0.00086, 0.00154, 0.00241, 0.00359,
                    0.00642, 0.01076,
                    0.02276,
                    0.07274)
## ONS 75-79 vs 75+: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2019estimates
P.death_nonicu[16] <- ((2325296/5687895) * P.death_nonicu[16]   + (1-2325296/5687895) * P.death_nonicu[17])
P.death_nonicu <- P.death_nonicu[1:16]
# instead of this assumption, use slightly higher IFRs informed by REACT3, Table 3
#P.death_nonicu[14:15] <- 0.0313 # if using this, overshooting observed deaths; rather only change 75+ to be conservative
P.death_nonicu[16] <- 0.116
# update prop symptomatic with ratio admisssion/cases over deaths/cases in UK on 15.07.2020
#probs[, "Prop_symp_hospitalised"] <- probs[, Prop_symp_hospitalised]/(18.5/2.3)*(44.7/15.4)
#probs[, "Prop_symp_hospitalised"] <- P.death_nonicu * (44.7/15.4)
P.symp_hospitalised <- c(0.010, 0.002, 0.002, 0.003,
                         0.005, 0.011, 0.015, 0.020,
                         0.027, 0.044, 0.062, 0.073,
                         0.080, 0.084, 0.109, 0.454)
P.icu_symp     = P.symp_hospitalised * reformat(probs[, Prop_hospitalised_critical]);
P.nonicu_symp  = P.symp_hospitalised * reformat(probs[, (1 - Prop_hospitalised_critical)]);
burden_processes = list(
  list(source = "Ip", type = "multinomial", names = c("to_icu", "to_nonicu", "to_nonhosp"), report = c("", "", ""),
       prob   = matrix(c(P.icu_symp, P.nonicu_symp, 1 - P.icu_symp - P.nonicu_symp), nrow = 3, ncol = 16, byrow = T),
       delays = matrix(c(cm_delay_gamma(7, 7, 60, 0.25)$p, cm_delay_gamma(7, 7, 60, 0.25)$p, cm_delay_skip(60, 0.25)$p),
                       nrow = 3, byrow = T)),
  list(source = "to_icu", type = "multinomial", names = "icu", report = "pi",
       prob   = matrix(1, nrow = 1, ncol = 16, byrow = T),
       delays = matrix(cm_delay_gamma(10, 10, 60, 0.25)$p, nrow = 1, byrow = T)),
  list(source = "to_nonicu", type = "multinomial", names = "nonicu", report = "pi",
       prob   = matrix(1, nrow = 1, ncol = 16, byrow = T),
       delays = matrix(cm_delay_gamma(8, 8, 60, 0.25)$p, nrow = 1, byrow = T)),
  list(source = "S", type = "multinomial", names = c("new_infections"), report = c("i"),
       prob   = matrix(1, nrow = 1, ncol = 16, byrow = T),
       delays = matrix(cm_delay_skip(60, 0.25)$p, nrow = 1)),
  list(source = "new_infections", type = "multinomial", names = c("to_death"), report = c(""),
       prob   = matrix(1, nrow = 1, ncol = 16, byrow = T),
       delays = matrix(cm_delay_gamma(4, 4, 60, 0.25)$p, nrow = 1, byrow = T)),
  list(source = "to_death", type = "multinomial", names = c("death", "null"), report = c("o", ""),
       prob   = matrix(c(P.death_nonicu, 1 - P.death_nonicu), nrow = 2, ncol = 16, byrow = T),
       delays = matrix(c(cm_delay_gamma(22, 22, 60, 0.25)$p, cm_delay_skip(60, 0.25)$p), nrow = 2, byrow = T))
)
