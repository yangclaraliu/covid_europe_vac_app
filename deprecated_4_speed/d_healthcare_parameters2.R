critical2 <- 0

picu_cocin_func = function(age)
{
  x = c(-0.1309118, 0, 17.2398874, 65.7016492, 100)
  y = c(-2.1825091, -2.1407043, -1.3993552, -1.2344361, -8.8191062)
  p = splinefun(x, y)(age)
  exp(p) / (1 + exp(p))
}
picu_cocin = picu_cocin_func(0:85)

# Infection fatality rate (derived from Levin et al., preprint)
ifr_levin = 100 * exp(-7.56 + 0.121 * 0:85) / (100 + exp(-7.56 + 0.121 * 0:85)) / 100;

# Infection hospitalisation rate (derived from Salje et al., Science)
ihr_salje = exp(-7.37 + 0.068 * 0:85) / (1 + exp(-7.37 + 0.068 * 0:85));

# Amalgamate probabilities
probabilities = data.table(age = 0:85, ihr = ihr_salje, ifr = ifr_levin, picu = picu_cocin)
probabilities[, age_group := pmin(15, age %/% 5)]
probabilities = probabilities[, lapply(.SD, mean), by = age_group, .SDcols = 2:4]

# Create model burden processes
P.critical = probabilities[, ihr * picu]
P.severe   = probabilities[, ihr * (1 - picu)]
P.death    = probabilities[, ifr]

burden_processes = list(
  list(source = "new_EEa", type = "multinomial", names = c("new_infections"), report = c("i"),
       prob   = matrix(1, nrow = 1, ncol = 16, byrow = T),
       delays = matrix(cm_delay_skip(60, 0.25)$p, nrow = 1)),
  
  list(source = "E", type = "multinomial", names = c("death", "null"), report = c("o", ""),
       prob = matrix(c(P.death, 1 - P.death), nrow = 2, ncol = 16, byrow = T),
       delays = matrix(c(cm_delay_gamma(26, 5, 60, 0.25)$p, cm_delay_skip(60, 0.25)$p), nrow = 2, byrow = T)),
  
  list(source = "E", type = "multinomial", names = c("to_hosp_critical", "to_hosp_critical2", "to_hosp_severe", "null"), report = c("", "", "", ""),
       prob = matrix(c(P.critical * (1 - critical2), P.critical * critical2, P.severe, 1 - P.critical - P.severe), nrow = 4, ncol = 16, byrow = T),
       delays = matrix(c(cm_delay_gamma(8.5, 5, 60, 0.25)$p, cm_delay_gamma(8.5, 5, 60, 0.25)$p, cm_delay_gamma(8.5, 5, 60, 0.25)$p, cm_delay_skip(60, 0.25)$p), nrow = 4, byrow = T)),
  
  list(source = "to_hosp_severe", type = "multinomial", names = "non_icu_severe", report = "pi",
       prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
       delays = matrix(cm_delay_gamma(14.6, 5, 60, 0.25)$p, nrow = 1, byrow = T)),
  
  list(source = "to_hosp_critical2", type = "multinomial", names = "non_icu_critical2", report = "pi",
       prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
       delays = matrix(cm_delay_gamma(15.6, 5, 60, 0.25)$p, nrow = 1, byrow = T)),
  
  list(source = "to_hosp_critical", type = "multinomial", names = "non_icu_critical", report = "pi",
       prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
       delays = matrix(cm_delay_gamma(6.0, 5, 60, 0.25)$p, nrow = 1, byrow = T)),
  
  list(source = "non_icu_critical", type = "multinomial", names = "icu_critical", report = "pi",
       prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
       delays = matrix(cm_delay_gamma(9.6, 5, 60, 0.25)$p, nrow = 1, byrow = T))
)