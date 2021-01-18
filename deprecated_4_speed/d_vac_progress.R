require(bsts)
require(e1071)
require(magrittr)

t <- 365*2 # start vaccinating from 2021-01-01
obj <- 0.5

vac_progress <- list()

# linear vaccination scenario
# y = a + t*x
# t = 0, y = 0, --> a = 
# y1 = t; y_730 = 730*t
# 731 * t * 730/2 = 0.5
 vac_linear <- function(obj, #final vacciantion target
                       t){ # days elapsed
  x <- (obj*2)/(t*(t+1))
  return(x * (1:t))
} 

# test vac_linear
 vac_linear(0.5, 365*2) -> vac_progress[["linear"]]
 
 # exponential vaccination
 f <- function(r){

      abs(GeometricSequence(t, 6e-7, r) %>% sum - obj)
}
res <- optimize(f, c(0.9,1.1))
r <- res$minimum
GeometricSequence(t, 6e-7, r) -> vac_progress[["exponential"]]

# 
sigmoid(-5:5) %>% 
  enframe(name = "val") %>% 
  full_join(enframe(1:t, name = "val") %>% 
              mutate(val = ntile(value, 11)) %>% 
              group_by(val) %>% group_split() %>% 
              map(filter, value == min(value)) %>% 
              bind_rows(),
            by = "val") %>% 
  right_join(enframe(1:t, name = "val"),
             by = c("value.y" = "val")) %>% 
  arrange(value.y) %>% 
  mutate(value.x = imputeTS::na_interpolation(value.x)) %>% 
  mutate(tot = sum(value.x),
         p = obj*value.x/tot) %>% 
  pull(p) -> vac_progress[["sigmoid"]]

vac_progress %<>% bind_cols() %>% data.table()
