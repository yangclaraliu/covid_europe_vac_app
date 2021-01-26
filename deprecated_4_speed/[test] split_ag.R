load("~/GitHub/covid_europe_vac_app/deprecated_4_speed/global.RData")
cm_path = "covidm_for_fitting/" # "~/GitHub/covidm_MTPs/covidm_for_fitting/"
cm_force_rebuild = F
cm_build_verbose = T
cm_version = 2
source(paste0(cm_path, "/R/covidm.R"))

cn <- "Belgium"

#### baseline ####
params <- cm_parameters_SEI3R(cn,
                              u = sus)
current_R0 <- cm_calc_R0(params, 1)
target_R0 <- 2.7
params$pop[[1]]$u <- params$pop[[1]]$u * target_R0 / current_R0
res <- cm_simulate(params)
baseline <- res$dynamics %>%
  group_by(t) %>% 
  filter(compartment == "cases") %>% 
  summarise(value = sum(value), .groups = "drop")

#### proposed ####
ag_names <- cm_matrices[[cn]]$home %>% colnames
rg_names <- c("comorbidity", "occupation", "other")

new_names <- CJ(tag1 = ag_names, tag2 = rg_names) %>% 
  mutate(tag3 = paste0(tag1, ",", tag2)) %>% 
  pull(tag3)

old_mat <- cm_matrices[[cn]] %>% 
  map(reshape2::melt) %>% 
  map(setNames, c("x", "y", "value"))

new_mat <-  CJ(Var1 = new_names, Var2 = new_names) %>% 
  separate(Var1, into = c("ag1", "rg1"), sep = ",", remove = F) %>% 
  separate(Var2, into = c("ag2", "rg2"), sep = ",", remove = F)

cm_matrices[[cn]] <- old_mat %>% 
  map(left_join, 
      new_mat,
      by = c("x" = "ag1",
             "y" = "ag2")) %>% 
  map(dplyr::select,
      Var1, Var2, value) %>% 
  map(reshape2::dcast,
      Var1 ~ Var2) %>% 
  map(column_to_rownames, var = "Var1") %>% 
  map(as.matrix)

cm_populations %>% 
  filter(name != cn) %>% 
  bind_rows(cm_get_demographics(cn, length(ag_names)) %>% 
              slice(rep(1:n(), each = 3)) %>% 
              mutate(rg = 1:n()%%3 %>% 
                       factor(., levels = c(0:2),
                              labels = rg_names),
                     age = paste0(age, ",", rg)) %>% 
              dplyr::select(-rg) %>% 
              mutate(name = cn,
                     f = f/length(rg_names),
                     m = m/length(rg_names))) %>% 
  setDT() -> cm_populations

params <- cm_parameters_SEI3R(cn,
                              u = rep(sus, each = length(rg_names)))
current_R0 <- cm_calc_R0(params, 1)
target_R0 <- 2.7
params$pop[[1]]$u <- params$pop[[1]]$u * target_R0 / current_R0
res <- cm_simulate(params)

toy <- res$dynamics %>%
  group_by(t) %>% 
  filter(compartment == "cases") %>% 
  summarise(value = sum(value), .groups = "drop")

## compare
baseline %>% 
  left_join(toy, "t") %>% 
  setNames(c("t", "baseline", "split")) %>% 
  ggplot(., aes(x = baseline, y = split)) +
  geom_abline(slope = 1, intercept = 0, size = 2, color = "red", alpha = 0.5) +
  geom_point() +
  theme_cowplot()

baseline %>% 
  left_join(toy, "t") %>% 
  pivot_longer(cols = c(value.x, value.y)) %>% 
  mutate(name = factor(name,
                       levels = c("value.x", "value.y"),
                       labels = c("baseline", "split"))) %>% 
  ggplot(., aes(x = t, y = value, color = name)) +
  # geom_area(alpha = 0.1) +
  geom_line(size = 2, alpha = 0.8) +
  theme_cowplot() +
  ggsci::scale_color_lancet() +
  labs(color = "")
