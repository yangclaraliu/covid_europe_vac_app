
# covid_europe_vac_app
## Buttons
- `toggleEpi`: controlling the appearance of epi parameters
- `update`: updating all the results panels
- `refresh`: updating milestone slots

## Input
- `cn`: country
- `cov[0-9]`: milestone vaccine coverage goals
- `date[0-9]`: milestone dates
- `max_cov`: previously `cov_tar`, maximum vaccine uptake
- `ms_covs`: technically an output object, UI object to be rendered to obtain milestone coverage goals dynamically
- `ms_dates`: technically an output object, UI object to be rendered to obtain milestone dates dynamically
- `n_ms`: number of milestones
- `waning_nat`: waning of infection induced natural immunity
- `waning_vac`: waning of vaccine induced immunity
- `rn`: basic reproduction number
- `ve`: vaccine efficacy

## Output
- `daily_vac`: daily vaccination progress by age
- `econ`: health economic outcomes (for different vaccination strategies) 
- `pho`: public health outcomes
- `supply`: overall vaccine supply