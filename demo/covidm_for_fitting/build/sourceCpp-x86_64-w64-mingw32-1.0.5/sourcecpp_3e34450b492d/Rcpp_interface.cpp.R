`.sourceCpp_3_DLLInfo` <- dyn.load('covidm_for_fitting/build/sourceCpp-x86_64-w64-mingw32-1.0.5/sourcecpp_3e34450b492d/sourceCpp_15.dll')

cm_backend_simulate_v2 <- Rcpp:::sourceCppFunction(function(parameters, n_run = 1L, seed = 0L, n_threads = 1L, file_out = "") {}, FALSE, `.sourceCpp_3_DLLInfo`, 'sourceCpp_3_cm_backend_simulate_v2')
cm_evaluate_distribution_v2 <- Rcpp:::sourceCppFunction(function(dist_code, steps = 101L, xmin = 0, xmax = -1) {}, FALSE, `.sourceCpp_3_DLLInfo`, 'sourceCpp_3_cm_evaluate_distribution_v2')
cm_backend_mcmc_test <- Rcpp:::sourceCppFunction(function(R_base_parameters, params_priors, seed, burn_in, iterations, n_threads, classic_gamma) {}, FALSE, `.sourceCpp_3_DLLInfo`, 'sourceCpp_3_cm_backend_mcmc_test')
cm_backend_optimize_test <- Rcpp:::sourceCppFunction(function(R_base_parameters, params_priors, maxeval, ftol_abs, seed, n_threads) {}, FALSE, `.sourceCpp_3_DLLInfo`, 'sourceCpp_3_cm_backend_optimize_test')
cm_backend_sample_fit_test <- Rcpp:::sourceCppFunction(function(R_base_parameters, posterior, n, seed) {}, FALSE, `.sourceCpp_3_DLLInfo`, 'sourceCpp_3_cm_backend_sample_fit_test')

rm(`.sourceCpp_3_DLLInfo`)
