`.sourceCpp_1_DLLInfo` <- dyn.load('covidm_for_fitting/build/sourceCpp-x86_64-w64-mingw32-1.0.5/sourcecpp_3e3424a7b4d/sourceCpp_13.dll')

cm_backend_simulate <- Rcpp:::sourceCppFunction(function(parameters, n_run = 1L, seed = 0L) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_cm_backend_simulate')
cm_evaluate_distribution <- Rcpp:::sourceCppFunction(function(dist_code, steps = 101L, xmin = 0, xmax = -1) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_cm_evaluate_distribution')
cm_backend_mcmc <- Rcpp:::sourceCppFunction(function(likelihood, extra_params, params_priors, seed, burn_in, n_chains, iterations, verbose, reeval_likelihood, in_parallel, n_threads) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_cm_backend_mcmc')
cm_backend_mcmc_init <- Rcpp:::sourceCppFunction(function(likelihood, extra_params, params_priors, initial, seed, burn_in, n_chains, iterations, verbose, reeval_likelihood, in_parallel, n_threads) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_cm_backend_mcmc_init')
cm_backend_optimize <- Rcpp:::sourceCppFunction(function(likelihood, extra_params, params_priors, seed, maxeval, ftol_abs, verbose) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_cm_backend_optimize')
cm_backend_prior_sample <- Rcpp:::sourceCppFunction(function(params_priors) {}, FALSE, `.sourceCpp_1_DLLInfo`, 'sourceCpp_1_cm_backend_prior_sample')

rm(`.sourceCpp_1_DLLInfo`)
