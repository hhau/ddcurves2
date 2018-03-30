#' Fit the Single hyperbolic tangent and with fixed correlation matrix.
#'
#' @param stan_data_list Stan data list 
#' @param ... other parameters that get passed to \link[rstan]{sampling}
#'
#' @return an object of class 'stanfit'
#' @export
single_tanh_fixed_corr_mat <- function(stan_data_list, ...) {
  model_fit <- rstan::sampling(stanmodels$single_tanh_fixed_cov_structure, 
                               data = stan_data_list, 
                               ...)
  return(model_fit)
} 
