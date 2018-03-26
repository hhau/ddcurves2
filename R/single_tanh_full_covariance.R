#' Fit the Single hyperbolic tangent and full covariance structure model.
#'
#' @param stan_data_list Stan data list 
#' @param ... other parameters that get passed to \link[rstan]{sampling}
#'
#' @return an object of class 'stanfit'
#' @export
single_tanh_full_covariance <- function(stan_data_list, ...) {
  model_fit <- rstan::sampling(stanmodels$single_tanh_full_cov_matrix, 
                               data = stan_data_list, 
                               ...)
  return(model_fit)
} 
