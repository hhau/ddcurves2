#' Fit the double tanh model, with no timeseries structre and an simple
#' hierarchical mean
#'
#' @param stan_data_list Stan data list, with elements:
#' n_depths (the number of depth values which we record density at)
#' n_times (the number of time points at which density is recorded)
#' depths (vector of depth values)
#' densities (an n_times * n_depths matrix of density observations.) 
#' @param ... other parameters that get passed to 
#'  \code{\link[rstan:stanmodel-method-sampling]{sampling}}
#'
#' @return an object of class 'stanfit'
#' @export
double_tanh_no_timeseries <- function(stan_data_list, ...) {
  model_fit <- rstan::sampling(stanmodels$double_tanh_hierarchical_mean_reparam, 
                               data = stan_data_list, ...)
  return(model_fit)
}
