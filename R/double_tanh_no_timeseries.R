#' Fit the double tanh model, with no timeseries structre and an simple
#' hierarchical mean
#'
#' @param stan_data_list  Stan data list 
#' @param ... other parameters that get passed to \link[rstan]{sampling}
#'
#' @return an object of class 'stanfit'
#' @export
double_tanh_no_timeseries <- function(stan_data_list, ...) {
  model_fit <- rstan::sampling(stanmodels$double_tanh_hierarchical_mean, 
                               data = stan_data_list, ...)
  return(model_fit)
}
