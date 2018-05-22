# Thu May 10 18:01:05 2018 ------------------------------
# wrapper for: double tanh model with time series structure

#' Fit the double tanh model, with time series structure
#'
#' @param stan_data_list Stan data list, for structure of this list, see:
#' \code{\link{double_tanh_no_timeseries}}
#' @param ... other parameters that get passed to \link[rstan]{sampling}
#'
#' @return an object of class 'stanfit'
#' @export
double_tanh_with_timeseries <- function(stan_data_list, ...) {
  model_fit <- rstan::sampling(stanmodels$double_tanh_time_series,
                               data = stan_data_list,
                               ...)
  return(model_fit)
}
