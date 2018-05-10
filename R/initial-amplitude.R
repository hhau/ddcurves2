# Thu May 10 18:04:19 2018 ------------------------------
# wrapper for: initial amplitude model 

#' Fit the initial amplitude model
#'
#' @param stan_data_list Stan data list 
#' @param ... other parameters that get passed to \link[rstan]{sampling}
#'
#' @return an object of class 'stanfit'
#' @export
initial_amplitude_model <- function(stan_data_list, ...) {
  model_fit <- rstan::sampling(stanmodels$initial_amplitude,
                               data = stan_data_list,
                               ...)
  return(model_fit)
}
