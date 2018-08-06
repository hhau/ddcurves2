# Thu May 10 18:04:19 2018 ------------------------------
# wrapper for: initial amplitude model

#' Fit the initial amplitude model
#'
#' @param stan_data_list Stan data list, with elements:
#' N (number of observations),
#' ssh (the vector of sea surface heights),
#' a_zero (the vector of initial amplitudes),
#' N_ssh_new (number of points to sample the PPD at),
#' ssh_new (sea surface heights at which we sample the PPD),
#' poly_deg (the degree of polynomial that we fit the the data)
#' @param ... other parameters that get passed to  
#'  \code{\link[rstan:stanmodel-method-sampling]{sampling}}
#'
#' @return an object of class 'stanfit'
#' @export
initial_amplitude_model <- function(stan_data_list, ...) {
  model_fit <- rstan::sampling(stanmodels$initial_amplitude,
                               data = stan_data_list,
                               ...)
  return(model_fit)
}

#' Sample the PPD for the initial amplitude model
#' 
#' Currently only works in settings where 
#' \code{\link[parallel]{mclapply}} functions.
#'
#' @param sea_surface_height the sea surface height
#' @param n_samples defaults to 500, how many samples to return
#'
#' @return vector of legnth n_samples
#' @export
a0_sample_generator <- function(sea_surface_height, n_samples = 500) {
  q <- 3 
  
  stan_dat <- list(
    N = length(a0_data_no_zeros$a_zero),
    ssh = a0_data_no_zeros$sea_surface_height,
    a_zero = a0_data_no_zeros$a_zero,
    N_ssh_new = 1,
    ssh_new = array(c(sea_surface_height), dim = c(1)),
    poly_deg = q
  )
  
  res <- parallel::mclapply(1:2, function(x) {
    capture.output({
      model_fit <- ddcurves2::initial_amplitude_model(
        stan_dat,
        warmup = 175,
        iter = 175 + (n_samples / 2),
        cores = 1,
        chains = 1,
        refresh = -1
      )
      pred_samples <- rstan::extract(model_fit, pars = "a_zero_new_orig")[[1]]
    })
    return(pred_samples)
  })
  
  return(unlist(res))
  
}
