#' Fit the naive (single mean, no time dependency) stan model
#'
#' @param stan_data_list Stan data list 
#' @param ... other parameters that get passed to \link[rstan]{sampling}
#'
#' @return an object of class 'stanfit'
#' @export
naive_model <- function(stan_data_list, ...) {
  res <- rstan::sampling(stanmodels$depth_contour_hierarchical_mean,
                         data = stan_data_list, ...)
  return(res)
}
