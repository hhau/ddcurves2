#' Sample the single tanh model, with fixed correlation structure and weak (les
#' tight bounds) normal mean on the beta_zero parameter
#'
#' @param stan_data_list list of data to pass into stan, including cholesky
#' of the empirical correlation structure
#' @param ... additional arguments to pass to \link[rstan]{sampling}
#'
#' @return stanfit object 
#' @export
singe_tanh_fixed_corr_weak_mean <- function(stan_data_list, 
                                            ...) {
  model_fit <- rstan::sampling(stanmodels$single_tanh_fixed_corr_structure_normal_mean, 
                               data = stan_data_list, 
                               ...)
  return(model_fit)
}
