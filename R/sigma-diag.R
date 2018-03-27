# sigma diagnostics

#' Generate traceplots of all parameters with 'sigma' in their name.
#'
#' @param stan_obj stan model fit
#' @param print logical, if TRUE and print_dir specified, print a pdf to print_dir
#' @param print_dir directory to put image in
#' @param ... optional params passed to pdf()
#'
#' @return
#' @export
mean_sigma_trace_diag <- function(stan_obj,
                                 print = FALSE,
                                 print_dir = "",
                                 ...) {
  par_strings <- grep("sigma", stan_obj@model_pars, value = TRUE)
  bayesplot::color_scheme_set("mix-teal-pink")
  all_sigma_samples <- rstan::extract(stan_obj, pars = par_strings, permuted = FALSE)
  res <- bayesplot::mcmc_trace(all_sigma_samples)
  print(res)
  
  if (print & (print_dir != "")) {
    pdf(file = paste0(print_dir, format(Sys.time(), "%F---%H-%M-%S"), "_all-sigma-trace.pdf"), ...)
    print(res)
    dev.off()
  }
  
  return(res)
}

#' Generate density plots of all parameters with 'sigma' in their name.
#'
#' @param stan_obj stan model fit
#' @param print logical, if TRUE and print_dir specified, print a pdf to print_dir
#' @param print_dir directory to put image in
#' @param ... optional params passed to pdf()
#'
#' @return
#' @export
mean_sigma_dens_diag <- function(stan_obj,
                                  print = FALSE,
                                  print_dir = "",
                                  ...) {
  par_strings <- grep("sigma", stan_obj@model_pars, value = TRUE)
  bayesplot::color_scheme_set("mix-teal-pink")
  all_sigma_samples <- rstan::extract(stan_obj, pars = par_strings, permuted = FALSE)
  res <- bayesplot::mcmc_dens_overlay(all_sigma_samples)
  print(res)
  
  if (print & (print_dir != "")) {
    pdf(file = paste0(print_dir, format(Sys.time(), "%F---%H-%M-%S"), "_all-sigma-trace.pdf"), ...)
    print(res)
    dev.off()
  }
  
  return(res)
}
