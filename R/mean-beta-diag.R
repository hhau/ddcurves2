# mean beta model diagnostic plots

#' Generate traceplots of the Mean's of the Betas.
#'
#' @param stan_obj stan model fit
#' @param print logical, if TRUE and print_dir specified, print a pdf to print_dir
#' @param print_dir directory to put image in
#' @param ... optional params passed to pdf()
#'
#' @return
#' @export
mean_beta_trace_diag <- function(stan_obj,
                                 print = FALSE,
                                 print_dir = "",
                                 ...) {
    par_strings <- grep("mean_*", stan_obj@model_pars, value = TRUE)
    bayesplot::color_scheme_set("mix-teal-pink")
    all_beta_mean_samples <- rstan::extract(stan_obj, pars = par_strings, permuted = FALSE)
    res <- bayesplot::mcmc_trace(all_beta_mean_samples)
    print(res)
    
    if (print & (print_dir != "")) {
      pdf(file = paste0(print_dir, format(Sys.time(), "%F---%H-%M-%S"), "_mean-beta-trace.pdf"), ...)
      print(res)
      dev.off()
    }
    
    return(res)
}

#' Generate density plots by chain of the mean's of the betas.
#'
#' Generate density plots of the Mean's of the Betas by chain
#'
#' @param stan_obj stan model fit
#' @param print logical, if TRUE and print_dir specified, print a pdf to print_dir
#' @param print_dir directory to put image in
#' @param ... optional params passed to pdf()
#'
#' @return
#' @export
mean_beta_density_diag <- function(stan_obj, 
                                   print = FALSE, 
                                   print_dir = "",
                                   ...) {
  par_strings <- grep("mean_*", stan_obj@model_pars, value = TRUE)
  bayesplot::color_scheme_set("mix-teal-pink")
  all_beta_mean_samples <- rstan::extract(stan_obj, pars = par_strings, permuted = FALSE)
  res <- bayesplot::mcmc_dens_overlay(all_beta_mean_samples)
  print(res)
  
  if (print & (print_dir != "")) {
    pdf(file = paste0(print_dir, format(Sys.time(), "%F---%H-%M-%S"), "_mean-beta-trace.pdf"), ...)
    print(res)
    dev.off()
  }
  
  return(res)
}
