#' Plot the Residual values (y) as a function of Depth (x)
#'
#' @param stan_data_list the data list passed to the stanmodel, for structure
#' of this list, see \code{\link{double_tanh_no_timeseries}}
#' @param stan_obj the stanfit object fitted using the aforementioned data
#' @param ... nused, possible graphical options
#'
#' @return ggplot object of the specified residual plot
#' @export
resid_depth_plot <- function(stan_data_list = stan_dat, stan_obj = model_fit, ...) {
  fitted_str <- grep("fitted", stan_obj@model_pars, value = TRUE)
  fitted_val_samples <- rstan::extract(stan_obj, fitted_str)
  mean_fitted_val <- apply(fitted_val_samples[[1]], 2:3, mean)
  mean_resid <- stan_data_list$densities - mean_fitted_val
  plot_df <- data.frame(x = rep(stan_dat$depths, nrow(stan_dat$densities)),
                        y = as.vector(t(mean_resid)))

  res_plot <- ggplot2::ggplot(data = plot_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(alpha = 0.152) +
    ggplot2::theme_bw() +
    ggplot2::geom_hline(yintercept = 0, colour = "grey", size = 1.5, alpha = 0.2) +
    ggplot2::xlab("Depth") +
    ggplot2::ylab("Residual")

  return(res_plot)
}
