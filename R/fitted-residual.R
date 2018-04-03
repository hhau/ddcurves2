#' Plot the residual values (y) against the fitted values (x)
#'
#' @param stan_data_list the data list passed to the stanmodel
#' @param stan_obj the stanfit object fitted using the aforementioned data
#' @param ... unused, possible graphical options
#'
#' @return ggplot object of the residual plot
#' @export
resid_fitted_plot <- function(stan_data_list = stan_dat,
                              stan_obj = model_fit, 
                              ...) {
  # get the mean fitted value out of the model fit
  # Maybe we should consider stratifying by chain and using the reisd's as a
  # diagnostic at some point? not worth doing now.
  fitted_str <- grep("fitted", stan_obj@model_pars, value = TRUE)
  fitted_val_samples <- rstan::extract(stan_obj, fitted_str)
  mean_fitted_val <- apply(fitted_val_samples[[1]], 2:3, mean)
  
  # calculate the residual
  mean_resid <- stan_data_list$densities - mean_fitted_val
  
  # shape into the appropriate x-y data frame for ggplot
  # hacky type coercion
  plot_df <- data.frame(x = as.vector(t(mean_fitted_val)),
                        y = as.vector(t(mean_resid)))
  
  # ggplot it
  res_plot <- ggplot2::ggplot(data = plot_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() + 
    ggplot2::xlab("Fitted value") +
    ggplot2::ylab("Residual")
  
  return(res_plot)
}
