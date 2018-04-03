#' Plot the residual values (y) against time (x)
#'
#' @param stan_data_list the data list passed to the stanmodel
#' @param stan_obj the stanfit object fitted using the aforementioned data
#' @param ... unused, possible graphical options
#'
#' @return ggplot object of the residual plot
#' @export
resid_time_plot <- function(stan_data_list = stan_dat,
                            stan_obj = model_fit, 
                            ...) {
  fitted_str <- grep("fitted", stan_obj@model_pars, value = TRUE)
  fitted_val_samples <- rstan::extract(stan_obj, fitted_str)
  mean_fitted_val <- apply(fitted_val_samples[[1]], 2:3, mean)
  
  # calculate the residual
  mean_resid <- stan_data_list$densities - mean_fitted_val
  
  plot_df <- data.frame(x = sort(rep(1:nrow(mean_resid), ncol(mean_resid))), 
                        y = as.vector(t(mean_resid)))
  
  res_plot <- ggplot2::ggplot(data = plot_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() + 
    ggplot2::xlab("Time (Index)") +
    ggplot2::ylab("Residual")
  
  
  return(res_plot)
}
