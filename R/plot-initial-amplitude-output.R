# Thu May 10 18:07:12 2018 ------------------------------
# initial amplitude plot maker


#' Plot the fitted curve and its credible intervals, and the prediction 
#' intervals.
#' 
#' All intervals are at a 95% level.
#'
#' @param model_fit an "initial_amplitde" model stanfit objects
#' @param stan_dat the data associated with the model_fit objects
#' @param ... unused currently
#'
#' @return a ggplot object
#' @export
plot_initial_amplitude_output <- function(model_fit, stan_dat, ...) {
  
  mu_samples <- rstan::extract(model_fit, "mu_orig")
  mu_df <- data.frame(
    mu_mean = apply(mu_samples[[1]], 2, mean),
    mu_lower = apply(mu_samples[[1]], 2, quantile, 0.025),
    mu_upper = apply(mu_samples[[1]], 2, quantile, 0.975)
  )
  
  plot_df <- as.data.frame(cbind(
    mu_df,
    a_zero = stan_dat$a_zero,
    ssh = stan_dat$ssh)
  )
  
  plot_df <- plot_df[order(plot_df$ssh),]
 
  # check the prediction intervals 
  pred_samples <- rstan::extract(model_fit, "a_zero_new_orig")
  pred_df <- data.frame(
    pred_mean = apply(pred_samples[[1]], 2, mean),
    pred_lower = apply(pred_samples[[1]], 2, quantile, 0.025),
    pred_upper = apply(pred_samples[[1]], 2, quantile, 0.975),
    pred_ssh = stan_dat$ssh_new
  )
  
  gg_obj <- ggplot2::ggplot(data = plot_df) +
    ggplot2::geom_point(ggplot2::aes(x = ssh, y = a_zero), alpha = 0.25) + 
    ggplot2::geom_line(ggplot2::aes(x = ssh, y = mu_mean), size = 1.3) +
    ggplot2::geom_line(data = pred_df, ggplot2::aes(x = pred_ssh, y = pred_mean), size = 1.1, col = "red", lty = "dashed") +
    ggplot2::geom_ribbon(ggplot2::aes(x = ssh, ymin = mu_lower, ymax = mu_upper), alpha = 0.6) +
    ggplot2::geom_ribbon(data = pred_df, ggplot2::aes(x = pred_ssh, ymin = pred_lower, ymax = pred_upper), alpha = 0.05, col = "red") + 
    ggplot2::xlab("Sea Surface Height") +
    ggplot2::ylab("Initial Amplitude (a0)") + 
    ggplot2::theme_bw()
  
  return(gg_obj)
  
}
