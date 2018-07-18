#' Approximately sample the posterior for a new set of depths and densities
#'
#' @param new_depths new depths at which new_densities is measured
#' @param new_densities new density measurements
#' @param n_samples number of approximate samples to draw. In some specific
#' circumstances this can be different to the number of samples returned
#' @param nlm_inits initial values for nlm (the minimiser), defaults are 
#' reasonable for data on the original scale. 
#'
#' @return list with two components, the first being the mean estimate, and the
#' second the samples around this mean
#' @export
sample_new_post <- function(
  new_depths,
  new_densities,
  n_samples = 1000, 
  nlm_inits = c(1020, 2, 75, 45, 150, 47)) {
  
  neg_log_posterior <- function(theta, y, z) {
    temp_mu <- ddcurves2:::.double_tanh_function(z, theta[1:6])
    
    # likelihood
    temp_log_post <- dnorm(x = as.numeric(y), mean = temp_mu, sd = exp(theta[7]), log = T)
    
    # prior for sigma
    temp_log_post <- temp_log_post + dnorm(exp(theta[7]), mean = 0.05, sd = 0.05, log = T)
    
    # flat prior for beta for now.
    
    return(-sum(temp_log_post))
    
  }
  
  # nlm does something weird and then has a winge, seems to be robust enough
  # though
  nlm_lp_fit <- suppressWarnings(nlm(
    neg_log_posterior,
    p = c(nlm_inits, log(0.05)),
    y = new_densities,
    z = new_depths,
    # print.level = 1, 
    iterlim = 500
  ))
  
  temp_mu <- nlm_lp_fit$estimate[1:6]
  new_samples <- t(do.call(cbind, lapply(as.list(1:n_samples), function(x) {
    temp_mu + ddcurves2::approx_post_chol %*% rnorm(6)
  })))
  
  index_vec <- apply(new_samples, 1, function(x) {
    !any(x < 0)
  })
  
  new_samples <- new_samples[index_vec, ]
  
  res <- list(
    beta_est = temp_mu,
    beta_samples = new_samples
  )

  return(res)  
}

#' produce a plot of the approximate posterior
#'
#' @param new_depths new_depths
#' @param new_densities new_depths 
#' @param ... arguments passed to  \link{sample_new_post}
#'
#' @return a ggplot object
#' @export
plot_post_new_points <- function(new_depths,
                                 new_densities,
                                 plot_quantile = 0.025,
                                 ...) {
  beta_samples_list <- sample_new_post(new_depths, new_densities, ...)
  
  point_df <- data.frame(
    x = new_depths,
    y = new_densities
  )
  
  z_new <- seq(from = min(new_depths) - 5, to = 0, length.out = 300)
  
  samples_df <- apply(beta_samples_list$beta_samples, 1, function(x) {
    ddcurves2:::.double_tanh_function(z_new, x)
  })
  
  curve_df <- data.frame(
    x = z_new,
    y_mean = apply(samples_df, 1, mean), 
    y_lower = apply(samples_df, 1, quantile, plot_quantile),
    y_upper = apply(samples_df, 1, quantile, 1 - plot_quantile)
  )
  
  gg_obj <- ggplot2::ggplot(data = curve_df) + 
    ggplot2::geom_point(data = point_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y_mean)) + 
    ggplot2::geom_ribbon(ggplot2::aes(x = x, ymin = y_lower, ymax = y_upper), alpha = 0.1) +
    ggplot2::theme_bw() +
    ggplot2::coord_flip() +
    ggplot2::xlab("Depth (m)") +
    ggplot2::ylab("Density")
  
  return(gg_obj)
  
}
