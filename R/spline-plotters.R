# plot fitted beta's by chain (y/n)
#' Plot Spline Posteriors
#'
#' Plot the spline posteriors over time, potentially split by chain
#'
#' @param model_fit a "stanfit" object of the latent spline model
#' @param stan_data_list the stan data list associated with the model_fit
#' @param split_by_chain should the plots be split by chain? If TRUE, then
#' they will be, if FALSE the chains will be combined
#' @param interval_quantile what is the lower quantile of the interval of 
#' interest (0.025 = 95\% intervals)
#'
#' @return a ggplot2 object? maybe something that has been grid.arranged?
#'  I think I can do this with faceting.
#' @export 
plot_spline_posteriors <- function(model_fit, 
                                   stan_data_list, 
                                   split_by_chain = TRUE, 
                                   interval_quantile = 0.025) {
  beta_strings <- grep("^beta", model_fit@model_pars, value = TRUE)
  
  if (split_by_chain) {
    beta_samples <- rstan::extract(model_fit, beta_strings, permuted = !split_by_chain)
    
    array_dim <- c(
      iterations = model_fit@stan_args[[1]]$iter - model_fit@stan_args[[1]]$warmup,
      chains = length(model_fit@stan_args),
      time_points = stan_data_list$n_t,
      beta_coef = 6
    )
    
    beta_samples <- array(
      data = as.vector(beta_samples),
      dim = array_dim
    ) 
    
    outer_temp_list <- list()
    
    # there was an idea here for labels, but things got really ugly really fast
    #labels <- sapply(sprintf("beta[%d]", 0:5), function(x) parse(text = x))
    
    # loop over the chains to make the data frame
    for (ii in 1:array_dim[2]) {
      # also loop over each beta, just easier this way
      inner_temp_list <- list()
      
      for (zz in 1:array_dim[4]) {
        temp_array <- beta_samples[, ii, , zz]
        inner_temp_list[[zz]] <- data.frame(
          t = as.numeric(stan_data_list$x_mat[, 2]),
          y_mean = apply(temp_array, 2, mean),
          y_lower = apply(temp_array, 2, quantile, interval_quantile),
          y_upper = apply(temp_array, 2, quantile, 1 - interval_quantile),
          beta_coef = zz - 1,
          chain_id = ii
        )
      }
      
      outer_temp_list[[ii]] <- dplyr::bind_rows(inner_temp_list)
    }
    
    plot_df <- dplyr::bind_rows(outer_temp_list)
    plot_df$beta_coef <- as.factor(plot_df$beta_coef)
    plot_df$chain_id <- as.factor(plot_df$chain_id)
    
    gg_obj <- ggplot2::ggplot(data = plot_df) +
      ggplot2::geom_line(ggplot2::aes(x = t, y = y_mean, col = chain_id)) +
      ggplot2::geom_ribbon(ggplot2::aes(x = t, ymin = y_lower, ymax = y_upper, group = chain_id), alpha = 0.15) +
      ggplot2::facet_grid(beta_coef~., scales = "free") +
      ggplot2::theme(strip.text = ggplot2::element_text(angle = 0)) + 
      ggplot2::theme_bw() +
      ggplot2::xlab("Scaled Time") + 
      ggplot2::ylab("Coefficient value")
    
    return(gg_obj)
    
  }
  
  # if we excute the following code, we are combining the chains.
  beta_samples <- rstan::extract(model_fit, beta_strings)[[1]]
  
  plot_df <- do.call(rbind, lapply(as.list(1:6), function(x) {
    temp_array <- beta_samples[,, as.numeric(x)]
    inner_df <- data.frame(
      t = stan_data_list$x_mat[, 2],
      y_mean = apply(temp_array, 2, mean),
      y_lower = apply(temp_array, 2, quantile, interval_quantile),
      y_upper = apply(temp_array, 2, quantile, 1 - interval_quantile),
      beta_coef = x - 1
    )
  }))  
  
  plot_df$beta_coef <- as.factor(plot_df$beta_coef)
  
  gg_obj <- ggplot2::ggplot(data = plot_df) +
    ggplot2::geom_line(ggplot2::aes(x = t, y = y_mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(x = t, ymin = y_lower, ymax = y_upper), alpha = 0.2) +
    ggplot2::facet_grid(beta_coef ~ ., scales = "free") + 
    ggplot2::theme_bw() + 
    ggplot2::xlab("Scaled Time") + 
    ggplot2::ylab("Coefficient value")
  
  return(gg_obj)  
}

# Wed Jul 11 12:44:22 2018 ------------------------------
# spline fitted curve function

#' Plot the fittedn curves from the spline model
#' @param model_fit a "stanfit" object of the latent spline model
#' @param stan_data_list the stan data list associated with the model_fit
#' @param times_to_plot a vector of time points to plot the fits for.
#' @param split_by_chain should the plots be split by chain? If TRUE, then
#' they will be, if FALSE the chains will be combined
#' @param interval_quantile what is the lower quantile of the interval of 
#' interest (0.025 = 95\% intervals)
#' @return a ggplot2 object
#' @export
plot_spline_fit <- function(model_fit, 
                            stan_data_list,
                            times_to_plot = 1:4, 
                            split_by_chain = TRUE,
                            interval_quantile = 0.025,
                            add_sigma_y = FALSE) {
  mu_strings <- grep("^mu_fit", model_fit@model_pars, value = TRUE)
  
  point_df <- data.frame(
    x = rep(stan_data_list$z, length(times_to_plot)),
    y = as.vector(t(stan_data_list$y[times_to_plot,])),
    time_point = as.factor(rep(times_to_plot, each = length(stan_data_list$z)))
  )
  
  if (split_by_chain) {
    fitted_samples <- rstan::extract(model_fit, pars = mu_strings, permuted = !split_by_chain)
    array_dim <- c(
      iterations = model_fit@sim$iter - model_fit@sim$warmup,
      chains = length(model_fit@stan_args),
      time_points = stan_data_list$n_t,
      fitted_values = stan_data_list$n_new
    )
    
    fitted_samples <- array(
      data = as.vector(fitted_samples),
      dim = array_dim
    )
    
    if (add_sigma_y) {
      sigma_y_samples <- rstan::extract(model_fit, pars = "sigma_y", permuted = FALSE)
      
      noise_samples <- array(
        data = 0,
        dim = dim(fitted_samples)
      )
      
      for (ii in 1:array_dim["iterations"]) {
        for (cc in 1:array_dim["chains"]) {
          noise_samples[ii, cc, , ] <- matrix(
            data = rnorm(
              n = array_dim["time_points"] * array_dim["fitted_values"],
              mean = 0,
              sd = sigma_y_samples[ii, cc, 1]
            ),
            nrow = array_dim["time_points"],
            ncol = array_dim["fitted_values"]
          )
        }
      }
      
      fitted_samples <- fitted_samples + noise_samples
      
    }
    
    
    plot_df <- dplyr::bind_rows(lapply(times_to_plot, function(time) {
      dplyr::bind_rows(lapply(1:array_dim["chains"], function(chain) {
        temp_array <- fitted_samples[, chain, time, ]
        res_df <- data.frame(
          x = stan_data_list$z_new,
          y_mean = apply(temp_array, 2, mean),
          y_lower = apply(temp_array, 2, quantile, interval_quantile),
          y_upper = apply(temp_array, 2, quantile, 1 - interval_quantile),
          chain_id = chain,
          time_point = time
        )
      }))
    }))
    
    plot_df$chain_id <- as.factor(plot_df$chain_id)
    plot_df$time_point <- as.factor(plot_df$time_point)
    
    gg_obj <- ggplot2::ggplot(data = plot_df) +
      ggplot2::geom_point(data = point_df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(ggplot2::aes(x = x, y = y_mean, col = chain_id)) +
      ggplot2::geom_ribbon(ggplot2::aes(x = x, ymin = y_lower, ymax = y_upper, group = chain_id), alpha = 0.15) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(time_point ~ .) +
      ggplot2::xlab("Depth (m)") + 
      ggplot2::ylab("Density")
    
    return(gg_obj)
    
  }
  
  # if we get to here, we are not splitting by chain
  fitted_samples <- rstan::extract(model_fit, pars = mu_strings)[[1]]
  
  if (add_sigma_y) {
    sigma_y_samples <- rstan::extract(model_fit, pars = "sigma_y")[[1]]
    
    fitted_dim <- dim(fitted_samples)
    names(fitted_dim) <- c("iterations", "time_points", "fitted_values")
    
    noise_samples <- array(
      data = 0,
      dim = fitted_dim
    )
    
    for (ii in 1:fitted_dim["iterations"]) {
      noise_samples[ii, , ] <- matrix(
        data = rnorm(
          n = fitted_dim["time_points"] * fitted_dim["fitted_values"],
          mean = 0,
          sd = sigma_y_samples[ii]
        ),
        nrow = fitted_dim["time_points"],
        ncol = fitted_dim["fitted_values"]
      )  
    }
    
    fitted_samples <- fitted_samples + noise_samples
    
  }
  
  
  plot_df <- dplyr::bind_rows(lapply(times_to_plot, function(time) {
    temp_array <- fitted_samples[, time, ]
    res_df <- data.frame(
      x = stan_dat$z_new,
      y_mean = apply(temp_array, 2, mean),
      y_lower = apply(temp_array, 2, quantile, interval_quantile),
      y_upper = apply(temp_array, 2, quantile, 1 - interval_quantile),
      time_point = time
    )
  }))
  
  plot_df$time_point <- as.factor(plot_df$time_point)
  
  gg_obj <- ggplot(data = plot_df) +
    ggplot2::geom_point(data = point_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y_mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(x = x, ymin = y_lower, ymax = y_upper), alpha = 0.2) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(time_point ~ .) +
    ggplot2::xlab("Depth (m)") + 
    ggplot2::ylab("Density")
  
  return(gg_obj)
  
}
