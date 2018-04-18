#' Plot the fitted curves for the double tanh model.
#' 
#' Plots the fitted curves for a single time index, for each chain.
#' THIS FUNCTION IS VERY FRAGILE, AND IS LIKELY TO BREAK IF YOU CHANGE ANYTHING
#' IN THE MODEL STRUCTURE (BECAUSE IT REORDERS THE PARAMETERS). 
#' 
#' You will know if it breaks because the plot will look really whacky.
#' 
#'
#' @param stan_obj a 'stanfit' object for a model with two tanh components
#' @param index a time point in the original data set
#' @param stan_dat the data set
#' @param z_new a sorted vector of z (x) values to plot the curve over 
#' @param ... currently unused  
#'
#' @return a ggplot2 object of the fitted curve, split by chain, with credible
#' interval
#' @export
double_fitted_curve_plotter <- function(stan_obj, index, stan_dat,
                                        z_new = seq(from = -300, to = 0, length.out = 301),
                                        ...) {
  
  # extract relevant beta samples, and put them in appropriate data structure
  beta_strs <- grep("^beta*", stan_obj@model_pars, value = TRUE)
  
  # do we want a plot by chain option? If so we will need different extract 
  # methods? I think i actually want this as default (diagnostic)
  beta_samples <- rstan::extract(stan_obj, pars = beta_strs, permuted = FALSE)
  n_chains <- dim(beta_samples)[2]
  index_samples_by_chain <- list()
  
  for (ii in 1:n_chains) {
    # index_vec_one <- grep(paste0(".+\\[", as.character(index),"\\]"), names(beta_samples[1, 1, ]))
    # index_vec_two <- grep(paste0(".+\\[", as.character(index), ",", as.character(ii), "\\]"), names(beta_samples[1, 1, ]))
    index_vec_one <- which(endsWith(names(beta_samples[1, 1, ]), sprintf('[%d]', index)))
    
    # hacky shit for the midpoint array
    index_vec_two <- which(endsWith(names(beta_samples[1, 1, ]), sprintf('[%d,%d]', index, 1)))
    index_vec_three <- which(endsWith(names(beta_samples[1, 1, ]), sprintf('[%d,%d]', index, 2)))
    index_vec <- c(index_vec_one, index_vec_two, index_vec_three)
    subset_samples <- beta_samples[, ii, index_vec]
    index_samples_by_chain[[ii]] <- subset_samples
  }
  
  index_samples_by_chain <- lapply(index_samples_by_chain, function(x){
    x[,c(1,2,5,3,6,4)]
  }) 
  
  # this chunk will take some time, comment out if it's not worthwhile
  #  also assumes the correct number of beta's, and in the right order.
  chain_fited_curves <- lapply(index_samples_by_chain, function(x) {
    apply(x, 1, function(x) {
      .double_tanh_function(z_new, x)
    })
  })
  
  
  chain_means <- lapply(index_samples_by_chain, function(x) {
    apply(x, 2, mean)
  })
  
  # do we acidentally have beta_four in there somewhere?
  # definitely a hack, that shouldn't get run in newer models
  # if (any(unlist(lapply(chain_means, length)) > 6)) {
  #   chain_means <- lapply(chain_means, function(x){
  #     x[-grep("four", names(x))]
  #   })
  # }
  
  # hack to get things into the right order, because of the mid point bullshit
  # chain_means <- lapply(chain_means, function(x){
  #   x[c(1,2,5,3,6,4)]
  # }) 
  
  # plot_df <- data.frame(x = z_new)
  
  plot_df <- c()
  
  for (ii in 1:n_chains) {
    res <- apply(chain_fited_curves[[ii]], 1, mean)
    lower_quant <- apply(chain_fited_curves[[ii]], 1, quantile, 0.025)
    upper_quant <- apply(chain_fited_curves[[ii]], 1, quantile, 0.975)
    t_df <- cbind(x = z_new, ii, y = res, lower_quant, upper_quant)
    plot_df <- rbind(plot_df, t_df)
  }
  
  colnames(plot_df)[which(colnames(plot_df) == "ii")] <- "chain_id"
  plot_data_df <- data.frame(x = stan_dat$depths, y = as.numeric(stan_dat$densities[index,]))
  
  plot_df <- as.data.frame(plot_df)
  plot_df$chain_id <- as.factor(plot_df$chain_id)
  
  gg_obj <- ggplot2::ggplot() +
    ggplot2::geom_line(data = plot_df,
                       mapping = ggplot2::aes(x = x, y = y, colour = chain_id)) +
    ggplot2::geom_ribbon(data = plot_df,
                         mapping = ggplot2::aes(x = x, ymin = lower_quant, ymax = upper_quant, group = chain_id), alpha = 0.1) +
    ggplot2::geom_point(data = plot_data_df, 
                        mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(paste0("Time point: ", index)) + 
    ggplot2::xlab("Depth") +
    ggplot2::ylab("Density")
  
  return(gg_obj)
  
}
