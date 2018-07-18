#' First 6 months of data
#'
#' Subtitle
#'
#' @format A list including 11 pressures, and ~737 measurements there of
"Crux_KP150_Phs1"

#' Second 6 months of data
#'
#' Subtitle
#'
#'@format A list including 12 pressures, and ~737 measurements there of
"Crux_KP150_Phs2"

#' Cholesky decomposition of the empirical correlation matrix of the fully 
#' independent, single tanh fits. 
#'
#' For the First 6 months of data, "Crux_KP150_Phs1"
#' 
#' @format an 11 x 11 lower triangular matrix, suitable for passing directly
#' into Stan's "cholesky_factor_corr" type.
"indep_fit_chol_emp_cor_mat"

#' The initial amplitude data, for both time series, with the zeros removed.
#' 
#' Note that the removal of the zero time points (due to filtering) means the
#' indices don't line up with the Crux* objects, if you want to get certain time
#' points, match by date (coerce both via as as.POSIXct). If you really wanted
#' to you could match via rownames (they technically still match).
#' 
#' @format a 1473 x 4 data.frame
"a0_data_no_zeros"


#' The cholesky needed to approximately sample the posterior around a 
#' curve.
#' 
#' This cholesky is found by getting the correlation matrix for the mu_{beta}
#' samples, and combining it with the average (over time) standard deviation
#' for each of the coefficients
#' 
#' @format a  6 x 6 lower triangular matrix
"approx_post_chol"
