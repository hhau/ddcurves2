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
