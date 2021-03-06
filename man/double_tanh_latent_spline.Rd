% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/double-tanh-latent-spline.R
\name{double_tanh_latent_spline}
\alias{double_tanh_latent_spline}
\title{Fit the double tanh model, with a latent spline over time on the beta
coefficients.}
\usage{
double_tanh_latent_spline(stan_data_list, ...)
}
\arguments{
\item{stan_data_list}{A named list of inputs to the Stan model, see details}

\item{...}{other options that will be passed to
\code{\link[rstan:stanmodel-method-sampling]{sampling}}}
}
\value{
a "stanfit" object
}
\description{
Fit the double tanh model, with a latent spline over time on the beta
coefficients.
}
\details{
the \code{stan_data_list} object should have the following named
elements:
\itemize{
\item n_t : the number of timepoints in the data set.
\item n_z : the number of depths the data is measured at.
\item z : vector of depths at which data is obtained, should be of length n_z
and not rescaled.
\item y : an n_t by n_z matrix of density observations.
\item n_k : one less than the number of knots you would like to have for the
spline.
\item x_mat : an n_t by 2 matrix, where the first column is just 1s, and the
second column is the time vector rescaled to be between zero and one,
usually this is evenly spaced.
\item z_mat : an n_t by n_k + 1 matrix of spline bases, formed by calling
\code{\link[splines]{bs}} in the correct manner.
\item n_new : number of new z_values to sample the fitted curve posterior at.
\item z_new: vector of new z_values to sample the fitted curve posterior at.
}
}
\examples{
\dontrun{
library(ddcurves2)
library(rstan)
library(bayesplot)
library(ggplot2)

density_mat <- Crux_KP150_Phs1$density_mat

n_t <- nrow(density_mat)
n_z <- ncol(density_mat)
z <- Crux_KP150_Phs1$depths
y <- density_mat

# make the spline bases
# the we choose the number of knots in here
x_mat <- seq(from = 0, to = 1, length.out = n_t)
n_k <- 25
sp_knots <- seq(from = 0, to = 1, length.out = n_k)
z_mat <- splines::bs(
  x_mat,
  knots = sp_knots[-c(1, length(sp_knots))],
  Boundary.knots = sp_knots[c(1, length(sp_knots))],
  intercept = FALSE
)
x_mat <- cbind(1, x_mat)
n_k <- ncol(z_mat)

n_new <- 100
z_new <- seq(from = Crux_KP150_Phs1$max_depth - 5, to = 0, length.out = n_new)

stan_dat <- list(
  n_t = n_t,
  n_z = n_z,
  z = z,
  y = y,
  n_k = n_k,
  x_mat = x_mat,
  z_mat = z_mat,
  n_new = n_new,
  z_new = z_new
)

model_fit <- double_tanh_latent_spline(
  stan_data_list = stan_dat,
  iter = 900,
  warmup = 600,
  cores = 3,
  chains = 3,
  refresh = 20,
  verbose = TRUE,
  save_warmup = FALSE,
  control = list(max_treedepth = 14, stepsize = 1e-6, adapt_delta = 0.995),
  init_r = 0.25
)
}

}
