functions {
  vector double_tanh(vector z, vector beta) {
    vector [num_elements(z)] res = beta[1] - beta[2] * (tanh((z + beta[3]) / beta[4]) + tanh((z + beta[3] + beta[5]) / beta[6]));
    return(res);
  }
}

data {
  int <lower = 0> n_t;
  int <lower = 0> n_z;
  vector [n_z] z;
  matrix [n_t, n_z] y;

  // spline terms
  int <lower = 0> n_k;
  matrix [n_t, 2] x_mat; // intercept and linear terms
  matrix [n_t, n_k] z_mat; // cubic spline deviations from the above

  // for fitted values, I'm lazy and want to do this all at once
  // there is a significant size / memory penalty for doing this inside stan
  int <lower = 0> n_new;
  vector [n_new] z_new;
}

transformed data {
  int n_beta = 6;
  // I can't do this on the rescaled-scale
  vector [n_beta] sp_intercept_offsets = [1020, 1, 90, 70, 15, 70]';
}

parameters {
  // intercept and slope terms
  vector [2] sp_beta_raw [n_beta];
  
  // spline coefficients (array with n_beta elements, where each element is 
  // an n_k long vector)
  vector [n_k] sp_u [n_beta];
  
  // each spline has its own sigma
  real <lower = 0> sigma_u [n_beta];


  // spline errors - unidentifiable mostly, don't add any additional width to the
  // posterior
  // vector [n_t] sp_error [n_beta];
  // real <lower = 0> sigma_sp_error [n_beta];

  // measurement error
  real <lower = 0> sigma_y;
}

transformed parameters {
  // there is a necessary lower bound at zero here, although there is not
  // mathematical mechanism to enforce this. If it is not present, the spline
  // is multimodal, and if we use the exp() function to ensure positivity, then
  // the model is numerically difficult. Hence, we just reject any values that
  // result in negative values for beta, and rely on stan's initialisation 
  // schema. Fortunately, the multiple modes are not "close" to each other.
  matrix <lower = 0>  [n_t, n_beta] beta;
  vector [2] sp_beta [n_beta];
  for (ii in 1:n_beta) {
    // arbitrary multiplier to make things a little more normal
    // better for initialisation.
    sp_beta[ii, 1] = sp_beta_raw[ii, 1] + sp_intercept_offsets[ii];
    sp_beta[ii, 2] = sp_beta_raw[ii, 2];

    beta[, ii] = x_mat * sp_beta[ii] + z_mat * sp_u[ii]; //+ sp_error[ii];

  }
}


model {
  // model level loop
  for (ii in 1:n_t) {
    vector [n_z] mu = double_tanh(z, beta[ii, ]');
    y[ii, ]' ~ normal(mu, sigma_y);
  }

  // spline level loop
  // use a random walk smoothing prior for the spline terms
  for (ii in 1:n_beta) {
    sp_u[ii, 1] ~ normal(0, 3);
    sp_beta_raw[ii, 1] ~ normal(0, 3);
    sp_beta_raw[ii, 2] ~ normal(0, 3);

    // this for-loop could be unfolded, and we could sample the errors
    // which may provide some speed up due to vectorisation, particularly for
    // n_k >= 30
    for (zz in 2:n_k) {
      sp_u[ii, zz] ~ normal(sp_u[ii, zz - 1], sigma_u[ii]);
    }

    sigma_u[ii] ~ normal(0, 1);
    // sp_error[ii] ~ normal(0, sigma_sp_error[ii]);

    
  }

  // the rest of them are scaled so should be easy to put default priors.
  // sigma_sp_error ~ gamma(3, 0.1);
  sigma_y ~ normal(0, 0.1);

}

generated quantities {
  matrix [n_t, n_new] mu_fit;
 
  for (ii in 1:n_t) {
    mu_fit[ii, ] = double_tanh(z_new, beta[ii, ]')';
  }

}
