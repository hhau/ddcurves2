data {
  // Number of depths
  int<lower = 0> n_depths;
  
  // Number of time points
  int<lower = 0> n_times;
  
  // Vector of depths measurements were taken at
  vector[n_depths] depths;
  
  // Matrix of measurements, each row is a set of measurements at one time point
  // These measurements must be on the ORIGINAL scale in order for the priors 
  // to make any kind of physical sense.
  matrix[n_times, n_depths] densities;
}

parameters {
  // Matrix of Z ~ N(0,1) Random variables that we are going to sample
  // matrix[n_times, 4] beta_raw;
  vector<lower = 1020, upper = 1030>[n_times] beta_zero;
  
  // now that we have a better identified model, these constraints might be
  // less sensible.
  vector<lower = 0>[n_times] beta_one;
  vector<lower = 0>[n_times] beta_two;
  vector<lower = 0>[n_times] beta_three;
  
  // regression coefficient means
  // vector[4] beta_mean;
  
  // because we can't do fancy constraints, have to enumerate them all out.
  real<lower = 1020, upper = 1030> mean_beta_zero;
  real<lower = 0> mean_beta_one;
  real<lower = 0> mean_beta_two;
  real<lower = 0> mean_beta_three;
  
  // variances of the regression coefficients
  vector<lower=0>[4] sigma_beta;
  
  // variance of the regression curve
  // real<lower = 0> sigma_curve;
  
  // 2018-03-26 we are switching to a correlated error structure.
  cholesky_factor_corr[n_depths] l_omega;
  // Here each depth has its own variance, which we may / may not want?
  // we can rep this later if we want the same variance at each depth.
  vector<lower=0>[n_depths] l_sigma;
  
}

transformed parameters {
  // matrix[n_times, 4] beta_final;
  matrix[n_times, n_depths] fitted_values;
  for (tt in 1:n_times) {
    // beta_final[tt,] = (beta_mean + beta_raw[tt,]' .* sigma_beta)';
    // vectorwise over individuals.
   fitted_values[tt,] = (beta_zero[tt] - beta_one[tt] * tanh((depths + beta_two[tt]) / beta_three[tt]))'; 
  }
  
}

model {
  // form the covariance matrix
  matrix[n_depths, n_depths] l_sigma_mat;
  l_sigma_mat = diag_pre_multiply(l_sigma, l_omega);
  
  // priors for covariance matrix
  l_sigma ~ normal(0, 4);
  l_omega ~ lkj_corr_cholesky(1);
  
  for (tt in 1:n_times) {
    // vectorwise over individuals.
    densities[tt,] ~ multi_normal_cholesky(fitted_values[tt,], l_sigma_mat);
    // beta_raw[tt, ] ~ normal(0, 1);
  }
  
  // elicited priors
  // beta_zero ~ normal(mean_beta_zero, sigma_beta[1]) T[0,];
  beta_one ~ normal(mean_beta_one, sigma_beta[2]);
  beta_two ~ normal(mean_beta_two, sigma_beta[3]);
  beta_three ~ normal(mean_beta_three, sigma_beta[4]);
  
  mean_beta_one ~ normal(3, 5) T[0,];
  mean_beta_two ~ normal(100, 10) T[0,];
  mean_beta_three ~ normal(80, 10) T[0,];

  // no extra truncation here because it is of type vector, and Stan does not
  // yet support truncation on vector types
  sigma_beta ~ normal(0, 5);
  // sigma_curve ~ normal(0, 1) T[0,];
}
