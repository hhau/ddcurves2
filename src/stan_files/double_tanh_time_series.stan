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
  // initial values for time.
  vector[6] beta_prime_t0;

  // ar parameters
  // they are all positive (even though the _could_ be negative) as there is
  // zero empircal chance they are negative
  // now that innovations are on the log scale, this assumption might need
  // revisiting.
  vector<lower = 0, upper = 1>[6] phi_prime;
  
  // ar constant, might require stabilising.
  // these processes all have positive mean, so kappa must be positive
  vector[6] kappa_prime;

  // ar error distribution
  vector<lower = 0>[6] beta_prime_sigma;

  // innovation container
  // I don't know how to make these innovations satisfy the positive_ordered
  // constraint that we require????
  matrix[n_times - 1, 6] innovations_prime;

  // error variance (original scale)
  real<lower = 0> sigma_curve;
}

transformed parameters {
  matrix[n_times, n_depths] fitted_values;
  matrix[n_times, 6] beta_orig;
  vector[6] t0_prior_means = log([1023, 1.1, 50, 42, 100, 45])';
  
  beta_orig[1, ] = exp(beta_prime_t0)';
  
  for (tt in 1:n_times) {
    if (tt >= 2) {
      beta_orig[tt, ] = exp(kappa_prime + phi_prime .* log(beta_orig[tt - 1,]') + innovations_prime[tt - 1,]')';
    }
    
    fitted_values[tt, ] = (beta_orig[tt, 1] - beta_orig[tt, 2] * (tanh((depths + beta_orig[tt, 3]) / beta_orig[tt, 4]) + 
                                                                  tanh((depths + beta_orig[tt, 3] + beta_orig[tt, 5]) / beta_orig[tt, 6])))';
  }
}

model {
  for (tt in 1:n_times) {  

    // print("tt:", tt);
    // print("fitted value: ", fitted_values[tt,]);

    densities[tt,] ~ normal(fitted_values[tt,], sigma_curve);

    if (tt >= 2) {
      innovations_prime[tt - 1, ] ~ normal(0, beta_prime_sigma);
      // print("innovations_prime: ", innovations_prime[tt - 1,]);
    }
  }

  // print("fitted value [1]: ", fitted_values[1,]);
  // print("fitted value [20]: ", fitted_values[20,]);
  // print("beta_orig [1]: ", beta_orig[1,]);
  // print("beta_orig [20]: ", beta_orig[20,]);

  // ar priors
  // that these should be closer to 1 than to zero. 
  phi_prime ~ beta(3, 1);

  // I really have no idea what on earth this should be
  kappa_prime ~ normal(0, 10);

  // this might be too tight? We shall see, again unsure as to the 
  // scale of these in this time series structure.
  beta_prime_sigma ~ normal(0, 5);

  // initial value priors
  // log scale
  beta_prime_t0 ~ normal(t0_prior_means, log(1.5));

  // It's tiny.
  sigma_curve ~ normal(0, 0.25);

}
