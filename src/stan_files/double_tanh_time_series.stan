functions {
  real ar_next(real prev, int index, vector kappa, vector phi, vector innovation) {
    real result;
    result = kappa[index] + (phi[index] * prev) + innovation[index];
    return(result);
  }
}

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
  real<lower = 0> beta_zero_t0;
  real<lower = 0> beta_one_t0;
  real<lower = 0> beta_three_t0;
  real<lower = 0> beta_six_t0;

  // beta_two and beta_five
  // need to be ordered for identifiability.
  positive_ordered[2] beta_midpoint_t0;

  // ar parameters
  // they are all positive (even though the _could_ be negative) as there is
  // zero empircal chance they are negative
  vector<lower = 0, upper = 1>[6] phi;
  
  // ar constant, might require stabilising.
  // these processes all have positive mean, so kappa must be positive
  vector<lower = 0>[6] kappa;

  // ar error distribution
  vector<lower = 0>[6] beta_sigma;

  // innovation container
  // I don't know how to make these innovations satisfy the positive_ordered
  // constraint that we require????
  matrix[n_times - 1, 6] innovations;


  real<lower = 0> sigma_curve;
}

transformed parameters {
  matrix[n_times, n_depths] fitted_values;
  
  real<lower = 0> beta_zero[n_times];
  real<lower = 0> beta_one[n_times];
  real<lower = 0> beta_three[n_times];
  real<lower = 0> beta_six[n_times];

  positive_ordered[2] beta_midpoint[n_times];

  beta_zero[1] = beta_zero_t0;
  beta_one[1] = beta_one_t0;
  beta_three[1] = beta_three_t0;
  beta_six[1] = beta_six_t0;  

  beta_midpoint[1, 1] = beta_midpoint_t0[1];
  beta_midpoint[1, 2] = beta_midpoint_t0[2];

  for (tt in 1:n_times) {
    if (tt >= 2) {
      beta_zero[tt] = ar_next(beta_zero[tt - 1], 1, kappa, phi, innovations[tt - 1, ]');
      beta_one[tt] = ar_next(beta_one[tt - 1], 2, kappa, phi, innovations[tt - 1, ]');
      beta_midpoint[tt, 1] = ar_next(beta_midpoint[tt - 1, 1], 3, kappa, phi, innovations[tt - 1, ]');
      beta_three[tt] = ar_next(beta_three[tt - 1], 4, kappa, phi, innovations[tt - 1, ]');
      beta_midpoint[tt, 2] = ar_next(beta_midpoint[tt - 1, 2], 5, kappa, phi, innovations[tt - 1, ]');
      beta_six[tt] = ar_next(beta_six[tt - 1], 6, kappa, phi, innovations[tt - 1, ]');
    }
   fitted_values[tt,] = (beta_zero[tt] - beta_one[tt] * (tanh((depths + beta_midpoint[tt, 1]) / beta_three[tt]) + tanh((depths + beta_midpoint[tt, 2]) / beta_six[tt])))'; 
  }
}

model {
  for (tt in 1:n_times) {
    // vectorwise over individuals.
    densities[tt,] ~ normal(fitted_values[tt,], sigma_curve);

     if (tt >= 2) {
      real a_beta_zero = -(kappa[1] + phi[1] * beta_zero[tt - 1]);
      real a_beta_one = -(kappa[2] + phi[2] * beta_one[tt - 1]);
      real a_beta_three = -(kappa[4] + phi[4] * beta_three[tt - 1]);
      real a_beta_six = -(kappa[6] + phi[6] * beta_six[tt - 1]);


      // oredered issues
      real a_beta_mid_one = -(kappa[3] + phi[3] * beta_midpoint[tt - 1, 1]);
      // I highly doubt this will work as there is lhs = rhs dependency induced here.
      real b_beta_mid_one = -(kappa[3] + phi[3] * beta_midpoint[tt - 1, 1]) + (kappa[5] + phi[5] * beta_midpoint[tt - 1, 2] + innovations[tt - 1, 5]);
      real a_beta_mid_two = -(kappa[5] + phi[5] * beta_midpoint[tt - 1, 2] + innovations[tt - 1, 3]) + (kappa[3] + phi[3] * beta_midpoint[tt - 1, 1]);

      innovations[tt - 1, 1] ~ normal(0, beta_sigma[1]) T[a_beta_zero, positive_infinity()];
      innovations[tt - 1, 2] ~ normal(0, beta_sigma[2]) T[a_beta_one, positive_infinity()];
      innovations[tt - 1, 4] ~ normal(0, beta_sigma[4]) T[a_beta_three, positive_infinity()];
      innovations[tt - 1, 6] ~ normal(0, beta_sigma[6]) T[a_beta_six, positive_infinity()];
      
      // this is much much harder
      innovations[tt - 1, 3] ~ normal(0, beta_sigma[3]) T[a_beta_mid_one, b_beta_mid_one];
      innovations[tt - 1, 5] ~ normal(0, beta_sigma[5]) T[a_beta_mid_two, positive_infinity()];   

    }
  }

  // ar priors
  phi ~ beta(3, 1);
  // I really have no idea what on earth this should be
  kappa ~ normal(100, 500);
  // this might be too tight? We shall see, again unsure as to the 
  // scale of these in this time series structure.
  beta_sigma ~ normal(0, 10);

  // initial value priors
  beta_zero_t0 ~ normal(1023, 2);
  beta_one_t0 ~ normal(1, 2);

  beta_three_t0 ~ normal(42, 3);
  beta_six_t0 ~ normal(45, 3);

  beta_midpoint_t0[1] ~ normal(50, 5);
  beta_midpoint_t0[2] ~ normal(150, 5);

  // It's tiny.
  sigma_curve ~ normal(0, 0.1);

}
