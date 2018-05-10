data {
  int <lower = 0> N;
  vector [N] ssh;
  vector <lower = 0> [N] a_zero;
  
  int <lower = 0> N_ssh_new;
  vector [N_ssh_new] ssh_new; 
  
  int <lower = 0> poly_deg;
}

transformed data {
  vector [N] ssh_rescl;
  vector [N] a_zero_rescl;
  matrix [N, poly_deg + 1] x_mat_rescl;
  matrix [N, poly_deg + 1] x_mat_orig;
  
  vector [N_ssh_new] ssh_new_rescl;
  matrix [N_ssh_new, poly_deg + 1] x_mat_new_rescl;
  matrix [N_ssh_new, poly_deg + 1] x_mat_new_orig;
  
  real mean_ssh;
  real <lower = 0> sd_ssh;
  real mean_a_zero;
  real <lower = 0> sd_a_zero;
  // real mean_ssh_new;
  // real <lower = 0> sd_ssh_new;
  
  mean_ssh = mean(ssh);
  sd_ssh = sd(ssh);
  mean_a_zero =  mean(a_zero);
  sd_a_zero = sd(a_zero);
  // mean_ssh_new = mean(ssh_new);
  // sd_ssh_new = sd(ssh_new);

  ssh_rescl = (ssh - mean_ssh) / (sd_ssh);
  a_zero_rescl = (a_zero - mean_a_zero) / (sd_a_zero);
  ssh_new_rescl = (ssh_new - mean_ssh) / (sd_ssh);
  
  // not optimal, but don't want to figure out something better now, would be
  // wasting time
  for (ii in 1:N) {
    for (zz in 0:poly_deg) {
      x_mat_rescl[ii, zz + 1] = ssh_rescl[ii] ^ zz;
      x_mat_orig[ii, zz + 1] = ssh[ii] ^ zz;
    }
  }
  
  for (ii in 1:N_ssh_new) {
    for (zz in 0:poly_deg) {
      x_mat_new_rescl[ii, zz + 1] = ssh_new_rescl[ii] ^ zz;
      x_mat_new_orig[ii, zz + 1] = ssh_new[ii] ^ zz;
    }
  }
 
}


parameters {
  real <lower = 0> sigma;
  real <lower = 0> nu;
  vector [poly_deg + 1] beta;
}

transformed parameters {
  vector [N] mu;
  mu = x_mat_rescl * beta;
}


model {
  a_zero_rescl ~ student_t(nu, mu, sigma);
  nu ~ normal(50, 50);
  sigma ~ normal(0, 1);
  beta ~ normal(0, 1);
}

generated quantities {
  vector [N] mu_orig;

  vector [N_ssh_new] mu_new_rescl;
  // vector [N_ssh_new] mu_new_orig;
  vector [N_ssh_new] a_zero_new_rescl;
  vector <lower = 0> [N_ssh_new] a_zero_new_orig;
  
  mu_orig = (mu * sd_a_zero) + mean_a_zero;
  
  mu_new_rescl = x_mat_new_rescl * beta;
  // mu_new_orig = (mu_new_rescl * sd_a_zero) + mean_a_zero;
  
  for (ii in 1:N_ssh_new) {
    a_zero_new_rescl[ii] = student_t_rng(nu, mu_new_rescl[ii], sigma);
  }
  
  // nearly got me realllllll gooood
  // the decision to fabs here and not set to zero is an interesting one,
  // unsure how much of an effect it will have
  a_zero_new_orig = fabs((a_zero_new_rescl * sd_a_zero) + mean_a_zero);
  
}
