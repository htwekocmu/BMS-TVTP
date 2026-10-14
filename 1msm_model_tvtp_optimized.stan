data {
  int<lower=1> T;
  int<lower=1> K_obs;
  int<lower=1> K_trans;
  matrix[T, K_obs] X_obs;
  matrix[T, K_trans] X_trans;
  vector[T] y;
  real mu_0;
  real<lower=0> sigma_0;
  real<lower=0> shape;
  real<lower=0> rate;
}

parameters {
  matrix[2, K_obs] beta;
  ordered[2] sigma;
  matrix[2, K_trans] delta;
}

transformed parameters {
  array[T] vector[2] log_lik;
  array[T] matrix[2, 2] Gamma;
  
  for (t in 1:T) {
    for (k in 1:2) {
      log_lik[t, k] = normal_lpdf(y[t] | dot_product(X_obs[t], beta[k]), sigma[k]);
    }
    
    real stay1 = inv_logit(dot_product(X_trans[t], delta[1]));
    real stay2 = inv_logit(dot_product(X_trans[t], delta[2]));
    
    Gamma[t, 1, 1] = stay1;
    Gamma[t, 1, 2] = 1 - stay1;
    Gamma[t, 2, 1] = 1 - stay2;
    Gamma[t, 2, 2] = stay2;
  }
}

model {
  to_vector(beta) ~ normal(mu_0, sigma_0);
  sigma ~ gamma(shape, rate);
  to_vector(delta) ~ normal(0, 1);
  
  // Forward algorithm for likelihood computation
  vector[2] log_alpha = log_lik[1];
  real log_p_y = log_sum_exp(log_alpha);
  
  log_alpha -= log_p_y;
  
  for (t in 2:T) {
    vector[2] log_alpha_t;
    for (j in 1:2) {
      log_alpha_t[j] = log_sum_exp(log_alpha + to_vector(log(Gamma[t, :, j]))) + log_lik[t, j];
    }
    log_p_y += log_sum_exp(log_alpha_t);
    log_alpha = log_alpha_t - log_sum_exp(log_alpha_t);
  }
  
  target += log_p_y;
}

generated quantities {
  array[T] int<lower=1, upper=2> z;
  vector[T] y_rep;
  vector[T] log_lik_dens;
  vector[T] log_lik_marginal;
  vector[2] pi_stationary;
  array[T] matrix[2, 2] Gamma_saved;  // Add this line to save Gamma
  
  // Copy Gamma to Gamma_saved for output
  for (t in 1:T) {
    Gamma_saved[t] = Gamma[t];
  }
  
  {
    // Compute stationary distribution from average transition matrix
    matrix[2, 2] Gamma_avg = rep_matrix(0, 2, 2);
    for (t in 1:T) {
      Gamma_avg += Gamma[t];
    }
    Gamma_avg /= T;
    
    // Solve pi * Gamma = pi with pi[1] + pi[2] = 1
    real d1 = Gamma_avg[1, 1] + Gamma_avg[2, 1];  // sum of first column
    real d2 = Gamma_avg[1, 2] + Gamma_avg[2, 2];  // sum of second column
    
    if (d1 > 1e-10 && d2 > 1e-10) {
      pi_stationary[1] = Gamma_avg[2, 1] / d1;
      pi_stationary[2] = Gamma_avg[1, 2] / d2;
      // Normalize to ensure sum = 1
      pi_stationary = pi_stationary / sum(pi_stationary);
    } else {
      pi_stationary = [0.5, 0.5]';
    }

    // Forward pass for marginal likelihood
    vector[2] log_alpha_temp = log(pi_stationary) + log_lik[1];
    real log_p_y_t = log_sum_exp(log_alpha_temp);
    log_lik_marginal[1] = log_p_y_t;
    log_alpha_temp -= log_p_y_t;

    for (t in 2:T) {
      vector[2] log_alpha_t;
      for (j in 1:2) {
        log_alpha_t[j] = log_sum_exp(log_alpha_temp + to_vector(log(Gamma[t, :, j]))) + log_lik[t, j];
      }
      log_p_y_t = log_sum_exp(log_alpha_t);
      log_lik_marginal[t] = log_p_y_t;
      log_alpha_temp = log_alpha_t - log_p_y_t;
    }
  }

  {
    // Backward sampling for state sequence
    vector[2] lp_initial = log(pi_stationary) + log_lik[1];
    z[1] = categorical_rng(softmax(lp_initial));
    y_rep[1] = normal_rng(dot_product(X_obs[1], beta[z[1]]), sigma[z[1]]);
    log_lik_dens[1] = normal_lpdf(y[1] | dot_product(X_obs[1], beta[z[1]]), sigma[z[1]]);

    for (t in 2:T) {
      int prev_state = z[t - 1];
      vector[2] trans_lp = to_vector(log(Gamma[t, prev_state, :]));
      z[t] = categorical_rng(softmax(trans_lp));
      y_rep[t] = normal_rng(dot_product(X_obs[t], beta[z[t]]), sigma[z[t]]);
      log_lik_dens[t] = normal_lpdf(y[t] | dot_product(X_obs[t], beta[z[t]]), sigma[z[t]]);
    }
  }
}