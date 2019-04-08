data {

  int<lower=0> n_j;            // number of trials
  real y[n_j];                 // sample estimate
  real<lower=0> sigma[n_j];    // sample sd

}

parameters {

  real mu;                  // true mean treatment effect
  real<lower=0> tau;        // scale of instantiated effects
  real theta_tilde[n_j];      // z of instantiated effect

}

transformed parameters {

  // Create the instantiated effect, unstd.
  real theta[n_j];
  for (j in 1:n_j)
    theta[j] = mu + tau * theta_tilde[j];

}

model {

  mu ~ uniform(-1, 1);
  tau ~ cauchy(0, 1);
  theta_tilde ~ student_t(3, 0, 1);
  y ~ normal(theta, sigma);

}
