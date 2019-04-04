data {

  int<lower=0> J;            // number of trials
  real y[J];                 // sample estimate
  real<lower=0> sigma[J];    // sample sd

}

parameters {

  real mu;                  // true mean treatment effect
  real<lower=0> tau;        // scale of instantiated effects
  real theta_tilde[J];      // z of instantiated effect

}

transformed parameters {

  // Create the instantiated effect, unstd.
  real theta[J];
  for (j in 1:J)
    theta[j] = mu + tau * theta_tilde[j];

}

model {

  mu ~ normal(0, 5);
  tau ~ cauchy(0, 5);
  theta_tilde ~ normal(0, 1);
  y ~ normal(theta, sigma);

}
