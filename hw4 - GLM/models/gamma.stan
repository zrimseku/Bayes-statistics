data {
  int<lower=1> n; // total number of data points
  int<lower=1> m; // number of predictors
  matrix[n, m] X; // independent variables
  vector[n] y;    // dependent variable
}

parameters {
  vector[m] b;         // coefficients
  real<lower=0> sigma; // stdev
}
transformed parameters {
  vector[n] mu; //the expected values (linear predictor)
  mu <- exp(X*b); //using the log link
}
model {
  // priors
  b[1] ~ cauchy(0, 10);
  b[2:] ~ cauchy(0, 2.5);

  // model
  y ~ gamma(mu .* mu / sigma, mu / sigma);
}
