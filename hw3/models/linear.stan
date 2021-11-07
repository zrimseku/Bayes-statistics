data {
  int<lower=1> n; // total number of data points
  vector[n] x;    // x values
  vector[n] y;    // y values
}

parameters {
  real<lower=0> b;              // beta
  real<lower=0> sigma; // stdev
}

model {
  // beta prior
  b ~ cauchy(0, 2.5);

  // model
  y ~ normal(x * b, sigma);
}