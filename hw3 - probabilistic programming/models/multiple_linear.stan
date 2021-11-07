data {
  int<lower=1> n; // total number of data points
  int<lower=1> m; // number of predictors
  matrix[n, m] X; // independent variables
  vector[n] y;    // dependent variable
}

parameters {
  vector<lower=0>[m] b;         // slope
  real<lower=0> sigma; // stdev
}

model {
  // priors
  b ~ cauchy(0, 2.5);

  // model
  y ~ normal(X * b, sigma);
}