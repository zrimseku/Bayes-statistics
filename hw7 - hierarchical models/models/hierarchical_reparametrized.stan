data {
  int<lower=0> n; // total number of measurements
  int<lower=0> m; // number of subjects
  int<lower=0,upper=1> r[n]; // results - success or fail
  int<lower=0> s[n]; // subject ids

}

parameters {
  // global parameters
  real<lower=0,upper=1> p0;
  real<lower=0> lambda;

  // success per subject
  vector<lower=0,upper=1>[m] p;
}

model {

  // percentage
  p ~ beta(p0 / lambda, (1 - p0) / lambda);

  // iterate over all measurements
  for (i in 1:n) {
    r[i] ~ bernoulli(p[s[i]]);
  }
}