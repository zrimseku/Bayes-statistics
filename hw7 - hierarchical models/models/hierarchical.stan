data {
  int<lower=0> n; // total number of measurements
  int<lower=0> m; // number of subjects
  int<lower=0,upper=1> r[n]; // results - success or fail
  int<lower=0> s[n]; // subject ids

}

parameters {
  // global parameters
  real<lower=0,upper=1> p0;     // mean                     -> alpha / (alpha + beta)
  real<lower=0> tau;            // total count / precision  -> alpha + beta

  // success per subject
  vector<lower=0,upper=1>[m] p;
}

model {

  // percentage
  p ~ beta(p0*tau, (1 - p0)*tau);

  // iterate over all measurements
  for (i in 1:n) {
    r[i] ~ bernoulli(p[s[i]]);
  }
}