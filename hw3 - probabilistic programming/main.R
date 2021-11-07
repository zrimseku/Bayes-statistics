# load libraries
library(cmdstanr)  # for interfacing Stan
library(ggplot2)   # for visualizations
library(ggdist)    # for distribution visualizations
library(tidyverse) # for data prep
library(posterior) # for extracting samples
library(bayesplot) # for some quick MCMC visualizations
library(mcmcse)    # for comparing samples and calculating MCSE

# compile the model
model <- cmdstan_model("./models/multiple_linear.stan")
model_lin <- cmdstan_model("./models/linear.stan")

# prepare the data
data <- read.csv("./data/50_startups.csv", sep=",")

# filter
newyork <- filter(data, state == 'NewYork')
california <- filter(data, state == 'California')
florida <- filter(data, state == 'Florida')


# prepare input data
stan_data_ny <- list(n = nrow(newyork), m = 3, X = select(newyork, c('research', 'administration', 'marketing')),
                     y = newyork$profit)
stan_data_cal <- list(n = nrow(california), m = 3, X = select(california, c('research', 'administration', 'marketing')),
                     y = california$profit)
stan_data_flo <- list(n = nrow(florida), m = 3, X = select(florida, c('research', 'administration', 'marketing')),
                     y = florida$profit)

# fit
fit_ny <- model$sample(
  data = stan_data_ny,
  seed = 1
)

fit_cal <- model$sample(
  data = stan_data_cal,
  seed = 1
)

fit_flo <- model$sample(
  data = stan_data_flo,
  seed = 1
)

# diagnostics
mcmc_trace(fit_ny$draws("b"))
mcmc_trace(fit_cal$draws("b"))
mcmc_trace(fit_flo$draws("b"))

fit_ny$summary()
fit_cal$summary()
fit_flo$summary()


# predictions
br_ny <- fit_ny$draws("b[1]")
ba_ny <- fit_ny$draws("b[2]")
bm_ny <- fit_ny$draws("b[3]")

br_cal <- fit_cal$draws("b[1]")
ba_cal <- fit_cal$draws("b[2]")
bm_cal <- fit_cal$draws("b[3]")

br_flo <- fit_flo$draws("b[1]")
ba_flo <- fit_flo$draws("b[2]")
bm_flo <- fit_flo$draws("b[3]")

cat('New York: beta_research = ', mcse(br_ny)$est, '+-', mcse(br_ny)$se, ', beta_admin = ', mcse(ba_ny)$est, '+-',
     mcse(ba_ny)$se,  ', beta_marketing: ', mcse(bm_ny)$est, '+-', mcse(bm_ny)$se)
cat('California: beta_research = ', mcse(br_cal)$est, '+-', mcse(br_cal)$se, ', beta_admin = ', mcse(ba_cal)$est, '+-',
     mcse(ba_cal)$se,  ', beta_marketing: ', mcse(bm_cal)$est, '+-', mcse(bm_cal)$se)
cat('Florida: beta_research = ', mcse(br_flo)$est, '+-', mcse(br_flo)$se, ', beta_admin = ', mcse(ba_flo)$est, '+-',
     mcse(ba_flo)$se,  ', beta_marketing: ', mcse(bm_flo)$est, '+-', mcse(bm_flo)$se)

# plots

mcmc_areas(fit_ny$draws("b"), prob = 0.8) + scale_y_discrete(breaks=c("b[1]","b[2]","b[3]"), labels=c("research", "administration", "marketing"), limits=rev) + labs(title = 'New York') + xlim(c(0, 1.2))
mcmc_areas(fit_cal$draws("b"), prob = 0.8) + scale_y_discrete(breaks=c("b[1]","b[2]","b[3]"), labels=c("research", "administration", "marketing"), limits=rev) + labs(title = 'California') + xlim(c(0, 1.2))
mcmc_areas(fit_flo$draws("b"), prob = 0.8) + scale_y_discrete(breaks=c("b[1]","b[2]","b[3]"), labels=c("research", "administration", "marketing"), limits=rev) + labs(title = 'Florida') + xlim(c(0, 1.2))

# interpretation
mcse(br_ny > ba_ny && ba_ny > bm_ny)

mcse(br_cal + ba_cal + bm_cal > br_flo + ba_flo + bm_flo)
mcse(br_cal + ba_cal + bm_cal > br_ny + ba_ny + bm_ny)
mcse(br_flo + ba_flo + bm_flo > br_ny + ba_ny + bm_ny)


mcse(br_cal / (br_cal + ba_cal + bm_cal))
mcse(ba_cal / (br_cal + ba_cal + bm_cal))
mcse(bm_cal / (br_cal + ba_cal + bm_cal))

mcse(br_ny / (br_ny + ba_ny + bm_ny))
mcse(ba_ny / (br_ny + ba_ny + bm_ny))
mcse(bm_ny / (br_ny + ba_ny + bm_ny))

mcse(br_flo / (br_flo + ba_flo + bm_flo))
mcse(ba_flo / (br_flo + ba_flo + bm_flo))
mcse(bm_flo / (br_flo + ba_flo + bm_flo))