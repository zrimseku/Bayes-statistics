# load libraries
library(cmdstanr)  # for interfacing Stan
library(ggplot2)   # for visualizations
library(ggdist)    # for distribution visualizations
library(tidyverse) # for data prep
library(posterior) # for extracting samples
library(bayesplot) # for some quick MCMC visualizations
library(mcmcse)    # for comparing samples and calculating MCSE
library(HDInterval)
library(loo)


# modleli: linear(GDP), multilinear(vse - regije), multilinear(vse (odstrani NA regije)), multilinear(vse? + GL + G-C),
# kaksna druga oblika?

# model ------------------------------------------------------------------------
# compile the model
multi_linear <- cmdstan_model("./models/linear_deviance.stan")
interactions <- cmdstan_model("./models/linear_interactions.stan")
log_gdp_regions <- cmdstan_model("./models/log_regions.stan")

# data -------------------------------------------------------------------------
data <- read_csv("data/happiness_final.csv")
data[,'Intercept'] <- 1

y_full <- data$Score
X_lin <- data[, c('Intercept', 'GDP')]
X_multi <- data[, c('Intercept', 'GDP', 'Life', 'Corruption', 'Freedom')]

data_regions <- na.omit(data[, c('Intercept', 'GDP', 'Life', 'Corruption', 'Freedom','Region', 'Score')])
X_regions <- data_regions[, c('Intercept', 'GDP', 'Life', 'Corruption', 'Freedom', 'Region')]
y_regions <- data_regions$Score

X_regions$Region <- factor(X_regions$Region)
contrasts(X_regions$Region) <- contr.treatment(n_distinct(X_regions$Region))
X_regions <- model.matrix(~ GDP + Life + Corruption + Freedom + Region, X_regions)


# modeling ---------------------------------------------------------------------

# stan_data
models <- c('Linear', 'Multilinear', 'Regions', 'Interactions', 'Inter_regions', 'log_GDP')
indep_data <- list(X_lin, X_multi, X_regions, X_multi, X_regions, X_regions)
stan_data <- list()
for (i in 1:6) {
  X <- indep_data[[i]]
  if (i %in% c(3, 5, 6)) {y <- y_regions} else {y <- y_full}
  stan_data[[models[i]]] <- list(n=nrow(X), m=ncol(X)-1, X=X, y=y)
}

# storages
log_lik <- list()
df_aic <- data.frame(AIC=numeric(), Model=factor())
fits <- list()

for (i in 6:6) {
  # set order
  model_name <- models[i]
  stan <- stan_data[[model_name]]

  cat('_________________________________', model_name, '________________________________')

  if (i %in% c(4, 5)) {
    stan$n_int <- 2
    stan$inter <- c(2, 3, 2, -4)
    model <- interactions
  } else if (i == 6) {
    model <- log_gdp_regions
  } else {
    model <- multi_linear
  }

  # fit
  fit <- model$sample(
    data = stan,
    parallel_chains = 4,
    iter_warmup = 500,
    iter_sampling = 1000,
    seed = i^2
  )

  fits[[model_name]] <- fit

  # extract
  log_lik[[model_name]] <- fit$draws(c("log_lik"))
  df_ll <- as_draws_df(fit$draws(c("log_lik")))

  # remove unwanted columns
  # also cast to regular data frame to avoid some warnings later on
  df_ll <- data.frame(df_ll %>% select(-.chain, -.iteration, -.draw))

  # average per row and store
  n_par <- length(fit$b)
  df_aic <- rbind(df_aic, data.frame(AIC=-2*rowSums(df_ll) + 2*(n_par), Model=as.factor(model_name)))
  # df_aic <- rbind(df_aic, data.frame(AIC=-2*rowSums(df_ll) + 2*(m+1), Order=as.factor(m)))
}

# diagnostics
for (model_name in models) {
  fit <- fits[[model_name]]
  # traceplot
  trace_plot <- mcmc_trace(fit$draws(c("b", "sigma")))
  print(trace_plot)
  # summary
  summ <- fit$summary(c("b", "sigma"))
  print(summ)
}


# compare ----------------------------------------------------------------------
# AIC
df_aic_summary <- df_aic %>% group_by(Model) %>%
  summarize(mean_AIC=mean(AIC),
            hdi5=hdi(AIC, credMass=0.9)[1],
            hdi95=hdi(AIC, credMass=0.9)[2])

# plot
ggplot(data=df_aic_summary, aes(x=Model, y=mean_AIC)) +
  geom_point(shape=16, size=2) +
  geom_linerange(aes(ymin = hdi5, ymax = hdi95), alpha=0.3) +
  xlab("Model") +
  ylab("AIC")

# WAIC
df_waic <- data.frame(WAIC=numeric(), SE=numeric(), Model=factor())

for (model_name in models) {
  waic <- waic(log_lik[[model_name]])
  df_waic <- rbind(df_waic, data.frame(waic=waic$estimates[3,1],
                                       SE=waic$estimates[3,2],
                                       Model=as.factor(model_name)))   # as.factor model_name???
}

# plot
ggplot(data=df_waic, aes(x=Model, y=waic)) +
  geom_point(shape=16, size=2) +
  geom_linerange(aes(ymin = (waic-SE), ymax = (waic+SE)), alpha=0.3) +
  xlab("Model") +
  ylab("WAIC")


# averaging
# calculate delta_waic
df_waic$delta_waic <- abs(df_waic$waic - min(df_waic$waic))

# calculate weights
df_waic$weight <- exp(-0.5 * df_waic$delta_waic) / sum(exp(-0.5 * df_waic$delta_waic))
df_waic$weight <- round(df_waic$weight, 2)

# plot
ggplot(data=df_waic, aes(x=Model, y=weight)) +
  geom_bar(stat="identity", fill="skyblue") +
  xlab("Model") +
  ylab("Akaike weight") +
  theme_minimal() +
  ylim(0, 1)

# LOOIC
df_loo <- data.frame(loo=numeric(), SE=numeric(), Model=factor())

for (model_name in models) {
  r_eff <- relative_eff(log_lik[[model_name]])
  loo <- loo(log_lik[[model_name]], r_eff=r_eff)
  df_loo <- rbind(df_loo, data.frame(loo=loo$estimates[3,1],
                                      SE=loo$estimates[3,2],
                                      Model=as.factor(model_name)))
}

# plot
ggplot(data=df_loo, aes(x=Model, y=loo)) +
  geom_point(shape=16, size=2) +
  geom_linerange(aes(ymin = (loo-SE), ymax = (loo+SE)), alpha=0.3) +
  xlab("Model") +
  ylab("LOOIC")


# averaging LOOIC
# calculate delta_loo
df_loo$delta_loo <- abs(df_loo$loo - min(df_loo$loo))

# calculate weights
df_loo$weight <- exp(-0.5 * df_loo$delta_loo) / sum(exp(-0.5 * df_loo$delta_loo))
df_loo$weight <- round(df_loo$weight, 2)

# plot
ggplot(data=df_loo, aes(x=Model, y=weight)) +
  geom_bar(stat="identity", fill="skyblue") +
  xlab("Model") +
  ylab("Akaike weight (LOOIC)") +
  theme_minimal() +
  ylim(0, 1)