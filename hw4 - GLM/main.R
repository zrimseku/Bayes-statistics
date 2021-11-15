# load libraries
library(cmdstanr)  # for interfacing Stan
library(ggplot2)   # for visualizations
library(ggdist)    # for distribution visualizations
library(tidyverse) # for data prep
library(posterior) # for extracting samples
library(bayesplot) # for some quick MCMC visualizations
library(mcmcse)    # for comparing samples and calculating MCSE
library(abind)

# compile the model
model <- cmdstan_model("./models/gamma.stan")

# prepare the data
data <- read.csv("./data/videogame_sales.csv", sep=",")

data$Platform <- factor(data$Platform)
contrasts(data$Platform) <- contr.treatment(n_distinct(data$Platform))  #, base=3)
X_genre <- model.matrix(~ Genre, data)
X_platform <- model.matrix(~ Platform, data)
y <- data$Sales

# plots for data analysis
# ggplot() + geom_density(aes(x=data[data$Platform == "PC", "Sales"], fill='PC'), alpha=0.5) + theme(legend.position=c(.9,.9)) +
#   geom_density(aes(x=data[data$Platform == "PS", "Sales"], fill='PS'), alpha=0.5) + scale_x_log10() +
#   geom_density(aes(x=data[data$Platform == "Xbox", "Sales"], fill='Xbox'), alpha = 0.5) + scale_fill_discrete(name = "Platform") + labs(x='Sales')
#
# ggplot() + geom_density(aes(x=data[data$Genre == "Shooter", "Sales"], fill='Shooter'), alpha=0.5) +
#   geom_density(aes(x=data[data$Genre != "Shooter", "Sales"], fill='RPG'), alpha=0.5) + scale_x_log10() +
#   scale_fill_discrete(name = "Genre") + labs(x='Sales') + theme(legend.position=c(.9,.9))

# prepare input data
stan_genre <- list(n = nrow(X_genre), m = ncol(X_genre), X = X_genre, y = y)
stan_platform <- list(n = nrow(X_platform), m = ncol(X_platform), X = X_platform, y = y)

# fit
fit_genre <- model$sample(
  data = stan_genre,
  seed = 1
)
fit_platform <- model$sample(
  data = stan_platform,
  seed = 1
)

# diagnostics
mcmc_trace(fit_genre$draws("b"))
mcmc_trace(fit_platform$draws("b"))

fit_genre$summary()
fit_platform$summary()


# predictions
genre_shooter <- exp(fit_genre$draws("b[1]") + fit_genre$draws("b[2]"))
genre_rpg <- exp(fit_genre$draws("b[1]"))
platform_ps <- exp(fit_platform$draws("b[1]") + fit_platform$draws("b[2]"))
platform_xbox <- exp(fit_platform$draws("b[1]") + fit_platform$draws("b[3]"))
platform_pc <- exp(fit_platform$draws("b[1]"))


cat('Genre: mu_shooter = ', mcse(genre_shooter)$est, '+-', mcse(genre_shooter)$se, ', mu_rpg = ', mcse(genre_rpg)$est, '+-',
     mcse(genre_rpg)$se,  ', mu_ps: ', mcse(platform_ps)$est, '+-', mcse(platform_ps)$se,
    ', mu_pc: ', mcse(platform_pc)$est, '+-', mcse(platform_pc)$se,
    ', mu_xbox: ', mcse(platform_xbox)$est, '+-', mcse(platform_xbox)$se)

# plots
variables(genre_rpg) <- 'RPG'
variables(genre_shooter) <- 'Shooter'
genre_mtx <- abind(genre_rpg, genre_shooter, along = 3)

variables(platform_xbox) <- 'Xbox'
variables(platform_ps) <- 'PS'
variables(platform_pc) <- 'PC'
platform_mtx <- abind(platform_pc, platform_ps, platform_xbox, along = 3)

mcmc_areas(genre_mtx, prob = 0.8) + labs(title = 'Genre') + xlim(c(0.5, 1))
mcmc_areas(platform_mtx, prob = 0.8) + labs(title = 'Platform') + xlim(c(0.5, 1.5))

# interpretation
mcse(genre_shooter > genre_rpg)
mcse(platform_xbox > platform_ps & platform_xbox > platform_pc)












# ggplot() + geom_histogram(aes(x=data[data$Platform == "PC", "Sales"], fill='PC'), alpha=0.5) +
#   geom_histogram(aes(x=data[data$Platform == "PS", "Sales"], fill='PS'), alpha=0.5) +scale_x_log10() +
#   geom_histogram(aes(x=data[data$Platform == "Xbox", "Sales"], fill='Xbox', alpha = 0.5)) + scale_fill_discrete(name = "Platform") + labs(x='Sales')
#


#
#

