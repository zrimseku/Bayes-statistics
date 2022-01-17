library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)
library(mcmcse)
library(cowplot)

data <- read.csv('data/basketball_shots.csv', sep = ';')

data <- data[which(data$SpecialRim == 0), c("PlayerID", "Made", "Position")]

guards <- data[which(data$Position == 'G'),]
centers <- data[which(data$Position == 'C'),]
forwards <- data[which(data$Position == 'F'),]

# new IDs
guard_ids <- list()
i <- 1
for (id in unique(guards$PlayerID)) {
  guard_ids[[id]] <- i
  i <- i+1
}
guards$NewID <- as.integer(unlist(guard_ids[guards$PlayerID]))

center_ids <- list()
i <- 1
for (id in unique(centers$PlayerID)) {
  center_ids[[id]] <- i
  i <- i+1
}
centers$NewID <- as.integer(unlist(center_ids[centers$PlayerID]))

forward_ids <- list()
i <- 1
for (id in unique(forwards$PlayerID)) {
  forward_ids[[id]] <- i
  i <- i+1
}
forwards$NewID <- as.integer(unlist(forward_ids[forwards$PlayerID]))


# fitting models
model <- cmdstan_model("models/hierarchical_reparametrized.stan")

stan_guards <- list(n=nrow(guards),
                  m=max(guards$NewID),
                  r=guards$Made,
                  s=guards$NewID)

fit_guards <- model$sample(
  data = stan_guards,
  parallel_chains = 4,
  seed = 0
)

stan_centers <- list(n=nrow(centers),
                  m=max(centers$NewID),
                  r=centers$Made,
                  s=centers$NewID)

fit_centers <- model$sample(
  data = stan_centers,
  parallel_chains = 4,
  seed = 1
)

stan_forwards <- list(n=nrow(forwards),
                  m=max(forwards$NewID),
                  r=forwards$Made,
                  s=forwards$NewID)

fit_forwards <- model$sample(
  data = stan_forwards,
  parallel_chains = 4,
  seed = 2
)

# diagnostics
# mcmc_trace(fit_guards$draws())
# mcmc_trace(fit_centers$draws())
# mcmc_trace(fit_forwards$draws())

# With what certainty can you claim that the best guard (Player #20) is on average a better free throw shooter than
# the best forward (Player #3)?
p20_newid <- guards[which(guards$PlayerID == 20)[1], 'NewID']     # 9
p3_newid <- forwards[which(forwards$PlayerID == 3)[1], 'NewID']   # 1
best_guard_draws <- fit_guards$draws(paste0('p[', p20_newid ,']'))
best_forward_draws <- fit_forwards$draws(paste0('p[', p3_newid ,']'))
cat('Best guard is better than best forward with probability: \n')
mcse(best_guard_draws > best_forward_draws)

ggplot() + geom_density(aes(x=best_guard_draws, fill='Best guard'), alpha=0.5) +
  geom_density(aes(x=best_forward_draws, fill ='Best forward'), alpha=0.5) +
  scale_fill_discrete(name = "Player") + labs(x='Probability of making the shot')



# We take a random guard and a random forward, each of them shoots one free throw.
# What are the probabilities of
#     A) the guard winning (guard makes the shot, forward misses the shot),
#     B) the forward winning (guard misses the shot, forward makes the shot), and
#     C) a tie (both players either miss the shot or hit the shot).
#     Compute the probabilities for other combinations as well (guard vs. center and forward vs. center).

probs_guard_makes <- c()
probs_forward_makes <- c()
probs_center_makes <- c()

for (i in 1:1000) {
  rnd_g <- sample(1:19, 1)
  rnd_c <- sample(1:7, 1)
  rnd_f <- sample(1:19, 1)
  probs_guard_makes <- c(probs_guard_makes, sample(fit_guards$draws(paste0('p[', rnd_g ,']')), 1))
  probs_forward_makes <- c(probs_forward_makes, sample(fit_forwards$draws(paste0('p[', rnd_f ,']')), 1))
  probs_center_makes <- c(probs_center_makes, sample(fit_centers$draws(paste0('p[', rnd_c ,']')), 1))
}

# guard vs. forward
g_wins_f <- probs_guard_makes * (1 - probs_forward_makes)
f_wins_g <- (1 - probs_guard_makes) * probs_forward_makes
g_f_tie <- probs_guard_makes * probs_forward_makes + (1 - probs_guard_makes) * (1 - probs_forward_makes)

# guard vs. center
g_wins_c <- probs_guard_makes * (1 - probs_center_makes)
c_wins_g <- (1 - probs_guard_makes) * probs_center_makes
g_c_tie <- probs_guard_makes * probs_center_makes + (1 - probs_guard_makes) * (1 - probs_center_makes)

# forwars vs. center
f_wins_c <- probs_forward_makes * (1 - probs_center_makes)
c_wins_f <- (1 - probs_forward_makes) * probs_center_makes
f_c_tie <- probs_forward_makes * probs_center_makes + (1 - probs_forward_makes) * (1 - probs_center_makes)

cat('Guard wins forward: \n')
mcse(g_wins_f)

cat('Forward wins guard: \n')
mcse(f_wins_g)

cat('FG tie \n')
mcse(g_f_tie)

cat('____________________________________')

cat('Guard wins center: \n')
mcse(g_wins_c)

cat('Center wins guard: \n')
mcse(c_wins_g)

cat('CG tie \n')
mcse(g_c_tie)

cat('____________________________________')

cat('Center wins forward: \n')
mcse(c_wins_f)

cat('Forward wins center: \n')
mcse(f_wins_c)

cat('FG tie \n')
mcse(f_c_tie)

ggplot() + geom_density(aes(x=probs_guard_makes, fill='Guard'), alpha=0.5) +
  geom_density(aes(x=probs_center_makes, fill ='Center'), alpha=0.5) +
  geom_density(aes(x=probs_forward_makes, fill ='Forward'), alpha=0.5) +
  scale_fill_discrete(name = "Position") + labs(x='Probability of making the shot')


# second (and third) approach  --> wrong, it underestimates uncertainty
# probs_guard_makes <- sample(fit_guards$draws('p0'), 1000)
# probs_forward_makes <- sample(fit_forwards$draws('p0'), 1000)
# probs_center_makes <- sample(fit_centers$draws('p0'), 1000)
# probs_guard_makes <- fit_guards$draws('p0')
# probs_forward_makes <- fit_forwards$draws('p0')
# probs_center_makes <- fit_centers$draws('p0')
