

model <- cmdstan_model("./models/gamma.stan")


 # prepare the data
data <- read.csv("./data/videogame_sales.csv", sep=",")

data$Platform <- factor(data$Platform)
contrasts(data$Platform) <- contr.treatment(n_distinct(data$Platform))  #, base=3)
X <- model.matrix(~ Genre + Platform, data)
y <- data$Sales
# prepare input data
stan <- list(n = nrow(X), m = ncol(X), X = X, y = y)

# fit
fit <- model$sample(
  data = stan,
  seed = 1
)

mu_rpg_pc <- exp(fit$draws("b[1]"))
mu_rpg_ps <- exp(fit$draws("b[1]") + fit$draws("b[3]"))
mu_rpg_x <- exp(fit$draws("b[1]") + fit$draws("b[4]"))
mu_s_pc <- exp(fit$draws("b[1]") + fit$draws("b[2]"))
mu_s_ps <- exp(fit$draws("b[1]") + fit$draws("b[2]") + fit$draws("b[3]"))
mu_s_x <- exp(fit$draws("b[1]") + fit$draws("b[2]") + fit$draws("b[4]"))

mean(data[data$Platform == "PS" & data$Genre == "Shooter", "Sales"])
mean(data[data$Platform == "Xbox" & data$Genre == "Shooter", "Sales"])

mcse(mu_s_x > mu_s_ps)


# [1] 2.215294
# [1] 1.563939
# $est
# [1] 0.942
#
# $se
# [1] 0.004532157

ggplot() + geom_density(aes(x=data[data$Platform == "PC" & data$Genre == "Shooter", "Sales"], color='PC Shooter')) +
  geom_density(aes(x=data[data$Platform == "PS" & data$Genre == "Shooter", "Sales"], color='PS Shooter')) + scale_x_log10() +
  geom_density(aes(x=data[data$Platform == "Xbox" & data$Genre == "Shooter", "Sales"], color='Xbox Shooter')) + scale_color_discrete(name = "Type") + labs(x='Sales') +
  geom_density(aes(x=data[data$Platform == "PC" & data$Genre != "Shooter", "Sales"], color='PC RPG')) +
  geom_density(aes(x=data[data$Platform == "PS" & data$Genre != "Shooter", "Sales"], color='PS RPG')) + scale_x_log10() +
  geom_density(aes(x=data[data$Platform == "Xbox" & data$Genre != "Shooter", "Sales"], color='Xbox RPG'))
