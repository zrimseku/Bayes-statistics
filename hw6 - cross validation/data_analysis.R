# load libraries
library(ggplot2)   # for visualizations

# prepare the data
data_kaggle <- read.csv("./data/happiness_kaggle.csv", sep=",")
data <- read.csv("./data/happiness_regions.csv", sep=",")

# Trust zgleda mal čudno
# ggplot(data, aes(x=data[,'Generosity'], group=data[,'Year']), fill=data[,'Year']) + geom_density(adjust=1.5, alpha=0.5)

cor(data_kaggle[c(1:489, 491:782), 3:9])

# Found out that we will only take life from data_kaggle additionaly
combined <- left_join(data, data_kaggle[,c('Country', 'Year', 'Life', 'Freedom')])
cor(combined[,c(2:5,7)])

data_interactions <- data
data_interactions['GC'] <- data['Corruption'] * data['GDP']
data_interactions['G-C'] <- 1 / data['Corruption'] * data['GDP']
data_interactions['GL'] <- combined['Life'] * data['GDP']

cor(data_interactions[,c(2:5, 7:9)])

# Found out that GDP / Corruption and GDP * Life are interaction with high correlation

# for (column in c('GDP', 'Corruption', 'Life', 'Trust', 'Generosity'))
plot(x=data[,'GDP'], y=data[,'Score'])
plot(x=data_interactions[,'G-C'], y=data_interactions[,'Score'])

write.csv(combined[, c(3, 1, 4, 7, 5, 6, 8, 2)], file = "data/happiness_final.csv", row.names = FALSE)