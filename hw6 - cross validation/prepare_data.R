library(readr)

first <- read.csv("2015.csv")
second <- read.csv("2016.csv")
third <- read.csv("2017.csv")
fourth <- read.csv("2018.csv")
fifth <- read.csv("2019.csv")

happiness <- read_csv("happiness.csv")


names <- c("Rank", "Country", "Happiness.Score", "Economy..GDP.per.Capita.", "Social", "Health..Life.Expectancy.",
           "Freedom", "Generosity", "Trust..Government.Corruption.")
colnames(fourth) <- names
colnames(fifth) <- names

first["Year"] <- 2015
second["Year"] <- 2016
third["Year"] <- 2017
fourth["Year"] <- 2018
fifth["Year"] <- 2019

regions <- full_join(first[,c("Country", "Region")], second[,c("Country", "Region")])
third <- left_join(third, regions)
fourth <- left_join(fourth, regions)
fifth <- left_join(fifth, regions)

take_columns <- Reduce(intersect, list(colnames(first), colnames(second), colnames(third), colnames(fourth), colnames(fifth)))

first <- first[take_columns]
second <- second[take_columns]
third <- third[take_columns]
fourth <- fourth[take_columns]
fifth <- fifth[take_columns]

all_data <- rbind(first, second, third, fourth, fifth)
colnames(all_data) <- c("Country","Region","Score","GDP","Life","Freedom","Trust","Generosity","Year")
all_data[,"Trust"] <- as.numeric(all_data[,"Trust"])

happiness <- happiness[c("country", "score", "year", "economy", "perceived_corruption")]
colnames(happiness) <- c("Country", "Score", "Year", "GDP", "Corruption", "Region")
happiness <- left_join(happiness, regions)

write.csv(happiness, file = "data/happiness_regions.csv", row.names = FALSE)

write.csv(all_data, file = "data/happiness_kaggle.csv", row.names = FALSE)

