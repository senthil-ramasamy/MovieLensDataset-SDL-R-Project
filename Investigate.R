library(ggplot2)
library(plyr)
library(RColorBrewer)
library(grid)

#load single genre file
mlDat <- read.csv("Results/unifiedMLData.csv")

#fix dates field
mlDat$release_date <- as.Date(mlDat$release_date, "%Y-%m-%d")

#sanity checking
str(mlDat)

summary(mlDat)

head(mlDat)
tail(mlDat)

#load multi genre file
mlDat_multi <- read.csv("Results/unifiedMLDataMulti.csv")

#fix dates field
mlDat_multi$release_date <- as.Date(mlDat_multi$release_date, "%Y-%m-%d")

#sanity checking


str(mlDat_multi)
summary(mlDat_multi)
head(mlDat_multi)
tail(mlDat_multi)


#prepare table for analysis of users

mlDat_user <- ddply(mlDat, ~user_id + age + gender + occupation, summarize, mean_rating = mean(rating))
agePlot <- ggplot(mlDat_user, aes(age)) + geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white")

agePlot <- agePlot + geom_density(alpha=.2, fill="#FF6666")
print(agePlot)
ggsave(filename = "agePlot.pdf")

#prepare table for analysis of movies
mlDat_movie <- ddply(mlDat, ~movie_title + release_date + genre, summarize, mean_rating = mean(rating))
datesPlot <- ggplot(mlDat_movie, aes(release_date)) + geom_histogram(aes(y=..density..), binwidth=500, colour="black", fill="white")
#alter axis
datesPlot <- datesPlot + geom_density(alpha=.2, fill="#FF6666")
print(datesPlot)
ggsave(filename = "datesPlot.pdf")

