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
ggsave(filename = "agePlot.jpg")

#prepare table for analysis of movies
mlDat_movie <- ddply(mlDat, ~movie_title + release_date + genre, summarize, mean_rating = mean(rating))
datesPlot <- ggplot(mlDat_movie, aes(release_date)) + geom_histogram(aes(y=..density..), binwidth=500, colour="black", fill="white")
#alter axis
datesPlot <- datesPlot + geom_density(alpha=.2, fill="#FF6666")
print(datesPlot)
ggsave(filename = "datesPlot.jpg")




# enna pannalam idhuku approm !

#Profession based analysis block block graph

#sorts by number of users
userPlot <- ggplot(mlDat_user, aes(x=reorder(occupation,occupation,
                                           function(x)-length(x)), fill = gender)) + geom_bar()

#fix axis
userPlot <- userPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
userPlot <- userPlot + ylab("number of users") + xlab("occupation")

#flip axis to make professions easier to read
#userPlot <- userPlot + coord_flip()

ggsave(filename = "userPlot.jpg")

gender_dat <- ddply(mlDat_user, ~occupation, summarize, perc_male = (length(gender[gender == "M"])/length(gender)), counts = -length(user_id))

#sorts by number of users
genderPlot <- ggplot(gender_dat, aes(x=reorder(occupation, counts), perc_male)) + geom_bar(stat="identity")
#fix axis
genderPlot <- genderPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
genderPlot <- genderPlot + ylab("percent male") + xlab("occupation")



#flip axis to make professions easier to read
#genderPlot <- genderPlot + coord_flip()

ggsave(filename = "genderPlot.jpg")


################

rankPlot <- ggplot(mlDat_user, aes(x=reorder(occupation,occupation,
                                            function(x)-length(x)), mean_rating)) + geom_violin()
#fix axis
rankPlot <- rankPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
rankPlot <- rankPlot + ylab("Average rating on Movies") + xlab("occupation")
#flip axis to make professions easier to read
#rankPlot <- rankPlot + coord_flip()

ggsave(filename = "rankPlot.jpg")

######################### genre analysis

genreCountPlot <- ggplot(mlDat_movie, aes(x=reorder(genre,genre,
                                           function(x)-length(x)))) + geom_bar()
#fix axis
genreCountPlot <- genreCountPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
genreCountPlot <- genreCountPlot + ylab("number of movies") + xlab("genre")
genreCountPlot <- genreCountPlot + coord_flip()
print(genreCountPlot)
ggsave(filename = "genreCountPlot.jpg")


######################## genre analysis - multiple

mlDat_movie_multi <- ddply(mlDat_multi, ~movie_title + release_date + genre, summarize, mean_rating = mean(rating))

genreCountPlot_multi <- ggplot(mlDat_movie_multi, aes(x=reorder(genre,genre,
                                           function(x)-length(x)))) + geom_bar()
#fix axis
genreCountPlot_multi <- genreCountPlot_multi + theme(axis.text.x = element_text(angle = 90, hjust = 1))
genreCountPlot_multi <- genreCountPlot_multi + ylab("number of movies") + xlab("genre")
genreCountPlot_multi <- genreCountPlot_multi + coord_flip()
print(genreCountPlot_multi)
ggsave(filename = "genreCountPlot_multi.jpg")


##################### gender based analysis whether this affects or not shit !



#mlDat_avgRating <- ddply(mlDat, ~genre, summarize, gender = "Both", rating = mean(rating))
mlDat_gender <- ddply(mlDat, ~genre + gender, summarize, rating = mean(rating))
#mlDat_gender <- rbind(mlDat_gender, mlDat_avgRating)

genderRatingPlot <- ggplot(mlDat_gender, aes(genre, rating)) + geom_histogram(stat="identity")
genderRatingPlot <- genderRatingPlot + facet_wrap(~ gender)
#fix axis
genderRatingPlot <- genderRatingPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
genderRatingPlot <- genderRatingPlot + coord_flip()
print(genderRatingPlot)
ggsave(filename = "genderRatingPlot_f_m.jpg")

#### both with avg rating point

mlDat_avgRating <- ddply(mlDat, ~genre, summarize, gender = "Both", rating = mean(rating))
mlDat_gender <- ddply(mlDat, ~genre + gender, summarize, rating = mean(rating))
mlDat_gender <- rbind(mlDat_gender, mlDat_avgRating)

genderRatingPlot <- ggplot(mlDat_gender, aes(genre, rating)) + geom_histogram(stat="identity")
genderRatingPlot <- genderRatingPlot + facet_wrap(~ gender)
#fix axis
genderRatingPlot <- genderRatingPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
genderRatingPlot <- genderRatingPlot + coord_flip()
print(genderRatingPlot)
ggsave(filename = "genderRatingPlot_f_m_both.jpg")


###############  heatmap pannalamaa ????????


mlDat_genre_occup <- ddply(mlDat, ~genre + occupation, summarize, mean_rating = mean(rating))

mlDat_genre_occup <- droplevels(subset(mlDat_genre_occup, occupation != "homemaker"))
mlDat_genre_occup <- droplevels(subset(mlDat_genre_occup, genre != "unknown"))
mlDat_genre_occup <- droplevels(subset(mlDat_genre_occup, genre != "Fantasy"))
mlDat_genre_occup <- droplevels(subset(mlDat_genre_occup, occupation != "none"))
mlDat_genre_occup <- droplevels(subset(mlDat_genre_occup, genre != "War"))
mlDat_genre_occup <- droplevels(subset(mlDat_genre_occup, occupation != "doctor"))


heatMapPalette <- colorRampPalette(rev(brewer.pal(4, "RdBu")))

#get data ready for heatmap
goHeat <- ggplot(mlDat_genre_occup, aes(x = genre, y = occupation, fill = mean_rating))
#rotate labels
goHeat <- goHeat + geom_tile() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#add colours
goHeat <- goHeat +  scale_fill_gradientn(colours = heatMapPalette(100))
#change background
goHeat <- goHeat + theme(panel.background = element_rect(fill='black'), 
                         panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank())
print(goHeat)
ggsave(filename = "genreOccupHeatMap.jpg")


###################33

mlDat_genre_occup <- ddply(mlDat_multi, ~genre + occupation, summarize, mean_rating = mean(rating))

#choose divergent colour tones to try and make distintions between like and dislike
heatMapPalette <- colorRampPalette(rev(brewer.pal(4, "RdBu")))

#get data ready for heatmap
goHeat2 <- ggplot(mlDat_genre_occup, aes(x = genre, y = occupation, fill = mean_rating))
#rotate labels
goHeat2 <- goHeat2 + geom_tile() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#add colours
goHeat2 <- goHeat2 +  scale_fill_gradientn(limits = c(2.5,4.6), colours = heatMapPalette(100))
#change background
goHeat2 <- goHeat2 + theme(panel.background = element_rect(fill='black'), 
                         panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank())
print(goHeat2)
ggsave(filename = "genreOccupHeatMap_multi.jpg")

