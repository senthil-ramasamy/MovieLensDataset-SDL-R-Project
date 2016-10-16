#load all library files
library(data.table)
library(Matrix)
library(proxy)

#load the data files as movies and ratings
movies <- read.csv("~/Works/Rworks/Movielens/Recom/movies.csv", stringsAsFactors=FALSE)
str(movies)
ratings <- read.csv("~/Works/Rworks/Movielens/Recom/ratings.csv", stringsAsFactors=FALSE)
str(ratings)

# simple content-based recommender engine

# data preprocessing - pipe seperated genres - had to be split
genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)

#tstrsplit
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres2) <- c(1:6)
head(genres2)

# create a matrix with columns representing every unique genre

genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")

genre_matrix <- matrix(0,8571,18) #empty matrix
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix

for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

#convert into dataframe

genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
} #convert from characters to integers  


# user profile matrix. This can be easily done with the dcast() function in the reshape2 package

binaryratings <- ratings
for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else{
    binaryratings[i,3] <- -1
  }
}

#long format to a wide format using dcast function

binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
binaryratings2 = binaryratings2[,-1]

#remove movieIds col. Rows are movieIds, cols are userIds

#Remove rows that are not rated from movies dataset - dakalti movies :|

movieIds <- length(unique(movies$movieId)) 
print(movieIds)
ratingmovieIds <- length(unique(ratings$movieId)) 
print(ratingmovieIds)
movies2 <- movies[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(movies2) <- NULL

#Remove rows that are not rated from genre_matrix2
genre_matrix3 <- genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(genre_matrix3) <- NULL

#Calculate dot product for User Profiles
result = matrix(0,18,706)
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c]))
  }
}

#Convert to Binary scale
for (i in 1:nrow(result)){
  if (result[i] < 0){
    result[i] <- 0
  }
  else {
    result[i] <- 1
  }
}

#user profile generated from the above

# Jaccard Distance to measure the similarity between user profiles, and the movie genre matrix
print("enter the user to be profiled for")
tofinduser = 2
result2 <- result[tofinduser,] #n th user's profile
sim_mat <- rbind.data.frame(result2, genre_matrix2)
sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)}))
#convert data to type integer

#Calculate Jaccard distance between user profile and all movies
library(vegan)
sim_results <- vegdist(sim_mat, method="jaccard")
sim_results <- as.data.frame(as.matrix(sim_results[1:8552]))

# how to arrive at number --- to find ? !!! @senthiran

rows <- which(sim_results == min(sim_results))

#Recommended movies

movies[rows,2]


############################

# user based collaborative filtering
library(reshape2)

checkvar <- FALSE
useridno <- 1;
towatch1 <- 4;
towatch2 <- 50;
towatch3 <- 100;
if(checkvar == TRUE){
  #assign already existing ratings to the medium rating of 3
  ratings$rating <- ifelse(ratings$userId == useridno, 3, ratings$rating)
  da <- subset(ratings, subset = userId == useridno)
  for(towatch in c(towatch1,towatch2,towatch3)){
  if( towatch %in% da$movieId){
    print("to watch"+towatch)
    ratings$rating <- ifelse(ratings$userId == useridno & ratings$movieId == towatch, 5, ratings$rating)
  }
  else{
    vec <- c(useridno,towatch,5)
    ratings <- rbind(ratings,vec)
  }
  }
}

#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds


#Creation of the Recommender Model

#Method: UBCF
#Similarity Calculation Method: Cosine Similarity
#Nearest Neighbors: 30

library(recommenderlab)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

#Normalize the data
ratingmat_norm <- normalize(ratingmat)


#Create Recommender Model. "UBCF" stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat_norm, method = "UBCF", param=list(method="Cosine",nn=30))

recom <- predict(recommender_model, ratingmat[useridno], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list <- as(recom, "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in c(1:10)){
  recom_result[i] <- movies[as.integer(recom_list[[1]][i]),2]
}

print(recom_result)

#evaluate model
evaluation_scheme <- evaluationScheme(ratingmat, method="cross-validation", k=5, given=3, goodRating=5) #k=5 meaning a 5-fold cross validation. given=3 meaning a Given-3 protocol
evaluation_results <- evaluate(evaluation_scheme, method="UBCF", n=c(1,3,5,10,15,20))
eval_results <- getConfusionMatrix(evaluation_results)[[1]]

print(eval_results)
