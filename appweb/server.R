library(shiny)
library(reshape2)
library(data.table)
library(Matrix)
library(proxy)
library(recommenderlab)
library(reshape2)
movies <- read.csv("~/Works/Rworks/Movielens/Recom/movies.csv", stringsAsFactors=FALSE)
str(movies)
ratings <- read.csv("~/Works/Rworks/Movielens/Recom/ratings.csv", stringsAsFactors=FALSE)
str(ratings)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  checkvar <- input$checkboxvalue;
  useridno <- input$useidnoinput;
  towatch1 <- input$tw1;
  towatch2 <- input$tw2;
  towatch3 <- input$tw3;
  if(checkvar == TRUE){
    #assign already existing ratings to the medium rating of 3
    ratings$rating <- ifelse(ratings$userId == useridno, 3, ratings$rating)
    da <- subset(ratings, subset = userId == useridno)
    for(towatch in c(towatch1,towatch2,towatch3)){
      if( towatch %in% da$movieId){
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
  
  output$summary <- renderPrint(recom_result)
  
  
  })
