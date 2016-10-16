library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Movie Recommender SDL Project!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput("useidnoinput",
                   label = h3("Enter User Id"), 
                   value = 1),
      h4("check this box true, if you want to choose 3 movie ids now and get similar recommendations based on that"),
      checkboxInput("checkboxvalue", "Choose 3 movies which i feel like watching now!", FALSE),
      numericInput("tw1",
                   label = h3("Enter Your First Preferred movie id"), 
                   value = 1),
      numericInput("tw2",
                   label = h3("Enter Your Second Preferred movie id"), 
                   value = 50),
      numericInput("tw3",
                   label = h3("Enter Your Third Preferred movie id"), 
                   value = 100)
      
      
      
    
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Recommendations are"),
      verbatimTextOutput("summary")
    )
  )
))
