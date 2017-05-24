library(shiny)

fig.width <- 400
fig.height <- 150

shinyUI(fluidPage(verticalLayout(
  fluidRow(
    align = "center",
    
    plotOutput("populationproportions", width = fig.width, height = fig.height),
    
    plotOutput("countplot", width = fig.width, height = fig.height + 70),
    
    "Yellow candies in last sample:", strong(textOutput("lastsampletext", inline = TRUE)),
    
    plotOutput("samplingstatisticplot", width = fig.width, height = fig.height)
    
  ),
  
  fluidRow(
    column(1),
    
    column(
      10,
      align = "center",
      
      actionButton("smallsample", "Take a random sample of 10"),
      actionButton("largesample", "Take 1000 random samples of 10"),
      actionButton("reset", "Reset")
      
    ),
    
    column(1)
    
  )
  

)))
