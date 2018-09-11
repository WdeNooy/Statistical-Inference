library(shiny)

fig.width <- 400
fig.height <- 250

shinyUI(fluidPage(verticalLayout(
  fluidRow(
    column(6,
           align = "center",
           plotOutput("countplot", width = fig.width, height = fig.height)
    ),
    column(6,
           plotOutput("samplingstatisticplot", width = fig.width, height = fig.height)
           )
  ),
  
  fluidRow(
    column(1),
    
    column(
      10,
      align = "center",
      
      actionButton("smallsample", "Take a new random sample"),
      actionButton("largesample", "Take 1000 random samples"),
      actionButton("reset", "Reset")
      
    ),
    
    column(1)
    
  )
  

)))
