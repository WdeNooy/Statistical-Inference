library(shiny)

fig.width <- 400
fig.height <- 150

shinyUI(fluidPage(verticalLayout(
  fluidRow(
    align = "center",
    
    plotOutput("populatieplot", width = fig.width, height = fig.height),
    
    plotOutput("countplot", width = fig.width, height = fig.height)
    
  ),
  
  fluidRow(
    column(1),
    
    column(
      10,
      align = "center",
      
      actionButton("smallsample", "Take a random sample of 10")
      
    ),
    
    column(1)
  )
  
)))
