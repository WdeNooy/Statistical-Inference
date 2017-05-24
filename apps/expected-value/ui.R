library(shiny)

fig.width <- 400
fig.height <- 150

shinyUI(fluidPage(verticalLayout(
  fluidRow(
    align = "center",
    
    sliderInput(
      "probslider",
      "Proportion of yellow candies in population",
      value = 0.2,
      min = 0,
      max = 1,
      step = 0.1
    )
  ),
  fluidRow(
    align = "center",
    p(strong("Sampling distribution")),
    plotOutput("expectedvalueplot", width = fig.width, height = fig.height)
  ),
  fluidRow(
    align = "center",
    
    numericInput(
      "answer",
      "How many yellow candies do you expect?",
      value = 0,
      min = 0,
      max = 10,
      step = 1
    ),
    actionButton("submit", "Check answer"),
    textOutput("answertext")
  )
)))
