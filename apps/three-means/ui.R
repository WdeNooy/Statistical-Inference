library(shiny)

fig.width = 400
fig.height = 150

shinyUI(fluidPage(
  verticalLayout(
    fluidRow(
      align = "center",
      plotOutput("populationplot", width = fig.width, height = fig.height)
    ),
    
    fluidRow(
      align = "center",
      plotOutput("samplingdistplot", width = fig.width, height = fig.height)
    ),
    
    fluidRow(
      align = "center",
      plotOutput("sampleplot", width = fig.width, height = fig.height)
    ),
    fluidRow(
      align = "center",
      sliderInput(
        "populationslider",
        label = "Population Mean",
        min = 0,
        max = 6,
        step = 0.1,
        value = 2
      )
    )
  )
))
