library(shiny)
#standard figure width and height
fig.width = 400
fig.height = 150

shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(
        align = "center",
        plotOutput(
          "populationpropplot",
          width = fig.width,
          height = fig.height
          )
        ),
      fluidRow(
        align = "center",
        plotOutput(
          "sampleplot",
          width = fig.width,
          height = fig.height
        )
      ),
      fluidRow(
        align = "center",
        plotOutput(
          "samplingdistplot",
          width = fig.width,
          height = fig.height
        )
      ),
      fluidRow(
        align = "center",
        sliderInput(
          "samplesizeslider",
          label = "Sample size of initial sample",
          min = 5,
          max = 150,
          value = 25,
          step = 5
        )
      )
    )
  )
)

