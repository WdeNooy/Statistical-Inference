library(shiny)

fig.width <- 400
fig.height <- 150

shinyUI(fluidPage(verticalLayout(
  fluidRow(
            align = "center",
            plotOutput("pvalueplot", width = fig.width, height = fig.height)
          ),
  fluidRow(
            align = "center",
            sliderInput(
                        inputId = "rangeslider",
                        label = "Average sample candy weight",
                        min = 0,
                        max = 6,
                        value = c(2,2.8),
                        step = 0.01,
                        width = 330
                        )
           )
)))
