library(shiny)
fig.height = 260 
fig.width = 400

shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(column(
        width = 4,
        align = "center",
        br(),
        br(),
        sliderInput("sampsizeslider",
                    label = "Sample size",
                    value = 20,
                    min = 5,
                    max = 150,
                    step = 1
        )
      ),
      column(
        width = 8,
        align = "center",
        plotOutput("mainplot",
                   width = fig.width,
                   height = fig.height
        )
      )
      )
    )
  )
)

