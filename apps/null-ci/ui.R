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
                    label = "Change the hypothesized population mean",
                    value = 5.5,
                    min = 1,
                    max = 10,
                    step = 0.05,
                    width = fig.width - 20)
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
