library(shiny)
fig.height = 300 
fig.width = 400

shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(align = "center",
                plotOutput("mainplot",
                           width = fig.width,
                           height = fig.height
                           )
                ),
      fluidRow(align = "center",
                sliderInput("sampsizeslider",
                  label = "Change the hypothesized population mean",
                  value = 5.5,
                  min = 1,
                  max = 10,
                  step = 0.05,
                  width = fig.width - 20
                )
               )
    )
  )
)
