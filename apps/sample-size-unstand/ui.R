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
                  label = "Sample size",
                  value = 15,
                  min = 5,
                  max = 150,
                  step = 1
                )
               )
    )
  )
)
