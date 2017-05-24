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
                  value = 25,
                  min = 5,
                  max = 50,
                  step = 5
                )
               )
    )
  )
)
