library(shiny)
fig.width = 400
fig.height = 150
shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(align = "center",
               plotOutput("mainplot",
                          width = fig.width,
                          height = fig.height + 50)
               ),
      fluidRow(align = "center",
               sliderInput("mainslider",
                           label = "Sample size",
                           min = 10,
                           max = 100,
                           value = 40,
                           step = 5
                          )
               )
    )
  )
)
