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
                           label = "Average candy weight: Upper limit",
                           min = 2.8,
                           max = 6,
                           value = 3.4,
                           step = 0.01,
                           ticks = FALSE
                          )
               )
    )
  )
)
