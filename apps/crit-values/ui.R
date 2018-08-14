library(shiny)
fig.width = 420
fig.height = 200
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
                           label = "Standard error",
                           min = .4,
                           max = 1.0,
                           value = .8,
                           step = .1
                          )
               )
    )
  )
)
