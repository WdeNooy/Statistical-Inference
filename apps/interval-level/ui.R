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
                           label = "Confidence level",
                           min = 50,
                           max = 100,
                           value = 95,
                           step = 1,
                           post = "%"
                          )
               )
    )
  )
)
