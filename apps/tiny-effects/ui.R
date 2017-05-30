library(shiny)
fig.height = 300 
fig.width = 400
shinyUI(fluidPage(
  verticalLayout(
    fluidRow(align = "center",
             plotOutput("mainplot",
                        width = fig.width,
                        height = fig.height
                        )
             ),
    fluidRow(align = "center",
              sliderInput("savslider",
                          label = "Sample average",
                          min = 2.8,
                          max = 3.0,
                          value = 2.9,
                          step = .01),
              sliderInput("ssizeslider",
                          label = "Sample size",
                          min = 10,
                          max = 15000,
                          value = 10,
                          step = 10
                          )
              )
  )

))
