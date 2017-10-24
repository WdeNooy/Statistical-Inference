library(shiny)
fig.height = 300 
fig.width = 400
shinyUI(fluidPage(
    fluidRow(column(4,
             sliderInput("savslider",
                         label = "Sample b",
                         min = -0.6,
                         max = 0.6,
                         value = 0.019,
                         step = .001),
             sliderInput("ssizeslider",
                         label = "Sample size",
                         min = 10,
                         max = 1000,
                         value = 250,
                         step = 10
             )
    ),
            column(8, 
           plotOutput("mainplot",
                        width = fig.width,
                        height = fig.height
                        )
             )
  )

))
