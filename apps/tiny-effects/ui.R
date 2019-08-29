library(shiny)
fig.height = 300 
fig.width = 400

shinyUI(
  fluidPage(
    fluidRow(column(4,
                    align = "left",
                    br(),
                    br(),
                    align = "center",
                    sliderInput("savslider",
                                label = "Sample average",
                                min = 2.8,
                                max = 3.3,
                                value = 2.9,
                                step = .01),
                    sliderInput("ssizeslider",
                                label = "Sample size",
                                min = 10,
                                max = 15000,
                                value = 10,
                                step = 10
                    )
             ),
             column(8, 
                   align = "center",
                   plotOutput("mainplot",
                        width = fig.width,
                        height = fig.height
                    )
             
             )
  )

))
