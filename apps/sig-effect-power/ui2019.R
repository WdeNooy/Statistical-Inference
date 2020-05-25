library(shiny)
fig.height = 300 
fig.width = 400

shinyUI(
  fluidPage(
    fluidRow(column(4,
                    align = "center",
                    br(),
                    br(),
                    sliderInput("savslider",
                                label = "Population average",
                                min = 2.8,
                                max = 3.2,
                                value = 2.9,
                                step = .05),
                    sliderInput("ssizeslider",
                                label = "Sample size",
                                min = 10,
                                max = 100,
                                value = 10,
                                step = 5
                    ),
                    actionButton("sampbutton",
                                 label = "Take 5 new samples")
    ),
    column(8,
           align = "center",
           plotOutput("mainplot",
                      width = fig.width,
                      height = fig.height
           )
    )
    )
  )
)
