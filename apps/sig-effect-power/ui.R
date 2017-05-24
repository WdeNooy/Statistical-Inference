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
                          label = "Population average",
                          min = 2.8,
                          max = 3.2,
                          value = 2.9,
                          step = .1),
              sliderInput("ssizeslider",
                          label = "Sample size",
                          min = 5,
                          max = 50,
                          value = 10,
                          step = 5
                          )
              ),
    fluidRow(align = "center",
             actionButton("sampbutton",
                          label = "Take new sample"))
  )

))
