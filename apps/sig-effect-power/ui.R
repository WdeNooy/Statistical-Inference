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
                          step = .05),
              sliderInput("ssizeslider",
                          label = "Sample size",
                          min = 10,
                          max = 100,
                          value = 10,
                          step = 5
                          )
              ),
    fluidRow(align = "center",
             actionButton("sampbutton",
                          label = "Take new sample"))
  )

))
