library(shiny)
fig.width = 450
fig.height = 300

shinyUI(
  fluidPage(
    fluidRow(column(4,
                    align = "left",
                    br(),
                    align = "center",
                    sliderInput("coefficientslider",
                                label = "Adjust the regression coefficient of the interaction variable:",
                                min = -0.8,
                                max = 0.8,
                                value = 0,
                                step = .01,
                                round = FALSE
                    ),
                    fluidRow(align = "center",
                             actionButton("samplebtn",
                                          label = "Take new sample"
                             )
                    )
    ),
    column(8, 
           align = "center",
           div(strong(textOutput("headtext"))),
           plotOutput("scatterplot",
                      height = fig.height,
                      width = fig.width
           )
    )
    ),
    fluidRow(align = "center",
             div(strong("Multiple regression equation:")),
             withMathJax(uiOutput("formulamult"))
    )    
  )
)
