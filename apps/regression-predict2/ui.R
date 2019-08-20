library(shiny)
fig.width = 450
fig.height = 300

shinyUI(
  fluidPage(
    fluidRow(column(4,
                    align = "left",
                    br(),
                    align = "center",
                    selectInput("smokeselector", 
                                label = "Select smoking status",
                                choices = c("Non-Smoker" = 0,"Smoker" = 1),
                                selected = "Non-Smoker"),
                    sliderInput("contactvalueslider",
                                label = "Adjust the value of contact with smokers:",
                                min = 0,
                                max = 10,
                                value = 6,
                                step = .1
                    ),
                    div(strong("Simplified regression equation:")),
                    withMathJax(uiOutput("formulasimple"))
    ),
    column(8, 
           align = "center",
           div(strong(textOutput("headtext"))),
           plotOutput("mainplot",
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
