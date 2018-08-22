library(shiny)
fig.width = 350
fig.height = 300

shinyUI(
  fluidPage(
    fluidRow(column(6,
                    align = "left",
                    br(),
                    div(strong("Simple regression equation:")),
                    withMathJax(helpText(
                      paste(
                        "$$\\color{blue}{attitude = 1.575 - 0.6 * exposure}$$"
                      )
                    )),
                    align = "center",
                    selectInput("smokeselector", 
                                label = "Select smoke variable value",
                                choices = c("Non-Smoker" = 0,"Smoker" = 1),
                                selected = "Non-Smoker"),
                    sliderInput("contactvalueslider",
                                label = "Adjust the value of Contact:",
                                min = 0,
                                max = 10,
                                value = 0,
                                step = .1
                    )
    ),
    column(5, 
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
             withMathJax(uiOutput("formulaui"))
    )    
  )
)
