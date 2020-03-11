library(shiny)
fig.width = 360
fig.height = 300

shinyUI(fluidPage(verticalLayout(
  fluidRow(column(6,
    fluidRow(align = "center",
           div(strong(textOutput("headtext"))),
           plotOutput("mainplot",
                      height = fig.height,
                      width = fig.width
                      )
           )),
  
    column(6,
    fluidRow(align = "center",
             br(),
             div(strong("Equation:")),
             withMathJax(helpText(
               paste("$$\\small{\\color{black}{attitude = b_0 + (b_1 + b_3 * }\\color{blue}{",
                   "contact",
                   "}\\color{black}{)*exposure + b_2*}\\color{blue}{",
                   "contact",
                   "}}$$"
           )))
           ),
    fluidRow(align = "center",
             withMathJax(uiOutput("formulaui"))
            ),
    fluidRow(align = "center",
             div(strong("Effect of exposure:"))
    ),
    fluidRow(align = "center",
             withMathJax(uiOutput("formulaslope"))
    ),
    fluidRow(align = "center",
           sliderInput("modvalueslider",
                       label = "Adjust the value of Contact (Moderator):",
                       min = 0,
                       max = 10,
                       value = 0,
                       step = .5)))
))

  
))

