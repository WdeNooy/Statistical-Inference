library(shiny)
fig.width = 300
fig.height = 280

shinyUI(fluidPage(verticalLayout(
  fluidRow(column(5,
    fluidRow(align = "center",
           div(strong(textOutput("headtext"))),
           plotOutput("mainplot",
                      height = fig.height,
                      width = fig.width
                      )
           )),
  
    column(7,
    fluidRow(align = "center",
             br(),
             div(strong("Equation:")),
             withMathJax(helpText(
               paste("$$\\small{\\color{black}{attitude = b_0 + b_1*exposure + b_2*}\\color{blue}{",
                   "contact",
                   "}\\color{black}{ + b_3 * }\\color{blue}{",
                   "contact",
                   "}\\color{black}{*exposure}}$$"
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
                       step = .1)))
))

  
))

