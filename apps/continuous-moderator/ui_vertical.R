library(shiny)
fig.width = 400
fig.height = 300

shinyUI(fluidPage(verticalLayout(
  fluidRow(align = "center",
           div(strong(textOutput("headtext"))),
           plotOutput("mainplot",
                      height = fig.height,
                      width = fig.width
                      )
           ),
  
  fluidRow(align = "center",
           div(strong("Equation:")),
           withMathJax(helpText(
             paste("$$\\color{black}{attitude = b_0 + (b_1 + b_3 * }\\color{blue}{",
                   "contact",
                   "}\\color{black}{)*exposure + b_2*}\\color{blue}{",
                   "contact",
                   "}$$"
           )))
           ),
  fluidRow(align = "center",
           withMathJax(uiOutput("formulaui"))
  ),
  fluidRow(align = "center",
           sliderInput("modvalueslider",
                       label = "Adjust the value of Contact (Moderator):",
                       min = 0,
                       max = 10,
                       value = 0,
                       step = .5))
)

  
))
