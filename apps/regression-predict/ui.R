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
           div(strong("Simple regression equation:")),
           withMathJax(helpText(
             paste(
               "$$\\color{darkgrey}{attitude = 1.575 - 0.6 * exposure}$$"
             )
           ))
  ),
  fluidRow(align = "center",
           div(strong("Multiple regression equation:")),
           withMathJax(uiOutput("formulaui"))
  ),
  fluidRow(align = "center",
           selectInput("smokeselector", 
                       label = "Select smoke variable value",
                       choices = c("Non-Smoker" = 0,"Smoker" = 1),
                       selected = "Non-Smoker")
  ),
  
  fluidRow(align = "center",
           sliderInput("contactvalueslider",
                       label = "Adjust the value of Contact:",
                       min = 0,
                       max = 10,
                       value = 3,
                       step = .5
            )
   )

  )
)
)
