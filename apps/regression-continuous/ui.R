library(shiny)
fig.width = 700
fig.height = 300

shinyUI(
  fluidPage(
    fluidRow(
      align = "center",
      plotOutput("mainplot",
                 height = fig.height,
                 width = fig.width
      )
    ),
    fluidRow(column(4,
                    align = "center",
                    sliderInput("expovalueslider",
                                label = "Adjust Exposure:",
                                min = 0,
                                max = 10,
                                value = 0,
                                step = 1
                    )
    ),
    column(8, 
           align = "center",
           div(strong("Regression equation:")),
           withMathJax(uiOutput("formulaui"))
    )
    )
  )
)
