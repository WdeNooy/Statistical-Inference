library(shiny)

fig.width = 400
fig.height = 220

shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(align = "center",
               plotOutput("scatterplot",
                          brush = "scatterbrush",
                          width = fig.width,
                          height = fig.height
               )
      ),
      fluidRow(align = "center",
               plotOutput("residplot",
                          width = fig.width,
                          height = fig.height
               )
      ),
      fluidRow(align = "center",
               sliderInput("residslider",
                           "Adjust residuals",
                            value = .5,
                            min = -1,
                            max = 1,
                            step = .1
               )
      )
    )
  )
)
