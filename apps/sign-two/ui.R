library(shiny)
fig.height = 260 
fig.width = 400

shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(align = "center",
                plotOutput("mainplot",
                           width = fig.width,
                           height = fig.height
                           )
                ),
      fluidRow(align = "center",
               sliderInput("slider", label = ("Non-rejection region"), min = 1, 
                           max = 10, value = c(3, 8), step = 0.025, width = 390),
                actionButton("resetButton", "New sample")
               )
    )
  )
)
