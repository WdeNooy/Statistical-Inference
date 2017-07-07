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
                actionButton("resetButton", "New sample")
               )
    )
  )
)
