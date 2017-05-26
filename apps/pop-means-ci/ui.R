library(shiny)
fig.height = 200 
fig.width = 400

shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(align = "center",
                plotOutput("mainplot",
                           width = fig.width,
                           height = fig.height,
                           click = "plot_click"
                           )
                )
               )
    )
  )
