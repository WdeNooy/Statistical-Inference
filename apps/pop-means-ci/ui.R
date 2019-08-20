library(shiny)
fig.height = 300 
fig.width = 600

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
