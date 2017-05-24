fig.width = 400
fig.height = 250

library(shiny)

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
              actionButton("sample",
                           label = "Generate sampling distribution"))
    )
  )
)

