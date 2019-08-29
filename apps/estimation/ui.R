library(shiny)
fig.width = 400
fig.height = 200
shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(column(width = 4,
                      align = "center",
                      br(),
                      sliderInput("cislider",
                                  label = "Confidence level",
                                  min = 50,
                                  max = 100,
                                  value = 95,
                                  step = 1,
                                  post = "%"
                      ),
                      sliderInput("nslider",
                                  label = "Sample size",
                                  min = 10,
                                  max = 100,
                                  value = 40,
                                  step = 5
                      )
               ),
               column(width = 8,
                      align = "center",
                      plotOutput("mainplot",
                                 width = fig.width,
                                 height = fig.height + 50)
               )
      )
    )
  )
)
