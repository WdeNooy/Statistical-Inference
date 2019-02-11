library(shiny)

fig.width = 400
fig.height = 300
shinyUI(
  fluidPage(
    fluidRow(column(4,
                    align = "center",
                    br(),
                    br(),
                    sliderInput("samplesizeslider",
                                label = "Sample size",
                                min = 5,
                                max = 50,
                                step = 1,
                                value = 25
                    ),
                    align = "center",
                    sliderInput("propslider",
                                label = "Population proportion",
                                min = 0,
                                max = 1,
                                step = 0.05,
                                value = 0.5
                    )
             ),
             column(8, 
                    align = "center",
                    plotOutput("mainplot",
                          width = fig.width,
                          height = fig.height
               )
      )
    )
  )
)
