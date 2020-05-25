library(shiny)
fig.height = 325 
fig.width = 500

shinyUI(
  fluidPage(
    fluidRow(column(3,
                    align = "left",
                    align = "center",
                    sliderInput("savslider",
                                label = "Sample mean difference",
                                min = 0,
                                max = 1.0,
                                value = 0.39,
                                step = .01),
                    sliderInput("ssizeslider",
                                label = "Sample size",
                                min = 10,
                                max = 200,
                                value = 20,
                                step = 10),
                    sliderInput("effectslider",
                                label = "True population mean difference",
                                min = 0,
                                max = 0.5,
                                value = 0.3,
                                step = .01),
                    checkboxInput("showpower", "Show test power")
             ),
             column(9, 
                   align = "center",
                   plotOutput("mainplot",
                        width = fig.width,
                        height = fig.height
                    )
             
             )
  )

))
