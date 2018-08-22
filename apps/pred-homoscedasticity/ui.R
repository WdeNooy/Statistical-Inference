library(shiny)

fig.width = 400
fig.height = 220

shinyUI(
  fluidPage(
    fluidRow(column(4,
                    align = "center",
                    br(),
                    br(),
                    sliderInput("residslider",
                                "Adjust homoscedasticity",
                                value = .5,
                                min = -1,
                                max = 1,
                                step = .1
                    )
    ),
            column(8,
                 align = "left",
                 plotOutput("scatterplot",
                      brush = "scatterbrush",
                      width = fig.width,
                      height = fig.height
                            ),
                 plotOutput("residplot",
                       width = fig.width,
                       height = fig.height
                            )
    )
    )
  )
)
