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
                                label = "Adjust the distribution of errors: 0 = homoscedastic",
                                value = .5,
                                min = -1,
                                max = 1,
                                step = .5
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
