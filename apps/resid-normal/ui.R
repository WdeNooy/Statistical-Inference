library(shiny)

fig.width = 360
fig.height = 220
shinyUI(
  fluidPage(
      fluidRow(column(6,
                      align = "center",
                      plotOutput("scatterplot",
                          brush = "scatterbrush",
                          width = fig.width,
                          height = fig.height
                      )
              ),
              column(6,
                      align = "center",
                      plotOutput("residplot",
                          width = fig.width,
                          height = fig.height
                    )
              )
      ),
      fluidRow(align = "center",
               actionButton("samplebutton",
                           "Draw a new sample"
               )
      )
    )
)

