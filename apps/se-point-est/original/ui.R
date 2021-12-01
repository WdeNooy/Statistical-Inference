library(shiny)
fig.width = 350
fig.height = 150

shinyUI(
  fluidPage(
    fluidRow(column(6,
                    align = "center",
                    plotOutput("sampleplot",
                            width = fig.width,
                            height = fig.height+100)
                    ),
             column(6,
                    align = "center",
                    plotOutput("sampdistplot",
                            width = fig.width,
                            height = fig.height + 100)
                    )
             ),
    fluidRow(column(4,
                    align = "center",
                    actionButton("smallsamplebutton",
                            label = "Add a single sample")
                    ),
             column(4,
                    align = "center",
                    actionButton("largesamplebutton",
                            label = "Add 100 samples")
                    ),
             column(4,
                    align = "center",
                    actionButton("resetbutton",
                          label = "Reset")
                    )
            )
    )
)
