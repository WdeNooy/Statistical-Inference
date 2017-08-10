library(shiny)
fig.width = 400
fig.height = 400

shinyUI(
  fluidPage(
    fluidRow(
      align = "center",
      plotOutput("mainplot",
                  width = fig.width,
                  height = fig.height
      )
    ),
    fluidRow(align = "center",
             actionButton("onesamplebtn",
                          "Take another sample"
             )
    )
  

  )
)
