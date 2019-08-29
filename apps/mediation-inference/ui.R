library(shiny)

fig.width = 460
fig.height = 320

shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(column(
        width = 7,
        align = "center",
               plotOutput("mainplot",
                          width = fig.width,
                          height = fig.height
               )
        ),
        column(
          width = 5,
          align = "center",
          plotOutput("histplot",
                     width = fig.width/2,
                     height = fig.height
               )
          )
        ),
      fluidRow(align = "center",
               actionButton("smallsamplebtn",
                            label  = "Draw a single sample"),
               actionButton("largesamplebtn",
                            label = "Draw 1000 samples"),
               actionButton("resetbtn",
                            label = "Reset")
      )
    )
  )
)

