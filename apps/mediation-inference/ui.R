library(shiny)

fig.width = 460
fig.height = 320

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
               plotOutput("histplot",
                          width = fig.width,
                          height = fig.height/2
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

