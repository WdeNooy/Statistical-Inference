library(shiny)
fig.width = 250
fig.height = 150
shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(align = "center",
               splitLayout(cellWidths = c("50%", "50%"),
                           plotOutput("beforepopplot",
                                      width = fig.width,
                                      height = fig.height + 50),
                           plotOutput("afterpopplot",
                                      width = fig.width,
                                      height = fig.height + 50)
               )
      ),
      fluidRow(align = "center",
               strong(htmlOutput("calculationtext"))),
      
      fluidRow(align = "center",
                          plotOutput("lastsampleplot",
                                      width = fig.width + 50,
                                      height = fig.height)
      ),
      
      fluidRow(align = "center",
               plotOutput("sampdistplot",
                          height = fig.height,
                          width = 2 * fig.width)
      ),
      fluidRow(align = "center",
               actionButton("smallsamplebutton",
                            label = "Sample 1 case"
               ),
               actionButton("largesamplebutton",
                            label = "Draw 1000 samples of size 5"
               ),
               actionButton("resetbutton",
                            label = "Reset"
               )
      )
    )
  )
)
