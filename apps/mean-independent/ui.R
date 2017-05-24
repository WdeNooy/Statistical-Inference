library(shiny)
fig.width = 250
fig.height = 150
shinyUI(
  fluidPage(
    verticalLayout(
     fluidRow(align = "center",
              splitLayout(cellWidths = c("50%", "50%"),
                     plotOutput("redpopplot",
                                width = fig.width,
                                height = fig.height + 50),
                     plotOutput("yellowpopplot",
                                width = fig.width,
                                height = fig.height + 50)
                     )
               ),
     fluidRow(align = "center",
              splitLayout(cellWidths = c("50%", "50%"),
                     plotOutput("redsampplot",
                                width = fig.width,
                                height = fig.height),
                     plotOutput("yellowsampplot",
                                width = fig.width,
                                height = fig.height)
              )
     ),
     fluidRow(align = "center",
              strong(htmlOutput("calculationtext"))),
     
     fluidRow(align = "center",
              plotOutput("sampdistplot",
                         height = fig.height,
                         width = 2 * fig.width)
              ),
     fluidRow(align = "center",
              actionButton("smallsamplebutton",
                            label = "Draw single sample"
                           ),
              actionButton("largesamplebutton",
                           label = "Draw 1000 samples"
                           ),
              actionButton("resetbutton",
                           label = "Reset"
                          )
              )
    )
  )
)
