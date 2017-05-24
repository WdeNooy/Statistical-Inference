library(shiny)
fig.width = 400
fig.height = 150

shinyUI(fluidPage(
  verticalLayout(
    fluidRow(align = "center",
             plotOutput("sampleplot",
                        width = fig.width,
                        height = fig.height+50)
             ),
    fluidRow(align = "center",
             plotOutput("sampdistplot",
                        width = fig.width,
                        height = fig.height + 100)
             ),
    fluidRow(align = "center",
             actionButton("smallsamplebutton",
                          label = "Take single sample"),
             actionButton("largesamplebutton",
                          label = "Take 100 samples"),
             actionButton("resetbutton",
                          label = "Reset"))
  )
  

))
