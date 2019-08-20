library(shiny)

fig.width = 450
fig.height = 320
shinyUI(
  fluidPage(
    fluidRow(column(4,
                    br(),
                    align = "center",
                    br(),
                    selectInput("selector",
                                "Select reference group",
                                choices = c( "Non-smoker", "Former smoker","Smoker")
                                
                    ),
                    actionButton("samplebutton", "New Plot"
                                 )
            ),
             column(8, 
                    align = "center",
                    plotOutput("scatterplot",
                        brush = "scatterbrush",
                        width = fig.width,
                        height = fig.height
                    )
            )
    )
  )
)
