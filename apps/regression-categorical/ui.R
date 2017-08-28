library(shiny)

fig.width = 400
fig.height = 350
shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(align = "center",
               plotOutput("scatterplot",
                          brush = "scatterbrush",
                          width = fig.width,
                          height = fig.height
               )
      ),
      fluidRow(align = "center",
               selectInput("selector",
                            "Select reference group",
                           choices = c( "Never smoked", "Stopped smoking","Smoking")
                           
               )
      ),
      fluidRow(align = "center",
               actionButton("samplebutton",
                            "New Plot"
               )
      )
    )
  )
)


