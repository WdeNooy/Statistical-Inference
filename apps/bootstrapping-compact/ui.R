library(shiny)
fig.width = 360
fig.height = 140
# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(align = "center",
               plotOutput(outputId = "sampleplot", 
                          height = fig.height, 
                          width = fig.width)
               ),
      fluidRow(align = "center",
               plotOutput(outputId = "bootstrappedplot",
                          height = fig.height, 
                          width = fig.width)
               ),
      fluidRow(align = "center",
               plotOutput(outputId = "sampdistplot",
                          height = fig.height, 
                          width = fig.width)
               ),
      fluidRow(align = "center",
               actionButton("bootstrapsmallaction", 
                            label = "Bootstrap one sample"),
               actionButton("bootstraplargeaction", 
                            label = "Bootstrap 1000 samples"),
               actionButton("firstsampleaction",
                            label = "Draw new initial sample")
               )
    )
  )
)
