library(shiny)
fig.width = 500
fig.height = 400

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
               div(style = "display:inline-block",
                   numericInput("clooneynumin",
                                label = "Clooney",
                                value = 6.4,
                                min = 3,
                                max = 8,
                                width = 100
                   )
               ),
               div(style = "display:inline-block",
                   numericInput("jolienumin",
                                label = "Jolie",
                                value = 6.8,
                                min = 3,
                                max = 8,
                                width = 100)
               ),
               div(style = "display:inline-block",
                   numericInput("endorsernumin",
                                label = "No endorser",
                                value = 3.3,
                                min = 3,
                                max = 7,
                                width = 100
                   )
               ),
               div(style = "display:inline-block",
                   actionButton("newsampbut",
                                label = "Update graph"
                   )
               )
      ) 
    )
    
  )
)
