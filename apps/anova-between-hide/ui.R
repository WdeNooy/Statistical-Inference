library(shiny)
fig.width = 500
fig.height = 400

shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(br(), br()),
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
                                value = 5.5,
                                min = 3,
                                max = 8,
                                width = 100
                   )
               ),
               div(style = "display:inline-block",
                   numericInput("jolienumin",
                                label = "Jolie",
                                value = 8.0,
                                min = 3,
                                max = 9,
                                width = 100)
               ),
               div(style = "display:inline-block",
                   numericInput("endorsernumin",
                                label = "No endorser",
                                value = 5.0,
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
      ),
      fluidRow(align = "center",
               checkboxGroupInput("selectGroup", label = "Show/hide elements in the graph", 
                                  choices = list("Group means" = 1, 
                                                 "Overall mean" = 2, 
                                                 "Total variance" = 3, 
                                                 "Between groups variance" = 4,
                                                 "Within groups variance" = 5,
                                                 "Eta-squared" = 6),
                                  selected = NULL, inline = TRUE)
        
      )
    )
    
  )
)
