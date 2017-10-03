fig.width = 400
fig.height = 300
shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(align = "center",
               plotOutput("scatterplot",
                          width = fig.width,
                          height = fig.height
               )
     ),
     fluidRow(align = "center",
              actionButton("samplebtn",
                           label = "Take new sample"
              )
    )
    )
  )
)
    
