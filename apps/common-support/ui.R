fig.width = 350
fig.height = 220
shinyUI(
  fluidPage(
    fluidRow(column(6,
                    align = "center",
                    div(
                      selectInput("selectinput",
                                  label = "Choose group",
                                  choices = c("All", "Smoker", "Former smoker", "Non-smoker"),
                                  selected = "All"            
                      ), 
                      align = "center"
                    ),
                    align = "center",
                    plotOutput("scatterplot",
                          width = fig.width,
                          height = fig.height
                    )
              ),
              column(6, 
                     br(),
                     actionButton("samplebtn",
                                  label = "Take new sample"
                     ),
                     br(), br(),
                     align = "center",
                     plotOutput("histogram",
                         width = fig.width,
                         height = fig.height
                    )
              )
    )
  )
)
    
