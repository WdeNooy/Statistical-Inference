fig.width = 400
fig.height = 220
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
              plotOutput("histogram",
                         width = fig.width,
                         height = fig.height
              )
     ),
     fluidRow(align = "center",
              div(
                selectInput("selectinput",
                             label = "Choose group",
                             choices = c("All", "Smoker", "Former smoker", "Non-smoker"),
                             selected = "All"
                
                             ), 
                align = "center"
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
    
