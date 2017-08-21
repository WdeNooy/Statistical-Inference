library(shiny)

fig.width = 400
fig.height = 300

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
               
               radioButtons(
                 "confradbut",
                 label = "Add confounder to the model",
                 choices = c("None" = "none",
                             "Age" = "age",
                             "News site use" = "news",
                             "Education" = "edu",
                             "Political cynicysm" = "polcyn")
                 ,
                 inline = TRUE
               )
      ) 
    )  
  )
)
