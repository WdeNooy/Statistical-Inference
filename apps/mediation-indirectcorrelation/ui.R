library(shiny)

fig.width = 600
fig.height = 300

shinyUI(
  fluidPage(
    fluidRow(column(2, 
                    align = "left",
                    br(),
                    br(),
                    radioButtons(
                      "confradbut",
                      label = "Add a confounder to the regression model",
                      choices = c("None" = "none",
                                  "Age" = "age",
                                  "News site use" = "news",
                                  "Education" = "edu")
                      ,
                      inline = FALSE
                      )
                    ),
              column(10,
                     align = "left",
                     plotOutput("mainplot",
                          width = fig.width,
                          height = fig.height
                          )
                     )
      ) 
  )
)
