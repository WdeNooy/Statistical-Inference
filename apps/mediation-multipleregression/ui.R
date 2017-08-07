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
      fluidRow(aligh = "center",
               div(
                 checkboxGroupInput("predcheckbox",
                                  label = "Predictors",
                                  choices = c("Age" = "age",
                                              "Education" = "education",
                                              "Pol. Interest" = "polinterest",
                                              "News site use" = "newssite"),
                                  inline = TRUE
                                  )
                 ,align = "center"
               )
      )
    )
  )
)
