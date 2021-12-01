library(shiny)
fig.width = 400
fig.height = 400


shinyUI(
  fluidPage(
    fluidRow(column(3,
                    br(), br(),
                    aligh = "left",
                    div(
                      checkboxGroupInput("predcheckbox",
                                  label = "Predictors",
                                  choices = c("Age" = "age",
                                              "Education" = "education",
                                              "Pol. Interest" = "polinterest",
                                              "News site use" = "newssite"),
                                  inline = FALSE
                                  )
                      )
                    ),
             column(9,
                    align = "center",
                    plotOutput("mainplot",
                          width = fig.width,
                          height = fig.height
                          )
                    )
             )
    )
)
