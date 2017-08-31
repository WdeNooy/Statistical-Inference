library(shiny)
fig.width = 500
fig.height = 400

shinyUI(fluidPage(verticalLayout(
  fluidRow(
    align = "center",
    plotOutput("mainplot",
               width = fig.width,
               height = fig.height)
  ),
  fluidRow(
    align = "center",
    checkboxGroupInput(
      "groupselect",
      label = "Display means for",
      choices = c("Endorser" = 1, "Sex" = 2),
      inline = TRUE
    )
  ),
  fluidRow(align = "center",
           actionButton("newsampbut",
                        label = "New sample"))
)))
