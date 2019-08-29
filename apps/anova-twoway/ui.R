library(shiny)
fig.width = 500
fig.height = 400

shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(column(
        width = 3,
        align = "center",
        br(),
        br(),
        checkboxGroupInput(
          "groupselect",
          label = "Display means for",
          choices = c("Endorser" = 1, "Sex" = 2),
          inline = TRUE
        ),
        actionButton("newsampbut",
                     label = "New sample")
        ),
      column(
        width = 9,
        align = "center",
        plotOutput("mainplot",
                   width = fig.width,
                   height = fig.height
        )
      )
      )
    )
  )
)
