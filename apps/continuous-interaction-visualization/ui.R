library(shiny)
fig.width = 400
fig.height = 300

shinyUI(
  fluidPage(
    verticalLayout( 
      fluidRow(align = "center",
        plotOutput("mainplot",
                    height = fig.height,
                    width = fig.width
        )
      ),
      fluidRow(align = "center",
               div(
                   checkboxGroupInput("checkboxes",
                                      label = "Show regression line at following value of moderator:",
                                      choices = c("Minimum",
                                                  "M - 2SD",
                                                  "M - 1SD",
                                                  "First tercile",
                                                  "M",
                                                  "Second tercile",
                                                  "M + 1SD",
                                                  "M + 2SD",
                                                  "Maximum"),
                                      selected = "none",
                                      inline = TRUE
                   ),
                   align = "center"
               )
      )
    )
  )
)

#minimum, maximum,
# first tercile, second tercile, third tercile, M - 2SD, M - 1SD, M, M + 1SD, M
# + 2SD.