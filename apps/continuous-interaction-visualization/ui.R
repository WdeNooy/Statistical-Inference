library(shiny)
fig.width = 400
fig.height = 300

shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(column(
        width = 3,
        align = "left",
        checkboxGroupInput("checkboxes",
                           label = "Show regression line at following moderator value:",
                           choices = c("Minimum",
                                       "Mean - 2SD",
                                       "Mean - 1SD",
                                       "First tercile",
                                       "Mean",
                                       "Second tercile",
                                       "Mean + 1SD",
                                       "Mean + 2SD",
                                       "Maximum"),
                           selected = "none",
                           inline = TRUE
        )
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

# shinyUI(
#   fluidPage(
#     verticalLayout( 
#       fluidRow(align = "center",
#         plotOutput("mainplot",
#                     height = fig.height,
#                     width = fig.width
#         )
#       ),
#       fluidRow(align = "center",
#                div(
#                    ,
#                    align = "center"
#                )
#       )
#     )
#   )
# )

#minimum, maximum,
# first tercile, second tercile, third tercile, M - 2SD, M - 1SD, M, M + 1SD, M
# + 2SD.