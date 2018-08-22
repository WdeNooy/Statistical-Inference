library(shiny)

fig.width = 450
fig.height = 220
shinyUI(
  fluidPage(
    fluidRow(column(3,
                  align = "center",
                  br(),
                  br(),
                  selectInput("typeselector",
                       "Select the association type",
                       choices = c("Linear",
                                   "U shaped",
                                   "Curved"),
                       selected = "Linear",
                       multiple = FALSE
                       )
                  ),
            column(9,
                   align = "left",
                   plotOutput("scatterplot",
                              brush = "scatterbrush",
                              width = fig.width,
                              height = fig.height
                              ),
                    plotOutput("residplot",
                                width = fig.width,
                                height = fig.height
                              )
                    )
            )
        )
)
