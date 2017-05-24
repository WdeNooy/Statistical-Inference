library(shiny)

fig.width = 400
fig.height = 220
shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(align = "center",
               plotOutput("scatterplot",
                          brush = "scatterbrush",
                          width = fig.width,
                          height = fig.height
               )
      ),
      fluidRow(align = "center",
               plotOutput("residplot",
                          width = fig.width,
                          height = fig.height
               )
      ),
      fluidRow(align = "center",
               selectInput("typeselector",
                           "Select the association type",
                           choices = c("Linear",
                                       "U shaped",
                                       "Curved"),
                           selected = "Linear",
                           multiple = FALSE
                )
      )
   )
  )
)
