library(shiny)

fig.width <- 360
fig.height <- 180

shinyUI(fluidPage(verticalLayout(
  fluidRow(
    column(6,
           align = "center",
           plotOutput("populationproportions", width = fig.width, height = fig.height)
          ),
    column(6,
           align = "center",
           br(),
           actionButton("smallsample", "Take a random sample of 10 candies"),
           br(),
           br(),
           actionButton("largesample", "Take 1000 random samples of 10 candies"),
           br(),
           br(),
           actionButton("reset", "Reset")
          )
    ),
  fluidRow(
    column(6,
           align = "center",
           plotOutput("countplot", width = fig.width, height = fig.height + 40),
           "Yellow candies in last sample:", strong(textOutput("lastsampletext", inline = TRUE))
    ),
    column(6,
           align = "center",
           plotOutput("samplingstatisticplot", width = fig.width, height = fig.height + 40)
    )
  )
)))
