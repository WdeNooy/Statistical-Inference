library(shiny)
fig.width = 400
fig.height = 300

shinyUI(fluidPage(
  tags$head(tags$script(src="sliderInit.js")),
    verticalLayout(
      fluidRow(column(4,
                      align = "center",
                      br(),
                      sliderInput("modcenterslider",
                        label = "Center Contact. Subtract from Contact:",
                        min = 0,
                        max = 10,
                        value = 0,
                        step = .5),
                      sliderInput("modvalueslider",
                        label = "Adjust the value of Contact (Moderator):",
                        min = 0,
                        max = 10,
                        value = 0,
                        step = .5)
                      ),
               column(8,
                      align = "center",
                      #div(strong(textOutput("headtext"))),
                      plotOutput("mainplot",
                              height = fig.height,
                              width = fig.width
                              )
                      )
               )
      )
  )
)
