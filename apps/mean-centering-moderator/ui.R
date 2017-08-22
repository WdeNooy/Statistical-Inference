library(shiny)
fig.width = 400
fig.height = 300

shinyUI(fluidPage(
  tags$head(tags$script(src="sliderInit.js")),
    verticalLayout(
  fluidRow(align = "center",
           div(strong(textOutput("headtext"))),
           plotOutput("mainplot",
                      height = fig.height,
                      width = fig.width
           )
  ),
 fluidRow(align = "center",
          sliderInput("modcenterslider",
                      label = "Center Contact. Subtract from Contract:",
                      min = 0,
                      max = 10,
                      value = 0,
                      step = .5)),
 fluidRow(align = "center",
          sliderInput("modvalueslider",
                      label = "Adjust the value of Contact (Moderator):",
                      min = 0,
                      max = 10,
                      value = 0,
                      step = .5)
 )
)))
