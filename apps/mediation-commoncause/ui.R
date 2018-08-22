library(shiny)

fig.width = 400
fig.height = 320

shinyUI(
  fluidPage(
    fluidRow(column(5,
                  align = "center",
                  br(),
                  sliderInput("agepolslider",
                       label = "Correlation Age with Pol. interest",
                       min = -0.9,
                       max = 0.9,
                       step = .05,
                       value = 0),
                  sliderInput("polreadslider",
                              label ="Correlation Pol. interest with Readingtime",
                              min = -0.9,
                              max = 0.9,
                              step = .05,
                              value = 0),
                  sliderInput("agereadslider",
                       label = "Correlation Age with Readingtime",
                       min = -0.9,
                       max = 0.9,
                       step = .05,
                       value = 0)
                  ),
           column(7,
                  align = "center",
                  plotOutput("mainplot",
                          width = fig.width,
                          height = fig.height
                          )
                  )
           )
  )
)
