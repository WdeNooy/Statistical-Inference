library(shiny)

fig.width = 400
fig.height = 350
shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(align = "center",
               plotOutput("mainplot",
                          width = fig.width,
                          height = fig.height
               )
      ),
      fluidRow(align = "center",
               sliderInput("samplesizeslider",
                           label = "Sample size",
                           min = 5,
                           max = 50,
                           step = 1,
                           value = 25
                           )
              
      ),
      fluidRow(align = "center",
               sliderInput("propslider",
                           label = "Sample proportion",
                           min = 0,
                           max = 1,
                           step = 0.1,
                           value = 0.5
               )
     )
    )
  )
)
