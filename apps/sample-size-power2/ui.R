library(shiny)
fig.width = 400
fig.height = 300

shinyUI(
  fluidPage(
    fluidRow(column(4,
                    align = "center",
                    sliderInput("efsizeslider",
                                label = "Standardized effect size",
                                min = 0.2,
                                max = 0.8,
                                value = 0.5,
                                step = 0.1),
                     selectInput("onetwoselect",
                                 label = "Test type",
                                 choices = c("Two-sided", "One-sided")
                                ),
                    align = "center",
                    sliderInput("siglevslider",
                                label = "Significance level",
                                min = 1,
                                max = 10,
                                value = 5,
                                step = 1,
                                post = "%"
                               ),
                    sliderInput("samsizeslider",
                                label = "Sample size",
                                min = 10,
                                max = 400,
                                value = 33, #initial power = 80%
                                step = 1
                               )
                    ),
             column(8, 
                    align = "center",
                    br(),
                    plotOutput("mainplot",
                               width = fig.width,
                               height = fig.height),
                    textOutput("ssizeuiout",container = h4)
                   )
          )
        )
)
