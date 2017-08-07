library(shiny)
fig.width = 400
fig.height = 300
# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    fluidRow(align = "center",
             plotOutput("mainplot",
                        width = fig.width,
                        height = fig.height)
    ),
    fluidRow(align = "center",
             sliderInput("efsizeslider",
                         label = "Effect size",
                         min = 0.2,
                         max = 0.8,
                         value = 0.5,
                         step = 0.1)
             ),
    fluidRow(align = "center",
             selectInput("onetwoselect",
                         label = "Test type",
                         choices = c("Two-sided", "One-sided")
             )
    ),
    fluidRow(align = "center",
             sliderInput("siglevslider",
                         label = "Sig. level",
                         min = 90,
                         max = 99,
                         value = 95,
                         step = 1,
                         post = "%"
             )
   ),
   fluidRow(align = "center",
            sliderInput("powerslider",
                        label = "Power",
                        min = 50,
                        max = 99,
                        value = 80,
                        step = 1,
                        post = "%"
            )
  ),
  fluidRow(align = "center",
           uiOutput("ssizeuiout")
           )
            
 
  )
)
