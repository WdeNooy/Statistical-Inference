library(shiny)
library(DT)

shinyUI(fluidPage(verticalLayout(
                  
  fluidRow(
    align = "center",
    
    dataTableOutput("probtable")
  
  ),
  fluidRow(
    align = "center",
    
    sliderInput("probslider",
                "Proportion of yellow candies in population",
                value = 0.2,
                min = 0,
                max = 1,
                step = 0.1)
  )
)))
