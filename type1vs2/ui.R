library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Type I vs. Type II error"),
  
  sidebarPanel(
    p("Change the significance level of the test and see what happens to the probabilities of Type I and Type II errors as well as test power."),
    sliderInput("alpha", 
                "Significance level:", 
                value = 0.05,
                min = 0.005, 
                max = 0.5,
                step = 0.001),
    p("Adapted from Tarik Gouhier, https://github.com/tgouhier/type1vs2")
    ),
  
  mainPanel(
    plotOutput("plot", height="600px")
  )
))
