library(shiny)

fig.width <- 400
fig.height <- 250

shinyUI(pageWithSidebar(
  
  headerPanel("Right-tailed t-test on one sample mean"),
  
  sidebarPanel(
    
    div(p("Change effect size and number of samples to see their effect on test power.")),
    
    div(
      
      sliderInput("effect.size", 
                  strong("Effect size"), 
                  min=0, max=1, value=0.2, step=.1, ticks=FALSE),
      sliderInput("sample.size",
                  strong("Number of observations in a sample"),
                  min=2, max=50, value=20, step=1, ticks=FALSE)
                
    )
  ),

  mainPanel(
    plotOutput("t.stats", height=fig.height),
    plotOutput("p.values", height=fig.height)
  )
    
))