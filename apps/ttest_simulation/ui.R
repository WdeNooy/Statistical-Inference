library(shiny)

fig.width <- 400
fig.height <- 250

shinyUI(fluidPage(
  verticalLayout(
    fluidRow(align = "center",
             plotOutput("t.stats",
                        width = fig.width,
                        height = fig.height)
    ),
    fluidRow(align = "center",
             plotOutput("p.values",
                        width = fig.width,
                        height = fig.height)
    ),
    fluidRow(align = "center",
             sliderInput("effect.size", 
                         "True effect size:", 
                         value = 0.2,
                         min = 0, 
                         max = 1,
                         step = 0.1, 
                         ticks=FALSE)
    ),
    fluidRow(align = "center",
             sliderInput("sample.size", 
                         "Number of observations in a sample:", 
                         value = 20,
                         min = 2, 
                         max = 50,
                         step = 1, 
                         ticks=FALSE),
             p("Adapted from ShinyApps-spark")
    )
  )))

# shinyUI(pageWithSidebar(
#   
#   headerPanel("Right-tailed t-test on one sample mean"),
#   
#   sidebarPanel(
#     
#     div(p("Change effect size and number of samples to see their effect on test power.")),
#     
#     div(
#       
#       sliderInput("effect.size", 
#                   strong("Effect size"), 
#                   min=0, max=1, value=0.2, step=.1, ticks=FALSE),
#       sliderInput("sample.size",
#                   strong("Number of observations in a sample"),
#                   min=2, max=50, value=20, step=1, ticks=FALSE)
#                 
#     )
#   ),
# 
#   mainPanel(
#     plotOutput("t.stats", height=fig.height),
#     plotOutput("p.values", height=fig.height)
#   )
#     
# ))