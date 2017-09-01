library(shiny)

fig.width = 400
fig.height = 350
shinyUI(
  fluidPage(
    verticalLayout(
      fluidRow(align = "center",
               plotOutput("scatterplot",
                          width = fig.width,
                          height = fig.height
               )
      ),
      fluidRow(align = "center",
#               div(strong("Equation:")),
               withMathJax(uiOutput("equationui"))
      ),
      fluidRow(align = "center",
               div(strong("Adjust group averages:")),
               sliderInput("nonsmokeraveragesli",
                           label = "Non Smoker",
                           min = -5,
                           max = 5,
                           step = 0.1,
                           value = -0.6)
      ),
      fluidRow(align = "center",
               sliderInput("smokeraveragesli",
                           label = "Smoker",
                           min = -5,
                           max = 5,
                           step = 0.1,
                           value = 1)
      )
    )
  )
)



