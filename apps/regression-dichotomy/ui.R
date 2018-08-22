library(shiny)

fig.width = 350
fig.height = 320
shinyUI(
  fluidPage(
    fluidRow(column(6,
                    align = "center",
                    br(),
#                    div(strong("Equation:")),
                    withMathJax(uiOutput("equationui")),
                    div(strong("Adjust average attitude towards smoking:")),
                    align = "center",
                    sliderInput("nonsmokeraveragesli",
                           label = "Non Smokers",
                           min = -5,
                           max = 5,
                           step = 0.1,
                           value = -0.6),
                     sliderInput("smokeraveragesli",
                           label = "Smokers",
                           min = -5,
                           max = 5,
                           step = 0.1,
                           value = 1)
                   ),
            column(5, 
                   align = "center",
                   plotOutput("scatterplot",
                              width = fig.width,
                              height = fig.height
                   )
      )
    )
  )
)



