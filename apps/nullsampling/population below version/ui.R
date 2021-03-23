library(shiny)
fig.height = 260 
fig.width = 400

shinyUI(
  fluidPage(
    sidebarPanel(
      fluidRow(radioButtons("steps", "Hypothesis testing",
                            c("Step 1: Specify the null hypothesis" = "step1",
                              "Step 2: Decision rule" = "step2",
                              "Step 3: Draw sample" = "step3",
                              "Step 4: Reject the null hypothesis?" = "step4"))
               ),
      fluidRow(align = "left",
               sliderInput("sampsizeslider",
                           label = "Change Hypothesized Population Mean",
                           value = 5.5, min = 1, max = 10, step = 0.05, width = fig.width - 20)
               ),
      
      fluidRow(align = "center",
               actionButton(
                 "resetButton", "Draw A New Sample",
                 icon("dice"), 
                 style="color: #fff; background-color: #2B83BA; border-color: #2B83BA")
               )
    ),
    mainPanel(plotOutput("mainplot"))
  )
)
