library(shiny)
fig.width = 480
fig.height = 300

shinyUI(
  fluidPage(
    sidebarPanel(
        br(),
        br(),
        radioButtons("steps", "Test Power",
                                  c("Step 1: Significance test" = "step1",
                                    "Step 2: A practically relevant effect size" = "step2",
                                    "Step 3: Type II error" = "step3",
                                    "Step 4: Power" = "step4")
                     ),
        br()
        ),
    mainPanel(
        align = "center",
        plotOutput("mainplot",
                    width = fig.width,
                    height = fig.height)
        )
    )
)
