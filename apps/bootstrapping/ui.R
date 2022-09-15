library(shiny)

# Changes: User Interface now consists of three columns. This serves two purposes: 
# (1) This creates the space necessary to label the candies
# (2) Ensures that the action button is placed below the corresponding plot (= easier)

shinyUI(
  mainPanel(
  splitLayout(
    # style = "border: 0.5px solid silver;",
    cellWidths = 230,
    cellArgs = list(style = "padding: 1px"),
    align = "center",
      verticalLayout(
        plotOutput("sampleplot"),
        actionButton("firstsampleaction", label = "New Sample"),
        actionButton("resetsampleaction", label = "Reset")
        ),
      verticalLayout(
        plotOutput("bootstrappedplot"),
        actionButton("bootstrapsmallaction", label = "Bootstrap 1 Sample")), 
      verticalLayout(
        plotOutput("sampdistplot"),
        actionButton("bootstraplargeaction", label = "Bootstrap 5,000 Samples"))
  )))
