library(RColorBrewer)

#General ggplot2 theme
theme_general <- function() {
  theme_classic() +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(hjust = 0.5, size = 13),
      panel.border=element_rect(fill=NA)
    )
}
# Standard colors to use
brewercolors <- brewer.pal( 5, name =  "Spectral")
brewercolors[3] <- "#ffff00"
names(brewercolors) <- c("Red", "Orange", "Yellow", "Green", "Blue")

# Function to print p values.
pprint <- function(pvalue) {
  ifelse(pvalue < .0005,
         "p < .001",
         paste0("p = ", format(round(pvalue,3), nsmall = 3)))
}
# Function to print (numeric) results except p values.
rprint <- function(value) {
  format(round(value,2), nsmall = 2)
}
# Function to print p values.
pprint <- function(value) {
  ifelse(value < 0.001, "p < .001",
    paste0("p = ", format(round(value,3), nsmall = 3))
         )
}