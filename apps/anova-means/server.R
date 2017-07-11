library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  source("../plottheme/styling.R", local = TRUE)
  
  n <- 4 #sample size per category
  start = 0.5 #minimum value of x
  end = 4 #maximum value of x

  #Function to space dots in plot
  spacemaker <-  function(categories, samples, spacing) {
    x <- numeric()
    for (i in 1:categories) {
      temp <-
        seq(i - categories / 2 * spacing, i + categories / 2 * spacing, by = spacing)
      x <- c(x, temp)
    }
    return(x)
  }
  output$mainplot <- renderPlot({
    
    #Dependance on new sample button
    input$newsampbut
    
    #Data frame containing samples of observations
    df <- isolate(
            data.frame(
              y = c(
                    input$clooneynumin - 0.7, input$clooneynumin + 1.3, 
                    input$clooneynumin - 1.1, input$clooneynumin + 0.5,
                    input$jolienumin + 0.8, input$jolienumin + 0.6, 
                    input$jolienumin - 1.2, input$jolienumin - 0.2,
                    input$endorsernumin - 0.9, input$endorsernumin + 2.1, 
                    input$endorsernumin - 0.5, input$endorsernumin - 0.7
                    ),
                    x = spacemaker(3,n,.15),
                    cat = rep(
                            c(
                              "Clooney",
                              "Jolie",
                              "NoEnd"
                              ),
                            each = n
                            )
            )
          )

    etasqrd <- summary.lm(aov(data = df, y  ~ cat))$r.squared
    ggplot(df,aes(x = x, y = y)) +
      #Mean of group Clooney
      geom_segment(x = start + .09,
                   xend = end - .09,
                   y = mean(subset(df, cat == "Clooney")$y),
                   yend = mean(subset(df, cat == "Clooney")$y),
                   size = 1.2,
                   aes(colour = "Clooney")) + 
      #Mean of Group Jolie
      geom_segment(x = start + .09,
                   xend = end - .09,
                   y = mean(subset(df, cat == "Jolie")$y),
                   yend = mean(subset(df, cat == "Jolie")$y),
                   size = 1.2,
                   aes(colour = "Jolie")) + 
      #Mean of Group No Endorser
      geom_segment(x = start + .09,
                   xend = end - .09,
                   y = mean(subset(df, cat == "NoEnd")$y),
                   yend = mean(subset(df, cat == "NoEnd")$y),
                   size = 1.2,
                   aes(colour = "No Endorser")) +  
      #Arrows between group means
      geom_segment(x = end - 0.6,
                   xend = end - 0.6,
                   y = mean(subset(df, cat == "Clooney")$y),
                   yend = mean(subset(df, cat == "Jolie")$y),
                   linetype = "solid",
                   color = "red",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed")) + 
      geom_segment(x = end - 0.4,
                   xend = end - 0.4,
                   y = mean(subset(df, cat == "NoEnd")$y),
                   yend = mean(subset(df, cat == "Jolie")$y),
                   linetype = "solid",
                   color = "red",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed")) + 
      geom_segment(x = end - 0.2,
                   xend = end - 0.2,
                   y = mean(subset(df, cat == "Clooney")$y),
                   yend = mean(subset(df, cat == "NoEnd")$y),
                   linetype = "solid",
                   color = "red",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed")) + 
      #Points representing observations
      geom_point(shape = 21,
                 size = 5,
                 aes(fill = cat)) +
      #Manual color definitions
      scale_colour_manual(values = c(
                                     "Clooney" = unname(brewercolors["Orange"]),
                                     "Jolie" = unname(brewercolors["Blue"]),
                                     "No Endorser" = unname(brewercolors["Green"])
                                      )
                          ) + 
      #Manual fill definitions
      scale_fill_manual(values = c(
                                  "Clooney" = unname(brewercolors["Orange"]),
                                  "Jolie" = unname(brewercolors["Blue"]),
                                  "NoEnd" = unname(brewercolors["Green"])
                                  )
                         ) + 
      #Eta squared text label
      geom_text(label = paste0("'eta'^2 == ", format(round(etasqrd,2), nsmall = 2)),
                x = end - 0.4,
                y = 9.5,
                hjust = 0.5,
                parse = TRUE) +
      #Legend definitions
      guides(colour = guide_legend(title = "Group Means:"),
             linetype = guide_legend(title = "Grand Mean:",label = FALSE),
             fill = FALSE) + 
      #X label definitions
      scale_x_continuous(breaks = 1:4,labels = c("Clooney", "Jolie", "No Endorser", ""), limits = c(0.5,4)) +
      scale_y_continuous(breaks = 1:10, limits = c(1,10)) +
      #coord_cartesian(ylim = c(min(df$y) - 1,max(df$y)+ 1)) +
      xlab("") + 
      ylab("Willingness to donate") + 
      theme_general() + 
      theme(legend.position = "top") 
      
  })

  
})
