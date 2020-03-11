library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  source("../plottheme/styling.R", local = TRUE)
  
  n <- 4 #sample size per category
  categories <- 3 #categories
  
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
    #Dataframe containing arrow positions 
    #tot = arrows between observation + grand mean
    #grpgr = grand mean to group mean
    #grpob = group mean to observation
    dfarrows <- data.frame(x.tot = df$x,
                           xend.tot = df$x,
                           y.tot = rep(mean(df$y),length(df$y)),
                           yend.tot = df$y,
                           x.grpgr = df$x + 0.05,
                           xend.grpgr = df$x + 0.05,
                           y.grpgr = rep(mean(df$y), length(df$y)),
                           yend.grpgr = rep(as.vector(by(df$y, df$cat, mean, simplify = TRUE)),
                                            each = 4),
                           
                           x.grpob = df$x - 0.05,
                           xend.grpob = df$x - 0.05,
                           y.grpob = rep(as.vector(by(df$y, df$cat, mean, simplify = TRUE)),
                                         each = 4),
                           yend.grpob = df$y
                           )
                           

    etasqrd <- summary.lm(aov(data = df, y  ~ cat))$r.squared
    ggplot(df,aes(x = x, y = y)) +
      #Grand mean line
      geom_line(y = mean(df$y),
                aes(linetype = "Grand Mean"),
                size = 1.3,
                alpha = ifelse("2" %in% input$selectGroup |
                               "3" %in% input$selectGroup |
                               "4" %in% input$selectGroup, 1, 0)) + 
      #Mean of group Clooney
      geom_segment(x = min(subset(df, cat == "Clooney")$x - .09),
                   xend = max(subset(df, cat == "Clooney")$x + .09),
                   y = mean(subset(df, cat == "Clooney")$y),
                   yend = mean(subset(df, cat == "Clooney")$y),
                   size = 1.2,
                   aes(colour = "Clooney"),
                   alpha = ifelse("1" %in% input$selectGroup |
                                  "4" %in% input$selectGroup |
                                  "5" %in% input$selectGroup, 1, 0)) + 
      #Mean of Group Jolie
      geom_segment(x = min(subset(df, cat == "Jolie")$x - .09),
                   xend = max(subset(df, cat == "Jolie")$x + .09),
                   y = mean(subset(df, cat == "Jolie")$y),
                   yend = mean(subset(df, cat == "Jolie")$y),
                   size = 1.2,
                   aes(colour = "Jolie"),
                   alpha = ifelse("1" %in% input$selectGroup |
                                    "4" %in% input$selectGroup |
                                    "5" %in% input$selectGroup, 1, 0)) + 
      #Mean of Group No Endorser
      geom_segment(x = min(subset(df, cat == "NoEnd")$x - .09),
                   xend = max(subset(df, cat == "NoEnd")$x + .09),
                   y = mean(subset(df, cat == "NoEnd")$y),
                   yend = mean(subset(df, cat == "NoEnd")$y),
                   size = 1.2,
                   aes(colour = "No Endorser"),
                   alpha = ifelse("1" %in% input$selectGroup |
                                    "4" %in% input$selectGroup |
                                    "5" %in% input$selectGroup, 1, 0)) +  
      #Arrows from grand mean to observation
      geom_segment(x = dfarrows$x.tot,
                   xend = dfarrows$xend.tot,
                   y = dfarrows$y.tot,
                   yend = dfarrows$yend.tot,
                   linetype = "dotted",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed"),
                   alpha = ifelse("3" %in% input$selectGroup, 1, 0)) +
      #Arrows from grand mean to group mean
      geom_segment(x = dfarrows$x.grpgr,
                   xend = dfarrows$xend.grpgr,
                   y = dfarrows$y.grpgr,
                   yend = dfarrows$yend.grpgr,
                   linetype = "solid",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed"),
                   alpha = ifelse("4" %in% input$selectGroup, 1, 0)) + 
      #Arrows from group to observation
      geom_segment(x = dfarrows$x.grpob,
                   xend = dfarrows$xend.grpob,
                   y = dfarrows$y.grpob,
                   yend = dfarrows$yend.grpob,
                   linetype = "solid",
                   color = "red",
                   arrow = arrow(length = unit(2,"mm"),
                                 ends = "both",
                                 type = "closed"),
                   alpha = ifelse("5" %in% input$selectGroup, 1, 0)) + 
      #Points representing observations
      geom_point(shape = 21,
                 size = 3,
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
      geom_text(label = paste("'eta'^2 ==", round(etasqrd,2)),
                x = 3,
                y = 9.5,
                parse = TRUE,
                alpha = ifelse("6" %in% input$selectGroup, 1, 0)) +
      #Legend definitions
      guides(colour = guide_legend(title = "Group Means"),
             linetype = guide_legend(title = "Grand Mean",label = FALSE),
             fill = FALSE) + 
      #X label definitions
      scale_x_continuous(name = "", breaks = 1:3,labels = c("Clooney", "Jolie", "No Endorser")) +
      scale_y_continuous(name = "Willingness", limits = c(1, 10), breaks = 1:10) +
      theme_general() + 
      theme(legend.position = "top", text = element_text(size = 15)) 
      
  })

  
})
