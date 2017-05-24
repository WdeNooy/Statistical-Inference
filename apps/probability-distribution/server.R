library(shiny)
library(DT)
library(RColorBrewer)


shinyServer(function(input, output) {
  
  #Load standard colors
  brewercolors <- brewer.pal(5, name =  "Spectral")
  names(brewercolors) <-c("Red", "Orange", "Yellow", "Green", "Blue")
  
  #Output the table
  output$probtable <- renderDataTable(server = FALSE,{
    
    #Generate probabilities of drawing # of yellow candy in sample of 10.  
    df <- data.frame(n_yellow = 0:10,
                     Probability = round(dbinom(0:10,10,input$probslider),3))
    
    #Prepare container for table, important bc of totals row in footer! 
    sketch <- htmltools::withTags(table(tableHeader(c("Yellow candies in a sample", "Probability")),
                                        tableFooter(c("Total", "1"))
                                        )
                                  )
    
    #Set options for data table
    opts = list(
      pageLength = 11,
      paging = FALSE,
      ordering = FALSE,
      searching = FALSE,
      info = FALSE,
      columnDefs = list(list(className = 'dt-center',
                        targets = 0:1))
    )
    #Make data table
    dt <-
      datatable(df,
                container = sketch,
                rownames = FALSE,
                options = opts)
    #Format datatable and add bars to fill
    formatStyle(
      table = dt,
      'Probability',
      background = styleColorBar(df$Probability, brewercolors['Blue']),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
              })

  
  })
  


