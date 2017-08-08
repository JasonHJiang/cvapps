# A reusable module for displaying data with
# a horizontal bar chart and a data table in a tab box

barTableUI1 <- function(id, header, titleInfo) {
  # Required for all modules
  # Must set the local namespace for this function
  # Example: if id = 'reporter', then the barchart module will be called
  #          'reporter-barchart'
  ns <- NS(id)
 
  # Basic title and info-box functionality.
  # The titleInfo argument should contain information pertaining to the graph
  # It is not optional
  infoClick <- h3(header,
                  tipify(
                    el = icon("info-circle"), trigger = "hover click",
                    title = titleInfo
                  ))
  
  # Must wrap UI elements in a tagList, then they follow standard Shiny UI placement
  # Notice the ids for Pie Chart and Table. They are wrapped in ns(). This means
  # that those ids are unique to this function, not the whole Shiny App.
  tagList(
    tabBox(
      tabPanel("Bar Chart",
               infoClick,
               htmlOutput(ns("barchart"))),
      tabPanel("Table",
               infoClick,
               htmlOutput(ns("table"))),
      width = 6
    )
  )
  
  
}

barTableUI <- function(plottitle, chartname, tablename,titleinfo) {
  infoClick <- h3(plottitle,
                  tipify(
                    el = icon("info-circle"), trigger = "hover click",
                    title = paste0(titleinfo)))
  tagList(
    tabBox(
      tabPanel("Bar Chart",
               infoClick,
               htmlOutput(chartname)),
      tabPanel("Table",
               infoClick,
               htmlOutput(tablename)),
      width = 6
    )
  )
}


barTable   <- function(input, output, session, dataChart, x, y, colour) {
  # Much like a shinyserver function, we require input, output and session
  # dataChart is where you will pass in your dataframe
  # x and y are for the bar chart, x being the labels and y being the counts
  # The colour argument is for the colour of the graph.
  output$barchart <- renderGvis({
    gvisBarChart_HCSC(dataChart, x, y, colour)
  })
  
  output$table    <- renderGvis({
    gvisTable(dataChart)
  })
}