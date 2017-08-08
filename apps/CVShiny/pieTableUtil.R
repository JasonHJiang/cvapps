# A reusable module for displaying data with 
# a pie chart and a data table in a tab box

pieTableUI <- function(plottitle, chartname, tablename,titleinfo) {
  infoClick <- h3(plottitle,
                  tipify(
                    el = icon("info-circle"), trigger = "hover click",
                    title = paste0(titleinfo)))
  tagList(
    tabBox(
      tabPanel("Pie Chart",
               infoClick,
               htmlOutput(chartname)),
      tabPanel("Table",
               infoClick,
               htmlOutput(tablename)),
      width = 6
    )
  )
}


pieTableUI1 <- function(id, header, titleInfo, width = 6) {
  # Required for all modules
  # Must set the local namespace for this function
  # Example: if id = 'reporter', then the piechart module will be called
  #          'reporter-piechart'
  ns <- NS(id)
  
  # Basic title and info-box functionality.
  # The titleInfo argument should contain information pertaining to the graph
  # It is not optional
  infoClick <- h3(header,
                  tipify(
                    el = icon("info-circle"), trigger = "hover click",
                    title = titleInfo
                  ))
}

pieTable   <- function(input, output, session, dataChart, x, y) {
  # Much like a shinyserver function, we require input, output and session
  # dataChart is where you will pass in your dataframe
  # x and y are for the Pie Chart, x being the labels and y being the counts.
  output$piechart <- renderGvis({
    gvisPieChart_HCSC(dataChart, x, y)
  })
  
  output$table    <- renderGvis({
    gvisTable(dataChart)
  })
  
}
