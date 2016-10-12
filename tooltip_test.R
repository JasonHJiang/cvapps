library(shiny)
ui <- shinyUI(
  pageWithSidebar(
    headerPanel("Density Map"),
    sidebarPanel(
      tags$div(title="Click here to slide through years",
      sliderInput("slider_year", "YEAR:", 
                  min = 2001, max = 2011, value = 2009))
    ),
    mainPanel(  
      plotOutput("event_heatmap_map", width = "100%", height = "100%")
    ))
)

library(shiny)
library(ggplot2)
server <- shinyServer(function(input, output) {
  output$event_heatmap_map <- renderPlot(width = "auto", height = 640,{
    
  })
})



shinyApp(ui, server)