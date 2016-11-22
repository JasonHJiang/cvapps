library(shiny)
library(shinyBS)
shinyApp(
  ui =
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          "adfasf", span(id = "infotest", icon("info")),
          sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30),
          bsTooltip("infotest", "The wait times will be broken into this many equally spaced bins",
                    "right", trigger = "click"),
          # bsPopover("infotest", "some title", "The wait times will be broken into this many equally spaced bins",
          #           "right")
          "adfasf",
          tipify(el = icon("info-circle"),
                 placement = "right",
                 title = "The wait times will be broken into this many equally spaced bins")
        ),
        mainPanel(
          plotOutput("distPlot")
        )
      )
    ),
  server =
    function(input, output, session) {
      output$distPlot <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
      })
      addPopover(session, "distPlot", "Data", content = paste0("Waiting time between ",
                                                               "eruptions and the duration of the eruption for the Old Faithful geyser ",
                                                               "in Yellowstone National Park, Wyoming, USA. Azzalini, A. and ",
                                                               "Bowman, A. W. (1990). A look at some data on the Old Faithful geyser. ",
                                                               "Applied Statistics 39, 357-365.

"), trigger = 'click')
    }
)
