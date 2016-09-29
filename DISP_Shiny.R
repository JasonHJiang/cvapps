library(shinydashboard)
library(jsonlite)
library(data.table)
library(ggplot2)
library(magrittr)
library(plotly)
library(pool)
library(shiny)
library(DT)
library(googleVis)
library(stringr)
library(utils)
library(dplyr)
library(zoo)

#### Data pre-processing and server connections ####
hcopen_pool <- dbPool(drv = "PostgreSQL",
                 host = "shiny.hc.local",
                 dbname = "hcopen",
                 user = "hcreader",
                 password = "canada1")
hcopen <- src_pool(hcopen_pool)

cv_prr <- hcopen %>% tbl("PT_PRR_160927")
cv_bcpnn <- hcopen %>% tbl("IC_160829") %>%
  dplyr::select(drug_code = `drug code`, event_effect = `event effect`, IC,
                LB95_IC = `Q_0.025(log(IC))`, UB95_IC = `Q_0.975(log(IC))`)
cv_ror <- hcopen %>% tbl("PT_ROR_160927")
cv_drug_rxn <- hcopen %>% tbl("cv_drug_rxn")

cv_drug_rxn_2006 <- cv_drug_rxn %>% filter(quarter >= 2006.1)
count_df_quarter <- group_by(cv_drug_rxn_2006,ing,PT_NAME_ENG,quarter) %>%
  dplyr::summarize(count = n_distinct(REPORT_ID))

# using pool reaches an error here: dplyr can just join using SQL commands, but in pool you can't join
master_table <- cv_prr %>%
  left_join(cv_bcpnn, by = c("drug_code", "event_effect")) %>%
  left_join(cv_ror, by = c("drug_code", "event_effect"))

# drug and adverse event dropdown menu choices
choices <- cv_prr %>% dplyr::distinct(drug_code) %>% as.data.frame()
choices <- c("", sort(choices$drug_code))

################################## UI component ####
ui <- dashboardPage(
  dashboardHeader(title = "Shiny DISP v0.02\nWARNING: This is a beta product. DO NOT use as sole evidence to support regulatory decisions.",  titleWidth = 1200),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
      menuItem("Disproportionality Analysis", tabName = "data", icon = icon("database")),
      conditionalPanel(
        "input.sidebarmenu === 'data'",
        selectizeInput(inputId ="search_drug",
                       label = "Generic Name/Ingredient",
                       choices = choices,
                       options = list(placeholder = 'Start typing to search...')),
        
        selectizeInput(inputId = "search_hlt",
                       label = "Adverse Event High-Level Term",
                       choices = NULL,
                       options = list(placeholder = '[Currently unused]',
                                      onInitialize = I('function() { this.setValue(""); }'))),
        
        selectizeInput(inputId = "search_pt",
                       label = "Adverse Event Preferred Term",
                       choices = NULL,
                       options = list(placeholder = 'Start typing to search...',
                                      onInitialize = I('function() { this.setValue(""); }'))),
        
        actionButton("search_button", "Search", width = '100%')
      ),
      
      menuItem("Documentation", tabName = "Documentation", icon = icon("flag")),
      # menuItem("Download", tabName = "downloaddata", icon = icon("fa fa-download")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"))
    )
    
  ), 
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                tabBox(
                  tabPanel(
                    "Time Plot",
                    plotOutput(outputId = "timeplot", height = "285px"),
                    tags$br(),
                    DT::dataTableOutput("display_table"),
                    tags$p("NOTE: The above table is ranked decreasingly by PRR value. All drug*reactions pairs that have PRR value of infinity are added at the end of the table."),
                    width = 12),
                  width=12
                  )
                  )
      ),
      
      tabItem(tabName = "Documentation",
              fluidRow(
                tabBox(
                  tabPanel(
                    "Documentation",
                    includeMarkdown("/home/shared/DISP data/CopyOfDISP about/DISP_about.md"),
                    tags$br(),
                    width = 12),
                  width=12
                )
              )
      ),
      
      tabItem(tabName = "aboutinfo",
              tags$h2("About the Shiny App"),
              tags$p("This is a prototyping platform to utilize open data sources (e.g. Canada Vigilance Adverse Reaction Online Database) 
                      to conduct disproportionality analysis for safety signal detection.
                      It provides visualizations in an interactive format to demonstrate the results of multiple disproportionality analysis.
                      This version was last updated in September of 2016.
                      Data provided by the Canada Vigilance Adverse Reaction Online Database: "),
              tags$a(href="http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php", "Click here!"),
              tags$p("Detailed documentation on all disproportionality analyses can be found in Documentation tab."),
              #tags$a(href = "https://rstudio.hres.ca/?view=rmarkdown", "Documentation of Analysis"),
              tags$br(),
              tags$strong("Authors:"),
              fluidRow(
                box(
                  tags$p("Daniel Buijs, MSc"),
                  tags$p("Data Scientist, Health Products and Food Branch"),
                  tags$p("Health Canada / Government of Canada"),
                  tags$p("daniel.buijs@hc-sc.gc.ca")
                ),
                box(
                  tags$p("Sophia He, BSc (in progress)"),
                  tags$p("Jr. Data Scientist Co-op, Health Products and Food Branch"),
                  tags$p("Health Canada / Government of Canada"),
                  tags$p("sophia.he@canada.ca or yunqingh@sfu.ca")
                ),
                box(
                  tags$p("Kevin Thai, BSc (in progress)"),
                  tags$p("Jr. Data Scientist Co-op, Health Products and Food Branch"),
                  tags$p("Health Canada / Government of Canada"),
                  tags$p("kevin.thai@canada.ca or kthai@uwaterloo.ca")
                )
              )
      )
      )
      ), 
  
  skin = "blue"
      )

############################## Server component ####
server <- function(input, output, session) {
  # Relabel rxns dropdown menu based on selected drug
  observe({
    if ("" == input$search_drug) {
      rxn_choices <- cv_prr %>%
        dplyr::distinct(event_effect) %>%
        as.data.frame() %>% `[[`(1) %>%
        sort()
    } else {
      rxn_choices <- cv_prr %>%
        filter(drug_code == input$search_drug) %>%
        dplyr::distinct(event_effect) %>%
        as.data.frame() %>% `[[`(1) %>%
        sort()
    }
    updateSelectizeInput(session, "search_pt",
                      label = "Select Adverse Event:",
                      choices = rxn_choices)
  })
  
  # PRR tab 
  cv_prr_tab <- reactive({
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
    # PRR and ROR values of Inf means there are no other drugs associated with that specific adverse reaction, so denomimator is zero!
    # prr_tab_df is simply the table displayed at the bottom
    prr_tab <- master_table
    if(input$search_drug != "") prr_tab %<>% filter(drug_code == input$search_drug)
    if(input$search_pt != "") prr_tab %<>% filter(event_effect == input$search_pt)
    
    # rank master table by PRR & suppress Inf to the end
    # ***** == "Infinity" is a way that currently works to filter equal to infinity in SQL with dplyr, might change
    prr_tab_inf <- prr_tab %>%
      filter(PRR == "Infinity") %>%
      dplyr::arrange(drug_code, event_effect) %>%
      as.data.frame()
    prr_tab_df <- prr_tab %>%
      filter(PRR != "Infinity") %>%
      dplyr::arrange(desc(PRR), desc(LB95_PRR), drug_code, event_effect) %>%
      as.data.frame() %>%
      bind_rows(prr_tab_inf)
    prr_tab_df %<>%
      dplyr::mutate(PRR = round(PRR,3),
                    UB95_PRR = round(UB95_PRR,3),
                    LB95_PRR = round(LB95_PRR,3),
                    LB95_IC = round(LB95_IC,3),
                    UB95_IC = round(UB95_IC,3),
                    IC = round(IC,3),
                    ROR = round(ROR,3),
                    UB95_ROR = round(UB95_ROR,3),
                    LB95_ROR = round(LB95_ROR,3)) %>%
      dplyr::select(drug_code, event_effect,
                    count = count.x, expected_count = expected_count.x,
                    PRR, LB95_PRR, UB95_PRR,
                    IC,  LB95_IC,  UB95_IC,
                    ROR, LB95_ROR, UB95_ROR)
    prr_tab_df
  })
  })
  
  # time-series data 
  time_data <- reactive({
    cv_prr_tab() # always update when this table changes, since order of execution isn't guaranteed
    isolate({
    top_pairs <- cv_prr_tab() %>%
      filter(PRR != Inf) %>%   # this comparison is done in R, not SQL, so Inf rather than "Infinity"
      dplyr::select(drug_code, event_effect) %>%
      head(10)
    if (1 == nrow(top_pairs)) {
      # the SQL IN comparison complains if there's only one value to match to (when we specify both drug and rxn)
      timeplot_df <- count_df_quarter %>% filter(ing == top_pairs$drug_code,
                                                 PT_NAME_ENG == top_pairs$event_effect) %>% as.data.frame()
    } else {
      timeplot_df <- count_df_quarter %>% filter(ing %in% top_pairs$drug_code,
                                                 PT_NAME_ENG %in% top_pairs$event_effect) %>% as.data.frame()
    }
    timeplot_df
  })
  })
  
  # PRR Time Plot
  output$timeplot <- renderPlot({
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
    current_drug <- ifelse(input$search_drug == "","All Drugs",input$search_drug)
    current_rxn <- ifelse(input$search_pt == "","Top 10 Reactions with Highest PRR Values",input$search_pt)
    })
    plottitle <- paste("Non-Cumulative Report Count Time Plot for:", current_drug, "&", current_rxn)
    
    df <- time_data() %>%
      mutate(qtr = as.yearqtr(quarter %>% as.character(), '%Y.%q'))
    p <- ggplot(df, aes(x = qtr, y = count)) +
      scale_x_yearqtr(breaks = seq(min(df$qtr), max(df$qtr), 0.25),
                      format = "%Y Q%q") +
      # scale_y_discrete(breaks = seq(-2, max(df$count)+2, 1)) +
      geom_line(aes(colour=PT_NAME_ENG)) + geom_point()  + 
      ggtitle(plottitle) + 
      xlab("Quarter") + 
      ylab("Report Count") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=30, vjust=0.9, hjust=1))
    print(p)
    
  })
  
  # pass either datatable object or data to be turned into one to renderDataTable
  output$display_table <- DT::renderDataTable(DT::datatable(
    cv_prr_tab(),
    extensions = 'Buttons',
    options = list(
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'pdf', 'colvis')
    )))
}

options(shiny.trace = FALSE, shiny.reactlog = FALSE)
shinyApp(ui, server)