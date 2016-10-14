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
source("common_ui.R")

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
master_table_pt <- cv_prr %>%
  left_join(cv_bcpnn, by = c("drug_code", "event_effect")) %>%
  left_join(cv_ror, by = c("drug_code", "event_effect"))

hlt_prr <- hcopen %>% tbl("HLT_PRR_160927")
hlt_ror <- hcopen %>% tbl("HLT_ROR_160927")
master_table_hlt <- hlt_prr %>%
  left_join(hlt_ror, by = c("drug_code", "event_effect"))

cv_drug_rxn_meddra <- hcopen %>% tbl("cv_drug_rxn_meddra")
cv_drug_rxn_2006 <- cv_drug_rxn_meddra %>% filter(quarter >= 2006.1)
count_quarter_pt <- group_by(cv_drug_rxn_2006, ing, PT_NAME_ENG, quarter) %>%
  dplyr::summarize(count = n_distinct(REPORT_ID))
count_quarter_hlt <- group_by(cv_drug_rxn_2006, ing, HLT_NAME_ENG, quarter) %>%
  dplyr::summarize(count = n_distinct(REPORT_ID))


# PT-HLT mapping
drug_PT_HLT <- cv_drug_rxn_2006 %>%
  dplyr::select(ing, PT_NAME_ENG, HLT_NAME_ENG) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(HLT_NAME_ENG)) %>% as.data.frame()
# drug and adverse event dropdown menu choices
drug_choices <- drug_PT_HLT %>% dplyr::distinct(ing) %>% as.data.frame()
drug_choices <- sort(drug_choices$ing)


################################## UI component ####
ui <- dashboardPage(
  dashboardHeader(title = titleWarning("Shiny DISP (v0.03)"),
                  titleWidth = 700),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
      menuItem("Disproportionality Analysis", tabName = "data", icon = icon("database")),
      conditionalPanel(
        "input.sidebarmenu === 'data'",
        selectizeInput(inputId ="search_drug",
                       label = "Generic Name/Ingredient",
                       choices = c("", drug_choices),
                       options = list(placeholder = 'Start typing to search...')),
        
        selectizeInput(inputId = "search_hlt",
                       label = "Adverse Event High-Level Term",
                       choices = NULL,
                       options = list(placeholder = 'Start typing to search...')),
        
        selectizeInput(inputId = "search_pt",
                       label = "Adverse Event Preferred Term",
                       choices = NULL,
                       options = list(placeholder = 'Start typing to search...')),
        
        checkboxInput(inputId = "checkbox_filter_pt",
                      label = "Only see PTs from chosen HLT",
                      value = FALSE),
        
        actionButton(inputId = "search_button",
                     label = "Search",
                     width = '100%')
      ),
      
      menuItem("Documentation", tabName = "Documentation", icon = icon("flag")),
      # menuItem("Download", tabName = "downloaddata", icon = icon("fa fa-download")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"))
    )
    
  ), 
  
  dashboardBody(
    customCSS(),
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                tabBox(
                  tabPanel(
                    "Preferred Terms",
                    plotOutput(outputId = "timeplot_pt", height = "285px"),
                    tags$br(),
                    DT::dataTableOutput("table_pt"),
                    tags$p("NOTE: The above table is ranked decreasingly by PRR value. All drug*reactions pairs that have PRR value of infinity are added at the end of the table."),
                    width = 12),
                  tabPanel(
                    "High-Level Terms",
                    plotOutput(outputId = "timeplot_hlt", height = "285px"),
                    tags$br(),
                    DT::dataTableOutput("table_hlt"),
                    tags$p("NOTE: The above table is ranked decreasingly by PRR value. All drug*reactions pairs that have PRR value of infinity are added at the end of the table."),
                    width = 12),
                  width = 12
                )
              )
      ),
      
      tabItem(tabName = "Documentation",
              fluidRow(
                box(
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
              aboutAuthors()
      )
      )
      ), 
  
  skin = "blue"
      )

############################## Server component ####
server <- function(input, output, session) {
  # Relabel rxns dropdown menu based on selected drug
  observeEvent(input$search_drug, {
    hlt_choices <- drug_PT_HLT
    pt_choices <- drug_PT_HLT
    if ("" != input$search_drug) {
      hlt_choices %<>% filter(ing == input$search_drug)
      pt_choices %<>% filter(ing == input$search_drug)
    }
    hlt_choices %<>%
      dplyr::distinct(HLT_NAME_ENG) %>%
      as.data.frame() %>% `[[`(1) %>%
      sort()
    pt_choices %<>%
      dplyr::distinct(PT_NAME_ENG) %>%
      as.data.frame() %>% `[[`(1) %>%
      sort()
    updateSelectizeInput(session, "search_hlt",
                         choices = c("", hlt_choices))
    updateSelectizeInput(session, "search_pt",
                         choices = c("", pt_choices))
  })
  
  # Relabel PT dropdown menu based on selected HLT
  observe({
    input$search_hlt
    input$checkbox_filter_pt
    isolate({
      pt_choices <- drug_PT_HLT
      pt_selected <- input$search_pt
      if ("" != input$search_drug)
        pt_choices %<>% filter(ing == input$search_drug)
      if (input$checkbox_filter_pt & "" != input$search_hlt)
        pt_choices %<>% filter(HLT_NAME_ENG == input$search_hlt)
      pt_choices %<>%
        dplyr::distinct(PT_NAME_ENG) %>%
        as.data.frame() %>% `[[`(1) %>%
        sort()
      if (! pt_selected %in% pt_choices) pt_selected = ""
      updateSelectizeInput(session, "search_pt",
                           choices = c("", pt_choices),
                           selected = pt_selected)
  })
  })
  
  # Select HLT dropdown menu based on selected PT
  observeEvent(input$search_pt, {
    if ("" != input$search_pt) {
      hlt_mapped <- drug_PT_HLT %>%
        filter(PT_NAME_ENG == input$search_pt) %>%
        dplyr::distinct(HLT_NAME_ENG) %>%
        as.data.frame() %>% `[[`(1)
      updateSelectizeInput(session, "search_hlt",
                           selected = hlt_mapped)
    }
  })
  
  # PRR tab 
  table_pt_data <- reactive({
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
    # PRR and ROR values of Inf means there are no other drugs associated with that specific adverse reaction, so denomimator is zero!
    # prr_tab_df is simply the table displayed at the bottom
    table <- master_table_pt %>%
      dplyr::select(drug_code, event_effect,
                    count = count.x, expected_count = expected_count.x,
                    PRR, LB95_PRR, UB95_PRR,
                    IC,  LB95_IC,  UB95_IC,
                    ROR, LB95_ROR, UB95_ROR)
    if(input$search_drug != "") table %<>% filter(drug_code == input$search_drug)
    if(input$search_pt != "") table %<>% filter(event_effect == input$search_pt)
    
    # rank master table by PRR & suppress Inf to the end
    # ***** == "Infinity" is a way that currently works to filter equal to infinity in SQL with dplyr, might change
    table_inf <- table %>%
      filter(PRR == "Infinity") %>%
      dplyr::arrange(drug_code, event_effect) %>%
      as.data.frame()
    table %<>%
      filter(PRR != "Infinity") %>%
      dplyr::arrange(desc(PRR), desc(LB95_PRR), drug_code, event_effect) %>%
      as.data.frame() %>%
      bind_rows(table_inf) %>%
      lapply(function(x) {if(is.numeric(x)) round(x,3) else x}) %>%
      as.data.frame()
    table
  })
  })
  
  # time-series data 
  time_data_pt <- reactive({
    table_pt_data() # always update when this table changes, since order of execution isn't guaranteed
    isolate({
    top_pairs <- table_pt_data() %>%
      filter(PRR != Inf) %>%   # this comparison is done in R, not SQL, so Inf rather than "Infinity"
      dplyr::select(drug_code, event_effect) %>%
      head(10)
    if (1 == nrow(top_pairs)) {
      # the SQL IN comparison complains if there's only one value to match to (when we specify both drug and rxn)
      timeplot_df <- count_quarter_pt %>% filter(ing == top_pairs$drug_code,
                                                 PT_NAME_ENG == top_pairs$event_effect) %>% as.data.frame()
    } else {
      timeplot_df <- count_quarter_pt %>% filter(ing %in% top_pairs$drug_code,
                                                 PT_NAME_ENG %in% top_pairs$event_effect) %>% as.data.frame()
    }
    timeplot_df
  })
  })
  
  # 
  table_hlt_data <- reactive({
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
      # PRR and ROR values of Inf means there are no other drugs associated with that specific adverse reaction, so denomimator is zero!
      # prr_tab_df is simply the table displayed at the bottom
      table <- master_table_hlt %>%
        dplyr::select(drug_code, event_effect,
                      count = count.x, expected_count = expected_count.x,
                      PRR, LB95_PRR, UB95_PRR,
                      ROR, LB95_ROR, UB95_ROR)
      if(input$search_drug != "") table %<>% filter(drug_code == input$search_drug)
      if(input$search_hlt != "") table %<>% filter(event_effect == input$search_hlt)
      
      # rank master table by PRR & suppress Inf to the end
      # ***** == "Infinity" is a way that currently works to filter equal to infinity in SQL with dplyr, might change
      table_inf <- table %>%
        filter(PRR == "Infinity") %>%
        dplyr::arrange(drug_code, event_effect) %>%
        as.data.frame()
      table %<>%
        filter(PRR != "Infinity") %>%
        dplyr::arrange(desc(PRR), desc(LB95_PRR), drug_code, event_effect) %>%
        as.data.frame() %>%
        bind_rows(table_inf) %>%
        lapply(function(x) {if(is.numeric(x)) round(x,3) else x}) %>%
        as.data.frame()
      table
    })
  })
  
  # time-series data 
  time_data_hlt <- reactive({
    table_hlt_data() # always update when this table changes, since order of execution isn't guaranteed
    isolate({
      top_pairs <- table_hlt_data() %>%
        filter(PRR != Inf) %>%   # this comparison is done in R, not SQL, so Inf rather than "Infinity"
        dplyr::select(drug_code, event_effect) %>%
        head(10)
      if (1 == nrow(top_pairs)) {
        # the SQL IN comparison complains if there's only one value to match to (when we specify both drug and rxn)
        timeplot_df <- count_quarter_hlt %>% filter(ing == top_pairs$drug_code,
                                                   HLT_NAME_ENG == top_pairs$event_effect) %>% as.data.frame()
      } else {
        timeplot_df <- count_quarter_hlt %>% filter(ing %in% top_pairs$drug_code,
                                                   HLT_NAME_ENG %in% top_pairs$event_effect) %>% as.data.frame()
      }
      timeplot_df
    })
  })
  
  
  
  # PRR Time Plot
  output$timeplot_pt <- renderPlot({
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
    current_drug <- ifelse(input$search_drug == "","All Drugs",input$search_drug)
    current_rxn <- ifelse(input$search_pt == "","Top 10 Reactions with Highest PRR Values",input$search_pt)
    })
    plottitle <- paste("Non-Cumulative Report Count Time Plot for:", current_drug, "&", current_rxn)
    
    df <- time_data_pt() %>%
      mutate(qtr = as.yearqtr(quarter %>% as.character(), '%Y.%q'))
    p <- ggplot(df, aes(x = qtr, y = count)) +
      scale_x_yearqtr(breaks = seq(min(df$qtr), max(df$qtr), 0.25),
                      format = "%Y Q%q") +
      ylim(0, max(df$count)) +
      geom_line(aes(colour=PT_NAME_ENG)) + geom_point()  + 
      ggtitle(plottitle) + 
      xlab("Quarter") + 
      ylab("Report Count") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=30, vjust=0.9, hjust=1))
    print(p)
    
  })
  
  # pass either datatable object or data to be turned into one to renderDataTable
  output$table_pt <- DT::renderDataTable(DT::datatable(
    table_pt_data(),
    extensions = 'Buttons',
    options = list(
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'pdf', 'colvis')
    )))
  
  # PRR Time Plot
  output$timeplot_hlt <- renderPlot({
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
      current_drug <- ifelse(input$search_drug == "","All Drugs",input$search_drug)
      current_rxn <- ifelse(input$search_hlt == "","Top 10 Reactions with Highest PRR Values",input$search_hlt)
    })
    plottitle <- paste("Non-Cumulative Report Count Time Plot for:", current_drug, "&", current_rxn)
    
    df <- time_data_hlt() %>%
      mutate(qtr = as.yearqtr(quarter %>% as.character(), '%Y.%q'))
    p <- ggplot(df, aes(x = qtr, y = count)) +
      scale_x_yearqtr(breaks = seq(min(df$qtr), max(df$qtr), 0.25),
                      format = "%Y Q%q") +
      ylim(0, max(df$count)) +
      geom_line(aes(colour=HLT_NAME_ENG)) + geom_point()  + 
      ggtitle(plottitle) + 
      xlab("Quarter") + 
      ylab("Report Count") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=30, vjust=0.9, hjust=1))
    print(p)
    
  })
  
  # pass either datatable object or data to be turned into one to renderDataTable
  output$table_hlt <- DT::renderDataTable(DT::datatable(
    table_hlt_data(),
    extensions = 'Buttons',
    options = list(
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'pdf', 'colvis')
    )))
}

options(shiny.trace = FALSE, shiny.reactlog = FALSE)
shinyApp(ui, server)