# data manip + utils
library(magrittr)
library(lubridate)
library(tidyr)
library(dplyr)
library(utils)
library(zoo)
library(pool)

# data visualizations
library(plotly)
library(ggplot2)
library(googleVis)

# Shiny libraries
library(shiny)
library(shinydashboard)
library(DT)

source("common_ui.R")

#### Data pre-processing and server connections ####
hcopen_pool <- dbPool(drv = "PostgreSQL",
                 host = "shiny.hc.local",
                 dbname = "hcopen",
                 user = "hcreader",
                 password = "canada1")
hcopen <- src_pool(hcopen_pool)
gc()

# removing these columns first results in a faster join
cv_bcpnn <- hcopen %>% tbl("PT_IC_161027") %>%
  select(drug_code, event_effect, count, expected_count, median_IC, LB95_IC, UB95_IC)
cv_rfet <- hcopen %>% tbl("PT_RFET_161101") %>%
  select(drug_code, event_effect, midRFET, RFET)
cv_prr <- hcopen %>% tbl("PT_PRR_160927") %>%
  select(drug_code, event_effect, PRR, LB95_PRR, UB95_PRR)
cv_ror <- hcopen %>% tbl("PT_ROR_160927") %>%
  select(drug_code, event_effect, ROR, LB95_ROR, UB95_ROR)
# cv_rrr <- hcopen %>% tbl("PT_RRR_160927")
cv_counts <- hcopen %>% tbl("PT_counts_161103") %>%
  select(drug_code, event_effect, drug_margin, event_margin)
master_table_pt <- cv_bcpnn %>%
  inner_join(cv_rfet, by = c("drug_code", "event_effect")) %>%
  inner_join(cv_prr, by = c("drug_code", "event_effect")) %>%
  inner_join(cv_ror, by = c("drug_code", "event_effect")) %>%
  inner_join(cv_counts, by = c("drug_code", "event_effect")) %>%
  select(1:4, 16:17, 5:15) %>%
  as.data.frame() # already pull entire table to display at onset of app, might as well do everything locally (faster)

hlt_bcpnn <- hcopen %>% tbl("HLT_IC_161027") %>%
  select(drug_code, event_effect, count, expected_count, median_IC, LB95_IC, UB95_IC)
hlt_rfet <- hcopen %>% tbl("HLT_RFET_161101") %>%
  select(drug_code, event_effect, midRFET, RFET)
hlt_prr <- hcopen %>% tbl("HLT_PRR_160927") %>%
  select(drug_code, event_effect, PRR, LB95_PRR, UB95_PRR)
hlt_ror <- hcopen %>% tbl("HLT_ROR_160927") %>%
  select(drug_code, event_effect, ROR, LB95_ROR, UB95_ROR)
# hlt_rrr <- hcopen %>% tbl("HLT_RRR_160927")
hlt_counts <- hcopen %>% tbl("HLT_counts_161103") %>%
  select(drug_code, event_effect, drug_margin, event_margin)
master_table_hlt <- hlt_bcpnn %>%
  inner_join(hlt_rfet, by = c("drug_code", "event_effect")) %>%
  inner_join(hlt_prr, by = c("drug_code", "event_effect")) %>%
  inner_join(hlt_ror, by = c("drug_code", "event_effect")) %>%
  inner_join(hlt_counts, by = c("drug_code", "event_effect")) %>%
  select(1:4, 16:17, 5:15) %>%
  as.data.frame()

cv_drug_rxn_meddra <- hcopen %>% tbl("cv_drug_rxn_meddra")
cv_drug_rxn_2006 <- cv_drug_rxn_meddra %>% filter(quarter >= 2006.1)
count_quarter_pt <- count(cv_drug_rxn_2006, ing, PT_NAME_ENG, quarter) %>% as.data.frame()
count_quarter_hlt <- count(cv_drug_rxn_2006, ing, HLT_NAME_ENG, quarter) %>% as.data.frame()
quarters <- count_quarter_pt %>% select(quarter) %>% distinct() %>% as.data.frame()


# PT-HLT mapping
drug_PT_HLT <- cv_drug_rxn_2006 %>%
  select(ing, PT_NAME_ENG, HLT_NAME_ENG) %>%
  distinct() %>%
  filter(!is.na(HLT_NAME_ENG))
# drug and adverse event dropdown menu choices
drug_choices <- drug_PT_HLT %>% distinct(ing) %>% as.data.frame() %>% `$`("ing") %>% sort()


################################## UI component ####
ui <- dashboardPage(
  dashboardHeader(title = titleWarning("Shiny DISP (v0.12)"),
                  titleWidth = 700),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Disproportionality Analysis", tabName = "data", icon = icon("database")),
      selectizeInput(inputId ="search_drug",
                     label = "Generic Name/Ingredient",
                     choices = c(drug_choices, "Start typing to search..." = "")),
      selectizeInput(inputId = "search_hlt",
                     label = "Adverse Event High-Level Term",
                     choices = c("Loading..." = "")),
      selectizeInput(inputId = "search_pt",
                     label = "Adverse Event Preferred Term",
                     choices = c("Loading..." = "")),
      checkboxInput(inputId = "checkbox_filter_pt",
                    label = "Only see PTs from chosen HLT",
                    value = FALSE),
      div(style="display: inline-block; width: 50%;",
          textInput(inputId = "min_count",
                  label = "Min. count:",
                  value = "0")),
      div(style="display: inline-block; width: 50%;",
        textInput(inputId = "min_exp",
                  label = "Min. expected:",
                  value = "0")),
      checkboxInput(inputId = "inf_filter",
                    label = "Exclude Inf PRR from table",
                    value = FALSE),
      checkboxInput(inputId = "display_total",
                    label = "Plot total counts for query",
                    value = FALSE),
      # hacky way to get borders correct
      tags$div(class="form-group shiny-input-container",
               actionButton(inputId = "search_button",
                            label = "Search",
                            width = '100%')
      ),
      tags$h3(strong("Current Query:")),
      tableOutput("current_search"),
      downloadButton(outputId = "pt_data_dl",
                     label = "Export PT data"),
      downloadButton(outputId = "hlt_data_dl",
                     label = "Export HLT data"),
      menuItem("Documentation", tabName = "Documentation", icon = icon("flag")),
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
      
      tabItem(tabName = "aboutinfo", box(
        width = 12,
        h2("About"),
        # using tags$p() and tags$a() inserts spaces between text and hyperlink...thanks R
        HTML(paste0(
          "<p>",
          "This app has been developed by the Data Sciences Unit of RMOD at Health Canada as part of the Open Data Initiative. ",
          "This is a prototyping platform to utilize open data sources (Canada Vigilance Adverse Reaction Online Database) ",
          "to conduct disproportionality analysis for safety signal detection. It provides visualizations in an interactive ",
          "format to demonstrate the results of multiple disproportionality analysis.",
          "</p>",
          "<p>",
          "DO NOT USE FOR REGULATORY DECISION MAKING! The methods described and results generated from the work presented herein ",
          "relate solely to the testing of methodologies and representations for the evaluation of drugs and AERs. This report ",
          "neither replaces nor is intended to replace or comment on any regulatory decisions made by Health Canada.",
          "</p>",
          "<p>",
          "Data provided by the Canada Vigilance Adverse Reaction Online Database. The recency of the data is therefore ",
          "dependent on when the data source is updated, and is the responsibility of the Canada Vigilance Program. ",
          "For more information, please refer to ",
          "<a href = \"http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php\">",
          "http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php",
          "</a>.",
          "</p>")),
        tags$p("Detailed documentation on all disproportionality analyses can be found in Documentation tab."),
        aboutAuthors()
      ))
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
      distinct(HLT_NAME_ENG) %>%
      as.data.frame() %>% `[[`(1) %>%
      sort()
    pt_choices %<>%
      distinct(PT_NAME_ENG) %>%
      as.data.frame() %>% `[[`(1) %>%
      sort()
    updateSelectizeInput(session, "search_hlt",
                         choices = c("Start typing to search..." = "", hlt_choices))
    updateSelectizeInput(session, "search_pt",
                         choices = c("Start typing to search..." = "", pt_choices))
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
                           choices = c("Start typing to search..." = "", pt_choices),
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
  
  ########## Reactive data processing
  # Data structure to store current query info
  current_search <- reactive({
    input$search_button
    isolate({
      min_count <- as.numeric(input$min_count)
      if (is.na(min_count) | min_count < 0) min_count = 0
      min_count <- floor(min_count)
      updateTextInput(session, "min_count", value = min_count)
      
      min_exp <- as.numeric(input$min_exp)
      if (is.na(min_exp) | min_exp < 0) min_exp = 0
      updateTextInput(session, "min_exp", value = min_exp)
      
      list(min_count = min_count,
           min_exp = min_exp,
           drug = input$search_drug,
           hlt = input$search_hlt,
           pt = input$search_pt,
           filter_inf = input$inf_filter,
           display_total = input$display_total)
    })
  })
  
  ########## Output
  # Display what query was searched
  output$current_search <- renderTable({
    data <- current_search()
    result <- data.frame(names = c("Generic Name:",
                                   "High-Level Term:",
                                   "Preferred Term:"),
                         terms = c(data$drug,
                                   data$hlt,
                                   data$pt),
                         stringsAsFactors=FALSE)
    result["" == result] <- "Not Specified"
    result
  }, include.colnames = FALSE)
  
  output$pt_data_dl <- downloadHandler(
    filename = function() {
      current_drug <- current_search()$drug
      if (current_drug == "") current_drug <- "all"
      current_drug <- gsub(" ", "_", current_drug)
      current_drug <- gsub("\\|", "-", current_drug)
      paste0('pt_data_', current_drug, '.csv')
    },
    content = function(file) {
      write.csv(table_pt_data(), file, row.names=FALSE)
    }
  )
  output$hlt_data_dl <- downloadHandler(
    filename = function() {
      current_drug <- current_search()$drug
      if (current_drug == "") current_drug <- "all"
      current_drug <- gsub(" ", "_", current_drug)
      current_drug <- gsub("\\|", "-", current_drug)
      paste0('hlt_data_', current_drug, '.csv')
    },
    content = function(file) {
      write.csv(table_hlt_data(), file, row.names=FALSE)
    }
  )
  
  # PRR tab
  table_pt_data <- reactive({
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
      data <- current_search()
      # PRR and ROR values of Inf means there are no other drugs associated with that specific adverse reaction, so denomimator is zero!
      # prr_tab_df is simply the table displayed at the bottom
      table <- master_table_pt
      if (data$drug != "") table %<>% filter(drug_code == data$drug)
      if (data$pt != "") table %<>% filter(event_effect == data$pt)
      if (data$filter_inf) table %<>% filter(PRR != Inf)
      table %>% filter(count >= data$min_count) %>%
        filter(expected_count >= data$min_exp) %>%
        arrange(desc(median_IC), desc(LB95_IC), drug_code, event_effect) %>%
        as.data.frame() %>%
        lapply(function(x) {if (is.numeric(x)) round(x,3) else x}) %>%
        as.data.frame()
    })
  })
  
  # pass either datatable object or data to be turned into one to renderDataTable
  output$table_pt <- DT::renderDataTable(DT::datatable(
    table_pt_data(),
    extensions = 'Buttons',
    options = list(
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = list(list(extend = 'colvis',
                          text = 'Columns to display',
                          columns = 5:17)),
      columnDefs = list(list(visible = FALSE,
                             targets = c(5, 6, 9, 13, 14, 16, 17)))
    )))
  
  table_hlt_data <- reactive({
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
      data <- current_search()
      # PRR and ROR values of Inf means there are no other drugs associated with that specific adverse reaction, so denomimator is zero!
      # prr_tab_df is simply the table displayed at the bottom
      table <- master_table_hlt
      if (data$drug != "") table %<>% filter(drug_code == data$drug)
      if (data$hlt != "") table %<>% filter(event_effect == data$hlt)
      if (data$filter_inf) table %<>% filter(PRR != Inf)
      table %<>% filter(count >= data$min_count) %>%
        filter(expected_count >= data$min_exp) %>%
        arrange(desc(median_IC), desc(LB95_IC), drug_code, event_effect) %>%
        as.data.frame() %>%
        lapply(function(x) {if (is.numeric(x)) round(x,3) else x}) %>%
        as.data.frame()
    })
  })
  
  # pass either datatable object or data to be turned into one to renderDataTable
  output$table_hlt <- DT::renderDataTable(DT::datatable(
    table_hlt_data(),
    extensions = 'Buttons',
    options = list(
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = list(list(extend = 'colvis',
                          text = 'Columns to display',
                          columns = 5:17)),
      columnDefs = list(list(visible = FALSE,
                             targets = c(5, 6, 9, 13, 14, 16, 17)))
    )))
  
  # time-series data
  time_data_pt <- reactive({
    cur_search <- current_search()
    
    top_pairs <- table_pt_data() %>% head(10) %>% select(drug_code, event_effect) %>%
      mutate(drug_code = as.character(drug_code), event_effect = as.character(event_effect))
    timeplot_df <- count_quarter_pt %>% semi_join(top_pairs, by = c("ing" = "drug_code", "PT_NAME_ENG" = "event_effect"))
    quarters_df <- quarters %>% cbind(n = 0)
    pairs_df <- top_pairs %>% rename(ing = drug_code, PT_NAME_ENG = event_effect) %>% cbind(n = 0)
    filled_time_df <- full_join(pairs_df, quarters_df, by = "n") %>%
      bind_rows(timeplot_df) %>%
      count(ing, PT_NAME_ENG, quarter, wt = n) %>%
      ungroup() %>%
      mutate(label = paste0(ing, "_", PT_NAME_ENG)) %>%
      select(label, quarter, nn)
    if (cur_search$display_total) {
      total_df <- count_quarter_pt
      if (cur_search$drug != "") total_df %<>% filter(ing == cur_search$drug)
      if (cur_search$pt != "") total_df %<>% filter(PT_NAME_ENG == cur_search$pt)
      total_df %<>% count(quarter, wt = n) %>%
        mutate(label = paste0("total for query")) %>%
        select(label, quarter, nn)
      filled_time_df %<>% rbind(total_df)
    }
    
    filled_time_df %<>% rename(n = nn)
    
    # filled_time_df <- timeplot_df %>%
    #   mutate(label = paste0(ing, "_", PT_NAME_ENG)) %>%
    #   select(-ing, -PT_NAME_ENG) %>%
    #   spread(label, count)
    # filled_time_df[is.na(filled_time_df)] <- 0
    # filled_time_df
  })
  
  
  # Time Plot
  output$timeplot_pt <- renderPlot({
    data <- current_search()
    
    current_drug <- ifelse(data$drug == "", "All Drugs", data$drug)
    current_rxn <- ifelse(data$pt == "", "Top 10 Reactions with Highest IC Estimates", data$pt)
    plottitle <- paste("Non-Cumulative Report Count Time Plot for:", current_drug, "&", current_rxn)
    
    df <- time_data_pt() %>%
      #   mutate(qtr = (quarter%%1 - 0.1)*2.5 + quarter%/%1)
      # gvisLineChart(df,
      #               xvar = "qtr",
      #               yvar = names(df)[2:11]
      #               options = list(
      #                 height = 350,
      #                 vAxis = "{title: 'Number of Reports'}",
      #                 hAxis = "{title: 'Month'}",
      #                 chartArea = "{top: 10, height: '80%', left: 120, width: '84%'}")
      # )
      mutate(qtr = as.yearqtr(quarter %>% as.character(), '%Y.%q'))
    p <- ggplot(df, aes(x = qtr, y = n)) +
      scale_x_yearqtr(breaks = seq(min(df$qtr), max(df$qtr), 0.25),
                      format = "%Y Q%q") +
      geom_line(aes(colour=label)) + geom_point()  +
      ggtitle(plottitle) +
      xlab("Quarter") +
      ylab("Report Count") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=30, vjust=0.9, hjust=1))
    print(p)
    
  })
  
  
  # time-series data
  time_data_hlt <- reactive({
    cur_search <- current_search()
    
    top_pairs <- table_hlt_data() %>% head(10) %>% select(drug_code, event_effect) %>%
      mutate(drug_code = as.character(drug_code), event_effect = as.character(event_effect))
    timeplot_df <- count_quarter_hlt %>% semi_join(top_pairs, by = c("ing" = "drug_code", "HLT_NAME_ENG" = "event_effect"))
    quarters_df <- quarters %>% cbind(n = 0)
    pairs_df <- top_pairs %>% rename(ing = drug_code, HLT_NAME_ENG = event_effect) %>% cbind(n = 0)
    filled_time_df <- full_join(pairs_df, quarters_df, by = "n") %>%
      bind_rows(timeplot_df) %>%
      count(ing, HLT_NAME_ENG, quarter, wt = n) %>%
      ungroup() %>%
      mutate(label = paste0(ing, "_", HLT_NAME_ENG)) %>%
      select(label, quarter, nn)
    if (cur_search$display_total) {
      total_df <- count_quarter_hlt
      if (cur_search$drug != "") total_df %<>% filter(ing == cur_search$drug)
      if (cur_search$hlt != "") total_df %<>% filter(HLT_NAME_ENG == cur_search$hlt)
      total_df %<>% count(quarter, wt = n) %>%
        mutate(label = paste0("total for query")) %>%
        select(label, quarter, nn)
      filled_time_df %<>% rbind(total_df)
    }
    
    filled_time_df %<>% rename(n = nn)
  })
  

  # Time Plot
  output$timeplot_hlt <- renderPlot({
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
      current_drug <- ifelse(input$search_drug == "","All Drugs",input$search_drug)
      current_rxn <- ifelse(input$search_hlt == "","Top 10 Reactions with Highest IC Estimates",input$search_hlt)
    })
    plottitle <- paste("Non-Cumulative Report Count Time Plot for:", current_drug, "&", current_rxn)

    df <- time_data_hlt() %>%
      mutate(qtr = as.yearqtr(quarter %>% as.character(), '%Y.%q'))
    p <- ggplot(df, aes(x = qtr, y = n)) +
      scale_x_yearqtr(breaks = seq(min(df$qtr), max(df$qtr), 0.25),
                      format = "%Y Q%q") +
      geom_line(aes(colour=label)) + geom_point()  +
      ggtitle(plottitle) +
      xlab("Quarter") +
      ylab("Report Count") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=30, vjust=0.9, hjust=1))
    print(p)
  })

}

options(shiny.trace = FALSE, shiny.reactlog = FALSE)
shinyApp(ui, server)