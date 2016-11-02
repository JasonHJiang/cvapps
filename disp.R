# data manip + utils
library(magrittr)
library(lubridate)
library(dplyr)
# use this more widely instead of data.frames?
library(data.table)
library(stringr)
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

cv_bcpnn <- hcopen %>% tbl("PT_IC_161027") %>%
  select(drug_code, event_effect, median_IC, LB95_IC, UB95_IC)
cv_rfet <- hcopen %>% tbl("PT_RFET_161101")
cv_prr <- hcopen %>% tbl("PT_PRR_160927")
cv_ror <- hcopen %>% tbl("PT_ROR_160927")
cv_rrr <- hcopen %>% tbl("PT_RRR_160927")
master_table_pt <- cv_bcpnn %>%
  inner_join(cv_rfet, by = c("drug_code", "event_effect")) %>%
  inner_join(cv_prr, by = c("drug_code", "event_effect")) %>%
  inner_join(cv_ror, by = c("drug_code", "event_effect"))

hlt_bcpnn <- hcopen %>% tbl("HLT_IC_161027") %>%
  select(drug_code, event_effect, median_IC, LB95_IC, UB95_IC)
hlt_rfet <- hcopen %>% tbl("HLT_RFET_161101")
hlt_prr <- hcopen %>% tbl("HLT_PRR_160927")
hlt_ror <- hcopen %>% tbl("HLT_ROR_160927")
hlt_rrr <- hcopen %>% tbl("HLT_RRR_160927")
master_table_hlt <- hlt_bcpnn %>%
  inner_join(hlt_rfet, by = c("drug_code", "event_effect")) %>%
  inner_join(hlt_prr, by = c("drug_code", "event_effect")) %>%
  inner_join(hlt_ror, by = c("drug_code", "event_effect"))

cv_drug_rxn_meddra <- hcopen %>% tbl("cv_drug_rxn_meddra")
cv_drug_rxn_2006 <- cv_drug_rxn_meddra %>% filter(quarter >= 2006.1)
count_quarter_pt <- group_by(cv_drug_rxn_2006, ing, PT_NAME_ENG, quarter) %>%
  dplyr::summarize(count = n_distinct(REPORT_ID)) %>%
  ungroup()
count_quarter_hlt <- group_by(cv_drug_rxn_2006, ing, HLT_NAME_ENG, quarter) %>%
  dplyr::summarize(count = n_distinct(REPORT_ID)) %>%
  ungroup()


# PT-HLT mapping
drug_PT_HLT <- cv_drug_rxn_2006 %>%
  dplyr::select(ing, PT_NAME_ENG, HLT_NAME_ENG) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(HLT_NAME_ENG))
# drug and adverse event dropdown menu choices
drug_choices <- drug_PT_HLT %>% dplyr::distinct(ing) %>% as.data.frame()
drug_choices <- sort(drug_choices$ing)


################################## UI component ####
ui <- dashboardPage(
  dashboardHeader(title = titleWarning("Shiny DISP (v0.10)"),
                  titleWidth = 700),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Disproportionality Analysis", tabName = "data", icon = icon("database")),
      selectizeInput(inputId ="search_drug",
                     label = "Generic Name/Ingredient",
                     choices = c("Start typing to search..." = "", drug_choices)),
      selectizeInput(inputId = "search_hlt",
                     label = "Adverse Event High-Level Term",
                     choices = c("Loading..." = "")),
      selectizeInput(inputId = "search_pt",
                     label = "Adverse Event Preferred Term",
                     choices = c("Loading..." = "")),
      checkboxInput(inputId = "checkbox_filter_pt",
                    label = "Only see PTs from chosen HLT",
                    value = FALSE),
      sliderInput(inputId = "min_count",
                  label = "Minimum count:", 
                  min=1, max=20, value=1),
      checkboxInput(inputId = "inf_filter_pt",
                    label = "Exclude Inf PRR from table",
                    value = TRUE),
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
      
      tabItem(tabName = "aboutinfo",
              h2("About the Shiny App"),
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
  
  # Data frame generate reactive function to be used to assign: data <- cv_reports_tab()
  search_tab <- reactive({
    input$search_button
    isolate({
      #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
      current_drug <- ifelse(input$search_drug == "", "Not Specified", input$search_drug)
      current_hlt <- ifelse(input$search_hlt == "", "Not Specified", input$search_hlt)
      current_pt <- ifelse(input$search_pt == "", "Not Specified", input$search_pt)
      
      search_tab_df <- data.frame(names = c("Generic Name:",
                                            "High-Level Term:",
                                            "Preferred Term:"),
                                  terms = c(current_drug,
                                            current_hlt,
                                            current_pt),
                                  stringsAsFactors=FALSE)
    })
  })
  # Display what query was searched
  output$current_search <- renderTable(
    search_tab(),
    include.colnames = FALSE
  )
  
  output$pt_data_dl <- downloadHandler(
    filename = function() {
      current_drug <- search_tab()$terms[1]
      if (current_drug == "Not Specified") current_drug <- "all"
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
      current_drug <- search_tab()$terms[1]
      if (current_drug == "Not Specified") current_drug <- "all"
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
    # PRR and ROR values of Inf means there are no other drugs associated with that specific adverse reaction, so denomimator is zero!
    # prr_tab_df is simply the table displayed at the bottom
    table <- master_table_pt %>%
      dplyr::select(drug_code, event_effect,
                    count = count.x, expected_count = expected_count.x,
                    median_IC, LB95_IC, UB95_IC,
                    midRFET, RFET,
                    PRR, LB95_PRR, UB95_PRR,
                    ROR, LB95_ROR, UB95_ROR) %>%
      filter(count >= input$min_count)
    if(input$search_drug != "") table %<>% filter(drug_code == input$search_drug)
    if(input$search_pt != "") table %<>% filter(event_effect == input$search_pt)

    # rank master table by PRR & suppress Inf to the end
    # ***** == "Infinity" is a way that currently works to filter equal to infinity in SQL with dplyr, might change
    table_inf <- table %>%
      filter(PRR == "Infinity") %>%
      dplyr::arrange(drug_code, event_effect) %>%
      as.data.frame(stringsAsFactors = FALSE)
    table %<>%
      filter(PRR != "Infinity") %>%
      dplyr::arrange(desc(median_IC), desc(LB95_IC), drug_code, event_effect) %>%
      as.data.frame(stringsAsFactors = FALSE)
    if (!input$inf_filter_pt) table %<>% bind_rows(table_inf)
    table %<>%
      lapply(function(x) {if(is.numeric(x)) round(x,3) else x}) %>%
      as.data.frame()
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
    if (0 == nrow(top_pairs)) {
      top_pairs <- table_pt_data() %>%
        dplyr::select(drug_code, event_effect) %>%
        head(10)
    }
    if (1 == nrow(top_pairs)) {
      # the SQL IN comparison complains if there's only one value to match to (when we specify both drug and rxn)
      timeplot_df <- count_quarter_pt %>% filter(ing == top_pairs$drug_code,
                                                 PT_NAME_ENG == top_pairs$event_effect) %>% as.data.frame()
    } else {
      timeplot_df <- count_quarter_pt %>% filter(ing %in% top_pairs$drug_code,
                                                 PT_NAME_ENG %in% top_pairs$event_effect) %>% as.data.frame()
    }

    report_quarters <- count_quarter_pt %>% select(quarter) %>% distinct() %>% as.data.frame()
    pairs_df <- top_pairs %>% rename(ing = drug_code, PT_NAME_ENG = event_effect) %>% data.frame(count = 0, stringsAsFactors = FALSE)
    quarters_df <- data.frame(quarter = report_quarters, count = 0)
    filled_time_df <- full_join(pairs_df, quarters_df) %>% bind_rows(timeplot_df)
    filled_time_df %<>% group_by(ing, PT_NAME_ENG, quarter) %>% summarise(count = sum(count))
    filled_time_df
  })
  })


  table_hlt_data <- reactive({
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
      # PRR and ROR values of Inf means there are no other drugs associated with that specific adverse reaction, so denomimator is zero!
      # prr_tab_df is simply the table displayed at the bottom
      table <- master_table_hlt %>%
        dplyr::select(drug_code, event_effect,
                      count = count.x, expected_count = expected_count.x,
                      median_IC, LB95_IC, UB95_IC,
                      midRFET, RFET,
                      PRR, LB95_PRR, UB95_PRR,
                      ROR, LB95_ROR, UB95_ROR) %>%
        filter(count >= input$min_count)
      if(input$search_drug != "") table %<>% filter(drug_code == input$search_drug)
      if(input$search_hlt != "") table %<>% filter(event_effect == input$search_hlt)

      # rank master table by PRR & suppress Inf to the end
      # ***** == "Infinity" is a way that currently works to filter equal to infinity in SQL with dplyr, might change
      table_inf <- table %>%
        filter(PRR == "Infinity") %>%
        dplyr::arrange(drug_code, event_effect) %>%
        as.data.frame(stringsAsFactors = FALSE)
      table %<>%
        filter(PRR != "Infinity") %>%
        dplyr::arrange(desc(median_IC), desc(LB95_IC), drug_code, event_effect) %>%
        as.data.frame(stringsAsFactors = FALSE)
      if (!input$inf_filter_pt) table %<>% bind_rows(table_inf)
      table %<>%
        lapply(function(x) {if(is.numeric(x)) round(x,3) else x}) %>%
        as.data.frame()
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
      if (0 == nrow(top_pairs)) {
        top_pairs <- table_pt_data() %>%
          dplyr::select(drug_code, event_effect) %>%
          head(10)
      }
      if (1 == nrow(top_pairs)) {
        # the SQL IN comparison complains if there's only one value to match to (when we specify both drug and rxn)
        timeplot_df <- count_quarter_hlt %>% filter(ing == top_pairs$drug_code,
                                                    HLT_NAME_ENG == top_pairs$event_effect) %>% as.data.frame()
      } else {
        timeplot_df <- count_quarter_hlt %>% filter(ing %in% top_pairs$drug_code,
                                                    HLT_NAME_ENG %in% top_pairs$event_effect) %>% as.data.frame()
      }
      report_quarters <- count_quarter_hlt %>% select(quarter) %>% distinct() %>% as.data.frame()
      pairs_df <- top_pairs %>% rename(ing = drug_code, HLT_NAME_ENG = event_effect) %>% data.frame(count = 0, stringsAsFactors = FALSE)
      quarters_df <- data.frame(quarter = report_quarters, count = 0)
      filled_time_df <- full_join(pairs_df, quarters_df) %>% bind_rows(timeplot_df)
      filled_time_df %<>% group_by(ing, HLT_NAME_ENG, quarter) %>% summarise(count = sum(count))
      filled_time_df
    })
  })
  


  # PRR Time Plot
  output$timeplot_pt <- renderPlot({
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
    current_drug <- ifelse(input$search_drug == "","All Drugs",input$search_drug)
    current_rxn <- ifelse(input$search_pt == "","Top 10 Reactions with Highest IC Estimates",input$search_pt)
    })
    plottitle <- paste("Non-Cumulative Report Count Time Plot for:", current_drug, "&", current_rxn)

    df <- time_data_pt() %>%
      mutate(qtr = as.yearqtr(quarter %>% as.character(), '%Y.%q'))
    p <- ggplot(df, aes(x = qtr, y = count)) +
      scale_x_yearqtr(breaks = seq(min(df$qtr), max(df$qtr), 0.25),
                      format = "%Y Q%q") +
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
      buttons = list(list(extend = 'colvis',
                          text = 'Columns to display',
                          columns = 5:15)),
      columnDefs = list(list(visible = FALSE,
                             targets = c(7, 11, 12, 14, 15)))
    )))

  # PRR Time Plot
  output$timeplot_hlt <- renderPlot({
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
      current_drug <- ifelse(input$search_drug == "","All Drugs",input$search_drug)
      current_rxn <- ifelse(input$search_hlt == "","Top 10 Reactions with Highest IC Estimates",input$search_hlt)
    })
    plottitle <- paste("Non-Cumulative Report Count Time Plot for:", current_drug, "&", current_rxn)

    df <- time_data_hlt() %>%
      mutate(qtr = as.yearqtr(quarter %>% as.character(), '%Y.%q'))
    p <- ggplot(df, aes(x = qtr, y = count)) +
      scale_x_yearqtr(breaks = seq(min(df$qtr), max(df$qtr), 0.25),
                      format = "%Y Q%q") +
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
      buttons = list(list(extend = 'colvis',
                          text = 'Columns to display',
                          columns = 5:15)),
      columnDefs = list(list(visible = FALSE,
                             targets = c(7, 11, 12, 14, 15)))
    )))
}

options(shiny.trace = FALSE, shiny.reactlog = FALSE)
shinyApp(ui, server)