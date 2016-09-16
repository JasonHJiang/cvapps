library(shinydashboard)
library(jsonlite)
library(lubridate)
library(plyr)
library(data.table)
library(ggplot2)
library(magrittr)
library(plotly)
library(shiny)
library(DT)
library(googleVis)
library(stringr)
library(utils)
library(dplyr)

#### Data pre-processing and server connections ####
hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")

cv_prr <- tbl(hcopen, "PRR_160826") %>% dplyr::select(-row.names) %>% dplyr::rename(LB95_PRR= LB95_CI_PRR)
cv_bcpnn <- tbl(hcopen, "IC_160829")%>%
  dplyr::select(drug_code = `drug code`, event_effect = `event effect`, IC,
                LB95_IC = `Q_0.025(log(IC))`, UB95_IC = `Q_0.975(log(IC))`)
cv_ror <- tbl(hcopen, "ROR_160826") %>% dplyr::select(-row.names)
cv_drug_rxn <- tbl(hcopen, "cv_drug_rxn")

cv_drug_rxn_2006 <- cv_drug_rxn %>% filter(quarter >= 2006.1)
count_df_quarter <- group_by(cv_drug_rxn_2006,ing,PT_NAME_ENG,quarter) %>%
  dplyr::summarize(count = n_distinct(REPORT_ID))

master_table <- cv_prr %>% left_join(cv_bcpnn) %>% left_join(cv_ror)

# drug and adverse event dropdown menu choices
choices <- cv_prr %>% dplyr::distinct(drug_code) %>% as.data.frame()
choices <- c("", sort(choices$drug_code))

#### UI component ####
ui <- dashboardPage(
  dashboardHeader(title = "Shiny DISP v0.01\nWARNING: This is a beta product. Please do NOT use as sole evidence to support regulartory decisions.",  titleWidth = 1200),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Disproportionality Analysis", tabName = "data", icon = icon("fa fa-cogs")),
      menuItem("Documentation", tabName = "Documentation", icon = icon("fa fa-binoculars")),
      # menuItem("ROR", tabName = "rordata", icon = icon("fa fa-database")),
      # menuItem("Download", tabName = "downloaddata", icon = icon("fa fa-download")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"))
    ),
    
    selectizeInput(inputId ="search_drug",
                   label = "Generic Name/Ingredient",
                   choices = choices,
                   options = list(placeholder = 'Start typing to search...')),
    
    selectizeInput(inputId = "search_rxn",
                   label = "Adverse Event Term",
                   choices = NULL,
                   options = list(placeholder = 'Start typing to search...',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    
    actionButton("search_button", "Search", width = '100%')
  ), 
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                tabBox(
                  tabPanel(
                    "Time Plot",
                    plotOutput(outputId = "top10_prr_timeplot"),
                    tags$br(),
                    dataTableOutput("display_table"),
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

#### Server component ####
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
    updateSelectizeInput(session, "search_rxn",
                      label = "Select Adverse Event:",
                      choices = rxn_choices)
  })
  
  # PRR tab 
  cv_prr_tab <- reactive({
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
    # PRR and ROR values of Inf means there are no other drugs associated with that specific adverse reaction, so denomimator is zero!
    # master_table[master_table$event_effect == "Refraction disorder",]
      # returns timeplot_df and prr_tab_df
      # timeplot_df is a data frame containing the counts per drug/rxn pair per quarter
      # prr_tab_df is simply the table displayed at the bottom
      # ***** == "Infinity" is a way that currently works to filter equal to infinity in SQL with dplyr, might change
    if(input$search_drug == "" & input$search_rxn == ""){
      default_pairs <- master_table %>% filter(PRR != "Infinity") %>% dplyr::top_n(10, PRR) %>%
        dplyr::select(drug_code, event_effect) %>% as.data.frame()
      timeplot_df <- count_df_quarter %>%
        filter(ing %in% default_pairs$drug_code &
                 PT_NAME_ENG %in% default_pairs$event_effect)
      
      # rank master table by PRR & suppress Inf to the end
      prr_tab_inf <- master_table %>% filter(PRR == "Infinity") %>% as.data.frame()
      prr_tab_df <- master_table %>% filter(PRR != "Infinity") %>%
        dplyr::arrange(desc(PRR), drug_code, event_effect) %>%
        as.data.frame() %>% bind_rows(prr_tab_inf)
    
    } else if(input$search_drug != "" & input$search_rxn == "") {
      prr_tab_inf <- master_table %>%
        filter(drug_code == input$search_drug) %>%
        filter(PRR == "Infinity") %>% as.data.frame()
      prr_tab_df <- master_table %>%
        filter(drug_code == input$search_drug) %>%
        filter(PRR != "Infinity") %>%
        dplyr::arrange(desc(PRR), drug_code, event_effect) %>%
        as.data.frame() %>%
        bind_rows(prr_tab_inf)
      
      timeplot_top10_rxn <- prr_tab_df[1:10,] %>% select(drug_code, event_effect)
      timeplot_df <- count_df_quarter %>%
        filter(ing %in% timeplot_top10_rxn$drug_code &
                 PT_NAME_ENG %in% timeplot_top10_rxn$event_effect)
    } else if(input$search_drug == "" & input$search_rxn != "") {
    } else {
      timeplot_df <- count_df_quarter %>%
        filter(ing == input$search_drug, PT_NAME_ENG == input$search_rxn)
      prr_tab_df <- master_table %>%
        filter(drug_code == input$search_drug, event_effect == input$search_rxn)
    }
    prr_tab_df %<>% as.data.frame()
    prr_tab_df %<>% dplyr::mutate(PRR = round(PRR,3),
                                  UB95_PRR = round(UB95_PRR,3),
                                  LB95_PRR = round(LB95_PRR,3),
                                  log_PRR = round(log_PRR,3),
                                  LB95_log_PRR = round(LB95_log_PRR,3),
                                  UB95_log_PRR = round(UB95_log_PRR,3),
                                  LB95_IC = round(LB95_IC,3),
                                  UB95_IC = round(UB95_IC,3),
                                  IC = round(IC,3),
                                  ROR = round(ROR,3),
                                  UB95_ROR = round(UB95_ROR,3),
                                  LB95_ROR = round(LB95_ROR,3),
                                  log_ROR = round(log_ROR,3),
                                  UB95_log_ROR = round(UB95_log_ROR,3),
                                  LB95_log_ROR = round(LB95_log_ROR,3))
    list(timeplot_df %<>% as.data.frame(), prr_tab_df)
  })
  })
  
  # PRR Time Plot
  output$top10_prr_timeplot <- renderPlot({
    df <- cv_prr_tab()[[1]]
    isolate({
    current_drug <- ifelse(input$search_drug == "","All Drugs",input$search_drug)
    current_rxn <- ifelse(input$search_rxn == "","All Reactions",input$search_rxn)
    })
    if(current_rxn == "All Reactions"){
      plottitle <- paste("Non-Cumulative Report Count Time Plot for:", current_drug, "& Top 10 Reactions with Highest PRR Values")
    } else {
      plottitle <- paste("Non-Cumulative Report Count Time Plot for:", current_drug, "&", current_rxn)
    }
    
    p <- df %>%
      ggplot(aes(x = quarter, y = count)) +
      geom_line(aes(colour=PT_NAME_ENG)) + geom_point()  + 
      ggtitle(plottitle) + 
      xlab("Quarter") + 
      ylab("Report Count") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=90, vjust=1))
    print(p)
    
  })

  # pass either datatable object or data to be turned into one to renderDataTable
  # DT::datatable(
  #   master_table, extensions = 'Buttons', options = list(
  #     dom = 'Bfrtip',
  #     buttons = c('csv')
  #   )
  # ),
  # PRR Tab: Reactions based on PRR associated with selected drug
  output$display_table <- renderDataTable({
    cv_prr_tab()[[2]] %>%
      select(drug_code, event_effect,
             PRR, LB95_PRR, UB95_PRR,
             IC,  LB95_IC,  UB95_IC,
             ROR, LB95_ROR, UB95_ROR)
  })
}

options(shiny.trace = FALSE, shiny.reactlog = FALSE)
shinyApp(ui, server)