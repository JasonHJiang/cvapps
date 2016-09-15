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

cv_prr <- tbl(hcopen, "PRR_160826") %>%
  dplyr::select(-row.names)
cv_bcpnn <- tbl(hcopen, "IC_160829")%>%
  dplyr::select(-c(row.names,count,`expected count`,`n11/E`,`drug margin`, `event margin`,FDR,FNR,Se,Sp,postH0)) %>%
  dplyr::rename(drug_code = `drug code`, event_effect = `event effect`)
cv_ror <- tbl(hcopen, "ROR_160826") %>%
  dplyr::select(-row.names)
cv_drug_rxn <- tbl(hcopen, "cv_drug_rxn")

cv_drug_rxn_2006 <- cv_drug_rxn %>% dplyr::filter(quarter >= 2006.1)
count_df_quarter <- group_by(cv_drug_rxn_2006,ing,PT_NAME_ENG,quarter) %>%
  dplyr::summarize(count = n_distinct(REPORT_ID)) %>% as.data.frame()
# load("/home/shared/DISP data/DISP_shiny_new/count_quarter_df.RData")

master_table <- cv_prr %>% left_join(cv_bcpnn, copy = TRUE) %>% left_join(cv_ror,  copy = TRUE) %>% filter(is.na(ROR) != TRUE) %>%
  dplyr::rename(LB95_IC = `Q_0.025(log(IC))`, UB95_IC = `Q_0.975(log(IC))`, LB95_PRR= LB95_CI_PRR) %>% as.data.frame()
  # dplyr::mutate(PRR = round(PRR,3),
  #               UB95_PRR = round(UB95_PRR,3),
  #               LB95_PRR = round(LB95_PRR,3),
  #               log_PRR = round(log_PRR,3),
  #               LB95_log_PRR = round(LB95_log_PRR,3),
  #               UB95_log_PRR = round(UB95_log_PRR,3),
  #               LB95_IC = round(LB95_IC,3),
  #               UB95_IC = round(UB95_IC,3),
  #               IC = round(IC,3),
  #               ROR = round(ROR,3),
  #               UB95_ROR = round(UB95_ROR,3),
  #               LB95_ROR = round(LB95_ROR,3),
  #               log_ROR = round(log_ROR,3),
  #               UB95_log_ROR = round(UB95_log_ROR,3),
  #               LB95_log_ROR = round(LB95_log_ROR,3))
# load("/home/shared/DISP data/DISP_shiny_new/master_table.RData")

# drug and adverse event dropdown menu choices
topdrugs <- cv_prr %>% dplyr::distinct(drug_code) %>% as.data.frame()
choices <- c("", sort(topdrugs$drug_code))

#### UI component ####
ui <- dashboardPage(
  dashboardHeader(title = "CV Shiny WARNING: This is a beta product. Please do NOT use as sole evidence to support regulartory decisions.",  titleWidth = 1200),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Disproportionality Analysis", tabName = "data", icon = icon("fa fa-cogs")),
      menuItem("Documentation", tabName = "Documentation", icon = icon("fa fa-binoculars")),
      # menuItem("ROR", tabName = "rordata", icon = icon("fa fa-database")),
      # menuItem("Download", tabName = "downloaddata", icon = icon("fa fa-download")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"))
    ),
    
    selectizeInput(inputId ="search_generic",
                   label = "Generic Name/Ingredient",
                   choices = choices,
                   options = list(placeholder = 'Start typing to search...')),
    
    selectizeInput(inputId = "search_rxn",
                   label = "Adverse Event Term",
                   choices = NULL,
                   options = list(placeholder = 'Start typing to search...',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    
    actionButton("searchButton", "Search", width = '100%')
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
                    dataTableOutput("master_table"),
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
                  tags$p("Sophia He, BSc in Progress"),
                  tags$p("Jr. Data Scientist Co-op, Health Products and Food Branch"),
                  tags$p("Health Canada / Government of Canada"),
                  tags$p("sophia.he@canada.ca & yunqingh@sfu.ca")
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
    if ("" == input$search_generic) {
      rxn_choices <- cv_prr %>%
        dplyr::distinct(event_effect) %>%
        as.data.frame() %>% `[[`(1) %>%
        sort()
    } else {
      rxn_choices <- cv_prr %>%
        dplyr::filter(drug_code == input$search_generic) %>%
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
    input$searchButton
    isolate({
    # Inf in PRR and ROR means there are no other drug associated with that specific adverse reaction, so denomimator is zero!
    # master_table[master_table$event_effect == "Refraction disorder",]
    
    if(input$search_generic == "" & input$search_rxn == ""){
      default_pairs <- master_table %>% filter(PRR != Inf) %>% arrange(desc(PRR)) %>% top_n(10, PRR) %>% select(drug_code, event_effect)
      default_count_df <- count_df_quarter[count_df_quarter$ing %in% default_pairs$drug_code & count_df_quarter$PT_NAME_ENG %in% default_pairs$event_effect,]
      
      timeplot_df <- default_count_df
      # rank master table by PRR & suppress Inf to the end
      prr_tab_df <- master_table %>% filter(is.infinite(PRR) == FALSE) %>% arrange(desc(PRR)) %>% bind_rows(master_table[is.infinite(master_table$PRR) == TRUE,])
    
      } else if(input$search_generic != "" & input$search_rxn == "") {
      timeplot_df <-  count_df_quarter %>% filter(ing == input$search_generic)
      
      # rank master table by PRR & suppress Inf to the end
      prr_tab_df1 <- master_table %>% filter(drug_code == input$search_generic) %>% filter(is.infinite(PRR) == FALSE) %>% arrange(desc(PRR))
      prr_tab_df <- prr_tab_df1 %>% bind_rows(prr_tab_df1[is.infinite(prr_tab_df1$PRR) == TRUE,])
      
      timeplot_top10_rxn <- prr_tab_df[1:10,] %>% select(drug_code, event_effect)
      timeplot_df <- count_df_quarter[count_df_quarter$ing %in% timeplot_top10_rxn$drug_code & count_df_quarter$PT_NAME_ENG %in% timeplot_top10_rxn$event_effect,]
      } else {
      timeplot_df <- count_df_quarter %>% filter(ing == input$search_generic, PT_NAME_ENG == input$search_rxn)
      prr_tab_df <- master_table %>% filter(drug_code == input$search_generic, event_effect == input$search_rxn)
    }

    list(timeplot_df, prr_tab_df)
  })
  })
  #current_drug = "OXYCODONE HYDROCHLORIDE"
  
  # PRR Time Plot
  output$top10_prr_timeplot <- renderPlot({
    df <- cv_prr_tab()[[1]]
    current_drug <- isolate(ifelse(input$search_generic == "",
                                   "All Drugs",
                                   input$search_generic))
    current_rxn <- isolate(ifelse(input$search_rxn == "",
                                  "All Reactions",
                                  input$search_rxn))
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
  output$master_table <- renderDataTable({
    cv_prr_tab()[[2]] %>%
      select(-c(log_PRR,LB95_log_PRR,UB95_log_PRR,log_ROR,UB95_log_ROR,LB95_log_ROR)) %>%
      select(c(1,2,3,5,4,8,6,7,9,11,10))
  })
  
}

options(shiny.trace = FALSE, shiny.reactlog = FALSE)
shinyApp(ui, server)