library(shinydashboard)
library(jsonlite)
# library(lubridate)
# library(plyr)
# library(data.table)
# library(ggplot2)
library(magrittr)
# library(plotly)
library(shiny)
# library(DT)
# library(googleVis)
# library(stringr)
# library(utils)
library(dplyr)

hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
cv_prr <- tbl(hcopen, "PRR_160826") %>% select(-row.names) #%>% as.data.frame()
cv_drug_rxn <- tbl(hcopen, "cv_drug_rxn")
cv_drug_rxn_2006 <- cv_drug_rxn %>% filter(quarter >= 2006.1)
count_df_quarter <- dplyr::summarise(group_by(cv_drug_rxn_2006,ing,PT_NAME_ENG,quarter), count = n_distinct(REPORT_ID)) #%>% as.data.frame()
ingred_list <- cv_prr %>% select(drug_code) %>% as.data.frame() %>% `[[`(1) %>% unique() %>% sort()
ingred_list <- c("", ingred_list)


############ UI for DISP shiny ################
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
    selectizeInput("search_ingred", 
                   "Generic Name/Ingredient",
                   ingred_list,
                   options = list(placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    
    # selectizeInput("search_rxn", 
    #                "Adverse Event Term",
    #                choices = NULL,
    #                options = list(create = TRUE,
    #                               placeholder = 'Please select an option below',
    #                               onInitialize = I('function() { this.setValue(""); }'))),
    actionButton("searchButton", "Search")
  ), 
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                box(title = 'hey world',
                    DT::dataTableOutput("master_table"),
                    # DT::datatable(
                    #   master_table, extensions = 'Buttons', options = list(
                    #     dom = 'Bfrtip',
                    #     buttons = c('csv')
                    #   )
                    # ),
                    tags$p("NOTE: The above table is ranked decreasingly by PRR value. All drug*reactions pairs that have PRR value of infinity are added at the end of the table."),
                    textOutput("aasdf"),
                    width = 12),
                width=12
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

options(shiny.trace = FALSE, shiny.reactlog = FALSE)

############################# Server of DISP Shiny #####################
server <- function(input, output, session) {

  # PRR tab 
  # cv_prr_tab <- reactive(input$searchButton)

  
  # PRR Tab: Reactions based on PRR associated with selected drug
  # output$master_table <- DT::renderDataTable({
  #   df <- as.data.frame(cv_prr_tab()[2]) 
  #   df <- df %>% dplyr::select(-c(log_PRR,LB95_log_PRR,UB95_log_PRR,log_ROR,UB95_log_ROR,LB95_log_ROR))
  #   data <- df[c(1,2,3,5,4,8,6,7,9,11,10)]
  #   data
  # }, server = TRUE)
  
  cv_prr_tab <- eventReactive(input$search_ingred, {
    if ("" == input$search_ingred) {
      cv_prr %>% as.data.frame()
    } else {
      cv_prr %>% dplyr::filter(drug_code == input$search_ingred) %>% as.data.frame()
    }
  })

  output$master_table <- DT::renderDataTable(cv_prr_tab())
  
}
shinyApp(ui, server)