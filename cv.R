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


########## Codes to fetch top 1000 specific results to be used in dropdown menu ############### 
# Temperary solution: fetch all tables to local and run functions on them
hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
cv_reports <- tbl(hcopen, "cv_reports")
cv_drug_product_ingredients <-  tbl(hcopen, "cv_drug_product_ingredients")
cv_report_drug <- tbl(hcopen, "cv_report_drug")
cv_reactions <- tbl(hcopen, "cv_reactions")
cv_report_drug_indication <- tbl(hcopen, "cv_report_drug_indication")
meddra <- tbl(hcopen, "meddra") %>%
  filter(Primary_SOC_flag == "Y") %>%
  select(PT_Term, HLT_Term, Version = MEDDRA_VERSION)

############### Create function ###################
# function to plot adverse reaction plot
# adrplot_test <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
#adrplot_df <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))

adrplot <- function(adrplot_test, plottitle) {
  adrplot_test <- adrplot_test %>% 
    dplyr::select(REPORT_ID,DATINTRECEIVED_CLEAN) %>%
    mutate(plot_date = floor_date(ymd(adrplot_test$DATINTRECEIVED_CLEAN), "month")) %>%
    dplyr::select(REPORT_ID,plot_date)
  
  nreports <- dplyr::summarise(group_by(adrplot_test,plot_date),count=n_distinct(REPORT_ID))
  total_reports <- sum(nreports$count)
  
  plottitle1 <- paste0(plottitle, " (", total_reports, " reports)") 
  
  
  plot <- nreports %>%
    ggplot(aes(x = plot_date, y = count)) +
    geom_line(stat = "identity", size = 0.1) +
    stat_smooth(method = "loess", size = 0.1) +
    ggtitle(plottitle1) + 
    xlab("Month") + 
    ylab("Number of Reports") +
    theme_bw() +
    theme(plot.title = element_text(lineheight=.8, face="bold"))
}

####################### datasets for menu setup ####################### 
#Fetch top 1000 most-reported brand/drug names
topbrands <- cv_report_drug %>%
  group_by(DRUGNAME) %>%
  dplyr::summarize(count = n_distinct(REPORT_ID)) %>%
  top_n(100, count) %>%
  dplyr::select(DRUGNAME) %>%
  as.data.frame()

#Fetch top 1000 most-reported reaction names
toprxns <- cv_reactions %>%
  group_by(PT_NAME_ENG) %>%
  dplyr::summarize(count = n_distinct(REPORT_ID)) %>%
  top_n(100, count) %>%
  dplyr::select(PT_NAME_ENG) %>%
  as.data.frame()

########################################################## UI for REPORT Tab shiny ############################################################## 
ui <- dashboardPage(
  dashboardHeader(title = titleWarning("CV Shiny (v0.05)"),
                  titleWidth = 700),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Reports", tabName = "reportdata", icon = icon("hospital-o")),
      menuItem("Drugs", tabName = "drugdata", icon = icon("flask")),
      menuItem("Patients", tabName = "patientdata", icon = icon("user-md")),
      menuItem("Reactions", tabName = "rxndata", icon = icon("heart-o")),
      menuItem("Download", tabName = "downloaddata", icon = icon("fa fa-download")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"))
    ),
    selectizeInput("search_brand", 
                "Brand Name (US Trade Name)",
                c("Please select an option below" = "", topbrands$DRUGNAME)),
    selectizeInput("search_rxn", 
                   "Adverse Event Term",
                   toprxns$PT_NAME_ENG,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    dateRangeInput("searchDateRange",
                   "Date Range",
                   start = "1965-01-01",
                   end = Sys.Date(),
                   startview = "year",
                   format = "yyyy-mm-dd"),
    selectizeInput("search_gender",
                   "Gender",
                   choices = c("All", "Male", "Female"),  #"Not specified", "Unknown"
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    actionButton("searchButton",
                 "Search",
                 width = '100%'),
    tags$br(),
    tags$h3(strong("Current Query:")),
    tableOutput("current_search")
    # downloadButton(outputId = "hlt_data_dl",
    #                label = "Export data")
  ), 
  
  dashboardBody(
    customCSS(),
    fluidRow(
      box(plotlyOutput(outputId = "timeplot"),
          tags$br(),
          tags$p("Reports by month from Canada Vigilance Adverse Reaction Online Database. 
                 Trendline is a local non-parametric regression calculated with the LOESS model. 
                 The shaded area is an approximation of the 95% confidence interval of the regression."),
          width = 12
          )
      ),
    tabItems(
      tabItem(tabName = "reportdata",
              fluidRow(
                box(title = tags$h3("Reporter"),
                    htmlOutput("reporterplot"), 
                    tags$br(),
                    tags$p("Qualification of the person who filed the report."),
                    tags$p("Unknown is the number of reports without the primarysource.qualification field."),
                    width = 4),
                box(title = tags$h3("Serious reports"),
                    htmlOutput("seriousplot"), 
                    tags$br(),
                    tags$p("Reports marked as serious."),
                    width = 4),
                box(title = tags$h3("Reasons for serious reports"),
                    htmlOutput("seriousreasonsplot"), 
                    tags$br(),
                    tags$p("Total sums to more than 100% because reports can be marked serious for multiple reasons."),
                    width = 4)
              )
      ),
      tabItem(tabName = "patientdata",
              fluidRow(
                box(title = tags$h3("Gender"),
                    htmlOutput("sexplot"),
                    tags$br(),
                    tags$p("Unknown includes reports explicitly marked unknown and Not Specified includes reports with no gender information."),
                    width = 3),
                box(title = tags$h3("Age Groups"),
                    htmlOutput("agegroupplot"),
                    tags$br(),
                    tags$p("Unknown includes reports with no age information."), 
                    width = 3),
                box(title = tags$h3("Age Histogram"),
                    plotlyOutput("agehist"),
                    width = 6)
              )
      ),
      tabItem(tabName = "drugdata",
              fluidRow(
                box(plotlyOutput("indicationptplot"),
                    tags$br(),
                    tags$p("This plot includes top 10 indications (PT) for drugs associated with the matching reports."),
                    width = 6),
                box(plotlyOutput("indicationhltplot"),
                    tags$br(),
                    tags$p("This plot includes top 10 indications (HLT) for drugs associated with the matching reports."),
                    width = 6),
                box(plotlyOutput("drugplot"),
                    tags$br(),
                    tags$p("This plot includes top 10 most-reported drugs with most-reported indication assocaiated with the seached drug."),
                    width = 6)
              )
      ),
      tabItem(tabName = "rxndata",
              fluidRow(
                box(title = tags$h3("Outcomes (all reactions)"),
                    htmlOutput("outcomeplot"),
                    width = 4),
                box(title = tags$h3("Top 10 Reactions (Preferred Terms) Associated with Searched Drug"),
                    htmlOutput("top_pt"),
                    tags$br(),
                    tags$p("For more rigorous analysis, use disproportionality statistics."),
                    width = 4),
                box(title = tags$h3("Top 10 Reactions (High-Level Terms) Associated with Searched Drug"),
                    htmlOutput("top_hlt"),
                    tags$br(),
                    tags$p("For more rigorous analysis, use disproportionality statistics."),
                    width = 4)
              )
      ),
      
      tabItem(tabName = "downloaddata",
              fluidRow(
                box(
                  tags$h3("Download Data Used for Current Searched Combination"),
                  tags$p("Please select a category: "),
                  selectInput("search_dataset_type",
                                 "Information Category",
                                 c("Report Info", "Drug Info", "Reaction Info")),
                  actionButton("search_report_type_dl","Prepare download"),
                  textOutput("download_reports_type"),
                  textOutput("download_reports_size"),
                  downloadButton('download_reports', 'Download')
                )
              )
      ),
      tabItem(tabName = "aboutinfo",
              tags$h2("About the Shiny App"),
              tags$p("This is a prototyping platform to utilize open data sources (e.g. Canada Vigilance Adverse Reaction Online Database) 
                      and provide visualizations in an interactive format. Further analysis can be conducted and added onto this platform to make 
                      better use of the data. Data provided by the Canada Vigilance Adverse Reaction Online Database: "),
              tags$a(href="http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php", "Click here!"),
              br(),
              aboutAuthors()
      )
    )
  ),
  skin = "blue"
)


############### Server Functions ###################
server <- function(input, output) {
  # Data frame generate reactive function to be used to assign: data <- cv_reports_tab()
  cv_search_tab <- reactive({
    input$searchButton
    isolate({
      #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
      current_brand <- ifelse(input$search_brand == "", NA, input$search_brand)
      current_rxn <- ifelse(input$search_rxn == "", NA, input$search_rxn)
      current_gender <- ifelse(input$search_gender == "", "All", input$search_gender)
      current_date_range <- input$searchDateRange
      
      search_tab_df <- data.frame(names = c("Brand Name:",
                                            "Adverse Reaction Term:",
                                            "Gender",
                                            "Date Range:"),
                                  terms = c(current_brand,
                                            current_rxn,
                                            current_gender,
                                            paste0(current_date_range[1], " to ", current_date_range[2])),
                                  stringsAsFactors=FALSE)
      
      search_tab_df$terms[is.na(search_tab_df$terms) == TRUE] <- "Not Specified (All)"
      return(list(search_tab_df = search_tab_df,
                  search_brand = current_brand,
                  search_rxn = current_rxn,
                  search_gender = current_gender,
                  searchDateRange = current_date_range))
    })})
  
  cv_master_tab_tbl <- reactive({
    #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
    current_brand <- cv_search_tab()$search_brand
    current_rxn <- cv_search_tab()$search_rxn
    current_gender <- cv_search_tab()$search_gender
    current_date_range <- cv_search_tab()$searchDateRange
    
    cv_reports_filtered <- cv_reports %>%
      filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])
    if (current_gender != "All") cv_reports_filtered %<>% filter(GENDER_ENG == current_gender)
    cv_report_drug_filtered <- cv_report_drug
    if (is.na(current_brand) == FALSE) cv_report_drug_filtered %<>% filter(DRUGNAME == current_brand)
    cv_reactions_filtered <- cv_reactions
    if (is.na(current_rxn) == FALSE) cv_reactions_filtered %<>% filter(PT_NAME_ENG == current_rxn)
    
    tab_master <-  cv_reports_filtered %>%
      semi_join(cv_report_drug_filtered, by = "REPORT_ID") %>%
      semi_join(cv_reactions_filtered, by = "REPORT_ID")
  })
  cv_master_tab <- reactive({cv_master_tab_tbl() %>% as.data.frame()})
  
  cv_drug_tab_indc <- reactive({
    cv_report_drug_indication_drg <- cv_report_drug_indication %>%
      dplyr::select(REPORT_ID, INDICATION_NAME_ENG)
    
    # Data frame used to obtain Top_25_indication bar chart: Indication is only associated with individual drug
    # When brand name is unspecified, chart shows top 25 indications associated with DRUGNAME="REMICADE" + date_range
    # When brand name is specified, chart shows top 25 indications associated with specified drug + date_range
    
    # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name 
    #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq) 
    meddra_filtered <- meddra %>% filter(Version == 'v.18.0')
    drugs_tab_indication <- cv_master_tab_tbl() %>%
      inner_join(cv_report_drug_indication_drg, by = "REPORT_ID") %>%
      inner_join(meddra_filtered, by = c("INDICATION_NAME_ENG" = "PT_Term")) %>%
      as.data.frame()
  })
  
  cv_drug_tab_topdrg <- reactive({
    cv_master_tab_tbl()
    isolate({
      #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
      current_brand <- cv_search_tab()$search_brand
      current_rxn <- cv_search_tab()$search_rxn
      current_gender <- cv_search_tab()$search_gender
      current_date_range <- cv_search_tab()$searchDateRange
      
      cv_report_drug_indication_drg <- cv_report_drug_indication %>%
        dplyr::select(REPORT_ID, INDICATION_NAME_ENG)
      
      # Data frame used to obtain Top_25_indication bar chart: Indication is only associated with individual drug
      # When brand name is unspecified, chart shows top 25 indications associated with DRUGNAME="REMICADE" + date_range
      # When brand name is specified, chart shows top 25 indications associated with specified drug + date_range
      
      # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name 
      #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq) 
      top_indications <- cv_master_tab_tbl() %>%
        inner_join(cv_report_drug_indication_drg, by = "REPORT_ID") %>%
        group_by(INDICATION_NAME_ENG) %>%
        dplyr::summarise(count = n_distinct(REPORT_ID)) %>%
        top_n(1, count) %>%
        as.data.frame()
      top_indications_final <- top_indications$INDICATION_NAME_ENG
      
      # indication import
      cv_report_drug_indication_drg <- cv_report_drug_indication %>%
        filter(INDICATION_NAME_ENG == top_indications_final) %>%
        dplyr::select(REPORT_ID, INDICATION_NAME_ENG, DRUGNAME)
      
      cv_reports_filtered <- cv_reports %>%
        filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])
      if (current_gender != "All") cv_reports_filtered %<>% filter(GENDER_ENG == current_gender)
      cv_report_drug_drg <- cv_report_drug %>% dplyr::select(REPORT_ID, DRUGNAME)
      if(is.na(current_brand) == FALSE) cv_report_drug_drg %<>% filter(DRUGNAME != current_brand)
      
      drugs_tab_topdrg <- cv_reports_filtered %>% 
        inner_join(cv_report_drug_drg)%>%
        semi_join(cv_report_drug_indication_drg) %>%
        dplyr::select(REPORT_ID, DRUGNAME, GENDER_ENG) %>%
        as.data.frame()
    })})
  cv_reactions_tbl <- reactive({
    current_brand <- cv_search_tab()$search_brand
    
    drugs_rxn_result <- cv_reactions %>%
      dplyr::select(REPORT_ID, PT_NAME_ENG)
    if (!is.na(current_brand)) {
      cv_reports_filtered <- cv_report_drug %>%
        filter(DRUGNAME == current_brand)
      drugs_rxn_result %<>%
        semi_join(cv_reports_filtered, by = "REPORT_ID")
    }
    drugs_rxn_result %<>%
      group_by(PT_NAME_ENG) %>%
      dplyr::summarise(count = n_distinct(REPORT_ID)) %>%
      top_n(10, count) %>%
      as.data.frame()
  })
  cv_reactions_hlt_tbl <- reactive({
    current_brand <- cv_search_tab()$search_brand
    
    drugs_rxn_result <- cv_reactions %>%
      dplyr::select(REPORT_ID, PT_NAME_ENG, MEDDRA_VERSION) %>%
      inner_join(meddra, by = c("PT_NAME_ENG" = "PT_Term", "MEDDRA_VERSION" = "Version"))
    if (!is.na(current_brand)) {
      cv_reports_filtered <- cv_report_drug %>%
        filter(DRUGNAME == current_brand)
      drugs_rxn_result %<>%
        semi_join(cv_reports_filtered, by = "REPORT_ID")
    }
    drugs_rxn_result %<>%
      group_by(HLT_Term) %>%
      dplyr::summarise(count = n_distinct(REPORT_ID)) %>%
      top_n(10, count) %>%
      as.data.frame()
  })

  cv_download_reports <- eventReactive(input$search_report_type_dl, {
    selected_ids <- cv_master_tab_tbl() %>% select(REPORT_ID)
    
    if(input$search_dataset_type == "Report Info"){
      reports_tab_master <- cv_reports %>%
        semi_join(selected_ids)
      
    } else if(input$search_dataset_type == "Drug Info"){
      reports_tab_master <- cv_report_drug %>%
        semi_join(selected_ids)
      
    } else if(input$search_dataset_type == "Reaction Info"){
      reports_tab_master <- cv_reactions %>%
        semi_join(selected_ids)
    }
    reports_tab_master %<>% as.data.frame()
    
    download_type <- paste("Report Type to be downloaded is", input$search_dataset_type)
    # reports_tab_master_size <- paste("Size of Dataset is",
    #                                  format(object.size(reports_tab_master),
    #                                         units = "auto"))
    reports_tab_master_size <- ""
    
    return(list(reports_tab_master = reports_tab_master,
                download_type = download_type,
                download_format = input$search_dataset_type,
                reports_tab_master_size = reports_tab_master_size)) 
  })
  

#########################################################################################################################################
  
  ############# Download Tab #################
  output$download_reports <- downloadHandler(
    filename = function() {paste0(cv_download_reports()$download_format, '.csv')},
    content = function(file){
      write.csv(cv_download_reports()$reports_tab_master,
                file,
                fileEncoding = "UTF-8",
                row.names = FALSE)
    }
  )
  output$download_reports_type <- renderText(cv_download_reports()$download_type)
  output$download_reports_size <- renderText(cv_download_reports()$reports_tab_master_size)
  
  ############## Construct Current_Query_Table for generic name, brand name, adverse reaction term & date range searched ############
  output$current_search <- renderTable({
     data <- cv_search_tab()$search_tab_df
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  ############### Create time plot #####################
  output$timeplot <- renderPlotly({
    
    data <- cv_master_tab() %>%
      dplyr::select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
                    OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN,GENDER_ENG)
    drug_selected <- cv_search_tab()$search_tab_df$terms[1]
    
    # specify the title of time plot based on reactive choice
    title <- ifelse(drug_selected == "Not Specified (All)", "All Drugs", drug_selected)
    plottitle <- paste("Drug Adverse Event Reports for", title)
    p <- adrplot(adrplot_test = data, plottitle = plottitle)
    ggplotly(p)
  })
  
  ############### Create Reporter pie chart ##############  
  output$reporterplot <- renderGvis({
    data <- cv_master_tab() %>%
      dplyr::select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
                    OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN,GENDER_ENG)
    reporter_df <- data %>% dplyr::select(REPORT_ID, REPORTER_TYPE_ENG)
    # test
    # data <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
    reporter_results<-dplyr::summarise(group_by(reporter_df, REPORTER_TYPE_ENG),count=n_distinct(REPORT_ID))
    reporter_results$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == ""] <- "Not Specified"
    
    gvisPieChart(reporter_results, 
                 labelvar = "REPORTER_TYPE_ENG",
                 numvar = "n", 
                 options = list(pieHole = 0.4))
  })
  
  ################ Create Serious reports pie chart ##################   
  output$seriousplot <- renderGvis({
    data <- cv_master_tab() %>%
      dplyr::select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
                    OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN,GENDER_ENG)
    serious_results <- data %>%
      dplyr::select(REPORT_ID,SERIOUSNESS_ENG) %>%
      group_by(SERIOUSNESS_ENG) %>%
      dplyr::summarise(count = n_distinct(REPORT_ID))
    serious_results$SERIOUSNESS_ENG[serious_results$SERIOUSNESS_ENG == ""] <- "Not Specified"
    
    gvisPieChart(serious_results, 
                 labelvar = "SERIOUSNESS_ENG",
                 numvar = "n", 
                 options = list(pieHole = 0.4))
  })
  
  ################ Create Serious Reason Reports chart ################## 
  output$seriousreasonsplot <- renderGvis({
    data <- cv_master_tab() %>%
      dplyr::select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
                    OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN,GENDER_ENG)
    total_serious <- dplyr::summarise(group_by(data, SERIOUSNESS_ENG),count=n_distinct(REPORT_ID))
    total_serious_final <- total_serious$count[total_serious$SERIOUSNESS_ENG == "Yes"]
    
    
    serious_reason_df <- data %>% mutate(SERIOUSNESS_ENG = ifelse(REPORT_ID == 645744, "Yes", SERIOUSNESS_ENG))
    # REPORT_ID = 645744 has serious_reason specifed but seriousness_eng is balnk
    
    # Congenital
    congenital_results <- dplyr::summarise(group_by(serious_reason_df, SERIOUSNESS_ENG,CONGENITAL_ANOMALY),count=n_distinct(REPORT_ID))
    
    if(any(congenital_results$CONGENITAL_ANOMALY == 1, na.rm=TRUE) ==TRUE){
      congenital_results_final <- filter(congenital_results,SERIOUSNESS_ENG == "Yes", CONGENITAL_ANOMALY == 1)%>%
        mutate(Reasons = "CONGENITAL ANOMALY")%>%
        ungroup() %>%
        dplyr::select(Reasons,count)
    } else {
      Reasons <- I("CONGENITAL ANOMALY")
      count <- c(0)
      congenital_results_final <- data.frame(Reasons,count)
    }
    
    # Death
    death_results <- dplyr::summarise(group_by(serious_reason_df, SERIOUSNESS_ENG,DEATH),count=n_distinct(REPORT_ID))
    
    if(any(death_results$DEATH == 1, na.rm=TRUE) ==TRUE){
      death_results_final <- filter(death_results,SERIOUSNESS_ENG == "Yes", DEATH == 1)%>%
        mutate(Reasons = "DEATH")%>%
        ungroup() %>%
        dplyr::select(Reasons,count)
    } else {
      Reasons <- I("DEATH")
      count <- c(0)
      death_results_final <- data.frame(Reasons,count)
    }
    
    # Disability
    disabling_results <- dplyr::summarise(group_by(serious_reason_df, SERIOUSNESS_ENG,DISABILITY),count=n_distinct(REPORT_ID))
    
    if(any(disabling_results$DISABILITY == 1, na.rm=TRUE) ==TRUE){
      disabling_results_final <- filter(disabling_results,SERIOUSNESS_ENG == "Yes", DISABILITY == 1)%>%
        mutate(Reasons = "DISABILITY")%>%
        ungroup() %>%
        dplyr::select(Reasons,count)
    } else {
      Reasons <- I("DISABILITY")
      count <- c(0)
      disabling_results_final <- data.frame(Reasons,count)
    }
    
    # Hospitalization
    hospital_results <- dplyr::summarise(group_by(serious_reason_df, SERIOUSNESS_ENG,HOSP_REQUIRED),count=n_distinct(REPORT_ID))
    
    if(any(hospital_results$HOSP_REQUIRED == 1, na.rm=TRUE) ==TRUE ) {
      hospital_results_final <- filter(hospital_results,SERIOUSNESS_ENG == "Yes",HOSP_REQUIRED == 1) %>%
        mutate(Reasons = "HOSPITALIZATION") %>%
        ungroup() %>%
        dplyr::select(Reasons,count)
    } else {
      Reasons <- I("HOSPITALIZATION")
      count <- c(0)
      hospital_results_final <- data.frame(Reasons,count)
    }
    
    # Lifethreatening
    lifethreaten_results <- dplyr::summarise(group_by(serious_reason_df, SERIOUSNESS_ENG,LIFE_THREATENING),count=n_distinct(REPORT_ID))
    
    if(any(lifethreaten_results$LIFE_THREATENING == 1, na.rm=TRUE) ==TRUE){
      lifethreaten_results_final <- filter(lifethreaten_results,SERIOUSNESS_ENG == "Yes", LIFE_THREATENING == 1)%>%
        mutate(Reasons = "LIFE_THREATENING")%>%
        ungroup() %>%
        dplyr::select(Reasons,count)
    } else {
      Reasons <- I("LIFE_THREATENING")
      count <- c(0)
      lifethreaten_results_final <- data.frame(Reasons,count)
    }
    
    # Other
    #serother_results <- ddply(serious_reason_df, c("SERIOUSNESS_ENG", "OTHER_MEDICALLY_IMP_COND"),count_func)
    serother_results <- dplyr::summarise(group_by(serious_reason_df, SERIOUSNESS_ENG,OTHER_MEDICALLY_IMP_COND),count=n_distinct(REPORT_ID))
    
    if(any(serother_results$OTHER_MEDICALLY_IMP_COND == 1, na.rm=TRUE) ==TRUE){
      serother_results_final <-filter(serother_results,SERIOUSNESS_ENG == "Yes", OTHER_MEDICALLY_IMP_COND == 1)%>%
        mutate(Reasons = "OTHER_MEDICALLY_IMP_COND")%>%
        ungroup() %>%
        dplyr::select(Reasons,count)
    } else {
      Reasons <- I("OTHER_MEDICALLY_IMP_COND")
      count <- c(0)
      serother_results_final <- data.frame(Reasons,count)
    }
    
    ## Check for NotSpecified ##
    serious_reason <-data %>%
      filter(SERIOUSNESS_ENG == "Yes" & is.na(DEATH) == TRUE & is.na(DISABILITY)==TRUE & is.na(CONGENITAL_ANOMALY)==TRUE & is.na(LIFE_THREATENING)==TRUE & 
               is.na(HOSP_REQUIRED)==TRUE & is.na(OTHER_MEDICALLY_IMP_COND) == TRUE)
    
    if(nrow(serious_reason)!=0){
      serious_reason <- serious_reason %>% 
        mutate(NotSpecified = "Yes")%>%
        dplyr::select(REPORT_ID, NotSpecified) 
      serious_reason_df <- left_join(data,serious_reason)%>% mutate(SERIOUSNESS_ENG = ifelse(REPORT_ID == 645744, "Yes", SERIOUSNESS_ENG))
      
      # NotSpecified
      #NotSpecified_results <- ddply(serious_reason_df,c("SERIOUSNESS_ENG","NotSpecified"),count_func)
      NotSpecified_results <- dplyr::summarise(group_by(serious_reason_df, SERIOUSNESS_ENG,NotSpecified),count=n_distinct(REPORT_ID))
      
      if(any(NotSpecified_results$NotSpecified == "Yes", na.rm=TRUE) ==TRUE){
        NotSpecified_results_final <- filter(NotSpecified_results,SERIOUSNESS_ENG == "Yes",NotSpecified == "Yes")%>%
          mutate(Reasons = "NotSpecified")%>%
          ungroup() %>%
          dplyr::select(Reasons,count) 
      } else {
        Reasons <- I("NotSpecified")
        count <- c(0)
        NotSpecified_results_final <- data.frame(Reasons,count)
      }
      # Combine all SeriousReasons Frequency tables with NotSpecified
      serious_reasons_restults <- congenital_results_final %>%
        full_join(death_results_final) %>%
        full_join(disabling_results_final) %>%
        full_join(hospital_results_final) %>%
        full_join(lifethreaten_results_final) %>%
        full_join(serother_results_final)%>%
        full_join(NotSpecified_results_final)
      
    } else {
      # Combine all SeriousReasons Frequency tables WITHOUT NotSpecified
      serious_reasons_restults <- congenital_results_final %>%
        full_join(death_results_final) %>%
        full_join(disabling_results_final) %>%
        full_join(hospital_results_final) %>%
        full_join(lifethreaten_results_final) %>%
        full_join(serother_results_final) 
    }
    
    # Calculate the percentage of each reason
    serious_reasons_restults <- mutate(serious_reasons_restults, percentage = count/total_serious_final*100 %>% signif(digits = 3)) %>% 
      dplyr::select(-count) %>%
      arrange(desc(percentage))
    
    # GoogleVis plot html: use plot() to graph it
    gvisBarChart(serious_reasons_restults, 
                 xvar = "Reasons",
                 yvar = "percentage", 
                 options = list(title = paste0("Reasons for serious reports (", total_serious_final, " serious reports)"),
                                legend = "{position:'none'}",
                                bars = 'horizontal',
                                #hAxis = "{format:'percent'}",
                                axes= "x: {
                                0: { side: 'top', label: 'Count'} 
  }",
                                bar = list(groupWidth =  '90%')
                 )
                 )
  })
  
  ################ Create Gender pie chart in Patient tab ##################  
  output$sexplot <- renderGvis({
    data <- cv_master_tab() %>%
      dplyr::select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_Y, AGE_GROUP_CLEAN)
    
    # replace blank in GENDER_ENG with character "Unknown"
    data$GENDER_ENG[data$GENDER_ENG == ""] <- "Unknown"
    
    sex_results <- dplyr::summarise(group_by(data, GENDER_ENG),count=n_distinct(REPORT_ID))
    
    gvisPieChart(sex_results, 
                 labelvar = "GENDER_ENG",
                 numvar = "count", 
                 options = list(pieHole = 0.4, pieSliceText="percentage", fontSize=12))
  })
  
  ################ Create Age Group pie chart in Patient tab ##################      
  output$agegroupplot <- renderGvis({
    data <- cv_master_tab() %>%
      dplyr::select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_Y, AGE_GROUP_CLEAN)
    
    # data1 <- cv_search_tab()
    # gender_selected <- data1$terms[3]
    
    patients_tab_output <- dplyr::summarise(group_by(data,AGE_GROUP_CLEAN),count=n_distinct(REPORT_ID))
    # 
    # if(gender_selected == "All"){
    #   patients_tab_output <- dplyr::summarise(group_by(data,AGE_GROUP_CLEAN),count=n_distinct(REPORT_ID))
    # } else {
    #   #patients_tab_df1 <- filter(data,GENDER_ENG == current_gender)
    #   patients_tab_output <- filter(data,GENDER_ENG == current_gender) %>% dplyr::summarise(group_by(AGE_GROUP_CLEAN),count=n_distinct(REPORT_ID))
    # }
    
    gvisPieChart(patients_tab_output, 
                 labelvar = "AGE_GROUP_CLEAN",
                 numvar = "count", 
                 options = list(pieHole = 0.4, pieSliceText='percentage', fontSize=12) )
  })
  
  ################ Create Age Group histogram in Patient tab ##################   
  output$agehist <- renderPlotly({
    data <- cv_master_tab() %>%
      dplyr::select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_Y, AGE_GROUP_CLEAN)
    
    # Age groups frequency table
    age_groups <- dplyr::summarise(group_by(data, AGE_GROUP_CLEAN),count=n_distinct(REPORT_ID))
    unknown <- age_groups$count[age_groups$AGE_GROUP_CLEAN == "Unknown"]
    
    plottitle <- paste0("Histogram of Patient Ages")
    if(unknown > 0) plottitle <- paste0(plottitle, "<br>(", unknown, "Reports with Unknown Age Group Excluded)")
    
    age_groups_hist <- data %>% filter(AGE_GROUP_CLEAN != "Unknown") #exclude the unknown
    
    hist <- ggplot(age_groups_hist, aes(x = AGE_Y, fill=AGE_GROUP_CLEAN)) +
      geom_histogram()+
      ggtitle(plottitle) + 
      xlab("Age at onset (years)") + 
      ylab("Number of Reports") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, size = rel(0.85),face="bold")) + 
      theme(axis.title.x = element_text(size = rel(0.8)))+
      scale_x_continuous(limits=c(0,130))
    ggplotly(hist)
    
  })
  
  ################ Create drug plots in Drug tab ################## 
  output$drugplot <- renderPlotly({
    
    data <- cv_drug_tab_topdrg()
    # replace blank in GENDER_ENG with character "Unknown"
    data$GENDER_ENG[data$GENDER_ENG == ""] <- "Unknown"
    
    drugs <-  dplyr::summarise(group_by(data, DRUGNAME),count=n_distinct(REPORT_ID))
    drugs_sorted<- drugs %>% dplyr::arrange(desc(count)) %>% top_n(n=10) 
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    
    library(scales)
    p <- ggplot(drugs_sorted, aes(x = DRUGNAME, y = count, fill = DRUGNAME)) + 
      geom_bar(stat = "identity") + 
      scale_x_discrete(limits = rev(drugs_sorted$DRUGNAME[1:10])) + 
      coord_flip() +
      ggtitle("Top 10 Drugs (in addition to search term)") +
      xlab("Drug (brand name)") + 
      ylab("Number of Reports") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), 
            legend.position = "none") +
      scale_y_continuous(limits= c(0,10000))
    ggplotly(p)
  })
  
  output$indicationptplot <- renderPlotly({
    data <- cv_drug_tab_indc()
    # replace blank in GENDER_ENG with character "Unknown"
    data$GENDER_ENG[data$GENDER_ENG == ""] <- "Unknown"


    indications <-  dplyr::summarise(group_by(data, INDICATION_NAME_ENG),count=n_distinct(REPORT_ID))
    indications_sorted<- indications %>% dplyr::arrange(desc(count)) %>% top_n(n=10)

    library(scales)
    p <- ggplot(indications_sorted, aes(x = INDICATION_NAME_ENG, y = count, fill = INDICATION_NAME_ENG)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits = rev(indications_sorted$INDICATION_NAME_ENG)) +
      coord_flip() +
      ggtitle("Top 10 Indications (PT)") +
      xlab("Indication") +
      ylab("Number") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"),
            legend.position = "none") +
      scale_y_continuous(limits=  c(0, max(indications_sorted$count)))
    ggplotly(p)
  })
  output$indicationhltplot <- renderPlotly({
    data <- cv_drug_tab_indc()
    data$GENDER_ENG[data$GENDER_ENG == ""] <- "Unknown"


    indications <-  dplyr::summarise(group_by(data, HLT_Term),count=n_distinct(REPORT_ID))
    indications_sorted<- indications %>% dplyr::arrange(desc(count)) %>% top_n(n=10)

    library(scales)
    p <- ggplot(indications_sorted, aes(x = HLT_Term, y = count, fill = HLT_Term)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits = rev(indications_sorted$HLT_Term)) +
      coord_flip() +
      ggtitle("Top 10 Indications (HLT)") +
      xlab("Indication") +
      ylab("Number") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"),
            legend.position = "none") +
      scale_y_continuous(limits=  c(0, max(indications_sorted$count)))
    ggplotly(p)
  })
  
  
  ################ Create Outcomes(all reactions) pie chart in Reaction tab ################## 
  output$outcomeplot <- renderGvis({
    data <- cv_master_tab() %>%
      dplyr::select(REPORT_ID, OUTCOME_ENG)
    
    outcome_results <-  dplyr::summarise(group_by(data, OUTCOME_ENG),count=n_distinct(REPORT_ID))
    
    gvisPieChart(outcome_results, 
                 labelvar = "OUTCOME_ENG",
                 numvar = "n", 
                 options = list(pieHole = 0.4))
  })
  
  output$top_pt <- renderGvis({
    data <- cv_reactions_tbl()
    gvisBarChart(data,
                 xvar = "PT_NAME_ENG",
                 yvar = "count",
                 options = list(
                   #vAxes="[{title:'Reactions'}",
                   legend = "{position:'none'}",
                   bars = 'horizontal',
                   axes= "x: {
                                 0: { side: 'top', label: 'Number of Reports'}}",
                   bar = list(groupWidth =  '90%'),
                   height=500)
    )})
  output$top_hlt <- renderGvis({
    data <- cv_reactions_hlt_tbl()
    gvisBarChart(data,
                 xvar = "HLT_Term",
                 yvar = "count",
                 options = list(
                   #vAxes="[{title:'Reactions'}",
                   legend = "{position:'none'}",
                   bars = 'horizontal',
                   axes= "x: {
                                 0: { side: 'top', label: 'Number of Reports'}}",
                   bar = list(groupWidth =  '90%'),
                   height=500)
    )})
}

shinyApp(ui, server)
