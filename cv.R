library(shinydashboard)
library(jsonlite)
library(lubridate)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)
library(plotly)
library(shiny)
library(DT)
library(googleVis)
# library(openfda)
library(stringr)
library(utils)
library(PhViD)
source("common_ui.R")


########## Codes to fetch top 1000 specific results to be used in dropdown menu ############### 
# Temperary solution: fetch all tables to local and run functions on them
hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
cv_reports <- tbl(hcopen, "cv_reports") 
cv_drug_product_ingredients <-  tbl(hcopen, "cv_drug_product_ingredients")
cv_report_drug <- tbl(hcopen,"cv_report_drug")
cv_reactions <- tbl(hcopen,"cv_reactions") 
cv_report_drug_indication <- tbl(hcopen,"cv_report_drug_indication")



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
  
  
  plottitle1 <- paste(plottitle, " (", total_reports, " reports)") 
  
  
  plot <- nreports %>%
    ggplot(aes(x = plot_date, y = count)) +
    geom_line(stat = "identity", size = 0.1) +
    stat_smooth(method = "loess", size = 0.1) +
    ggtitle(plottitle1) + 
    xlab("Month") + 
    ylab("Number of Reports") +
    theme_bw() +
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  
   #ggplotly(p= plot)
}

################################## Function for Report tab ################################## 
reports_tab <- function(current_brand,current_rxn,current_gender,current_date_range) { 
  cv_reports_sorted_rp <- if(current_gender != "All"){
    cv_reports %>%
      dplyr::select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
                    OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN, GENDER_ENG) %>%
      filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2], GENDER_ENG == current_gender)
  } else {
    cv_reports %>%
      dplyr::select(REPORT_ID, SERIOUSNESS_ENG, REPORTER_TYPE_ENG, DEATH, DISABILITY, CONGENITAL_ANOMALY,LIFE_THREATENING, HOSP_REQUIRED, 
                    OTHER_MEDICALLY_IMP_COND, DATINTRECEIVED_CLEAN,GENDER_ENG) %>%
      filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])
  }
  
  cv_report_drug_rp <- if(is.na(current_brand) == FALSE){
    cv_report_drug %>%
      dplyr::select(REPORT_ID, DRUGNAME) %>%
      filter(DRUGNAME == current_brand)
  } else {
    cv_report_drug %>%
      dplyr::select(REPORT_ID, DRUGNAME)
  }
  cv_reactions_rp <- if(is.na(current_rxn) == FALSE){
    cv_reactions %>%
      dplyr::select(REPORT_ID, PT_NAME_ENG) %>%
      filter(PT_NAME_ENG == current_rxn)
  } else {
    cv_reactions %>%
      dplyr::select(REPORT_ID, PT_NAME_ENG)
  }
  
  reports_tab_master <-  cv_reports_sorted_rp%>%
    semi_join(cv_report_drug_rp) %>%
    semi_join(cv_reactions_rp) %>%
    as.data.frame()
  return(reports_tab_master) 
}

################################## Function for Patient tab ################################## 
patients_tab <- function( current_brand, current_rxn,current_gender,current_date_range) { 
  # Import tables with particular search items with method to deal with unspecified search term
  cv_reports_sorted_pt <- if(current_gender != "All"){
    cv_reports %>%
      dplyr::select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_Y,AGE_GROUP_CLEAN) %>%
      filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2], GENDER_ENG == current_gender)
  } else {
    cv_reports %>%
      dplyr::select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG, AGE_Y,AGE_GROUP_CLEAN) %>%
      filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])  
  }
  
  cv_report_drug_pt <- if(is.na(current_brand) == FALSE){
    cv_report_drug %>%
      dplyr::select(REPORT_ID, DRUGNAME) %>%
      filter(DRUGNAME == current_brand)
  } else {
    cv_report_drug %>%
      dplyr::select(REPORT_ID, DRUGNAME)
  }
  
  cv_reactions_pt <-if(is.na(current_rxn) == FALSE){
    cv_reactions %>%
      dplyr::select(REPORT_ID, PT_NAME_ENG) %>%
      filter(PT_NAME_ENG == current_rxn)
  } else {
    cv_reactions %>%
      dplyr::select(REPORT_ID, PT_NAME_ENG)
  }
  patients_tab_master <-cv_reports_sorted_pt%>%
    semi_join(cv_report_drug_pt) %>%
    semi_join(cv_reactions_pt) %>%
    as.data.frame()
  
  return(patients_tab_master) 
}

################################## Function for Drug tab ################################## 
drugs_tab_indt <- function(current_brand, current_rxn,current_gender, current_date_range) { 
  # Import tables with particular search items
  cv_reports_sorted_drg <- if(current_gender != "All"){
    cv_reports %>%
      dplyr::select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG) %>%
      filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2], GENDER_ENG == current_gender) 
  } else {
    cv_reports %>%
      dplyr::select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG) %>%
      filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])
  }
  
  cv_report_drug_drg <- if(is.na(current_brand) == FALSE){
    cv_report_drug %>%
      dplyr::select(REPORT_ID,DRUGNAME) %>%
      filter(DRUGNAME == current_brand)
  } else {
    cv_report_drug %>%
      dplyr::select(REPORT_ID,DRUGNAME)
  }
  
  cv_reactions_drg <- if(is.na(current_rxn) == FALSE){
    cv_reactions %>%
      dplyr::select(REPORT_ID, PT_NAME_ENG) %>%
      filter(PT_NAME_ENG == current_rxn)
  } else {
    cv_reactions %>%
      dplyr::select(REPORT_ID, PT_NAME_ENG)
  }
  
  cv_report_drug_indication_drg <- cv_report_drug_indication %>% dplyr::select(REPORT_ID, INDICATION_NAME_ENG)
  
  # Data frame used to obtain Top_25_indication bar chart: Indication is only associated with individual drug
  # When brand name is unspecified, chart shows top 25 indications associated with DRUGNAME="REMICADE" + date_range
  # When brand name is specified, chart shows top 25 indications associated with specified drug + date_range
  
  # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name 
  #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq) 
  drugs_tab_indication <- cv_reports_sorted_drg%>%
    semi_join(cv_report_drug_drg, by="REPORT_ID") %>% 
    semi_join(cv_reactions_drg, by="REPORT_ID") %>%
    inner_join(cv_report_drug_indication_drg) %>%
    as.data.frame()
  
  return(drugs_tab_indication)
}


drugs_tab_topdrg <- function(current_brand,current_rxn,current_gender,current_date_range) {

  df <- drugs_tab_indt(current_brand = current_brand, current_rxn = current_rxn, current_gender =current_gender, current_date_range=current_date_range)
  
  indications <-  dplyr::summarise(group_by(df, INDICATION_NAME_ENG),count=n_distinct(REPORT_ID))
  top_indications<- indications %>% arrange(desc(count)) %>% top_n(n=1) %>% dplyr::select(INDICATION_NAME_ENG)
  top_indications_final <- top_indications$INDICATION_NAME_ENG
  
  
  # indication import
  cv_report_drug_indication_drg <- cv_report_drug_indication %>%
    dplyr::select(REPORT_ID, INDICATION_NAME_ENG, DRUGNAME) %>%
    filter(INDICATION_NAME_ENG == top_indications_final)
  cv_reports_sorted_drg <- if(current_gender != "All"){
    cv_reports %>%
      dplyr::select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG) %>%
      filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2], GENDER_ENG == current_gender)
  } else{
    cv_reports %>%
      dplyr::select(REPORT_ID, DATINTRECEIVED_CLEAN, GENDER_ENG) %>%
      filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])
  }
  
  cv_report_drug_drg <- if(is.na(current_brand) == FALSE){
    cv_report_drug %>%
      dplyr::select(REPORT_ID,DRUGNAME) %>%
      filter(DRUGNAME != current_brand)
  } else {
    cv_report_drug %>%
      dplyr::select(REPORT_ID,DRUGNAME)
  }
  
  drugs_tab_topdrg <- cv_reports_sorted_drg %>% 
    inner_join(cv_report_drug_drg)%>%
    semi_join(cv_report_drug_indication_drg) %>%
    dplyr::select(REPORT_ID, DRUGNAME, GENDER_ENG) %>%
    as.data.frame()
  
  return(drugs_tab_topdrg)
}

################################## Function for Reactions tab ################################## 
reactions_tab <- function(current_brand,current_rxn,current_gender,current_date_range) { 
  
  # Import tables with particular search items with method to deal with unspecified search term
  cv_reports_sorted_rxn <- if(current_gender != "All"){
    cv_reports %>%
      dplyr::select(REPORT_ID, DATINTRECEIVED_CLEAN, OUTCOME_ENG,GENDER_ENG) %>%
      filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2]) %>%
      filter(GENDER_ENG == current_gender)
  } else {
    cv_reports %>%
      dplyr::select(REPORT_ID, DATINTRECEIVED_CLEAN, OUTCOME_ENG,GENDER_ENG) %>%
      filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])  
  } 
  
  cv_report_drug_rxn <- if(is.na(current_brand) == FALSE){
    cv_report_drug %>%
      dplyr::select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME) %>%
      filter(DRUGNAME == current_brand)
  } else {
    cv_report_drug %>%
      dplyr::select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME)
  }
  cv_reactions_rxn <- if(is.na(current_rxn) == FALSE){
    cv_reactions %>%
      dplyr::select(REPORT_ID, PT_NAME_ENG) %>%
      filter(PT_NAME_ENG == current_rxn)
  } else {
    cv_reactions %>%
      dplyr::select(REPORT_ID, PT_NAME_ENG)
  }
  
  # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each OUTCOME_ENG 
  #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq)
  reactions_tab_master <- cv_reports_sorted_rxn %>%
    semi_join(cv_report_drug_rxn) %>%
    semi_join(cv_reactions_rxn) %>%
    dplyr::select(REPORT_ID, OUTCOME_ENG) %>%
    as.data.frame()
  
  return(reactions_tab_master)
}



drugs_rxn <- function(current_brand,current_date_range){
  if(is.na(current_brand) == FALSE){
    cv_report_drug_rxn <- cv_report_drug %>%
      dplyr::select(REPORT_ID,DRUGNAME) %>%
      filter(DRUGNAME == current_brand)
    
    cv_reactions_rxn <- cv_reactions %>% dplyr::select(REPORT_ID, PT_NAME_ENG)
    
    drugs_rxn_df <- cv_reactions_rxn %>% inner_join(cv_report_drug_rxn) %>% as.data.frame()
    drugs_rxn_result <- dplyr::summarise(group_by(drugs_rxn_df, DRUGNAME,PT_NAME_ENG),count=n_distinct(REPORT_ID))%>% 
      dplyr::arrange(desc(count))%>%
      top_n(10) %>%
      as.data.frame()
  } else {
    cv_reactions_rxn  <- cv_reactions %>% dplyr::select(REPORT_ID, PT_NAME_ENG)
    
    toprxn <- dplyr::summarise(group_by(cv_reactions_rxn, PT_NAME_ENG),count=n_distinct(REPORT_ID))
    drugs_rxn_result <- toprxn %>% dplyr::arrange(desc(count)) %>% top_n(10) %>% dplyr::select(PT_NAME_ENG, count) %>% as.data.frame()
  }
  
  return(drugs_rxn_result)
}

##################### Download Function ####################
download <- function(current_brand,current_rxn,current_gender,current_date_range,current_report_type){
  # Report info type (only filtered cv_reports included)
  if(current_report_type == "Report Info"){
    cv_reports_sorted_dl <- if(current_gender != "All"){
      cv_reports %>%
        filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2], GENDER_ENG == current_gender)
    } else {
      cv_reports %>%
        filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])
    }
    
    cv_report_drug_dl <- if(is.na(current_brand) == FALSE){
      cv_report_drug %>%
        dplyr::select(REPORT_ID, DRUGNAME) %>%
        filter(DRUGNAME == current_brand)
    } else {
      cv_report_drug %>%
        dplyr::select(REPORT_ID, DRUGNAME)
    }
    cv_reactions_dl <- if(is.na(current_rxn) == FALSE){
      cv_reactions %>%
        dplyr::select(REPORT_ID, PT_NAME_ENG) %>%
        filter(PT_NAME_ENG == current_rxn)
    } else {
      cv_reactions %>%
        dplyr::select(REPORT_ID, PT_NAME_ENG)
    }
    
    
    reports_tab_master <-  cv_reports_sorted_dl%>%
      semi_join(cv_report_drug_dl) %>%
      semi_join(cv_reactions_dl) %>%
      as.data.frame()
  }
  
  # Drug Info
  if(current_report_type == "Drug Info"){
    cv_reports_sorted_dl <- if(current_gender != "All"){
      cv_reports %>%
        dplyr::select(REPORT_ID)%>%
        filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2], GENDER_ENG == current_gender)
    } else {
      cv_reports %>%
        dplyr::select(REPORT_ID)%>%
        filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])
    }
    
    cv_report_drug_dl <- if(is.na(current_brand) == FALSE){
      cv_report_drug %>%
        filter(DRUGNAME == current_brand)
    } else {
      cv_report_drug
    }
    cv_reactions_dl <- if(is.na(current_rxn) == FALSE){
      cv_reactions %>%
        dplyr::select(REPORT_ID, PT_NAME_ENG) %>%
        filter(PT_NAME_ENG == current_rxn)
    } else {
      cv_reactions %>%
        dplyr::select(REPORT_ID, PT_NAME_ENG)
    }
    
    
    reports_tab_master <-  cv_report_drug_dl%>%
      semi_join(cv_reports_sorted_dl) %>%
      semi_join(cv_reactions_dl) %>%
      as.data.frame()
  }
  
  # Reaction Info
  if(current_report_type == "Reaction Info"){
    cv_reports_sorted_dl <- if(current_gender != "All"){
      cv_reports %>%
        dplyr::select(REPORT_ID)%>%
        filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2], GENDER_ENG == current_gender)
    } else {
      cv_reports %>%
        dplyr::select(REPORT_ID)%>%
        filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])
    }
    
    cv_report_drug_dl <- if(is.na(current_brand) == FALSE){
      cv_report_drug %>%
        dplyr::select(REPORT_ID, DRUGNAME) %>%
        filter(DRUGNAME == current_brand)
    } else {
      cv_report_drug %>% dplyr::select(REPORT_ID, DRUGNAME)
    }
    
    cv_reactions_dl <- if(is.na(current_rxn) == FALSE){
      cv_reactions %>%
        filter(PT_NAME_ENG == current_rxn)
    } else {
      cv_reactions 
    }
    
    reports_tab_master <-  cv_reactions_dl%>%
      semi_join(cv_reports_sorted_dl) %>%
      semi_join(cv_report_drug_dl) %>%
      as.data.frame()
  }
  
  return(reports_tab_master)
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
  dashboardHeader(title = "CV Shiny"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Reports", tabName = "reportdata", icon = icon("hospital-o")),
      menuItem("Drugs", tabName = "drugdata", icon = icon("flask")),
      menuItem("Patients", tabName = "patientdata", icon = icon("user-md")),
      menuItem("Reactions", tabName = "rxndata", icon = icon("heart-o")),
      menuItem("Signal Detection", tabName = "DISPdata", icon = icon("fa fa-binoculars")),
      menuItem("Download", tabName = "downloaddata", icon = icon("fa fa-download")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"))
    ),
    selectizeInput("search_brand", 
                   "Brand Name (US Trade Name)",
                   topbrands$DRUGNAME,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    
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
    actionButton("searchButton", "Search"),
    tags$br(),
    tags$h3(strong("Current Query:")),
    tableOutput("current_search")
  ), 
  
  dashboardBody(
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
                box(htmlOutput("reporterplot"), 
                    tags$br(),
                    tags$p("Qualification of the person who filed the report."),
                    tags$p("Unknown is the number of reports without the primarysource.qualification field."),
                    title = tags$h2("Reporter"), width = 4),
                box(htmlOutput("seriousplot"), 
                    tags$br(),
                    tags$p("Reports marked as serious."),
                    title = tags$h2("Serious reports"), width = 4),
                box(htmlOutput("seriousreasonsplot"), 
                    tags$br(),
                    tags$p("Total sums to more than 100% because reports can be marked serious for multiple reasons."),
                    title = tags$h2("Reasons for serious reports"), width = 4)
              )
      ),
      tabItem(tabName = "patientdata",
              fluidRow(
                box(htmlOutput("sexplot"),
                    tags$br(),
                    tags$p("Unknown includes reports explicitly marked unknown and Not Specified includes reports with no gender information."),
                    title = tags$h2("Gender"), width = 4),
                box(htmlOutput("agegroupplot"),
                    tags$br(),
                    tags$p("Unknown includes reports with no age information."), 
                    title = tags$h2("Age Groups"), width = 4),
                box(plotlyOutput("agehist"), title = tags$h2("Age Histogram"), width = 4)
              )
      ),
      tabItem(tabName = "drugdata",
              fluidRow(
                box(plotOutput("indicationplot"),
                    tags$br(),
                    tags$p("This plot includes top_10 indications for drugs associated with the matching reports."), width = 4),
                box(plotOutput("drugplot"),
                    tags$br(),
                    tags$p("This plot includes top_10 most-reported drugs with most-reported indication assocaiated with the seached drug."), width = 4)
              )
      ),
      tabItem(tabName = "rxndata",
              fluidRow(
                box(htmlOutput("outcomeplot"), title = tags$h2("Outcomes (all reactions)")),
                box(htmlOutput("rxnTbl"), title = tags$h2("Top10 Reactions Associated with Searched Drug"))
              )
      ),
      tabItem(tabName = "DISPdata",
              fluidRow(
                box(
                  htmlOutput("signalplot_BCPNN"), title = tags$h2("Top 10 Signals Detected By BCPNN Method"),width=4.5,
                  tags$p("BCPNN method detects signals based on Information Component (IC) which evaluate the association between each Drug*AR pair. Those ICs are then sorted based on strength of the signals. Details about this method are explained in the About Tab.")
                ),
                box(
                  htmlOutput("signalplot_PRR"), title = tags$h2("Top 10 Signals Detected By PRR Method"),width=4.5,
                  tags$p("PRR method detects signals based on Proportional Reporting Ratio (PRR) which detects more-frequently-reported Drug*AR pairs. Those PRR are then sorted based on their respective strengths. Details about this method are explained in the About Tab.")
                )
              )
      ),
      
      tabItem(tabName = "downloaddata",
              fluidRow(
                # box(plotOutput("indicationplot"),
                #     tags$br(),
                #     tags$p("This plot includes top_10 indications for drugs associated with the matching reports."), width = 4),
                box(
                  title = tags$h2("Download Dataset for Disproportionality Analysis"),
                  dateRangeInput("searchDateRange_DISP",
                                 "Date Range",
                                 start = "2015-01-01",
                                 end = "2015-03-31",
                                 startview = "year",
                                 format = "yyyy-mm-dd"),
                  actionButton("searchDISPButton", "Search"),
                  tableOutput("current_DISP_search"),
                  tableOutput("current_DISP_size"),
                  downloadButton('downloadData_DISP', 'Download')
                ),
                
                box(
                  tags$h2("Download Data Used for Current Searched Combination"),
                  tags$h3("Please select a category: "),
                  selectizeInput("search_dataset_type",
                                 "Information Category",
                                 choices = c("Report Info", "Drug Info", "Reaction Info"),  
                                 options = list(create = TRUE,
                                                placeholder = 'Please select an option below',
                                                onInitialize = I('function() { this.setValue(""); }'))),
                  actionButton("search_report_type_dl","Go"),
                  tableOutput("download_reports_type"),
                  tableOutput("download_reports_size"),
                  downloadButton('download_reports', 'Download')
                )
                # box(plotOutput("drugplot"),
                #     tags$br(),
                #     tags$p("This plot includes top_10 most-reported drugs with most-reported indication assocaiated with the seached drug."), width = 4)
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
  cv_reports_tab <- reactive({
    input$searchButton
    isolate({
      #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
      current_brand <- ifelse(input$search_brand == "", NA, input$search_brand)
      current_rxn <- ifelse(input$search_rxn == "", NA, input$search_rxn)
      current_gender <- ifelse(input$search_gender == "", "All", input$search_gender)
      current_date_range <- input$searchDateRange
    
    reports_tab_df <- reports_tab(current_brand=current_brand,current_rxn=current_rxn,current_gender =current_gender,current_date_range=current_date_range)
    
    return(reports_tab_df)
  })})
  
  # sample datasets of what is being graphed/used
  #output$outputReports <- renderTable({
  #  cv_reports_tab()[1:4,c("ACTIVE_INGREDIENT_NAME","DRUGNAME","DATINTRECEIVED_CLEAN","PT_NAME_ENG")]
  #})
  
  #observe({updatedplyr::selectizeInput(session, )})
  
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
                                terms = c(current_brand,current_rxn,current_gender,paste(current_date_range[1]," to ", current_date_range[2])),
                                stringsAsFactors=FALSE)
    
    search_tab_df$terms[is.na(search_tab_df$terms) == TRUE] <- "Not Specified (All)"
    return(search_tab_df)
  })})
  
  cv_patients_tab <- reactive({
    input$searchButton
    isolate({
      #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
      current_brand <- ifelse(input$search_brand == "", NA, input$search_brand)
      current_rxn <- ifelse(input$search_rxn == "", NA, input$search_rxn)
      current_gender <- ifelse(input$search_gender == "", "All", input$search_gender)
      current_date_range <- input$searchDateRange
      
    patients_tab_df <- patients_tab(current_brand=current_brand,current_rxn=current_rxn,current_gender =current_gender,current_date_range=current_date_range)
    
    return(patients_tab_df)
  })})
  
  cv_drug_tab_topdrg <- reactive({
    input$searchButton
    isolate({
      #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
      current_brand <- ifelse(input$search_brand == "", NA, input$search_brand)
      current_rxn <- ifelse(input$search_rxn == "", NA, input$search_rxn)
      current_gender <- ifelse(input$search_gender == "", "All", input$search_gender)
      current_date_range <- input$searchDateRange
      
    drugs_tab_topdrg_df <- drugs_tab_topdrg(current_brand=current_brand,current_rxn=current_rxn,current_gender =current_gender,current_date_range=current_date_range)
    
    return(drugs_tab_topdrg_df)
  })})
  
  cv_drug_tab_indc <- reactive({
    input$searchButton
    isolate({
      #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
      current_brand <- ifelse(input$search_brand == "", NA, input$search_brand)
      current_rxn <- ifelse(input$search_rxn == "", NA, input$search_rxn)
      current_gender <- ifelse(input$search_gender == "", "All", input$search_gender)
      current_date_range <- input$searchDateRange
      drugs_tab_indt_df <- drugs_tab_indt(current_brand=current_brand,current_rxn=current_rxn,current_gender =current_gender,current_date_range=current_date_range)
      
      return(drugs_tab_indt_df)
    })})
  
  cv_reactions_tab <- reactive({
    input$searchButton
    isolate({
      #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
      current_brand <- ifelse(input$search_brand == "", NA, input$search_brand)
      current_rxn <- ifelse(input$search_rxn == "", NA, input$search_rxn)
      current_gender <- ifelse(input$search_gender == "", "All", input$search_gender)
      current_date_range <- input$searchDateRange
      reactions_tab_df <- reactions_tab(current_brand=current_brand,current_rxn=current_rxn,current_gender = current_gender,current_date_range=current_date_range)
      
      return(reactions_tab_df)
    })})
  
  cv_reactions_tbl <- reactive({
    input$searchButton
    isolate({
      #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
      current_brand <- ifelse(input$search_brand == "", NA, input$search_brand)
      current_date_range <- input$searchDateRange
      drugs_rxn_df <- drugs_rxn(current_brand=current_brand,current_date_range=current_date_range)
      
      return(drugs_rxn_df)
    })})
  
  
  cv_download_DISP <- reactive({
    input$searchDISPButton
    current_date_range <- isolate(input$searchDateRange_DISP)
    escape.POSIXt <- dplyr:::escape.Date
    
    part1 <-cv_drug_product_ingredients %>% dplyr::select(DRUG_PRODUCT_ID,ACTIVE_INGREDIENT_NAME) %>% left_join(cv_report_drug) %>% 
      dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME) %>% 
      left_join(cv_reactions) %>% dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME, PT_NAME_ENG) #%>% as.data.table(n=-1)
    
    part2 <- cv_reports  %>% 
      filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2]) %>%
      dplyr::select(REPORT_ID,DATINTRECEIVED_CLEAN) #%>% as.data.table(n=-1)
    
    DISP_final <- dplyr::summarise(group_by(semi_join(part1,part2),ACTIVE_INGREDIENT_NAME,PT_NAME_ENG), count = n_distinct(REPORT_ID)) %>% as.data.frame()
    
    DISP_date_range <- as.data.frame(
      paste("Data Range: ", current_date_range[1], "to", current_date_range[2])
    )
    DISP_final_size <- as.data.frame(
      paste("Size of Dataset for Dispropotionality Analysis is ",round(as.numeric(object.size(DISP_final))/1000000,digits=0),"MB")
    )
    
    return(list(DISP_final <- DISP_final,
                DISP_date_range <- DISP_date_range,
                DISP_final_size <- DISP_final_size))
  })
  
  
  ########## Download searched dataset ############# 
  cv_download_reports <- reactive({
    input$searchButton
    isolate({
      #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
      current_brand <- ifelse(input$search_brand == "", NA, input$search_brand)
      current_rxn <- ifelse(input$search_rxn == "", NA, input$search_rxn)
      current_gender <- ifelse(input$search_gender == "", "All", input$search_gender)
      current_date_range <- input$searchDateRange
      
      # report type used for downloading dataset
      input$search_report_type_dl
      current_report_type <- isolate(ifelse(input$search_dataset_type == "",
                                            "Report Info",
                                            input$search_dataset_type))
      
      reports_tab_master <- download(current_brand=current_brand,current_rxn=current_rxn,current_gender=current_gender,current_date_range=current_date_range,current_report_type=current_report_type)
      
      download_type <- as.data.frame(
        paste("Report Type to be downloaded is ",current_report_type)
      )
      
      reports_tab_master_size <- as.data.frame(
        paste("Size of Dataset is ",round(as.numeric(object.size(reports_tab_master))/1000000,digits=0),"MB")
      )
      return(list(reports_tab_master <- reports_tab_master,
                  download_type <- download_type,
                  reports_tab_master_size <- reports_tab_master_size)) 
    })})
  
  
  ############################## DISP tab ##############################
  # BCPNN
  cv_disp_BCPNN <- reactive({
    input$DISPButton
    current_date_range <- isolate(input$searchDateRange_DISP_ANALYSIS)
    #current_date_range <- c(ymd("20000401", ymd("20000630")))
    escape.POSIXt <- dplyr:::escape.Date
    
    part1 <-cv_drug_product_ingredients %>% dplyr::select(DRUG_PRODUCT_ID,ACTIVE_INGREDIENT_NAME) %>% left_join(cv_report_drug) %>% 
      dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME) %>% 
      left_join(cv_reactions) %>% dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME, PT_NAME_ENG) #%>% as.data.table(n=-1)
    
    part2 <- cv_reports  %>% 
      filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2]) %>%
      dplyr::select(REPORT_ID,DATINTRECEIVED_CLEAN) #%>% as.data.table(n=-1)
    
    DISP_final <- dplyr::summarise(group_by(semi_join(part1,part2),ACTIVE_INGREDIENT_NAME,PT_NAME_ENG), count = n_distinct(REPORT_ID)) %>% as.data.frame()
    
    bayes_table <- as.PhViD(DISP_final, MARGIN.THRES = 1) 
    bayes_result <- BCPNN(bayes_table, RR0 = 1, MIN.n11 = 3, DECISION = 3,DECISION.THRES = 0.05, RANKSTAT = 2, MC=FALSE)
    
    signals <- as.data.frame(bayes_result$SIGNAL) 
    signals_final <- signals %>% mutate(D_AR_Comb = paste(signals$`drug code`, " * ", signals$`event effect`))
    return(signals_final)
  })
  
  # PRR
  cv_disp_PRR <- reactive({
    input$DISPButton
    current_date_range <- isolate(input$searchDateRange_DISP_ANALYSIS)
    escape.POSIXt <- dplyr:::escape.Date
    
    part1 <-cv_drug_product_ingredients %>% dplyr::select(DRUG_PRODUCT_ID,ACTIVE_INGREDIENT_NAME) %>% left_join(cv_report_drug) %>% 
      dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME) %>% 
      left_join(cv_reactions) %>% dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME, PT_NAME_ENG) #%>% as.data.table()
    
    part2 <- cv_reports  %>% 
      filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2]) %>%
      dplyr::select(REPORT_ID,DATINTRECEIVED_CLEAN) #%>% as.data.table()
    
    DISP_final <- dplyr::summarise(group_by(semi_join(part1,part2),ACTIVE_INGREDIENT_NAME,PT_NAME_ENG), count = n_distinct(REPORT_ID)) %>% as.data.frame()
    
    bayes_table_PRR <- as.PhViD(DISP_final, MARGIN.THRES = 1) 
    
    PRR_results <- PRR(bayes_table_PRR, RR0=1, MIN.n11 = 1, DECISION = 3, DECISION.THRES = 0.05, RANKSTAT = 2)
    
    signals <-  PRR_results$SIGNAL %>% as.data.frame()
    signals_final <- signals %>% mutate(D_AR_Comb = paste(signals$`drug code`, " * ", signals$`event effect`)) %>% arrange(desc(`LB95(log(PRR))`)) %>% top_n(10,wt=`LB95(log(PRR))`)
    return(signals_final)
  })
#########################################################################################################################################
  
  ############# Download Tab #################
  ############# DISP download ##########
  output$downloadData_DISP <- downloadHandler(
    filename = function() { paste('DISP_input', '.csv', sep='') },
    content = function(file) {
      data <- as.data.frame(cv_download_DISP()[1])
      write.csv(data, file)
    }
  )
  
  output$current_DISP_search <-renderTable({
    data<- as.data.frame(cv_download_DISP()[2])
  }, include.rownames = FALSE, include.colnames = FALSE) 
  
  output$current_DISP_size <-  renderTable({
    data <- as.data.frame(cv_download_DISP()[3])
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  
  
  
  ############ Download part ################
  output$download_reports <- downloadHandler(
    filename = function() { paste('reports', '.csv', sep='') },
    content = function(file) {
      data <- as.data.frame(cv_download_reports()[1])
      write.csv(data, file)
    }
  )
  
  output$download_reports_type <-renderTable({
    data <- as.data.frame(cv_download_reports()[2])
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  output$download_reports_size <- renderTable({
    data <- as.data.frame(cv_download_reports()[3])
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  
  
  
  ############## Construct Current_Query_Table for generic name, brand name, adverse reaction term & date range searched ############
  output$current_search <- renderTable({
     data<- cv_search_tab()
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  ############### Create time plot #####################
  output$timeplot <- renderPlotly({
    
    data <- cv_reports_tab()
    #data <-reports_tab(current_brand=current_brand,current_rxn=current_rxn,current_gender =current_gender,current_date_range=current_date_range)
    #drug_selected <- data$DRUGNAME[1] 
    
    data1 <- cv_search_tab()
    drug_selected <- data1$terms[1]
    
    # specify the title of time plot based on reactive choice
    title <- ifelse(drug_selected == "Not Specified (All)", "All Drugs",drug_selected)
    plottitle <- paste("Drug Adverse Event Reports for", title)
    p <- adrplot(adrplot_test = data, plottitle = plottitle)
    #print(p)
    ggplotly(p)
  })
  
  ############### Create Reporter pie chart ##############  
  output$reporterplot <- renderGvis({
    data <- cv_reports_tab()
    # test
    # data <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
    reporter_df <- data %>% dplyr::select(REPORT_ID, REPORTER_TYPE_ENG)
    
    # Use ddply & count_func to count number of unique report for each REPORTER_TYPE_ENG
    reporter_results<-dplyr::summarise(group_by(reporter_df, REPORTER_TYPE_ENG),count=n_distinct(REPORT_ID))
    reporter_results$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == ""] <- "Not Specified"
    
    gvisPieChart(reporter_results, 
                 labelvar = "REPORTER_TYPE_ENG",
                 numvar = "n", 
                 options = list(pieHole = 0.4))
    # plot this pie chart
    # plot(gvisPieChart(reporter_results, 
    #                   labelvar = "REPORTER_TYPE_ENG",
    #                   numvar = "n", 
    #                   options = list(pieHole = 0.4)))
  })
  
  ################ Create Serious reports pie chart ##################   
  output$seriousplot <- renderGvis({
    data <- cv_reports_tab()
    
    serious_df <-  data%>%
      dplyr::select(REPORT_ID,SERIOUSNESS_ENG)
    
    serious_results <- dplyr::summarise(group_by(serious_df, SERIOUSNESS_ENG),count=n_distinct(REPORT_ID))
    serious_results$SERIOUSNESS_ENG[serious_results$SERIOUSNESS_ENG == ""] <- "Not Specified"
    
    gvisPieChart(serious_results, 
                 labelvar = "SERIOUSNESS_ENG",
                 numvar = "n", 
                 options = list(pieHole = 0.4))
    #plot(gvisPieChart(serious_results, 
    #                  labelvar = "SERIOUSNESS_ENG",
    #                  numvar = "n", 
    #                  options = list(pieHole = 0.4)))
  })
  
  ################ Create Serious Reason Reports chart ################## 
  output$seriousreasonsplot <- renderGvis({
    data <- cv_reports_tab()
    
    # calculate total number of serious reports
    total_serious <- dplyr::summarise(group_by(data, SERIOUSNESS_ENG),count=n_distinct(REPORT_ID))
    total_serious_final <- total_serious$count[total_serious$SERIOUSNESS_ENG == "Yes"]
    
    
    serious_reason_df <- data %>% mutate(SERIOUSNESS_ENG = ifelse(REPORT_ID == 645744, "Yes", SERIOUSNESS_ENG))
    # REPORT_ID = 645744 has serious_reason specifed but seriousness_eng is balnk
    
    # Congenital               
    #congenital_results <- ddply(serious_reason_df,c("SERIOUSNESS_ENG","CONGENITAL_ANOMALY"),count_func)
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
    #death_results <- ddply(serious_reason_df, c("SERIOUSNESS_ENG", "DEATH"),count_func)
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
    #disabling_results <-  ddply(serious_reason_df, c("SERIOUSNESS_ENG", "DISABILITY"),count_func)
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
    #hospital_results <- ddply(serious_reason_df,c("SERIOUSNESS_ENG","HOSP_REQUIRED"),count_func)
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
    #lifethreaten_results <- ddply(serious_reason_df, c("SERIOUSNESS_ENG", "LIFE_THREATENING"),count_func)
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
    data <- cv_patients_tab()
    
    # replace blank in GENDER_ENG with character "Unknown"
    data$GENDER_ENG[data$GENDER_ENG == ""] <- "Unknown"
    
    sex_results <-dplyr::summarise(group_by(data, GENDER_ENG),count=n_distinct(REPORT_ID))
    
    gvisPieChart(sex_results, 
                 labelvar = "GENDER_ENG",
                 numvar = "count", 
                 options = list(pieHole = 0.4, pieSliceText="percentage", fontSize=12))
  })
  
  ################ Create Age Group pie chart in Patient tab ##################      
  output$agegroupplot <- renderGvis({
    data <- cv_patients_tab()
    
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
    data <- cv_patients_tab()
    
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
    
    print(hist)
    ggplotly(hist)
    
  })
  
  ################ Create drug plots in Drug tab ################## 
  output$drugplot <- renderPlot({
    
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
    p
    print(p)
    ggplotly(p)
  })
  
  output$indicationplot <- renderPlot({
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
      ggtitle("Top 10 Indications") +
      xlab("Indication") + 
      ylab("Number") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), 
            legend.position = "none") +
      scale_y_continuous(limits=  c(0, max(indications_sorted$count)))
    p
    print(p)
    ggplotly(p)
  })
  
  # sample datasets of what is being graphed/used
  #output$outputReports <- renderTable(
  #  cv_drug_tab_indc()
  #)
  
  ################ Create Outcomes(all reactions) pie chart in Reaction tab ################## 
  output$outcomeplot <- renderGvis({
    data <- cv_reactions_tab()
    
    outcome_results <-  dplyr::summarise(group_by(data, OUTCOME_ENG),count=n_distinct(REPORT_ID))
    
    gvisPieChart(outcome_results, 
                 labelvar = "OUTCOME_ENG",
                 numvar = "n", 
                 options = list(pieHole = 0.4))
  })
  
  output$rxnTbl <- renderGvis({
    data <- cv_reactions_tbl()
    
    # GoogleVis plot html: use plot() to graph it
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
                   height=500
                 )
    )
    
  })
  
  ##################### Create DISP Tab Graphs #######################
  output$signalplot_BCPNN<- renderGvis({
    data <- cv_disp_BCPNN()
    data_final <- data %>% top_n(10)
    
    # GoogleVis plot html: use plot() to graph it
    gvisBarChart(data_final,
                 xvar = "D_AR_Comb",
                 yvar = "Q_0.025(log(IC))",
                 options = list(
                   #vAxes="[{title:'D*AR Combination'}",
                   legend = "{position:'none'}",
                   bars = 'horizontal',
                   # axes= "x: {
                   #   0: { side: 'top', label: 'Number of Reports'}}",
                   bar = list(groupWidth =  '90%'),
                   height=500,
                   vAxis.textStyle = "{color:'black',fontName:'Courier',fontSize:5}",
                   title = "Top 10 Signals Detected By BCPNN Method",
                   hAxes="[{title:'Strength of Signal'}]"
                 )
    )
    
  })
  
  output$signalplot_PRR <- renderGvis({
    data<- cv_disp_PRR()
    
    signals_plot <- gvisBarChart(data,
                                 xvar = "D_AR_Comb",
                                 yvar = "LB95(log(PRR))",
                                 options = list(
                                   #vAxes="[{title:'D*AR Combination'}",
                                   legend = "{position:'none'}",
                                   bars = 'horizontal',
                                   # axes= "x: {
                                   #   0: { side: 'top', label: 'Number of Reports'}}",
                                   bar = list(groupWidth =  '90%'),
                                   height=500,
                                   vAxis.textStyle = "{color:'black',fontName:'Courier',fontSize:5}",
                                   title = "Top 10 Signals Detected By PRR Method",
                                   hAxes="[{title:'Strength of Signal'}]"
                                 )
    )
  })
}

shinyApp(ui, server)
