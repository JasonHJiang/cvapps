# data manip + utils
library(magrittr)
library(lubridate)
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
library(shinyBS)
library(shinyjs)
library(DT)

source("common_ui.R")

########## Codes to fetch top 1000 specific results to be used in dropdown menu ############### 
# Temperary solution: fetch all tables to local and run functions on them
hcopen_pool <- dbPool(drv = "PostgreSQL",
                      host = "shiny.hc.local",
                      dbname = "hcopen",
                      user = "hcreader",
                      password = "canada1")
hcopen <- src_pool(hcopen_pool)
cv_reports <- tbl(hcopen, "cv_reports_20160630")
cv_drug_product_ingredients <-  tbl(hcopen, "cv_drug_product_ingredients_20160630")
cv_report_drug <- tbl(hcopen, "cv_report_drug_20160630")
cv_reactions <- tbl(hcopen, "cv_reactions_20160630")
cv_report_drug_indication <- tbl(hcopen, "cv_report_drug_indication_20160630")
cv_substances <- tbl(hcopen, "cv_substances")
meddra <- tbl(hcopen, "meddra") %>%
  filter(Primary_SOC_flag == "Y") %>%
  select(PT_Term, HLT_Term, Version = MEDDRA_VERSION)

####################### datasets for menu setup ####################### 
#Fetch brand/drug names
topbrands <- cv_report_drug %>%
  distinct(DRUGNAME) %>%
  as.data.frame() %>%
  `[[`(1) %>%
  sort() %>%
  `[`(-c(1,2)) # dropping +ARTHRI-PLUS\u0099 which is problematic
topings_dpd <- cv_substances %>%
  distinct(ing) %>%
  as.data.frame() %>%
  `[[`(1) %>%
  sort()
topings_cv <- cv_drug_product_ingredients %>%
  distinct(ACTIVE_INGREDIENT_NAME) %>%
  as.data.frame() %>%
  `[[`(1) %>%
  sort()
pt_choices <- cv_reactions %>%
  distinct(PT_NAME_ENG) %>%
  as.data.frame() %>%
  `[[`(1) %>%
  sort()

header <- dashboardHeader(title = titleWarning("CV Shiny (v0.15)"),
                          titleWidth = 700)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Time Plot", tabName = "timedata", icon = icon("line-chart")),
    menuItem("Reports", tabName = "reportdata", icon = icon("hospital-o")),
    menuItem("Patients", tabName = "patientdata", icon = icon("user-md")),
    menuItem("Drugs", tabName = "drugdata", icon = icon("flask")),
    menuItem("Reactions", tabName = "rxndata", icon = icon("heart-o")),
    menuItem("About", tabName = "aboutinfo", icon = icon("info"), selected = TRUE)
  ),
  #div(
  #  style="display: inline-block; vertical-align: bottom; height: 54px;",
  #  ,
  tableOutput("current_search")
)



body <- dashboardBody(
  customCSS(),
  fluidRow(
    box(
      useShinyjs(),
      id = "search_form",
      column(4,
             selectInput(
               "name_type", h5("Drug Name Type"),
               c("Brand Name" = "brand",
                 "Active Ingredient" = "ingredient")),
             selectInput(
               "drug_inv", h5("Drug Involvement"),
               c("Any",
                 "Suspect",
                 "Concomitant"))),
      column(4,
             dateRangeInput(
               "searchDateRange",
               h5("Date Range"),
               start = "1965-01-01",
               end = "2016-06-30",
               startview = "decade"),
             conditionalPanel(
               condition = "input.name_type == 'brand'",
               selectizeInput("search_brand", 
                              h5(
                                "Brand Name (Canadian Trade Name)",
                                tipify(el = icon("warning"), trigger = "hover click",
                                       title = paste0(
                                         "Note that if your search does not return results, ",
                                         "that means that there are no reports for your query. ",
                                         "Also note that the drop down menu only lists the first ",
                                         "1000 alphabetical results. Type to search for what you are ",
                                         "looking for if it does not show up on the drop down menu."))),
                              c(topbrands, "Start typing to search..." = ""),
                              multiple = TRUE,
                              options = list(dropdownParent = 'body',
                                             selected = topbrands[1]))),
      
             conditionalPanel(
               condition = "input.name_type == 'ingredient'",
               selectizeInput("search_ing", 
                              h5(
                                "Active Ingredient",
                                tipify(el = icon("warning"), trigger = "hover click",
                                       title = paste0(
                                         "Note that if your search does not return results, ",
                                         "that means that there are no reports for your query. ",
                                         "Also note that the drop down menu only lists the first ",
                                         "1000 alphabetical results. Type to search for what you are ",
                                         "looking for if it does not show up on the drop down menu."))),
                              c(topings_cv, "Start typing to search..." = ""),
                              multiple = TRUE,
                              options = list(dropdownParent = 'body',
                                             selected = topings_cv[1])))),
      column(4,
             selectInput(
               "search_gender",
               h5("Select Gender"),
               c("All",
                 "Male",
                 "Female")),
             selectizeInput(
               "search_rxn", 
               h5("Preferred Term (PT)"),
               c("Start typing to search..." = "", pt_choices),
               multiple = TRUE,
               selected = pt_choices[1])),
      width = 9),
    box(
      column(4,
             div(
               class="form-group shiny-input-container",
               actionButton("searchButton",
                            "",
                            icon = icon("search"),
                            width = '100%',
                            style="background-color: #4885ed; border-color: #3885ed")),
             div(
               actionButton("clear_search",
                            "",
                            icon = icon("close"),
                            width = '100%'))),
      column(8,
             selectInput("search_dataset_type",
                         h5("Download Type"),
                         c("Report Data", "Drug Data", "Reaction Data")),
             downloadButton(outputId = 'download_reports',
                            label = 'Download')),
      width = 3)
    ),
  
  tabItems(
    # Time Plot Tab -----------------------------------------------------------
    tabItem(
      tabName = "timedata",
      fluidRow(
        box(htmlOutput(outputId = "timeplot_title"),
            htmlOutput(outputId = "timeplot"),
            "Reports by month from Canada Vigilance Adverse Reaction Online Database",
            width = 12))),
    
    # Reports Tab -------------------------------------------------------------
    tabItem(
      tabName = "reportdata",
      fluidRow(
        box(
          # Seriousness Data Visualization ------------------------------------
          h3("Seriousness",
             tipify(
               el = icon("info-circle"), trigger = "hover click",
               title = paste0(
                 "A serious report contains a serious adverse reaction, determined by the reporter ",
                 "of the report at the time of reporting. Slices may not be visible if they are too small.")
             )),
          width = 6,
          column(
            8,
            htmlOutput("seriousplot")),
          column(
            4,
            selectInput(
              "report.serious.format", h5("Data Display Options"),
              c("Pie Chart"  = "serious.pie",
                "Data Table" = "serious.table")))),
        box(
          # Reporter Data Visualization ---------------------------------------
          h3("Reporter Type",
             tipify(
               el = icon("info-circle"), trigger = "hover click",
               title = paste0(
                 "Indicates who reported the adverse reaction and their relationship to the patient. ",
                 "Slices may not be visible if they are too small.")
             )),
          width = 6,
          column(
            8,
            htmlOutput("reporterplot")),
          column(
            4,
            selectInput(
              "report.reporter.format", h5("Data Display Options"),
              c("Pie Chart"  = "reporter.pie",
                "Data Table" = "reporter.table"))))),
        
      fluidRow(
        box(
          # Reasons for Seriousness Data Visualization ------------------------
          h3("Reason(s) for Seriousness",
             tipify(
               el = icon("info-circle"), trigger = "hover click",
               title = paste0("The serious condition which the adverse event resulted in. Total may sum to",
                              " more than the total number of reports because reports can be marked serious for multiple reasons"))),
          width = 6,
          column(
            8,
            htmlOutput("seriousreasonsplot")),
          column(
            4,
            selectInput(
              "report.seriousreasons.format", h5("Data Display Options"),
              c("Bar Chart"  = "reporter.bar",
                "Data Table" = "reporter.table")))))),
    # Patients Tab ------------------------------------------------------------
    tabItem(
      tabName = "patientdata",
      fluidRow(
        box(
          # Gender Data Visualization -----------------------------------------
          h3("Gender",
             tipify(
               el = icon("info-circle"), trigger = "hover click",
               title = paste0(
                 "Gender of the patient as it was provided by the reporter. ",
                 "Where the gender is unknown, the reporter is unaware of the gender. ",
                 "Where the gender is not specified, the reporter did not specify the gender of the patient."))),
          width = 6,
          column(
            8,
            htmlOutput("sexplot")
          ),
          column(
            4,
            selectInput(
              "patient.sexplot.format", h5("Data Display Options"),
              c("Pie Chart"  = "patient.pie",
                "Data Table" = "patient.table")
            )
          )
        ),
        box(
          # Age Group Data Visualization --------------------------------------
          h3("Age Group",
             tipify(
               el = icon("info-circle"), trigger = "hover click",
               title =  HTML(paste0(
                 "Age group of the patient when the adverse effect occurred.<br>",
                 "<br>Neonate: <= 25 days",
                 "<br>Infant: > 25 days to < 1 yr",
                 "<br>Child: >= 1 yr to < 13 yrs",
                 "<br>Adolescent: >= 13 yrs to < 18 yrs",
                 "<br>Adult: >= 18 yrs to <= 65 yrs",
                 "<br>Elderly: > 65 yrs")))),
          width = 6,
          column(
            8,
            htmlOutput("agegroupplot")
          ),
          column(
            4,
            selectInput(
              "patient.agegroupplot.format", h5("Data Display Options"),
              c("Pie Chart"  = "patient.pie",
                "Data Table" = "patient.table")
            )
          )
        )
      ),
      fluidRow(
        box(
          # Age Histogram Visualization ---------------------------------------
          htmlOutput("agehisttitle"),
          plotlyOutput("agehist"),
          width = 6))),
    # Drug Tab ----------------------------------------------------------------
    tabItem(
      tabName = "drugdata",
      fluidRow(
        box(
          # Reports per Indication --------------------------------------------
          h3("Reports per Indication (all reported drugs)",
             tipify(
               el = icon("info-circle"), trigger = "hover click",
               title = paste(
                 "Indication refers to the particular condition for which a health product was taken. ",
                 "This plot includes all indications for all drugs present in the matching reports. ",
                 "The search query filters unique reports, which may have one or more drugs associated with them."))),
          width = 6,
          column(
            8,
            htmlOutput("indication_plot")
          ),
          column(
            4,
            selectInput(
              "drug.indication_plot.format", h5("Data Display Options"),
              c("Bar Chart"  = "drug.bar",
                "Data Table" = "drug.table")))),
        tabBox(
          tabPanel("Suspect",
                   h3("Most Frequently Reported Drugs (Brand Name)",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste0(
                           "This plot includes all drugs present in the matching reports. ",
                           "The search query filters unique reports, which may have one or more drugs associated with them. ",
                           "The reporter suspects that the health product caused the adverse reaction."))),
                   fluidRow(
                     column(8,
                            htmlOutput("suspect_drugs")),
                     column(4,
                            selectInput(
                              "drug.suspect.format", h5("Data Display Options"),
                              c("Bar Chart"  = "drug.bar",
                                "Data Table" = "drug.table"))))),
          tabPanel("Concomitant",
                   h3("Most Frequently Reported Drugs (Brand Name)",
                      tipify(
                        el = icon("info-circle"), trigger = "hover click",
                        title = paste0(
                          "This plot includes all drugs present in the matching reports. ",
                          "The search query filters unique reports, which may have one or more drugs associated with them. ",
                          "The health product is not suspected, but the patient was taking it at the time of the adverse reaction."))),
                   fluidRow(
                     column(8,
                            htmlOutput("concomitant_drugs")),
                     column(4,
                            selectInput(
                              "drug.concomitant.format", h5("Data Display Options"),
                              c("Bar Chart"  = "drug.bar",
                                "Data Table" = "drug.table"))))),
          tabPanel("All",
                   h3("Most Frequently Reported Drugs (Brand Name)",
                      tipify(
                        el = icon("info-circle"), trigger = "hover click",
                        title = paste0(
                          "This plot includes all drugs present in the matching reports. ",
                          "The search query filters unique reports, which may have one or more drugs associated with them."))),
                   fluidRow(
                     column(8,
                            htmlOutput("all_drugs")),
                     column(4,
                            selectInput(
                              "drug.all.format", h5("Data Display Options"),
                              c("Bar Chart"  = "drug.bar",
                                "Data Table" = "drug.table"))))),
          width = 6)),
      fluidRow(
        box(htmlOutput("drugcounttitle"),
            htmlOutput("drugcount_plot"),
            width = 8))),
    # Reaction Tab ------------------------------------------------------------
    tabItem(tabName = "rxndata",
            fluidRow(
              box(
                h3("Most Frequent Adverse Events (Preferred Terms)",
                   tipify(
                     el = icon("info-circle"), trigger = "hover click",
                     title = paste0(
                       "MedDRA Preferred Term is a distinct descriptor (single medical concept) for a symptom, ",
                       "sign, disease, diagnosis, therapeutic indication, investigation, surgical, or medical ",
                       "procedure, and medical, social, or family history characteristic. For more rigorous analysis, ",
                       "use disproportionality statistics."))),
                width = 6,
                column(8,
                       htmlOutput("top_pt")),
                column(4,
                       selectInput(
                         "rxn.top_pt.format", h5("Data Display Options"),
                         c("Bar Chart"  = "rxn.bar",
                           "Data Table" = "rxn.table")))),
              box(
                h3("Most Frequent Adverse Events (High-Level Terms)",
                   tipify(
                     el = icon("info-circle"), trigger = "hover click",
                     title = "For more rigorous analysis, use disproportionality statistics.")),
                width = 6,
                column(8,
                       htmlOutput("top_hlt")),
                column(4,
                       selectInput(
                         "rxn.top_hlt.format", h5("Data Display Options"),
                         c("Bar Chart"  = "rxn.bar",
                           "Data Table" = "rxn.table"))))),
            fluidRow(
              box(
                h3("Report Outcome",
                   tipify(
                     el = icon("info-circle"), trigger = "hover click",
                     title = paste0(
                       "The report outcome represents the outcome of the reported case as described by the reporter ",
                       "at the time of reporting and does not infer a causal relationship. The report outcome is not ",
                       "based on a scientific evaluation by Health Canada."))),
                width = 6,
                column(8,
                       htmlOutput("outcomeplot")),
                column(4,
                       selectInput(
                         "rxn.outcomeplot.format", h5("Data Display Options"),
                         c("Pie Chart"  = "rxn.pie",
                           "Data Table" = "rxn.table")))))),
    # About Tab ---------------------------------------------------------------
    tabItem(tabName = "aboutinfo",
            box(
              width = 12,
              h2("About"),
              # using tags$p() and tags$a() inserts spaces between text and hyperlink...thanks R
              HTML(paste0(
                "<p>",
                "This is a beta product. DO NOT use as sole evidence to support regulatory decisions or to make decisions regarding ",
                "medical care. Consult your healthcare professional if you have any questions or concerns related to the use of health products.",
                "</p>",
                "<p>",
                "This app has been developed by the Data Sciences Unit of RMOD at Health Canada as part of the Open Data Initiative. ",
                "This is a prototype experiment that utilizes publically available data (Canada Vigilance Adverse Reaction Online Database) ", 
                "and provide visualizations in an interactive format. Health Canada collects and maintains a high volume of adverse event ", 
                "reports associated with different drugs and products. This app allows users to effortlessly interact with the reports ", 
                "database, conduct searches and view results in highly interactive dashboards. To support innovation, coordination and ", 
                "to support Canadians, this interface permits the users to export search results (with no limitation to the number of rows) ", 
                "in various file formats such as CSV and Excel for further exploration and experimentation.",
                "</p>",
                "<br>",
                "<p>",
                "<strong>Data last updated: 2016-06-30</strong><br>",
                "Data provided by the Canada Vigilance Adverse Reaction Online Database. The recency of the data is therefore ",
                "dependent on when the data source is updated, and is the responsibility of the Canada Vigilance Program. ",
                "For more information, please refer to ",
                "<a href = \"http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php\">",
                "http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php</a>.",
                "</p>")),
              aboutAuthors()))))

ui <- dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input, output) {
  ##### Reactive data processing
  # Data structure to store current query info
  current_search <- reactiveValues()
  subset_cv <- reactiveValues()
  
  # We need to have a reactive structure here so that it activates upon loading
  reactiveSearchButton <- reactive(as.vector(input$searchButton))
  observeEvent(reactiveSearchButton(), {
    showModal(modalDialog(
      title = list(icon("spinner", class = "fa-pulse"), "Results loading..."),
      "Please wait while reports are being retrieved.",
      footer = NULL,
      size = "s"))
    
    if (input$name_type == "brand") {
      name <- input$search_brand
    } else if (input$name_type == "ingredient") {
      name <- input$search_ing
    } else {
      name <- input$search_ing2
    }
    
    cv_reports_filtered_ids <- cv_reports %>%
      filter(DATINTRECEIVED_CLEAN >= input$searchDateRange[1], DATINTRECEIVED_CLEAN <= input$searchDateRange[2])
    if (input$search_gender == 'Male' | input$search_gender == 'Female') {
      cv_reports_filtered_ids %<>% filter(GENDER_ENG == input$search_gender)
    }
    cv_reports_filtered_ids %<>% select(REPORT_ID)
    
    cv_report_drug_filtered <- cv_report_drug
    if (input$name_type == "brand" & !is.null(name)) {
      if (length(name) == 1) cv_report_drug_filtered %<>% filter(DRUGNAME == name)
      else cv_report_drug_filtered %<>% filter(DRUGNAME %in% name)
      
    } else if (input$name_type == "ingredient2" & !is.null(name) && name != "") {
      related_drugs <- cv_substances %>% filter(ing == name)
      cv_report_drug_filtered %<>% semi_join(related_drugs, by = "DRUGNAME")
      
    } else if (input$name_type == "ingredient" & !is.null(name)) {
      if (length(name) == 1) related_drugs <- cv_drug_product_ingredients %>% filter(ACTIVE_INGREDIENT_NAME == name)
      else related_drugs <- cv_drug_product_ingredients %>% filter(ACTIVE_INGREDIENT_NAME %in% name)
      cv_report_drug_filtered %<>% semi_join(related_drugs, by = "DRUG_PRODUCT_ID")
      
    }
    if (input$drug_inv != "Any") cv_report_drug_filtered %<>% filter(DRUGINVOLV_ENG == input$drug_inv)
    cv_reactions_filtered <- cv_reactions %>% filter(PT_NAME_ENG != "")
    if (!is.null(input$search_rxn)) {
      if (length(input$search_rxn) == 1) cv_reactions_filtered %<>% filter(PT_NAME_ENG == input$search_rxn)
      else cv_reactions_filtered %<>% filter(PT_NAME_ENG %in% input$search_rxn)
    }
    
    selected_ids <-  cv_reports_filtered_ids %>%
      semi_join(cv_report_drug_filtered, by = "REPORT_ID") %>%
      semi_join(cv_reactions_filtered, by = "REPORT_ID")
    n_ids <- selected_ids %>% tally() %>% as.data.frame() %>% `$`('n')
    if (n_ids == 0) {
      removeModal()
      showModal(modalDialog(
        title = list(icon("exclamation-triangle"), "No results found!"),
        "There were no reports matching your query.",
        size = "s",
        easyClose = TRUE))
      return()
    }
    
    current_search$name_type <- input$name_type
    current_search$name <- name
    current_search$drug_inv <- input$drug_inv
    current_search$rxn <- input$search_rxn
    current_search$date_range <- input$searchDateRange
    
    # so then all data is polled upon search, not just when display corresponding plot
    subset_cv$report <- cv_reports %>%
      semi_join(selected_ids, by = "REPORT_ID") %>% as.data.frame()
    subset_cv$drug <- cv_report_drug %>%
      semi_join(selected_ids, by = "REPORT_ID") %>%
      left_join(cv_report_drug_indication, by = c("REPORT_DRUG_ID", "REPORT_ID", "DRUG_PRODUCT_ID", "DRUGNAME")) %>% as.data.frame()
    subset_cv$rxn <- cv_reactions %>%
      semi_join(selected_ids, by = "REPORT_ID") %>%
      left_join(meddra, by = c("PT_NAME_ENG" = "PT_Term", "MEDDRA_VERSION" = "Version")) %>% as.data.frame()
    removeModal()
  })
  
  cv_download_reports <- reactive({
    data_type <- ""
    
    if(input$search_dataset_type == "Report Data"){
      reports_tab_master <- subset_cv$report
      data_type <- "report"
    } else if(input$search_dataset_type == "Drug Data"){
      reports_tab_master <- subset_cv$drug
      data_type <- "drug"
    } else if(input$search_dataset_type == "Reaction Data"){
      reports_tab_master <- subset_cv$rxn
      data_type <- "rxn"
    }
    reports_tab_master %<>% as.data.frame()
    
    return(list(
      reports_tab_master = reports_tab_master,
      data_type = data_type
    ))
  })
  
  output$current_search <- renderTable({
    data <- current_search
    result <- data.frame(names = c("Name Type:",
                                   "Name:",
                                   "Adverse Reaction Term:",
                                   "Date Range:"),
                         values = c(data$name_type %>% toupper(),
                                    paste0(data$name, collapse = ", "),
                                    paste0(data$rxn, collapse = ", "),
                                    paste(data$date_range, collapse = " to ")),
                         stringsAsFactors = FALSE)
    result$values["" == result$values] <- "All"
    result
  },
  include.colnames = FALSE
  )

# Time Plot Tab Plots ---------------------------------------------------------
  output$timeplot_title <- renderUI({
    nreports <- subset_cv$report %>%
      distinct(REPORT_ID) %>%
      tally() %>%
      as.data.frame()
    drug_name <- paste0(current_search$name, collapse = ", ")
    rxn_name <- paste0(current_search$rxn, collapse = ", ")
    
    if ("" == drug_name) drug_name <- "All Drugs"
    if ("" == rxn_name) rxn_name <- "All Reactions"
    plottitle <- paste0("Drug Adverse Event Reports for ", drug_name, " and ", rxn_name, " (", nreports, " reports)")
    h3(strong(plottitle))
  })
  output$timeplot <- renderGvis({
    # adrplot_test <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria"))
    data <- subset_cv$report
    
    total_results <- data %>%
      group_by(DATINTRECEIVED_CLEAN) %>%
      summarise(count = n()) %>%
      as.data.frame() %>%
      mutate(month = floor_date(ymd(DATINTRECEIVED_CLEAN), "month")) %>%
      count(month, wt = count) %>%
      rename(total = n)
    serious_results <- data %>%
      filter(SERIOUSNESS_ENG == "Yes") %>%
      group_by(DATINTRECEIVED_CLEAN) %>%
      summarise(count = n()) %>%
      as.data.frame() %>%
      mutate(month = floor_date(ymd(DATINTRECEIVED_CLEAN), "month")) %>%
      count(month, wt = count) %>%
      rename(serious = n)
    death_results <- data %>%
      filter(DEATH == 1) %>%
      group_by(DATINTRECEIVED_CLEAN) %>%
      summarise(count = n()) %>%
      as.data.frame() %>%
      mutate(month = floor_date(ymd(DATINTRECEIVED_CLEAN), "month")) %>%
      count(month, wt = count) %>%
      rename(death = n)
    
    nmonths <- interval(min(total_results$month), max(total_results$month)) %/% months(1)
    time_list <- min(total_results$month) + months(0:nmonths)
    
    results <- data.frame(month = time_list) %>%
      left_join(total_results, by = "month") %>%
      left_join(serious_results, by = "month") %>%
      left_join(death_results, by = "month")
    results[is.na(results)] <- 0
    
    
    gvisLineChart(results,
                  xvar = "month",
                  yvar = c("total", "serious", "death"),
                  options = list(
                    height = 350,
                    vAxis = "{title: 'Number of Reports'}",
                    hAxis = "{title: 'Date Received (grouped by month)'}",
                    chartArea = "{top: 10, height: '80%', left: 120, width: '84%'}",
                    colors = colorCodeToString(google_colors[c(18, 13, 2)])
                  ))
  })
# Reporter Tab Plots --------------------------------------------------------
  
  serious_results <- reactive({
    subset_cv$report %>%
    count(SERIOUSNESS_ENG) %>%
    select(SERIOUSNESS_ENG, n) %>%
    as.data.frame() %>%
    mutate(label = NA,
           label = ifelse(SERIOUSNESS_ENG == "Yes", "Serious", label),
           label = ifelse(SERIOUSNESS_ENG == "No", "Non-serious", label),
           label = ifelse(SERIOUSNESS_ENG == "", "Not reported", label)) %>%
    select(label, n) %>%
    slice(match(c("Serious", "Non-serious", "Not reported"), label))
  })
  
  
  output$seriousplot <- renderGvis({
    serious_results <- serious_results()
    if (input$report.serious.format == "serious.pie") {
      gvisPieChart_HCSC(serious_results, "label", "count")
    } else {
      gvisTable(serious_results)
    }
  })
  
  
  
  reporter_results <- reactive({
    reporter <- subset_cv$report %>%
      count(REPORTER_TYPE_ENG) %>%
      as.data.frame()
    reporter$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == ""] <- "Not reported"
    reporter$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == "Consumer Or Other Non Health Professional"] <- "Consumer or non-health professional"
    reporter$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == "Other Health Professional"] <- "Other health professional"
  })
  
  output$reporterplot <- renderGvis({
    reporter_results <- reporter_results()
    if (input$report.reporter.format == "reporter.pie") {
      gvisPieChart_HCSC(reporter_results, "REPORTER_TYPE_ENG", "count")
    } else {
      gvisTable(reporter_results)
    }
  })
  
  serious_reasons <- reactive({
    subset_cv$report %>%
    filter(SERIOUSNESS_ENG == "Yes")
    
    n_congen <- data %>%
      filter(CONGENITAL_ANOMALY == 1) %>%
      tally() %>% as.data.frame() %>% `$`(n)
    n_death <- data %>%
      filter(DEATH == 1) %>%
      tally() %>% as.data.frame() %>% `$`(n)
    n_disab <- data %>%
      filter(DISABILITY == 1) %>%
      tally() %>% as.data.frame() %>% `$`(n)
    n_lifethreat <- data %>%
      filter(LIFE_THREATENING == 1) %>%
      tally() %>% as.data.frame() %>% `$`(n)
    n_hosp <- data %>%
      filter(HOSP_REQUIRED == 1) %>%
      tally() %>% as.data.frame() %>% `$`(n)
    n_other <- data %>%
      filter(OTHER_MEDICALLY_IMP_COND == 1) %>%
      tally() %>% as.data.frame() %>% `$`(n)
    ## Check for NotSpecified ##
    n_notspec <- data %>%
      filter(DEATH != 1 | is.na(DEATH)) %>%
      filter(DISABILITY != 1 | is.na(DISABILITY)) %>%
      filter(CONGENITAL_ANOMALY != 1 | is.na(CONGENITAL_ANOMALY)) %>%
      filter(LIFE_THREATENING != 1 | is.na(LIFE_THREATENING)) %>%
      filter(HOSP_REQUIRED != 1 | is.na(HOSP_REQUIRED)) %>%
      filter(OTHER_MEDICALLY_IMP_COND != 1 | is.na(OTHER_MEDICALLY_IMP_COND)) %>%
      tally() %>% as.data.frame() %>% `$`(n)
    
      serious_reasons <- data.frame(label = c("Death",
                                              "Life-threatening",
                                              "Hospitalization",
                                              "Disability",
                                              "Congenital anomaly",
                                              "Other medically important condition",
                                              "Not specified"),
                                    count = c(n_death,
                                              n_lifethreat,
                                              n_hosp,
                                              n_disab,
                                              n_congen,
                                              n_other,
                                              n_notspec),
                                    stringsAsFactors = FALSE)})
  
  output$seriousreasonsplot <- renderGvis({
    serious_reasons <- serious_reasons()
    if (input$report.seriousreasons.format == "reporter.bar") {
      gvisBarChart(serious_reasons,
                   xvar = "label",
                   yvar = "count",
                   options = list(
                     legend = "{position: 'none'}",
                     hAxis = "{title: 'Number of Reports'}",
                     chartArea = "{top: 0, height: '80%', left: 150, width: '60%'}",
                     bar = "{groupWidth: '90%'}",
                     colors = colorCodeToString(google_colors[5])
                   ))
    } else {
      gvisTable(serious_reasons)
    }
  })
  
# Patient Tab Plots -----------------------------------------------------------
  output$sexplot <- renderGvis({
    data <- subset_cv$report %>%
      count(GENDER_ENG) %>%
      as.data.frame()
    # replace blank in GENDER_ENG with character "Unknown"
    data$GENDER_ENG[data$GENDER_ENG == ""] <- "Not specified"
    sex_results <- count(data, GENDER_ENG, wt = n)
    
    if (input$patient.sexplot.format == "patient.pie") {
      gvisPieChart_HCSC(sex_results, "GENDER_ENG", "n")
    } else {
      gvisTable(sex_results)
    }
  })
  
  output$agegroupplot <- renderGvis({
    age_groups <- subset_cv$report %>%
      count(AGE_GROUP_CLEAN) %>%
      as.data.frame()
    age_group_order <- data.frame(AGE_GROUP_CLEAN = c("Neonate",
                                                      "Infant",
                                                      "Child",
                                                      "Adolescent",
                                                      "Adult",
                                                      "Elderly",
                                                      "Unknown"),
                                  stringsAsFactors = FALSE)
    data <- left_join(age_group_order, age_groups, by = "AGE_GROUP_CLEAN")
    data[is.na(data)] <- 0 # always including empty rows means colour-scheme will be consistent
    
    if (input$patient.agegroupplot.format == "patient.pie") {
      gvisPieChart_HCSC(data, "AGE_GROUP_CLEAN", "n")
    } else {
      gvisTable(data)
    }
  })
  
  output$agehisttitle <- renderUI({
    excluded_count <- subset_cv$report %>%
      filter(AGE_GROUP_CLEAN != "Unknown", AGE_Y > 100) %>%
      tally() %>% `$`('n')
    HTML(paste0("<h3>Histogram of Patient Ages ",
                tipify(
                  el = icon("info-circle"), trigger = "hover click",
                  title = "Distribution of number of reports per age, colour-coded by age group. Each bin groups 2 years."),
                "<br>(", excluded_count, " reports with age greater than 100 excluded)", "</h3>"))
  })
  
  output$agehist <- renderPlotly({
    age_groups <- subset_cv$report %>% filter(AGE_GROUP_CLEAN != "Unknown", AGE_Y <= 100) %>% arrange(AGE_Y)
    age_groups$AGE_GROUP_CLEAN %<>% factor(levels = c("Neonate", "Infant", "Child", "Adolescent", "Adult", "Elderly"))
    
    # joining by remaining terms so you can assign the right colours to the legend
    colours_df <- data.frame(
      AGE_GROUP_CLEAN = c("Neonate", "Infant", "Child", "Adolescent", "Adult", "Elderly"),
      colours = google_colors[1:6],
      stringsAsFactors = FALSE) %>%
      semi_join(age_groups, by = "AGE_GROUP_CLEAN")
    
    hist <- ggplot(age_groups, aes(x = AGE_Y, fill = AGE_GROUP_CLEAN)) +
      geom_histogram(breaks = seq(0, 100, by = 1)) +
      scale_fill_manual(values = colours_df$colours) +
      xlab("Age at onset (years)") +
      ylab("Number of Reports") +
      theme_bw()
    ggplotly(hist)
  })
  
# Drug Tab Plots --------------------------------------------------------------
  
  output$indication_plot <- renderGvis({
    # Data frame used to obtain Top_25_indication bar chart: Indication is only associated with individual drug
    # When brand name is unspecified, chart shows top 25 indications associated with all drugs + date_range
    # When brand name is specified, chart shows top 25 indications associated with specified drug + date_range
    indications_sorted <- subset_cv$drug %>%
      count(INDICATION_NAME_ENG) %>%
      arrange(desc(n)) %>%
      as.data.frame() %>%
      filter(!is.na(INDICATION_NAME_ENG)) %>%
      head(25)
    
    # NOTE ABOUT INDICATIONS STRUCTURE:
    # REPORT_ID -> multiple drugs per report
    # DRUG_ID -> multiple reports may use the same drugs
    # REPORT_DRUG_ID -> unique for each drug/report combination. count is less than total reports since drugs can have multiple indications
    # so distinct REPORT_DRUG_ID x INDICATION_NAME_ENG includes the entire set of reports
    
    if (input$drug.indication_plot.format == "drug.bar") {
      gvisBarChart_HCSC(indications_sorted, "INDICATION_NAME_ENG", "n", google_colors[1])
    } else {
      gvisTable(indications_sorted)
    }
  })
  
  output$suspect_drugs <- renderGvis({
    # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name
    #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq)
    data <- subset_cv$drug %>%
      filter(DRUGINVOLV_ENG == "Suspect") %>%
      distinct(REPORT_ID, DRUGNAME) %>%
      count(DRUGNAME) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
    
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    if (input$drug.suspect.format == "drug.bar") {
      gvisBarChart_HCSC(data, "DRUGNAME", "n", google_colors[2])
    } else {
      gvisTable(data)
    }
  })
  
  output$concomitant_drugs <- renderGvis({
    # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name
    #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq)
    data <- subset_cv$drug %>%
      filter(DRUGINVOLV_ENG == "Concomitant") %>%
      distinct(REPORT_ID, DRUGNAME) %>%
      count(DRUGNAME) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
    
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    if (input$drug.concomitant.format == "drug.bar") {
      gvisBarChart_HCSC(data, "DRUGNAME", "n", google_colors[2])
    } else {
      gvisTable(data)
    }
  })
  
  output$all_drugs <- renderGvis({
    # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name
    #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq)
    data <- subset_cv$drug %>%
      distinct(REPORT_ID, DRUGNAME) %>%
      count(DRUGNAME) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
    
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    if (input$drug.all.format == "drug.bar") {
      gvisBarChart_HCSC(data, "DRUGNAME", "n", google_colors[2])
    } else {
      gvisTable(data)
    }
  })
  
  output$drugcounttitle <- renderUI({
    excluded_count <- subset_cv$drug %>%
      count(REPORT_ID) %>%
      filter(n > 20) %>%
      count() %>%
      `$`('nn')
    HTML(paste0("<h3>Number of Drugs per Report ",
                tipify(
                  el = icon("info-circle"), trigger = "hover click",
                  title = paste0(
                    "This plot indicates how many reports include how many drugs. ",
                    "The search query filters unique reports, which may have one or more drugs associated with them.")),
                "<br>(", excluded_count, " reports with more than 20 drugs excluded)", "</h3>"))
  })
  
  output$drugcount_plot <- renderGvis({
    data <- subset_cv$drug %>%
      count(REPORT_ID) %>%
      filter(n <= 20) %>%
      select(n) %>%
      as.data.frame()
    
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    gvisHistogram(data, options = list(
      legend = "{ position: 'none' }",
      height = 300,
      vAxis = "{title: 'Number of Reports'}",
      hAxis = "{title: 'Number of Drugs in Report'}",
      chartArea = "{top: 20, height: '75%', left: 80, width: '90%'}",
      histogram = "{ hideBucketItems: true, bucketSize: 1 }"))
  })
# Reactions Tab Plots ---------------------------------------------------------
  output$top_pt <- renderGvis({
    data <- subset_cv$rxn %>%
      count(PT_NAME_ENG) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
    
    if (input$rxn.top_pt.format == "rxn.bar") {
      gvisBarChart_HCSC(data, "PT_NAME_ENG", "n", google_colors[1])
    } else {
      gvisTable(data)
    }
  })
  output$top_hlt <- renderGvis({
    data <- subset_cv$rxn %>%
      filter(!is.na(HLT_Term)) %>%
      count(HLT_Term) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
    
    if (input$rxn.top_hlt.format == "rxn.bar") {
      gvisBarChart_HCSC(data, "HLT_Term", "n", google_colors[2])
    } else {
      gvisTable(data)
    }
    
  })
  output$outcomeplot <- renderGvis({
    data <- subset_cv$report %>%
      count(OUTCOME_ENG) %>%
      as.data.frame()
    
    if (input$rxn.outcomeplot.format == "rxn.pie") {
      gvisPieChart_HCSC(data, "OUTCOME_ENG", "n")
    } else {
      gvisTable(data)
    }
    
  })
  
# Download Functions ----------------------------------------------------------
  output$download_reports <- downloadHandler(
    filename = function() {
      current_rxn <- paste0(current_search$rxn, collapse = "+")
      if (current_rxn == "") current_rxn <- "all"
      current_drug <- paste0(current_search$name, collapse = "+")
      if (current_drug == "") current_drug <- "all"
      current_drug <- gsub(" ", "_", current_drug)
      paste0(cv_download_reports()$data_type, '_', current_drug, '_', current_rxn, '.csv')
    },
    content = function(file){
      write.csv(cv_download_reports()$reports_tab_master,
                file,
                fileEncoding = "UTF-8",
                row.names = FALSE)
    }
  )
  
  observeEvent(input$clear_search, {
    reset("search_form")
  })
}


shinyApp(ui, server)
