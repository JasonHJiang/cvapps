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
# cv_reports <- tbl(hcopen, "cv_reports")
# cv_drug_product_ingredients <-  tbl(hcopen, "cv_drug_product_ingredients")
# cv_report_drug <- tbl(hcopen, "cv_report_drug")
# cv_reactions <- tbl(hcopen, "cv_reactions")
# cv_report_drug_indication <- tbl(hcopen, "cv_report_drug_indication")
# cv_substances <- tbl(hcopen, "cv_substances")
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
topings <- cv_substances %>%
  distinct(ing) %>%
  as.data.frame() %>%
  `[[`(1) %>%
  sort()

ui <- dashboardPage(
  dashboardHeader(title = titleWarning("CV Shiny (v0.13)"),
                  titleWidth = 700),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Reports", tabName = "reportdata", icon = icon("hospital-o")),
      menuItem("Patients", tabName = "patientdata", icon = icon("user-md")),
      menuItem("Drugs", tabName = "drugdata", icon = icon("flask")),
      menuItem("Reactions", tabName = "rxndata", icon = icon("heart-o")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"))
    ),
    conditionalPanel(
      condition = "input.name_type == 'brand'",
      selectizeInput("search_brand", 
                     "Brand Name (Canadian Trade Name)",
                     c(topbrands, "Start typing to search..." = ""))),
    conditionalPanel(
      condition = "input.name_type == 'ingredient'",
      selectizeInput("search_ing", 
                     "Active Ingredient",
                     c(topings, "Start typing to search..." = ""))),
    div(style="display: inline-block; width: 47%;",
        radioButtons("name_type", "Drug name type:",
                     c("Brand Name" = "brand",
                       "Active Ingredient" = "ingredient"),
                     selected = "ingredient")),
    div(style="display: inline-block; vertical-align:top; width: 52%",
        radioButtons("drug_inv", "Drug Involvement:",
                     c("Suspect",
                       "Concomitant",
                       "Any"))),
    selectizeInput("search_rxn", 
                   "Preferred Term (PT)",
                   c("Loading..." = "")),
    dateRangeInput("searchDateRange",
                   "Date Range",
                   start = "1965-01-01",
                   end = "2016-06-30",
                   startview = "decade"),
    # hacky way to get borders correct
    conditionalPanel(
      condition = "input.search_rxn != 'disable'",
      tags$div(
        class="form-group shiny-input-container",
        actionButton("searchButton",
                     "Search",
                     width = '100%')
      )),
    div(style="display: inline-block; width: 161px;",
        selectInput("search_dataset_type",
                    "Download Type",
                    c("Report Data", "Drug Data", "Reaction Data"))),
    div(style="display: inline-block; vertical-align: bottom; height: 54px;",
        downloadButton(outputId = 'download_reports',
                       label = 'Download')),
    tableOutput("current_search")
  ), 
  
  dashboardBody(
    customCSS(),
    fluidRow(
      box(htmlOutput(outputId = "timeplot_title"),
          htmlOutput(outputId = "timeplot"),
          "Reports by month from Canada Vigilance Adverse Reaction Online Database.",
          width = 12
          )
      ),
    tabItems(
      tabItem(tabName = "reportdata",
              fluidRow(
                box(h3("Reporter Type",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste0(
                           "Indicates who reported the adverse reaction and their relationship to the patient. ",
                           "Slices may not be visible if they are too small.")
                       )),
                    htmlOutput("reporterplot"),
                    width = 3),
                box(h3("Seriousness",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste0(
                           "A serious report contains a serious adverse reaction, determined by the reporter ",
                           "of the report at the time of reporting. Slices may not be visible if they are too small.")
                       )),
                    htmlOutput("seriousplot"),
                    width = 3),
                box(h3("Reason(s) for Seriousness",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste0("The serious condition which the adverse event resulted in. Total may sum to",
                                        " more than the total number of reports because reports can be marked serious for multiple reasons"))),
                    htmlOutput("seriousreasonsplot"),
                    width = 5)
              )
      ),
      tabItem(tabName = "patientdata",
              fluidRow(
                box(h3("Gender",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste0(
                           "Gender of the patient as it was provided by the reporter. ",
                           "Where the gender is unknown, the reporter is unaware of the gender. ",
                           "Where the gender is not specified, the reporter did not specify the gender of the patient."))),
                    htmlOutput("sexplot"),
                    width = 3),
                box(h3("Age Group",
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
                    htmlOutput("agegroupplot"),
                    width = 3),
                box(htmlOutput("agehisttitle"),
                    plotlyOutput("agehist"),
                    width = 6)
              )
      ),
      tabItem(tabName = "drugdata",
              fluidRow(
                box(h3("Most Frequent Indications",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste(
                           "Indication refers to the particular condition for which a health product was taken. ",
                           "This plot includes all indications for all drugs present in the matching reports. ",
                           "The search query filters unique reports, which may have one or more drugs associated with them."))),
                    htmlOutput("indication_plot"),
                    width = 6),
                tabBox(
                  tabPanel("Suspect",
                           h3("Most Frequently Occurring Drugs (Brand Name)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "This plot includes all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them. ",
                                  "The reporter suspects that the health product caused the adverse reaction."))),
                           htmlOutput("suspect_drugs")),
                  tabPanel("Concomitant",
                           h3("Most Frequently Occurring Drugs (Brand Name)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "This plot includes all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them. ",
                                  "The health product is not suspected, but the patient was taking it at the time of the adverse reaction."))),
                           htmlOutput("concomitant_drugs")),
                  tabPanel("All",
                           h3("Most Frequently Occurring Drugs (Brand Name)",
                              tipify(
                                el = icon("info-circle"), trigger = "hover click",
                                title = paste0(
                                  "This plot includes all drugs present in the matching reports. ",
                                  "The search query filters unique reports, which may have one or more drugs associated with them."))),
                           htmlOutput("all_drugs")),
                  width = 6)
              ),
              fluidRow(
                box(htmlOutput("drugcounttitle"),
                    htmlOutput("drugcount_plot"),
                    width = 8)
              )
      ),
      tabItem(tabName = "rxndata",
              fluidRow(
                box(h3("Most Frequent Adverse Events (Preferred Terms)",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste0(
                           "MedDRA Preferred Term is a distinct descriptor (single medical concept) for a symptom, ",
                           "sign, disease, diagnosis, therapeutic indication, investigation, surgical, or medical ",
                           "procedure, and medical, social, or family history characteristic. For more rigorous analysis, ",
                           "use disproportionality statistics."))),
                    htmlOutput("top_pt"),
                    width = 6),
                box(h3("Most Frequent Adverse Events (High-Level Terms)",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = "For more rigorous analysis, use disproportionality statistics.")),
                    htmlOutput("top_hlt"),
                    width = 6)
              ),
              fluidRow(
                box(h3("Report Outcome",
                       tipify(
                         el = icon("info-circle"), trigger = "hover click",
                         title = paste0(
                           "The report outcome represents the outcome of the reported case as described by the reporter ",
                           "at the time of reporting and does not infer a causal relationship. The report outcome is not ",
                           "based on a scientific evaluation by Health Canada."))),
                    htmlOutput("outcomeplot"),
                    width = 4)
              )
      ),
      
      tabItem(tabName = "aboutinfo",
              box(
              width = 12,
              h2("About"),
              # using tags$p() and tags$a() inserts spaces between text and hyperlink...thanks R
              HTML(paste0(
                "<p>",
                "This is a beta product. DO NOT use as sole evidence to support regulatory decisions or to make decisions regarding ",
                "medical care. Always speak to your health care provider about the risks and benefits of Health Canada regulated Products.",
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
              aboutAuthors()
              )
      )
    )
  )
)

############### Server Functions ###################
server <- function(input, output, session) {
  # Relabel PT dropdown menu based on selected name
  observe({
    input$search_brand
    input$search_ing
    input$name_type
    input$drug_inv
    isolate({
      pt_selected <- input$search_rxn

      pt_choices <- cv_reactions

      related_reports <- cv_report_drug
      if (input$drug_inv != "Any") related_reports %<>% filter(DRUGINVOLV_ENG == input$drug_inv)
      if (input$name_type == "brand" & input$search_brand != "") {
        related_reports %<>% filter(DRUGNAME == input$search_brand)

      } else if (input$name_type == "ingredient" & input$search_ing != "") {
        related_drugs <- cv_substances %>% filter(ing == input$search_ing) %>% distinct(DRUGNAME)
        related_reports %<>% semi_join(related_drugs, by = "DRUGNAME")
      }
      
      pt_choices <- pt_choices %>%
        semi_join(related_reports, by = "REPORT_ID") %>%
        distinct(PT_NAME_ENG) %>%
        as.data.frame()
      if (nrow(pt_choices) == 0) {
        updateSelectizeInput(session, "search_rxn",
                             choices = c("No reactions for this selection!" = "disable"),
                             selected = "disable")
      } else {
        pt_choices %<>% `[[`(1) %>% sort()
        if (! pt_selected %in% pt_choices) pt_selected = ""
        updateSelectizeInput(session, "search_rxn",
                             choices = c("Start typing to search..." = "", pt_choices),
                             selected = pt_selected)
      }

    })
  })


  ##### Reactive data processing
  # Data structure to store current query info
  current_search <- reactive({
    input$searchButton
    isolate({
      if (input$name_type == "brand") {
        name <- input$search_brand
      } else {
        name <- input$search_ing
      }
      list(
        name_type  = input$name_type,
        name       = name,
        drug_inv   = input$drug_inv,
        rxn        = input$search_rxn,
        date_range = input$searchDateRange
      )
    })})
  subset_cv <- reactive({
    data <- current_search()

    cv_reports_filtered <- cv_reports %>%
      filter(DATINTRECEIVED_CLEAN >= data$date_range[1], DATINTRECEIVED_CLEAN <= data$date_range[2]) %>%
      select(REPORT_ID)
    cv_report_drug_filtered <- cv_report_drug
    if (data$name_type == "brand" & data$name != "") {
      cv_report_drug_filtered %<>% filter(DRUGNAME == data$name)
    } else if (data$name_type == "ingredient" & data$name != "") {
      related_drugs <- cv_substances %>% filter(ing == data$name)
      cv_report_drug_filtered %<>% semi_join(related_drugs, by = "DRUGNAME")
    }
    if (data$drug_inv != "Any") cv_report_drug_filtered %<>% filter(DRUGINVOLV_ENG == data$drug_inv)
    cv_reactions_filtered <- cv_reactions %>% filter(PT_NAME_ENG != "")
    if ("" != data$rxn) cv_reactions_filtered %<>% filter(PT_NAME_ENG == data$rxn)

    selected_ids <-  cv_reports_filtered %>%
      semi_join(cv_report_drug_filtered, by = "REPORT_ID") %>%
      semi_join(cv_reactions_filtered, by = "REPORT_ID")

    # so then all data is polled upon search, not just when display corresponding plot
    report_subset <- cv_reports %>%
      semi_join(selected_ids, by = "REPORT_ID") %>% as.data.frame()
    drug_subset <- cv_report_drug %>%
      semi_join(selected_ids, by = "REPORT_ID") %>%
      left_join(cv_report_drug_indication, by = c("REPORT_DRUG_ID", "REPORT_ID", "DRUG_PRODUCT_ID", "DRUGNAME")) %>% as.data.frame()
    rxn_subset <- cv_reactions %>%
      semi_join(selected_ids, by = "REPORT_ID") %>%
      left_join(meddra, by = c("PT_NAME_ENG" = "PT_Term", "MEDDRA_VERSION" = "Version")) %>% as.data.frame()

    list(report = report_subset,
         drug_tbl = drug_subset,
         rxn_tbl = rxn_subset)
  })
  cv_download_reports <- reactive({
    data_type <- ""

    if(input$search_dataset_type == "Report Data"){
      reports_tab_master <- subset_cv()$report
      data_type <- "report"
    } else if(input$search_dataset_type == "Drug Data"){
      reports_tab_master <- subset_cv()$drug
      data_type <- "drug"
    } else if(input$search_dataset_type == "Reaction Data"){
      reports_tab_master <- subset_cv()$rxn
      data_type <- "rxn"
    }
    reports_tab_master %<>% as.data.frame()

    # reports_tab_master_size <- paste("Size of Dataset is",
    #                                  format(object.size(reports_tab_master),
    #                                         units = "auto"))

    return(list(
      reports_tab_master = reports_tab_master,
      data_type = data_type
    ))
  })
  ages <- reactive({
    data <- subset_cv()$report %>%
      filter(AGE_UNIT_ENG != "", !is.na(AGE_Y))

    # Age groups frequency table
    age_decade <- data %>%
      filter(AGE_UNIT_ENG == "Decade") %>%
      mutate(AGE_Y = AGE_Y * 10) %>%
      count(AGE_Y) %>%
      as.data.frame()
    age_years <- data %>%
      filter(AGE_UNIT_ENG == "Years") %>%
      count(AGE_Y) %>%
      as.data.frame()
    age_months <- data %>%
      filter(AGE_UNIT_ENG == "Months") %>%
      mutate(AGE_Y = AGE_Y / 12) %>%
      count(AGE_Y) %>%
      as.data.frame()
    age_weeks <- data %>%
      filter(AGE_UNIT_ENG == "Weeks") %>%
      mutate(AGE_Y = AGE_Y / 52) %>%
      count(AGE_Y) %>%
      as.data.frame()
    age_days <- data %>%
      filter(AGE_UNIT_ENG == "Days") %>%
      mutate(AGE_Y = AGE_Y / 365) %>%
      count(AGE_Y) %>%
      as.data.frame()
    age_hours <- data %>%
      filter(AGE_UNIT_ENG == "Hours") %>%
      mutate(AGE_Y = AGE_Y / (365*24)) %>%
      count(AGE_Y) %>%
      as.data.frame()
    age_minutes <- data %>%
      filter(AGE_UNIT_ENG == "Minutes") %>%
      mutate(AGE_Y = AGE_Y / (365*24*60)) %>%
      count(AGE_Y) %>%
      as.data.frame()
    age_unknown <- subset_cv()$report %>%
      filter(AGE_UNIT_ENG == "" | is.na(AGE_Y)) %>%
      tally() %>%
      as.data.frame() %>%
      cbind("AGE_Y" = NA, .)
    ages <- bind_rows(age_decade,
                      age_years,
                      age_months,
                      age_weeks,
                      age_days,
                      age_hours,
                      age_minutes,
                      age_unknown) %>%
      count(AGE_Y, wt = n) %>%
      mutate(age_group = NA,
             age_group = ifelse(AGE_Y <= 25/365, "Neonate", age_group),
             age_group = ifelse(AGE_Y > 25/365 & AGE_Y < 1, "Infant", age_group),
             age_group = ifelse(AGE_Y >= 1 & AGE_Y < 13, "Child", age_group),
             age_group = ifelse(AGE_Y >= 13 & AGE_Y < 18, "Adolescent", age_group),
             age_group = ifelse(AGE_Y >= 18 & AGE_Y <= 65, "Adult", age_group),
             age_group = ifelse(AGE_Y > 65, "Elderly", age_group),
             age_group = ifelse(is.na(AGE_Y), "Unknown", age_group))
  })

  
  ##### Output ####
  ##### Construct Current_Query_Table for generic name, brand name, adverse reaction term & date range searched
  output$current_search <- renderTable({
    data <- current_search()
    result <- data.frame(names = c("Name Type:",
                                   "Name:",
                                   "Adverse Reaction Term:",
                                   "Date Range:"),
                         values = c(data$name_type %>% toupper(),
                                    data$name,
                                    data$rxn,
                                    paste(data$date_range, collapse = " to ")),
                         stringsAsFactors = FALSE)
    result$values["" == result$values] <- "Not Specified"
    result
  },
  include.colnames = FALSE
  )
  
  ##### Create time plot  ###
  output$timeplot_title <- renderUI({
    nreports <- subset_cv()$report %>%
      distinct(REPORT_ID) %>%
      tally() %>%
      as.data.frame()
    drug_name <- current_search()$name
    rxn_name <- current_search()$rxn

    if ("" == drug_name) drug_name <- "All Drugs"
    if ("" == rxn_name) rxn_name <- "All Reactions"
    plottitle <- paste0("Drug Adverse Event Reports for ", drug_name, " and ", rxn_name, " (", nreports, " reports)")
    h3(strong(plottitle))
  })
  output$timeplot <- renderGvis({
    # adrplot_test <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria"))
    data <- subset_cv()$report

    time_results <- data %>%
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
    results <- time_results %>%
      left_join(serious_results, by = "month") %>%
      left_join(death_results, by = "month")
    results$serious[is.na(results$serious)] <- 0
    results$death[is.na(results$death)] <- 0

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
  
  ##### Data about Reports
  output$reporterplot <- renderGvis({
    reporter_results <- subset_cv()$report %>%
      count(REPORTER_TYPE_ENG) %>%
      as.data.frame()
    reporter_results$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == ""] <- "Not reported"
    reporter_results$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == "Consumer Or Other Non Health Professional"] <- "Consumer or non-health professional"
    reporter_results$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == "Other Health Professional"] <- "Other health professional"
    # data <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))

    gvisPieChart_HCSC(reporter_results, "REPORTER_TYPE_ENG", "count")
  })
  output$seriousplot <- renderGvis({
    serious_results <- subset_cv()$report %>%
      count(SERIOUSNESS_ENG) %>%
      select(SERIOUSNESS_ENG, n) %>%
      as.data.frame() %>%
      mutate(label = NA,
             label = ifelse(SERIOUSNESS_ENG == "Yes", "Serious", label),
             label = ifelse(SERIOUSNESS_ENG == "No", "Non-serious", label),
             label = ifelse(SERIOUSNESS_ENG == "", "Not reported", label)) %>%
      select(label, n) %>%
      slice(match(c("Serious", "Non-serious", "Not reported"), label))
    
    gvisPieChart_HCSC(serious_results, "label", "count")
  })
  output$seriousreasonsplot <- renderGvis({
    data <- subset_cv()$report %>%
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
                                  stringsAsFactors = FALSE)

    gvisBarChart(serious_reasons,
                 xvar = "label",
                 yvar = "count",
                 options = list(
                   legend = "{position: 'none'}",
                   hAxis = "{title: 'Number of Reports'}",
                   chartArea = "{top: 0, height: '80%', left: 150, width: '60%'}",
                   bar = "{groupWidth: '90%'}",
                   colors = colorCodeToString(google_colors[5])
                 )
    )
  })
  
  ### Data about Patients
  output$sexplot <- renderGvis({
    data <- subset_cv()$report %>%
      count(GENDER_ENG) %>%
      as.data.frame()
    # replace blank in GENDER_ENG with character "Unknown"
    data$GENDER_ENG[data$GENDER_ENG == ""] <- "Not specified"
    sex_results <- count(data, GENDER_ENG, wt = n)

    gvisPieChart_HCSC(sex_results, "GENDER_ENG", "n")
  })
  output$agegroupplot <- renderGvis({
    age_groups <- ages() %>%
      count(age_group, wt = nn)
    age_group_order <- data.frame(age_group = c("Neonate",
                                                "Infant",
                                                "Child",
                                                "Adolescent",
                                                "Adult",
                                                "Elderly",
                                                "Unknown"),
                                  stringsAsFactors = FALSE)
    data <- left_join(age_group_order, age_groups, by = "age_group")
    data[is.na(data)] <- 0 # always including empty rows means colour-scheme will be consistent

    gvisPieChart_HCSC(data, "age_group", "n")
  })
  output$agehisttitle <- renderUI({
    excluded_count <- ages() %>%
      filter(age_group != "Unknown", AGE_Y > 100) %>%
      `$`('nn') %>% sum()
    HTML(paste0("<h3>Histogram of Patient Ages ",
                tipify(
                  el = icon("info-circle"), trigger = "hover click",
                  title = "Distribution of number of reports per age, colour-coded by age group. Each bin groups 2 years."),
                "<br>(", excluded_count, " reports with age greater than 100 excluded)", "</h3>"))
  })
  output$agehist <- renderPlotly({
    age_groups <- ages() %>% filter(age_group != "Unknown", AGE_Y <= 100)
    age_groups$age_group %<>% factor(levels = c("Neonate", "Infant", "Child", "Adolescent", "Adult", "Elderly"))
    
    # joining by remaining terms so you can assign the right colours to the legend
    colours_df <- data.frame(
      age_group = c("Neonate", "Infant", "Child", "Adolescent", "Adult", "Elderly"),
      colours = google_colors[1:6],
      stringsAsFactors = FALSE) %>%
      semi_join(distinct(age_groups, age_group), by = "age_group")
    
    hist <- ggplot(age_groups, aes(x = AGE_Y, weight = nn, fill = age_group)) +
      geom_histogram(breaks = seq(0, 100, by = 2)) +
      scale_fill_manual(values = colours_df$colours) +
      xlab("Age at onset (years)") +
      ylab("Number of Reports") +
      theme_bw()
    ggplotly(hist)
  })

  #### Data about Drugs
  output$indication_plot <- renderGvis({
    # Data frame used to obtain Top_25_indication bar chart: Indication is only associated with individual drug
    # When brand name is unspecified, chart shows top 25 indications associated with all drugs + date_range
    # When brand name is specified, chart shows top 25 indications associated with specified drug + date_range
    indications_sorted <- subset_cv()$drug_tbl %>%
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

    gvisBarChart_HCSC(indications_sorted, "INDICATION_NAME_ENG", "n", google_colors[1])
  })
  output$suspect_drugs <- renderGvis({
    # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name
    #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq)
    data <- subset_cv()$drug_tbl %>%
      filter(DRUGINVOLV_ENG == "Suspect") %>%
      distinct(REPORT_ID, DRUGNAME) %>%
      count(DRUGNAME) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
    
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    gvisBarChart_HCSC(data, "DRUGNAME", "n", google_colors[2])
  })
  output$concomitant_drugs <- renderGvis({
    # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name
    #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq)
    data <- subset_cv()$drug_tbl %>%
      filter(DRUGINVOLV_ENG == "Concomitant") %>%
      distinct(REPORT_ID, DRUGNAME) %>%
      count(DRUGNAME) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
    
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    gvisBarChart_HCSC(data, "DRUGNAME", "n", google_colors[2])
  })
  output$all_drugs <- renderGvis({
    # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name
    #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq)
    data <- subset_cv()$drug_tbl %>%
      distinct(REPORT_ID, DRUGNAME) %>%
      count(DRUGNAME) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
    
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    gvisBarChart_HCSC(data, "DRUGNAME", "n", google_colors[2])
  })
  output$drugcounttitle <- renderUI({
    excluded_count <- subset_cv()$drug_tbl %>%
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
    data <- subset_cv()$drug_tbl %>%
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

  #### Data about Reactions
  output$top_pt <- renderGvis({
    data <- subset_cv()$rxn_tbl %>%
      count(PT_NAME_ENG) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()

    gvisBarChart_HCSC(data, "PT_NAME_ENG", "n", google_colors[1])
  })
  output$top_hlt <- renderGvis({
    data <- subset_cv()$rxn_tbl %>%
      filter(!is.na(HLT_Term)) %>%
      count(HLT_Term) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()

    gvisBarChart_HCSC(data, "HLT_Term", "n", google_colors[2])
  })
  output$outcomeplot <- renderGvis({
    data <- subset_cv()$report %>%
      count(OUTCOME_ENG) %>%
      as.data.frame()

    gvisPieChart_HCSC(data, "OUTCOME_ENG", "n")
  })

  ############# Download Tab
  output$download_reports <- downloadHandler(
    filename = function() {
      current_rxn <- current_search()$rxn
      if (current_rxn == "") current_rxn <- "all"
      current_drug <- current_search()$name
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
  
}

shinyApp(ui, server)
