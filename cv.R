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
cv_reports <- tbl(hcopen, "cv_reports")
cv_drug_product_ingredients <-  tbl(hcopen, "cv_drug_product_ingredients")
cv_report_drug <- tbl(hcopen, "cv_report_drug")
cv_reactions <- tbl(hcopen, "cv_reactions")
cv_report_drug_indication <- tbl(hcopen, "cv_report_drug_indication")
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
  sort()
topings <- cv_substances %>%
  distinct(ing) %>%
  as.data.frame() %>%
  `[[`(1) %>%
  sort()

ui <- dashboardPage(
  dashboardHeader(title = titleWarning("CV Shiny (v0.11)"),
                  titleWidth = 700),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Reports", tabName = "reportdata", icon = icon("hospital-o")),
      menuItem("Patients", tabName = "patientdata", icon = icon("user-md")),
      menuItem("Drugs", tabName = "drugdata", icon = icon("flask")),
      menuItem("Reactions", tabName = "rxndata", icon = icon("heart-o")),
      menuItem("Download", tabName = "downloaddata", icon = icon("download")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"))
    ),
    conditionalPanel(
      condition = "input.name_type == 'brand'",
      selectizeInput("search_brand", 
                     "Brand Name",
                     c("Start typing to search..." = "", topbrands))),
    conditionalPanel(
      condition = "input.name_type == 'ing'",
      selectizeInput("search_ing", 
                     "Active Ingredient",
                     c("Start typing to search..." = "", topings))),
    radioButtons("name_type", "Drug name type:",
                 c("Brand Name" = "brand",
                   "Active Ingredient" = "ing"),
                 selected = "ing"),
    selectizeInput("search_rxn", 
                   "Adverse Event Term",
                   c("Loading..." = "")),
    dateRangeInput("searchDateRange",
                   "Date Range",
                   start = "1965-01-01",
                   end = Sys.Date(),
                   startview = "year",
                   format = "yyyy-mm-dd"),
    # hacky way to get borders correct
    tags$div(
      class="form-group shiny-input-container",
      actionButton("searchButton",
                   "Search",
                   width = '100%')
    ),
    tags$br(),
    tags$h3(strong("Current Query:")),
    tableOutput("current_search")
    # downloadButton(outputId = "hlt_data_dl",
    #                label = "Export data")
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
                box(h3("Reporter"),
                    htmlOutput("reporterplot"),
                    "Qualification of the person who filed the report.",
                    width = 4),
                box(h3("Serious reports"),
                    htmlOutput("seriousplot"),
                    "Reports marked as serious.",
                    width = 4),
                box(h3("Reasons for serious reports"),
                    htmlOutput("seriousreasonsplot"),
                    "Total sums to more than 100% because reports can be marked serious for multiple reasons.",
                    width = 4)
              )
      ),
      tabItem(tabName = "patientdata",
              fluidRow(
                box(h3("Gender"),
                    htmlOutput("sexplot"),
                    p("Unknown includes reports explicitly marked unknown and Not Specified includes reports with no gender information."),
                    width = 3),
                box(h3("Age Groups"),
                    htmlOutput("agegroupplot"),
                    p("Unknown includes reports with no age information."), 
                    width = 3),
                box(h3("Age Histogram"),
                    plotlyOutput("agehist"),
                    width = 6)
              )
      ),
      tabItem(tabName = "drugdata",
              fluidRow(
                box(h3("Most Frequent Indications"),
                    htmlOutput("indication_plot"),
                    p("This plot includes all indications for all drugs present in the matching reports.
                       The search query filters unique reports, which may have one or more drugs associated with them."),
                    width = 6),
                box(h3("Most Frequently Occurring Drugs (Brand Name)"),
                    htmlOutput("drug_plot"),
                    p("This plot includes all drugs present in the matching reports.
                       The search query filters unique reports, which may have one or more drugs associated with them."),
                    width = 6)
              )
      ),
      tabItem(tabName = "rxndata",
              fluidRow(
                box(h3("Most Frequent Adverse Events (Preferred Terms)"),
                    htmlOutput("top_pt"),
                    p("For more rigorous analysis, use disproportionality statistics."),
                    width = 6),
                box(h3("Most Frequent Adverse Events (High-Level Terms)"),
                    htmlOutput("top_hlt"),
                    p("For more rigorous analysis, use disproportionality statistics."),
                    width = 6)
              ),
              fluidRow(
                box(h3("Outcomes of Adverse Events"),
                    htmlOutput("outcomeplot"),
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
                "<p>",
                "Data provided by the Canada Vigilance Adverse Reaction Online Database. The recency of the data is therefore ",
                "dependent on when the data source is updated, and is the responsibility of the Canada Vigilance Program. ",
                "For more information, please refer to ",
                "<a href = \"http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php\">",
                "http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/index-eng.php",
                "</a>.",
                "</p>")),
              aboutAuthors()
              )
      )
    )
  ),
  skin = "blue"
)


############### Server Functions ###################
server <- function(input, output, session) {
  # Relabel PT dropdown menu based on selected name
  observe({
    input$search_brand
    input$search_ing
    input$name_type
    isolate({
      pt_selected <- input$search_rxn
      
      pt_choices <- cv_reactions
      
      if (input$name_type == "brand" & input$search_brand != "") {
        related_reports <- cv_report_drug %>% filter(DRUGNAME == input$search_brand)
        pt_choices %<>% semi_join(related_reports, by = "REPORT_ID")
        
      } else if (input$name_type == "ing" & input$search_ing != "") {
        related_drugs <- cv_substances %>% filter(ing == input$search_ing) %>% distinct(DRUGNAME)
        related_reports <- cv_report_drug %>% semi_join(related_drugs, by = "DRUGNAME")
        pt_choices %<>% semi_join(related_reports, by = "REPORT_ID")
      }
      pt_choices %<>%
        distinct(PT_NAME_ENG) %>%
        as.data.frame() %>%
        `[[`(1) %>%
        sort()
      
      if (! pt_selected %in% pt_choices) pt_selected = ""
      updateSelectizeInput(session, "search_rxn",
                           choices = c("Start typing to search..." = "", pt_choices),
                           selected = pt_selected)
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
        rxn        = input$search_rxn,
        date_range = input$searchDateRange
      )
    })})
  cv_master_tab_tbl <- reactive({
    data <- current_search()
    
    cv_reports_filtered <- cv_reports %>%
      filter(DATINTRECEIVED_CLEAN >= data$date_range[1], DATINTRECEIVED_CLEAN <= data$date_range[2])
    cv_report_drug_filtered <- cv_report_drug
    if (data$name_type == "brand" & data$name != "") {
      cv_report_drug_filtered %<>% filter(DRUGNAME == data$name)
    } else if (data$name_type == "ing" & data$name != "") {
      related_drugs <- cv_substances %>% filter(ing == data$name)
      cv_report_drug_filtered %<>% semi_join(related_drugs, by = "DRUGNAME")
    }
    cv_reactions_filtered <- cv_reactions
    if ("" != data$rxn) cv_reactions_filtered %<>% filter(PT_NAME_ENG == data$rxn)
    
    tab_master <-  cv_reports_filtered %>%
      semi_join(cv_report_drug_filtered, by = "REPORT_ID") %>%
      semi_join(cv_reactions_filtered, by = "REPORT_ID")
  })
  cv_master_tab <- reactive({cv_master_tab_tbl() %>% as.data.frame()})
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
  ages <- reactive({
    data <- cv_master_tab() %>%
      dplyr::select(REPORT_ID, AGE_UNIT_ENG, AGE_Y) %>%
      filter(AGE_UNIT_ENG != "", !is.na(AGE_Y))
    
    # Age groups frequency table
    age_decade <- data %>% 
      filter(AGE_UNIT_ENG == "Decade") %>%
      mutate(AGE_Y = AGE_Y * 10)
    age_years <- data %>% 
      filter(AGE_UNIT_ENG == "Years")
    age_months <- data %>% 
      filter(AGE_UNIT_ENG == "Months") %>%
      mutate(AGE_Y = AGE_Y / 12)
    age_weeks <- data %>% 
      filter(AGE_UNIT_ENG == "Weeks") %>%
      mutate(AGE_Y = AGE_Y / 52)
    age_days <- data %>% 
      filter(AGE_UNIT_ENG == "Days") %>%
      mutate(AGE_Y = AGE_Y / 365)
    age_hours <- data %>% 
      filter(AGE_UNIT_ENG == "Hours") %>%
      mutate(AGE_Y = AGE_Y / (365*24))
    age_minutes <- data %>% 
      filter(AGE_UNIT_ENG == "Minutes") %>%
      mutate(AGE_Y = AGE_Y / (365*24*60))
    age_unknown <- cv_master_tab() %>%
      dplyr::select(REPORT_ID, AGE_UNIT_ENG, AGE_Y) %>%
      filter(AGE_UNIT_ENG == "" | is.na(AGE_Y)) %>%
      mutate(AGE_Y = NA)
    ages <- bind_rows(age_decade,
                      age_years,
                      age_months,
                      age_weeks,
                      age_days,
                      age_hours,
                      age_minutes,
                      age_unknown) %>%
      group_by(AGE_Y) %>%
      summarise(count = n()) %>%
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
    nreports <- cv_master_tab() %>%
      distinct(REPORT_ID) %>%
      nrow()
    drug_name <- current_search()$name
    
    title <- ifelse("" == drug_name, "All Drugs", drug_name)
    plottitle <- paste0("Drug Adverse Event Reports for ", title, " (", nreports, " reports)")
    h3(strong(plottitle))
  })
  output$timeplot <- renderGvis({
    # adrplot_test <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria"))
    data <- cv_master_tab() %>%
      mutate(month = floor_date(ymd(DATINTRECEIVED_CLEAN), "month")) %>%
      group_by(month)
    
    time_results <- data %>%
      summarise(total = n_distinct(REPORT_ID))
    serious_results <- data %>%
      filter(SERIOUSNESS_ENG == "Yes") %>%
      summarise(serious = n_distinct(REPORT_ID))
    death_results <- data %>%
      filter(DEATH == 1) %>%
      summarise(death = n_distinct(REPORT_ID))
    results <- time_results %>%
      left_join(serious_results, by = "month") %>%
      left_join(death_results, by = "month")
    results$serious[is.na(results$serious)] <- 0
    results$death[is.na(results$death)] <- 0
    
    colors_string <- google_colors[c(19, 14, 2)] %>%
      {paste0("'", ., "'")} %>%
      paste(collapse = ", ") %>%
      {paste0("[", ., "]")}
    gvisLineChart(results,
                  xvar = "month",
                  yvar = c("total", "serious", "death"),
                  options = list(
                    height = 350,
                    vAxis = "{title: 'Number of Reports'}",
                    hAxis = "{title: 'Month'}",
                    chartArea = "{top: 10, height: '80%', left: 120, width: '84%'}",
                    colors = colors_string
                  ))
  })
  
  ##### Data about Reports
  output$reporterplot <- renderGvis({
    reporter_results <- cv_master_tab() %>%
      group_by(REPORTER_TYPE_ENG) %>%
      summarise(count = n_distinct(REPORT_ID))
    reporter_results$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == ""] <- "Not Reported"
    # data <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
    gvisPieChart(reporter_results, 
                 labelvar = "REPORTER_TYPE_ENG",
                 numvar = "count", 
                 options = list(pieHole = 0.4))
  })
  output$seriousplot <- renderGvis({
    serious_results <- cv_master_tab() %>%
      group_by(SERIOUSNESS_ENG) %>%
      summarise(count = n_distinct(REPORT_ID))
    serious_results$SERIOUSNESS_ENG[serious_results$SERIOUSNESS_ENG == ""] <- "Not Reported"
    
    gvisPieChart(serious_results, 
                 labelvar = "SERIOUSNESS_ENG",
                 numvar = "count", 
                 options = list(pieHole = 0.4))
  })
  output$seriousreasonsplot <- renderGvis({
    data <- cv_master_tab() %>%
      filter(SERIOUSNESS_ENG == "Yes")
    
    total_serious <- data %>%
      distinct(REPORT_ID) %>%
      nrow()
    
    n_congen <- data %>%
      filter(CONGENITAL_ANOMALY == 1) %>%
      distinct(REPORT_ID) %>%
      nrow()
    n_death <- data %>%
      filter(DEATH == 1) %>%
      distinct(REPORT_ID) %>%
      nrow()
    n_disab <- data %>%
      filter(DISABILITY == 1) %>%
      distinct(REPORT_ID) %>%
      nrow()
    n_lifethreat <- data %>%
      filter(LIFE_THREATENING == 1) %>%
      distinct(REPORT_ID) %>%
      nrow()
    n_hosp <- data %>%
      filter(HOSP_REQUIRED == 1) %>%
      distinct(REPORT_ID) %>%
      nrow()
    n_other <- data %>%
      filter(OTHER_MEDICALLY_IMP_COND == 1) %>%
      distinct(REPORT_ID) %>%
      nrow()
    
    ## Check for NotSpecified ##
    n_notspec <- data %>%
      filter(DEATH != 1 | is.na(DEATH)) %>%
      filter(DISABILITY != 1 | is.na(DISABILITY)) %>%
      filter(CONGENITAL_ANOMALY != 1 | is.na(CONGENITAL_ANOMALY)) %>%
      filter(LIFE_THREATENING != 1 | is.na(LIFE_THREATENING)) %>%
      filter(HOSP_REQUIRED != 1 | is.na(HOSP_REQUIRED)) %>%
      filter(OTHER_MEDICALLY_IMP_COND != 1 | is.na(OTHER_MEDICALLY_IMP_COND)) %>%
      distinct(REPORT_ID) %>%
      nrow()
    
    serious_reasons <- data.frame(label = c("Congenital anomaly",
                                            "Death",
                                            "Disability",
                                            "Hospitalization",
                                            "Life-threatening",
                                            "Other medically important condition",
                                            "Not specified"),
                                  count = c(n_congen,
                                            n_death, 
                                            n_disab,
                                            n_lifethreat,
                                            n_hosp,
                                            n_other,
                                            n_notspec),
                                  stringsAsFactors = FALSE) %>%
      mutate(percentage = count/total_serious * 100) %>% 
      arrange(desc(percentage))
    serious_reasons$percentage %<>% round(digits = 2)
    
    gvisBarChart(serious_reasons,
                 xvar = "label",
                 yvar = "percentage",
                 options = list(
                   legend = "{position: 'none'}",
                   hAxis = "{title: 'Percentage'}",
                   chartArea = "{top: 0, height: '80%', left: 100, width: '60%'}",
                   bar = "{groupWidth: '90%'}"
                 )
    )
  })
  
  #### Data about Patients
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
  output$agegroupplot <- renderGvis({
    age_groups <- ages() %>%
      group_by(age_group) %>%
      summarise(count = sum(count))
    
    gvisPieChart(age_groups, 
                 labelvar = "age_group",
                 numvar = "count", 
                 options = list(height = 300,
                                pieHole = 0.3,
                                fontSize = 12,
                                sliceVisibilityThreshold = 0.0001)
                 )
  })
  output$agehist <- renderPlotly({
    age_groups <- ages() %>% filter(age_group != "Unknown", AGE_Y <= 130)
    unknown <- ages() %>% filter(age_group != "Unknown", AGE_Y > 130)
    unknown_count <- sum(unknown$count)
    
    plottitle <- paste0("Histogram of Patient Ages")
    if(unknown_count > 0) plottitle <- paste0(plottitle, "<br>(", unknown_count, " reports with age greater than 130 excluded)")
    
    hist <- ggplot(age_groups, aes(x = AGE_Y, weight = count, fill = age_group)) +
      geom_histogram() +
      ggtitle(plottitle) + 
      xlab("Age at onset (years)") + 
      ylab("Number of Reports") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, size = rel(0.85),face="bold")) +
      scale_x_continuous(limits = c(0, 120))
    #   theme(axis.title.x = element_text(size = rel(0.8)))
    ggplotly(hist)
  })
  
  #### Data about Drugs
  output$indication_plot <- renderGvis({
    # Data frame used to obtain Top_25_indication bar chart: Indication is only associated with individual drug
    # When brand name is unspecified, chart shows top 25 indications associated with all drugs + date_range
    # When brand name is specified, chart shows top 25 indications associated with specified drug + date_range
    
    indications_sorted <- cv_master_tab_tbl() %>%
      inner_join(cv_report_drug_indication, by = "REPORT_ID") %>%
      group_by(INDICATION_NAME_ENG) %>%
      summarise(count = n_distinct(REPORT_ID)) %>%
      # arrange(desc(count)) %>%
      top_n(n = 25, count) %>%
      as.data.frame() 
    
    # NOTE ABOUT INDICATIONS STRUCTURE:
    # REPORT_ID -> multiple drugs per report
    # DRUG_ID -> multiple reports may use the same drugs
    # REPORT_DRUG_ID -> unique for each drug/report combination. count is less than total reports since drugs can have multiple indications
    # so distinct REPORT_DRUG_ID x INDICATION_NAME_ENG includes the entire set of reports
    
    gvisBarChart_HCSC(indications_sorted, "INDICATION_NAME_ENG", "count", google_colors[1])
  })
  output$drug_plot <- renderGvis({
    # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name 
    #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq) 
    data <- cv_master_tab_tbl() %>% 
      inner_join(cv_report_drug, by = "REPORT_ID") %>%
      select(REPORT_ID, DRUGNAME) %>%
      group_by(DRUGNAME) %>%
      summarise(count = n_distinct(REPORT_ID)) %>%
      # arrange(desc(count)) %>%
      top_n(n = 25, count) %>%
      as.data.frame()
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    gvisBarChart_HCSC(data, "DRUGNAME", "count", google_colors[2])
  })
  
  #### Data about Reactions
  output$top_pt <- renderGvis({
    search <- current_search()
    
    data <- cv_reactions %>% select(REPORT_ID, PT_NAME_ENG)
    if ("" != search$name) {
      if (search$name_type == "brand") {
        cv_reports_filtered <- cv_report_drug %>% filter(DRUGNAME == search$name)
        data %<>% semi_join(cv_reports_filtered, by = "REPORT_ID")
      } else if (search$name_type == "ing") {
        related_drugs <- cv_substances %>% filter(ing == search$name) %>% distinct(DRUGNAME)
        cv_reports_filtered <- cv_report_drug %>% semi_join(related_drugs, by = "DRUGNAME")
        data %<>% semi_join(cv_reports_filtered, by = "REPORT_ID")
      }
    }
    data %<>%
      group_by(PT_NAME_ENG) %>%
      dplyr::summarise(count = n_distinct(REPORT_ID)) %>%
      top_n(25, count) %>%
      as.data.frame()
    gvisBarChart_HCSC(data, "PT_NAME_ENG", "count", google_colors[1])
  })
  output$top_hlt <- renderGvis({
    search <- current_search()
    data <- cv_reactions %>%
      dplyr::select(REPORT_ID, PT_NAME_ENG, MEDDRA_VERSION) %>%
      inner_join(meddra, by = c("PT_NAME_ENG" = "PT_Term", "MEDDRA_VERSION" = "Version"))
    if ("" != search$name) {
      if (search$name_type == "brand") {
        cv_reports_filtered <- cv_report_drug %>% filter(DRUGNAME == search$name)
        data %<>% semi_join(cv_reports_filtered, by = "REPORT_ID")
      } else if (search$name_type == "ing") {
        related_drugs <- cv_substances %>% filter(ing == search$name) %>% distinct(DRUGNAME)
        cv_reports_filtered <- cv_report_drug %>% semi_join(related_drugs, by = "DRUGNAME")
        data %<>% semi_join(cv_reports_filtered, by = "REPORT_ID")
      }
    }
    data %<>%
      group_by(HLT_Term) %>%
      dplyr::summarise(count = n_distinct(REPORT_ID)) %>%
      top_n(25, count) %>%
      as.data.frame()
    gvisBarChart_HCSC(data, "HLT_Term", "count", google_colors[2])
  })
  output$outcomeplot <- renderGvis({
    data <- cv_master_tab() %>%
      dplyr::select(REPORT_ID, OUTCOME_ENG)
    
    outcome_results <-  dplyr::summarise(group_by(data, OUTCOME_ENG),count=n_distinct(REPORT_ID))
    
    gvisPieChart(outcome_results, 
                 labelvar = "OUTCOME_ENG",
                 numvar = "n", 
                 options = list(pieHole = 0.4))
  })
  
  ############# Download Tab
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
  
}

shinyApp(ui, server)
