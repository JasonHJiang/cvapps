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

library(openfda)
source("common_ui.R")

####### for an overview of this code, try clicking the arrow at the side of the editor
#< so you understand what are all the high-level functions and outputs in the server fxn
# https://github.com/FDA/openfda/issues/29

#### Preamble ####
dbuijs_api_key <- "QPV3z0OxVhzLfp5EGZTLT6QZGcTRrtXpYebmAs4n"
# how to use
# topdrugs <- fda_query("/drug/event.json") %>% 
#   #fda_api_key(dbuijs_api_key) %>%
#   fda_count("patient.drug.openfda.generic_name.exact") %>% 
#   fda_limit(1000) %>% 
#   fda_exec()


topdrugs <- fda_query("/drug/event.json") %>%
  fda_count("patient.drug.openfda.generic_name.exact") %>% 
  fda_limit(1000) %>% 
  fda_exec()
# first two results are 0XYGEN and 3% DICLOFENAC SPRAY -> the 3% causes to glitches, 0xygen is bad typo
# https://github.com/FDA/openfda/issues/29
topdrugs <- c("Start typing to search..." = "", topdrugs$term %>% sort() %>% tail(-2))
topbrands <- fda_query("/drug/event.json") %>%
  fda_count("patient.drug.openfda.brand_name.exact") %>% 
  fda_limit(1000) %>% 
  fda_exec()
topbrands <- c("Start typing to search..." = "", topbrands$term %>% sort())
hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
meddra <- tbl(hcopen, "meddra") %>%
  filter(Primary_SOC_flag == "Y") %>%
  select(PT_Term, HLT_Term, Version = MEDDRA_VERSION) %>%
  mutate(term = toupper(PT_Term)) %>%
  as.data.frame()


reporter_code <- data.table(term = 1:5,
                            label = c("Physician",
                                      "Pharmacist",
                                      "Other Health Professional",
                                      "Lawyer",
                                      "Consumer or Non-Health Professional"))
outcome_code <- data.table(term = 1:6,
                           label = c("Recovererd/resolved",
                                     "Recovering/resolving",
                                     "Not recovered/not resolved",
                                     "Recovered/resolved with sequelae",
                                     "Fatal",
                                     "Unknown"))
sex_code <- data.table(term = 0:2,
                       label = c("Unknown",
                                 "Male",
                                 "Female"))
age_code <- data.table(term = 800:805,
                       label = c("Decade",
                                 "Year",
                                 "Month",
                                 "Week",
                                 "Day",
                                 "Hour"))


ui <- dashboardPage(
  dashboardHeader(title = titleWarning("Shiny FAERS (v0.11)"),
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
      condition = "input.name_type == 'generic'",
      selectizeInput("search_generic", 
                     "Generic Name", 
                     topdrugs)),
    conditionalPanel(
      condition = "input.name_type == 'brand'",
      selectizeInput("search_brand", 
                     "Brand Name (US Trade Name)",
                     topbrands)),
    radioButtons("name_type", "Drug name type:",
                 c("Generic" = "generic",
                   "Brand Name" = "brand")),
    selectizeInput("search_rxn", 
                   "Adverse Event Term",
                   c("Loading..." = "")),
    dateRangeInput("searchDateRange", 
                   "Date Range", 
                   start = "2000-01-01", 
                   startview = "year"),
    # hacky way to get borders correct
    tags$div(class="form-group shiny-input-container",
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
          "Trendline is a local non-parametric regression calculated with the LOESS model. ",
          "The shaded area is an approximation of the 95% confidence interval of the regression.",
          htmlOutput(outputId = "search_url"),
          width = 12
          )
      ),
    tabItems(
      tabItem(tabName = "reportdata",
              fluidRow(
                box(h3("Reporter"),
                    htmlOutput("reporterplot"),
                    p("Qualification of the person who filed the report."),
                    p("Unknown is the number of reports without the primarysource.qualification field."),
                    width = 3),
                box(h3("Serious reports"),
                    htmlOutput("seriousplot"),
                    p("Reports marked as serious."),
                    width = 3),
                box(h3("Reasons for serious reports"),
                    htmlOutput("seriousreasonsplot"),
                    p("Total sums to more than 100% because reports can be marked serious for multiple reasons."),
                    width = 3),
                box(h3("Country"),
                    htmlOutput("countryplot"),
                    p("Country the reaction(s) occurred in. This is not necessarily the same country the report was received from."),
                    width = 3)
              )
      ),
      tabItem(tabName = "patientdata",
              fluidRow(
                box(h3("Gender"),
                    htmlOutput("sexplot"),
                    p("Unknown includes both reports explicitly marked unknown and reports with no gender information."),
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
                           The open.fda.gov search API does not allow searching or filtering within drugs.
                           The search query filters unique reports, which may have one or more drugs associated with them.
                           It is not currently possible to search for only those indications associated with a specific drug.
                           "),
                    width = 6),
                box(h3("Most Frequently Occurring Drugs (Generic Name)"),
                    htmlOutput("drug_plot"),
                    p("This plot includes all drugs present in the matching reports.
                       The open.fda.gov search API does not allow searching or filtering within drugs.
                       The search query filters unique reports, which may have one or more drugs associated with them.
                       It is not currently possible to retrieve correlations between drugs."),
                    width = 6)
              ),
              fluidRow(
                box(h3("Most Frequent Established Pharmaceutical Classes"),
                    htmlOutput("drugclassplot"),
                    p("This plot includes all drug classes present in the matching reports, including the search term.
                       The total number of instances for each class will be geater 
                       than the number of reports when reports include more than one drug of the same class.
                       The open.fda.gov search API does not allow searching or filtering within drugs.
                       The search query filters unique reports, which may have one or more drugs associated with them.
                       It is not currently possible to retrieve correlations between drugs."), width = 6)
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
                    p("Based on the counts for the top 1000 adverse event preferred terms. ",
                      "For more rigorous analysis, use disproportionality statistics."),
                    width = 6)
              ),
              fluidRow(
                box(h3("Outcomes of Adverse Events"),
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
                "This is a beta product developed using Open FDA data. DO NOT use as sole evidence to support ",
                "regulatory decisions or making decisions regarding medical care.  We do not hold any responsibility ",
                "for the authenticity or legitimacy of data.",
                "</p>",
                "<p>",
                "This app has been developed by the Data Sciences Unit of RMOD at Health Canada. ",
                "This app is a prototype experiment that utilizes publically available data (Open FDA) ",
                "and presents it in an interactive way for enhanced visualizations. This app allows users to ",
                "effortlessly interact with the reports database, conduct searches and view results in highly ",
                "interactive dashboards. <b>NOTE</b>: This app uses US Trade Name and Generic Name definitions; ",
                "therefore it may not be relevant in Canadian Context.",
                "</p>",
                "<p>",
                "Data provided by the U.S. Food and Drug Administration (FDA), retrieved through the openFDA API (",
                "<a href = \"https://open.fda.gov\">",
                "https://open.fda.gov",
                "</a>",
                "). The recency of the data is therefore dependent on when the API data source is updated, ",
                "and Health Canada claims no responsibility for out-of-date information. For more information, please refer to ",
                "<a href = \"https://open.fda.gov/drug/event/reference/\">",
                "https://open.fda.gov/drug/event/reference/",
                "</a>.",
                "</p>")),
              aboutAuthors()
              )
      )
    )
  ), 
  skin = "blue"
)


server <- function(input, output, session) {
  # Relabel PT dropdown menu based on selected name
  observe({
    input$search_generic
    input$search_brand
    input$name_type
    isolate({
      pt_selected <- input$search_rxn
      
      if ((input$name_type == "generic" & input$search_generic == "") |
          (input$name_type == "brand" & input$search_brand == "")) {
        pt_choices <- fda_query("/drug/event.json") %>%
          fda_count("patient.reaction.reactionmeddrapt.exact") %>%
          fda_limit(1000) %>%
          fda_exec()
        pt_choices <- pt_choices$term
      }
      
      if (input$name_type == "generic" & input$search_generic != "") {
        query_str <- paste0('"', gsub(" ", "+", input$search_generic), '"')
        pt_choices <- fda_query("/drug/event.json") %>%
          fda_filter("patient.drug.openfda.generic_name.exact", query_str) %>%
          fda_count("patient.reaction.reactionmeddrapt.exact") %>%
          fda_limit(1000) %>%
          fda_exec()
        pt_choices <- pt_choices$term
      } else if (input$name_type == "brand" & input$search_brand != "") {
        query_str <- paste0('"', gsub(" ", "+", input$search_brand), '"')
        pt_choices <- fda_query("/drug/event.json") %>%
          fda_filter("patient.drug.openfda.brand_name.exact", query_str) %>%
          fda_count("patient.reaction.reactionmeddrapt.exact") %>%
          fda_limit(1000) %>%
          fda_exec()
        pt_choices <- pt_choices$term
      } 
      
      if (! pt_selected %in% pt_choices) pt_selected = ""
      updateSelectizeInput(session, "search_rxn",
                           choices = c("Start typing to search..." = "", pt_choices %>% sort()),
                           selected = pt_selected)
    })
  })
  
  
  
  ########## Reactive data processing
  # Data structure to store current query info
  current_search <- reactive({
    input$searchButton
    isolate({
      if (input$name_type == "generic") {
        name <- input$search_generic
      } else {
        name <- input$search_brand
      }
      
      list(
        name_type  = input$name_type,
        name       = name,
        rxn        = input$search_rxn,
        date_range = input$searchDateRange
      )
    })})
  faers_query <- reactive({
    data <- current_search()
    
    openfda_query <- fda_query("/drug/event.json")
    query_str <- paste0("[", data$date_range[1], "+TO+", data$date_range[2], "]")
    openfda_query %<>% fda_filter("receivedate", query_str)
    if("" != data$name) {
      query_str <- paste0('"', gsub(" ", "+", data$name), '"')
      if (data$name_type == "generic") {
        openfda_query %<>% fda_filter("patient.drug.openfda.generic_name.exact", query_str)
      } else {
        openfda_query %<>% fda_filter("patient.drug.openfda.brand_name.exact", query_str)
      }
    }
    if("" != data$rxn) {
      query_str <- paste0('"', gsub(" ", "+", data$rxn), '"')
      openfda_query %<>% fda_filter("patient.reaction.reactionmeddrapt.exact", query_str)
    }
    
    query_url <- openfda_query %>%
      fda_search() %>%
      fda_limit(100) %>%
      fda_url()
    
    total_reports <- openfda_query %>%
      fda_search() %>%
      fda_url() %>%
      fda_fetch() %>%
      .$meta %>%
      .$results %>%
      .$total
    
    list(
      query_url = query_url,
      total_reports = total_reports,
      openfda_query = openfda_query
    )
  })
  ages <- reactive({
    query <- faers_query()$openfda_query
    
    age_decades <- query %>% 
      fda_filter("patient.patientonsetageunit", "800") %>%
      fda_count("patient.patientonsetage") %>%
      fda_limit(1000) %>%
      fda_exec()
    if(is.null(age_decades)) age_decades <- data.table(term = numeric(), count = numeric())
    age_decades %<>% mutate(term = term*10)
    
    age_years <- query %>% 
      fda_filter("patient.patientonsetageunit", "801") %>%
      fda_count("patient.patientonsetage") %>%
      fda_limit(1000) %>%
      fda_exec()
    if(is.null(age_years)) age_years <- data.table(term = numeric(), count = numeric())
    
    age_months <- query %>% 
      fda_filter("patient.patientonsetageunit", "802") %>%
      fda_count("patient.patientonsetage") %>%
      fda_limit(1000) %>%
      fda_exec()
    if(is.null(age_months)) age_months <- data.table(term = numeric(), count = numeric())
    age_months %<>% mutate(term = term/12)
    
    age_weeks <- query %>% 
      fda_filter("patient.patientonsetageunit", "803") %>%
      fda_count("patient.patientonsetage") %>%
      fda_limit(1000) %>%
      fda_exec()
    if(is.null(age_weeks)) age_weeks <- data.table(term = numeric(), count = numeric())
    age_weeks %<>% mutate(term = term/52)
    
    age_days <- query %>% 
      fda_filter("patient.patientonsetageunit", "804") %>%
      fda_count("patient.patientonsetage") %>%
      fda_limit(1000) %>%
      fda_exec()
    if(is.null(age_days)) age_days <- data.table(term = numeric(), count = numeric())
    age_days %<>% mutate(term = term/365)
    
    age_hours <- query %>% 
      fda_filter("patient.patientonsetageunit", "805") %>%
      fda_count("patient.patientonsetage") %>%
      fda_limit(1000) %>%
      fda_exec()
    
    if(is.null(age_hours)) age_hours <- data.table(term = numeric(), count = numeric())
    age_hours %<>% mutate(term = term/(365*24))
    
    unknown <- query %>%
      fda_filter("_missing_", "patient.patientonsetage") %>%
      fda_url() %>%
      fda_fetch() %>%
      .$meta %>%
      .$results %>%
      .$total
    age_unknown <- data.frame(term = NA, count = unknown)
    
    ages <- bind_rows(age_decades,
                      age_years,
                      age_months,
                      age_weeks,
                      age_days,
                      age_hours,
                      age_unknown) %>%
      group_by(term) %>%
      summarise(count = sum(count)) %>%
      mutate(age_group = NA,
             age_group = ifelse(term <= 25/365, "Neonate", age_group),
             age_group = ifelse(term > 25/365 & term < 1, "Infant", age_group),
             age_group = ifelse(term >= 1 & term < 13, "Child", age_group),
             age_group = ifelse(term >= 13 & term < 18, "Adolescent", age_group),
             age_group = ifelse(term >= 18 & term <= 65, "Adult", age_group),
             age_group = ifelse(term > 65, "Elderly", age_group),
             age_group = ifelse(is.na(term), "Unknown", age_group))
  })
  
  ########## Output
  output$search_url <- renderUI({
    url <- faers_query()$query_url 
    HTML(paste0("Reports by month from US FDA FAERS (open.fda.gov). Search URL: <a href = ", url, ">", url, "</a>"))
  })
  output$current_search <- renderTable({
    data <- current_search()
    result <- data.table(names = c("Name Type:", 
                                   "Name:", 
                                   "Adverse Reaction Term:",
                                   "Date Range:"),
                         values = c(toupper(data$name_type),
                                    data$name,
                                    data$rxn,
                                    paste(data$date_range, collapse = " to ")))
    result$values["" == result$values] <- "Not Specified"
    result
  },
  include.colnames = FALSE)
  
  ### Create time plot
  output$timeplot_title <- renderUI({
    query <- faers_query()$openfda_query
    drug_name <- current_search()$name
    
    time_results <- query %>%
      fda_count("receivedate") %>%
      fda_limit(1000) %>%
      fda_exec()
    title <- drug_name
    if ("" == title) title <- "All Drugs"
    nreports <- sum(time_results$count)
    plottitle <- paste0("Drug Adverse Event Reports for ", title, " (", nreports, " reports)")
    h3(strong(plottitle))
  })
  output$timeplot <- renderGvis({
    query <- faers_query()$openfda_query
    
    time_results <- query %>%
      fda_count("receivedate") %>%
      fda_limit(1000) %>%
      fda_exec() %>%
      mutate(month = floor_date(ymd(time), "month")) %>%
      dplyr::count(month, wt = count) %>%
      rename(total = n)
    serious_results <- query %>%
      fda_filter("serious", "1") %>%
      fda_count("receivedate") %>%
      fda_limit(1000) %>%
      fda_exec() %>%
      mutate(month = floor_date(ymd(time), "month")) %>%
      dplyr::count(month, wt = count) %>%
      rename(serious = n)
    death_results <- query %>%
      fda_filter("seriousnessdeath", "1") %>%
      fda_count("receivedate") %>%
      fda_limit(1000) %>%
      fda_exec() %>%
      mutate(month = floor_date(ymd(time), "month")) %>%
      dplyr::count(month, wt = count) %>%
      rename(death = n)
    results <- time_results %>%
      left_join(serious_results, by = "month") %>%
      left_join(death_results, by = "month")
    results$serious[is.na(results$serious)] <- 0
    results$death[is.na(results$death)] <- 0
    
    # plot <- ggplot(results) +
    #   aes(x = month, y = total, color = "total") +
    #   geom_line() +
    #   stat_smooth(method = "loess", size = 0.1) +
    #   geom_line(aes(x = month, y = serious, color = "serious")) +
    #   geom_line(aes(x = month, y = death, color = "death")) +
    #   ggtitle(plottitle) + 
    #   xlab("Month") + 
    #   ylab("Number of Reports") +
    #   theme_bw() +
    #   theme(plot.title = element_text(lineheight=.8, face="bold"))
    # ggplotly(plot)
    colors <- google_colors[c(19, 14, 2)]
    colors_string <- colors %>%
      sapply(function(x) paste0("'", x, "'")) %>%
      paste(collapse = ", ") %>%
      {paste0("[", ., "]")}
    gvisLineChart(results,
                  xvar = "month",
                  yvar = c("total", "serious", "death"),
                  options = list(
                    height = 344,
                    vAxis = "{title: 'Number of Reports'}",
                    chartArea = "{top: 20, height: '80%', left: 100, width: '85%'}",
                    colors = colors_string
                  ))
  })
  
  ### Data about Reports
  output$reporterplot <- renderGvis({
    data <- faers_query()
    
    reporter_results <- data$openfda_query %>% 
      fda_count("primarysource.qualification") %>% 
      fda_exec() 
    if(is.null(reporter_results)) reporter_results <- data.table(term = numeric(), count = numeric())
    
    reporter_results <- reporter_results %>%
      left_join(reporter_code) %>%
      select(label, count)
    
    unknown <- data$openfda_query %>%
      fda_filter("_missing_", "primarysource.qualification") %>%
      fda_url() %>%
      fda_fetch() %>%
      .$meta %>%
      .$results %>%
      .$total
    
    if(!is.null(unknown)){ reporter_results <- rbind(reporter_results, c("Not Reported", unknown)) %>%
      mutate(count = as.numeric(count))
    }
    
    gvisPieChart(reporter_results, 
                 labelvar = "label",
                 numvar = "count", 
                 options = list(pieHole = 0.4))
  })
  output$seriousplot <- renderGvis({
    data <- faers_query()
    serious_results <- data$openfda_query %>% 
      fda_count("serious") %>% 
      fda_exec() 
    if(is.null(serious_results)) serious_results <- data.table(term = numeric(), count = numeric())
    
    serious_results <- serious_results %>%
      mutate(label = ifelse(term == 1, "Serious", "Non-serious")) %>%
      select(label, count)
    
    unknown <- data$openfda_query %>%
      fda_filter("_missing_", "serious") %>%
      fda_url() %>%
      fda_fetch() %>%
      .$meta %>%
      .$results %>%
      .$total
    
    if(!is.null(unknown)){
      serious_results <- rbind(serious_results, c("Not Reported", unknown)) %>%
      mutate(count = as.numeric(count))
    }
    
    
    gvisPieChart(serious_results, 
                 labelvar = "label",
                 numvar = "count", 
                 options = list(pieHole = 0.4))
  })
  output$seriousreasonsplot <- renderGvis({
    data <- faers_query()
    
    total_serious <- data$openfda_query %>% 
      fda_count("serious") %>% 
      fda_exec() %>%
      filter(term == "1") %>%
      .$count
    
    congenital_results <- data$openfda_query %>% 
      fda_count("seriousnesscongenitalanomali") %>% 
      fda_exec()
    if(is.null(congenital_results)) congenital_results <- data.table(term = character(), count = numeric())
    
    death_results <-  data$openfda_query %>% 
      fda_count("seriousnessdeath") %>% 
      fda_exec()
    if(is.null(death_results)) death_results <- data.table(term = character(), count = numeric())
    
    disabling_results <-  data$openfda_query %>% 
      fda_count("seriousnessdisabling") %>% 
      fda_exec()
    if(is.null(disabling_results)) disabling_results <- data.table(term = character(), count = numeric())
    
    hospital_results <-  data$openfda_query %>% 
      fda_count("seriousnesshospitalization") %>% 
      fda_exec()
    if(is.null(hospital_results)) hospital_results <- data.table(term = character(), count = numeric())
    
    lifethreaten_results <-  data$openfda_query %>% 
      fda_count("seriousnesslifethreatening") %>% 
      fda_exec()
    if(is.null(lifethreaten_results)) lifethreaten_results <- data.table(term = character(), count = numeric())
    
    serother_results <-  data$openfda_query %>% 
      fda_count("seriousnessother") %>% 
      fda_exec()
    if(is.null(serother_results)) serother_results <- data.table(term = character(), count = numeric())
    
    serious_reasons <- bind_rows("Congenital Anomaly" = congenital_results,
                                 "Death" = death_results,
                                 "Disabling" = disabling_results,
                                 "Hospitalization" = hospital_results,
                                 "Life-threatening" = lifethreaten_results,
                                 "Other" = serother_results,
                                 .id = "label") %>%
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
  output$countryplot <- renderGvis({
    data <- faers_query()
    
    country_results <- data$openfda_query %>% 
      fda_count("occurcountry") %>% 
      fda_exec() 
    if(is.null(country_results)) country_results <- data.table(term = character(), count = numeric())
    
    unknown <- data$openfda_query %>%
      fda_filter("_missing_", "occurcountry") %>%
      fda_url() %>%
      fda_fetch() %>%
      .$meta %>%
      .$results %>%
      .$total
    
    if(!is.null(unknown)){ country_results <- rbind(country_results, c("Unknown", unknown)) %>%
      mutate(count = as.numeric(count))
    }
    
    gvisPieChart(country_results, 
                 labelvar = "term",
                 numvar = "count", 
                 options = list(pieHole = 0.4))
  })
  
  ### Data about Patients
  output$sexplot <- renderGvis({
    data <- faers_query()
    
    sex_results <- data$openfda_query %>% 
      fda_count("patient.patientsex") %>% 
      fda_exec() %>%
      inner_join(sex_code, by = "term")
    if(is.null(sex_results)) sex_results <- data.table(term = numeric(), count = numeric())
    
    unknown <- data$openfda_query %>%
      fda_filter("_missing_", "patient.patientsex") %>%
      fda_url() %>%
      fda_fetch() %>%
      .$meta %>%
      .$results %>%
      .$total
    if (!is.null(unknown)) sex_results %<>% rbind(c(3, unknown, "Not Reported"))
    sex_results %<>% select(label, count) %>% mutate(count = as.numeric(count))
    
    gvisPieChart(sex_results,
                 labelvar = "label",
                 numvar = "count",
                 options = list(pieHole = 0.4))
  })
  output$agegroupplot <- renderGvis({
    age_groups <- ages() %>%
      group_by(age_group) %>%
      summarise(count = sum(count))
    
    gvisPieChart(age_groups, 
                 labelvar = "age_group",
                 numvar = "count", 
                 options = list(pieHole = 0.4,
                                sliceVisibilityThreshold = 0.001))
  })
  output$agehist <- renderPlotly({
    age_groups <- ages() %>% filter(age_group != "Unknown", term <= 130)
    unknown <- ages() %>% filter(age_group != "Unknown", term > 130)
    unknown_count <- sum(unknown$count)
    
    plottitle <- paste0("Histogram of Patient Ages")
    if(unknown_count > 0) plottitle <- paste0(plottitle, "<br>(", unknown_count, " reports with age greater than 130 excluded)")
    
    hist <- ggplot(age_groups, aes(x = term, weight = count, fill = age_group)) +
      geom_histogram() +
      ggtitle(plottitle) + 
      xlab("Age at onset (years)") + 
      ylab("Number of Reports") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold")) +
      scale_x_continuous(limits = c(0, 120))
    ggplotly(hist)
  })
  
  ### Data about Drugs
  output$indication_plot <- renderGvis({
    query <- faers_query()$openfda_query
    
    indications <- query %>%
      fda_count("patient.drug.drugindication.exact") %>%
      fda_limit(25) %>%
      fda_exec()
    
    if(is.null(indications)) indications <- data.table(term = character(), count = numeric())
    gvisBarChart_HCSC(indications, "term", "count", google_colors[1])
  })
  output$drug_plot <- renderGvis({
    query <- faers_query()$openfda_query
    
    drugs <- query %>%
      fda_count("patient.drug.openfda.generic_name.exact") %>%
      fda_limit(25) %>%
      fda_exec()
    
    if (is.null(drugs)) drugs <- data.table(term = character(), count = numeric())
    gvisBarChart_HCSC(drugs, "term", "count", google_colors[2])
  })
  output$drugclassplot <- renderGvis({
    query <- faers_query()$openfda_query
    
    drugclass <- query %>%
      fda_count("patient.drug.openfda.pharm_class_epc.exact") %>%
      fda_limit(25) %>%
      fda_exec()
    
    if(is.null(drugclass)) drugclass <- data.table(term = character(), count = numeric())
    gvisBarChart_HCSC(drugclass, "term", "count", google_colors[3])
  })
  
  ### Data about Reactions
  output$top_pt <- renderGvis({
    query <- faers_query()$openfda_query
    data <- query %>%
      fda_count("patient.reaction.reactionmeddrapt.exact") %>%
      fda_limit(25) %>%
      fda_exec()
    gvisBarChart_HCSC(data, "term", "count", google_colors[1])
    })
  output$top_hlt <- renderGvis({
    data <- faers_query()$openfda_query %>%
      fda_count("patient.reaction.reactionmeddrapt.exact") %>%
      fda_limit(1000) %>%
      fda_exec() %>%
      inner_join(meddra, by = "term") %>%
      distinct(term, HLT_Term, count) %>%
      group_by(HLT_Term) %>%
      summarise(count = sum(count)) %>%
      top_n(25, count) %>%
      arrange(desc(count))
    gvisBarChart_HCSC(data, "HLT_Term", "count", google_colors[2])
    })
  output$outcomeplot <- renderGvis({
    outcome_results <- faers_query()$openfda_query %>% 
      fda_count("patient.reaction.reactionoutcome") %>% 
      fda_exec()
    if(is.null(outcome_results)) outcome_results <- data.table(term = numeric(), count = numeric())
    
    outcome_results <- outcome_results %>%  
      left_join(outcome_code) %>%
      select(label, count)
    
    gvisPieChart(outcome_results, 
                 labelvar = "label",
                 numvar = "count", 
                 options = list(pieHole = 0.4))
  })
  
}

shinyApp(ui, server)
