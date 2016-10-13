library(shinydashboard)
library(jsonlite)
library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)
library(plotly)
library(shiny)
library(DT)
library(googleVis)
library(openfda)
library(stringr)
source("common_ui.R")


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
topbrands <- fda_query("/drug/event.json") %>%
  fda_count("patient.drug.openfda.brand_name.exact") %>% 
  fda_limit(1000) %>% 
  fda_exec()
toprxns <- fda_query("/drug/event.json") %>%
  fda_count("patient.reaction.reactionmeddrapt.exact") %>% 
  fda_limit(1000) %>% 
  fda_exec()


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

adrplot <- function(monthlyadrdata, plottitle){
  nreports <- sum(monthlyadrdata$n)
  plottitle <- paste0(str_replace_all(plottitle, "\\+", " "), " (", nreports, " reports)") 
  plot <- monthlyadrdata %>%
    ggplot(aes(x = month, y = n)) +
    geom_line(stat = "identity", size = 0.1) +
    stat_smooth(method = "loess", size = 0.1) +
    ggtitle(plottitle) + 
    xlab("Month") + 
    ylab("Number of Reports") +
    theme_bw() +
    theme(plot.title = element_text(lineheight=.8, face="bold"))
}

format1K <- function(x){
  x/1000
}



#### UI ####
ui <- dashboardPage(
  dashboardHeader(title = "Shiny FAERS (v0.05)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Reports", tabName = "reportdata", icon = icon("hospital-o")),
      menuItem("Patients", tabName = "patientdata", icon = icon("user-md")),
      menuItem("Drugs", tabName = "drugdata", icon = icon("flask")),
      menuItem("Reactions", tabName = "rxndata", icon = icon("heart-o")),
      menuItem("About", tabName = "aboutinfo", icon = icon("info"))
    ),
    selectizeInput("search_generic", 
                   "Generic Name (Active Ingredient)", 
                   topdrugs$term,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    selectizeInput("search_brand", 
                   "Brand Name (US Trade Name)",
                   topbrands$term,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    selectizeInput("search_rxn", 
                   "Adverse Event Term",
                   toprxns$term,
                   options = list(create = TRUE,
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'))),
    dateRangeInput("searchDateRange", 
                   "Date Range", 
                   start = "2000-01-01", 
                   startview = "year"),
    actionButton("searchButton", "Search", width = '100%'),
    tags$br(),
    tags$h3(strong("Current Query:")),
    tableOutput("current_search")
  ),
  
  dashboardBody(
    fluidRow(
      box(plotlyOutput(outputId = "timeplot"),
          tags$br(),
          tags$p("Reports by month from US FDA FAERS (open.fda.gov). 
                 Trendline is a local non-parametric regression calculated with the LOESS model. 
                 The shaded area is an approximation of the 95% confidence interval of the regression."),
          tags$p("Search URL:"),
          uiOutput(outputId = "search_url"),
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
                    title = tags$h2("Reporter"), width = 3),
                box(htmlOutput("seriousplot"), 
                    tags$br(),
                    tags$p("Reports marked as serious."),
                    title = tags$h2("Serious reports"), width = 3),
                box(htmlOutput("seriousreasonsplot"), 
                    tags$br(),
                    tags$p("Total sums to more than 100% because reports can be marked serious for multiple reasons."),
                    title = tags$h2("Reasons for serious reports"), width = 3),
                box(htmlOutput("countryplot"), 
                    tags$br(),
                    tags$p("Country the reaction(s) occurred in. This is not necessarily the same country the report was received from."),
                    title = tags$h2("Country"), width = 3)
              )
      ),
      tabItem(tabName = "patientdata",
              fluidRow(
                box(htmlOutput("sexplot"),
                    tags$br(),
                    tags$p("Unknown includes both reports explicitly marked unknown and reports with no gender information."),
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
                    tags$p("This plot includes all indications for all drugs associated with the matching reports.
                           The open.fda.gov search API does not allow searching or filtering within drugs.
                           The search query filters unique reports, which may have one or more drugs associated with them.
                           It is not currently possible to search for only those indications associated with a specific drug.
                           "), width = 6),
                box(plotOutput("drugplot"),
                    tags$p("This plot includes all drugs associated with the matching reports, except the search term.
                           The open.fda.gov search API does not allow searching or filtering within drugs.
                           The search query filters unique reports, which may have one or more drugs associated with them.
                           It is not currently possible to retrieve correlations between drugs."), width = 6),
                box(plotOutput("drugclassplot"),
                    tags$p("This plot includes all drug classes associated with the matching reports, including the search term.
                           The total number of instances for each class will be geater 
                           than the number of reports when reports include more than one drug of the same class.
                           The open.fda.gov search API does not allow searching or filtering within drugs.
                           The search query filters unique reports, which may have one or more drugs associated with them.
                           It is not currently possible to retrieve correlations between drugs."), width = 6)
              )
      ),
      tabItem(tabName = "rxndata",
              fluidRow(
                box(htmlOutput("outcomeplot"), title = tags$h2("Outcomes (all reactions)"))
              )
      ),
      tabItem(tabName = "aboutinfo",
              tags$p("Data provided by the U.S. Food and Drug Administration (https://open.fda.gov)"),
              aboutAuthors()
      )
    )
  ), 
  skin = "blue"
)


#### server ####
server <- function(input, output) {
  # Data structure to store current query info
  faers_query <- reactive({
    input$searchButton
    isolate({
    current_generic <- ifelse(input$search_generic == "",
                                      NA,
                                      input$search_generic) %>% 
                                 str_replace_all(" ", "+")
    current_brand <- ifelse(input$search_brand == "",
                                    NA,
                                    input$search_brand) %>% str_replace_all(" ", "+")
    current_rxn <- ifelse(input$search_rxn == "",
                                  NA,
                                  input$search_rxn) %>% str_replace_all(" ", "+")
    current_date_range <- input$searchDateRange
    querydate <- paste0("[",
                        paste(current_date_range, collapse = "+TO+"),
                        "]")
    openfda_query <- fda_query("/drug/event.json") %>%
      fda_filter("receivedate", querydate)
    if(!is.na(current_generic)) openfda_query <- openfda_query %>%
      fda_filter("patient.drug.openfda.generic_name.exact", paste0('"', current_generic, '"'))
    if(!is.na(current_brand)) openfda_query <- openfda_query %>%
      fda_filter("patient.drug.openfda.brand_name.exact", paste0('"', current_brand, '"'))
    if(!is.na(current_rxn)) openfda_query <- openfda_query %>%
      fda_filter("patient.reaction.reactionmeddrapt.exact", paste0('"', current_rxn, '"'))
    
    query_url <- openfda_query %>% fda_search() %>% fda_url()
    
    total_reports <- query_url %>%
      fda_fetch() %>%
      .$meta %>%
      .$results %>%
      .$total
    
    return(list(current_search = list(current_generic,
                                      current_brand,
                                      current_rxn,
                                      current_date_range),
                query_url = query_url,
                openfda_query = openfda_query,
                total_reports = total_reports))
  })})
  
  ages <- reactive({
    data <- faers_query()
    
    age_years <- data$openfda_query %>% 
      fda_filter("patient.patientonsetageunit", "801") %>%
      fda_count("patient.patientonsetage") %>%
      fda_exec()  
    if(is.null(age_years)) age_years <- data.table(term = numeric(), count = numeric())
    
    age_decades <- data$openfda_query %>% 
      fda_filter("patient.patientonsetageunit", "800") %>%
      fda_count("patient.patientonsetage") %>%
      fda_exec()  
    
    if(is.null(age_decades)) age_decades <- data.table(term = numeric(), count = numeric())
    age_decades <- age_decades %>%
      mutate(term = term*10)
    
    age_months <- data$openfda_query %>% 
      fda_filter("patient.patientonsetageunit", "802") %>%
      fda_count("patient.patientonsetage") %>%
      fda_exec() 
    if(is.null(age_months)) age_months <- data.table(term = numeric(), count = numeric())
    age_months <- age_months %>% 
      mutate(term = term/12)
    
    age_weeks <- data$openfda_query %>% 
      fda_filter("patient.patientonsetageunit", "803") %>%
      fda_count("patient.patientonsetage") %>%
      fda_exec() 
    if(is.null(age_weeks)) age_weeks <- data.table(term = numeric(), count = numeric())
    age_weeks <- age_weeks %>% 
      mutate(term = term/52)
    
    age_days <- data$openfda_query %>% 
      fda_filter("patient.patientonsetageunit", "804") %>%
      fda_count("patient.patientonsetage") %>%
      fda_exec() 
    if(is.null(age_days)) age_days <- data.table(term = numeric(), count = numeric())
    age_days <- age_days %>%  mutate(term = term/365)
    
    age_hours <- data$openfda_query %>% 
      fda_filter("patient.patientonsetageunit", "805") %>%
      fda_count("patient.patientonsetage") %>%
      fda_exec() 
    if(is.null(age_hours)) age_hours <- data.table(term = numeric(), count = numeric())
    age_hours <- age_hours %>%  mutate(term = term/(365*24))
    
    ages <- bind_rows(age_years, 
                      age_decades,
                      age_months,
                      age_weeks,
                      age_days,
                      age_hours) %>%
      count(term, wt = count)
  })
  
  output$search_url <- renderUI({
    url <- faers_query()$query_url 
    tags$a(url, href=url)
  })
  
  output$current_search <- renderTable({
    data <- faers_query()
    data.table(names = c("Generic Name:", 
                         "Brand Name:", 
                         "Adverse Reaction Term:",
                         "Date Range:"),
               values = c(ifelse(is.na(data$current_search[[1]]),
                                 "Not Specified",
                                 data$current_search[[1]]),
                          ifelse(is.na(data$current_search[[2]]),
                                 "Not Specified",
                                 data$current_search[[2]]),
                          ifelse(is.na(data$current_search[[3]]),
                                 "Not Specified (All)",
                                 data$current_search[[3]]),
                          paste(data$current_search[[4]], collapse = " to ")))
  }, include.rownames = FALSE, include.colnames = FALSE)
  
  output$timeplot <- renderPlotly({
    
    data <- faers_query()
    
    time_results <- data$openfda_query %>%
      fda_count("receivedate") %>%
      fda_exec() 
    if(is.null(time_results)){
      time_results <- data.table(month = numeric(), count = numeric())
    } else{
      time_results <- time_results %>%
        mutate(month = floor_date(ymd(time), "month")) %>%
        count(month, wt = count)
    }
    
    title <- ifelse(!is.na(data$current_search[[1]]), data$current_search[[1]], data$current_search[[2]])
    plottitle <- paste("Drug Adverse Event Reports for", title)
    p <- adrplot(time_results, plottitle)
    #print(p)
    ggplotly(p)
  })
  
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
    
    if(!is.null(unknown)){ reporter_results <- rbind(reporter_results, c("Unknown", unknown)) %>%
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
      mutate(label = ifelse(term == 1, "Yes", "No")) %>%
      select(label, count)
    
    unknown <- data$openfda_query %>%
      fda_filter("_missing_", "serious") %>%
      fda_url() %>%
      fda_fetch() %>%
      .$meta %>%
      .$results %>%
      .$total
    
    if(!is.null(unknown)){ serious_results <- rbind(serious_results, c("Unknown", unknown)) %>%
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
    
    
    serious_reasons <- bind_rows("congenital" = congenital_results,
                                 "death" = death_results,
                                 "disabling" = disabling_results,
                                 "hospitalization" = hospital_results,
                                 "lifethreatening" = lifethreaten_results,
                                 "other" = serother_results,
                                 .id = "label") %>%
      select(label, count)
    
    serious_reasons <- mutate(serious_reasons, percentage = count/total_serious*100 %>% signif(digits = 3)) %>% 
      select(-count) %>%
      arrange(desc(percentage))
    
    gvisBarChart(serious_reasons, 
                 xvar = "label",
                 yvar = "percentage", 
                 options = list(title = paste0("Reasons for serious reports (", total_serious, " serious reports)"),
                                legend = "{position:'none'}",
                                bars = 'horizontal',
                                #hAxis = "{format:'percent'}",
                                axes= "x: {
                                0: { side: 'top', label: 'Percentage'} 
  }",
                                bar = list(groupWidth =  '90%')
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
  
  output$sexplot <- renderGvis({
    data <- faers_query()
    
    sex_results <- data$openfda_query %>% 
      fda_count("patient.patientsex") %>% 
      fda_exec()
    if(is.null(sex_results)) sex_results <- data.table(term = numeric(), count = numeric())
    
    sex_results <- sex_results %>%
      left_join(sex_code) %>%
      select(label, count)
    
    unknown <- data$openfda_query %>%
      fda_filter("_missing_", "patient.patientsex") %>%
      fda_url() %>%
      fda_fetch() %>%
      .$meta %>%
      .$results %>%
      .$total
    
    if(!is.null(unknown)){ 
      sex_results[sex_results$label == "Unknown", "count"] <- sex_results[sex_results$label == "Unknown", "count"] + unknown
    }
    
    gvisPieChart(sex_results, 
                 labelvar = "label",
                 numvar = "count", 
                 options = list(pieHole = 0.4))
  })
  
  output$agegroupplot <- renderGvis({
    ages <- ages()
    
    data <- faers_query()
    
    age_groups <- mutate(ages,
                         age_group = NA,
                         age_group = ifelse(term <= 28/365, "Neonate", age_group),
                         age_group = ifelse(term <= 2 & term > 28/365, "Infant", age_group),
                         age_group = ifelse(term > 2 & term < 12, "Child", age_group),
                         age_group = ifelse(term >= 12 & term < 18, "Adolescent", age_group),
                         age_group = ifelse(term >= 18 & term < 65, "Adult", age_group),
                         age_group = ifelse(term >= 65, "Elderly", age_group),
                         age_group = ifelse(term >= 130, "Unknown", age_group)) %>%
      count(age_group, wt = n, sort = TRUE) %>%
      rename(n = nn)
    
    unknown <- data$openfda_query %>%
      fda_filter("_missing_", "patient.patientonsetage") %>%
      fda_url() %>%
      fda_fetch() %>%
      .$meta %>%
      .$results %>%
      .$total
    
    if(!is.null(unknown)){ age_groups <- rbind(age_groups, c("Unknown", unknown)) %>%
      mutate(n = as.numeric(n))
    }
    
    gvisPieChart(age_groups, 
                 labelvar = "age_group",
                 numvar = "n", 
                 options = list(pieHole = 0.4))
  })
  
  output$agehist <- renderPlotly({
    ages <- ages()
    
    age_groups <- mutate(ages,
                         age_group = NA,
                         age_group = ifelse(term <= 28/365, "Neonate", age_group),
                         age_group = ifelse(term <= 2 & term > 28/365, "Infant", age_group),
                         age_group = ifelse(term > 2 & term < 12, "Child", age_group),
                         age_group = ifelse(term >= 12 & term < 18, "Adolescent", age_group),
                         age_group = ifelse(term >= 18 & term < 65, "Adult", age_group),
                         age_group = ifelse(term >= 65, "Elderly", age_group)) %T>%
                         {unknown <<- filter(., term >= 130) %>% nrow()} %>%
      filter(term < 130)
    plottitle <- paste0("Histogram of Patient Ages")
    if(unknown > 0) plottitle <- paste0(plottitle, "<br>(", unknown, " ages greater than 130 excluded)")
    
    hist <- ggplot(age_groups, aes(x = term, weight = n, fill = age_group)) +
      geom_histogram() +
      ggtitle(plottitle) + 
      xlab("Age at onset (years)") + 
      ylab("Number of Reports") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    
    ggplotly(hist)
    
  })
  
  output$drugplot <- renderPlot({
    
    data <- faers_query()
    
    drugs <- data$openfda_query %>%
      fda_count("patient.drug.openfda.generic_name.exact") %>%
      fda_limit(1000) %>%
      fda_exec()
    
    if(is.null(drugs)) drugs <- data.table(term = character(), count = numeric())
    if(!is.na(data$current_search[[1]])) drugs <- filter(drugs, !term == data$current_search[[1]])
    
    p <- ggplot(drugs[1:25,], aes(x = term, y = count, fill = term)) + 
      geom_bar(stat = "identity") + 
      scale_x_discrete(limits = rev(drugs$term[1:25])) + 
      coord_flip() +
      ggtitle("Top 25 Drugs (in addition to search term)") +
      xlab("Drug (generic name)") + 
      ylab("Number of Reports (thousands)") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), 
            legend.position = "none") +
      scale_y_continuous(labels = format1K)
    p
  })
  
  output$drugclassplot <- renderPlot({
    data <- faers_query()
    
    drugclass <- data$openfda_query %>%
      fda_count("patient.drug.openfda.pharm_class_epc.exact") %>%
      fda_limit(1000) %>%
      fda_exec()
    
    if(is.null(drugclass)) drugclass <- data.table(term = character(), count = numeric())
    
    p <- ggplot(drugclass[1:25,], aes(x = term, y = count, fill = term)) + 
      geom_bar(stat = "identity") + 
      scale_x_discrete(limits = rev(drugclass$term[1:25])) + 
      coord_flip() +
      ggtitle("Top 25 Drug Classes \n(including search term)") +
      xlab("Established Pharmaceutical Class") + 
      ylab("Number of Instances (thousands)") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), 
            legend.position = "none") +
      scale_y_continuous(labels = format1K)
    p
  })
  
  output$outcomeplot <- renderGvis({
    data <- faers_query()
    
    outcome_results <- data$openfda_query %>% 
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
  
  output$indicationplot <- renderPlot({
    data <- faers_query()
    
    indications <- data$openfda_query %>%
      fda_count("patient.drug.drugindication.exact") %>%
      fda_limit(1000) %>%
      fda_exec()
    
    if(is.null(indications)) indications <- data.table(term = character(), count = numeric())
    
    p <- ggplot(indications[1:25,], aes(x = term, y = count, fill = term)) + 
      geom_bar(stat = "identity") + 
      scale_x_discrete(limits = rev(indications$term[1:25])) + 
      coord_flip() +
      ggtitle("Top 25 Indications (All Drugs)") +
      xlab("Indication") + 
      ylab("Number (thousands)") +
      theme_bw() +
      theme(plot.title = element_text(lineheight=.8, face="bold"), 
            legend.position = "none") +
      scale_y_continuous(labels = format1K)
    p
  })
  
  #   output$indicationstable
  #   output$drugclassestable
  #   output$drugstable
  #   output$aerchart
  #   output$aertermstable
  
  }

shinyApp(ui, server)