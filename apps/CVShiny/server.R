shinyServer(function(input, output, session) {
  updateSelectizeInput(session, 'search_brand', choices = topbrands, server = TRUE)
  updateSelectizeInput(session, 'search_ing', choices = topings_cv, server = TRUE)
  updateSelectizeInput(session, 'search_rxn', choices = pt_choices, server = TRUE)
  updateSelectizeInput(session, 'search_soc', choices = soc_choices, server = TRUE)
  
  # callModule(cvshiny_selectinput, 'search_soc', data = soc_choices)
  # callModule(cvshiny_selectinput, 'search_brand', data = topbrands)
  
  
  ##### Reactive data processing
  # Data structure to store current query info
  current_search <- reactiveValues()
  #subset_cv <- reactiveValues()
  selected_ids <- reactiveValues()
  report_tab <- reactiveValues()

  
  
  
  # We need to have a reactive structure here so that it activates upon loading
  reactiveSearchButton <- reactive(as.vector(input$searchButton))
  
  

  
 observeEvent(reactiveSearchButton(),
    withProgress(message = 'Calculation in progress', value = 0, {
                 
    if (input$name_type == "brand") {
      name <- input$search_brand
    } else if (input$name_type == "ingredient") {
      name <- input$search_ing
    } else {
      name <- input$search_ing2
    }
    
    startDate <- paste(input$searchStartYear, input$searchStartMonth, '1', collapse = "-") %>% ymd(tz = 'EST')
    endDate <- paste(input$searchEndYear, input$searchEndMonth, '1', collapse = "-") %>% ymd(tz = 'EST') 
    dateRange <- c(startDate, endDate)
    
    current_search$name_type <- input$name_type
    current_search$name <- name
    current_search$drug_inv <- input$drug_inv
    current_search$rxn <- input$search_rxn
    current_search$gender <- input$search_gender
    current_search$soc <- input$search_soc
    current_search$age <- input$search_age
    current_search$date_range <- dateRange
    current_search$checkbox_filter <- input$filter_over_100

    incProgress(1/8, detail = 'Filtering Report IDs')
    

    
    cv_reports_filtered_ids <- cv_reports %>%
      filter(DATINTRECEIVED_CLEAN >= current_search$date_range[1], DATINTRECEIVED_CLEAN <= current_search$date_range[2])
    if (current_search$gender == 'Male' | current_search$gender == 'Female') {
      cv_reports_filtered_ids %<>% filter(GENDER_ENG == current_search$gender)
    }
    incProgress(1/8, detail = 'Applying Age Constraints')

    if (current_search$checkbox_filter & current_search$age[2] == 100) {
      cv_reports_filtered_ids %<>% filter(AGE_Y >= current_search$age[1])
    } else {
      cv_reports_filtered_ids %<>% filter(AGE_Y >= current_search$age[1] & AGE_Y <= current_search$age[2])
    }
    cv_reports_filtered_ids %<>% select(REPORT_ID)


    cv_report_drug_filtered <- cv_report_drug
    if (current_search$name_type == "brand" & !is.null(current_search$name)) {
      if (length(current_search$name) == 1) cv_report_drug_filtered %<>% filter(DRUGNAME == current_search$name)
      else cv_report_drug_filtered %<>% filter(DRUGNAME %in% current_search$name)

      incProgress(1/8, detail = 'Filtering by Brand')

    } else if (current_search$name_type == "ingredient2" & !is.null(current_search$name) && current_search$name != "") {
      related_drugs <- cv_substances %>% filter(ing == current_search$name)
      cv_report_drug_filtered %<>% semi_join(related_drugs, by = "DRUGNAME")

    } else if (current_search$name_type == "ingredient" & !is.null(current_search$name)) {
      if (length(current_search$name) == 1) related_drugs <- cv_drug_product_ingredients %>% filter(ACTIVE_INGREDIENT_NAME == current_search$name)
      else related_drugs <- cv_drug_product_ingredients %>% filter(ACTIVE_INGREDIENT_NAME %in% current_search$name)
      cv_report_drug_filtered %<>% semi_join(related_drugs, by = "DRUG_PRODUCT_ID")

      incProgress(1/8, detail = 'Filtering by Ingredient')

    }
    if (current_search$drug_inv != "Any") cv_report_drug_filtered %<>% filter(DRUGINVOLV_ENG == current_search$drug_inv)

    incProgress(2/8, detail = 'Filtering Reactions')



    cv_reactions_filtered <- cv_reactions %>% filter(PT_NAME_ENG != "")
    if (!is.null(current_search$rxn)) {
      if (length(current_search$rxn) == 1) {
        cv_reactions_filtered %<>% filter(PT_NAME_ENG == current_search$rxn | SMQ == current_search$rxn) %>% distinct()
      } else {
        cv_reactions_filtered %<>% filter(PT_NAME_ENG %in% current_search$rxn | SMQ %in% current_search$rxn) %>% distinct()
      }
    }
    if (!is.null(current_search$soc)) {
      if (length(current_search$soc) == 1) cv_reactions_filtered %<>% filter(SOC_NAME_ENG == current_search$soc)
      else cv_reactions_filtered %<>% filter(SOC_NAME_ENG %in% current_search$soc)
    }

    # cv_reports_filtered_ids %<>% as.data.frame()
    # cv_report_drug_filtered %<>% as.data.frame()
    # cv_reactions_filtered %<>% as.data.frame()


    selected_ids$ids <-  cv_reports_filtered_ids %>%
      semi_join(cv_report_drug_filtered, by = "REPORT_ID") %>%
      semi_join(cv_reactions_filtered, by = "REPORT_ID") %>% as.data.frame()
    incProgress(1/8, detail = 'Checking for no reports...')
    n_ids <- selected_ids$ids %>% nrow()
    if (n_ids == 0) {
      setProgress(1)
      showModal(modalDialog(
        title = list(icon("exclamation-triangle"), "No results found!"),
        "There were no reports matching your query.",
        size = "s",
        easyClose = TRUE))
      return()
    }

    incProgress(1/8, detail = 'Fetching data...')
    
    # so then all data is polled upon search, not just when display corresponding plot
    # subset_cv$report <- cv_reports %>%
    #   semi_join(selected_ids, by = "REPORT_ID")
    # subset_cv$drug <- cv_report_drug %>%
    #   semi_join(selected_ids, by = "REPORT_ID") %>%
    #   left_join(cv_report_drug_indication, by = c("REPORT_DRUG_ID", "REPORT_ID", "DRUG_PRODUCT_ID", "DRUGNAME")) 
    # subset_cv$rxn <- cv_reactions %>%
    #   semi_join(selected_ids, by = "REPORT_ID") %>%
    #   left_join(meddra, by = c("PT_NAME_ENG" = "PT_Term", "MEDDRA_VERSION" = "Version"))
    
    
    
    
    
    
    
    
    
    
    incProgress(1/8)
                 })
  )
  
  
  cv_download_reports <- reactive({
    data_type <- ""
    
    if(input$search_dataset_type == "Report Data"){
      reports_tab_master <- mainDataSelection() %>% as.data.frame() %>% `[`(, input$column_select_report) # FLAG
      data_type <- "report"
    } else if(input$search_dataset_type == "Drug Data"){
      reports_tab_master <- drugDataSelection() %>% as.data.frame() %>% `[`(, input$column_select_drug)
      data_type <- "drug"
    } else if(input$search_dataset_type == "Reaction Data"){
      reports_tab_master <- rxnDataSelection() %>% as.data.frame() %>% `[`(, input$column_select_reaction)
      data_type <- "rxn"
    }
    
    return(list(
      reports_tab_master = reports_tab_master,
      data_type = data_type
    ))
  })
  
  ##### Output ####
  ##### Construct Current_Query_Table for generic name, brand name, adverse reaction term & date range searched
  output$current_search <- renderTable({
    data <- current_search
    result <- data.frame(names = c("Name Type:",
                                   "Age Range:",
                                   "Gender:",
                                   "Name:",
                                   "Adverse Reaction Term:",
                                   "System Organ Class:",
                                   "Date Range:"),
                         values = c(data$name_type %>% toupper(),
                                    sprintf("%s to %s%s", data$age[1], data$age[2], ifelse(data$checkbox_filter & data$age[2] == 100, '+', '')),
                                    data$gender,
                                    paste0(data$name, collapse = ", "),
                                    paste0(data$rxn, collapse = ", "),
                                    paste0(data$soc, collapse = ", "),
                                    paste(data$date_range, collapse = " to ")),
                         stringsAsFactors = FALSE)
    result$values["" == result$values] <- "Not Specified"
    result
  },
  include.colnames = FALSE
  )
  
  mainDataSelection <- reactive({
    # mychart_pool <- src_pool(hcopen_pool)
    # search_function(mychart_pool, current_search)
    data <- semi_join(cv_reports, selected_ids$ids, by = "REPORT_ID", copy=T)
    data
  })
  ##### Create time plot  ###
  
  output$timeplot_title <- renderUI({
    data <- mainDataSelection()

    nreports <- data %>%
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
  output$mychart <- renderLineChart({
    # adrplot_test <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria"))
    # report_tab$main_chart
    # mychart_pool <- src_pool(hcopen_pool)
    # cv_reports <- tbl(pool_src, paste0('cv_reports_', time_period))
    # cv_report_drug <- tbl(pool_src, paste0('cv_report_drug_', time_period))
    # cv_drug_product_ingredients <- tbl(pool_src, paste0('cv_drug_product_ingredients_', time_period))
    # cv_reactions <- tbl(pool_src, paste0('cv_reactions_', time_period))
    # search_function(mychart_pool, current_search)
    # data <- semi_join(cv_reports, selected_ids, by = "REPORT_ID")
    data <- mainDataSelection()



    dates <- data %>% select(DATINTRECEIVED_CLEAN) %>% summarize(date_min = min(DATINTRECEIVED_CLEAN),
                                                                 date_max = max(DATINTRECEIVED_CLEAN)) %>%
      as.data.frame()

    two_years <- 730

    if ((dates$date_max - dates$date_min) >= two_years) {
      time_period <- "year"
      time_function <- function(x) {years(x)}
    } else {
      time_period <- "month"
      time_function <- function(x) {months(x)}
    }

    data_r <- data %>% select(c(DATINTRECEIVED_CLEAN, SERIOUSNESS_ENG, DEATH)) %>%
      dplyr::mutate(time_p = date_trunc(time_period, DATINTRECEIVED_CLEAN))


    total_results <- data_r %>%
      group_by(time_p) %>%
      summarize(total = n())


    serious_results <- data_r %>%
      filter(SERIOUSNESS_ENG == "Yes") %>%
      group_by(time_p) %>%
      summarize(serious = n())



    death_results <- data_r %>%
      filter(DEATH == 1) %>%
      group_by(time_p) %>%
      summarize(death = n())


    ntime_p <- interval(dates$date_min, dates$date_max) %/% time_function(1)
    time_list <- min(dates$date_min %>% floor_date(time_period)) + time_function(0:ntime_p)

    results_to_be_mapped <- left_join(total_results, serious_results, by = 'time_p') %>%
      left_join(death_results, by = 'time_p') %>% as.data.frame() %>%
      mutate(time_p = ymd(time_p))

    results <- data.frame(time_p = time_list) %>%
      left_join(results_to_be_mapped, by = 'time_p')

    results[is.na(results)] <- 0
    results
  })

  ##### Data about Reports
  
  reporterplot_data <- reactive({
    df <- mainDataSelection() %>%
      count(REPORTER_TYPE_ENG) %>%
      as.data.frame()
    
    df$REPORTER_TYPE_ENG[df$REPORTER_TYPE_ENG == ""] <- "Not reported"
    df$REPORTER_TYPE_ENG[df$REPORTER_TYPE_ENG == "Consumer Or Other Non Health Professional"] <- "Consumer or non-health professional"
    df$REPORTER_TYPE_ENG[df$REPORTER_TYPE_ENG == "Other Health Professional"] <- "Other health professional"
    
    df
    
  })
  callModule(pieTable, "reporterplot", dataChart = reporterplot_data(), x = "REPORTER_TYPE_ENG", y = "count")
  
  seriousplot_data <- reactive({
    mainDataSelection() %>%
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
  callModule(pieTable, "seriousplot", dataChart = seriousplot_data(), x = "label", y = "count")

  output$seriousreasonsplot <- renderGvis({
    data <- mainDataSelection() %>%
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
  sexplot_data <- reactive({
    data <- mainDataSelection() %>%
      count(GENDER_ENG) %>%
      as.data.frame()
    data$GENDER_ENG[data$GENDER_ENG == ""] <- "Not specified"
    sex_results <- count(data, GENDER_ENG, wt = n)
    sex_results
  })
  
  callModule(pieTable, "sexplot", dataChart = sexplot_data(), x = "GENDER_ENG", y = "n")

  
  agegroup_data <-reactive({
    age_groups <- mainDataSelection() %>%
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
    data
  })
  callModule(pieTable, "agegroupplot", dataChart = agegroup_data(), x = "AGE_GROUP_CLEAN", y = "n")

  

  output$agehisttitle <- renderUI({
    excluded_count <- mainDataSelection() %>%
      filter(AGE_GROUP_CLEAN != "Unknown", AGE_Y > 100) %>%
      tally() %>% as.data.frame() %>% `$`(n)
    HTML(paste0("<h3>Histogram of Patient Ages ",
                tipify(
                  el = icon("info-circle"), trigger = "hover click",
                  title = "Distribution of number of reports per age, colour-coded by age group. Each bin groups 2 years."),
                "<br>(", excluded_count, " reports with age greater than 100 excluded)", "</h3>"))
  })
  output$agehist <- renderPlotly({
    age_groups <- mainDataSelection() %>% filter(AGE_GROUP_CLEAN != "Unknown", AGE_Y <= 100) %>%
      arrange(AGE_Y) %>% 
      select(c(AGE_Y, AGE_GROUP_CLEAN)) %>%
      as.data.frame()
    age_groups$AGE_GROUP_CLEAN %<>% factor(levels = c("Neonate", "Infant", "Child", "Adolescent", "Adult", "Elderly"))

    # joining by remaining terms so you can assign the right colours to the legend
    colours_df <- data.frame(
      AGE_GROUP_CLEAN = c("Neonate", "Infant", "Child", "Adolescent", "Adult", "Elderly"),
      colours = google_colors[1:6],
      stringsAsFactors = FALSE) %>%
      semi_join(age_groups, by = "AGE_GROUP_CLEAN")

    hist <- ggplot(age_groups, aes(x = AGE_Y, fill = AGE_GROUP_CLEAN)) +
      geom_histogram(breaks = seq(0, 100, by = 2)) +
      scale_fill_manual(values = colours_df$colours) +
      xlab("Age at onset (years)") +
      ylab("Number of Reports") +
      theme_bw()
    ggplotly(hist)
  })
  
  #### Data about Drugs
  drugDataSelection <- reactive({
    data <- semi_join(cv_report_drug, selected_ids$ids, by = "REPORT_ID", copy = T)
    #%>%       left_join(cv_report_drug_indication, by = c("REPORT_DRUG_ID", "REPORT_ID", "DRUG_PRODUCT_ID", "DRUGNAME"))
    data
  })
  indication_data <- reactive({
    # Data frame used to obtain Top_25_indication bar chart: Indication is only associated with individual drug
    # When brand name is unspecified, chart shows top 25 indications associated with all drugs + date_range
    # When brand name is specified, chart shows top 25 indications associated with specified drug + date_range
    
    
    # NOTE ABOUT INDICATIONS STRUCTURE:
    # REPORT_ID -> multiple drugs per report
    # DRUG_ID -> multiple reports may use the same drugs
    # REPORT_DRUG_ID -> unique for each drug/report combination. count is less than total reports since drugs can have multiple indications
    # so distinct REPORT_DRUG_ID x INDICATION_NAME_ENG includes the entire set of reports
    data <- drugDataSelection() %>%
      count(INDICATION_NAME_ENG) %>%
      arrange(desc(n)) %>%
      as.data.frame() %>%
      filter(!is.na(INDICATION_NAME_ENG)) %>%
      head(25)
    data
  })
  
  callModule(barTable, "indicationplot", dataChart = indication_data(), x = "INDICATION_NAME_ENG", y = "n", colour = google_colors[1])
  # output$indication_plot <- renderGvis({
  #   gvisBarChart_HCSC(indication_data(), "INDICATION_NAME_ENG", "n", google_colors[1])
  # })
  # output$indication_plot.table <- renderGvis({
  #   gvisTable(indication_data())
  # })
  # output$indication_plot.sus <- renderGvis({
  #   gvisBarChart_HCSC(indication_data(), "INDICATION_NAME_ENG", "n", google_colors[1])
  # })
  # output$indication_plot.table.sus <- renderGvis({
  #   gvisTable(indication_data())
  # })
  # output$indication_plot.con <- renderGvis({
  #   gvisBarChart_HCSC(indication_data(), "INDICATION_NAME_ENG", "n", google_colors[1])
  # })
  # output$indication_plot.table.con <- renderGvis({
  #   gvisTable(indication_data())
  # })
  
  all_data <- reactive({
    data <- drugDataSelection() %>%
      distinct(REPORT_ID, DRUGNAME) %>%
      count(DRUGNAME) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
    data
  })
  
  callModule(barTable, "all_drugs", dataChart = all_data(), x = "DRUGNAME", y = "n", colour = google_colors[2])
  # output$drug_all <- renderUI({
  #   data <- all_data()
  #   
  #   switch(input$all_select,
  #          "barchart" = gvisBarChart_HCSC(data, "DRUGNAME", "n", google_colors[2]),
  #          "table" = gvisTable(data))
  # })
  
  
  suspect_data <- reactive({
    data <- drugDataSelection() %>%
      filter(DRUGINVOLV_ENG == "Suspect") %>%
      dplyr::distinct(REPORT_ID, DRUGNAME) %>%
      count(DRUGNAME) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
  })
  
  callModule(barTable, "suspect_drugs", dataChart = suspect_data(), x = "DRUGNAME", y = "n", colour = google_colors[3])
  output$suspect_drugs <- renderGvis({
    # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name
    #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq)
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    gvisBarChart_HCSC(suspect_data(), "DRUGNAME", "n", google_colors[2])
  })
  
  output$suspect_drugs.table <- renderGvis({
    gvisTable(suspect_data())
  })
  
  
  concomitant_data <- reactive({
    data <- drugDataSelection() %>%
      filter(DRUGINVOLV_ENG == "Concomitant") %>%
      distinct(REPORT_ID, DRUGNAME) %>%
      count(DRUGNAME) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
  })
   
  callModule(barTable, "concomitant_drugs", dataChart = concomitant_data(), x = "DRUGNAME", y = "n", colour = google_colors[4])
  output$concomitant_drugs <- renderGvis({
    # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name
    #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq)
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    gvisBarChart_HCSC(concomitant_data(), "DRUGNAME", "n", google_colors[2])
  })
  
  output$concomitant_drugs.table <- renderGvis({
    data <- concomitant_data()
    gvisTable(data)
  })
 
  output$drugcounttitle <- renderUI({
    excluded_count <- drugDataSelection() %>%
      count(REPORT_ID) %>%
      filter(n > 20) %>%
      count() %>% as.data.frame() %>% `$`('nn')
    HTML(paste0("<h3>Number of Drugs per Report ",
                tipify(
                  el = icon("info-circle"), trigger = "hover click",
                  title = paste0(
                    "This plot indicates how many reports include how many drugs. ",
                    "The search query filters unique reports, which may have one or more drugs associated with them.")),
                "<br>(", excluded_count, " reports with more than 20 drugs excluded)", "</h3>"))
  })
  
  drugcount_data <- reactive({
    data <- drugDataSelection() %>%
      count(REPORT_ID) %>%
      filter(n <= 20) %>%
      group_by(n) %>%
      count() %>%
      arrange(n) %>%
      as.data.frame() %>%
      mutate(`Number of Drugs` = as.factor(n),
             `Number of Drugs in Report` = nn) %>%
      select(-c(n,nn))
  })
  
  output$drugcount_plot <- renderGvis({
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    gvisColumnChart(drugcount_data(), 'Number of Drugs', "Number of Drugs in Report", options = list(
      legend = "{ position: 'none' }",
      height = 600,
      vAxis = "{title: 'Number of Reports'}",
      hAxis = "{title: 'Number of Drugs in Report'}",
      chartArea = "{top: 20, height: '75%', left: 80, width: '90%'}")
      )
  })
  
  
  #### Data about Reactions
  
  rxnDataSelection <- reactive({
    data <- cv_reactions %>%
      semi_join(selected_ids$ids, by = "REPORT_ID", copy = T) 
    data
  })
  
  top_pt_data <- reactive({
    data <- rxnDataSelection() %>%
      count(PT_NAME_ENG) %>%
      arrange(desc(n)) %>%
      head(15) %>%
      as.data.frame()
    data
  })
  callModule(barTable, "toppt", dataChart = top_pt_data(), x = "PT_NAME_ENG", y = "n", colour = google_colors[1])
  
  top_hlt_data <- reactive({
    data <- rxnDataSelection() %>%
      filter(!is.na(HLT_Term)) %>%
      count(HLT_Term) %>%
      arrange(desc(n)) %>%
      head(15) %>%
      as.data.frame()
    data
  })
  callModule(barTable, "tophlt", dataChart = top_hlt_data(), x = "HLT_Term", y = "n", colour = google_colors[2])

  
  outcomeplot_data <- reactive({
    mainDataSelection() %>%
      count(OUTCOME_ENG) %>%
      as.data.frame()
  })
  callModule(pieTable, "outcomeplot", dataChart = outcomeplot_data(), x = "OUTCOME_ENG", y = "n")

  
  ############# Download Tab
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
  
}
)