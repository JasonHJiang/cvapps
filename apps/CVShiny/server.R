shinyServer(function(input, output, session) {
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
    
    if (input$filter_over_100) {
      cv_reports_filtered_ids %<>% filter(AGE_Y >= input$search_age[1])
    } else {
      cv_reports_filtered_ids %<>% filter(AGE_Y >= input$search_age[1] & AGE_Y <= input$search_age[2])
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
      if (length(input$search_rxn) == 1) {
        cv_reactions_filtered %<>% filter(PT_NAME_ENG == input$search_rxn | SMQ == input$search_rxn) %>% distinct()
      } else {
        cv_reactions_filtered %<>% filter(PT_NAME_ENG %in% input$search_rxn | SMQ %in% input$search_rxn) %>% distinct()
      }
    }
    if (!is.null(input$search_soc)) {
      if (length(input$search_soc) == 1) cv_reactions_filtered %<>% filter(SOC_NAME_ENG == input$search_soc)
      else cv_reactions_filtered %<>% filter(SOC_NAME_ENG %in% input$search_soc)
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
    
    # reports_tab_master_size <- paste("Size of Dataset is",
    #                                  format(object.size(reports_tab_master),
    #                                         units = "auto"))
    
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
                                   "Name:",
                                   "Adverse Reaction Term:",
                                   "Date Range:"),
                         values = c(data$name_type %>% toupper(),
                                    paste0(data$name, collapse = ", "),
                                    paste0(data$rxn, collapse = ", "),
                                    paste(data$date_range, collapse = " to ")),
                         stringsAsFactors = FALSE)
    result$values["" == result$values] <- "Not Specified"
    result
  },
  include.colnames = FALSE
  )
  
  ##### Create time plot  ###
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
  #renderGvis({
  output$mychart <- renderLineChart({ 
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

    results
    #print(results)



    # gvisLineChart(results,
    #               xvar = "month",
    #               yvar = c("total", "serious", "death"),
    #               options = list(
    #                 height = 350,
    #                 vAxis = "{title: 'Number of Reports'}",
    #                 hAxis = "{title: 'Date Received (grouped by month)'}",
    #                 chartArea = "{top: 10, height: '80%', left: 120, width: '84%'}",
    #                 colors = colorCodeToString(google_colors[c(18, 13, 2)])
    #               ))
  })
  
  ##### Data about Reports
  output$reporterplot <- renderGvis({
    reporter_results <- subset_cv$report %>%
      count(REPORTER_TYPE_ENG) %>%
      as.data.frame()
    reporter_results$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == ""] <- "Not reported"
    reporter_results$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == "Consumer Or Other Non Health Professional"] <- "Consumer or non-health professional"
    reporter_results$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == "Other Health Professional"] <- "Other health professional"
    # data <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
    
    gvisPieChart_HCSC(reporter_results, "REPORTER_TYPE_ENG", "count")
  })
  output$reporterplot.table <- renderGvis({
    reporter_results <- subset_cv$report %>%
      count(REPORTER_TYPE_ENG) %>%
      as.data.frame()
    reporter_results$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == ""] <- "Not reported"
    reporter_results$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == "Consumer Or Other Non Health Professional"] <- "Consumer or non-health professional"
    reporter_results$REPORTER_TYPE_ENG[reporter_results$REPORTER_TYPE_ENG == "Other Health Professional"] <- "Other health professional"
    # data <- reports_tab(current_generic="ampicillin",current_brand="PENBRITIN",current_rxn="Urticaria",date_ini=ymd("19650101"),date_end=ymd("20151231"))
    
    gvisTable(reporter_results)
  })
  output$seriousplot <- renderGvis({
    serious_results <- subset_cv$report %>%
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
  output$seriousplot.table <- renderGvis({
    serious_results <- subset_cv$report %>%
      count(SERIOUSNESS_ENG) %>%
      select(SERIOUSNESS_ENG, n) %>%
      as.data.frame() %>%
      mutate(label = NA,
             label = ifelse(SERIOUSNESS_ENG == "Yes", "Serious", label),
             label = ifelse(SERIOUSNESS_ENG == "No", "Non-serious", label),
             label = ifelse(SERIOUSNESS_ENG == "", "Not reported", label)) %>%
      select(label, n) %>%
      slice(match(c("Serious", "Non-serious", "Not reported"), label))
    
    gvisTable(serious_results)
  })
  output$seriousreasonsplot <- renderGvis({
    data <- subset_cv$report %>%
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
    data <- subset_cv$report %>%
      count(GENDER_ENG) %>%
      as.data.frame()
    # replace blank in GENDER_ENG with character "Unknown"
    data$GENDER_ENG[data$GENDER_ENG == ""] <- "Not specified"
    sex_results <- count(data, GENDER_ENG, wt = n)
    
    gvisPieChart_HCSC(sex_results, "GENDER_ENG", "n")
  })
  output$sexplot.table <- renderGvis({
    data <- subset_cv$report %>%
      count(GENDER_ENG) %>%
      as.data.frame()
    # replace blank in GENDER_ENG with character "Unknown"
    data$GENDER_ENG[data$GENDER_ENG == ""] <- "Not specified"
    sex_results <- count(data, GENDER_ENG, wt = n)
    
    gvisTable(sex_results)
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
    
    gvisPieChart_HCSC(data, "AGE_GROUP_CLEAN", "n")
  })
  output$agegroupplot.table <- renderGvis({
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
    
    gvisTable(data)
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
      geom_histogram(breaks = seq(0, 100, by = 2)) +
      scale_fill_manual(values = colours_df$colours) +
      xlab("Age at onset (years)") +
      ylab("Number of Reports") +
      theme_bw()
    ggplotly(hist)
  })
  
  #### Data about Drugs
  indication_data <- reactive({
    # Data frame used to obtain Top_25_indication bar chart: Indication is only associated with individual drug
    # When brand name is unspecified, chart shows top 25 indications associated with all drugs + date_range
    # When brand name is specified, chart shows top 25 indications associated with specified drug + date_range
    
    
    # NOTE ABOUT INDICATIONS STRUCTURE:
    # REPORT_ID -> multiple drugs per report
    # DRUG_ID -> multiple reports may use the same drugs
    # REPORT_DRUG_ID -> unique for each drug/report combination. count is less than total reports since drugs can have multiple indications
    # so distinct REPORT_DRUG_ID x INDICATION_NAME_ENG includes the entire set of reports
    indications_sorted <- subset_cv$drug %>%
      count(INDICATION_NAME_ENG) %>%
      arrange(desc(n)) %>%
      as.data.frame() %>%
      filter(!is.na(INDICATION_NAME_ENG)) %>%
      head(25)
  })
  
  output$indication_plot <- renderGvis({
    gvisBarChart_HCSC(indication_data(), "INDICATION_NAME_ENG", "n", google_colors[1])
  })
  output$indication_plot.table <- renderGvis({
    gvisTable(indication_data())
  })
  output$indication_plot.sus <- renderGvis({
    gvisBarChart_HCSC(indication_data(), "INDICATION_NAME_ENG", "n", google_colors[1])
  })
  output$indication_plot.table.sus <- renderGvis({
    gvisTable(indication_data())
  })
  output$indication_plot.con <- renderGvis({
    gvisBarChart_HCSC(indication_data(), "INDICATION_NAME_ENG", "n", google_colors[1])
  })
  output$indication_plot.table.con <- renderGvis({
    gvisTable(indication_data())
  })
  
  suspect_data <- reactive({
    data <- subset_cv$drug %>%
      filter(DRUGINVOLV_ENG == "Suspect") %>%
      distinct(REPORT_ID, DRUGNAME) %>%
      count(DRUGNAME) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
  })
  
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
    data <- subset_cv$drug %>%
      filter(DRUGINVOLV_ENG == "Concomitant") %>%
      distinct(REPORT_ID, DRUGNAME) %>%
      count(DRUGNAME) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
  })
  
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
  
  
  all_data <- reactive({
    data <- subset_cv$drug %>%
      distinct(REPORT_ID, DRUGNAME) %>%
      count(DRUGNAME) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
  })
  
  output$all_drugs <- renderGvis({
    # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name
    #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq)
    data <- all_data()
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    gvisBarChart_HCSC(data, "DRUGNAME", "n", google_colors[2])
  })
  
  output$all_drugs.table <- renderGvis({
    data <- all_data()
    gvisTable(data)
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
  
  drugcount_data <- reactive({
    data <- subset_cv$drug %>%
      count(REPORT_ID) %>%
      filter(n <= 20) %>%
      select(n) %>%
      as.data.frame()
  })
  
  output$drugcount_plot <- renderGvis({
    # the top drugs reported here might be influenced by such drug is originally most reported among all reports
    gvisHistogram(drugcount_data(), options = list(
      legend = "{ position: 'none' }",
      height = 300,
      vAxis = "{title: 'Number of Reports'}",
      hAxis = "{title: 'Number of Drugs in Report'}",
      chartArea = "{top: 20, height: '75%', left: 80, width: '90%'}",
      histogram = "{ hideBucketItems: true, bucketSize: 1 }"))
  })
  
  
  #### Data about Reactions
  
  top_pt_data <- reactive({
    data <- subset_cv$rxn %>%
      count(PT_NAME_ENG) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
  })
  
  output$top_pt <- renderGvis({
    gvisBarChart_HCSC(top_pt_data(), "PT_NAME_ENG", "n", google_colors[1])
  })
  output$top_pt.table <- renderGvis({
    gvisTable(top_pt_data())
  })
  
  top_hlt_data <- reactive({
    data <- subset_cv$rxn %>%
      filter(!is.na(HLT_Term)) %>%
      count(HLT_Term) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      as.data.frame()
  })
  output$top_hlt <- renderGvis({
    gvisBarChart_HCSC(top_hlt_data(), "HLT_Term", "n", google_colors[2])
  })
  output$top_hlt.table <- renderGvis({
    gvisTable(top_hlt_data())
  })
  
  outcomeplot_data <- reactive({
    data <- subset_cv$report %>%
      count(OUTCOME_ENG) %>%
      as.data.frame()
  })
  output$outcomeplot <- renderGvis({
    gvisPieChart_HCSC(outcomeplot_data(), "OUTCOME_ENG", "n")
  })
  output$outcomeplot.table <- renderGvis({
    gvisTable(outcomeplot_data())
  })
  
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