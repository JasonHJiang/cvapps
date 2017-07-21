server <- function(input, output, session) {
  # Relabel rxns dropdown menu based on selected drug
  observeEvent(input$search_drug, {
    hlt_choices <- drug_PT_HLT
    pt_choices <- drug_PT_HLT
    if (all("" != input$search_drug)) {
      if (length(input$search_drug) == 1) {
        hlt_choices %<>% filter(ing == input$search_drug)
        pt_choices %<>% filter(ing == input$search_drug)
      } else {
        hlt_choices %<>% filter(ing %in% c(input$search_drug))
        pt_choices %<>% filter(ing %in% c(input$search_drug))
      }
    }
    soc_choices <- pt_choices %>% distinct()
    
    hlt_choices %<>% distinct(HLT_NAME_ENG) %>% as.data.frame() %>% `[[`(1) %>% sort()
    pt_choices %<>% distinct(PT_NAME_ENG) %>% as.data.frame() %>% `[[`(1) %>% sort()
    updateSelectizeInput(session, "search_hlt",
                         choices = c("Start typing to search..." = "", hlt_choices))
    updateSelectizeInput(session, "search_pt",
                         choices = c("Start typing to search..." = "", pt_choices))
  })
  observeEvent(c(input$search_button,input$search_hlt),{
    if(is.null(input$search_pt)){
      updateTabsetPanel(session, "tabbox",selected ="panel2")
    }else {
      updateTabsetPanel(session, "tabbox",selected ="panel1")
    } 
  
    })
  observeEvent(input$search_hlt, {
    if (input$checkbox_filter_pt) {
      pt_choices <- drug_PT_HLT
      
      if (all("" != input$search_hlt) & !is.null(input$search_hlt)) {
        if (length(input$search_hlt) == 1) {
          pt_choices %<>% filter(HLT_NAME_ENG == input$search_hlt)
        } else {
          pt_choices %<>% filter(HLT_NAME_ENG %in% c(input$search_hlt))
        }
      }
      if (all("" != input$search_drug) & !is.null(input$search_drug)) {
        if (length(input$search_drug) == 1) {
          pt_choices %<>% filter(ing == input$search_drug)
        } else {
          pt_choices %<>% filter(ing %in% c(input$search_drug))
        }
      }
      
      pt_choices %<>% distinct(PT_NAME_ENG) %>% as.data.frame() %>% `[[`(1) %>% sort()
      updateSelectizeInput(session, "search_pt",
                           choices = c("Start typing to search..." = "", pt_choices))
    }}, ignoreNULL = FALSE)
  
  
  
  observeEvent(input$checkbox_filter_pt, {
    pt_choices <- drug_PT_HLT
    print(input$search_drug)
    if (input$checkbox_filter_pt) {
      if (all("" != input$search_hlt) & !is.null(input$search_hlt)) {
        if (length(input$search_hlt) == 1) {
          pt_choices %<>% filter(HLT_NAME_ENG == input$search_hlt)
        } else {
          pt_choices %<>% filter(HLT_NAME_ENG %in% c(input$search_hlt))
        }
      }
    }
    if (all("" != input$search_drug) & !is.null(input$search_drug)) {
      if (length(input$search_drug) == 1) {
        pt_choices %<>% filter(ing == input$search_drug)
      } else {
        pt_choices %<>% filter(ing %in% c(input$search_drug))
      }
    }
    pt_choices %<>% distinct(PT_NAME_ENG) %>% as.data.frame() %>% `[[`(1) %>% sort()
    updateSelectizeInput(session, "search_pt",
                         choices = c("Start typing to search..." = "", pt_choices))
  })
  
  
  
  
  ########## Reactive data processing
  # Data structure to store current query info
  current_search <- reactive({
    input$search_button
    isolate({
      withProgress(message = 'Calculation in progress', value = 0, {
        
        min_count <- as.numeric(input$min_count)
        if (is.na(min_count) | min_count < 0) min_count = 0
        min_count <- floor(min_count)
        updateTextInput(session, "min_count", value = min_count)
        incProgress(1/3)
        
        min_exp <- as.numeric(input$min_exp)
        if (is.na(min_exp) | min_exp < 0) min_exp = 0
        updateTextInput(session, "min_exp", value = min_exp)
        incProgress(1/3)
        
        list(min_count = min_count,
             min_exp = min_exp,
             drug = input$search_drug,
             hlt = input$search_hlt,
             pt = input$search_pt,
             filter_inf = input$inf_filter,
             display_total_pt = input$display_total_pt,
             display_total_hlt = input$display_total_hlt)
      })
    })
    
    
    
  })
  
  
  
  
  
  ########## Output
  # Display what query was searched
  output$current_search <- renderTable({
    data <- current_search()
    print(data)
    result <- data.frame(names = c("Generic Name:",
                                   "High-Level Term:",
                                   "Preferred Term:"),
                         terms = c(paste0(data$drug, collapse = ", "),
                                   paste0(data$hlt, collapse = ", "),
                                   paste0(data$pt, collapse = ", ")),
                         stringsAsFactors=FALSE)
    result["" == result] <- "Not Specified"
    result
  }, include.colnames = FALSE)
  output$pt_data_dl <- downloadHandler(
    filename = function() {
      current_drug <- current_search()$drug
      if (current_drug == "") current_drug <- "all"
      current_drug <- gsub(" ", "_", current_drug)
      current_drug <- gsub("\\|", "-", current_drug)
      paste0('pt_data_', current_drug, '.csv')
    },
    content = function(file) {
      write.csv(table_pt_data(), file, row.names=FALSE)
    }
  )
  output$hlt_data_dl <- downloadHandler(
    filename = function() {
      current_drug <- current_search()$drug
      if (current_drug == "") current_drug <- "all"
      current_drug <- gsub(" ", "_", current_drug)
      current_drug <- gsub("\\|", "-", current_drug)
      paste0('hlt_data_', current_drug, '.csv')
    },
    content = function(file) {
      write.csv(table_hlt_data(), file, row.names=FALSE)
    }
  )
  
  # PRR tab
  table_pt_data <- reactive({
#    if(input$table_selection=="All"){
#     table <- master_table_pt
#   }else if (input$table_selection=="PT seen before"){
#     table<-master_table_pt%>%filter(count<5)
#   }else {
#     table<-master_table_pt%>%filter(count>5)
#   }
    
    table<-master_table_pt
    
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
      data <- current_search()
      print(data)
      if (is.null(data$drug)) data$drug = ""
      if (is.null(data$pt)) data$pt = ""
      if (is.null(data$hlt)) data$hlt = ""
      # PRR and ROR values of Inf means there are no other drugs associated with that specific adverse reaction, so denomimator is zero!
      # prr_tab_df is simply the table displayed at the bottom
      
      
      #if (all(data$drug != "")) table %<>% filter(drug_code == data$drug %>% as.data.frame())
      drugs <- data$drug %>% as.data.frame()
      names(drugs) <- 'drug_code'
      if (all(drugs != "")) table %<>% semi_join(drugs,copy=TRUE)
      
      pts <- data$pt %>%as.data.frame()
      names(pts) <- 'event_effect'
      if (all(pts != "")) table%<>% semi_join(pts,copy=TRUE)
      
      if (data$filter_inf) table%<>% filter(PRR != Inf)
      table %>% filter(count >= data$min_count) %>%
        filter(expected_count >= data$min_exp) %>%
        arrange(desc(median_IC), desc(LB95_IC), drug_code, event_effect) %>%
        as.data.table() %>%
        lapply(function(x) {if (is.numeric(x)) round(x,3) else x}) %>%
        as.data.table()
    })
    
    
  })
  
  # pass either datatable object or data to be turned into one to renderDataTable
  output$table_pt <- DT::renderDataTable(DT::datatable(
    table_pt_data(),
    extensions = 'Buttons',
    options = list(
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = list(list(extend = 'colvis',
                          text = 'Columns to display',
                          columns = 5:25)),
      columnDefs = list(list(visible = FALSE,
                             targets = c(5:6, 9:10, 16:17, 19:20, 22:23)))
    )))
  # 
  table_hlt_data <- reactive({
    input$search_button # hacky way to get eventReactive but also initial load
    isolate({
      data <- current_search()
      if (is.null(data$drug)) data$drug = ""
      if (is.null(data$pt)) data$pt = ""
      if (is.null(data$hlt)) data$hlt = ""
      # PRR and ROR values of Inf means there are no other drugs associated with that specific adverse reaction, so denomimator is zero!
      # prr_tab_df is simply the table displayed at the bottom
      table <- master_table_hlt
      drugs <- data$drug %>% as.data.frame()
      names(drugs) <- 'drug_code'
      if (all(drugs != "")) table %<>% semi_join(drugs,copy=TRUE)
      
      hlts <- data$hlt %>% as.data.frame()
      names(hlts) <- 'event_effect'
      if (all(hlts != "")) table %<>% semi_join(hlts,copy=TRUE)
      
      if (data$filter_inf) table %<>% filter(PRR != Inf)
      table %<>% filter(count >= data$min_count) %>%
        filter(expected_count >= data$min_exp) %>%
        arrange(desc(median_IC), desc(LB95_IC), drug_code, event_effect) %>%
        as.data.table() %>%
        lapply(function(x) {if (is.numeric(x)) round(x,3) else x}) %>%
        as.data.table()
    })
  })
  
  # pass either datatable object or data to be turned into one to renderDataTable
  output$table_hlt <- DT::renderDataTable(DT::datatable(
    table_hlt_data(),
    extensions = 'Buttons',
    options = list(
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = list(list(extend = 'colvis',
                          text = 'Columns to display',
                          columns = 5:25)),
      columnDefs = list(list(visible = FALSE,
                             targets = c(5:6, 9:10, 16:17, 19:20, 22:23)))
    )))
  
  # time-series data
  time_data_pt <- reactive({
    cur_search <- current_search()
    
    top_pairs <- table_pt_data() %>% head(10) %>% select(drug_code, event_effect) %>%
      mutate(drug_code = as.character(drug_code), event_effect = as.character(event_effect))
    timeplot_df <- count_quarter_pt %>% semi_join(top_pairs, by = c("ing" = "drug_code", "PT_NAME_ENG" = "event_effect"))
    quarters_df <- quarters %>% cbind(n = 0)
    if (nrow(top_pairs) > 0) {
      pairs_df <- top_pairs %>% rename(ing = drug_code, PT_NAME_ENG = event_effect) %>% cbind(n = 0)
    } else {
      showModal(modalDialog(
        title = list(icon("exclamation-triangle"), "No results found!"),
        "There were no reports matching your query.",
        size = "s",
        easyClose = TRUE))
      pairs_df <- data.frame(ing = NA, PT_NAME_ENG = NA, n = NA)# top_pairs %>% rename(ing = drug_code, PT_NAME_ENG = event_effect) %>% 
    }
    filled_time_df <- full_join(pairs_df, quarters_df, by = "n") %>%
      bind_rows(timeplot_df) %>%
      count(ing, PT_NAME_ENG, quarter, wt = n) %>%
      ungroup() %>%
      mutate(label = paste0(ing, "_", PT_NAME_ENG)) %>%
      select(label, quarter, nn)
    if (cur_search$display_total_pt & is.null(cur_search$pt)) {
      total_df <- count_quarter_pt
      if (!is.null(cur_search$drug)) total_df %<>% filter(ing == cur_search$drug)
      if (!is.null(cur_search$pt)) total_df %<>% filter(PT_NAME_ENG == cur_search$pt)
      total_df %<>% count(quarter, wt = n) %>%
        mutate(label = paste0("total for query")) %>%
        select(label, quarter, nn)
      filled_time_df <- total_df
    }
    
    filled_time_df %<>% rename(n = nn)
    
    
    
    # filled_time_df <- timeplot_df %>%
    #   mutate(label = paste0(ing, "_", PT_NAME_ENG)) %>%
    #   select(-ing, -PT_NAME_ENG) %>%
    #   spread(label, count)
    # filled_time_df[is.na(filled_time_df)] <- 0
    # filled_time_df
  })
  
  output$current_pt_title<- renderText({
    cur_search <- current_search()
    paste("Non-Cumulative Report Count Time Plot for:", 
          paste0(if(is.null(cur_search$drug)) {"All Drugs"} else {cur_search$drug}, collapse = ", "), "&",
          paste0(if(is.null(cur_search$pt)) {"Top 10 Reactions with Highest IC Estimates"} else {cur_search$pt}, collapse = ", "))
  })
  
  time_data_pt %>% mutate(qtr = as.yearqtr(quarter %>% as.character(), '%Y.%q')) %>%
    ggvis(~qtr, ~n) %>% 
    add_axis("x", title = "Quarter", properties = axis_props(
      label = list(angle = 330))) %>%
    add_axis("y", title = "Report Count") %>%
    
    add_tooltip(function(data){paste0("PT: ", data$label, "<br>", 
                                      "Count: ", as.character(data$n))}, 
                "hover") %>%
    layer_points(fill = ~label, stroke := "black") %>%
    group_by(label) %>%
    layer_paths(stroke = ~label) %>%
    set_options(width = 'auto') %>%  bind_shiny("timeplot_pt", "data")
  
  
  
  # Time Plot
  # output$timeplot_pt <- renderPlot({
  #   data <- current_search()
  # 
  #   current_drug <- ifelse(data$drug == "", "All Drugs", data$drug)
  #   current_rxn <- ifelse(data$pt == "", "Top 10 Reactions with Highest IC Estimates", data$pt)
  #   plottitle <- paste("Non-Cumulative Report Count Time Plot for:", current_drug, "&", current_rxn)
  # 
  #   df <- time_data_pt() %>%
  #     #   mutate(qtr = (quarter%%1 - 0.1)*2.5 + quarter%/%1)
  #     # gvisLineChart(df,
  #     #               xvar = "qtr",
  #     #               yvar = names(df)[2:11]
  #     #               options = list(
  #     #                 height = 350,
  #     #                 vAxis = "{title: 'Number of Reports'}",
  #     #                 hAxis = "{title: 'Month'}",
  #     #                 chartArea = "{top: 10, height: '80%', left: 120, width: '84%'}")
  #     # )
  #     mutate(qtr = as.yearqtr(quarter %>% as.character(), '%Y.%q'))
  #   # p <- ggplot(df, aes(x = qtr, y = n, label = label)) +
  #   #   scale_x_yearqtr(breaks = seq(min(df$qtr), max(df$qtr), 0.25),
  #   #                   format = "%Y Q%q") +
  #   #   geom_line(aes(colour=label)) + geom_point()  +
  #   #   ggtitle(plottitle) +
  #   #   xlab("Quarter") +
  #   #   ylab("Report Count") +
  #   #   theme_bw() +
  #   #   theme(plot.title = element_text(face="bold", hjust=0.1, size = rel(0.75)),
  #   #         axis.text.x = element_text(angle=30, vjust=0.9, hjust=1, size=rel(0.75)),
  #   #         axis.title = element_text(size = rel(0.75)),
  #   #         axis.title.x = element_text(vjust = 0))
  #   # p <- plot_ly(df, x = ~qtr, y = ~n, mode = 'lines+markers') %>%
  #   #      layout(title = plottitle,
  #   #             font = list(size = 8))
  # 
  # 
  #   print(p)
  # 
  # })
  
  # time-series data
  time_data_hlt <- reactive({
    cur_search <- current_search()
    
    
    top_pairs <- table_hlt_data() %>% head(10) %>% select(drug_code, event_effect) %>%
      mutate(drug_code = as.character(drug_code), event_effect = as.character(event_effect))
    timeplot_df <- count_quarter_hlt %>% semi_join(top_pairs, by = c("ing" = "drug_code", "HLT_NAME_ENG" = "event_effect"))
    quarters_df <- quarters %>% cbind(n = 0)
    if (nrow(top_pairs) > 0) {
      pairs_df <- top_pairs %>% rename(ing = drug_code, HLT_NAME_ENG = event_effect) %>% cbind(n = 0)
    } else {
      showModal(modalDialog(
        title = list(icon("exclamation-triangle"), "No results found!"),
        "There were no reports matching your query.",
        size = "s",
        easyClose = TRUE))
      pairs_df <- data.frame(ing = NA, HLT_NAME_ENG = NA, n = NA)
    }
    filled_time_df <- full_join(pairs_df, quarters_df, by = "n") %>%
      bind_rows(timeplot_df) %>%
      count(ing, HLT_NAME_ENG, quarter, wt = n) %>%
      ungroup() %>%
      mutate(label = paste0(ing, "_", HLT_NAME_ENG)) %>%
      select(label, quarter, nn)
    if (cur_search$display_total_hlt & is.null(cur_search$hlt)) {
      total_df <- count_quarter_hlt
      if (!is.null(cur_search$drug)) total_df %<>% filter(ing == cur_search$drug)
      if (!is.null(cur_search$hlt)) total_df %<>% filter(HLT_NAME_ENG == cur_search$hlt)
      total_df %<>% count(quarter, wt = n) %>%
        mutate(label = paste0("total for query")) %>%
        select(label, quarter, nn)
      filled_time_df <- total_df
    }
    
    filled_time_df %<>% rename(n = nn)
  })
  
  # output$timeplot_hlt <- renderPlot({
  #   input$search_button # hacky way to get eventReactive but also initial load
  #   isolate({
  #     current_drug <- ifelse(is.null(input$search_drug),"All Drugs",input$search_drug)
  #     current_rxn <- ifelse(is.null(input$search_hlt),"Top 10 Reactions with Highest IC Estimates",input$search_hlt)
  #   })
  #   plottitle <- paste("Non-Cumulative Report Count Time Plot for:", current_drug, "&", current_rxn)
  #   
  #   df <- time_data_hlt() %>%
  #     mutate(qtr = as.yearqtr(quarter %>% as.character(), '%Y.%q'))
  # 
  #   p <- ggplot(df, aes(x = qtr, y = n)) +
  #     scale_x_yearqtr(breaks = seq(min(df$qtr), max(df$qtr), 0.25),
  #                     format = "%Y Q%q") +
  #     ifelse(sum(df$n) == 0, geom_blank(), geom_line(aes(colour=label)) + geom_point()) +
  #     ggtitle(plottitle) +
  #     xlab("Quarter") +
  #     ylab("Report Count") +
  #     theme_bw() +
  #     theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=30, vjust=0.9, hjust=1))
  #   print(p)
  # })
  
  output$current_hlt_title<- renderText({
    cur_search <- current_search()
    paste("Non-Cumulative Report Count Time Plot for:", 
          paste0(if(is.null(cur_search$drug)) {"All Drugs"} else {cur_search$drug}, collapse = ", "), "&",
          paste0(if(is.null(cur_search$hlt)) {"Top 10 Reactions with Highest IC Estimates"} else {cur_search$hlt}, collapse = ", "))
  })
  
  time_data_hlt %>% mutate(qtr = as.yearqtr(quarter %>% as.character(), '%Y.%q')) %>%
    ggvis(~qtr, ~n) %>% 
    add_axis("x", title = "Quarter", properties = axis_props(
      label = list(angle = 330))) %>%
    add_axis("y", title = "Report Count") %>%
    
    add_tooltip(function(data){paste0("HLT: ", data$label, "<br>", 
                                      "Count: ", as.character(data$n))}, 
                "hover") %>%
    layer_points(fill = ~label, stroke := "black") %>%
    group_by(label) %>%
    layer_paths(stroke = ~label) %>%
    set_options(width = 'auto') %>%  bind_shiny("timeplot_hlt", "data")
  
  
}