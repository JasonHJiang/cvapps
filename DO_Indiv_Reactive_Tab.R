#########################################################################################################################################  
# # Data frame generate reactive function to be used to assign: data <- cv_reports_tab() 
# cv_reports_tab <- reactive({
#   input$searchButton
#   #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
#   current_brand <- isolate(ifelse(input$search_brand == "",
#                                   NA,
#                                   input$search_brand)) 
#   current_rxn <- isolate(ifelse(input$search_rxn == "",
#                                 NA,
#                                 input$search_rxn)) 
#   current_date_range <- isolate(input$searchDateRange)
#   #date_ini <- isolate(as.POSIXct(input$search_date_ini))
#   #date_end <- isolate(as.POSIXct(input$search_date_end))
#   
#   
#   escape.POSIXt <- dplyr:::escape.Date
#   
#   setwd("~/CV_Shiny_DrugTabOnly")
#   source("DO_Reports_Tab_Func SHe.R")
#   reports_tab_df <- reports_tab(current_brand=current_brand,current_rxn=current_rxn,current_date_range=current_date_range)
#   
#   return(reports_tab_df)
# })
# 
# # sample datasets of what is being graphed/used
# #output$outputReports <- renderTable({
# #  cv_reports_tab()[1:4,c("ACTIVE_INGREDIENT_NAME","DRUGNAME","DATINTRECEIVED_CLEAN","PT_NAME_ENG")]
# #})
# 
# hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
# cv_search_tab <- reactive({
#   input$searchButton
#   #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
#   current_brand <- isolate(ifelse(input$search_brand == "",
#                                   NA,
#                                   input$search_brand)) 
#   current_rxn <- isolate(ifelse(input$search_rxn == "",
#                                 NA,
#                                 input$search_rxn)) 
#   current_date_range <- isolate(input$searchDateRange)
#   #date_ini <- isolate(input$search_date_ini)
#   #date_end <- isolate(input$search_date_end)
#   
#   escape.POSIXt <- dplyr:::escape.Date
#   
#   search_tab_df <- data.frame(names = c("Brand Name:", 
#                                         "Adverse Reaction Term:",
#                                         "Date Range:"),
#                               terms = c(current_brand,current_rxn,paste(current_date_range[1]," to ", current_date_range[2])),
#                               stringsAsFactors=FALSE)
#   #paste(date_ini, " to ",date_end)
#   search_tab_df$terms[is.na(search_tab_df$terms) == TRUE] <- "Not Specified"
#   return(search_tab_df)
# })
# 
# hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
# cv_patients_tab <- reactive({
#   input$searchButton
#   #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
#   current_brand <- isolate(ifelse(input$search_brand == "",
#                                   NA,
#                                   input$search_brand)) 
#   current_rxn <- isolate(ifelse(input$search_rxn == "",
#                                 NA,
#                                 input$search_rxn)) 
#   current_date_range <- isolate(input$searchDateRange)
#   #date_ini <- isolate(as.POSIXct(input$search_date_ini))
#   #date_end <- isolate(as.POSIXct(input$search_date_end))
#   
#   escape.POSIXt <- dplyr:::escape.Date
#   
#   setwd("~/CV_Shiny_DrugTabOnly")
#   source("DO_Patients_Tab_Func SHe.R")
#   
#   patients_tab_df <- patients_tab(current_brand=current_brand,current_rxn=current_rxn,current_date_range=current_date_range)
#   
#   return(patients_tab_df)
# })
# 
# hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
# cv_drug_tab_topdrg <- reactive({
#   input$searchButton
#   #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
#   current_brand <- isolate(ifelse(input$search_brand == "",
#                                   NA,
#                                   input$search_brand)) 
#   current_rxn <- isolate(ifelse(input$search_rxn == "",
#                                 NA,
#                                 input$search_rxn)) 
#   current_date_range <- isolate(input$searchDateRange)
#   #date_ini <- isolate(as.POSIXct(input$search_date_ini))
#   #date_end <- isolate(as.POSIXct(input$search_date_end))
#   
#   escape.POSIXt <- dplyr:::escape.Date
#   
#   setwd("~/CV_Shiny_DrugTabOnly")
#   source("DO_Drugs_Tab_Func SHe.R")
#   
#   drugs_tab_topdrg_df <- drugs_tab_topdrg(current_brand=current_brand,current_rxn=current_rxn,current_date_range=current_date_range)
#   
#   
#   #drugs <- ddply(drugs_tab_df,"DRUGNAME", count_func) 
#   #drugs_sorted <- drugs[order(desc(drugs$n)),] 
#   #drugs_sorted <- drugs %>% arrange(desc(n)) %>% top_n(25)
#   
#   return(drugs_tab_topdrg_df)
# })
# 
# hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
# cv_drug_tab_indc <- reactive({
#   input$searchButton
#   #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
#   current_brand <- isolate(ifelse(input$search_brand == "",
#                                   NA,
#                                   input$search_brand)) 
#   current_rxn <- isolate(ifelse(input$search_rxn == "",
#                                 NA,
#                                 input$search_rxn)) 
#   current_date_range <- isolate(input$searchDateRange)
#   #date_ini <- isolate(as.POSIXct(input$search_date_ini))
#   #date_end <- isolate(as.POSIXct(input$search_date_end))
#   
#   escape.POSIXt <- dplyr:::escape.Date
#   
#   setwd("~/CV_Shiny_DrugTabOnly")
#   source("DO_Drugs_Tab_Func SHe.R")
#   
#   drugs_tab_indt_df <- drugs_tab_indt(current_brand=current_brand,current_rxn=current_rxn,current_date_range=current_date_range)
#   
#   return(drugs_tab_indt_df)
# })
# 
# hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
# cv_reactions_tab <- reactive({
#   input$searchButton
#   #codes about select specific generic, brand and reaction name in search side bar, making sure they're not NA
#   current_brand <- isolate(ifelse(input$search_brand == "",
#                                   NA,
#                                   input$search_brand)) 
#   current_rxn <- isolate(ifelse(input$search_rxn == "",
#                                 NA,
#                                 input$search_rxn)) 
#   current_date_range <- isolate(input$searchDateRange)
#   #date_ini <- isolate(as.POSIXct(input$search_date_ini))
#   #date_end <- isolate(as.POSIXct(input$search_date_end))
#   
#   escape.POSIXt <- dplyr:::escape.Date
#   
#   setwd("~/CV_Shiny_DrugTabOnly")
#   source("DO_Reactions_Tab_Func SHe.R")
#   
#   reactions_tab_df <- reactions_tab(current_brand=current_brand,current_rxn=current_rxn,current_date_range=current_date_range)
#   
#   return(reactions_tab_df)
# })
#########################################################################################################################################