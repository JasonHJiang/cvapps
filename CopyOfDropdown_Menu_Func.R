library(dplyr)
library(plyr)
library(lubridate)
n =1000

############## Function to create top 1000 terms included in the dropdown menu #############
topingd_func <- function(n){
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  #cv_reports_dm <- cv_reports %>% select(REPORT_ID)
  
  #cv_report_drug_dm <- cv_report_drug %>% select(REPORT_ID, DRUG_PRODUCT_ID)
  
  #cv_drug_product_ingredients_dm <- cv_drug_product_ingredients %>% select(DRUG_PRODUCT_ID, ACTIVE_INGREDIENT_NAME)
  
  #ingd_master <-  cv_reports_dm %>% left_join(cv_report_drug_dm) %>% left_join(cv_drug_product_ingredients_dm) 
  
  topingd <-  dplyr::summarise(group_by(cv_drug_product_ingredients, ACTIVE_INGREDIENT_NAME),count=n_distinct(REPORT_ID)) 
  topingd_final <-  topingd %>% arrange(desc(count)) %>% top_n(n) %>% dplyr::select(ACTIVE_INGREDIENT_NAME)%>% as.data.table()
  return(topingd_final)
}

topdrug_func <- function(n){
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  #cv_reports_dm <- cv_reports %>% select(REPORT_ID)
  
  cv_report_drug_dm <- cv_report_drug %>% dplyr::select(REPORT_ID, DRUGNAME)
  
  #drugs_master <-cv_reports_dm %>% left_join(cv_report_drug_dm) 
  
  topdrugs <- dplyr::summarise(group_by(cv_report_drug_dm, DRUGNAME),count=n_distinct(REPORT_ID)) %>% as.data.table()
  topdrugs_final <- topdrugs %>% arrange(desc(count)) %>% top_n(n) %>% dplyr::select(DRUGNAME)

  # topdrugs_final <- cv_report_drug %>% 
  #                     group_by(DRUGNAME) %>%
  #                     summarize(count = n_distinct(REPORT_ID)) %>%
  #                     top_n(100) %>%
  #                     arrange(desc(count)) %>%
  #                     collect()
  
  return(topdrugs_final)
} 

toprxn_func <- function(n){
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  #cv_reports_dm <- cv_reports %>% select(REPORT_ID)
  #cv_reports_dm1 <- as.data.frame(cv_reports_dm,n=-1)
  
  cv_reactions_dm <- cv_reactions %>% select(REPORT_ID, PT_NAME_ENG)

  
  #rxn_master <- cv_reports_dm %>% left_join(cv_reactions_dm) 
  
  toprxn <- dplyr::summarise(group_by(cv_reactions_dm, PT_NAME_ENG),count=n_distinct(REPORT_ID)) %>% as.data.table()
  toprxn_final <- toprxn %>% arrange(desc(count)) %>% top_n(100) %>% select(PT_NAME_ENG)
  
  # toprxn_final <- cv_reactions %>% 
  #                 group_by(PT_NAME_ENG) %>%
  #                 summarize(count = n_distinct(REPORT_ID)) %>%
  #                 top_n(100) %>%
  #                 arrange(desc(count)) %>%
  #                 collect()
  
  
  return(toprxn_final)
  #test <- cv_reports_dm1 %>% left_join(cv_reactions_dm1)
}







  
