# Tables used: CV_reports (REPORT_ID, DATINTRECEIVED_CLEAN)
#              CV_Report_Drug (REPORT_ID, DRUGNAME)


reactions_tab <- function(current_brand,current_rxn,current_date_range) { 
  # connect to CV database
  #hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  # Import tables with particular search items with method to deal with unspecified search term
  cv_reports_sorted_rxn <- cv_reports %>%
                            select(REPORT_ID, DATINTRECEIVED_CLEAN, OUTCOME_ENG) %>%
                            filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])
                            #filter(DATINTRECEIVED_CLEAN >= date_ini) %>%
                            #filter(DATINTRECEIVED_CLEAN <= date_end)
  
  cv_report_drug_rxn <- if(is.na(current_brand) == FALSE){
                          cv_report_drug %>%
                            select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME) %>%
                            filter(DRUGNAME == current_brand)
                        } else {
                          cv_report_drug %>%
                            select(REPORT_ID,DRUG_PRODUCT_ID,DRUGNAME)
                        }
  cv_reactions_rxn <- if(is.na(current_rxn) == FALSE){
                        cv_reactions %>%
                          select(REPORT_ID, PT_NAME_ENG) %>%
                          filter(PT_NAME_ENG == current_rxn)
                      } else {
                        cv_reactions %>%
                          select(REPORT_ID, PT_NAME_ENG)
                      }
  
  # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each OUTCOME_ENG 
  #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq)
  reactions_tab_master <- cv_reports_sorted_rxn %>%
                          semi_join(cv_report_drug_rxn) %>%
                          semi_join(cv_reactions_rxn) %>%
                          select(REPORT_ID, OUTCOME_ENG) %>%
                          collect()
  
  return(reactions_tab_master)
}