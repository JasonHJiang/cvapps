# For Top_25_Indication: cv_reports (REPORT_ID, DATINTRECEIVED_CLEAN)
#                        cv_report_drug (REPORT_ID,DRUGNAME)
#                        cv_report_drug_indication (REPORT_ID, INDICATION_NAME_ENG)

drugs_tab_indt <- function(current_brand, current_rxn,current_date_range) { 
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")

  
  # Import tables with particular search items
  cv_reports_sorted_drg <- cv_reports %>%
                            select(REPORT_ID, DATINTRECEIVED_CLEAN) %>%
                            filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])
                            #filter(DATINTRECEIVED_CLEAN >= date_ini) %>%
                            #filter(DATINTRECEIVED_CLEAN <= date_end)
  
  cv_report_drug_drg <- if(is.na(current_brand) == FALSE){
                          cv_report_drug %>%
                            select(REPORT_ID,DRUGNAME) %>%
                            filter(DRUGNAME == current_brand)
                        } else {
                          cv_report_drug %>%
                            select(REPORT_ID,DRUGNAME)
                        }

  cv_reactions_drg <- if(is.na(current_rxn) == FALSE){
                        cv_reactions %>%
                          select(REPORT_ID, PT_NAME_ENG) %>%
                          filter(PT_NAME_ENG == current_rxn)
                      } else {
                        cv_reactions %>%
                          select(REPORT_ID, PT_NAME_ENG)
                      }
  
  cv_report_drug_indication_drg <- cv_report_drug_indication %>%
                                    select(REPORT_ID, INDICATION_NAME_ENG)
  
  # Data frame used to obtain Top_25_indication bar chart: Indication is only associated with individual drug
  # When brand name is unspecified, chart shows top 25 indications associated with DRUGNAME="REMICADE" + date_range
  # When brand name is specified, chart shows top 25 indications associated with specified drug + date_range

    # When generic, brand & reaction names are unspecified, count number of UNIQUE reports associated with each durg_name 
  #    (some REPORT_ID maybe duplicated due to multiple REPORT_DRUG_ID & DRUG_PRODUCT_ID which means that patient has diff dosage/freq) 
  drugs_tab_indication <- cv_reports_sorted_drg%>%
                            semi_join(cv_report_drug_drg, by="REPORT_ID") %>% 
                            semi_join(cv_reactions_drg, by="REPORT_ID") %>%
                            inner_join(cv_report_drug_indication_drg) %>%
                            collect()
  
  return(drugs_tab_indication)
}

########################################################################################################################
# sample search combination 
#current_brand <- "REMICADE"
#date_ini <- ymd("19730601")
#date_end <- ymd("20160526")

# Function to product DF of Top_25_Drugs used for the top indication of searched brand in DRUGS Tab
# For Top_25_Drugs: cv_reports (REPORT_ID, DATINTRECEIVED_CLEAN)
#                   cv_report_drug (REPORT_ID,DRUGNAME)
#                   cv_report_drug_indication (REPORT_ID, INDICATION_NAME_ENG)

drugs_tab_topdrg <- function(current_brand,current_rxn,current_date_range) { 
  # connect to CV database
  hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
  
  setwd("~/CV_Shiny_DrugTabOnly")
  source("DO_Drugs_Tab_Func SHe.R")
  df <- drugs_tab_indt(current_brand = current_brand, current_rxn = current_rxn, current_date_range=current_date_range)
 
  indications <-  dplyr::summarise(group_by(df, INDICATION_NAME_ENG),count=n_distinct(REPORT_ID))
  top_indications<- indications %>% arrange(desc(count)) %>% top_n(n=1) %>% select(INDICATION_NAME_ENG)
  top_indications_final <- top_indications$INDICATION_NAME_ENG
  

  # indication import
  cv_report_drug_indication_drg <- cv_report_drug_indication %>%
                                    select(REPORT_ID, INDICATION_NAME_ENG, DRUGNAME) %>%
                                    filter(INDICATION_NAME_ENG == top_indications_final)
  cv_reports_sorted_drg <- cv_reports %>%
                            select(REPORT_ID, DATINTRECEIVED_CLEAN) %>%
                            filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2])
                            #filter(DATINTRECEIVED_CLEAN >= date_ini) %>%
                            #filter(DATINTRECEIVED_CLEAN <= date_end)
  
  cv_report_drug_drg <- if(is.na(current_brand) == FALSE){
                          cv_report_drug %>%
                            select(REPORT_ID,DRUGNAME) %>%
                            filter(DRUGNAME != current_brand)
                        } else {
                          cv_report_drug %>%
                            select(REPORT_ID,DRUGNAME)
                        }
 
  drugs_tab_topdrg <- cv_reports_sorted_drg %>% 
                      inner_join(cv_report_drug_drg)%>%
                      semi_join(cv_report_drug_indication_drg) %>%
                      select(REPORT_ID, DRUGNAME) %>%
                      collect()

  return(drugs_tab_topdrg)
}