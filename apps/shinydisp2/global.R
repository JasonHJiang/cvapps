library(magrittr)
library(lubridate)
library(tidyr)
library(dplyr)
library(utils)
library(zoo)
library(pool)
library(data.table)
# data visualizations
library(plotly)
library(ggplot2)
library(googleVis)
library(ggvis)

# Shiny libraries
library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)
library(markdown)

source("common_ui.R")

#### Data pre-processing and server connections ####
hcopen<- dbPool(RPostgreSQL::PostgreSQL(),
                      host = "shiny.hc.local",
                      dbname = "hcopen_old",
                      user = "hcreader",
                      password = "canada1")


#type in data_date to specify the version used for disp app
data_date   <- "20160630" 
master_table_pt_name<-paste0("master_table_pt_",data_date)
master_table_hlt_name<-paste0("master_table_hlt_",data_date)

#load tables for the app
master_table_pt<-hcopen%>%tbl(master_table_pt_name)
master_table_hlt<-hcopen%>%tbl(master_table_hlt_name)
cv_drug_rxn_meddra <- hcopen %>% tbl("cv_drug_rxn_meddra")
cv_drug_rxn_2006 <- cv_drug_rxn_meddra %>% filter(quarter >= 2006.1)
count_quarter_pt <- count(cv_drug_rxn_2006, ing, PT_NAME_ENG, quarter) %>% as.data.table()
count_quarter_hlt <- count(cv_drug_rxn_2006, ing, HLT_NAME_ENG, quarter) %>% as.data.table()
quarters <- count_quarter_pt %>% select(quarter) %>% distinct() %>% as.data.table()


# PT-HLT mapping
drug_PT_HLT <- cv_drug_rxn_2006 %>%
  select(ing, PT_NAME_ENG, HLT_NAME_ENG, SOC_NAME_ENG) %>%
  distinct() %>%
  filter(!is.na(HLT_NAME_ENG))
# drug and adverse event dropdown menu choices
drug_choices <- drug_PT_HLT %>% distinct(ing) %>% as.data.table() %>% `$`("ing") %>% sort()

