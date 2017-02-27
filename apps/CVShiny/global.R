# data manip + utils
library(magrittr)
library(lubridate)
library(dplyr)
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
library(shinyBS)
library(DT)

source("common_ui.R")
source("linechart.R")


########## Codes to fetch top 1000 specific results to be used in dropdown menu ############### 
# Temperary solution: fetch all tables to local and run functions on them
hcopen_pool <- dbPool(drv = "PostgreSQL",
                      host = "shiny.hc.local",
                      dbname = "hcopen",
                      user = "hcreader",
                      password = "canada1")
hcopen <- src_pool(hcopen_pool)
cv_meddra_pt_map <- tbl(hcopen, "cv_meddra_pt_map")
cv_reports <- tbl(hcopen, "cv_reports_20160630")
cv_drug_product_ingredients <-  tbl(hcopen, "cv_drug_product_ingredients_20160630")
cv_report_drug <- tbl(hcopen, "cv_report_drug_20160630")
cv_reactions <- tbl(hcopen, "cv_reactions_20160630") %>% # SOC is HERE
  left_join(cv_meddra_pt_map, by=c('PT_NAME_ENG' = 'PT'))
cv_report_drug_indication <- tbl(hcopen, "cv_report_drug_indication_20160630")
cv_substances <- tbl(hcopen, "cv_substances")

meddra <- tbl(hcopen, "meddra") %>%
  filter(Primary_SOC_flag == "Y") %>%
  select(PT_Term, HLT_Term, Version = MEDDRA_VERSION)

####################### datasets for menu setup ####################### 
#Fetch brand/drug names
topbrands <- cv_report_drug %>%
  distinct(DRUGNAME) %>%
  as.data.frame() %>%
  `[[`(1) %>%
  sort() %>%
  `[`(-c(1,2)) # dropping +ARTHRI-PLUS\u0099 which is problematic
topings_dpd <- cv_substances %>%
  distinct(ing) %>%
  as.data.frame() %>%
  `[[`(1) %>%
  sort()
topings_cv <- cv_drug_product_ingredients %>%
  distinct(ACTIVE_INGREDIENT_NAME) %>%
  as.data.frame() %>%
  `[[`(1) %>%
  sort()
smq_choices <- cv_reactions %>%
  distinct(SMQ) %>%
  as.data.frame() %>%
  filter(!is.na(SMQ)) %>%
  `[[`(1) %>%
  sort()
pt_choices <- cv_reactions %>%
  distinct(PT_NAME_ENG) %>% 
  as.data.frame() %>%
  `[[`(1) %>%
  c(smq_choices) %>%
  sort()
soc_choices <- cv_reactions %>%
  distinct(SOC_NAME_ENG) %>%
  as.data.frame() %>%
  `[[`(1) %>%
  sort()
