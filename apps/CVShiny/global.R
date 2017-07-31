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
source("pieTableUtil.R")
source("barTableUtil.R")

# ON NEW RELEASE, CHANGE THIS DATE --------------------------------------------
data_date   <- "20160630"                                                    #|
# -----------------------------------------------------------------------------

########## Codes to fetch top 1000 specific results to be used in dropdown menu ############### 
# Temperary solution: fetch all tables to local and run functions on them
hcopen_pool <- dbPool(drv      = RPostgreSQL::PostgreSQL(),
                      host     = "shiny.hc.local",
                      dbname   = "hcopen",
                      user     = "hcreader",
                      password = "canada1")

# Not sure if this is the correct way to implement a database connection.
# hcopen      <- src_pool(hcopen_pool)


# Appending date to the table names in the PostgreSQL database
table_name_cv_reports                  <- paste0("cv_reports_", data_date)
table_name_cv_drug_product_ingredients <- paste0("cv_drug_product_ingredients_", data_date)
table_name_cv_report_drug              <- paste0("cv_report_drug_indication_joined_", data_date)
table_name_cv_reactions                <- paste0("cv_reactions_meddra_", data_date)

# Creating tbls for the data tables we care about
# Again, not sure if this is the correct way to implement
cv_reports                  <- tbl(hcopen_pool, table_name_cv_reports)
cv_drug_product_ingredients <- tbl(hcopen_pool, table_name_cv_drug_product_ingredients)
cv_report_drug              <- tbl(hcopen_pool, table_name_cv_report_drug)
cv_reactions                <- tbl(hcopen_pool, table_name_cv_reactions)
cv_substances               <- tbl(hcopen_pool, "cv_substances")

cv_reports_temp <- tbl(hcopen_pool, table_name_cv_reports) %>%
  select(REPORT_ID, SERIOUSNESS_ENG,DEATH)
# cv_reports_temp$DEATH[cv_reports_temp$DEATH == 1] <- "Yes"
# cv_reports_temp$DEATH[is.na(cv_reports_temp$DEATH)] <- "No"

cv_report_drug %<>% left_join(cv_reports_temp, "REPORT_ID" = "REPORT_ID")
cv_reactions %<>% left_join(cv_reports_temp, "REPORT_ID" = "REPORT_ID")

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


# Grabbing column names from the tbl metadata.
# Used for selecting columns in the downloads tab.
cv_report_drug_names <- cv_report_drug$ops$vars
cv_reaction_names   <- cv_reactions$ops$vars
cv_reports_names     <- cv_reports$ops$vars