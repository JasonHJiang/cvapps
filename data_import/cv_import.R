# This script downloads and imports the Canada Vigilance Database from Health Canada
# Returns data.tables with the cv_ prefix, and cvextractdate with the date of the extract
# Last modified by Kevin Thai, Nov. 25, 2016

# it looks like _LX are legend keys for corresponding codes, but they also have the name present
# in the report table

library(rvest)
library(lubridate)
library(stringr)
library(magrittr)
library(dplyr)
library(data.table)
library(purrr)
library(RPostgreSQL)

cvlanding <- "http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/extract_extrait-eng.php"
cvreadmeurl <- "http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/structure-eng.php"

# Get the CV extract date
cvextractdate <- html_session(cvlanding) %>%
  html_nodes('p:contains("time period:")') %>%
  html_text() %>%
  str_extract("(?<=1965 to )\\d{4}-\\d{2}-\\d{2}") %>%
  str_replace_all("-", "")

# Download and unzip the CV extract
cvzipurl <- html_session(cvlanding) %>%
  html_node('a[href$=".zip"]') %>%
  html_attr("href") %>%
  str_c("http://www.hc-sc.gc.ca", .)
if (!(file.exists("../data/cv"))) dir.create("../data/cv", recursive = TRUE)
download.file(cvzipurl, "../data/cv/cvextract.zip")
unzip("../data/cv/cvextract.zip", exdir = "../data/cv/unzipped/", junkpaths = TRUE)

# CV Variable Names
# Grab the readme page
cvreadme <- html_session(cvreadmeurl)
# Extract the table names
cvtablenames <- cvreadme %>%
  html_nodes("h2:contains('.txt')") %>%
  html_text() %>%
  str_extract(regex(".*(?=\\.txt)")) %>%
  tolower() %>%
  str_replace("^drug_product$", "drug_products") %>%
  str_replace("^report_links_lx$", "report_links") %>%
  str_c("cv_", .)
# Grab the html tables with the variables
cvvartables <- html_table(cvreadme)

# Put the table names and variables into a list, tables are same order as table heading titles
cvvar <- list()
for (i in 1:length(cvtablenames)) cvvar[[cvtablenames[i]]] <- cvvartables[[i]]$`Attribute Physical Name`

# Grab the file names
cvfiles <- list.files("../data/cv/unzipped/", pattern = ".*txt")
cvtables <- list()
# Import the files
for (i in cvfiles) {
  varname <- i %>% 
    str_extract(regex(".*(?=\\.txt$)")) %>%
    str_c("cv_", .)
  cvfilepath <- paste0("../data/cv/unzipped/", i)
  
  cvtables[[varname]] <- read.delim(cvfilepath,
                                    header = FALSE,
                                    sep = "$",
                                    stringsAsFactors = FALSE,
                                    strip.white = TRUE)
  print(paste0(varname, " read!"))
}

#Variable names
for (i in cvtablenames) names(cvtables[[i]]) <- cvvar[[i]]

#Add 4-digit years. R assumes that 2-digits years are 00-68 = 2000-2068 and 69-99 = 1969-1999
#The following code fixes this by adding new columns with 4 digit years
#Original columns are preserved, new columns added with the dates in character and POSIX format
cvtables$cv_reports %<>% mutate(DATRECEIVED_CLEAN = dmy(DATRECEIVED) %>%
                                      ifelse(year(.) > 2050, . - years(100), .) %>%
                                      as.Date(origin = origin),
                                    DATRECEIVED_CHAR = as.character(DATRECEIVED_CLEAN),
                                    DATINTRECEIVED_CLEAN = dmy(DATINTRECEIVED) %>%
                                      ifelse(year(.) > 2050, . - years(100), .) %>%
                                      as.Date(origin = origin),
                                    DATINTRECEIVED_CHAR = as.character(DATINTRECEIVED_CLEAN))

# Recode Age Groups. Original Columns are preserved. New columns added.
# Neonate: greater than zero and up to 25 days, inclusively
# Infant: greater than 25 days and less than 1 year
# Child: greater or equal to 1 and less than 13 years
# Adolescent: greater or equal to 13 and less than 18 years
# Adult: greater or equal to 18 and up to 65 years, inclusively
# Elderly: greater than 65

cvtables$cv_reports %<>%
  mutate(AGE_GROUP_CLEAN = NA,
         AGE_GROUP_CLEAN = ifelse(AGE_Y <= 25/365, "Neonate", AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(AGE_Y > 25/365 & AGE_Y < 1, "Infant", AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(AGE_Y >= 1 & AGE_Y < 13, "Child", AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(AGE_Y >= 13 & AGE_Y < 18, "Adolescent", AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(AGE_Y >= 18 & AGE_Y <= 65, "Adult", AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(AGE_Y > 65, "Elderly", AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(is.na(AGE_Y), "Unknown", AGE_GROUP_CLEAN))
if (is.na(cvtables$cv_reports$AGE_GROUP_CLEAN) %>% sum()) stop("One or more reports didn't get assigned an AGE_GROUP_CLEAN")

# Clean up transients
rm(list = c("cvlanding",
            "cvreadmeurl",
            "cvzipurl",
            "cvreadme",
            "cvtablenames",
            "cvvartables",
            "cvvar",
            "cvfiles",
            "varname",
            "cvfilepath",
            "i"))


hcopen <- dbConnect(drv = dbDriver("PostgreSQL"), host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
for (i in names(cvtables)) {
  # ensure none of them exist before trying to write
  dbWriteTable(hcopen, str_c(i, "_", cvextractdate), cvtables[[i]], row.names = FALSE)
}
dbDisconnect(hcopen)

