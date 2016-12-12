library(magrittr)
library(dplyr)

load_meddra <- function(zip_file, location_in_zip, version) {
  meddra <- unz(zip_file, filename = location_in_zip) %>%
    read.table(sep = "$", quote = "", comment.char = "", stringsAsFactors = FALSE) %>%
    dplyr::select(-c(V10, V13)) %>%    # dropping last empty column and null column
    dplyr::rename(PT_Code = V1, HLT_Code = V2, HLGT_Code = V3, SOC_Code = V4, PT_Term = V5, HLT_Term = V6,
                  HLGT_Term = V7, SOC_Term = V8, SOC_Abbrev = V9, Primary_PT_SOC = V11, Primary_SOC_flag = V12)
  meddra$MEDDRA_VERSION = version
  meddra
}

# Create the MedDRA table
old_dir <- setwd("~")
meddra_190 <- load_meddra("MedDRA/meddra_19_0_english (no pw).zip", "English/MedAscii/mdhier.asc", "v.19.0")
meddra_181 <- load_meddra("MedDRA/MedAscii18.1.zip", "MedAscii/mdhier.asc", "v.18.1")
meddra_180 <- load_meddra("MedDRA/meddra_18_0_english.zip", "meddra_18_0_english/MedAscii/mdhier.asc", "v.18.0")
meddra_171 <- load_meddra("MedDRA/MedDRA 17.1 English Files.zip", "MedAscii/mdhier.asc", "v.17.1")
meddra_170 <- load_meddra("MedDRA/MedDRA 17_0 English.zip", "MedDRA 17_0 English/MedAscii/mdhier.asc", "v.17.0")
meddra_161 <- load_meddra("MedDRA/meddra_16_1_english.zip", "meddra_16_1_english/MedAscii/mdhier.asc", "v.16.1")
meddra_160 <- load_meddra("MedDRA/MedAscii_English16.0.zip", "MedAscii/mdhier.asc", "v.16.0")
# meddra_151 <- load_meddra("MedDRA/MedDRA_15_1_English.zip", "MedAscii/mdhier.asc", "v.15.1")
meddra_150 <- load_meddra("MedDRA/MedDRA_15_0_English.zip", "MedAscii/mdhier.asc", "v.15.0")
meddra <- rbind(meddra_190, meddra_181, meddra_180, meddra_171, meddra_170, meddra_161, meddra_160, meddra_150)
rm(list = c("meddra_190", "meddra_181", "meddra_180", "meddra_171", "meddra_170", "meddra_161", "meddra_160", "meddra_150"))
# write.csv(meddra, file = 'meddra_terms.csv', row.names = FALSE)

# Load the CV table from Postgres database
hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
if (!(exists("cv_drug_rxn") && class(cv_drug_rxn) == "data.frame"))
  cv_drug_rxn <- tbl(hcopen, "cv_drug_rxn") %>% as.data.frame()

# Join them ! (and rearrange and rename columns)
# only take the primary terms since each Preferred Term links to multiple HLTs
meddra %<>% filter(Primary_SOC_flag == "Y")
cv_drug_rxn_meddra <-
  dplyr::left_join(cv_drug_rxn, meddra, by = c("PT_NAME_ENG" = "PT_Term", "MEDDRA_VERSION" = "MEDDRA_VERSION")) %>%
  dplyr::select(REPORT_ID:PT_NAME_ENG, HLT_NAME_ENG = HLT_Term, HLGT_NAME_ENG = HLGT_Term, SOC_NAME_ENG:month)

setwd(old_dir)


# testconnect <- dbConnect(drv = "PostgreSQL", host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
# dbWriteTable(testconnect, "cv_drug_rxn_meddra", cv_drug_rxn_meddra, row.names = FALSE)
# dbRemoveTable(testconnect, "cv_drug_rxn_meddra")
# dbDisconnect(testconnect)
