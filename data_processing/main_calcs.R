
####################################################################################################################################################
############## The above are the datasets uploaded in the hcopen database: PRR_160826, IC_160829, ROR_160826, RRR_160826 & GPS_160831 ##############
############### Sept 1 2016


# you can use this on a subset on the data (for speed), but for the calculations
# to be correct (aka same as whole dataset run) you must ensure:
# 1. total count N is the same
# 2. the marginal counts must be the same
# this means the best way to run it on a subset of combinations is to create the PhViD matrix
# from the total data, then row-subset L and data. if you subset the count matrix, then
# the PhViD matrix will have different marginal sums and total count when you call the PhViD function
# -> calculations are different

library(DBI)
library(magrittr)
library(dplyr)
source("~/stat_processing/stats_functions.R")


################################ Initial dataframe ####
hcopen <- src_postgres(host = "shiny.hc.local", user = "hcwriter", dbname = "hcopen", password = "canada2")
cv_drug_rxn_meddra <- tbl(hcopen, "cv_drug_rxn_meddra")
cv_drug_rxn_2006 <- cv_drug_rxn_meddra %>%
  filter(!is.na(PT_NAME_ENG)) %>%
  filter(quarter >= 2006.1)
count_df <- cv_drug_rxn_2006 %>%
  group_by(ing, PT_NAME_ENG) %>%    # coerce to data frame to drop grouped and tbl attributes
  dplyr::summarise(count = n_distinct(REPORT_ID)) %>% as.data.frame()
count_df_hlt <- cv_drug_rxn_2006 %>%
  group_by(ing, HLT_NAME_ENG) %>%    # coerce to data frame to drop grouped and tbl attributes
  dplyr::summarise(count = n_distinct(REPORT_ID)) %>% as.data.frame() %>%
  filter(!is.na(HLT_NAME_ENG))

# count_df_quarter <-
#   group_by(cv_drug_rxn_2006, ing, HLT_NAME_ENG, quarter) %>%
#   dplyr::summarise(count = n_distinct(REPORT_ID)) %>% as.data.frame()

# DISP_final <- cv_drug_rxn %>%
#   filter(!is.na(PT_NAME_ENG)) %>%
#   group_by(quarter,ing,PT_NAME_ENG) %>%
#   summarise(count = n_distinct(REPORT_ID)) %>%
#   as.data.frame()
# DISP_final$quarter <- as.factor(DISP_final$quarter)
# quarters <- levels(DISP_final$quarter)
# # frequency table filtered by quarter
# DISP_table <- lapply(quarters, function(x) DISP_final %>% filter(quarter == x) %>% dplyr::select(-quarter))
# # # as.PhVid_HCSC for each quarter of frequency table
# bayes_table <- lapply(DISP_table, as.PhViD_HCSC)

# input_df3 <- as.PhViD(count_df)
input_df <- as.PhViD_HCSC(count_df)
input_df_hlt <- as.PhViD_HCSC(count_df_hlt)

# temp <- cbind(input_df$L, input_df$data) %>%
#   rename(drug_code = ing, event_effect = PT_NAME_ENG,
#          count = n11, drug_margin = n1., event_margin = n.1)
# testconnect <- dbConnect(drv = "PostgreSQL", host = "shiny.hc.local", user = "hcwriter", dbname = "hcopen", password = "canada2")
# dbWriteTable(testconnect, "PT_counts_161103", temp, row.names = FALSE)
# dbDisconnect(testconnect)
# temp <- cbind(input_df_hlt$L, input_df_hlt$data) %>%
#   rename(drug_code = ing, event_effect = HLT_NAME_ENG,
#          count = n11, drug_margin = n1., event_margin = n.1)
# testconnect <- dbConnect(drv = "PostgreSQL", host = "shiny.hc.local", user = "hcwriter", dbname = "hcopen", password = "canada2")
# dbWriteTable(testconnect, "HLT_counts_161103", temp, row.names = FALSE)
# dbDisconnect(testconnect)


# 
# ings <- cv_drug_rxn %>% distinct(ing) %>% lapply(function(x) strsplit(x, "\\|")) %>%
#   unlist() %>% as.vector() %>% sort() %>% unique()
# in_dpd_ther <- inner_join(data.frame(ing = ings), dpd_ther_all, by = c(ing = "TC_ATC")) %>%
#   dplyr::select(ing, TC_ATC_NUMBER)
# missing <- ings[! ings %in% toupper(dpd_ther_all$TC_ATC)]
# missings <- missing[! missing %in% toupper(dpd_ingred_all$INGREDIENT)]

# drugnames <- cv_drug_rxn %>% distinct(DRUGNAME, ing)
# dpd_drug_all <- tbl(hcopen, "dpd_drug_all") %>%
#   dplyr::select(BRAND_NAME, DRUG_CODE, AI_GROUP_NO) %>% as.data.frame()
# dpd_ther_all <- tbl(hcopen, "dpd_ther_all") %>%
#   dplyr::select(DRUG_CODE, TC_ATC_NUMBER, TC_ATC) %>% as.data.frame()
# result <- drugnames %>%
#   left_join(dpd_drug_all, by = c(DRUGNAME = "BRAND_NAME")) %>%
#   left_join(dpd_ther_all, by = "DRUG_CODE") %>%
#   distinct(DRUGNAME, ing, .keep_all = TRUE)
# dpd_substances <- tbl(hcopen, "dpd_substances") %>% as.data.frame()
# table(drugnames$ing %in% dpd_substances$ing | drugnames$DRUGNAME %in% dpd_drug_all$BRAND_NAME
#       | drugnames$ing %in% dpd_ther_all$TC_ATC | drugnames$ing %in% toupper(dpd_ingred_all$INGREDIENT))

DATA <- input_df$data
N <- input_df$N
L <- input_df$L
n11 <- DATA[,1]
n1. <- DATA[,2] # marginal drug counts
n.1 <- DATA[,3] # marginal AE counts
n10 <- n1. - n11
n01 <- n.1 - n11
n00 <- N - (n11+n10+n01)
expected_count <- n1. * n.1 / N

############################################## PRR ####
# the order of PRR output is sorted by the rankstat, so don't calculate standard dev from
#   DATA assuming it's in same order as PRR results
# PRR_result <- vector(mode = "list")
# PRR_result <- PRR(input_df, DECISION = 3, RANKSTAT = 2)
# PRR_PhViD <- as.data.frame(PRR_result$ALLSIGNALS)
# PRR_PhViD %<>% dplyr::arrange(`drug code`, `event effect`)


PRR <- (n11 / (n11 + n10)) / (n01 / (n01 + n00))
logPRR <- log(PRR)
var_logPRR <- 1/n11 - 1/(n11 + n10) + 1/n01 - 1/(n01 + n00)
LB95_logPRR <- qnorm(0.025,logPRR,sqrt(var_logPRR))
UB95_logPRR <- qnorm(0.975,logPRR,sqrt(var_logPRR))
LB95_PRR <- exp(LB95_logPRR)
UB95_PRR <- exp(UB95_logPRR)
PRR_result <- data.frame(drug_code = L[[1]], event_effect = L[[2]],
                         count = n11, expected_count, 
                         PRR, LB95_PRR, UB95_PRR,
                         logPRR, LB95_logPRR, UB95_logPRR,
                         var_logPRR,
                         stringsAsFactors = FALSE)
rm("PRR", "logPRR", "var_logPRR", "LB95_logPRR", "UB95_logPRR",
   "LB95_PRR", "UB95_PRR")

# testconnect <- dbConnect(drv = "PostgreSQL", host = "shiny.hc.local", user = "hcwriter", dbname = "hcopen", password = "canada2")
# dbWriteTable(testconnect, "HLT_PRR_160927", PRR_HCSC, row.names = FALSE)
# dbDisconnect(testconnect)


############################################## ROR ####
# ROR_result <- vector(mode = "list")
# ROR_result <- ROR(input_df, OR0 = 1, MIN.n11 = 1, DECISION = 3,DECISION.THRES = 0, RANKSTAT = 2)
# ROR_PhViD <- as.data.frame(ROR_result$ALLSIGNALS)

ROR <- n11 * n00 /(n10 * n01)
logROR <- log(ROR)
var_logROR <- 1/n11 + 1/n10 + 1/n01 + 1/n00
LB95_logROR <- qnorm(0.025,logROR,sqrt(var_logROR))
UB95_logROR <- qnorm(0.975,logROR,sqrt(var_logROR))
LB95_ROR <- exp(LB95_logROR)
UB95_ROR <- exp(UB95_logROR)
ROR_result <- data.frame(drug_code = L[[1]], event_effect = L[[2]],
                         count = n11, expected_count, 
                         ROR, LB95_ROR, UB95_ROR,
                         logROR, LB95_logROR, UB95_logROR,
                         var_logROR,
                         stringsAsFactors = FALSE)
rm("ROR", "logROR", "var_logROR", "LB95_logROR", "UB95_logROR",
   "LB95_ROR", "UB95_ROR")

# testconnect <- dbConnect(drv = "PostgreSQL", host = "shiny.hc.local", user = "hcwriter", dbname = "hcopen", password = "canada2")
# dbWriteTable(testconnect, "HLT_ROR_160921", ROR_HCSC, row.names = FALSE)
# dbDisconnect(testconnect)


############################################## RRR ####
RRR <- (n11*N) / (n1.*n.1)
logRRR <- log(RRR)
var_logRRR <- 1/n11 - 1/n1. + 1/n.1 - 1/N
LB95_logRRR <- qnorm(0.025,logRRR,sqrt(var_logRRR))
UB95_logRRR <- qnorm(0.975,logRRR,sqrt(var_logRRR))
LB95_RRR <- exp(LB95_logRRR)
UB95_RRR <- exp(UB95_logRRR)
RRR_result <- data.frame(drug_code = L[[1]], event_effect = L[[2]],
                         count = n11, expected_count, 
                         RRR, LB95_RRR, UB95_RRR,
                         logRRR, LB95_logRRR, UB95_logRRR,
                         var_logRRR,
                         stringsAsFactors = FALSE)
rm("RRR", "logRRR", "var_logRRR", "LB95_logRRR", "UB95_logRRR",
   "LB95_RRR", "UB95_RRR")

# testconnect <- dbConnect(drv = "PostgreSQL", host = "shiny.hc.local", user = "hcwriter", dbname = "hcopen", password = "canada2")
# dbWriteTable(testconnect, "HLT_RRR_160921", RRR_HCSC, row.names = FALSE)
# dbDisconnect(testconnect)


############################################# RFET ####
RFET_result <- RFET_HCSC(input_df)
# testconnect <- dbConnect(drv = "PostgreSQL",host = "shiny.hc.local", user = "hcwriter", dbname = "hcopen", password = "canada2")
# dbRemoveTable(testconnect, "PT_RFET_161101")
# dbWriteTable(testconnect, "PT_RFET_161101", RFET_result, row.names = FALSE)
# dbDisconnect(testconnect)

RFET_result <- RFET_HCSC(input_df_hlt)
# testconnect <- dbConnect(drv = "PostgreSQL",host = "shiny.hc.local", user = "hcwriter", dbname = "hcopen", password = "canada2")
# dbRemoveTable(testconnect, "HLT_RFET_161101")
# dbWriteTable(testconnect, "HLT_RFET_161101", RFET_result, row.names = FALSE)
# dbDisconnect(testconnect)


############################################ BCPNN ####
BCPNN_result <- BCPNN_HCSC(input_df_hlt, MC = TRUE, NB.MC = 10000)
BCPNN_result %<>% dplyr::select(drug_code = `drug code`, event_effect = `event effect`,
                                count, expected_count = `expected count`, median_IC,
                                LB95_IC = `Q_0.025(log(IC))`, UB95_IC = `Q_0.975(log(IC))`,
                                postH0, `n11/E (RRR)` = `n11/E`)
# testconnect <- dbConnect(drv = "PostgreSQL", host = "shiny.hc.local", user = "hcwriter", dbname = "hcopen", password = "canada2")
# dbWriteTable(testconnect, "HLT_IC_161027", BCPNN_result, row.names = FALSE)
# dbDisconnect(testconnect)




############################################## GPS ####

# GPS_result <- GPS(input_df, RANKSTAT = 2)
# GPS_quantile <- as.data.frame(GPS_result$ALLSIGNALS)
# GPS_result <- GPS(input_df, RANKSTAT = 3)
# GPS_postE <- as.data.frame(GPS_result$ALLSIGNALS)
# this is how you can calculate FDR, etc. for each rankstat
# FDR <- (cumsum(GPS_postE$postH0[order(GPS_postE$postE_lambda,decreasing=TRUE)]) / (1:235803))
# FDR <- (cumsum(GPS_quantile$postH0[order(GPS_quantile$Q0.05_lambda,decreasing=TRUE)]) / (1:235803))
GPS_result1 <- GPS(input_df_hlt, RANKSTAT = 2)
GPS_quantile <- as.data.frame(GPS_result1$ALLSIGNALS) %>% arrange(drug, event)
GPS_result2 <- GPS(input_df_hlt, RANKSTAT = 3)
GPS_postE <- as.data.frame(GPS_result2$ALLSIGNALS) %>% arrange(drug, event)
GPS_final <- data.frame(
  drug_code = GPS_quantile$drug,
  event_effect = GPS_quantile$event,
  count = GPS_quantile$count,
  expected_count = GPS_quantile$`expected count`,
  `n11/E` = GPS_quantile$`n11/E`,
  postH0 = GPS_quantile$postH0,
  postE_lambda = GPS_postE$`post E(Lambda)`,
  Q0.05_lambda = GPS_quantile$`Q_0.05(lambda)`
)

hcopen <- dbConnect(drv = "PostgreSQL", host = "shiny.hc.local", user = "hcwriter", dbname = "hcopen", password = "canada2")
# dbRemoveTable(testconnect, "PT_GPS_161121")
dbWriteTable(hcopen, "HLT_GPS_161121", GPS_final, row.names = FALSE)
dbGetQuery(hcopen, 'GRANT SELECT ON "PT_GPS_161121" TO hcreader')
dbDisconnect(hcopen)



