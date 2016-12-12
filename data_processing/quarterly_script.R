####################################################################################################################################################
############## The following are the datasets uploaded in the hcopen database in 2016 July, e.g. cv_bcpnn_160712 etc. ###################################
####################################################################################################################################################

library(magrittr)
library(dplyr)
library(data.table)
source('~/scripts/stats_functions.R')
hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
cv_drug_rxn <- tbl(hcopen, "cv_drug_rxn")

# need to get rid of NA in PT_NAME_ENG in cv_drug_rxn
DISP_final <- cv_drug_rxn %>%
  filter(!is.na(PT_NAME_ENG)) %>%
  group_by(quarter,ing,PT_NAME_ENG) %>%
  summarise(count = n_distinct(REPORT_ID)) %>%
  as.data.frame()

# all levels of quarters
DISP_final$quarter <- as.factor(DISP_final$quarter)
quarters <- levels(DISP_final$quarter)
# frequency table filtered by quarter
DISP_table <- lapply(quarters, function(x) DISP_final %>% filter(quarter == x) %>% dplyr::select(-quarter))
# # as.PhVid_HCSC for each quarter of frequency table
bayes_table <- lapply(DISP_table, as.PhViD_HCSC)

#### PRODUCES cv_prr_160713 and cv_ror_160714
# #### PRR for each pair of drug/event per quarter non-cumulatively
# bayes_PRR_result <- lapply(bayes_table, function(x) PRR(x, DECISION = 3, RANKSTAT = 2))
# bayes_PRR_result_final <- lapply(bayes_PRR_result, function(x) list(x$ALLSIGNALS))
# names(bayes_PRR_result_final) <- lapply(quarters, function(x) paste0("PRR - Quarter: ", x))
# # add quarter column to each element in bayes_PRR_result_final list
# for(i in 1:length(quarters)) bayes_PRR_result_final[[i]] <- Map(cbind, bayes_PRR_result_final[[i]], quarter = quarters[i])
# # merge all elements in bayes_PRR_result_final list
# bayes_PRR_result_all <-  plyr::ldply(bayes_PRR_result_final, data.frame)
# # change ID variable to just quarter and make event.effect to upper case
# bayes_PRR_result_all$.id <- stringr::str_sub(bayes_PRR_result_all$.id,-6,-1)
# bayes_PRR_result_all$event.effect <- toupper(bayes_PRR_result_all$event.effect)
# bayes_PRR_result_all %<>% dplyr::select(-quarter)
# bayes_PRR_result_all %<>% mutate(drug.code = as.character(drug.code)) %>% arrange(.id, drug.code, event.effect)
# b <- tbl(hcopen, "cv_prr_160713") %>% as.data.frame() %>% dplyr::select(-row.names) %>% arrange(.id, drug.code, event.effect)
# all.equal(bayes_PRR_result_all, b)
# # save(bayes_PRR_result_all, file="PRR_0713.RData")

#### PRODUCES cv_prr_160713 and cv_ror_160714
# # ROR on each bayes_table
# bayes_ROR_result <- lapply(bayes_table, function(x) ROR(x, DECISION = 3, DECISION.THRES = 0, RANKSTAT = 2))
# # ALLSIGNALS of each bayes_ROR_result
# bayes_ROR_result_final <- lapply(bayes_ROR_result, function(x) list(x$ALLSIGNALS))
# names(bayes_ROR_result_final) <- lapply(quarters, function(x) paste0("ROR - Quarter: ", x))
# 
# # add quarter column to each element in bayes_ROR_result_final list
# for(i in 1:length(quarters)){
#   bayes_ROR_result_final[[i]] <- Map(cbind, bayes_ROR_result_final[[i]], quarter = quarters[i])
# }
# 
# # merge all elements in bayes_PRR_result_final list
# bayes_ROR_result_all <-  plyr::ldply(bayes_ROR_result_final, data.frame)
# # change ID variable to just quarter and make event.effect to upper case
# bayes_ROR_result_all$.id <- stringr::str_sub(bayes_ROR_result_all$.id,-6,-1)
# bayes_ROR_result_all$event.effect <- toupper(bayes_ROR_result_all$event.effect)
# bayes_ROR_result_all %<>% dplyr::select(-quarter)
# bayes_ROR_result_all %<>% mutate(drug.code = as.character(drug.code)) %>% arrange(.id, drug.code, event.effect)
# b <- tbl(hcopen, "cv_ror_160714") %>% as.data.frame() %>% dplyr::select(-row.names) %>% arrange(.id, drug.code, event.effect)
# all.equal(bayes_ROR_result_all, b)
# # save(bayes_ROR_result_all, file="bayes_ROR_result_all0714.RData")

####################### Disproportionality analysis (Quarterly) using BCPNN
# # BCPNN_SHe is to include IC results in the final output
# for(i in 1:length(quarters)) bayes_result_final[i] <- list(list(BCPNN_HCSC(bayes_table[[i]], MC = TRUE, NB.MC = 100)))
# names(bayes_result_final) <- lapply(quarters, function(x) paste0("BCPNN - Quarter: ", x))
# # add quarter column to each element in bayes_result_final list
# for(i in 1:length(quarters)) bayes_result_final[[i]] <- Map(cbind, bayes_result_final[[i]], quarter = quarters[i])
# 
# # merge all elements in bayes_result_final list
# bayes_result_all <-  plyr::ldply(bayes_result_final, data.frame)
# # change ID variable to just quarter and make event.effect to upper case
# bayes_result_all$.id <- stringr::str_sub(bayes_result_all$.id,-6,-1)
# bayes_result_all$event.effect <- toupper(bayes_result_all$event.effect)
# bayes_result_all %<>% dplyr::select(-quarter)
# 
# # originally produced cv_bcpnn_160805
# # save(bayes_result_all, file="BCPNN_0805.RData")


# #########################Cumulative BCPNN (too many NA for IC because of insufficient number of simulation) ##############
# # Cumulative as.PhVid_CUM1_SHe function1 to generate ing+AR+n11+n1.+n.1 dataframes within a list:bayes_cumulative_table1
# bayes_cumulative_table <- bayes_table %>% lapply(function(x) cbind(x$L,x$data))
# # merge elements of bayes_cumulative_table list together
# for(i in 1:length(quarters)) bayes_cumulative_table[[i]] <- cbind(bayes_cumulative_table[[i]], quarter = quarters[i])
# bayes_cumulative_table_all <- bind_rows(bayes_cumulative_table)
# 
# # calculate cumulative n11, n1. & n.1
# bayes_cumulative_table_final <- bayes_cumulative_table_all %>% 
#   group_by(ing,PT_NAME_ENG) %>%
#   mutate(n11_cum = cumsum(n11), n1._cum = cumsum(n1.), n.1_cum = cumsum(n.1)) %>%
#   select(quarter, ing, PT_NAME_ENG,n11_cum,n1._cum,n.1_cum) %>%
#   rename(n11 = n11_cum , n1. = n1._cum ,  n.1 = n.1_cum )
# 
# 
# # format bayes_cumulative_table_final into a list (bayes_cumulative_table) for BCPNN process
# bayes_cumulative_table <- vector(mode = "list")
# for (i in 1:length(quarters)){
#   bayes_cumulative_table[i] <- list(as.PhVid_CUM2_SHe(bayes_cumulative_table_final,i))
# }
# # after calculating cumulative sums, convert it back into a list of PhViD data frames to process BCPNN on


################################# IMPORTANT NOTES ON CUMULATIVE BCPNN ################################
# the reason why lots of NA are produced in Q2.5Log(IC) for cumulative BCPNN is that since both n1. and n.1 are cumulative as well, 
# when conducting Monte Carlo simulation, N - n1. and N - n.1 are used as parameters which can produce negative value. 
# MC Simulation includes random generation from the Dirichlet distribution (using rdirichlet which uses random generation of Gamma Distribution),
# the shape and scale parameters for such generation CANNOT be negative. 
# the reason why N - n1._cum or N - n.1_cum are negative is that n11 is much smaller than n1._cum and n.1_cum. 

# not true, we just need to adjust for a cumulative N as well then ^

# cumulative <- bayes_cumulative_table_all %>% 
#   group_by(ing,PT_NAME_ENG) %>%
#   mutate(n11_cum = cumsum(n11), n1._cum = cumsum(n1.), n.1_cum = cumsum(n.1)) %>%
#   ungroup(ing, PT_NAME_ENG)
# cumulative$PT_NAME_ENG <- toupper(cumulative$PT_NAME_ENG)


################################################## Instead of using cumulative count of D*AR pair, plot cumulative IC ##################################################################
hcopen <- src_postgres(host = "shiny.hc.local", user = "hcreader", dbname = "hcopen", password = "canada1")
# cv_bcpnn_160729 is with IC column but with NA
# cv_bcpnn_160805 is with IC column but without NA
cv_bcpnn <- tbl(hcopen, "cv_bcpnn_160805")%>% as.data.frame()
head(cv_bcpnn)
cv_bcpnn %>% filter(.id == 2009.2, drug.code == "OXYCODONE HYDROCHLORIDE", event.effect == "DRUG DEPENDENCE")

cv_bcpnn_cumulative <- cv_bcpnn %>% group_by(drug.code,event.effect) %>% mutate(IC_Cumulative = cumsum(IC), Q_IC_Cumulative = cumsum(Q_0.025.log.IC..)) 

df <- cv_bcpnn_cumulative %>% filter(drug.code == "PENICILLIN V", event.effect == "RASH")
df <- cv_bcpnn_cumulative %>% filter(drug.code == "NICOTINE", event.effect == "CHEST PAIN")
df <- cv_bcpnn_cumulative%>% filter(drug.code == "OXYCODONE HYDROCHLORIDE", event.effect == "DRUG DEPENDENCE")

p <- df %>%
  ggplot(aes(x = `.id`, y = IC_Cumulative,group = 1)) +
  geom_line() + geom_point()  + 
  ggtitle("plottitle") + 
  xlab("Quarter") + 
  ylab("Cumulative 2.5% Quantile of Posterior Distribution of IC") +
  theme_bw() +
  theme(plot.title = element_text(lineheight=.8, face="bold"), axis.text.x = element_text(angle=90, vjust=1))
ggplotly(p)
