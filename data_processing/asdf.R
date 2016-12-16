###### CODE FOR CV_DRUG_RXN?????

# this should be the precursor to cv_drug_rxn, since it was found before DISP was its own app,
# to construct a count table (when there was no reference to cv_drug_rxn table)
part1 <- cv_drug_product_ingredients %>%
  dplyr::select(DRUG_PRODUCT_ID,ACTIVE_INGREDIENT_NAME) %>%
  left_join(cv_report_drug) %>% 
  dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME) %>% 
  left_join(cv_reactions) %>%
  dplyr::select(REPORT_ID,ACTIVE_INGREDIENT_NAME, PT_NAME_ENG)
reports_in_date_range <- cv_reports %>% 
  filter(DATINTRECEIVED_CLEAN >= current_date_range[1], DATINTRECEIVED_CLEAN <= current_date_range[2]) %>%
  dplyr::select(REPORT_ID,DATINTRECEIVED_CLEAN)
DISP_final <- semi_join(part1, reports_in_date_range) %>%
  group_by(ACTIVE_INGREDIENT_NAME, PT_NAME_ENG) %>%
  dplyr::summarise(count = n_distinct(REPORT_ID)) %>%
  as.data.frame()
bayes_table <- as.PhViD(DISP_final, MARGIN.THRES = 1) 




# ### NOTES ABOUT IMPLEMENTATION FIXES:
# 
# - BCPNN function:
#   - used IC_monte as the IC vector; THIS IS INCORRECT, IC_monte is the sampled/simulated values for each pair,
#     and regenerated and of length NB.MC, not the actual IC estimation for the interval. There is no IC estimate,
#     and what was being attached was just the first m elements from the last simulation run (hence when it's not in the bounds)
#   - sorted UB before appending to data.frame; doesn't make sense because now the largest UB goes with the largest LB, etc....
# 
# - cumulative BCPNN:
#   - each cumulative sum didn't make sense, you must take the total count up to that year, even when the pair doesn't occur
#     (see false-positive avoidance in Bate, 1998)
# 

