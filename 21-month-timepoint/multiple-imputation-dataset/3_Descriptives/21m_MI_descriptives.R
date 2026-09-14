#Script: data descriptives for Multiple imputation dataset, at 21 month timepoint

#-------------------------------------------------------------------------------
source(here::here("21-month-timepoint/multiple-imputation-dataset/1_Packages", "21m_packages.R"))

df_working <- read.csv(here::here("21-month-timepoint/multiple-imputation-dataset/2_Import_and_Clean", "df_working_mi.csv"))


#-------------------------------------------------------------------------------
#creating contingency tables - main predictor variable only ----
cont_tab_sum_anx <- df_working %>%
  tbl_summary(
    by = anx_ans,
    include = sum_attitude,
    missing = "no"
  ) %>%
  modify_spanning_header(label ~ "Likelihood of going to the Doctor for Anxiety based on Sum Attitudes") %>%
  bold_labels() %>%
  modify_caption("**Table 1.**")

print(cont_tab_sum_anx)

cont_tab_sum_dep <- df_working %>%
  tbl_summary(
    by = dep_ans,
    include = sum_attitude,
    missing = "no"
  ) %>%
  modify_spanning_header(label ~ "Likelihood of going to the Doctor for Depression based on Sum Attitudes") %>%
  bold_labels() %>%
  modify_caption("**Table 2.**")

print(cont_tab_sum_dep)

cont_tab_sum_per <- df_working %>%
  tbl_summary(
    by = per_ans,
    include = sum_attitude,
    missing = "no"
  ) %>%
  modify_spanning_header(label ~ "Likelihood of going to the Doctor for Problems with Periods based on Sum Attitudes") %>%
  bold_labels() %>%
  modify_caption("**Table 3.**")

print(cont_tab_sum_per)


## Contingency table - main predictor only
cont_tab_sum_all <- tbl_merge(
  tbls = list(cont_tab_sum_anx, cont_tab_sum_dep, cont_tab_sum_per),
  tab_spanner = c("**Anxiety**", "**Depression**", "**Problems with Periods**")
) %>%
  modify_caption("**Table 4. Summary Statistics for Sum Attitudes by Anxiety, Depression, and Menstrual Problems**")

cont_tab_sum_all

#-------------------------------------------------------------------------------
# contigency tables - all covariates
# have to first order the outcomes so that it produces a better looking contingency table
df_working$home_own_status_ans <- factor(
  df_working$home_own_status_ans,
  levels = c("Other", "Publicly rented", "Privately rented", "Mortgaged", "Owned")
)


df_working$educ_level_name <- factor(
  df_working$educ_level_name,
  levels = c("O-level, Vocational, CSE, GCSE or none", "A-level", "Degree")
)

df_working$mums_hlth_eval <- factor(
  df_working$mums_hlth_eval,
  levels = c("Never well", "Often unwell", "Mostly well", "Fit and well")
)

df_working$mar_status_ans <- factor(
  df_working$mar_status_ans,
  levels = c("Not currently married", "Married")
)

df_working$dr_changed_ever <- factor(
  df_working$dr_changed_ever,
  levels = c("Recent Dr change", "No recent Dr change")
)


## Combined contingency table - all covariates {.tabset}

### Anxiety

# contingency tables all covariates
cont_tab_all_anx <- df_working %>%
  tbl_summary(
    by = anx_ans,
    include = c(
      sum_sum_attitude, sum_age, sum_parity, soc_class_ans, educ_level_name, home_own_status_ans,  mar_status_ans, relig_name, mums_hlth_eval, dr_changed_ever, sum_soc_supp_mis, sum_weigh_life, sum_anx_score, sum_dep_score 
    ),
    missing = "no"
  ) %>%
  modify_spanning_header(label ~ "Likelihood of going to the Doctor for Anxiety based on Patient Characteristics") %>%
  bold_labels() %>%
  modify_caption("**Table 5.**")

cont_tab_all_anx


### Depression
cont_tab_all_dep <- df_working %>%
  tbl_summary(
    by = dep_ans,
    include = c(
      sum_sum_attitude, sum_age, sum_parity, soc_class_ans, educ_level_name, home_own_status_ans,  mar_status_ans, relig_name, mums_hlth_eval, dr_changed_ever, sum_soc_supp_mis, sum_weigh_life, sum_anx_score, sum_dep_score 
    ),
    missing = "no"
  ) %>%
  modify_spanning_header(label ~ "Likelihood of going to the Doctor for Depression based on Patient Characteristics") %>%
  bold_labels() %>%
  modify_caption("**Table 6.**")

cont_tab_all_dep

### Menstrual problems
cont_tab_all_per <- df_working %>%
  tbl_summary(
    by = per_ans,
    include = c(
      sum_sum_attitude, sum_age, sum_parity, soc_class_ans, educ_level_name, home_own_status_ans,  mar_status_ans, relig_name, mums_hlth_eval, dr_changed_ever, sum_soc_supp_mis, sum_weigh_life, sum_anx_score, sum_dep_score 
    ),
    missing = "no"
  ) %>%
  modify_spanning_header(label ~ "Likelihood of going to the Doctor for Problems with Periods based on Patient Characteristics") %>%
  bold_labels() %>%
  modify_caption("**Table 7.**")


cont_tab_all_per

cont_tab_all <- tbl_merge(
  tbls = list(cont_tab_all_anx, cont_tab_all_dep, cont_tab_all_per),
  tab_spanner = c("**Anxiety**", "**Depression**", "**Problems with Periods**")
) %>%
  modify_caption("**Summary of Patient Continuous Characteristics by Anxiety, Depression, and Menstrual problems**")

print(cont_tab_all)


#-------------------------------------------------------------------------------
# saving out the master contingency table
saveRDS(
  cont_tab_all,
  (here::here("21-month-timepoint/multiple-imputation-dataset/3_Descriptives", "cont_tab_all_mi.rds"))
)

write.csv(df_working, (here::here("21-month-timepoint/multiple-imputation-dataset/3_Descriptives", "df_working_mi.csv" )))
