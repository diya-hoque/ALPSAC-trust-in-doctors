#Script: data descriptives for original and complete-case datasets at 6 year timepoint

source(here::here("6-year-timepoint/complete-case-dataset/1_Packages", "6y_packages.R"))

df_working <- read.csv(here::here("6-year-timepoint/complete-case-dataset/2_Import_and_Clean", "df_working_6y.csv"))


#-------------------------------------------------------------------------------  
# creating contingency tables 
# anxiety - no confidence - mothers
cont_tab_conf_anx_m <- df_working %>%
  tbl_summary(
    by = m_anx_ans,
    include = m_conf_nhs_ans,
    missing = "no",
    label = list(m_conf_nhs_ans ~ "Mothers",
                 sum_sum_attitude = "Attitudes towards doctors",
                 home_own_status_ans = "Home Ownership Status",
                 parity = "Parity",
                 educ_level_name = "Educational attainment",
                 relig_name = "Religion",
                 mums_hlth_eval = "Evaluation of own health",
                 sum_soc_supp = "Social Support Score",
                 sum_weigh_life = "Weighted life events score",
                 mar_status_ans = "Marital Status",
                 dr_changed_ever = "Recent doctor change",
                 sum_age = "Age",
                 sep = "Socioeconomic Position",
                 sum_anx_score = "Crown-crisp anxiety score",
                 sum_dep_score = "Crown-crisp depression score"
    )
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 1.**")

cont_tab_conf_anx_m

# anxiety - no confidence - partners
cont_tab_conf_anx_p <- df_working %>%
  tbl_summary(
    by = p_anx_ans,
    include = p_conf_nhs_ans,
    missing = "no",
    label = list(p_conf_nhs_ans ~ "Partners")
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 2.**")

cont_tab_conf_anx_p

# anxiety - ill quickly - mothers
cont_tab_quick_anx_m <- df_working %>%
  tbl_summary(
    by = m_anx_ans,
    include = m_ill_quick_ans,
    missing = "no",
    label = list(m_ill_quick_ans ~ "Mothers")
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 3.**")

cont_tab_quick_anx_m

# anxiety - ill quickly - partners
cont_tab_quick_anx_p <- df_working %>%
  tbl_summary(
    by = p_anx_ans,
    include = p_ill_quick_ans,
    missing = "no",
    label = list(p_ill_quick_ans ~ "Partners")
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 4.**")
print(cont_tab_quick_anx_p)

cont_tab_quick_anx_p

# anxiety - always helpful - mothers
cont_tab_help_anx_m <- df_working %>%
  tbl_summary(
    by = m_anx_ans,
    include = m_alwys_help_ans,
    missing = "no",
    label = list(m_alwys_help_ans ~ "Mothers")
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 5.**")

cont_tab_help_anx_m

# anxiety - always helpful - partner
cont_tab_help_anx_p <- df_working %>%
  tbl_summary(
    by = p_anx_ans,
    include = p_alwys_help_ans,
    missing = "no",
    label = list(p_alwys_help_ans ~ "Partners")
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 6.**")

cont_tab_help_anx_p

##
# depression - no confidence - mothers
cont_tab_conf_dep_m <- df_working %>%
  tbl_summary(
    by = m_dep_ans,
    include = m_conf_nhs_ans,
    missing = "no",
    label = list(m_conf_nhs_ans ~ "Mothers")
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 7.**")

cont_tab_conf_dep_m

# depression - no confidence - partners
cont_tab_conf_dep_p <- df_working %>%
  tbl_summary(
    by = p_dep_ans,
    include = p_conf_nhs_ans,
    missing = "no",
    label = list(p_conf_nhs_ans ~ "Partners")
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 8.**")

cont_tab_conf_dep_p

# depression - ill quickly - mothers
cont_tab_quick_dep_m <- df_working %>%
  tbl_summary(
    by = m_dep_ans,
    include = m_ill_quick_ans,
    missing = "no",
    label = list(m_ill_quick_ans ~ "Mothers")
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 9.**")

cont_tab_quick_dep_m

# depression - ill quickly -partners
cont_tab_quick_dep_p <- df_working %>%
  tbl_summary(
    by = p_dep_ans,
    include = p_ill_quick_ans,
    missing = "no",
    label = list(p_ill_quick_ans ~ "Partners")
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 10.**")
print(cont_tab_quick_dep_p)

cont_tab_quick_dep_p

# depression - always helpful - mothers
cont_tab_help_dep_m <- df_working %>%
  tbl_summary(
    by = m_dep_ans,
    include = m_alwys_help_ans,
    missing = "no",
    label = list(m_alwys_help_ans ~ "Mothers")
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 11.**")

cont_tab_help_dep_m 

# depression - always helpful - partner
cont_tab_help_dep_p <- df_working %>%
  tbl_summary(
    by = p_dep_ans,
    include = p_alwys_help_ans,
    missing = "no",
    label = list(p_alwys_help_ans ~ "Partners")
  ) %>%
  bold_labels() %>%
  modify_caption("**Table 12.**")

cont_tab_help_dep_p

#-------------------------------------------------------------------------------
## Contingency tables 

### Anxiety vs No confidence in the NHS

tbl_conf_anx_combined <- tbl_stack(
  tbls = list(cont_tab_conf_anx_m, cont_tab_conf_anx_p)
) %>%
  modify_spanning_header(all_stat_cols() ~ "Question 1: I don’t have any confidence in the national health service vs Did you go to the doctor for Anxiety") %>%
  bold_labels() %>%
  modify_caption("**Combined Table: Mother and Partner Responses**")

tbl_conf_anx_combined




### Anxiety vs Doctor would come quickly for my sick child


tbl_quick_anx_combined <- tbl_stack(
  tbls = list(cont_tab_quick_anx_m, cont_tab_quick_anx_p)
) %>%
  modify_spanning_header(all_stat_cols() ~ "Question 2: I know that if my child was
very ill my doctor would
come quickly vs Did you go to the doctor for Anxiety") %>%
  bold_labels() %>%
  modify_caption("**Combined Table: Mother and Partner Responses**")

tbl_quick_anx_combined


### Anxiety vs Doctor in the clinic always helpful


tbl_help_anx_combined <- tbl_stack(
  tbls = list(cont_tab_help_anx_m, cont_tab_help_anx_p)
) %>%
  modify_spanning_header(all_stat_cols() ~ "Question 3: The doctor in the
clinic is always helpful vs Did you go to the doctor for Anxiety") %>%
  bold_labels() %>%
  modify_caption("**Combined Table: Mother and Partner Responses**")

tbl_help_anx_combined


### Depression vs no confidence in the NHS

tbl_conf_dep_combined <- tbl_stack(
  tbls = list(cont_tab_conf_dep_m, cont_tab_conf_dep_p)
) %>%
  modify_spanning_header(all_stat_cols() ~ "Question 1: I have no confidence in the NHS vs Did you go to the doctor for Depression") %>%
  bold_labels() %>%
  modify_caption("**Combined Table: Mother and Partner Responses**")

tbl_conf_dep_combined


### Depression vs Doctor would come quickly for my sick child

tbl_quick_dep_combined <- tbl_stack(
  tbls = list(cont_tab_quick_dep_m, cont_tab_quick_dep_p)
) %>%
  modify_spanning_header(all_stat_cols() ~ "Question 2: I know that if my child was
very ill my doctor would
come quickly vs Did you go to the doctor for Depression") %>%
  bold_labels() %>%
  modify_caption("**Combined Table: Mother and Partner Responses**")

tbl_quick_dep_combined


### Depression vs Doctor in the clinic always helpful

tbl_help_dep_combined <- tbl_stack(
  tbls = list(cont_tab_help_dep_m, cont_tab_help_dep_p)
) %>%
  modify_spanning_header(all_stat_cols() ~ "Question 3: The doctor in the
clinic is always helpful vs Did you go to the doctor for Depression") %>%
  bold_labels() %>%
  modify_caption("**Combined Table: Mother and Partner Responses**")

tbl_help_dep_combined

#------------------------------------------------------------------------------
# Contingency tables - all covariates  

## Anxiety vs I don't have any confidence in the NHS  

### Mother
cont_tab_all_anx_m_conf <- df_working %>%
  tbl_summary(
    by = m_anx_ans,
    include = c(
      m_home_own_status_ans, m_parity, m_edu_level_name, m_relig_name,
      m_eval_hlth_ans, m_sum_soc_supp, m_sum_weigh_life, m_mar_status_ans,
      m_dr_changed_ever, m_sum_age, m_sep, m_conf_nhs_ans
    ),
    missing = "no",
    label = list(
      m_conf_nhs_ans = "No confidence in the NHS",
      m_home_own_status_ans = "Home Ownership Status",
      m_parity = "Parity",
      m_edu_level_name = "Educational attainment",
      m_relig_name = "Religion",
      m_eval_hlth_ans = "Evaluation of own health",
      m_sum_soc_supp = "Social Support Score",
      m_sum_weigh_life = "Weighted life events score",
      m_mar_status_ans = "Marital Status",
      m_dr_changed_ever = "Recent doctor change",
      m_sum_age = "Age",
      m_sep = "Socioeconomic Position",
      m_sum_anx_score = "Crown-crisp anxiety score",
      m_sum_dep_score = "Crown-crisp depression score"
    )
  ) %>%
  modify_spanning_header(label ~ "Comparing Mother's anxiety to confidence in NHS, with all covariates") %>%
  bold_labels() %>%
  modify_caption("**Table 1.**")

cont_tab_all_anx_m_conf




### Partner

cont_tab_all_anx_p_conf <- df_working %>%
  tbl_summary(
    by = p_anx_ans,
    include = c(
      m_home_own_status_ans, m_parity, p_edu_level_name, p_relig_name,
      p_eval_hlth_ans, p_sum_soc_supp, p_sum_weigh_life, p_mar_status_ans,
      m_dr_changed_ever, p_sum_age, p_sep, p_conf_nhs_ans
    ),
    missing = "no",
    label = list(
      p_conf_nhs_ans = "No confidence in the NHS",
      m_home_own_status_ans = "Home Ownership Status",
      m_parity = "Parity",
      p_edu_level_name = "Educational attainment",
      p_relig_name = "Religion",
      p_eval_hlth_ans = "Evaluation of own health",
      p_sum_soc_supp = "Social Support Score",
      p_sum_weigh_life = "Weighted life events score",
      p_mar_status_ans = "Marital Status",
      m_dr_changed_ever = "Recent doctor change",
      p_sum_age = "Age",
      p_sep = "Socioeconomic Position",
      p_sum_anx_score = "Crown-crisp anxiety score",
      p_sum_dep_score = "Crown-crisp depression score"
    )
  ) %>%
  modify_spanning_header(label ~ "Comparing Partner's anxiety to confidence in NHS, with all covariates") %>%
  bold_labels() %>%
  modify_caption("**Table 2.**")
cont_tab_all_anx_p_conf



## Anxiety vs I know that if my child was very ill my doctor would come quickly  

### Mother


cont_tab_all_anx_m_quick <- df_working %>%
  tbl_summary(
    by = m_anx_ans,
    include = c(
      m_home_own_status_ans, m_parity, m_edu_level_name, m_relig_name,
      m_eval_hlth_ans, m_sum_soc_supp, m_sum_weigh_life, m_mar_status_ans,
      m_dr_changed_ever, m_sum_age, m_sep, m_ill_quick_ans
    ),
    missing = "no",
    label = list(
      m_ill_quick_ans = "If my child was very ill, a doctor would come quickly",
      m_home_own_status_ans = "Home Ownership Status",
      m_parity = "Parity",
      m_edu_level_name = "Educational attainment",
      m_relig_name = "Religion",
      m_eval_hlth_ans = "Evaluation of own health",
      m_sum_soc_supp = "Social Support Score",
      m_sum_weigh_life = "Weighted life events score",
      m_mar_status_ans = "Marital Status",
      m_dr_changed_ever = "Recent doctor change",
      m_sum_age = "Age",
      m_sep = "Socioeconomic Position",
      m_sum_anx_score = "Crown-crisp anxiety score",
      m_sum_dep_score = "Crown-crisp depression score"
    )
  ) %>%
  modify_spanning_header(label ~ "Comparing Mother's anxiety to belief Dr would 
                         come quickly for sick child, with all covariates") %>%
  bold_labels() %>%
  modify_caption("**Table 1.**")

cont_tab_all_anx_m_quick



### Partner


cont_tab_all_anx_p_quick <- df_working %>%
  tbl_summary(
    by = p_anx_ans,
    include = c(
      m_home_own_status_ans, m_parity, p_edu_level_name, p_relig_name,
      p_eval_hlth_ans, p_sum_soc_supp, p_sum_weigh_life, p_mar_status_ans,
      m_dr_changed_ever, p_sum_age, p_sep, p_ill_quick_ans
    ),
    missing = "no",
    label = list(
      p_ill_quick_ans = "If my child was very ill, a doctor would come quickly",
      m_home_own_status_ans = "Home Ownership Status",
      m_parity = "Parity",
      p_edu_level_name = "Educational attainment",
      p_relig_name = "Religion",
      p_eval_hlth_ans = "Evaluation of own health",
      p_sum_soc_supp = "Social Support Score",
      p_sum_weigh_life = "Weighted life events score",
      p_mar_status_ans = "Marital Status",
      m_dr_changed_ever = "Recent doctor change",
      p_sum_age = "Age",
      p_sep = "Socioeconomic Position",
      p_sum_anx_score = "Crown-crisp anxiety score",   
      p_sum_dep_score = "Crown-crisp depression score"
    )
  ) %>%
  modify_spanning_header(label ~ "Comparing Partner's anxiety to belief Dr would 
                         come quickly for sick child, with all covariates") %>%
  bold_labels() %>%
  modify_caption("**Table 4.**")
cont_tab_all_anx_p_quick


## Anxiety vs The doctor in the clinic is always helpful  

### Mother


cont_tab_all_anx_m_help <- df_working %>%
  tbl_summary(
    by = m_anx_ans,
    include = c(
      m_home_own_status_ans, m_parity, m_edu_level_name, m_relig_name,
      m_eval_hlth_ans, m_sum_soc_supp, m_sum_weigh_life, m_mar_status_ans,
      m_dr_changed_ever, m_sum_age, m_sep, m_alwys_help_ans
    ),
    missing = "no",
    label = list(
      m_alwys_help_ans = "The doctor in the clinic is always helpful",
      m_home_own_status_ans = "Home Ownership Status",
      m_parity = "Parity",
      m_edu_level_name = "Educational attainment",
      m_relig_name = "Religion",
      m_eval_hlth_ans = "Evaluation of own health",
      m_sum_soc_supp = "Social Support Score",
      m_sum_weigh_life = "Weighted life events score",
      m_mar_status_ans = "Marital Status",
      m_dr_changed_ever = "Recent doctor change",
      m_sum_age = "Age",
      m_sep = "Socioeconomic Position",
      m_sum_anx_score = "Crown-crisp anxiety score",   
      m_sum_dep_score = "Crown-crisp depression score"
    )
  ) %>%
  modify_spanning_header(label ~ "Comparing Mother's anxiety to belief that the doctors in the clinic are always helpful, with all covariates") %>%
  bold_labels() %>%
  modify_caption("**Table 1.**")

cont_tab_all_anx_m_help


### Partner


cont_tab_all_anx_p_help <- df_working %>%
  tbl_summary(
    by = p_anx_ans,
    include = c(
      m_home_own_status_ans, m_parity, p_edu_level_name, p_relig_name,
      p_eval_hlth_ans, p_sum_soc_supp, p_sum_weigh_life, p_mar_status_ans,
      m_dr_changed_ever, p_sum_age, p_sep, p_alwys_help_ans
    ),
    missing = "no",
    label = list(
      p_alwys_help_ans = "The doctor in the clinic is always helpful",
      m_home_own_status_ans = "Home Ownership Status",
      m_parity = "Parity",
      p_edu_level_name = "Educational attainment",
      p_relig_name = "Religion",
      p_eval_hlth_ans = "Evaluation of own health",
      p_sum_soc_supp = "Social Support Score",
      p_sum_weigh_life = "Weighted life events score",
      p_mar_status_ans = "Marital Status",
      m_dr_changed_ever = "Recent doctor change",
      p_sum_age = "Age",
      p_sep = "Socioeconomic Position",
      p_sum_anx_score = "Crown-crisp anxiety score",   
      p_sum_dep_score = "Crown-crisp depression score"
    )
  ) %>%
  modify_spanning_header(label ~ "Comparing Partner's anxiety to belief that the doctors in the clinic are always helpful, with all covariates") %>%
  bold_labels() %>%
  modify_caption("**Table 6.**")

print(cont_tab_all_anx_p_help)


## Depression vs I don't have any confidence in the NHS  

### Mother



cont_tab_all_dep_m_conf <- df_working %>%
  tbl_summary(
    by = m_dep_ans,
    include = c(
      m_home_own_status_ans, m_parity, m_edu_level_name, m_relig_name,
      m_eval_hlth_ans, m_sum_soc_supp, m_sum_weigh_life, m_mar_status_ans,
      m_dr_changed_ever, m_sum_age, m_sep, m_conf_nhs_ans
    ),
    missing = "no",
    label = list(
      m_conf_nhs_ans = "No confidence in the NHS",
      m_home_own_status_ans = "Home Ownership Status",
      m_parity = "Parity",
      m_edu_level_name = "Educational attainment",
      m_relig_name = "Religion",
      m_eval_hlth_ans = "Evaluation of own health",
      m_sum_soc_supp = "Social Support Score",
      m_sum_weigh_life = "Weighted life events score",
      m_mar_status_ans = "Marital Status",
      m_dr_changed_ever = "Recent doctor change",
      m_sum_age = "Age",
      m_sep = "Socioeconomic Position",
      m_sum_anx_score = "Crown-crisp anxiety score",   
      m_sum_dep_score = "Crown-crisp depression score"
    )
  ) %>%
  modify_spanning_header(label ~ "Comparing Mother's depression to confidence in NHS, with all covariates") %>%
  bold_labels() %>%
  modify_caption("**Table 1.**")

cont_tab_all_dep_m_conf




### Partner


cont_tab_all_dep_p_conf <- df_working %>%
  tbl_summary(
    by = p_dep_ans,
    include = c(
      m_home_own_status_ans, m_parity, p_edu_level_name, p_relig_name,
      p_eval_hlth_ans, p_sum_soc_supp, p_sum_weigh_life, p_mar_status_ans,
      m_dr_changed_ever, p_sum_age, p_sep, p_conf_nhs_ans
    ),
    missing = "no",
    label = list(
      p_conf_nhs_ans = "No confidence in the NHS",
      m_home_own_status_ans = "Home Ownership Status",
      m_parity = "Parity",
      p_edu_level_name = "Educational attainment",
      p_relig_name = "Religion",
      p_eval_hlth_ans = "Evaluation of own health",
      p_sum_soc_supp = "Social Support Score",
      p_sum_weigh_life = "Weighted life events score",
      p_mar_status_ans = "Marital Status",
      m_dr_changed_ever = "Recent doctor change",
      p_sum_age = "Age",
      p_sep = "Socioeconomic Position",
      p_sum_anx_score = "Crown-crisp anxiety score",   
      p_sum_dep_score = "Crown-crisp depression score"
    )
  ) %>%
  modify_spanning_header(label ~ "Comparing Partner's depression to confidence in NHS, with all covariates") %>%
  bold_labels() %>%
  modify_caption("**Table 2.**")
cont_tab_all_dep_p_conf



## Depression vs I know that if my child was very ill my doctor would come quickly  

### Mother


cont_tab_all_dep_m_quick <- df_working %>%
  tbl_summary(
    by = m_dep_ans,
    include = c(
      m_home_own_status_ans, m_parity, m_edu_level_name, m_relig_name,
      m_eval_hlth_ans, m_sum_soc_supp, m_sum_weigh_life, m_mar_status_ans,
      m_dr_changed_ever, m_sum_age, m_sep, m_ill_quick_ans
    ),
    missing = "no",
    label = list(
      m_ill_quick_ans = "If my child was very ill, a doctor would come quickly",
      m_home_own_status_ans = "Home Ownership Status",
      m_parity = "Parity",
      m_edu_level_name = "Educational attainment",
      m_relig_name = "Religion",
      m_eval_hlth_ans = "Evaluation of own health",
      m_sum_soc_supp = "Social Support Score",
      m_sum_weigh_life = "Weighted life events score",
      m_mar_status_ans = "Marital Status",
      m_dr_changed_ever = "Recent doctor change",
      m_sum_age = "Age",
      m_sep = "Socioeconomic Position",
      m_sum_anx_score = "Crown-crisp anxiety score",   
      m_sum_dep_score = "Crown-crisp depression score"
    )
  ) %>%
  modify_spanning_header(label ~ "Comparing Mother's depression to belief Dr would 
                         come quickly for sick child, with all covariates") %>%
  bold_labels() %>%
  modify_caption("**Table 1.**")

cont_tab_all_dep_m_quick



### Partner


cont_tab_all_dep_p_quick <- df_working %>%
  tbl_summary(
    by = p_dep_ans,
    include = c(
      m_home_own_status_ans, m_parity, p_edu_level_name, p_relig_name,
      p_eval_hlth_ans, p_sum_soc_supp, p_sum_weigh_life, p_mar_status_ans,
      m_dr_changed_ever, p_sum_age, p_sep, p_ill_quick_ans
    ),
    missing = "no",
    label = list(
      p_ill_quick_ans = "If my child was very ill, a doctor would come quickly",
      m_home_own_status_ans = "Home Ownership Status",
      m_parity = "Parity",
      p_edu_level_name = "Educational attainment",
      p_relig_name = "Religion",
      p_eval_hlth_ans = "Evaluation of own health",
      p_sum_soc_supp = "Social Support Score",
      p_sum_weigh_life = "Weighted life events score",
      p_mar_status_ans = "Marital Status",
      m_dr_changed_ever = "Recent doctor change",
      p_sum_age = "Age",
      p_sep = "Socioeconomic Position",
      p_sum_anx_score = "Crown-crisp anxiety score",   
      p_sum_dep_score = "Crown-crisp depression score"
    )
  ) %>%
  modify_spanning_header(label ~ "Comparing Partner's depression to belief Dr would 
                         come quickly for sick child, with all covariates") %>%
  bold_labels() %>%
  modify_caption("**Table 4.**")
cont_tab_all_dep_p_quick


## Depression vs The doctor in the clinic is always helpful  

### Mother


cont_tab_all_dep_m_help <- df_working %>%
  tbl_summary(
    by = m_dep_ans,
    include = c(
      m_home_own_status_ans, m_parity, m_edu_level_name, m_relig_name,
      m_eval_hlth_ans, m_sum_soc_supp, m_sum_weigh_life, m_mar_status_ans,
      m_dr_changed_ever, m_sum_age, m_sep, m_alwys_help_ans
    ),
    missing = "no",
    label = list(
      m_alwys_help_ans = "The doctor in the clinic is always helpful",
      m_home_own_status_ans = "Home Ownership Status",
      m_parity = "Parity",
      m_edu_level_name = "Educational attainment",
      m_relig_name = "Religion",
      m_eval_hlth_ans = "Evaluation of own health",
      m_sum_soc_supp = "Social Support Score",
      m_sum_weigh_life = "Weighted life events score",
      m_mar_status_ans = "Marital Status",
      m_dr_changed_ever = "Recent doctor change",
      m_sum_age = "Age",
      m_sep = "Socioeconomic Position",
      m_sum_anx_score = "Crown-crisp anxiety score",   
      m_sum_dep_score = "Crown-crisp depression score"
    )
  ) %>%
  modify_spanning_header(label ~ "Comparing Mother's depression to belief that the doctors in the clinic are always helpful, with all covariates") %>%
  bold_labels() %>%
  modify_caption("**Table 1.**")

cont_tab_all_dep_m_help


### Partner


cont_tab_all_dep_p_help <- df_working %>%
  tbl_summary(
    by = p_dep_ans,
    include = c(
      m_home_own_status_ans, m_parity, p_edu_level_name, p_relig_name,
      p_eval_hlth_ans, p_sum_soc_supp, p_sum_weigh_life, p_mar_status_ans,
      m_dr_changed_ever, p_sum_age, p_sep, p_alwys_help_ans
    ),
    missing = "no",
    label = list(
      p_alwys_help_ans = "The doctor in the clinic is always helpful",
      m_home_own_status_ans = "Home Ownership Status",
      m_parity = "Parity",
      p_edu_level_name = "Educational attainment",
      p_relig_name = "Religion",
      p_eval_hlth_ans = "Evaluation of own health",
      p_sum_soc_supp = "Social Support Score",
      p_sum_weigh_life = "Weighted life events score",
      p_mar_status_ans = "Marital Status",
      m_dr_changed_ever = "Recent doctor change",
      p_sum_age = "Age",
      p_sep = "Socioeconomic Position",
      p_sum_anx_score = "Crown-crisp anxiety score",   
      p_sum_dep_score = "Crown-crisp depression score"
    )
  ) %>%
  modify_spanning_header(label ~ "Comparing Partner's depression to belief that the doctors in the clinic are always helpful, with all covariates") %>%
  bold_labels() %>%
  modify_caption("**Table 6.**")

print(cont_tab_all_anx_p_help)


write.csv(df_working, (here::here("6-year-timepoint/complete-case-dataset/3_Descriptives", "df_working_6y.csv" )))