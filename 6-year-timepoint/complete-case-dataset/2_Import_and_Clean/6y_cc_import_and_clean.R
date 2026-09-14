# Script: data import and cleaning for complete-case dataset for 6 year timepoint

library(here) # to create relative paths

source(here::here("6-year-timepoint/complete-case-dataset/1_Packages", "6y_packages.R"))

stata_data_linked <- read_dta(here::here("6-year-timepoint/complete-case-dataset/Data", "linked_data_5.dta"))


#---------------------------------------------------------------------
# creating a data frame
all_data <- data.frame(stata_data_linked)

#-------------------------------------------------------------------------------
# duplicating and renaming variables 
# duplicating and renaming variables 
cols_to_duplicate <- c("l9996a", "b032", "j370", "d813", "c755",
                       "c645a", "a006", "l3000", "g948", 
                       "g227", "j346", "g269", "g275", 
                       
                       
                       "pj9996a", "ph7040", "pb153", "c765", "pb325a", "pj3000", "pe211", "pe345", 
                       "pj2100", "pf4110",
                       
                       "l8010", "l8011", "l8012", "pj8010", "pj8011", "pj8012",
                       "l3010", "l3011", "pj3010", "pj3011")

newcol_names <- c("m_age", "m_parity", "m_mar_status", "m_relig", "m_sep",
                  "m_edu_level", "m_home_own_status", "m_eval_hlth", "m_dr_change",
                  "m_soc_supp", "m_weigh_life", "m_anx_score", "m_dep_score",
                  
                  "p_age", "p_mar_status", "p_relig", "p_sep", "p_edu_level", "p_eval_hlth", "p_soc_supp",     
                  "p_weigh_life", "p_anx_score", "p_dep_score",
                  "m_conf_nhs", "m_ill_quick", "m_alwys_help", "p_conf_nhs", "p_ill_quick", "p_alwys_help",
                  "m_anx", "m_dep", "p_anx", "p_dep")


covariates <- c("m_age", "m_parity", "m_mar_status", "m_relig", "m_sep",
                "m_edu_level", "m_home_own_status", "m_eval_hlth", "m_dr_change",
                "m_soc_supp", "m_weigh_life", "p_age", "p_mar_status", "p_ethn_group",
                "p_relig", "p_sep", "p_edu_level", "p_eval_hlth", "p_soc_supp", 
                "p_weigh_life")
exposures <- c("m_conf_nhs", "m_ill_quick", "m_alwys_help", "p_conf_nhs", "p_ill_quick", "p_alwys_help")

outcomes <- c("m_anx", "m_dep", "p_anx", "p_dep")

must_keep <- c("aln", "mz010a", "mum_in_alsp", "mum_in_core", "mum_enrol_status",
               "mum_and_preg_enrolled", "mz005l", "mz005m", "mz013", "mz014",
               "mz028b", "a006", "a525", "b032", "b650", "b663", "b665", "b667", "c645a", "c755", "c765",
               "bestgest", "aln", "partner_in_alspac", "partner_data", "partner_enrolled", 
               "partner_in_core", "pz_mult", "pz_multid", "partner_changed", "partner_changed_when",
               "partner_age", "second_partner_age")


#-------------------------------------------------------------------------------
#creating a data frame with all variables and all renamed variables
all_data <- all_data
all_data[newcol_names] <- all_data[cols_to_duplicate]

#-------------------------------------------------------------------------------
#creating a data frame with all variables (but replacing orignally named to renamed)

df_working <- all_data %>%
  select("aln", "mz010a", "mum_in_alsp", "mum_in_core", "mum_enrol_status",
         "mum_and_preg_enrolled", "mz005l", "mz005m", "mz013", "mz014",
         "mz028b", "a006", "a525", "b032", "b650", "b663", "b665", "b667", "c645a", 
         "c755", "c765",
         "bestgest", "aln", "partner_in_alspac", "partner_data", "partner_enrolled", 
         "partner_in_core", "pz_mult", "pz_multid", "partner_changed", 
         "partner_changed_when",
         "partner_age", "second_partner_age", "m_age", "m_parity", "m_mar_status",  
         "m_relig", "m_sep",
         "m_edu_level", "m_home_own_status", "m_eval_hlth", "m_dr_change",
         "m_soc_supp", "m_weigh_life", "m_anx_score", "m_dep_score",
         
         "p_age", "p_mar_status", 
         "p_relig", "p_sep", "p_edu_level", "p_eval_hlth", "p_soc_supp", 
         "p_weigh_life", "p_anx_score", "p_dep_score", 
         "m_conf_nhs", "m_ill_quick", "m_alwys_help", "p_conf_nhs", "p_ill_quick", 
         "p_alwys_help",
         "m_anx", "m_dep", "p_anx", "p_dep")


# this data frame contains all variables original to the dataset but my outcomes,
# exposures and covariates have been renamed for clarity and ease
# changing haven.labelled to numeric

df_working <- df_working %>%
  mutate(across(where(~ inherits(., "haven_labelled")), ~ as.numeric(.)))

#-------------------------------------------------------------------------------
# filtering so only mothers under 51 are included
df_working <- df_working %>%
  filter(m_age < 51| is.na(m_age))

#-------------------------------------------------------------------------------
# removing all irrelevant numbers

# exposures
df_working$m_conf_nhs[df_working$m_conf_nhs == -1] <- NA
df_working$m_ill_quick[df_working$m_ill_quick == -1] <- NA
df_working$m_alwys_help[df_working$m_alwys_help == -1] <- NA

df_working$m_conf_nhs[df_working$m_conf_nhs == -11] <- NA
df_working$m_ill_quick[df_working$m_ill_quick == -11] <- NA
df_working$m_alwys_help[df_working$m_alwys_help == -11] <- NA

df_working$m_conf_nhs[df_working$m_conf_nhs == -10] <- NA
df_working$m_ill_quick[df_working$m_ill_quick == -10] <- NA
df_working$m_alwys_help[df_working$m_alwys_help == -10] <- NA

df_working$m_conf_nhs[df_working$m_conf_nhs == -9] <- NA
df_working$m_ill_quick[df_working$m_ill_quick == -9] <- NA
df_working$m_alwys_help[df_working$m_alwys_help == -9] <- NA

df_working$m_conf_nhs[df_working$m_conf_nhs == -8] <- NA
df_working$m_ill_quick[df_working$m_ill_quick == -8] <- NA
df_working$m_alwys_help[df_working$m_alwys_help == -8] <- NA

df_working$p_conf_nhs[df_working$p_conf_nhs == -1] <- NA
df_working$p_ill_quick[df_working$p_ill_quick == -1] <- NA
df_working$p_alwys_help[df_working$p_alwys_help == -1] <- NA

df_working$p_conf_nhs[df_working$p_conf_nhs == -11] <- NA
df_working$p_ill_quick[df_working$p_ill_quick == -11] <- NA
df_working$p_alwys_help[df_working$p_alwys_help == -11] <- NA

df_working$p_conf_nhs[df_working$p_conf_nhs == -10] <- NA
df_working$p_ill_quick[df_working$p_ill_quick == -10] <- NA
df_working$p_alwys_help[df_working$p_alwys_help == -10] <- NA

df_working$p_conf_nhs[df_working$p_conf_nhs == -9] <- NA
df_working$p_ill_quick[df_working$p_ill_quick == -9] <- NA
df_working$p_alwys_help[df_working$p_alwys_help == -9] <- NA

df_working$p_conf_nhs[df_working$p_conf_nhs == -8] <- NA
df_working$p_ill_quick[df_working$p_ill_quick == -8] <- NA
df_working$p_alwys_help[df_working$p_alwys_help == -8] <- NA

# outcomes
df_working$m_anx[df_working$m_anx == -1] <- NA
df_working$m_anx[df_working$m_anx == -11] <- NA
df_working$m_anx[df_working$m_anx == -10] <- NA
df_working$m_anx[df_working$m_anx == -8] <- NA

df_working$p_anx[df_working$p_anx == -1] <- NA
df_working$p_anx[df_working$p_anx == -11] <- NA
df_working$p_anx[df_working$p_anx == -10] <- NA
df_working$p_anx[df_working$p_anx == -8] <- NA

df_working$m_dep[df_working$m_dep == -1] <- NA
df_working$m_dep[df_working$m_dep == -11] <- NA
df_working$m_dep[df_working$m_dep == -10] <- NA
df_working$m_dep[df_working$m_dep == -8] <- NA

df_working$p_dep[df_working$p_dep == -1] <- NA
df_working$p_dep[df_working$p_dep == -11] <- NA
df_working$p_dep[df_working$p_dep == -10] <- NA
df_working$p_dep[df_working$p_dep == -8] <- NA


# covariates 
df_working$m_age[df_working$m_age == -1] <- NA
df_working$m_age[df_working$m_age == -11] <- NA
df_working$m_age[df_working$m_age == -10] <- NA
df_working$m_parity[df_working$m_parity == -1] <- NA
df_working$m_parity[df_working$m_parity == -2] <- NA
df_working$m_parity[df_working$m_parity == -7] <- NA
df_working$m_mar_status[df_working$m_mar_status == -1] <- NA
df_working$m_relig[df_working$m_relig == -1] <- NA
df_working$m_sep[df_working$m_sep == -1] <- NA
df_working$m_sep[df_working$m_sep == 65] <- NA
df_working$m_edu_level[df_working$m_edu_level == -1] <- NA
df_working$m_home_own_status[df_working$m_home_own_status == -1] <- NA
df_working$m_eval_hlth[df_working$m_eval_hlth == -1] <- NA
df_working$m_dr_change[df_working$m_dr_change == -1] <- NA
df_working$m_soc_supp[df_working$m_soc_supp == -1] <- NA
df_working$m_weigh_life[df_working$m_weigh_life == -1] <- NA
df_working$p_age[df_working$p_age == -1] <- NA
df_working$p_mar_status[df_working$p_mar_status == -1] <- NA
df_working$p_relig[df_working$p_relig == -1] <- NA
df_working$p_sep[df_working$p_sep == -1] <- NA
df_working$p_sep[df_working$p_sep == 65] <- NA
df_working$p_edu_level[df_working$p_edu_level == -1] <- NA
df_working$p_eval_hlth[df_working$p_eval_hlth == -1] <- NA
df_working$p_soc_supp[df_working$p_soc_supp == -1] <- NA
df_working$p_weigh_life[df_working$p_weigh_life == -1] <- NA

df_working$m_anx_score[df_working$m_anx_score == -1] <- NA
df_working$m_dep_score[df_working$m_dep_score == -1] <- NA
df_working$p_anx_score[df_working$p_anx_score == -10] <- NA
df_working$p_anx_score[df_working$p_anx_score == -1] <- NA
df_working$p_dep_score[df_working$p_dep_score == -11] <- NA
df_working$p_dep_score[df_working$p_dep_score == -11] <- NA
df_working$p_dep_score[df_working$p_dep_score == -1] <- NA

#-----------------------------------------------------------------------------------
# recoding so variables are more intuitive 
# exposures
# m_conf_nhs is already coded so that the higher the score, the more positive the 
# perception of the NHS

df_working$m_ill_quick <- as.numeric(as.character(df_working$m_ill_quick))

df_working <- df_working %>%
  mutate(m_ill_quick = ifelse(m_ill_quick == 1, 4,
                              ifelse(m_ill_quick == 2, 3,
                                     ifelse(m_ill_quick == 3, 2,
                                            ifelse(m_ill_quick == 4, 1, NA_real_)))))

df_working$m_alwys_help <- as.numeric(as.character(df_working$m_alwys_help))

df_working <- df_working %>%
  mutate(m_alwys_help = ifelse(m_alwys_help == 1, 4,
                               ifelse(m_alwys_help == 2, 3,
                                      ifelse(m_alwys_help == 3, 2,
                                             ifelse(m_alwys_help == 4, 1, NA_real_)))))

# p_conf_nhs is already coded so that the higher the score, the more positive the 
# perception of the NHS

df_working$p_ill_quick <- as.numeric(as.character(df_working$p_ill_quick))

df_working <- df_working %>%
  mutate(p_ill_quick = ifelse(p_ill_quick == 1, 4,
                              ifelse(p_ill_quick == 2, 3,
                                     ifelse(p_ill_quick == 3, 2,
                                            ifelse(p_ill_quick == 4, 1, NA_real_)))))

df_working$p_alwys_help <- as.numeric(as.character(df_working$p_alwys_help))

df_working <- df_working %>%
  mutate(p_alwys_help = ifelse(p_alwys_help == 1, 4,
                               ifelse(p_alwys_help == 2, 3,
                                      ifelse(p_alwys_help == 3, 2,
                                             ifelse(p_alwys_help == 4, 1, NA_real_)))))

# outcomes
# anxiety
df_working$m_anx <- as.numeric(as.character(df_working$m_anx))

df_working$m_anx <- ifelse(df_working$m_anx == 1, 1,
                           ifelse(df_working$m_anx == 2, 0,
                                  ifelse(df_working$m_anx == 3, 2, df_working$m_anx)))

df_working$p_anx <- as.numeric(as.character(df_working$p_anx))

df_working$p_anx <- ifelse(df_working$p_anx == 1, 1,
                           ifelse(df_working$p_anx == 2, 0,
                                  ifelse(df_working$p_anx == 3, 2, df_working$p_anx)))

#depression
df_working$m_dep <- as.numeric(as.character(df_working$m_dep))

df_working$m_dep <- ifelse(df_working$m_dep == 1, 1,
                           ifelse(df_working$m_dep == 2, 0,
                                  ifelse(df_working$m_dep == 3, 2, df_working$m_dep)))

df_working$p_dep <- as.numeric(as.character(df_working$p_dep))

df_working$p_dep <- ifelse(df_working$p_dep == 1, 1,
                           ifelse(df_working$p_dep == 2, 0,
                                  ifelse(df_working$p_dep == 3, 2, df_working$p_dep)))

# covariates
# parity
df_working <- df_working %>%
  mutate(m_parity = case_when(
    m_parity == 0 ~ "0",
    m_parity == 1 ~ "1",
    m_parity == 2 ~ "2",
    m_parity == 3 ~ "3",
    m_parity == 4 ~ "4",
    m_parity == 5 ~ "5",
    m_parity %in% c(6, 7, 8, 11, 13, 22) ~ "6<=",
    TRUE ~ NA_character_
  ))

# marital status 
df_working$m_mar_status <- as.numeric(as.character(df_working$m_mar_status))

df_working <- df_working %>%
  mutate(m_mar_status = case_when(
    m_mar_status == 1 ~ 0,
    m_mar_status == 2 ~ 0,
    m_mar_status == 3 ~ 0,
    m_mar_status == 4 ~ 0,
    m_mar_status == 5 ~ 1,
    m_mar_status == 6 ~ 1,
    TRUE ~ NA_real_
  ))

df_working$p_mar_status <- as.numeric(as.character(df_working$p_mar_status))

df_working <- df_working %>%
  mutate(p_mar_status = case_when(
    p_mar_status == 1 ~ 0,
    p_mar_status == 2 ~ 0,
    p_mar_status == 3 ~ 0,
    p_mar_status == 4 ~ 0,
    p_mar_status == 5 ~ 1,
    p_mar_status == 6 ~ 1,
    TRUE ~ NA_real_
  ))



# religion - Christian, non-Christian or none
df_working$m_relig <- as.numeric(as.character(df_working$m_relig))

df_working <- df_working %>%
  mutate(m_relig = case_when(
    m_relig == 0 ~ 2,
    m_relig == 1 ~ 0,
    m_relig == 2 ~ 0,
    m_relig == 3 ~ 0,
    m_relig == 4 ~ 0,
    m_relig == 5 ~ 0,
    m_relig == 6 ~ 0,
    m_relig == 7 ~ 1,
    m_relig == 8 ~ 1,
    m_relig == 9 ~ 1,
    m_relig == 10 ~ 1,
    m_relig == 11 ~ 1,
    m_relig == 12 ~ 1,
    m_relig == 13 ~ 1,
    TRUE ~ NA_real_
  ))

df_working$p_relig <- as.numeric(as.character(df_working$p_relig))

df_working <- df_working %>%
  mutate(p_relig = case_when(
    p_relig == 0 ~ 2,
    p_relig == 1 ~ 0,
    p_relig == 2 ~ 0,
    p_relig == 3 ~ 0,
    p_relig == 4 ~ 0,
    p_relig == 5 ~ 0,
    p_relig == 6 ~ 0,
    p_relig == 7 ~ 1,
    p_relig == 8 ~ 1,
    p_relig == 9 ~ 1,
    p_relig == 10 ~ 1,
    p_relig == 11 ~ 1,
    p_relig == 12 ~ 1,
    p_relig == 13 ~ 1,
    TRUE ~ NA_real_
  ))

# education level
df_working$m_edu_level <- as.numeric(as.character(df_working$m_edu_level))
df_working <- df_working %>%
  mutate(m_edu_level = case_when(
    m_edu_level == 1 ~ 0,
    m_edu_level == 2 ~ 0,
    m_edu_level == 3 ~ 0,
    m_edu_level == 4  ~ 1,
    m_edu_level == 5  ~ 2,
    TRUE ~ NA_real_
  ))

df_working$p_edu_level <- as.numeric(as.character(df_working$p_edu_level))
df_working <- df_working %>%
  mutate(p_edu_level = case_when(
    p_edu_level == 1 ~ 0,
    p_edu_level == 2 ~ 0,
    p_edu_level == 3 ~ 0,
    p_edu_level == 4  ~ 1,
    p_edu_level == 5  ~ 2,
    TRUE ~ NA_real_
  ))

# home ownership status
df_working$m_home_own_status <- as.numeric(as.character(df_working$m_home_own_status))

df_working <- df_working %>%
  mutate(m_home_own_status = case_when(
    m_home_own_status == 7 ~ 0,
    m_home_own_status == 3 ~ 1,
    m_home_own_status == 6 ~ 1,
    m_home_own_status == 4 ~ 2,
    m_home_own_status == 5 ~ 2,
    m_home_own_status == 0 ~ 3,
    m_home_own_status == 1 ~ 3,
    m_home_own_status == 2 ~ 4,
    TRUE ~ NA_real_
  ))

#evaluation of own health
df_working$m_eval_hlth <- as.numeric(as.character(df_working$m_eval_hlth))

df_working <- df_working %>%
  mutate(m_eval_hlth = case_when(
    m_eval_hlth == 1 ~ 4,
    m_eval_hlth == 2 ~ 3,
    m_eval_hlth == 3 ~ 2,
    m_eval_hlth == 4 ~ 1,
    TRUE ~ NA_real_
  ))

df_working$p_eval_hlth <- as.numeric(as.character(df_working$p_eval_hlth))

df_working <- df_working %>%
  mutate(p_eval_hlth = case_when(
    p_eval_hlth == 1 ~ 4,
    p_eval_hlth == 2 ~ 3,
    p_eval_hlth == 3 ~ 2,
    p_eval_hlth == 4 ~ 1,
    TRUE ~ NA_real_
  ))

# whether or not Dr has been changed
df_working$m_dr_change <- as.numeric(as.character(df_working$m_dr_change))

df_working <- df_working %>%
  mutate(m_dr_change = case_when(
    m_dr_change == 1 ~ 1,
    m_dr_change == 2 ~ 0,
    TRUE ~ NA_real_
  ))

# socioeconomic position - split into professional managerial/other - reversing the scale 
df_working <- df_working %>%
  mutate(m_sep = case_when(
    m_sep == 1 ~ 1,
    m_sep == 2 ~ 1,
    m_sep == 3 ~ 0,
    m_sep == 4  ~ 0,
    m_sep == 5  ~ 0,
    m_sep == 6  ~ 0,
    TRUE ~ NA_real_
  ))


df_working <- df_working %>%
  mutate(p_sep = case_when(
    p_sep == 1 ~ 1,
    p_sep == 2 ~ 1,
    p_sep == 3 ~ 0,
    p_sep == 4  ~ 0,
    p_sep == 5  ~ 0,
    p_sep == 6  ~ 0,
    TRUE ~ NA_real_
  ))

#-------------------------------------------------------------------------------
# adding labels to values

# exposures
df_working <- df_working %>%
  mutate(m_conf_nhs_ans = case_when(
    m_conf_nhs == 1 ~ "Exactly",
    m_conf_nhs == 2 ~ "Often",
    m_conf_nhs == 3 ~ "Sometimes",
    m_conf_nhs == 4  ~ "Never"
  ))

df_working <- df_working %>%
  mutate(m_ill_quick_ans = case_when(
    m_ill_quick == 1 ~ "Never",
    m_ill_quick == 2 ~ "Sometimes",
    m_ill_quick == 3 ~ "Often",
    m_ill_quick == 4  ~ "Exactly"
  ))

df_working <- df_working %>%
  mutate(m_alwys_help_ans = case_when(
    m_alwys_help == 1 ~ "Never",
    m_alwys_help == 2 ~ "Sometimes",
    m_alwys_help == 3 ~ "Often",
    m_alwys_help == 4  ~ "Exactly"
  ))


df_working <- df_working %>%
  mutate(p_conf_nhs_ans = case_when(
    p_conf_nhs == 1 ~ "Exactly",
    p_conf_nhs == 2 ~ "Often",
    p_conf_nhs == 3 ~ "Sometimes",
    p_conf_nhs == 4  ~ "Never"
  ))

df_working <- df_working %>%
  mutate(p_ill_quick_ans = case_when(
    p_ill_quick == 1 ~ "Never",
    p_ill_quick == 2 ~ "Sometimes",
    p_ill_quick == 3 ~ "Often",
    p_ill_quick == 4  ~ "Exactly"
  ))

df_working <- df_working %>%
  mutate(p_alwys_help_ans = case_when(
    p_alwys_help == 1 ~ "Never",
    p_alwys_help == 2 ~ "Sometimes",
    p_alwys_help == 3 ~ "Often",
    p_alwys_help == 4  ~ "Exactly"
  ))

#outcomes
df_working <- df_working %>%
  mutate(m_anx_ans = case_when(
    m_anx == 0 ~ "Yes but saw no Dr",
    m_anx == 1 ~ "Yes and saw Dr",
    m_anx == 2 ~ "No"
  ))

df_working <- df_working %>%
  mutate(p_anx_ans = case_when(
    p_anx == 0 ~ "Yes but saw no Dr",
    p_anx == 1 ~ "Yes and saw Dr",
    p_anx == 2 ~ "No"
  ))

df_working <- df_working %>%
  mutate(m_dep_ans = case_when(
    m_dep == 0 ~ "Yes but saw no Dr",
    m_dep == 1 ~ "Yes and saw Dr",
    m_dep == 2 ~ "No"
  ))

df_working <- df_working %>%
  mutate(p_dep_ans = case_when(
    p_dep == 0 ~ "Yes but saw no Dr",
    p_dep == 1 ~ "Yes and saw Dr",
    p_dep == 2 ~ "No"
  ))

# marital status
df_working <- df_working %>%
  mutate(m_mar_status_ans = case_when(
    p_mar_status == 0 ~ "Not currently married",
    p_mar_status == 1 ~ "Married"
  ))

df_working <- df_working %>%
  mutate(p_mar_status_ans = case_when(
    p_mar_status == 0 ~ "Not currently married",
    p_mar_status == 1 ~ "Married"
  ))

# recent dr change
df_working <- df_working %>%
  mutate(
    m_dr_changed_ever = ifelse(
      m_dr_change == 0,
      "No recent Dr change",
      ifelse(
        m_dr_change == 1,
        "Recent Dr change",
        NA_character_
      )
    )
  )

# religion
df_working <- df_working %>%
  mutate(m_relig_name = case_when(
    m_relig == 0 ~ "Christian",
    m_relig == 1 ~ "Non-Christian",
    m_relig == 2 ~ "None"
  ))

df_working <- df_working %>%
  mutate(p_relig_name = case_when(
    p_relig == 0 ~ "Christian",
    p_relig == 1 ~ "Non-Christian",
    p_relig == 2 ~ "None"
  ))

# education level
df_working <- df_working %>%
  mutate(m_edu_level_name = case_when(
    m_edu_level == 0 ~ "O-level, Vocational, CSE, GCSE or none",
    m_edu_level == 1 ~ "A-level",
    m_edu_level == 2 ~ "Degree"
  ))

df_working <- df_working %>%
  mutate(p_edu_level_name = case_when(
    p_edu_level == 0 ~ "O-level, Vocational, CSE, GCSE or none",
    p_edu_level == 1 ~ "A-level",
    p_edu_level == 2 ~ "Degree"
  ))

# mother's home ownership status
df_working <- df_working %>%
  mutate(m_home_own_status_ans = case_when(
    m_home_own_status == 0 ~ "Other",
    m_home_own_status == 1 ~ "Publicly rented",
    m_home_own_status == 2 ~ "Privately rented",
    m_home_own_status == 3 ~ "Mortgaged",
    m_home_own_status == 4 ~ "Owned"
  ))

# evaluation of own health
df_working <- df_working %>%
  mutate(m_eval_hlth_ans = case_when(
    m_eval_hlth == 1 ~ "Never well",
    m_eval_hlth == 2 ~ "Often unwell",
    m_eval_hlth == 3 ~ "Mostly well",
    m_eval_hlth == 4  ~ "Fit and well"
  ))

df_working <- df_working %>%
  mutate(p_eval_hlth_ans = case_when(
    p_eval_hlth == 1 ~ "Never well",
    p_eval_hlth == 2 ~ "Often unwell",
    p_eval_hlth == 3 ~ "Mostly well",
    p_eval_hlth == 4  ~ "Fit and well"
  ))

df_working <- df_working %>%
  mutate(m_sep_ans = case_when(
    m_sep == 1 ~ "Professional/managerial",
    m_sep == 0 ~ "Other"
  ))

df_working <- df_working %>%
  mutate(p_sep_ans = case_when(
    p_sep == 1 ~ "Professional/managerial",
    p_sep == 0 ~ "Other"
  ))



col_order <- c("aln", "mz010a", "mum_in_alsp", "mum_in_core", "mum_enrol_status",
               "mum_and_preg_enrolled", "mz005l", "mz005m", "mz013", "mz014",
               "mz028b", "a525", "b650", "b663", "b665", "b667", "c755", "c765",
               "bestgest", "aln", "partner_in_alspac", "partner_data", 
               "partner_enrolled", 
               "partner_in_core", "pz_mult", "pz_multid", "partner_changed", 
               "partner_changed_when",
               "partner_age", "second_partner_age", "m_conf_nhs", "m_conf_nhs_ans",
               "m_ill_quick", "m_ill_quick_ans", "m_alwys_help", "m_alwys_help_ans",
               "p_conf_nhs", "p_conf_nhs_ans", "p_ill_quick", "p_ill_quick_ans",
               "p_alwys_help", "p_alwys_help_ans",
               "m_anx", "m_anx_ans", "m_dep", "m_dep_ans", "p_anx", "p_anx_ans", 
               "p_dep", "p_dep_ans", "m_sep", "m_sep_ans",
               "m_age", "m_parity", "m_mar_status", "m_mar_status_ans", "m_relig", 
               "m_relig_name", "m_sep", "m_edu_level",
               "m_edu_level_name", "m_home_own_status", "m_home_own_status_ans",
               "m_eval_hlth", "m_eval_hlth_ans", "m_dr_change", "m_dr_changed_ever",
               "m_soc_supp", "m_weigh_life", "m_anx_score", "m_dep_score",
               "p_age", "p_mar_status", 
               "p_mar_status_ans", "p_relig", "p_relig_name",
               "p_sep", "p_sep_ans", "p_edu_level", "p_edu_level_name", "p_eval_hlth", 
               "p_eval_hlth_ans", "p_soc_supp", "p_weigh_life", "p_anx_score", "p_dep_score")


df_working <- df_working[, col_order]


#-------------------------------------------------------------------------------
# creating a correlation matrix to see if the exposures can be turned into one 
# summary score

df_cor_m <- df_working %>%
  select(c("m_conf_nhs", "m_ill_quick", "m_alwys_help")) %>% 
  drop_na()

cor_matrix_m <- cor(df_cor_m)


corrplot(cor_matrix_m, method="number", type = "lower")

# the correlation matrices suggest that these variables aren't correlated enough 
# to create a composite score 
col_corrplot<- c("#9d0053", "#c8257b", "#da4493", "#e164a6",  "#f584c0","#fcb1d9","#fcd7d9", "#fce6d9", "#fce6d9", "#f8ffaf", 
                 "#e5f25c", "#dae83d", "#ccda28", "#bccb0b", "#aab809", "#899316", "#737a22", "#5b6110", "#43470c", "#24260f")

df_cor_m <- df_working %>%
  select(c("m_conf_nhs", "m_ill_quick", "m_alwys_help")) %>% 
  drop_na()

cor_matrix_m <- cor(df_cor_m)

cor_m <- corrplot(cor_matrix_m, method = "number", type = "full", col = rev(col_corrplot))

cor_labels <- c("No confidence", "Doctor would come quickly", "Doctor is always helpful")


breaks <- c(-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

par(mar = c(6, 6, 8, 2), oma = c(4, 0, 4, 0))

corrplot(
  cor_matrix_m,
  method = "circle",
  type = "full",
  col = rev(col_corrplot),
  breaks = breaks,
  tl.col = "#D35400",
  tl.labels = cor_labels,
  tl.srt = 45
)

mtext("Correlation matrix of Doctor characteristics", 
      side = 3,
      line = 0.5, 
      outer = TRUE,
      col.main = "#D35400",
      cex.main = 2,
      font.main = 1.5)

mtext(
  "Variables: supp = supportive, symp = sympathetic, int = interested, help = helpful, easy = easy to talk to, time = willing to give time", 
  side = 1, 
  line = 0.5, 
  outer = TRUE,
  col = "#D35400", 
  cex = 0.8, 
  font = 1
)

df_cor_p <- df_working %>%
  select(c("p_conf_nhs", "p_ill_quick", "p_alwys_help")) %>% 
  drop_na()

cor_matrix_p <- cor(df_cor_p)

cor_p <- corrplot(cor_matrix_p, method = "number", type = "full", col = rev(col_corrplot))

cor_labels <- c("No confidence", "Doctor would come quickly", "Doctor is always helpful")

col_corrplot <- c("#9d0053", "#c8257b", "#da4493", "#e164a6",  "#f584c0","#fcb1d9","#fcd7d9", "#fce6d9", "#fce6d9", "#f8ffaf", 
                  "#e5f25c", "#dae83d", "#ccda28", "#bccb0b", "#aab809", "#899316", "#737a22", "#5b6110", "#43470c", "#24260f")

breaks <- c(-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

par(mar = c(6, 6, 8, 2), oma = c(4, 0, 4, 0))

corrplot(
  cor_matrix_p,
  method = "circle",
  type = "full",
  col = rev(col_corrplot),
  breaks = breaks,
  tl.col = "#D35400",
  tl.labels = cor_labels,
  tl.srt = 45
)

#-------------------------------------------------------------------------------
# adding labels to columns
df_working <- df_working %>%
  set_variable_labels(
    m_home_own_status = "Home ownership status",
    m_home_own_status_ans = "Home ownership status",
    m_edu_level = "Highest educational attainment",
    m_edu_level_name = "Highest educational attainment",
    m_eval_hlth = "Evaluation of own health",
    m_eval_hlth_ans = "Evaluation of own health",
    m_soc_supp = "Social support socre",
    m_weigh_life = "Weighted life event score",
    m_dr_change = "Recent doctor change",
    m_dr_changed_ever = "Recent doctor change",
    m_mar_status = "Marital status",
    m_mar_status_ans = "Marital status",
    m_relig = "Religion",
    m_relig_name = "Religion",
    p_edu_level = "Highest educational attainment",
    p_edu_level_name = "Highest educational attainment",
    p_eval_hlth = "Evaluation of own health",
    p_eval_hlth_ans = "Evaluation of own health",
    p_soc_supp = "Social support socre",
    p_weigh_life = "Weighted life event score",
    p_mar_status = "Marital status",
    p_mar_status_ans = "Marital status",
    p_relig = "Religion",
    p_relig_name = "Religion",
    m_conf_nhs = "I do not have any confidence in the NHS",
    m_conf_nhs_ans = "I do not have any confidence in the NHS",
    p_conf_nhs = "I do not have any confidence in the NHS",
    p_conf_nhs_ans = "I do not have any confidence in the NHS",
    m_ill_quick = "I know if my child was ill, the doctor would come quickly",
    m_ill_quick_ans = "I know if my child was ill, the doctor would come quickly",
    p_ill_quick = "I know if my child was ill, the doctor would come quickly",
    p_ill_quick_ans = "I know if my child was ill, the doctor would come quickly",
    m_alwys_help = "The doctor in the clinic is always helpful",
    m_alwys_help_ans = "The doctor in the clinic is always helpful",
    p_alwys_help = "The doctor in the clinic is always helpful",
    p_alwys_help_ans = "The doctor in the clinic is always helpful",
    m_anx = "Have you had/continued to have anxiety/nerves since study child's 5th birthday",
    m_anx_ans = "Have you had/continued to have anxiety/nerves since study child's 5th birthday",
    p_anx = "Have you had/continued to have anxiety/nerves since study child's 5th birthday",
    p_anx_ans = "Have you had/continued to have anxiety/nerves since study child's 5th birthday",
    m_anx_score = "Crown-crisp anxiety score",
    p_anx_score = "Crown-crisp anxiety score",
    m_dep_score = "Crown-crisp depression score",
    p_dep_score = "Crown-crisp depression score",
    m_sep_ans = "Socioeconomic position",
    p_sep_ans = "Socioeconomic position"
  )



# categorising continuous outcomes to get them ready for contingency tables 
df_working$m_dr_changed_ever <- factor(df_working$m_dr_changed_ever,
                                       levels = c("Recent Dr change", "No recent Dr change"))

df_working$m_sum_age <- cut(df_working$m_age,
                            breaks = c(15, 20, 25, 30, 35, 40, 46, 50),
                            labels = c("15-20", "21-25", "26-30", "31-35", "36-40", "41-46", "47-50"))

df_working$p_sum_age <- cut(df_working$p_age,
                            breaks = c(15, 20, 25, 30, 35, 40, 46, 51, 56, 61, 80),
                            labels = c("15-20", "21-25", "26-30", "31-35", "36-40", "41-46", "47-51", "52-56", "57-61", "61+"))

df_working$m_sum_soc_supp <- cut(df_working$m_soc_supp,
                                 breaks = c(0, 4, 11, 18, 25, 30),
                                 labels = c("0-4", "5-11", "12-18", "19-25", "26-30"))

df_working$p_sum_soc_supp <- cut(df_working$p_soc_supp,
                                 breaks = c(0, 4, 11, 18, 25, 30),
                                 labels = c("0-4", "5-11", "12-18", "19-25", "26-30"))

df_working$m_sum_weigh_life <- cut(df_working$m_weigh_life,
                                   breaks = c(0, 9, 19, 29, 38),
                                   labels = c("0-9", "10-19", "20-29", "30-38"))

df_working$p_sum_weigh_life <- cut(df_working$p_weigh_life,
                                   breaks = c(0, 9, 19, 29, 38),
                                   labels = c("0-9", "10-19", "20-29", "30-38"))

df_working$m_sum_anx_score <- cut(df_working$m_anx_score,
                                  breaks = c(0, 4, 8, 12, 16),
                                  labels = c("0-4", "5-8", "9-12", "12-16"))

df_working$p_sum_anx_score <- cut(df_working$p_anx_score,
                                  breaks = c(0, 4, 8, 12, 16),
                                  labels = c("0-4", "5-8", "9-12", "12-16"))

df_working$m_sum_dep_score <- cut(df_working$m_dep_score,
                                  breaks = c(0, 2, 6, 10, 14),
                                  labels = c("0-2", "3-6", "7-10", "11-14"))

df_working$p_sum_dep_score <- cut(df_working$p_dep_score,
                                  breaks = c(0, 2, 6, 10, 14),
                                  labels = c("0-2", "3-6", "7-10", "11-14"))



# have to first order the outcomes so that it produces a better looking contingency tables

#exposures
# mother
df_working$m_conf_nhs_ans <- factor(
  df_working$m_conf_nhs_ans, 
  levels = c("Never", "Sometimes", "Often", "Exactly")
)

df_working$m_ill_quick_ans <- factor(
  df_working$m_ill_quick_ans, 
  levels = c("Exactly", "Often", "Sometimes", "Never")
)

df_working$m_alwys_help_ans <- factor(
  df_working$m_alwys_help_ans, 
  levels = c("Exactly", "Often", "Sometimes", "Never")
)

# partner
df_working$p_conf_nhs_ans <- factor(
  df_working$p_conf_nhs_ans, 
  levels = c("Never", "Sometimes", "Often", "Exactly")
)

df_working$p_ill_quick_ans <- factor(
  df_working$p_ill_quick_ans, 
  levels = c("Exactly", "Often", "Sometimes", "Never")
)

df_working$p_alwys_help_ans <- factor(
  df_working$p_alwys_help_ans, 
  levels = c("Exactly", "Often", "Sometimes", "Never")
)

# covariates
df_working$m_home_own_status_ans <- factor(
  df_working$m_home_own_status_ans,
  levels = c("Other", "Publicly rented", "Privately rented", "Mortgaged", "Owned")
)

df_working$m_edu_level_name <- factor(
  df_working$m_edu_level_name,
  levels = c("O-level, Vocational, CSE, GCSE or none", "A-level", "Degree")
)

df_working$m_eval_hlth_ans <- factor(
  df_working$m_eval_hlth_ans,
  levels = c("Fit and well", "Mostly well", "Often unwell", "Never well")
)

df_working$m_mar_status_ans <- factor(
  df_working$m_mar_status_ans,
  levels = c("Not currently married", "Married")
)

df_working$m_relig_name <- factor(
  df_working$m_relig_name,
  levels = c("None", "Christian", "Non-Christian")
)

df_working$p_edu_level_name <- factor(
  df_working$p_edu_level_name,
  levels = c("O-level, Vocational, CSE, GCSE or none", "A-level", "Degree")
)


df_working$p_eval_hlth_ans <- factor(
  df_working$p_eval_hlth_ans,
  levels = c("Never well", "Often unwell", "Mostly well", "Fit and well")
)

df_working$p_mar_status_ans <- factor(
  df_working$p_mar_status_ans,
  levels = c("Not currently married", "Married")
)

df_working$p_relig_name <- factor(
  df_working$p_relig_name,
  levels = c("None", "Christian", "Non-Christian")
)


write.csv(df_working, here::here("6-year-timepoint/complete-case-dataset/2_Import_and_Clean", "df_working_6y.csv"))
