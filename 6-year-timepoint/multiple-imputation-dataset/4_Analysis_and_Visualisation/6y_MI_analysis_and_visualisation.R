# Script: data analysis and visualisation for multiple imputation dataset at 6 year timepoint
#-------------------------------------------------------------------------------
source(here::here("6-year-timepoint/multiple-imputation-dataset/1_Packages", "21m_packages.R"))

df_working <- read.csv(here::here("6-year-timepoint/multiple-imputation-dataset/3_Descriptives", "df_working_MI.csv" ))


#-------------------------------------------------------------------------------
# recoding to get data ready for logistic regression models - only including 
# 0 = people who experienced a condition and did not go to the dr
# 1 = people who experienced a condition and did go to the dr
# outcomes
# anxiety
df_working$m_anx <- as.numeric(as.character(df_working$m_anx))

df_working$m_anx <- dplyr::recode(df_working$m_anx,
                                  `1` = 1,
                                  `0` = 0,
                                  `2` = NA_real_)

# depression
df_working$m_dep <- as.numeric(as.character(df_working$m_dep))

df_working$m_dep <- dplyr::recode(df_working$m_dep,
                                  `1` = 1,
                                  `0` = 0,
                                  `2` = NA_real_)

#partner anxiety
df_working$p_anx <- as.numeric(as.character(df_working$p_anx))

df_working$p_anx <- dplyr::recode(df_working$p_anx,
                                  `1` = 1,
                                  `0` = 0,
                                  `2` = NA_real_)
# depression
df_working$p_dep <- as.numeric(as.character(df_working$p_dep))

df_working$p_dep <- dplyr::recode(df_working$p_dep,
                                  `1` = 1,
                                  `0` = 0,
                                  `2` = NA_real_)

#changing parity so it is numeric and not a factor
df_working$m_parity <- as.numeric(as.character(df_working$m_parity))

# mother datasets
df_m_ill_quick_anx  <- df_working %>% filter (!is.na(m_anx))
df_m_conf_nhs_anx   <- df_working %>% filter (!is.na(m_anx))
df_m_alwys_help_anx <- df_working %>% filter (!is.na(m_anx))
df_m_ill_quick_dep  <- df_working %>% filter (!is.na(m_dep))
df_m_conf_nhs_dep   <- df_working %>% filter (!is.na(m_dep))
df_m_alwys_help_dep <- df_working %>% filter (!is.na(m_dep))

# partner datasets
df_p_ill_quick_anx <- df_working %>%  filter (!is.na(p_anx))
df_p_conf_nhs_anx <- df_working %>%   filter (!is.na(p_anx))
df_p_alwys_help_anx <- df_working %>% filter (!is.na(p_anx))
df_p_ill_quick_dep <- df_working %>%  filter (!is.na(p_dep))
df_p_conf_nhs_dep <- df_working %>%   filter (!is.na(p_dep))
df_p_alwys_help_dep <- df_working %>% filter (!is.na(p_dep))

#-------------------------------------------------------------------------------
# Missingness exploration

  
# m anx ill quick
missing_table_m_anx_ill <- data.frame(
  Variable = names(df_m_ill_quick_anx[c(
    "m_ill_quick", "m_age", "m_mar_status",
    "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
    "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", 
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_anx", "c_anx", "e_anx", "f_anx",  # anxiety scores
    "b_dep", "c_dep", "e_dep", "f_dep", # depression scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )]),
  Missing_Percent_m_anx_ill = colMeans(is.na(df_m_ill_quick_anx[c(
    "m_ill_quick", "m_age", "m_mar_status",
    "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
    "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_anx", "c_anx", "e_anx", "f_anx",  # anxiety scores
    "b_dep", "c_dep", "e_dep", "f_dep", # depression scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )])) * 100
)


missing_table_m_anx_ill <- missing_table_m_anx_ill[order(-missing_table_m_anx_ill$Missing_Percent_m_anx_ill), ]

View(missing_table_m_anx_ill)

# m dep ill quick
missing_table_m_dep_ill <- data.frame(
  Variable = names(df_m_ill_quick_dep[c(
    "m_ill_quick", "m_age", "m_mar_status",
    "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
    "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_dep_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_anx", "c_anx", "e_anx", "f_anx",  # anxiety scores
    "b_dep", "c_dep", "e_dep", "f_dep", # depression scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )]),
  Missing_Percent_m_dep_ill = colMeans(is.na(df_m_ill_quick_dep[c(
    "m_ill_quick", "m_age", "m_mar_status",
    "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
    "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_dep_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_anx", "c_anx", "e_anx", "f_anx",  # anxiety scores
    "b_dep", "c_dep", "e_dep", "f_dep", # depression scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )])) * 100
)


missing_table_m_dep_ill <- missing_table_m_dep_ill[order(-missing_table_m_dep_ill$Missing_Percent_m_dep_ill), ]

View(missing_table_m_dep_ill)


# p anx ill quick
missing_table_p_anx_ill <- data.frame(
  Variable = names(df_p_ill_quick_anx[c(
    "p_ill_quick", "p_age", "p_mar_status",
    "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
    "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score",
    "pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age", #partner age
    "pf_mar", "pg_mar", # marital status
    "ph_relig", # religion
    "pb_soc_class", # social class
    "pb_uni", "ph_uni", # uni degree
    "pg_ptner_hlth", #eval of own health
    "pb_soc_supp", "pc_soc_supp", #social support
    "pb_weigh_life", "pc_weigh_life", # weighted life events
    "pb_anx", "pc_anx", "pe_anx", # anx scores
    "pb_dep", "pc_dep", "pe_dep" #dep scores
  )]),
  Missing_Percent_p_anx_ill = colMeans(is.na(df_p_ill_quick_anx[c(
    "p_ill_quick", "p_age", "p_mar_status",
    "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
    "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score",
    "pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age", #partner age
    "pf_mar", "pg_mar", # marital status
    "ph_relig", # religion
    "pb_soc_class", # social class
    "pb_uni", "ph_uni", # uni degree
    "pg_ptner_hlth", #eval of own health
    "pb_soc_supp", "pc_soc_supp", #social support
    "pb_weigh_life", "pc_weigh_life", # weighted life events
    "pb_anx", "pc_anx", "pe_anx", # anx scores
    "pb_dep", "pc_dep", "pe_dep" #dep scores
  )])) * 100
)


missing_table_p_anx_ill <- missing_table_p_anx_ill[order(-missing_table_p_anx_ill$Missing_Percent_p_anx_ill), ]

View(missing_table_p_anx_ill)

# p dep ill quick
missing_table_p_dep_ill <- data.frame(
  Variable = names(df_p_ill_quick_dep[c(
    "p_ill_quick", "p_age", "p_mar_status",
    "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
    "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_dep_score",
    "pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age", #partner age
    "pf_mar", "pg_mar", # marital status
    "ph_relig", # religion
    "pb_soc_class", # social class
    "pb_uni", "ph_uni", # uni degree
    "pg_ptner_hlth", #eval of own health
    "pb_soc_supp", "pc_soc_supp", #social support
    "pb_weigh_life", "pc_weigh_life", # weighted life events
    "pb_anx", "pc_anx", "pe_anx", # anx scores
    "pb_dep", "pc_dep", "pe_dep" #dep scores
  )]),
  Missing_Percent_p_dep_ill = colMeans(is.na(df_p_ill_quick_dep[c(
    "p_ill_quick", "p_age", "p_mar_status",
    "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
    "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_dep_score",
    "pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age", #partner age
    "pf_mar", "pg_mar", # marital status
    "ph_relig", # religion
    "pb_soc_class", # social class
    "pb_uni", "ph_uni", # uni degree
    "pg_ptner_hlth", #eval of own health
    "pb_soc_supp", "pc_soc_supp", #social support
    "pb_weigh_life", "pc_weigh_life", # weighted life events
    "pb_anx", "pc_anx", "pe_anx", # anx scores
    "pb_dep", "pc_dep", "pe_dep" #dep scores
  )])) * 100
)


missing_table_p_dep_ill <- missing_table_p_dep_ill[order(-missing_table_p_dep_ill$Missing_Percent_p_dep_ill), ]

View(missing_table_p_dep_ill)

# m anx conf 
missing_table_m_anx_conf <- data.frame(
  Variable = names(df_m_conf_nhs_anx[c(
    "m_conf_nhs", "m_age", "m_mar_status",
    "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
    "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_anx", "c_anx", "e_anx", "f_anx",  # anxiety scores
    "b_dep", "c_dep", "e_dep", "f_dep", # depression scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )]),
  Missing_Percent_m_anx_conf = colMeans(is.na(df_m_conf_nhs_anx[c(
    "m_conf_nhs", "m_age", "m_mar_status",
    "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
    "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_anx", "c_anx", "e_anx", "f_anx",  # anxiety scores
    "b_dep", "c_dep", "e_dep", "f_dep", # depression scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )])) * 100
)


missing_table_m_anx_conf <- missing_table_m_anx_conf[order(-missing_table_m_anx_conf$Missing_Percent_m_anx_conf), ]

View(missing_table_m_anx_conf)

# m dep conf
missing_table_m_dep_conf <- data.frame(
  Variable = names(df_m_conf_nhs_dep[c(
    "m_conf_nhs", "m_age", "m_mar_status",
    "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
    "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_dep_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_anx", "c_anx", "e_anx", "f_anx",  # anxiety scores
    "b_dep", "c_dep", "e_dep", "f_dep", # depression scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )]),
  Missing_Percent_m_dep_conf = colMeans(is.na(df_m_conf_nhs_dep[c(
    "m_conf_nhs", "m_age", "m_mar_status",
    "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
    "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_dep_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_anx", "c_anx", "e_anx", "f_anx",  # anxiety scores
    "b_dep", "c_dep", "e_dep", "f_dep", # depression scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )])) * 100
)


missing_table_m_dep_conf <- missing_table_m_dep_conf[order(-missing_table_m_dep_conf$Missing_Percent_m_dep_conf), ]

View(missing_table_m_dep_conf)


# p anx conf
missing_table_p_anx_conf <- data.frame(
  Variable = names(df_p_conf_nhs_anx[c(
    "p_conf_nhs", "p_age", "p_mar_status",
    "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
    "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "m_anx_score",
    "pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age", #partner age
    "pf_mar", "pg_mar", # marital status
    "ph_relig", # religion
    "pb_soc_class", # social class
    "pb_uni", "ph_uni", # uni degree
    "pg_ptner_hlth", #eval of own health
    "pb_soc_supp", "pc_soc_supp", #social support
    "pb_weigh_life", "pc_weigh_life", # weighted life events
    "pb_anx", "pc_anx", "pe_anx", # anx scores
    "pb_dep", "pc_dep", "pe_dep" #dep scores
  )]),
  Missing_Percent_p_anx_conf = colMeans(is.na(df_p_conf_nhs_anx[c(
    "p_conf_nhs", "p_age", "p_mar_status",
    "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
    "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "m_anx_score",
    "pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age", #partner age
    "pf_mar", "pg_mar", # marital status
    "ph_relig", # religion
    "pb_soc_class", # social class
    "pb_uni", "ph_uni", # uni degree
    "pg_ptner_hlth", #eval of own health
    "pb_soc_supp", "pc_soc_supp", #social support
    "pb_weigh_life", "pc_weigh_life", # weighted life events
    "pb_anx", "pc_anx", "pe_anx", # anx scores
    "pb_dep", "pc_dep", "pe_dep" #dep scores
  )])) * 100
)


missing_table_p_anx_conf <- missing_table_p_anx_conf[order(-missing_table_p_anx_conf$Missing_Percent_p_anx_conf), ]

View(missing_table_p_anx_conf)

# p dep conf
missing_table_p_dep_conf <- data.frame(
  Variable = names(df_p_conf_nhs_dep[c(
    "p_conf_nhs", "p_age", "p_mar_status",
    "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
    "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score",
    "pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age", #partner age
    "pf_mar", "pg_mar", # marital status
    "ph_relig", # religion
    "pb_soc_class", # social class
    "pb_uni", "ph_uni", # uni degree
    "pg_ptner_hlth", #eval of own health
    "pb_soc_supp", "pc_soc_supp", #social support
    "pb_weigh_life", "pc_weigh_life", # weighted life events
    "pb_anx", "pc_anx", "pe_anx", # anx scores
    "pb_dep", "pc_dep", "pe_dep" #dep scores
  )]),
  Missing_Percent_p_dep_conf = colMeans(is.na(df_p_conf_nhs_dep[c(
    "p_conf_nhs", "p_age", "p_mar_status",
    "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
    "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score",
    "pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age", #partner age
    "pf_mar", "pg_mar", # marital status
    "ph_relig", # religion
    "pb_soc_class", # social class
    "pb_uni", "ph_uni", # uni degree
    "pg_ptner_hlth", #eval of own health
    "pb_soc_supp", "pc_soc_supp", #social support
    "pb_weigh_life", "pc_weigh_life", # weighted life events
    "pb_anx", "pc_anx", "pe_anx", # anx scores
    "pb_dep", "pc_dep", "pe_dep" #dep scores
  )])) * 100
)


missing_table_p_dep_conf <- missing_table_p_dep_conf[order(-missing_table_p_dep_conf$Missing_Percent_p_dep_conf), ]

View(missing_table_p_dep_conf)

# m anx alwys help 
missing_table_m_anx_help <- data.frame(
  Variable = names(df_m_alwys_help_anx[c(
    "m_alwys_help", "m_age", "m_mar_status",
    "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
    "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_anx", "c_anx", "e_anx", "f_anx",  # anxiety scores
    "b_dep", "c_dep", "e_dep", "f_dep", # depression scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )]),
  Missing_Percent_m_anx_help = colMeans(is.na(df_m_alwys_help_anx[c(
    "m_alwys_help", "m_age", "m_mar_status",
    "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
    "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_anx", "c_anx", "e_anx", "f_anx",  # anxiety scores
    "b_dep", "c_dep", "e_dep", "f_dep", # depression scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )])) * 100
)


missing_table_m_anx_help <- missing_table_m_anx_help[order(-missing_table_m_anx_help$Missing_Percent_m_anx_help), ]

View(missing_table_m_anx_help)

# m dep conf
missing_table_m_dep_help <- data.frame(
  Variable = names(df_m_alwys_help_dep[c(
    "m_alwys_help", "m_age", "m_mar_status",
    "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
    "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_dep_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_anx", "c_anx", "e_anx", "f_anx",  # anxiety scores
    "b_dep", "c_dep", "e_dep", "f_dep", # depression scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )]),
  Missing_Percent_m_dep_help = colMeans(is.na(df_m_alwys_help_dep[c(
    "m_alwys_help", "m_age", "m_mar_status",
    "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
    "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_dep_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_anx", "c_anx", "e_anx", "f_anx",  # anxiety scores
    "b_dep", "c_dep", "e_dep", "f_dep", # depression scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )])) * 100
)


missing_table_m_dep_help <- missing_table_m_dep_help[order(-missing_table_m_dep_help$Missing_Percent_m_dep_help), ]

View(missing_table_m_dep_help)


# p anx conf
missing_table_p_anx_help <- data.frame(
  Variable = names(df_p_alwys_help_anx[c(
    "p_alwys_help", "p_age", "p_mar_status",
    "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
    "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score",
    "pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age", #partner age
    "pf_mar", "pg_mar", # marital status
    "ph_relig", # religion
    "pb_soc_class", # social class
    "pb_uni", "ph_uni", # uni degree
    "pg_ptner_hlth", #eval of own health
    "pb_soc_supp", "pc_soc_supp", #social support
    "pb_weigh_life", "pc_weigh_life", # weighted life events
    "pb_anx", "pc_anx", "pe_anx", # anx scores
    "pb_dep", "pc_dep", "pe_dep" #dep scores
  )]),
  Missing_Percent_p_anx_conf = colMeans(is.na(df_p_alwys_help_anx[c(
    "p_alwys_help", "p_age", "p_mar_status",
    "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
    "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score",
    "pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age", #partner age
    "pf_mar", "pg_mar", # marital status
    "ph_relig", # religion
    "pb_soc_class", # social class
    "pb_uni", "ph_uni", # uni degree
    "pg_ptner_hlth", #eval of own health
    "pb_soc_supp", "pc_soc_supp", #social support
    "pb_weigh_life", "pc_weigh_life", # weighted life events
    "pb_anx", "pc_anx", "pe_anx", # anx scores
    "pb_dep", "pc_dep", "pe_dep" #dep scores
  )])) * 100
)


missing_table_p_anx_conf <- missing_table_p_anx_conf[order(-missing_table_p_anx_conf$Missing_Percent_p_anx_conf), ]

View(missing_table_p_anx_conf)

# p anx help
missing_table_p_anx_help <- data.frame(
  Variable = names(df_p_alwys_help_anx[c(
    "p_alwys_help", "p_age", "p_mar_status",
    "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
    "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "m_anx_score",
    "pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age", #partner age
    "pf_mar", "pg_mar", # marital status
    "ph_relig", # religion
    "pb_soc_class", # social class
    "pb_uni", "ph_uni", # uni degree
    "pg_ptner_hlth", #eval of own health
    "pb_soc_supp", "pc_soc_supp", #social support
    "pb_weigh_life", "pc_weigh_life", # weighted life events
    "pb_anx", "pc_anx", "pe_anx", # anx scores
    "pb_dep", "pc_dep", "pe_dep" #dep scores
  )]),
  Missing_Percent_p_anx_help = colMeans(is.na(df_p_alwys_help_anx[c(
    "p_alwys_help", "p_age", "p_mar_status",
    "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
    "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep" , "m_anx_score",
    "pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age", #partner age
    "pf_mar", "pg_mar", # marital status
    "ph_relig", # religion
    "pb_soc_class", # social class
    "pb_uni", "ph_uni", # uni degree
    "pg_ptner_hlth", #eval of own health
    "pb_soc_supp", "pc_soc_supp", #social support
    "pb_weigh_life", "pc_weigh_life", # weighted life events
    "pb_anx", "pc_anx", "pe_anx", # anx scores
    "pb_dep", "pc_dep", "pe_dep" #dep scores
  )])) * 100
)


missing_table_p_anx_help <- missing_table_p_anx_help[order(-missing_table_p_anx_help$Missing_Percent_p_anx_help), ]

View(missing_table_p_anx_help)

# p dep help
missing_table_p_dep_help <- data.frame(
  Variable = names(df_p_alwys_help_dep[c(
    "p_alwys_help", "p_age", "p_mar_status",
    "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
    "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "m_dep_score",
    "pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age", #partner age
    "pf_mar", "pg_mar", # marital status
    "ph_relig", # religion
    "pb_soc_class", # social class
    "pb_uni", "ph_uni", # uni degree
    "pg_ptner_hlth", #eval of own health
    "pb_soc_supp", "pc_soc_supp", #social support
    "pb_weigh_life", "pc_weigh_life", # weighted life events
    "pb_anx", "pc_anx", "pe_anx", # anx scores
    "pb_dep", "pc_dep", "pe_dep" #dep scores
  )]),
  Missing_Percent_p_dep_help = colMeans(is.na(df_p_alwys_help_dep[c(
    "p_alwys_help", "p_age", "p_mar_status",
    "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
    "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep" , "m_dep_score",
    "pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age", #partner age
    "pf_mar", "pg_mar", # marital status
    "ph_relig", # religion
    "pb_soc_class", # social class
    "pb_uni", "ph_uni", # uni degree
    "pg_ptner_hlth", #eval of own health
    "pb_soc_supp", "pc_soc_supp", #social support
    "pb_weigh_life", "pc_weigh_life", # weighted life events
    "pb_anx", "pc_anx", "pe_anx", # anx scores
    "pb_dep", "pc_dep", "pe_dep" #dep scores
  )])) * 100
)


missing_table_p_dep_help <- missing_table_p_dep_help[order(-missing_table_p_dep_help$Missing_Percent_p_dep_help), ]

View(missing_table_p_dep_help)


 


#-------------------------------------------------------------------------------
# Checking correlatedness
  
df_m_ill_quick_anx$m_parity <- as.numeric(as.character(df_m_ill_quick_anx$m_parity))
df_m_conf_nhs_anx$m_parity <- as.numeric(as.character(df_m_conf_nhs_anx$m_parity))  
df_m_alwys_help_anx$m_parity <- as.numeric(as.character(df_m_alwys_help_anx$m_parity))
df_m_ill_quick_dep$m_parity <- as.numeric(as.character(df_m_ill_quick_dep$m_parity)) 
df_m_conf_nhs_dep$m_parity <- as.numeric(as.character(df_m_conf_nhs_dep$m_parity))  
df_m_alwys_help_dep$m_parity <- as.numeric(as.character(df_m_alwys_help_dep$m_parity))
df_p_ill_quick_anx$m_parity <- as.numeric(as.character(df_p_ill_quick_anx$m_parity)) 
df_p_conf_nhs_anx$m_parity <- as.numeric(as.character(df_p_conf_nhs_anx$m_parity))  
df_p_alwys_help_anx$m_parity <- as.numeric(as.character(df_p_alwys_help_anx$m_parity))
df_p_ill_quick_dep$m_parity <- as.numeric(as.character(df_p_ill_quick_dep$m_parity)) 
df_p_conf_nhs_dep$m_parity <- as.numeric(as.character(df_p_conf_nhs_dep$m_parity))  
df_p_alwys_help_dep$m_parity <- as.numeric(as.character(df_p_alwys_help_dep$m_parity))


datasets_m <- list(
  m_ill_quick_anx   = df_m_ill_quick_anx,
  m_conf_nhs_anx    = df_m_conf_nhs_anx,
  m_alwys_help_anx =  df_m_alwys_help_anx,
  m_ill_quick_dep   = df_m_ill_quick_dep,
  m_conf_nhs_dep    = df_m_conf_nhs_dep,
  m_alwys_help_dep  = df_m_alwys_help_dep
)

datasets_p <- list(
  p_ill_quick_anx   = df_p_ill_quick_anx,
  p_conf_nhs_anx    = df_p_conf_nhs_anx,
  p_alwys_help_anx =  df_p_alwys_help_anx,
  p_ill_quick_dep   = df_p_ill_quick_dep,
  p_conf_nhs_dep    = df_p_conf_nhs_dep,
  p_alwys_help_dep  = df_p_alwys_help_dep
)

datasets_all <- list(
  m_ill_quick_anx   = df_m_ill_quick_anx ,
  m_conf_nhs_anx    = df_m_conf_nhs_anx  ,
  m_alwys_help_anx  = df_m_alwys_help_anx,
  m_ill_quick_dep   = df_m_ill_quick_dep ,
  m_conf_nhs_dep    = df_m_conf_nhs_dep  ,
  m_alwys_help_dep  = df_m_alwys_help_dep,
  p_ill_quick_anx   = df_p_ill_quick_anx ,
  p_conf_nhs_anx    = df_p_conf_nhs_anx  ,
  p_alwys_help_anx  = df_p_alwys_help_anx,
  p_ill_quick_dep   = df_p_ill_quick_dep ,
  p_conf_nhs_dep    = df_p_conf_nhs_dep  ,
  p_alwys_help_dep  = df_p_alwys_help_dep
)


# functions
##chi squared
run_chi_sq <- function(df, vars, outcome) {
  data.frame(
    variable = vars,
    chi_sq = sapply(vars, function(v) {
      tab <- table(df[[v]], df[[outcome]])
      chisq.test(tab)$statistic
    }),
    p_value = sapply(vars, function(v) {
      tab <- table(df[[v]], df[[outcome]])
      chisq.test(tab)$p.value
    })
  )
}

## correlation matrix
run_cor <- function(df, vars, outcome) {
  data.frame(
    variable = vars,
    correlation = sapply(vars, function(v) {
      cor(df[[v]], df[[outcome]], use = "complete.obs")
    })
  )
}


# home ownership
vars_home_m <- c("f_home_own_status", "a_car_own")

chi_results <- lapply(datasets_m, run_chi_sq,
                      vars = vars_home_m,
                      outcome = "m_home_own_status")

chi_results

#marital status
vars_mar_m <- "g_mar_status"

cor_mar_status_m <- lapply(
  datasets_m,
  run_cor,
  vars = vars_mar_m,
  outcome = "m_mar_status"
)

cor_mar_status_m

vars_mar_p <- c("pf_mar", "pg_mar")

cor_mar_status_p <- lapply(
  datasets_p,
  run_cor,
  vars = vars_mar_p,
  outcome = "p_mar_status"
)

cor_mar_status_p

#educational attainment
vars_edu_m <- "k_uni"

cor_edu_m <- lapply(
  datasets_m,
  run_cor,
  vars = vars_edu_m,
  outcome = "m_edu_level"
)

cor_edu_m

vars_edu_p <- c("pb_uni", "ph_uni")

cor_edu_p <- lapply(
  datasets_p,
  run_cor,
  vars = vars_edu_p,
  outcome = "p_edu_level"
)

cor_edu_p

#socioeconomic position
vars_sep_m <- c("g_soc_class", "c_soc_class", "b_soc_class")

cor_sep_m <- lapply(
  datasets_m,
  run_cor,
  vars = vars_sep_m,
  outcome = "m_sep"
)

cor_sep_m

vars_sep_p <- "pb_soc_class"

cor_sep_p <- lapply(
  datasets_p,
  run_cor,
  vars = vars_sep_p,
  outcome = "p_sep"
)

cor_sep_p

#anxiety
vars_anx_m <- c("b_anx", "c_anx", "e_anx", "f_anx")

cor_anx_m <- lapply(
  datasets_m,
  run_cor,
  vars = vars_anx_m,
  outcome = "m_anx_score"
)

cor_anx_m

vars_anx_p <- c("pb_anx", "pc_anx", "pe_anx")

cor_anx_p <- lapply(
  datasets_p,
  run_cor,
  vars = vars_anx_p,
  outcome = "p_anx_score"
)

cor_anx_p

# depression
vars_dep_m <- c("b_dep", "c_dep", "e_dep", "f_dep")

cor_dep_m <- lapply(
  datasets_m,
  run_cor,
  vars = vars_dep_m,
  outcome = "m_dep_score"
)

cor_dep_m

vars_dep_p <- c("pb_dep", "pc_dep", "pe_dep")

cor_dep_p <- lapply(
  datasets_p,
  run_cor,
  vars = vars_dep_p,
  outcome = "p_dep_score"
)

cor_dep_p


#evaluation of own health
vars_eval_m <- c("b_hlth", "f_hlth")

cor_eval_m <- lapply(
  datasets_m,
  run_cor,
  vars = vars_eval_m,
  outcome = "m_eval_hlth"
)

cor_eval_m



vars_eval_p <- "pg_ptner_hlth"

cor_eval_p <- lapply(
  datasets_p,
  run_cor,
  vars = vars_eval_p,
  outcome = "p_eval_hlth"
)

cor_eval_p


#social support score
vars_soc_supp_m <- c("d_soc_supp", "e_soc_supp", "f_soc_supp")

cor_soc_m <- lapply(
  datasets_m,
  run_cor,
  vars = vars_soc_supp_m,
  outcome = "m_soc_supp"
)

cor_soc_m

vars_soc_supp_p <- c("pb_soc_supp", "pc_soc_supp")

cor_soc_p <- lapply(
  datasets_p,
  run_cor,
  vars = vars_soc_supp_p,
  outcome = "p_soc_supp"
)

cor_soc_p

#weighted life events score 
vars_weigh_life_m <- c("b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life")

cor_weigh_m <- lapply(
  datasets_m,
  run_cor,
  vars = vars_weigh_life_m,
  outcome = "m_soc_supp"
)

cor_weigh_m

vars_weigh_life_p <- c("pb_weigh_life", "pc_weigh_life")

cor_weigh_p <- lapply(
  datasets_p,
  run_cor,
  vars = vars_weigh_life_p,
  outcome = "p_weigh_life"
)

cor_weigh_p

#recent Dr change
vars_dr_m <- "l_dr"

cor_dr_m <- lapply(
  datasets_m,
  run_cor,
  vars = vars_dr_m,
  outcome = "m_dr_change"
)

cor_dr_m



#religion - chi squared
vars_relig_m <- c("k_relig") 

chi_relig_m <- lapply(datasets_m, run_chi_sq,
                      vars = vars_relig_m,
                      outcome = "m_relig")

chi_relig_m



vars_relig_p <- "ph_relig" 

chi_relig_p <- lapply(datasets_p, run_chi_sq,
                      vars = vars_relig_p,
                      outcome = "p_relig")

chi_relig_p

#parity 
vars_parity_m_p <- c("g_parity")

cor_parity_m_p <- lapply(
  datasets_all,
  run_cor,
  vars = vars_parity_m_p,
  outcome = "m_parity"
)

cor_parity_m_p

str(df_p_conf_nhs_anx$g_parity)

#age

vars_age_m <- c("b_age", "e_age", "f_age")

cor_age_m <- lapply(
  datasets_m,
  run_cor,
  vars = vars_age_m,
  outcome = "m_age"
)

cor_age_m

vars_age_p <- c("pa_age", "pb_age", "pk_age", "pc_age", "pd_age", "pe_age")

cor_parity_p <- lapply(
  datasets_p,
  run_cor,
  vars = vars_age_p,
  outcome = "p_age"
)

cor_parity_p


# attitudes towards drs
vars_attitude <- c("g_no_conf", "g_helpful") 

cor_table_m_ill_anx <- data.frame(
  variable = vars_attitude,
  correlation = sapply(vars_attitude, function(v) cor(df_m_ill_quick_anx[[v]], df_m_ill_quick_anx$m_ill_quick, use = "complete.obs"))
)

cor_table_m_ill_anx

cor_table_m_ill_dep <- data.frame(
  variable = vars_attitude,
  correlation = sapply(vars_attitude, function(v) cor(df_m_ill_quick_dep[[v]], df_m_ill_quick_dep$m_ill_quick, use = "complete.obs"))
)

cor_table_m_ill_dep

cor_table_m_conf_anx <- data.frame(
  variable = vars_attitude,
  correlation = sapply(vars_attitude, function(v) cor(df_m_conf_nhs_anx[[v]], df_m_conf_nhs_anx$m_conf_nhs, use = "complete.obs"))
)

cor_table_m_conf_anx

cor_table_m_conf_dep <- data.frame(
  variable = vars_attitude,
  correlation = sapply(vars_attitude, function(v) cor(df_m_conf_nhs_dep[[v]], df_m_conf_nhs_dep$m_conf_nhs, use = "complete.obs"))
)

cor_table_m_conf_dep

cor_table_m_help_anx <- data.frame(
  variable = vars_attitude,
  correlation = sapply(vars_attitude, function(v) cor(df_m_alwys_help_anx[[v]], df_m_alwys_help_anx$m_alwys_help, use = "complete.obs"))
)

cor_table_m_help_anx

cor_table_m_help_dep <- data.frame(
  variable = vars_attitude,
  correlation = sapply(vars_attitude, function(v) cor(df_m_alwys_help_dep[[v]], df_m_alwys_help_dep$m_alwys_help, use = "complete.obs"))
)

cor_table_m_help_dep



 
#-------------------------------------------------------------------------------
# mice
# mothers  

# creating new dataframes to only include variables of interest

## m ill quick anx
vars_m_anx_ill <- c(
  "m_ill_quick", "m_anx", "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score",
  
  "b_soc_class", "f_age", "e_age", "b_age", "f_soc_supp", "e_soc_supp", "d_soc_supp", "f_anx", "e_anx", "a_car_own", "f_dep", "e_dep" # aux variables
)

df_m_anx_ill_mi <- df_m_ill_quick_anx[vars_m_anx_ill]

## m conf nhs anx
vars_m_anx_conf <- c(
  "m_conf_nhs", "m_anx", "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score",
  
  "b_soc_class", "f_age", "e_age", "b_age", "f_soc_supp", "e_soc_supp", "d_soc_supp", "f_anx", "e_anx", "a_car_own","f_dep", "e_dep" # aux variables
)

df_m_anx_conf_mi <- df_m_conf_nhs_anx[vars_m_anx_conf]

## m alwys help anx
vars_m_anx_help <- c(
  "m_alwys_help", "m_anx", "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score",
  
  "b_soc_class", "f_age", "e_age", "b_age", "f_soc_supp", "e_soc_supp", "d_soc_supp", "f_anx", "e_anx", "a_car_own","f_dep", "e_dep" # aux variables
)

df_m_anx_help_mi <- df_m_alwys_help_anx[vars_m_anx_help]


#depression 
## m ill quick dep
vars_m_dep_ill <- c(
  "m_ill_quick", "m_dep", "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_dep_score", "m_anx_score",
  
  "b_soc_class", "f_age", "e_age", "b_age", "f_soc_supp", "e_soc_supp", "d_soc_supp", "e_dep", "f_dep", "a_car_own", "f_anx", "e_anx"
)

df_m_dep_ill_mi <- df_m_ill_quick_dep[vars_m_dep_ill]

## m conf nhs dep
vars_m_dep_conf <- c(
  "m_conf_nhs", "m_dep", "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_dep_score", "m_anx_score",
  
  "b_soc_class", "f_age", "e_age", "b_age", "f_soc_supp", "e_soc_supp", "d_soc_supp", "e_dep", "f_dep", "a_car_own", "f_anx", "e_anx" # aux variables
)

df_m_dep_conf_mi <- df_m_conf_nhs_dep[vars_m_dep_conf]

## m alwys help dep
vars_m_dep_help <- c(
  "m_alwys_help", "m_dep", "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score",
  
  "b_soc_class", "f_age", "e_age", "b_age", "f_soc_supp", "e_soc_supp", "d_soc_supp", "e_dep", "f_dep", "a_car_own", "f_anx", "e_anx" # aux variables
)

df_m_dep_help_mi <- df_m_alwys_help_dep[vars_m_dep_help]

#partners
## p ill quick anx
vars_p_anx_ill <- c(
  "p_ill_quick", "p_anx", "p_age", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score",
  "m_ill_quick", "m_dep", "m_age", "m_mar_status", # mother variables are being included in partner datasets
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status", # because some mother variables have been used as proxies
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score", # & need imputing
  
  "pg_mar", "pf_mar", "pd_age", "pc_age", "pa_age", "pb_age", "pc_soc_supp", "a_car_own", "pe_anx", "pc_anx", "pb_anx" # aux variables 
)

df_p_anx_ill_mi <- df_p_ill_quick_anx[vars_p_anx_ill]

## p no conf anx
vars_p_anx_conf <- c(
  "p_conf_nhs", "p_anx", "p_age", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score",
  "m_conf_nhs", "m_dep", "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score",
  
  "pg_mar", "pf_mar", "pd_age", "pc_age", "pa_age", "pb_age", "pc_soc_supp", "a_car_own", "pe_anx", "pc_anx", "pb_anx" # aux variables 
)

df_p_anx_conf_mi <- df_p_conf_nhs_anx[vars_p_anx_conf]


## p alwys help anx
vars_p_anx_help <- c(
  "p_alwys_help", "p_anx", "p_age", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score",
  "m_alwys_help", "m_dep", "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score",
  
  "pg_mar", "pf_mar", "pd_age", "pc_age", "pa_age", "pb_age", "pc_soc_supp", "a_car_own", "pe_anx", "pc_anx", "pb_anx" # aux variables 
)

df_p_anx_help_mi <- df_p_alwys_help_anx[vars_p_anx_help]


#depression 
## p no conf dep
vars_p_dep_conf <- c(
  "p_conf_nhs", "p_dep", "p_age", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_dep_score", "p_anx_score",
  "m_conf_nhs", "m_dep", "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score",
  
  "pg_mar", "pf_mar", "pd_age", "pc_age", "pa_age", "pb_age", "pc_soc_supp", "a_car_own", "pe_anx", "pc_anx", "pb_anx" # aux variables 
)

df_p_dep_conf_mi <- df_p_conf_nhs_dep[vars_p_dep_conf]

## p ill quick dep
vars_p_dep_ill <- c(
  "p_ill_quick", "p_dep", "p_age", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_dep_score", "p_anx_score",
  "m_ill_quick", "m_dep", "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score",
  
  "pg_mar", "pf_mar", "pd_age", "pc_age", "pa_age", "pb_age", "pc_soc_supp", "a_car_own", "pe_anx", "pc_anx", "pb_anx" # aux variables 
)

df_p_dep_ill_mi <- df_p_ill_quick_dep[vars_p_dep_ill]

## p alwys help dep
vars_p_dep_help <- c(
  "p_alwys_help", "p_dep", "p_age", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_dep_score", "p_anx_score",
  "m_alwys_help", "m_dep", "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score",
  
  "pg_mar", "pf_mar", "pd_age", "pc_age", "pa_age", "pb_age", "pc_soc_supp", "a_car_own", "pe_anx", "pc_anx", "pb_anx" # aux variables 
)

df_p_dep_help_mi <- df_p_alwys_help_dep[vars_p_dep_help]


 

  
datasets_m_mi <- list(
  df_m_anx_ill_mi,
  df_m_anx_conf_mi,
  df_m_anx_help_mi,
  df_m_dep_ill_mi,
  df_m_dep_conf_mi,
  df_m_dep_help_mi
)


datasets_m_anx_mi <- list(
  df_m_anx_ill_mi,
  df_m_anx_conf_mi,
  df_m_anx_help_mi
)

datasets_m_dep_mi <- list(
  df_m_dep_ill_mi,
  df_m_dep_conf_mi,
  df_m_dep_help_mi
)


# Binary variables
binary_vars <- c("m_anx", "m_dep", "m_mar_status", "m_edu_level", "m_dr_change", "m_sep", "a_car_own")
datasets_m_mi <- lapply(datasets_m_mi, function(df) {
  vars_present <- intersect(binary_vars, names(df))
  df[vars_present] <- lapply(df[vars_present], factor)
  df
})

#continuous variables
numeric_vars <- c("m_age", "m_parity", "m_soc_supp", "m_anx_score", "m_dep_score", "f_age", 
                  "e_age", "b_age", "f_soc_supp", "e_soc_supp", "d_soc_supp", "e_anx", "m_weigh_life", "f_anx", "e_dep", "f_dep")
datasets_m_mi <- lapply(datasets_m_mi, function(df) {
  vars_present_m <- intersect(numeric_vars, names(df))
  df[vars_present_m] <- lapply(df[vars_present_m], function(x) {
    as.numeric(as.character(x))
  })
  df
})

#Categorical variables
factor_vars <- c("m_ill_quick", "m_conf_nhs", "m_alwys_help", "m_relig", "m_home_own_status", "m_eval_hlth", "b_soc_class")
datasets_m_mi <- lapply(datasets_m_mi, function(df) {
  vars_present <- intersect(factor_vars, names(df))
  df[vars_present] <- lapply(df[vars_present], factor)
  df
})


impute_vars <- c(
  "m_anx", "m_mar_status", "m_edu_level", "m_dr_change", "m_sep", "m_age", "m_parity", "m_soc_supp",  
  "m_weigh_life", "m_anx_score", "m_dep_score", "m_ill_quick", "m_conf_nhs", "m_alwys_help", "m_relig",  
  "m_home_own_status", "m_eval_hlth"
)

aux_vars <- c("a_car_own", "f_age", 
              "e_age", "b_age", "f_soc_supp", "e_soc_supp", "d_soc_supp", "e_anx", "f_anx", "e_dep", "f_dep",
              "b_soc_class")

# partners
datasets_p_mi <- list(
  df_p_anx_ill_mi,
  df_p_anx_conf_mi,
  df_p_anx_help_mi,
  df_p_dep_ill_mi,
  df_p_dep_conf_mi,
  df_p_dep_help_mi
)


datasets_p_anx_mi <- list(
  df_p_anx_ill_mi,
  df_p_anx_conf_mi,
  df_p_anx_help_mi
)

datasets_p_dep_mi <- list(
  df_p_dep_ill_mi,
  df_p_dep_conf_mi,
  df_p_dep_help_mi
)

# Binary variables
binary_vars_p <- c("p_anx", "p_dep", "p_mar_status", "p_edu_level", "m_dr_change", "p_sep", "a_car_own", "pg_mar",
                   "pf_mar")
datasets_p_mi <- lapply(datasets_p_mi, function(df) {
  vars_present_p <- intersect(binary_vars_p, names(df))
  df[vars_present_p] <- lapply(df[vars_present_p], factor)
  df
})

#continuous variables
numeric_vars_p <- c("p_age", "m_parity", "p_soc_supp", "p_weigh_life", "p_anx_score", "p_dep_score", "pa_age", 
                    "pb_age", "pc_age", "pd_age", "pc_soc_supp", "pc_anx", "pe_anx")
datasets_p_mi <- lapply(datasets_p_mi, function(df) {
  vars_present_p <- intersect(numeric_vars_p, names(df))
  df[vars_present_p] <- lapply(df[vars_present_p], function(x) {
    as.numeric(as.character(x))
  })
  df
})

#Categorical variables
factor_vars_p <- c("p_ill_quick", "p_conf_nhs", "p_alwys_help", "p_relig", "m_home_own_status", "p_eval_hlth")
datasets_p_mi <- lapply(datasets_p_mi, function(df) {
  vars_present_p <- intersect(factor_vars_p, names(df))
  df[vars_present_p] <- lapply(df[vars_present_p], factor)
  df
})


impute_vars_p <- c(
  "p_anx", "p_dep", "p_mar_status", "p_edu_level", "m_dr_change", "p_sep",
  "p_age", "m_parity", "p_soc_supp", "p_weigh_life", "p_anx_score", "p_dep_score",
  "p_ill_quick", "p_conf_nhs", "p_alwys_help", "p_relig", "m_home_own_status", "p_eval_hlth"
)

aux_vars_p <- c("pg_mar", "pf_mar", "pa_age", "pb_age", "pc_age", "pd_age", "pc_soc_supp", "pc_anx", "pe_anx", "pb_anx")



 


# m anx ill

binary_vars <- c(
  "m_mar_status",
  "m_dr_change",
  "m_sep",
  "a_car_own",
  "b_soc_class"
)

factor_vars <- c(
  "m_ill_quick",
  "m_relig",
  "m_edu_level",
  "m_home_own_status",
  "m_eval_hlth"
)

numeric_vars <- c(
  "m_age",
  "m_parity",
  "m_soc_supp",
  "m_anx_score",
  "m_weigh_life",
  "m_dep_score",
  "f_soc_supp",
  "e_soc_supp",
  "d_soc_supp",
  "f_anx",
  "e_anx",
  "f_dep",
  "e_dep"
)

# outcome (NOT imputed)
outcome_var <- "m_anx"

# 2. remove unused variables
drop_vars <- c("b_age", "e_age", "f_age")

df_m_anx_ill_mi <- df_m_anx_ill_mi[
  , !names(df_m_anx_ill_mi) %in% drop_vars
]

# 3. set variable classes
# binary -> numeric 0/1
df_m_anx_ill_mi[binary_vars] <- lapply(
  df_m_anx_ill_mi[binary_vars],
  function(x) as.numeric(as.character(x))
)

# categorical
df_m_anx_ill_mi[factor_vars] <- lapply(
  df_m_anx_ill_mi[factor_vars],
  factor
)

# continuous
df_m_anx_ill_mi[numeric_vars] <- lapply(
  df_m_anx_ill_mi[numeric_vars],
  as.numeric
)

# test run
ini <- mice(
  df_m_anx_ill_mi,
  maxit = 0,
  printFlag = FALSE
)

method <- ini$method
pm <- ini$predictorMatrix

# 5. add imputation method for each variable
# continuous
method[numeric_vars] <- "pmm"

# binary
method[binary_vars] <- "logreg"

# unordered categorical
method[factor_vars] <- "polyreg"

# outcome NOT imputed
method[outcome_var] <- ""

# 6. Predictor matrix - which variables predict each variable for mice
# everyone predicts everyone
pm[,] <- 1

# no self-prediction
diag(pm) <- 0

# outcome not imputed
pm[outcome_var, ] <- 0

# BUT outcome allowed as predictor
pm[, outcome_var] <- 1
pm[outcome_var, outcome_var] <- 0

# 7. run imputation
imp_anx_m_quick <- mice(
  data = df_m_anx_ill_mi,
  method = method,
  predictorMatrix = pm,
  m = 50,
  maxit = 20,
  seed = 123,
  printFlag = TRUE
)

# 8. to check everything
 should now be zero for all imputed vars
colSums(is.na(complete(imp_anx_m_quick, 1)))
 check problems
imp_anx_m_quick$loggedEvents
 missing data pattern
md.pattern(df_m_anx_ill_mi)
 convergence plots
plot(imp_anx_m_quick)
 
# saving the imputation in case R crashes

saveRDS(imp_anx_m_quick, (here::here( "6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_anx_m_quick.rds")))

 


# m anx conf
  
# m conf anx
# Binary variables
df_m_anx_conf_mi$m_mar_status <- as.numeric(df_m_anx_conf_mi$m_mar_status)
df_m_anx_conf_mi$m_dr_change  <- as.numeric(df_m_anx_conf_mi$m_dr_change)
df_m_anx_conf_mi$m_sep        <- as.numeric(df_m_anx_conf_mi$m_sep)

# Categorical variables
df_m_anx_conf_mi$m_conf_nhs       <- factor(df_m_anx_conf_mi$m_conf_nhs)
df_m_anx_conf_mi$m_relig           <- factor(df_m_anx_conf_mi$m_relig)
df_m_anx_conf_mi$m_edu_level       <- factor(df_m_anx_conf_mi$m_edu_level)
df_m_anx_conf_mi$m_home_own_status <- factor(df_m_anx_conf_mi$m_home_own_status)
df_m_anx_conf_mi$m_eval_hlth       <- factor(df_m_anx_conf_mi$m_eval_hlth)


#dry run
ini_m_conf_anx <- mice(df_m_anx_conf_mi, maxit = 0, printFlag = FALSE)

method <- ini_m_conf_anx$method
pm_m_conf_anx     <- ini_m_conf_anx$predictorMatrix

# Prediction matrix
## dont predict itself
diag(pm_m_conf_anx) <- 0

#continuous /norm 
method[c(
  "m_age",           
  "m_parity",      
  "m_soc_supp",  
  "m_anx_score",
  "m_weigh_life", 
  "m_dep_score"
)] <- "pmm"

#binary / logreg
method[c(
  "m_mar_status","m_dr_change", "m_sep"
)] <- "logreg"

# Categorical / polyreg
method[c(
  "m_conf_nhs", "m_relig","m_edu_level","m_home_own_status",
  "m_eval_hlth"
)] <- "polyreg"


method["m_anx"] <- ""
method["b_soc_class"] <- ""
method["a_car_own"] <- ""
method["f_age"] <- ""
method["e_age"] <- ""
method["b_age"] <- ""
method["f_soc_supp"] <- ""
method["e_soc_supp"] <- ""
method["d_soc_supp"] <- ""
method["f_anx"] <- ""
method["e_anx"] <- ""
method["f_dep"] <- ""
method["e_dep"] <- ""

aux_vars <- c(
  "a_car_own","b_soc_class",
  "f_age","e_age","b_age",
  "f_soc_supp","e_soc_supp","f_anx", "f_dep", "e_dep", "d_soc_supp", "e_anx"
)

pm_m_conf_anx[aux_vars, ] <- 0

# m_ill_quick
pm_m_conf_anx["m_conf_nhs", ] <- 0
pm_m_conf_anx["m_conf_nhs", c(
  "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# age
pm_m_conf_anx["m_age", ] <- 0
pm_m_conf_anx["m_age", c(
  "m_conf_nhs", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "f_age", 
  "e_age", "b_age", "m_dep_score"
)] <- 1

# mar_status
pm_m_conf_anx["m_mar_status", ] <- 0
pm_m_conf_anx["m_mar_status", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# parity
pm_m_conf_anx["m_parity", ] <- 0
pm_m_conf_anx["m_parity", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# home_own_status
pm_m_conf_anx["m_home_own_status", ] <- 0
pm_m_conf_anx["m_home_own_status", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "a_car_own", "m_dep_score"
)] <- 1

# mum_hlth
pm_m_conf_anx["m_eval_hlth", ] <- 0
pm_m_conf_anx["m_eval_hlth", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_home_own_status", "m_dep_score"
)] <- 1

# soc_supp_mis
pm_m_conf_anx["m_soc_supp", ] <- 0
pm_m_conf_anx["m_soc_supp", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "f_soc_supp",
  "e_soc_supp", "m_home_own_status", "m_dep_score", "d_soc_supp"
)] <- 1

# sum_weigh_life
pm_m_conf_anx["m_weigh_life", ] <- 0
pm_m_conf_anx["m_weigh_life", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_dr_change", "m_sep", "m_anx_score", "m_home_own_status", "m_dep_score"
)] <- 1

# dr_change
pm_m_conf_anx["m_dr_change", ] <- 0
pm_m_conf_anx["m_dr_change", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_sep", "m_anx_score", "m_home_own_status", "m_weigh_life", "m_dep_score"
)] <- 1

# soc_class
pm_m_conf_anx["m_sep", ] <- 0
pm_m_conf_anx["m_sep", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_anx_score", "m_home_own_status",
  "b_soc_class", "m_weigh_life", "m_dr_change", "m_dep_score"
)] <- 1


# anx_score
pm_m_conf_anx["m_anx_score", ] <- 0
pm_m_conf_anx["m_anx_score", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_home_own_status",
  "f_anx", "m_weigh_life", "m_sep", "m_dr_change", "m_dep_score", "e_anx"
)] <- 1

# dep_score
pm_m_conf_anx["m_dep_score", ] <- 0
pm_m_conf_anx["m_dep_score", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_home_own_status",
  "m_weigh_life", "m_sep", "m_dr_change", "m_anx_score", "f_dep", "e_dep"
)] <- 1

# Do not predict anx
pm_m_conf_anx["m_anx", ] <- 0

## --- Run multiple imputation ---
imp_anx_m_conf <- mice(
  data = df_m_anx_conf_mi,
  method = method,
  predictorMatrix = pm_m_conf_anx,
  m = 50,
  maxit = 20,
  seed = 123
)


 


  
# saving the imputation in case R crashes

saveRDS(imp_anx_m_conf, (here::here( "6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_anx_m_conf.rds")))

 


# m anx help
  
# m help anx
# Binary variables
df_m_anx_help_mi$m_dr_change  <- factor(df_m_anx_help_mi$m_dr_change)
df_m_anx_help_mi$m_mar_status <- factor(df_m_anx_help_mi$m_mar_status)
df_m_anx_help_mi$m_sep        <- factor(df_m_anx_help_mi$m_sep)

# Categorical variables
df_m_anx_help_mi$m_alwys_help       <- factor(df_m_anx_help_mi$m_alwys_help)
df_m_anx_help_mi$m_relig           <- factor(df_m_anx_help_mi$m_relig)
df_m_anx_help_mi$m_edu_level       <- factor(df_m_anx_help_mi$m_edu_level)
df_m_anx_help_mi$m_home_own_status <- factor(df_m_anx_help_mi$m_home_own_status)
df_m_anx_help_mi$m_eval_hlth       <- factor(df_m_anx_help_mi$m_eval_hlth)



#dry run
ini_m_help_anx <- mice(df_m_anx_help_mi, maxit = 0, printFlag = FALSE)

method <- ini_m_help_anx$method
pm_m_help_anx     <- ini_m_help_anx$predictorMatrix

# Prediction matrix
## dont predict itself
diag(pm_m_help_anx) <- 0

#continuous /norm 

method[c(
  "m_age",           
  "m_parity",      
  "m_soc_supp",  
  "m_anx_score",
  "m_weigh_life"
)] <- "pmm"

#binary / logreg
method[c(
  "m_mar_status","m_dr_change", "m_sep"
)] <- "logreg"

# Categorical / polyreg
method[c(
  "m_alwys_help", "m_relig","m_edu_level","m_home_own_status",
  "m_eval_hlth"
)] <- "polyreg"


method["m_anx"] <- ""
method["b_soc_class"] <- ""
method["a_car_own"] <- ""
method["f_age"] <- ""
method["e_age"] <- ""
method["b_age"] <- ""
method["f_soc_supp"] <- ""
method["e_soc_supp"] <- ""
method["d_soc_supp"] <- ""
method["f_anx"] <- ""
method["e_anx"] <- ""
method["f_dep"] <- ""
method["e_dep"] <- ""

aux_vars <- c(
  "a_car_own","b_soc_class",
  "f_age","e_age","b_age",
  "f_soc_supp","e_soc_supp","f_anx", "f_dep", "e_dep", "d_soc_supp", "e_anx"
)

pm_m_help_anx[aux_vars, ] <- 0


# m_ill_quick
pm_m_help_anx["m_alwys_help", ] <- 0
pm_m_help_anx["m_alwys_help", c(
  "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# age
pm_m_help_anx["m_age", ] <- 0
pm_m_help_anx["m_age", c(
  "m_alwys_help", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "f_age", 
  "e_age", "b_age", "m_dep_score"
)] <- 1

# mar_status
pm_m_help_anx["m_mar_status", ] <- 0
pm_m_help_anx["m_mar_status", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# parity
pm_m_help_anx["m_parity", ] <- 0
pm_m_help_anx["m_parity", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# home_own_status
pm_m_help_anx["m_home_own_status", ] <- 0
pm_m_help_anx["m_home_own_status", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "a_car_own", "m_dep_score"
)] <- 1

# mum_hlth
pm_m_help_anx["m_eval_hlth", ] <- 0
pm_m_help_anx["m_eval_hlth", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_home_own_status", "m_dep_score"
)] <- 1

# soc_supp_mis
pm_m_help_anx["m_soc_supp", ] <- 0
pm_m_help_anx["m_soc_supp", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "f_soc_supp",
  "e_soc_supp", "m_home_own_status", "m_dep_score", "d_soc_supp"
)] <- 1

# sum_weigh_life
pm_m_help_anx["m_weigh_life", ] <- 0
pm_m_help_anx["m_weigh_life", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_dr_change", "m_sep", "m_anx_score", "m_home_own_status", "m_dep_score"
)] <- 1

# dr_change
pm_m_help_anx["m_dr_change", ] <- 0
pm_m_help_anx["m_dr_change", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_sep", "m_anx_score", "m_home_own_status", "m_weigh_life", "m_dep_score"
)] <- 1

# soc_class
pm_m_help_anx["m_sep", ] <- 0
pm_m_help_anx["m_sep", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_anx_score", "m_home_own_status",
  "b_soc_class", "m_weigh_life", "m_dr_change", "m_dep_score"
)] <- 1


# anx_score
pm_m_help_anx["m_anx_score", ] <- 0
pm_m_help_anx["m_anx_score", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_home_own_status",
  "f_anx", "m_weigh_life", "m_sep", "m_dr_change", "m_dep_score", "e_anx"
)] <- 1

# dep_score
pm_m_help_anx["m_dep_score", ] <- 0
pm_m_help_anx["m_dep_score", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_home_own_status",
  "f_dep", "e_dep", "m_weigh_life", "m_sep", "m_dr_change", "m_anx_score"
)] <- 1

# Do not predict anx
pm_m_help_anx["m_anx", ] <- 0

## --- Run multiple imputation ---
imp_anx_m_help <- mice(
  data = df_m_anx_help_mi,
  method = method,
  predictorMatrix = pm_m_help_anx,
  m = 50,
  maxit = 20,
  seed = 123
)


 

# saving the imputation in case R crashes

saveRDS(imp_anx_m_help, (here::here( "6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_anx_m_help.rds")))


 


# m dep ill
  
# M dep ill 
# Binary variables
df_m_dep_ill_mi$m_mar_status <- factor(df_m_dep_ill_mi$m_mar_status)
df_m_dep_ill_mi$m_dr_change  <- factor(df_m_dep_ill_mi$m_dr_change)
df_m_dep_ill_mi$m_sep        <- factor(df_m_dep_ill_mi$m_sep)

# Categorical variables
df_m_dep_ill_mi$m_ill_quick       <- factor(df_m_dep_ill_mi$m_ill_quick)
df_m_dep_ill_mi$m_relig           <- factor(df_m_dep_ill_mi$m_relig)
df_m_dep_ill_mi$m_edu_level       <- factor(df_m_dep_ill_mi$m_edu_level)
df_m_dep_ill_mi$m_home_own_status <- factor(df_m_dep_ill_mi$m_home_own_status)
df_m_dep_ill_mi$m_eval_hlth       <- factor(df_m_dep_ill_mi$m_eval_hlth)


#dry run
ini_m_ill_dep <- mice(df_m_dep_ill_mi, maxit = 0, printFlag = FALSE)

method <- ini_m_ill_dep$method
pm_m_ill_dep     <- ini_m_ill_dep$predictorMatrix

# Prediction matrix
## dont predict itself
diag(pm_m_ill_dep) <- 0

# inputting imputation method

#continuous /norm 
method[c(
  "m_age",           
  "m_parity",      
  "m_soc_supp",  
  "m_anx_score",
  "m_weigh_life", 
  "m_dep_score"
)] <- "pmm"

#binary / logreg
method[c(
  "m_mar_status","m_dr_change", "m_sep"
)] <- "logreg"

# Categorical / polyreg
method[c(
  "m_ill_quick", "m_relig","m_edu_level","m_home_own_status",
  "m_eval_hlth"
)] <- "polyreg"

method["m_dep"] <- ""
method["e_dep"] <- ""
method["b_soc_class"] <- ""
method["a_car_own"] <- ""
method["f_age"] <- ""
method["e_age"] <- ""
method["b_age"] <- ""
method["f_soc_supp"] <- ""
method["e_soc_supp"] <- ""
method["d_soc_supp"] <- ""
method["f_dep"] <- ""
method["f_anx"] <- ""
method["e_anx"] <- ""

aux_vars <- c(
  "a_car_own","b_soc_class",
  "f_age","e_age","b_age",
  "f_soc_supp","e_soc_supp","f_dep", "e_dep", "d_soc_supp", "e_anx"
)

pm_m_ill_dep[aux_vars, ] <- 0

# m_ill_quick
pm_m_ill_dep["m_ill_quick", ] <- 0
pm_m_ill_dep["m_ill_quick", c(
  "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# age
pm_m_ill_dep["m_age", ] <- 0
pm_m_ill_dep["m_age", c(
  "m_ill_quick", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "f_age", 
  "e_age", "b_age", "m_dep_score"
)] <- 1

# mar_status
pm_m_ill_dep["m_mar_status", ] <- 0
pm_m_ill_dep["m_mar_status", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# parity
pm_m_ill_dep["m_parity", ] <- 0
pm_m_ill_dep["m_parity", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# home_own_status
pm_m_ill_dep["m_home_own_status", ] <- 0
pm_m_ill_dep["m_home_own_status", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "a_car_own", "m_dep_score"
)] <- 1

# mum_hlth
pm_m_ill_dep["m_eval_hlth", ] <- 0
pm_m_ill_dep["m_eval_hlth", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_home_own_status", "m_dep_score"
)] <- 1

# soc_supp
pm_m_ill_dep["m_soc_supp", ] <- 0
pm_m_ill_dep["m_soc_supp", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "f_soc_supp",
  "e_soc_supp", "m_home_own_status", "m_dep_score", "d_soc_supp"
)] <- 1

# sum_weigh_life
pm_m_ill_dep["m_weigh_life", ] <- 0
pm_m_ill_dep["m_weigh_life", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_dr_change", "m_sep", "m_anx_score", "m_home_own_status", "m_dep_score"
)] <- 1

# dr_change
pm_m_ill_dep["m_dr_change", ] <- 0
pm_m_ill_dep["m_dr_change", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_sep", "m_anx_score", "m_home_own_status", "m_weigh_life", "m_dep_score"
)] <- 1

# soc_class
pm_m_ill_dep["m_sep", ] <- 0
pm_m_ill_dep["m_sep", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_anx_score", "m_home_own_status",
  "b_soc_class", "m_weigh_life", "m_dr_change", "m_dep_score"
)] <- 1


# anx_score
pm_m_ill_dep["m_anx_score", ] <- 0
pm_m_ill_dep["m_anx_score", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_home_own_status",
  "f_anx", "m_weigh_life", "m_sep", "m_dr_change", "m_dep_score", "e_anx"
)] <- 1

# dep_score
pm_m_ill_dep["m_dep_score", ] <- 0
pm_m_ill_dep["m_dep_score", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_home_own_status",
  "f_anx", "m_weigh_life", "m_sep", "m_dr_change", "m_anx_score","f_dep", "e_dep"
)] <- 1

# Do not predict anx
pm_m_ill_dep["m_dep", ] <- 0

## --- Run multiple imputation ---
imp_dep_m_quick <- mice(
  data = df_m_dep_ill_mi,
  method = method,
  predictorMatrix = pm_m_ill_dep,
  m = 50,
  maxit = 20,
  seed = 123
)



 

  
# saving the imputation in case R crashes
saveRDS(imp_dep_m_quick, (here::here( "6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_dep_m_quick.rds")))



# m dep conf
  
# m conf anx
# Binary variables
df_m_dep_conf_mi$m_mar_status <- factor(df_m_dep_conf_mi$m_mar_status)
df_m_dep_conf_mi$m_dr_change  <- factor(df_m_dep_conf_mi$m_dr_change)
df_m_dep_conf_mi$m_sep        <- factor(df_m_dep_conf_mi$m_sep)

# Categorical variables
df_m_dep_conf_mi$m_conf_nhs        <- factor(df_m_dep_conf_mi$m_conf_nhs)
df_m_dep_conf_mi$m_relig           <- factor(df_m_dep_conf_mi$m_relig)
df_m_dep_conf_mi$m_edu_level       <- factor(df_m_dep_conf_mi$m_edu_level)
df_m_dep_conf_mi$m_home_own_status <- factor(df_m_dep_conf_mi$m_home_own_status)
df_m_dep_conf_mi$m_eval_hlth       <- factor(df_m_dep_conf_mi$m_eval_hlth)


#dry run
ini_m_conf_dep <- mice(df_m_dep_conf_mi, maxit = 0, printFlag = FALSE)

method <- ini_m_conf_dep$method
pm_m_conf_dep     <- ini_m_conf_dep$predictorMatrix

# Prediction matrix
## dont predict itself
diag(pm_m_conf_dep) <- 0

#continuous /norm 
method[c(
  "m_age",           
  "m_parity",      
  "m_soc_supp",  
  "m_anx_score",
  "m_weigh_life", 
  "m_dep_score"
)] <- "pmm"

#binary / logreg
method[c(
  "m_mar_status","m_dr_change", "m_sep"
)] <- "logreg"

# Categorical / polyreg
method[c(
  "m_conf_nhs", "m_relig","m_edu_level","m_home_own_status",
  "m_eval_hlth"
)] <- "polyreg"


method["m_dep"] <- ""
method["e_dep"] <- ""
method["b_soc_class"] <- ""
method["a_car_own"] <- ""
method["f_age"] <- ""
method["e_age"] <- ""
method["b_age"] <- ""
method["f_soc_supp"] <- ""
method["e_soc_supp"] <- ""
method["d_soc_supp"] <- ""
method["f_dep"] <- ""
method["f_anx"] <- ""
method["e_anx"] <- ""

aux_vars <- c(
  "a_car_own","b_soc_class",
  "f_age","e_age","b_age",
  "f_soc_supp","e_soc_supp","f_dep", "e_dep", "d_soc_supp", "e_anx"
)

pm_m_conf_dep[aux_vars, ] <- 0

# m_ill_quick
pm_m_conf_dep["m_conf_nhs", ] <- 0
pm_m_conf_dep["m_conf_nhs", c(
  "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# age
pm_m_conf_dep["m_age", ] <- 0
pm_m_conf_dep["m_age", c(
  "m_conf_nhs", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "f_age", 
  "e_age", "b_age", "m_dep_score"
)] <- 1

# mar_status
pm_m_conf_dep["m_mar_status", ] <- 0
pm_m_conf_dep["m_mar_status", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# parity
pm_m_conf_dep["m_parity", ] <- 0
pm_m_conf_dep["m_parity", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# home_own_status
pm_m_conf_dep["m_home_own_status", ] <- 0
pm_m_conf_dep["m_home_own_status", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "a_car_own", "m_dep_score"
)] <- 1

# mum_hlth
pm_m_conf_dep["m_eval_hlth", ] <- 0
pm_m_conf_dep["m_eval_hlth", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_home_own_status", "m_dep_score"
)] <- 1

# soc_supp_mis
pm_m_conf_dep["m_soc_supp", ] <- 0
pm_m_conf_dep["m_soc_supp", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "f_soc_supp",
  "e_soc_supp", "m_home_own_status", "m_dep_score", "d_soc_supp"
)] <- 1

# sum_weigh_life
pm_m_conf_dep["m_weigh_life", ] <- 0
pm_m_conf_dep["m_weigh_life", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_dr_change", "m_sep", "m_anx_score", "m_home_own_status", "m_dep_score"
)] <- 1

# dr_change
pm_m_conf_dep["m_dr_change", ] <- 0
pm_m_conf_dep["m_dr_change", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_sep", "m_anx_score", "m_home_own_status", "m_weigh_life", "m_dep_score"
)] <- 1

# soc_class
pm_m_conf_dep["m_sep", ] <- 0
pm_m_conf_dep["m_sep", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_anx_score", "m_home_own_status",
  "b_soc_class", "m_weigh_life", "m_dr_change", "m_dep_score"
)] <- 1


# anx_score
pm_m_conf_dep["m_anx_score", ] <- 0
pm_m_conf_dep["m_anx_score", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_home_own_status",
  "f_anx", "m_weigh_life", "m_sep", "m_dr_change", "m_dep_score", "e_anx"
)] <- 1

# dep_score
pm_m_conf_dep["m_dep_score", ] <- 0
pm_m_conf_dep["m_dep_score", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_home_own_status",
  "f_dep", "e_dep", "m_weigh_life", "m_sep", "m_dr_change", "m_anx_score"
)] <- 1

# Do not predict dep
pm_m_conf_dep["m_dep", ] <- 0

## --- Run multiple imputation ---
imp_dep_m_conf <- mice(
  data = df_m_dep_conf_mi,
  method = method,
  predictorMatrix = pm_m_conf_dep,
  m = 50,
  maxit = 20,
  seed = 123
)


 

  
# saving the imputation in case R crashes
saveRDS(imp_dep_m_conf, (here::here( "6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_dep_m_conf.rds")))

 

# m dep help

# m help dep
# Binary variables
df_m_dep_help_mi$m_dr_change  <- factor(df_m_dep_help_mi$m_dr_change)
df_m_dep_help_mi$m_mar_status <- factor(df_m_dep_help_mi$m_mar_status)
df_m_dep_help_mi$m_sep        <- factor(df_m_dep_help_mi$m_sep)

# Categorical variables
df_m_dep_help_mi$m_alwys_help      <- factor(df_m_dep_help_mi$m_alwys_help)
df_m_dep_help_mi$m_relig           <- factor(df_m_dep_help_mi$m_relig)
df_m_dep_help_mi$m_edu_level       <- factor(df_m_dep_help_mi$m_edu_level)
df_m_dep_help_mi$m_home_own_status <- factor(df_m_dep_help_mi$m_home_own_status)
df_m_dep_help_mi$m_eval_hlth       <- factor(df_m_dep_help_mi$m_eval_hlth)



#dry run
ini_m_help_dep <- mice(df_m_dep_help_mi, maxit = 0, printFlag = FALSE)

method <- ini_m_help_dep$method
pm_m_help_dep     <- ini_m_help_dep$predictorMatrix

# Prediction matrix
## dont predict itself
diag(pm_m_help_dep) <- 0

#continuous /norm 
#continuous /norm 
method[c(
  "m_age",           
  "m_parity",      
  "m_soc_supp",  
  "m_anx_score",
  "m_weigh_life",
  "m_dep_score"
)] <- "pmm"

#binary / logreg
method[c(
  "m_mar_status","m_dr_change", "m_sep"
)] <- "logreg"

# Categorical / polyreg
method[c(
  "m_alwys_help", "m_relig","m_edu_level","m_home_own_status",
  "m_eval_hlth"
)] <- "polyreg"


method["m_dep"] <- ""
method["e_dep"] <- ""
method["b_soc_class"] <- ""
method["a_car_own"] <- ""
method["f_age"] <- ""
method["e_age"] <- ""
method["b_age"] <- ""
method["f_soc_supp"] <- ""
method["e_soc_supp"] <- ""
method["d_soc_supp"] <- ""
method["f_dep"] <- ""
method["f_anx"] <- ""
method["e_anx"] <- ""

aux_vars <- c(
  "a_car_own","b_soc_class",
  "f_age","e_age","b_age",
  "f_soc_supp","e_soc_supp","f_dep", "e_dep", "d_soc_supp", "e_anx"
)

pm_m_help_dep[aux_vars, ] <- 0


# m_ill_quick
pm_m_help_dep["m_alwys_help", ] <- 0
pm_m_help_dep["m_alwys_help", c(
  "m_age", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# age
pm_m_help_dep["m_age", ] <- 0
pm_m_help_dep["m_age", c(
  "m_alwys_help", "m_mar_status",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "f_age", 
  "e_age", "b_age", "m_dep_score"
)] <- 1

# mar_status
pm_m_help_dep["m_mar_status", ] <- 0
pm_m_help_dep["m_mar_status", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_parity", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# parity
pm_m_help_dep["m_parity", ] <- 0
pm_m_help_dep["m_parity", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# home_own_status
pm_m_help_dep["m_home_own_status", ] <- 0
pm_m_help_dep["m_home_own_status", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "a_car_own", "m_dep_score"
)] <- 1

# mum_hlth
pm_m_help_dep["m_eval_hlth", ] <- 0
pm_m_help_dep["m_eval_hlth", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_home_own_status", "m_dep_score"
)] <- 1

# soc_supp_mis
pm_m_help_dep["m_soc_supp", ] <- 0
pm_m_help_dep["m_soc_supp", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "f_soc_supp",
  "e_soc_supp", "m_home_own_status", "m_dep_score", "d_soc_supp"
)] <- 1

# sum_weigh_life
pm_m_help_dep["m_weigh_life", ] <- 0
pm_m_help_dep["m_weigh_life", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_dr_change", "m_sep", "m_anx_score", "m_home_own_status", "m_dep_score"
)] <- 1

# dr_change
pm_m_help_dep["m_dr_change", ] <- 0
pm_m_help_dep["m_dr_change", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_sep", "m_anx_score", "m_home_own_status", "m_weigh_life", "m_dep_score"
)] <- 1

# soc_class
pm_m_help_dep["m_sep", ] <- 0
pm_m_help_dep["m_sep", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_anx_score", "m_home_own_status",
  "b_soc_class", "m_weigh_life", "m_dr_change", "m_dep_score"
)] <- 1


# anx_score
pm_m_help_dep["m_anx_score", ] <- 0
pm_m_help_dep["m_anx_score", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_home_own_status",
  "f_anx", "m_weigh_life", "m_sep", "m_dr_change", "m_dep_score", "e_anx"
)] <- 1

# dep_score
pm_m_help_dep["m_dep_score", ] <- 0
pm_m_help_dep["m_dep_score", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_home_own_status",
  "f_dep", "e_dep", "m_weigh_life", "m_sep", "m_dr_change", "m_anx_score"
)] <- 1

# Do not predict anx
pm_m_help_dep["m_dep", ] <- 0

## --- Run multiple imputation ---
imp_dep_m_help <- mice(
  data = df_m_dep_help_mi,
  method = method,
  predictorMatrix = pm_m_help_dep,
  m = 50,
  maxit = 20,
  seed = 123
)

 

  
# saving the imputation in case R crashes

saveRDS(imp_dep_m_help, (here::here( "6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_dep_m_help.rds")))


 

# PARTNERS

# p anx ill
  
# p anx ill 
# Binary variables
df_p_anx_ill_mi$p_mar_status <- factor(df_p_anx_ill_mi$p_mar_status)
df_p_anx_ill_mi$m_dr_change  <- factor(df_p_anx_ill_mi$m_dr_change)
df_p_anx_ill_mi$p_sep        <- factor(df_p_anx_ill_mi$p_sep)

# Categorical variables
df_p_anx_ill_mi$p_ill_quick       <- factor(df_p_anx_ill_mi$p_ill_quick)
df_p_anx_ill_mi$p_relig           <- factor(df_p_anx_ill_mi$p_relig)
df_p_anx_ill_mi$p_edu_level       <- factor(df_p_anx_ill_mi$p_edu_level)
df_p_anx_ill_mi$m_home_own_status <- factor(df_p_anx_ill_mi$m_home_own_status)
df_p_anx_ill_mi$p_eval_hlth       <- factor(df_p_anx_ill_mi$p_eval_hlth)


#dry run
ini_p_ill_anx <- mice(df_p_anx_ill_mi, maxit = 0, printFlag = FALSE)

method <- ini_p_ill_anx$method
pm_p_ill_anx     <- ini_p_ill_anx$predictorMatrix

# Prediction matrix
## dont predict itself
diag(pm_p_ill_anx) <- 0

# inputting imputation method

#continuous /norm 
method[c(
  "p_age",           
  "m_parity",      
  "p_soc_supp",  
  "p_anx_score",
  "p_weigh_life",
  "p_dep_score"
)] <- "pmm"

#binary / logreg
method[c(
  "p_mar_status","m_dr_change", "p_sep"
)] <- "logreg"

# Categorical / polyreg
method[c(
  "p_ill_quick", "p_relig","p_edu_level","m_home_own_status",
  "p_eval_hlth"
)] <- "polyreg"


method["p_anx" ] <- ""
method["pg_mar"] <- ""
method["pf_mar"] <- ""
method["pd_age"] <- ""
method["pc_age"] <- ""
method["pa_age"] <- ""
method["pb_age"] <- ""
method["pb_anx"] <- ""
method["pc_anx"] <- ""
method["pe_anx"] <- ""
method["pc_soc_supp"] <- ""
method[c("m_ill_quick", "m_dep", "m_age", "m_mar_status", 
         "m_relig", "m_edu_level",
         "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_sep", "m_anx_score", "m_dep_score", "a_car_own")] <- ""



aux_vars <- c(
  "pg_mar", "pf_mar", "pd_age", "pc_age", "pa_age", "pb_age", "pc_soc_supp", "m_ill_quick", "m_dep", "m_age",  
  "m_mar_status", "m_relig", "m_edu_level",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life","m_sep", "m_anx_score", "m_dep_score", "a_car_own"
)


# p_ill_quick
pm_p_ill_anx["p_ill_quick", ] <- 0
pm_p_ill_anx["p_ill_quick", c(
  "p_age", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score"
)] <- 1

# age
pm_p_ill_anx["p_age", ] <- 0
pm_p_ill_anx["p_age", c(
  "p_ill_quick", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "pa_age", 
  "pb_age", "pc_age", "pd_age", "p_dep_score"
)] <- 1

# mar_status
pm_p_ill_anx["p_mar_status", ] <- 0
pm_p_ill_anx["p_mar_status", c(
  "p_ill_quick", "p_age",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score", "pg_mar", "pf_mar"
)] <- 1

# parity
pm_p_ill_anx["m_parity", ] <- 0
pm_p_ill_anx["m_parity", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# home_own_status
pm_p_ill_anx["m_home_own_status", ] <- 0
pm_p_ill_anx["m_home_own_status", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "a_car_own", "m_dep_score"
)] <- 1

# mum_hlth
pm_p_ill_anx["p_eval_hlth", ] <- 0
pm_p_ill_anx["p_eval_hlth", c(
  "p_ill_quick", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "m_home_own_status", "p_dep_score"
)] <- 1

# soc_supp_mis
pm_p_ill_anx["p_soc_supp", ] <- 0
pm_p_ill_anx["p_soc_supp", c(
  "p_ill_quick", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_weigh_life", "m_dr_change", "p_sep", "m_anx_score", "pc_soc_supp",
  "m_home_own_status", "p_dep_score"
)] <- 1

# weigh_life
pm_p_ill_anx["p_weigh_life", ] <- 0
pm_p_ill_anx["p_weigh_life", c(
  "p_ill_quick", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "m_dr_change", "p_sep", "p_anx_score", "m_home_own_status", "p_dep_score"
)] <- 1

# dr_change 
pm_p_ill_anx["m_dr_change", ] <- 0
pm_p_ill_anx["m_dr_change", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_sep", "m_anx_score", "m_home_own_status", "m_weigh_life", "m_dep_score"
)] <- 1

# sep
pm_p_ill_anx["p_sep", ] <- 0
pm_p_ill_anx["p_sep", c(
  "p_ill_quick", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "p_anx_score", "m_home_own_status",
  "p_weigh_life", "m_dr_change", "p_dep_score"
)] <- 1


# anx_score
pm_p_ill_anx["p_anx_score", ] <- 0
pm_p_ill_anx["p_anx_score", c(
  "p_ill_quick", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "m_home_own_status",
  "p_weigh_life", "p_sep", "m_dr_change", "p_dep_score","pc_anx", "pe_anx"
)] <- 1

# dep_score
pm_p_ill_anx["p_dep_score", ] <- 0
pm_p_ill_anx["p_dep_score", c(
  "p_ill_quick", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "m_home_own_status",
  "p_weigh_life", "p_sep", "m_dr_change", "p_anx_score"
)] <- 1



# Do not predict anx
pm_p_ill_anx["p_anx", ] <- 0

## --- Run multiple imputation ---
imp_anx_p_quick <- mice(
  data = df_p_anx_ill_mi,
  method = method,
  predictorMatrix = pm_p_ill_anx,
  m = 50,
  maxit = 20,
  seed = 123
)


 

  
# saving the imputation in case R crashes
saveRDS(imp_anx_p_quick, (here::here( "6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_anx_p_quick.rds")))

 


# p anx conf
  
# p conf anx
# Binary variables
df_p_anx_conf_mi$p_mar_status <- factor(df_p_anx_conf_mi$p_mar_status)
df_p_anx_conf_mi$m_dr_change  <- factor(df_p_anx_conf_mi$m_dr_change)
df_p_anx_conf_mi$p_sep        <- factor(df_p_anx_conf_mi$p_sep)

# Categorical variables
df_p_anx_conf_mi$p_conf_nhs        <- factor(df_p_anx_conf_mi$p_conf_nhs)
df_p_anx_conf_mi$p_relig           <- factor(df_p_anx_conf_mi$p_relig)
df_p_anx_conf_mi$p_edu_level       <- factor(df_p_anx_conf_mi$p_edu_level)
df_p_anx_conf_mi$m_home_own_status <- factor(df_p_anx_conf_mi$m_home_own_status)
df_p_anx_conf_mi$p_eval_hlth       <- factor(df_p_anx_conf_mi$p_eval_hlth)


#dry run
ini_p_conf_anx <- mice(df_p_anx_conf_mi, maxit = 0, printFlag = FALSE)

method <- ini_p_conf_anx$method
pm_p_conf_anx     <- ini_p_conf_anx$predictorMatrix

# Prediction matrix
## dont predict itself
diag(pm_p_conf_anx) <- 0

#continuous /norm 
method[c(
  "p_age",           
  "m_parity",      
  "p_soc_supp",  
  "p_anx_score",
  "p_weigh_life", 
  "p_dep_score"
)] <- "pmm"

#binary / logreg
method[c(
  "p_mar_status","m_dr_change", "p_sep"
)] <- "logreg"

# Categorical / polyreg
method[c(
  "p_conf_nhs", "p_relig","p_edu_level","m_home_own_status",
  "p_eval_hlth"
)] <- "polyreg"


method["p_anx" ] <- ""
method["pg_mar"] <- ""
method["pf_mar"] <- ""
method["pd_age"] <- ""
method["pc_age"] <- ""
method["pa_age"] <- ""
method["pb_age"] <- ""
method["pc_anx"] <- ""
method["pe_anx"] <- ""
method["pc_soc_supp"] <- ""
method[c("m_conf_nhs", "m_dep", "m_age", "m_mar_status", 
         "m_relig", "m_edu_level",
         "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_sep", "m_anx_score", "m_dep_score", "a_car_own")] <- ""

aux_vars <- c(
  "pg_mar", "pf_mar", "pd_age", "pc_age", "pa_age", "pb_age", "pc_soc_supp", "m_conf_nhs", "m_dep", "m_age",  
  "m_mar_status", "m_relig", "m_edu_level", "pc_anx", "pe_anx",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_sep", "m_anx_score", "m_dep_score", "a_car_own"
)


# p_conf_nhs
pm_p_conf_anx["p_conf_nhs", ] <- 0
pm_p_conf_anx["p_conf_nhs", c(
  "p_age", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score"
)] <- 1

# age
pm_p_conf_anx["p_age", ] <- 0
pm_p_conf_anx["p_age", c(
  "p_conf_nhs", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "pa_age", 
  "pb_age", "pc_age", "pd_age", "p_dep_score"
)] <- 1

# mar_status
pm_p_conf_anx["p_mar_status", ] <- 0
pm_p_conf_anx["p_mar_status", c(
  "p_conf_nhs", "p_age",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score", "pg_mar", "pf_mar"
)] <- 1

# parity
pm_p_conf_anx["m_parity", ] <- 0
pm_p_conf_anx["m_parity", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# home_own_status
pm_p_conf_anx["m_home_own_status", ] <- 0
pm_p_conf_anx["m_home_own_status", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "a_car_own", "m_dep_score"
)] <- 1

# eval_hlth
pm_p_conf_anx["p_eval_hlth", ] <- 0
pm_p_conf_anx["p_eval_hlth", c(
  "p_conf_nhs", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "m_home_own_status", "p_dep_score"
)] <- 1

# soc_supp
pm_p_conf_anx["p_soc_supp", ] <- 0
pm_p_conf_anx["p_soc_supp", c(
  "p_conf_nhs",  "p_age",
  "p_relig",     "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score",
  "pc_soc_supp", "m_home_own_status", "p_dep_score"
)] <- 1

# weigh_life
pm_p_conf_anx["p_weigh_life", ] <- 0
pm_p_conf_anx["p_weigh_life", c(
  "p_conf_nhs", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "m_dr_change", "p_sep", "p_anx_score", "m_home_own_status", "p_dep_score"
)] <- 1

# dr_change
pm_p_conf_anx["m_dr_change", ] <- 0
pm_p_conf_anx["m_dr_change", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_sep", "m_anx_score", "m_home_own_status", "m_weigh_life", "m_dep_score"
)] <- 1

# soc_class
pm_p_conf_anx["p_sep", ] <- 0
pm_p_conf_anx["p_sep", c(
  "p_conf_nhs", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "p_anx_score", "m_home_own_status",
  "p_weigh_life", "m_dr_change", "p_dep_score"
)] <- 1


# anx_score
pm_p_conf_anx["p_anx_score", ] <- 0
pm_p_conf_anx["p_anx_score", c(
  "p_conf_nhs", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "m_home_own_status",
  "p_weigh_life", "p_sep", "m_dr_change", "p_dep_score", "pe_anx", "pc_anx"
)] <- 1

# dep_score
pm_p_conf_anx["p_dep_score", ] <- 0
pm_p_conf_anx["p_dep_score", c(
  "p_conf_nhs", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "m_home_own_status",
  "p_weigh_life", "p_sep", "m_dr_change", "p_anx_score"
)] <- 1

# Do not predict anx
pm_p_conf_anx["p_anx", ] <- 0

## --- Run multiple imputation ---
imp_anx_p_conf <- mice(
  data = df_p_anx_conf_mi,
  method = method,
  predictorMatrix = pm_p_conf_anx,
  m = 50,
  maxit = 20,
  seed = 123
)


 


  
# saving the imputation in case R crashes
saveRDS(imp_anx_p_conf, (here::here( "6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_anx_p_conf.rds")))

 


# p anx help
  
# m help anx
# Binary variables
df_p_anx_help_mi$m_dr_change  <- factor(df_p_anx_help_mi$m_dr_change)
df_p_anx_help_mi$p_mar_status <- factor(df_p_anx_help_mi$p_mar_status)
df_p_anx_help_mi$p_sep        <- factor(df_p_anx_help_mi$p_sep)

# Categorical variables
df_p_anx_help_mi$p_alwys_help      <- factor(df_p_anx_help_mi$p_alwys_help)
df_p_anx_help_mi$p_relig           <- factor(df_p_anx_help_mi$p_relig)
df_p_anx_help_mi$p_edu_level       <- factor(df_p_anx_help_mi$p_edu_level)
df_p_anx_help_mi$m_home_own_status <- factor(df_p_anx_help_mi$m_home_own_status)
df_p_anx_help_mi$p_eval_hlth       <- factor(df_p_anx_help_mi$p_eval_hlth)



#dry run
ini_p_help_anx <- mice(df_p_anx_help_mi, maxit = 0, printFlag = FALSE)

method <- ini_p_help_anx$method
pm_p_help_anx     <- ini_p_help_anx$predictorMatrix

# Prediction matrix
## dont predict itself
diag(pm_p_help_anx) <- 0

#continuous /norm 
#continuous /norm 
method[c(
  "p_age",           
  "m_parity",      
  "p_soc_supp",  
  "p_anx_score",
  "p_weigh_life",
  "p_dep_score"
)] <- "pmm"

#binary / logreg
method[c(
  "p_mar_status","m_dr_change", "p_sep"
)] <- "logreg"

# Categorical / polyreg
method[c(
  "p_alwys_help", "p_relig","p_edu_level","m_home_own_status",
  "p_eval_hlth"
)] <- "polyreg"


method["p_anx" ] <- ""
method["pg_mar"] <- ""
method["pf_mar"] <- ""
method["pd_age"] <- ""
method["pc_age"] <- ""
method["pa_age"] <- ""
method["pb_age"] <- ""
method["pc_anx"] <- ""
method["pe_anx"] <- ""
method["pc_soc_supp"] <- ""
method[c("m_alwys_help", "m_dep", "m_age", "m_mar_status", 
         "m_relig", "m_edu_level",
         "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_sep", "m_anx_score", "m_dep_score", "a_car_own")] <- ""


aux_vars <- c(
  "pg_mar", "pf_mar", "pd_age", "pc_age", "pa_age", "pb_age", "pc_soc_supp", "m_conf_nhs", "m_dep", "m_age",  
  "m_mar_status", "m_relig", "m_edu_level",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_sep", "m_anx_score", "m_dep_score", "a_car_own"
)



# p_ill_quick
pm_p_help_anx["p_alwys_help", ] <- 0
pm_p_help_anx["p_alwys_help", c(
  "p_age",       "p_mar_status",
  "p_relig",     "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp",  "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score"
)] <- 1

# age
pm_p_help_anx["p_age", ] <- 0
pm_p_help_anx["p_age", c(
  "p_alwys_help", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "pa_age", 
  "pb_age", "pc_age", "pd_age", "p_dep_score"
)] <- 1

# mar_status
pm_p_help_anx["p_mar_status", ] <- 0
pm_p_help_anx["p_mar_status", c(
  "p_alwys_help", "p_age",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score", "pg_mar", "pf_mar"
)] <- 1

# parity
pm_p_help_anx["m_parity", ] <- 0
pm_p_help_anx["m_parity", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# home_own_status
pm_p_help_anx["m_home_own_status", ] <- 0
pm_p_help_anx["m_home_own_status", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "a_car_own", "m_dep_score"
)] <- 1

# mum_hlth
pm_p_help_anx["p_eval_hlth", ] <- 0
pm_p_help_anx["p_eval_hlth", c(
  "p_alwys_help", "p_age",
  "p_relig",      "p_edu_level",  "p_mar_status", "m_parity",
  "p_soc_supp",   "m_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "m_home_own_status", "p_dep_score"
)] <- 1

# soc_supp
pm_p_help_anx["m_soc_supp", ] <- 0
pm_p_help_anx["m_soc_supp", c(
  "p_alwys_help", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score",
  "pc_soc_supp", "m_home_own_status", "p_dep_score"
)] <- 1

# weigh_life
pm_p_help_anx["p_weigh_life", ] <- 0
pm_p_help_anx["p_weigh_life", c(
  "p_alwys_help", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "m_dr_change", "p_sep", "p_anx_score", "m_home_own_status", "p_dep_score"
)] <- 1

# dr_change
pm_p_help_anx["m_dr_change", ] <- 0
pm_p_help_anx["m_dr_change", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_sep", "m_anx_score", "m_home_own_status", "m_weigh_life", "m_dep_score"
)] <- 1

# soc_class
pm_p_help_anx["p_sep", ] <- 0
pm_p_help_anx["p_sep", c(
  "p_alwys_help", "p_age",
  "p_relig",      "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth",  "p_soc_supp",  "p_anx_score", "m_home_own_status",
  "p_weigh_life", "m_dr_change", "p_dep_score"
)] <- 1


# anx_score
pm_p_help_anx["m_anx_score", ] <- 0
pm_p_help_anx["m_anx_score", c(
  "p_alwys_help", "p_age",
  "p_relig",      "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth",  "p_soc_supp", "m_home_own_status",
  "p_weigh_life", "p_sep", "m_dr_change", "p_dep_score", "pc_anx", "pe_anx"
)] <- 1

# dep_score
pm_p_help_anx["p_dep_score", ] <- 0
pm_p_help_anx["p_dep_score", c(
  "p_alwys_help", "p_age",
  "p_relig",      "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth",  "p_soc_supp",  "m_home_own_status",
  "p_weigh_life", "p_sep",       "m_dr_change", "p_anx_score"
)] <- 1

# Do not predict anx
pm_p_help_anx["p_anx", ] <- 0

## --- Run multiple imputation ---
imp_anx_p_help <- mice(
  data = df_p_anx_help_mi,
  method = method,
  predictorMatrix = pm_p_help_anx,
  m = 50,
  maxit = 20,
  seed = 123
)


 
# saving the imputation in case R crashes
saveRDS(imp_anx_p_help, (here::here( "6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_anx_p_help.rds")))


 


# p dep ill
  
# p anx ill 
# Binary variables
df_p_dep_ill_mi$p_mar_status <- factor(df_p_dep_ill_mi$p_mar_status)
df_p_dep_ill_mi$m_dr_change  <- factor(df_p_dep_ill_mi$m_dr_change)
df_p_dep_ill_mi$p_sep        <- factor(df_p_dep_ill_mi$p_sep)

# Categorical variables
df_p_dep_ill_mi$p_ill_quick       <- factor(df_p_dep_ill_mi$p_ill_quick)
df_p_dep_ill_mi$p_relig           <- factor(df_p_dep_ill_mi$p_relig)
df_p_dep_ill_mi$p_edu_level       <- factor(df_p_dep_ill_mi$p_edu_level)
df_p_dep_ill_mi$m_home_own_status <- factor(df_p_dep_ill_mi$m_home_own_status)
df_p_dep_ill_mi$p_eval_hlth       <- factor(df_p_dep_ill_mi$p_eval_hlth)


#dry run
ini_p_ill_dep <- mice(df_p_dep_ill_mi, maxit = 0, printFlag = FALSE)

method <- ini_p_ill_dep$method
pm_p_ill_dep    <- ini_p_ill_dep$predictorMatrix

# Prediction matrix
## dont predict itself
diag(pm_p_ill_dep) <- 0

# inputting imputation method

#continuous /norm 
method[c(
  "p_age",           
  "m_parity",      
  "p_soc_supp",  
  "p_anx_score",
  "p_weigh_life",
  "p_dep_score"
)] <- "pmm"

#binary / logreg
method[c(
  "p_mar_status","m_dr_change", "p_sep"
)] <- "logreg"

# Categorical / polyreg
method[c(
  "p_ill_quick", "p_relig","p_edu_level","m_home_own_status",
  "p_eval_hlth"
)] <- "polyreg"


method["p_dep" ] <- ""
method["pg_mar"] <- ""
method["pf_mar"] <- ""
method["pd_age"] <- ""
method["pc_age"] <- ""
method["pa_age"] <- ""
method["pb_age"] <- ""
method["pb_anx"] <- ""
method["pc_anx"] <- ""
method["pe_anx"] <- ""
method["pc_soc_supp"] <- ""
method[c("m_ill_quick", "m_dep", "m_age", "m_mar_status", 
         "m_relig", "m_edu_level",
         "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_sep", "m_anx_score", "m_dep_score", "a_car_own")] <- ""



aux_vars <- c(
  "pg_mar", "pf_mar", "pd_age", "pc_age", "pa_age", "pb_age", "pc_soc_supp", "m_ill_quick", "m_dep", "m_age",  
  "m_mar_status", "m_relig", "m_edu_level",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life","m_sep", "m_anx_score", "m_dep_score", "a_car_own"
)


# p_ill_quick
pm_p_ill_dep["p_ill_quick", ] <- 0
pm_p_ill_dep["p_ill_quick", c(
  "p_age", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score"
)] <- 1

# age
pm_p_ill_dep["p_age", ] <- 0
pm_p_ill_dep["p_age", c(
  "p_ill_quick", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "pa_age", 
  "pb_age", "pc_age", "pd_age", "p_dep_score"
)] <- 1

# mar_status
pm_p_ill_dep["p_mar_status", ] <- 0
pm_p_ill_dep["p_mar_status", c(
  "p_ill_quick", "p_age",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score", "pg_mar", "pf_mar"
)] <- 1

# parity
pm_p_ill_dep["m_parity", ] <- 0
pm_p_ill_dep["m_parity", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# home_own_status
pm_p_ill_dep["m_home_own_status", ] <- 0
pm_p_ill_dep["m_home_own_status", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "a_car_own", "m_dep_score"
)] <- 1

# mum_hlth
pm_p_ill_dep["p_eval_hlth", ] <- 0
pm_p_ill_dep["p_eval_hlth", c(
  "p_ill_quick", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "m_home_own_status", "p_dep_score"
)] <- 1

# soc_supp_mis
pm_p_ill_dep["p_soc_supp", ] <- 0
pm_p_ill_dep["p_soc_supp", c(
  "p_ill_quick", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_weigh_life", "m_dr_change", "p_sep", "m_anx_score", "pc_soc_supp",
  "m_home_own_status", "p_dep_score"
)] <- 1

# weigh_life
pm_p_ill_dep["p_weigh_life", ] <- 0
pm_p_ill_dep["p_weigh_life", c(
  "p_ill_quick", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "m_dr_change", "p_sep", "p_anx_score", "m_home_own_status", "p_dep_score"
)] <- 1

# dr_change 
pm_p_ill_dep["m_dr_change", ] <- 0
pm_p_ill_dep["m_dr_change", c(
  "m_ill_quick", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_sep", "m_anx_score", "m_home_own_status", "m_weigh_life", "m_dep_score"
)] <- 1

# sep
pm_p_ill_dep["p_sep", ] <- 0
pm_p_ill_dep["p_sep", c(
  "p_ill_quick", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "p_anx_score", "m_home_own_status",
  "p_weigh_life", "m_dr_change", "p_dep_score"
)] <- 1


# anx_score
pm_p_ill_dep["p_anx_score", ] <- 0
pm_p_ill_dep["p_anx_score", c(
  "p_ill_quick", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "m_home_own_status",
  "p_weigh_life", "p_sep", "m_dr_change", "p_dep_score","pc_anx", "pe_anx"
)] <- 1

# dep_score
pm_p_ill_dep["p_dep_score", ] <- 0
pm_p_ill_dep["p_dep_score", c(
  "p_ill_quick", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "m_home_own_status",
  "p_weigh_life", "p_sep", "m_dr_change", "p_anx_score"
)] <- 1



# Do not predict dep
pm_p_ill_dep["p_dep", ] <- 0

## --- Run multiple imputation ---
imp_dep_p_quick <- mice(
  data = df_p_dep_ill_mi,
  method = method,
  predictorMatrix = pm_p_ill_dep,
  m = 50,
  maxit = 20,
  seed = 123
)

 

  
# saving the imputation in case R crashes
saveRDS(imp_dep_p_quick, (here::here( "6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_dep_p_quick.rds")))


 

# p dep conf
  
# p conf anx
# Binary variables
df_p_dep_conf_mi$p_mar_status <- factor(df_p_dep_conf_mi$p_mar_status)
df_p_dep_conf_mi$m_dr_change  <- factor(df_p_dep_conf_mi$m_dr_change)
df_p_dep_conf_mi$p_sep        <- factor(df_p_dep_conf_mi$p_sep)

# Categorical variables
df_p_dep_conf_mi$p_conf_nhs        <- factor(df_p_dep_conf_mi$p_conf_nhs)
df_p_dep_conf_mi$p_relig           <- factor(df_p_dep_conf_mi$p_relig)
df_p_dep_conf_mi$p_edu_level       <- factor(df_p_dep_conf_mi$p_edu_level)
df_p_dep_conf_mi$m_home_own_status <- factor(df_p_dep_conf_mi$m_home_own_status)
df_p_dep_conf_mi$p_eval_hlth       <- factor(df_p_dep_conf_mi$p_eval_hlth)


#dry run
ini_p_conf_dep <- mice(df_p_dep_conf_mi, maxit = 0, printFlag = FALSE)

method <- ini_p_conf_dep$method
pm_p_conf_dep     <- ini_p_conf_dep$predictorMatrix

# Prediction matrix
## dont predict itself
diag(pm_p_conf_dep) <- 0

#continuous /norm 
method[c(
  "p_age",           
  "m_parity",      
  "p_soc_supp",  
  "p_anx_score",
  "p_weigh_life", 
  "p_dep_score"
)] <- "pmm"

#binary / logreg
method[c(
  "p_mar_status","m_dr_change", "p_sep"
)] <- "logreg"

# Categorical / polyreg
method[c(
  "p_conf_nhs", "p_relig","p_edu_level","m_home_own_status",
  "p_eval_hlth"
)] <- "polyreg"


method["p_dep" ] <- ""
method["pg_mar"] <- ""
method["pf_mar"] <- ""
method["pd_age"] <- ""
method["pc_age"] <- ""
method["pa_age"] <- ""
method["pb_age"] <- ""
method["pc_anx"] <- ""
method["pe_anx"] <- ""
method["pc_soc_supp"] <- ""
method[c("m_conf_nhs", "m_dep", "m_age", "m_mar_status", 
         "m_relig", "m_edu_level",
         "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_sep", "m_anx_score", "m_dep_score", "a_car_own")] <- ""

aux_vars <- c(
  "pg_mar", "pf_mar", "pd_age", "pc_age", "pa_age", "pb_age", "pc_soc_supp", "m_conf_nhs", "m_dep", "m_age",  
  "m_mar_status", "m_relig", "m_edu_level",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_sep", "m_anx_score", "m_dep_score", "a_car_own"
)


# p_conf_nhs
pm_p_conf_dep["p_conf_nhs", ] <- 0
pm_p_conf_dep["p_conf_nhs", c(
  "p_age", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score"
)] <- 1

# age
pm_p_conf_dep["p_age", ] <- 0
pm_p_conf_dep["p_age", c(
  "p_conf_nhs", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "pa_age", 
  "pb_age", "pc_age", "pd_age", "p_dep_score"
)] <- 1

# mar_status
pm_p_conf_dep["p_mar_status", ] <- 0
pm_p_conf_dep["p_mar_status", c(
  "p_conf_nhs", "p_age",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score", "pg_mar", "pf_mar"
)] <- 1

# parity
pm_p_conf_dep["m_parity", ] <- 0
pm_p_conf_dep["m_parity", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# home_own_status
pm_p_conf_dep["m_home_own_status", ] <- 0
pm_p_conf_dep["m_home_own_status", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "a_car_own", "m_dep_score"
)] <- 1

# eval_hlth
pm_p_conf_dep["p_eval_hlth", ] <- 0
pm_p_conf_dep["p_eval_hlth", c(
  "p_conf_nhs", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "m_home_own_status", "p_dep_score"
)] <- 1

# soc_supp
pm_p_conf_dep["p_soc_supp", ] <- 0
pm_p_conf_dep["p_soc_supp", c(
  "p_conf_nhs",  "p_age",
  "p_relig",     "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score",
  "pc_soc_supp", "m_home_own_status", "p_dep_score"
)] <- 1

# weigh_life
pm_p_conf_dep["p_weigh_life", ] <- 0
pm_p_conf_dep["p_weigh_life", c(
  "p_conf_nhs", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "m_dr_change", "p_sep", "p_anx_score", "m_home_own_status", "p_dep_score"
)] <- 1

# dr_change
pm_p_conf_dep["m_dr_change", ] <- 0
pm_p_conf_dep["m_dr_change", c(
  "m_conf_nhs", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_sep", "m_anx_score", "m_home_own_status", "m_weigh_life", "m_dep_score"
)] <- 1

# soc_class
pm_p_conf_dep["p_sep", ] <- 0
pm_p_conf_dep["p_sep", c(
  "p_conf_nhs", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "p_anx_score", "m_home_own_status",
  "p_weigh_life", "m_dr_change", "p_dep_score"
)] <- 1


# anx_score
pm_p_conf_dep["p_anx_score", ] <- 0
pm_p_conf_dep["p_anx_score", c(
  "p_conf_nhs", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "m_home_own_status",
  "p_weigh_life", "p_sep", "m_dr_change", "p_dep_score", "pe_anx", "pc_anx"
)] <- 1

# dep_score
pm_p_conf_dep["p_dep_score", ] <- 0
pm_p_conf_dep["p_dep_score", c(
  "p_conf_nhs", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "m_home_own_status",
  "p_weigh_life", "p_sep", "m_dr_change", "p_anx_score"
)] <- 1

# Do not predict anx
pm_p_conf_dep["p_dep", ] <- 0

## --- Run multiple imputation ---
imp_dep_p_conf <- mice(
  data = df_p_dep_conf_mi,
  method = method,
  predictorMatrix = pm_p_conf_dep,
  m = 50,
  maxit = 20,
  seed = 123
)



 

  
# saving the imputation in case R crashes
saveRDS(imp_dep_p_conf, (here::here( "6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_dep_p_conf.rds")))

 

# p dep help
# m help anx
# Binary variables
df_p_dep_help_mi$m_dr_change  <- factor(df_p_dep_help_mi$m_dr_change)
df_p_dep_help_mi$p_mar_status <- factor(df_p_dep_help_mi$p_mar_status)
df_p_dep_help_mi$p_sep        <- factor(df_p_dep_help_mi$p_sep)

# Categorical variables
df_p_dep_help_mi$p_alwys_help      <- factor(df_p_dep_help_mi$p_alwys_help)
df_p_dep_help_mi$p_relig           <- factor(df_p_dep_help_mi$p_relig)
df_p_dep_help_mi$p_edu_level       <- factor(df_p_dep_help_mi$p_edu_level)
df_p_dep_help_mi$m_home_own_status <- factor(df_p_dep_help_mi$m_home_own_status)
df_p_dep_help_mi$p_eval_hlth       <- factor(df_p_dep_help_mi$p_eval_hlth)



#dry run
ini_p_help_dep <- mice(df_p_dep_help_mi, maxit = 0, printFlag = FALSE)

method <- ini_p_help_dep$method
pm_p_help_dep     <- ini_p_help_dep$predictorMatrix

# Prediction matrix
## dont predict itself
diag(pm_p_help_dep) <- 0

#continuous /norm 
#continuous /norm 
method[c(
  "p_age",           
  "m_parity",      
  "p_soc_supp",  
  "p_anx_score",
  "p_weigh_life",
  "p_dep_score"
)] <- "pmm"

#binary / logreg
method[c(
  "p_mar_status","m_dr_change", "p_sep"
)] <- "logreg"

# Categorical / polyreg
method[c(
  "p_alwys_help", "p_relig","p_edu_level","m_home_own_status",
  "p_eval_hlth"
)] <- "polyreg"


method["p_dep" ] <- ""
method["pg_mar"] <- ""
method["pf_mar"] <- ""
method["pd_age"] <- ""
method["pc_age"] <- ""
method["pa_age"] <- ""
method["pb_age"] <- ""
method["pc_anx"] <- ""
method["pe_anx"] <- ""
method["pc_soc_supp"] <- ""
method[c("m_alwys_help", "m_dep", "m_age", "m_mar_status", 
         "m_relig", "m_edu_level",
         "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_sep", "m_anx_score", "m_dep_score", "a_car_own")] <- ""


aux_vars <- c(
  "pg_mar", "pf_mar", "pd_age", "pc_age", "pa_age", "pb_age", "pc_soc_supp", "m_conf_nhs", "m_dep", "m_age",  
  "m_mar_status", "m_relig", "m_edu_level",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_sep", "m_anx_score", "m_dep_score", "a_car_own"
)



# p_ill_quick
pm_p_help_dep["p_alwys_help", ] <- 0
pm_p_help_dep["p_alwys_help", c(
  "p_age",       "p_mar_status",
  "p_relig",     "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp",  "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score"
)] <- 1

# age
pm_p_help_dep["p_age", ] <- 0
pm_p_help_dep["p_age", c(
  "p_alwys_help", "p_mar_status",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "pa_age", 
  "pb_age", "pc_age", "pd_age", "p_dep_score"
)] <- 1

# mar_status
pm_p_help_dep["p_mar_status", ] <- 0
pm_p_help_dep["p_mar_status", c(
  "p_alwys_help", "p_age",
  "p_relig", "p_edu_level", "m_parity", "m_home_own_status",
  "p_eval_hlth", "p_soc_supp", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "p_dep_score", "pg_mar", "pf_mar"
)] <- 1

# parity
pm_p_help_dep["m_parity", ] <- 0
pm_p_help_dep["m_parity", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_home_own_status",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "m_dep_score"
)] <- 1

# home_own_status
pm_p_help_dep["m_home_own_status", ] <- 0
pm_p_help_dep["m_home_own_status", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_weigh_life", "m_dr_change", "m_sep", "m_anx_score", "a_car_own", "m_dep_score"
)] <- 1

# mum_hlth
pm_p_help_dep["p_eval_hlth", ] <- 0
pm_p_help_dep["p_eval_hlth", c(
  "p_alwys_help", "p_age",
  "p_relig",      "p_edu_level",  "p_mar_status", "m_parity",
  "p_soc_supp",   "m_weigh_life", "m_dr_change", "p_sep", "p_anx_score", "m_home_own_status", "p_dep_score"
)] <- 1

# soc_supp
pm_p_help_dep["m_soc_supp", ] <- 0
pm_p_help_dep["m_soc_supp", c(
  "p_alwys_help", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_weigh_life", "m_dr_change", "p_sep", "p_anx_score",
  "pc_soc_supp", "m_home_own_status", "p_dep_score"
)] <- 1

# weigh_life
pm_p_help_dep["p_weigh_life", ] <- 0
pm_p_help_dep["p_weigh_life", c(
  "p_alwys_help", "p_age",
  "p_relig", "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth", "p_soc_supp", "m_dr_change", "p_sep", "p_anx_score", "m_home_own_status", "p_dep_score"
)] <- 1

# dr_change
pm_p_help_dep["m_dr_change", ] <- 0
pm_p_help_dep["m_dr_change", c(
  "m_alwys_help", "m_age",
  "m_relig", "m_edu_level", "m_mar_status", "m_parity",
  "m_eval_hlth", "m_soc_supp", "m_sep", "m_anx_score", "m_home_own_status", "m_weigh_life", "m_dep_score"
)] <- 1

# soc_class
pm_p_help_dep["p_sep", ] <- 0
pm_p_help_dep["p_sep", c(
  "p_alwys_help", "p_age",
  "p_relig",      "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth",  "p_soc_supp",  "p_anx_score", "m_home_own_status",
  "p_weigh_life", "m_dr_change", "p_dep_score"
)] <- 1


# anx_score
pm_p_help_dep["m_anx_score", ] <- 0
pm_p_help_dep["m_anx_score", c(
  "p_alwys_help", "p_age",
  "p_relig",      "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth",  "p_soc_supp", "m_home_own_status",
  "p_weigh_life", "p_sep", "m_dr_change", "p_dep_score", "pc_anx", "pe_anx"
)] <- 1

# dep_score
pm_p_help_dep["p_dep_score", ] <- 0
pm_p_help_dep["p_dep_score", c(
  "p_alwys_help", "p_age",
  "p_relig",      "p_edu_level", "p_mar_status", "m_parity",
  "p_eval_hlth",  "p_soc_supp",  "m_home_own_status",
  "p_weigh_life", "p_sep",       "m_dr_change", "p_anx_score"
)] <- 1

# Do not predict dep
pm_p_help_dep["p_dep", ] <- 0

## --- Run multiple imputation ---
imp_dep_p_help <- mice(
  data = df_p_dep_help_mi,
  method = method,
  predictorMatrix = pm_p_help_dep,
  m = 50,
  maxit = 20,
  seed = 123
)


 

  
# saving the imputation in case R crashes

saveRDS(imp_dep_p_help, (here::here( "6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_dep_p_help.rds")))


 




# Regression models  
#loading the imputations back into RStudio

imp_anx_m_quick <- readRDS(here::here ("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_anx_m_quick.rds")
imp_anx_m_conf  <- readRDS(here::here ("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_anx_m_conf.rds") 
imp_anx_m_help  <- readRDS(here::here ("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_anx_m_help.rds") 
imp_dep_m_quick <- readRDS(here::here ("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_dep_m_quick.rds")
imp_dep_m_conf  <- readRDS(here::here ("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_dep_m_conf.rds") 
imp_dep_m_help  <- readRDS(here::here ("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_dep_m_help.rds") 
imp_anx_p_quick <- readRDS(here::here ("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_anx_p_quick.rds")
imp_anx_p_conf  <- readRDS(here::here ("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_anx_p_conf.rds") 
imp_anx_p_help  <- readRDS(here::here ("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_anx_p_help.rds") 
imp_dep_p_quick <- readRDS(here::here ("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_dep_p_quick.rds")
imp_dep_p_conf  <- readRDS(here::here ("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_dep_p_conf.rds") 
imp_dep_p_help  <- readRDS(here::here ("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "imp_dep_p_help.rds") 

 



#-------------------------------------------------------------------------------  
# unadjusted regression models
# mother 

m_anx_reg_unad_tab_conf   <- with(imp_anx_m_conf, glm(m_anx ~  as.numeric(m_conf_nhs), family = binomial()))
m_anx_reg_unad_tab_quick  <- with(imp_anx_m_quick, glm(m_anx ~ as.numeric(m_ill_quick), family = binomial()))
m_anx_reg_unad_tab_help   <- with(imp_anx_m_help, glm(m_anx ~  as.numeric(m_alwys_help), family = binomial()))

pooled_m_anx_reg_unad_tab_conf  <- pool(m_anx_reg_unad_tab_conf  )
pooled_m_anx_reg_unad_tab_quick <- pool(m_anx_reg_unad_tab_quick )
pooled_m_anx_reg_unad_tab_help  <- pool(m_anx_reg_unad_tab_help  )

summary(pooled_m_anx_reg_unad_tab_conf , exponentiate = TRUE)
summary(pooled_m_anx_reg_unad_tab_quick, exponentiate = TRUE)
summary(pooled_m_anx_reg_unad_tab_help , exponentiate = TRUE)

m_dep_reg_unad_tab_conf   <- with(imp_dep_m_conf, glm(m_dep ~  as.numeric(m_conf_nhs), family = binomial()))
m_dep_reg_unad_tab_quick  <- with(imp_dep_m_quick, glm(m_dep ~ as.numeric(m_ill_quick), family = binomial()))
m_dep_reg_unad_tab_help   <- with(imp_dep_m_help, glm(m_dep ~  as.numeric(m_alwys_help), family = binomial()))

pooled_m_dep_reg_unad_tab_conf  <- pool(m_dep_reg_unad_tab_conf  )
pooled_m_dep_reg_unad_tab_quick <- pool(m_dep_reg_unad_tab_quick )
pooled_m_dep_reg_unad_tab_help  <- pool(m_dep_reg_unad_tab_help  )

summary(pooled_m_dep_reg_unad_tab_conf , exponentiate = TRUE)
summary(pooled_m_dep_reg_unad_tab_quick, exponentiate = TRUE)
summary(pooled_m_dep_reg_unad_tab_help , exponentiate = TRUE)

#partners
p_anx_reg_unad_tab_conf   <- with(imp_anx_p_conf, glm(p_anx ~  as.numeric(p_conf_nhs), family = binomial()))
p_anx_reg_unad_tab_quick  <- with(imp_anx_p_quick, glm(p_anx ~ as.numeric(p_ill_quick), family = binomial()))
p_anx_reg_unad_tab_help   <- with(imp_anx_p_help, glm(p_anx ~  as.numeric(p_alwys_help), family = binomial()))

pooled_p_anx_reg_unad_tab_conf  <- pool(p_anx_reg_unad_tab_conf  )
pooled_p_anx_reg_unad_tab_quick <- pool(p_anx_reg_unad_tab_quick )
pooled_p_anx_reg_unad_tab_help  <- pool(p_anx_reg_unad_tab_help  )

summary(pooled_p_anx_reg_unad_tab_conf , exponentiate = TRUE)
summary(pooled_p_anx_reg_unad_tab_quick, exponentiate = TRUE)
summary(pooled_p_anx_reg_unad_tab_help , exponentiate = TRUE)

p_dep_reg_unad_tab_conf   <- with(imp_dep_p_conf, glm(p_dep ~  as.numeric(p_conf_nhs), family = binomial()))
p_dep_reg_unad_tab_quick  <- with(imp_dep_p_quick, glm(p_dep ~ as.numeric(p_ill_quick), family = binomial()))
p_dep_reg_unad_tab_help   <- with(imp_dep_p_help, glm(p_dep ~  as.numeric(p_alwys_help), family = binomial()))

pooled_p_dep_reg_unad_tab_conf  <- pool(p_dep_reg_unad_tab_conf  )
pooled_p_dep_reg_unad_tab_quick <- pool(p_dep_reg_unad_tab_quick )
pooled_p_dep_reg_unad_tab_help  <- pool(p_dep_reg_unad_tab_help  )

summary(pooled_p_dep_reg_unad_tab_conf , exponentiate = TRUE)
summary(pooled_p_dep_reg_unad_tab_quick, exponentiate = TRUE)
summary(pooled_p_dep_reg_unad_tab_help , exponentiate = TRUE)

# unadjusted odds tables with N values
m_anx_table_quick <- tbl_regression(
  m_anx_reg_unad_tab_quick, 
  exponentiate = TRUE, 
  label = list("as.numeric(m_ill_quick)" ~ 
                 "I know that if my child was very ill my doctor would come quickly"))

print(m_anx_table_quick)


m_anx_table_conf <- tbl_regression(
  m_anx_reg_unad_tab_conf,
  exponentiate = TRUE, 
  label = list("as.numeric(m_conf_nhs)" ~ "I don’t have any confidence in the national health service"))

print(m_anx_table_conf)


m_anx_table_help <- tbl_regression(
  m_anx_reg_unad_tab_help, 
  exponentiate = TRUE, 
  label = list("as.numeric(m_alwys_help)" ~ 
                 "The doctor in the clinic is always helpful"))

print(m_anx_table_help)

# depression
m_dep_table_quick <- tbl_regression(
  m_dep_reg_unad_tab_quick, 
  exponentiate = TRUE, 
  label = list("as.numeric(m_ill_quick)" ~ 
                 "I know that if my child was very ill my doctor would come quickly"))

print(m_dep_table_quick)


m_dep_table_conf <- tbl_regression(
  m_dep_reg_unad_tab_conf,
  exponentiate = TRUE, 
  label = list("as.numeric(m_conf_nhs)" ~ "I don’t have any confidence in the national health service"))

print(m_dep_table_conf)


m_dep_table_help <- tbl_regression(
  m_dep_reg_unad_tab_help, 
  exponentiate = TRUE, 
  label = list("as.numeric(m_alwys_help)" ~ 
                 "The doctor in the clinic is always helpful"))

print(m_dep_table_help)


# combined
unad_table_m_anx <- tbl_stack(
  list(m_anx_table_conf, m_anx_table_quick, m_anx_table_help)
) %>%
  modify_caption(glue::glue("**Table 1.** Unadjusted Odds Ratios for Anxiety against all exposures"))

print(unad_table_m_anx)

unad_table_m_dep <- tbl_stack(
  list(m_dep_table_conf, m_dep_table_quick, m_dep_table_help)
) %>%
  modify_caption(glue::glue("**Table 1.** Unadjusted Odds Ratios for Depression against all exposures"))

print(unad_table_m_dep)


# partner
# unadjusted odds tables
p_anx_table_quick <- tbl_regression(
  p_anx_reg_unad_tab_quick, 
  exponentiate = TRUE, 
  label = list("as.numeric(p_ill_quick)" ~ 
                 "I know that if my child was very ill my doctor would come quickly"))

print(p_anx_table_quick)


p_anx_table_conf <- tbl_regression(
  p_anx_reg_unad_tab_conf,
  exponentiate = TRUE, 
  label = list("as.numeric(p_conf_nhs)" ~ "I don’t have any confidence in the national health service"))

print(p_anx_table_conf)


p_anx_table_help <- tbl_regression(
  p_anx_reg_unad_tab_help, 
  exponentiate = TRUE, 
  label = list("as.numeric(p_alwys_help)" ~ 
                 "The doctor in the clinic is always helpful"))

print(p_anx_table_help)

# depression
p_dep_table_quick <- tbl_regression(
  p_dep_reg_unad_tab_quick, 
  exponentiate = TRUE, 
  label = list("as.numeric(p_ill_quick)" ~ 
                 "I know that if my child was very ill my doctor would come quickly"))

print(p_dep_table_quick)


p_dep_table_conf <- tbl_regression(
  p_dep_reg_unad_tab_conf,
  exponentiate = TRUE, 
  label = list("as.numeric(p_conf_nhs)" ~ "I don’t have any confidence in the national health service"))

print(p_dep_table_conf)


p_dep_table_help <- tbl_regression(
  p_dep_reg_unad_tab_help, 
  exponentiate = TRUE, 
  label = list("as.numeric(p_alwys_help)" ~ 
                 "The doctor in the clinic is always helpful"))

print(p_dep_table_help)

# combined
unad_table_p_anx <- tbl_stack(
  list(p_anx_table_conf, p_anx_table_quick, p_anx_table_help)
) %>%
  modify_caption(glue::glue("**Table 1.** Unadjusted Odds Ratios for Anxiety against all exposures"))

print(unad_table_p_anx)

unad_table_p_dep <- tbl_stack(
  list(p_dep_table_conf, p_dep_table_quick, p_dep_table_help)
) %>%
  modify_caption(glue::glue("**Table 1.** Unadjusted Odds Ratios for Depression against all exposures"))

print(unad_table_p_dep)


 

#-------------------------------------------------------------------------------  
# adjusted model 1 
# mother no confidence

m_anx_reg_adj_1_conf <- with(
  imp_anx_m_conf,
  glm(
    m_anx ~ as.numeric(m_conf_nhs) + m_age + m_parity + m_mar_status +
      m_relig + m_sep + m_edu_level + m_home_own_status + m_anx_score + m_dep_score,
    family = binomial(link = "logit")
  ))

print(m_anx_reg_adj_1_conf)

pooled_m_anx_reg_adj_1_conf <- pool(m_anx_reg_adj_1_conf)

summary(pooled_m_anx_reg_adj_1_conf, exponentiate = TRUE)

m_adj_anx_table_1_conf <- tbl_regression(m_anx_reg_adj_1_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(m_conf_nhs)" ~ "I don’t have any confidence in the national health service",
                                           m_age ~ "Age",
                                           m_parity ~ "Parity",
                                           m_mar_status ~ "Marital status",
                                           m_relig ~ "Religion",
                                           m_sep ~ "Socioeconomic status",
                                           m_edu_level ~ "Educational attainment",
                                           m_home_own_status ~ "Home ownership",
                                           m_anx_score ~ "Crown-crisp anxiety score",
                                           m_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_1_conf)

# mother quick

m_anx_reg_adj_1_quick <- with(
  imp_anx_m_quick,
  glm(m_anx ~ as.numeric(m_ill_quick) + m_age + m_parity + m_mar_status +
        m_relig + m_sep + m_edu_level + m_home_own_status + m_anx_score + m_dep_score,
      family = binomial(link = "logit")
  ))

print(m_anx_reg_adj_1_quick)

pooled_m_anx_reg_adj_1_quick <- pool(m_anx_reg_adj_1_quick)

summary(pooled_m_anx_reg_adj_1_quick, exponentiate = TRUE)

m_adj_anx_table_1_quick <- tbl_regression(m_anx_reg_adj_1_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            "as.numeric(m_ill_quick)" ~ 
                                              "I know that if my child was very ill my doctor would come quickly",
                                            m_age ~ "Mother's age",
                                            m_parity ~ "Mother's parity",
                                            m_mar_status ~ "Mother's marital status",
                                            m_relig ~ "Mother's religion",
                                            m_sep ~ "Mother's socioeconomic status",
                                            m_edu_level ~ "Mother's highest educational attainment",
                                            m_home_own_status ~ "Mother's home ownership",
                                            m_anx_score ~ "Crown-crisp anxiety score",
                                            m_dep_score ~ "Crown-crisp depression score"
                                          )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_1_quick)

# always helpful

m_anx_reg_adj_1_help <- with(
  imp_anx_m_help,
  glm(m_anx ~ as.numeric(m_alwys_help) + m_age + m_parity + m_mar_status +
        m_relig + m_sep + m_edu_level + m_home_own_status + m_anx_score + m_dep_score,
      family = binomial(link = "logit")
  ))

print(m_anx_reg_adj_1_help)

pooled_m_anx_reg_adj_1_help <- pool(m_anx_reg_adj_1_help)

summary(pooled_m_anx_reg_adj_1_help, exponentiate = TRUE)

m_adj_anx_table_1_help <- tbl_regression(m_anx_reg_adj_1_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(m_alwys_help)" ~
                                             "The doctor in the clinic is always helpful",
                                           m_age ~ "Mother's age",
                                           m_parity ~ "Mother's parity",
                                           m_mar_status ~ "Mother's marital status",
                                           m_relig ~ "Mother's religion",
                                           m_sep ~ "Mother's socioeconomic status",
                                           m_edu_level ~ "Mother's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_anx_score ~ "Crown-crisp anxiety score",
                                           m_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_1_help)

# mother depression

m_dep_reg_adj_1_conf <- with(
  imp_dep_m_conf,
  glm(m_dep ~ as.numeric(m_conf_nhs) + m_age + m_parity + m_mar_status + 
        m_relig + m_sep + m_edu_level + m_home_own_status + m_anx_score + m_dep_score,
      family = binomial(link = "logit")
  ))

print(m_dep_reg_adj_1_conf)

pooled_m_dep_reg_adj_1_conf <- pool(m_dep_reg_adj_1_conf)

summary(pooled_m_dep_reg_adj_1_conf, exponentiate = TRUE)

m_adj_dep_table_1_conf <- tbl_regression(m_dep_reg_adj_1_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(m_conf_nhs)" ~ 
                                             "I don’t have any confidence in the national health service",
                                           m_age ~ "Mother's age",
                                           m_parity ~ "Mother's parity",
                                           m_mar_status ~ "Mother's marital status",
                                           m_relig ~ "Mother's religion",
                                           m_sep ~ "Mother's socioeconomic status",
                                           m_edu_level ~ "Mother's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_anx_score ~ "Crown-crisp anxiety score",
                                           m_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_1_conf)

# mother quick

m_dep_reg_adj_1_quick <- with(
  imp_dep_m_quick,
  glm(m_dep ~ as.numeric(m_ill_quick) + m_age + m_parity + m_mar_status +
        m_relig + m_sep + m_edu_level + m_home_own_status + m_anx_score + m_dep_score,
      family = binomial(link = "logit")
  ))

print(m_dep_reg_adj_1_quick)

pooled_m_dep_reg_adj_1_quick <- pool(m_dep_reg_adj_1_quick)

summary(pooled_m_dep_reg_adj_1_quick, exponentiate = TRUE)


m_adj_dep_table_1_quick <- tbl_regression(m_dep_reg_adj_1_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            "as.numeric(m_ill_quick)" ~ 
                                              "I know that if my child was very ill my doctor would come quickly",
                                            m_age ~ "Mother's age",
                                            m_parity ~ "Mother's parity",
                                            m_mar_status ~ "Mother's marital status",
                                            m_relig ~ "Mother's religion",
                                            m_sep ~ "Mother's socioeconomic status",
                                            m_edu_level ~ "Mother's highest educational attainment",
                                            m_home_own_status ~ "Mother's home ownership",
                                            m_anx_score ~ "Crown-crisp anxiety score",
                                            m_dep_score ~ "Crown-crisp depression score"
                                          )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_1_quick)

# always helpful

m_dep_reg_adj_1_help <- with(
  imp_dep_m_help,
  glm(m_dep ~ as.numeric(m_alwys_help) + m_age + m_parity + m_mar_status +
        m_relig + m_sep + m_edu_level + m_home_own_status + m_anx_score + m_dep_score,
      family = binomial(link = "logit")
  ))

print(m_dep_reg_adj_1_help)

pooled_m_dep_reg_adj_1_help <- pool(m_dep_reg_adj_1_help)

summary(pooled_m_dep_reg_adj_1_help, exponentiate = TRUE)


m_adj_dep_table_1_help <- tbl_regression(m_dep_reg_adj_1_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(m_alwys_help)" ~ 
                                             "The doctor in the clinic is always helpful",
                                           m_age ~ "Mother's age",
                                           m_parity ~ "Mother's parity",
                                           m_mar_status ~ "Mother's marital status",
                                           m_relig ~ "Mother's religion",
                                           m_sep ~ "Mother's socioeconomic status",
                                           m_edu_level ~ "Mother's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_anx_score ~ "Crown-crisp anxiety score",
                                           m_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_1_help)

#-------------------------------------------------------------------------------
# partner adjusted model 1
# adjusted model 1 
# no confidence
p_anx_reg_adj_1_conf <- with(
  imp_anx_p_conf,
  glm(
    p_anx ~ as.numeric(p_conf_nhs) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + p_anx_score + p_dep_score,
    family = binomial(link = "logit")
  ))

print(p_anx_reg_adj_1_conf)

pooled_p_anx_reg_adj_1_conf <- pool(p_anx_reg_adj_1_conf)

summary(pooled_p_anx_reg_adj_1_conf, exponentiate = TRUE)

p_adj_anx_table_1_conf <- tbl_regression(p_anx_reg_adj_1_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(p_conf_nhs)" ~ 
                                             "I don’t have any confidence in the national health service",
                                           p_age ~ "Partner's age",
                                           m_parity ~ "Mother's parity",
                                           p_mar_status ~ "Partners's marital status",
                                           p_relig ~ "Partner's religion",
                                           p_sep ~ "Partner's socioeconomic status",
                                           p_edu_level ~ "Partner's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           p_anx_score ~ "Crown-crisp anxiety score",
                                           p_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_1_conf)

# partner quick
p_anx_reg_adj_1_quick <- with(
  imp_anx_p_quick,
  glm(
    p_anx ~ as.numeric(p_ill_quick) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + p_anx_score + p_dep_score,
    family = binomial(link = "logit")
  ))

print(p_anx_reg_adj_1_quick)

pooled_p_anx_reg_adj_1_quick <- pool(p_anx_reg_adj_1_quick)

summary(pooled_p_anx_reg_adj_1_quick, exponentiate = TRUE)

p_adj_anx_table_1_quick <- tbl_regression(p_anx_reg_adj_1_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            "as.numeric(p_ill_quick)" ~ 
                                              "I know that if my child was very ill my doctor would come quickly",
                                            p_age ~       "Partner's age",
                                            m_parity ~    "Mother's parity",
                                            p_mar_status ~"Partner's marital status",
                                            p_relig ~     "Partner's religion",
                                            p_sep ~       "Partner's socioeconomic status",
                                            p_edu_level ~ "Partner's highest educational attainment",
                                            m_home_own_status ~ "Mother's home ownership",
                                            p_anx_score ~ "Crown-crisp anxiety score",
                                            p_dep_score ~ "Crown-crisp depression score"
                                          )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_1_quick)

# always helpful

p_anx_reg_adj_1_help <- with(
  imp_anx_p_help,
  glm(
    p_anx ~ as.numeric(p_alwys_help) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + p_anx_score + p_dep_score,
    family = binomial(link = "logit")
  ))

print(p_anx_reg_adj_1_help)

pooled_p_anx_reg_adj_1_help <- pool(p_anx_reg_adj_1_help)

summary(pooled_p_anx_reg_adj_1_help, exponentiate = TRUE)

p_adj_anx_table_1_help <- tbl_regression(p_anx_reg_adj_1_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(p_alwys_help)" ~ 
                                             "The doctor in the clinic is always helpful",
                                           p_age ~        "Partner's age",
                                           m_parity ~     "Mother's parity",
                                           p_mar_status ~ "Partner's marital status",
                                           p_relig ~      "Partner's religion",
                                           p_sep ~        "Partner's socioeconomic status",
                                           p_edu_level ~  "Partner's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           p_anx_score ~ "Crown-crisp anxiety score",
                                           p_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_1_help)

# partner depression
p_dep_reg_adj_1_conf <- with(
  imp_dep_p_conf,
  glm(
    p_dep ~ as.numeric(p_conf_nhs) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + p_anx_score + p_dep_score,
    family = binomial(link = "logit")
  ))

print(p_dep_reg_adj_1_conf)

pooled_p_dep_reg_adj_1_conf <- pool(p_dep_reg_adj_1_conf)

summary(pooled_p_dep_reg_adj_1_conf, exponentiate = TRUE)

p_adj_dep_table_1_conf <- tbl_regression(p_dep_reg_adj_1_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(p_conf_nhs)" ~   
                                             "I don’t have any confidence in the national health service",
                                           p_age ~        "Partner's age",
                                           m_parity ~     "Mother's parity",
                                           p_mar_status ~ "Partner's marital status",
                                           p_relig ~      "Partner's religion",
                                           p_sep ~        "Partner's socioeconomic status",
                                           p_edu_level ~  "Partner's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           p_anx_score ~ "Crown-crisp anxiety score",
                                           p_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_1_conf)

# quick
p_dep_reg_adj_1_quick <- with(
  imp_dep_p_quick,
  glm(
    p_dep ~ as.numeric(p_ill_quick) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + p_anx_score + p_dep_score,
    family = binomial(link = "logit")
  ))

print(p_dep_reg_adj_1_quick)

pooled_p_dep_reg_adj_1_quick <- pool(p_dep_reg_adj_1_quick)

summary(pooled_p_dep_reg_adj_1_quick, exponentiate = TRUE)

p_adj_dep_table_1_quick <- tbl_regression(p_dep_reg_adj_1_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            "as.numeric(p_ill_quick)" ~  
                                              "I know that if my child was very ill my doctor would come quickly",
                                            p_age ~        "Partner's age",
                                            m_parity ~     "Mother's parity",
                                            p_mar_status ~ "Partner's marital status",
                                            p_relig ~      "Partner's religion",
                                            p_sep ~        "Partner's socioeconomic status",
                                            p_edu_level ~  "Partner's highest educational attainment",
                                            m_home_own_status ~ "Mother's home ownership",
                                            p_anx_score ~ "Crown-crisp anxiety score",
                                            p_dep_score ~ "Crown-crisp depression score"
                                          )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_1_quick)

# always helpful

p_dep_reg_adj_1_help <- with(
  imp_dep_p_help,
  glm(
    p_dep ~ as.numeric(p_alwys_help) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + p_anx_score + p_dep_score,
    data = df_working,
    family = binomial(link = "logit")
  ))

print(p_dep_reg_adj_1_help)

pooled_p_dep_reg_adj_1_quick <- pool(p_dep_reg_adj_1_quick)

summary(pooled_p_dep_reg_adj_1_quick, exponentiate = TRUE)

p_adj_dep_table_1_help <- tbl_regression(p_dep_reg_adj_1_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(p_alwys_help)" ~  
                                             "The doctor in the clinic is always helpful",
                                           p_age ~         "Partner's age",
                                           m_parity ~      "Mother's parity",
                                           p_mar_status ~  "Partner's marital status",
                                           p_relig ~       "Partner's religion",
                                           p_sep ~         "Partner's socioeconomic status",
                                           p_edu_level ~   "Partner's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           p_anx_score ~ "Crown-crisp anxiety score",
                                           p_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_1_help)


#-------------------------------------------------------------------------------
# adjusted model 2 
# mother no confidence

m_anx_reg_adj_2_conf <- with(
  imp_anx_m_conf,
  glm(
    m_anx ~ as.numeric(m_conf_nhs) + m_age + m_parity + m_mar_status +
      m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth + m_anx_score + m_dep_score,
    family = binomial(link = "logit")
  ))

print(m_anx_reg_adj_2_conf)

pooled_m_anx_reg_adj_2_conf <- pool(m_anx_reg_adj_2_conf)

summary(pooled_m_anx_reg_adj_2_conf, exponentiate = TRUE)

m_adj_anx_table_2_conf <- tbl_regression(m_anx_reg_adj_2_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(m_conf_nhs)" ~ "I don’t have any confidence in the national health service",
                                           m_age ~ "Age",
                                           m_parity ~ "Parity",
                                           m_mar_status ~ "Marital status",
                                           m_relig ~ "Religion",
                                           m_sep ~ "Socioeconomic status",
                                           m_edu_level ~ "Educational attainment",
                                           m_home_own_status ~ "Home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           m_eval_hlth ~ "Mother's evaluation of own health",
                                           m_anx_score ~ "Crown-crisp anxiety score",
                                           m_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_2_conf)

# mother quick

m_anx_reg_adj_2_quick <- with(
  imp_anx_m_quick,
  glm(m_anx ~ as.numeric(m_ill_quick) + m_age + m_parity + m_mar_status +
        m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth + m_anx_score + m_dep_score,
      family = binomial(link = "logit")
  ))

print(m_anx_reg_adj_2_quick)

pooled_m_anx_reg_adj_2_quick <- pool(m_anx_reg_adj_2_quick)

summary(pooled_m_anx_reg_adj_2_quick, exponentiate = TRUE)

m_adj_anx_table_2_quick <- tbl_regression(m_anx_reg_adj_2_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            "as.numeric(m_ill_quick)" ~ 
                                              "I know that if my child was very ill my doctor would come quickly",
                                            m_age ~ "Mother's age",
                                            m_parity ~ "Mother's parity",
                                            m_mar_status ~ "Mother's marital status",
                                            m_relig ~ "Mother's religion",
                                            m_sep ~ "Mother's socioeconomic status",
                                            m_edu_level ~ "Mother's highest educational attainment",
                                            m_home_own_status ~ "Mother's home ownership",
                                            m_dr_change ~ "Recent doctor change",
                                            m_eval_hlth ~ "Mother's evaluation of own health",
                                            m_anx_score ~ "Crown-crisp anxiety score",
                                            m_dep_score ~ "Crown-crisp depression score"
                                          )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_2_quick)

# always helpful

m_anx_reg_adj_2_help <- with(
  imp_anx_m_help,
  glm(m_anx ~ as.numeric(m_alwys_help) + m_age + m_parity + m_mar_status +
        m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth + m_anx_score + m_dep_score,
      family = binomial(link = "logit")
  ))

print(m_anx_reg_adj_2_help)

pooled_m_anx_reg_adj_2_help <- pool(m_anx_reg_adj_2_help)

summary(pooled_m_anx_reg_adj_2_help, exponentiate = TRUE)

m_adj_anx_table_2_help <- tbl_regression(m_anx_reg_adj_2_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(m_alwys_help)" ~
                                             "The doctor in the clinic is always helpful",
                                           m_age ~ "Mother's age",
                                           m_parity ~ "Mother's parity",
                                           m_mar_status ~ "Mother's marital status",
                                           m_relig ~ "Mother's religion",
                                           m_sep ~ "Mother's socioeconomic status",
                                           m_edu_level ~ "Mother's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           m_eval_hlth ~ "Mother's evaluation of own health",
                                           m_anx_score ~ "Crown-crisp anxiety score",
                                           m_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_2_help)

# mother depression

m_dep_reg_adj_2_conf <- with(
  imp_dep_m_conf,
  glm(m_dep ~ as.numeric(m_conf_nhs) + m_age + m_parity + m_mar_status + 
        m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth + m_anx_score + m_dep_score,
      family = binomial(link = "logit")
  ))

print(m_dep_reg_adj_2_conf)

pooled_m_dep_reg_adj_2_conf <- pool(m_dep_reg_adj_2_conf)

summary(pooled_m_dep_reg_adj_2_conf, exponentiate = TRUE)

m_adj_dep_table_2_conf <- tbl_regression(m_dep_reg_adj_2_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(m_conf_nhs)" ~ 
                                             "I don’t have any confidence in the national health service",
                                           m_age ~ "Mother's age",
                                           m_parity ~ "Mother's parity",
                                           m_mar_status ~ "Mother's marital status",
                                           m_relig ~ "Mother's religion",
                                           m_sep ~ "Mother's socioeconomic status",
                                           m_edu_level ~ "Mother's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           m_eval_hlth ~ "Mother's evaluation of own health",
                                           m_anx_score ~ "Crown-crisp anxiety score",
                                           m_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_2_conf)

# mother quick

m_dep_reg_adj_2_quick <- with(
  imp_dep_m_quick,
  glm(m_dep ~ as.numeric(m_ill_quick) + m_age + m_parity + m_mar_status +
        m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth + m_anx_score + m_dep_score,
      family = binomial(link = "logit")
  ))

print(m_dep_reg_adj_2_quick)

pooled_m_dep_reg_adj_2_quick <- pool(m_dep_reg_adj_2_quick)

summary(pooled_m_dep_reg_adj_2_quick, exponentiate = TRUE)


m_adj_dep_table_2_quick <- tbl_regression(m_dep_reg_adj_2_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            "as.numeric(m_ill_quick)" ~ 
                                              "I know that if my child was very ill my doctor would come quickly",
                                            m_age ~ "Mother's age",
                                            m_parity ~ "Mother's parity",
                                            m_mar_status ~ "Mother's marital status",
                                            m_relig ~ "Mother's religion",
                                            m_sep ~ "Mother's socioeconomic status",
                                            m_edu_level ~ "Mother's highest educational attainment",
                                            m_home_own_status ~ "Mother's home ownership",
                                            m_dr_change ~ "Recent doctor change",
                                            m_eval_hlth ~ "Mother's evaluation of own health",
                                            m_anx_score ~ "Crown-crisp anxiety score",
                                            m_dep_score ~ "Crown-crisp depression score"
                                          )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_2_quick)

# always helpful

m_dep_reg_adj_2_help <- with(
  imp_dep_m_help,
  glm(m_dep ~ as.numeric(m_alwys_help) + m_age + m_parity + m_mar_status +
        m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth + m_anx_score + m_dep_score,
      family = binomial(link = "logit")
  ))

print(m_dep_reg_adj_2_help)

pooled_m_dep_reg_adj_2_help <- pool(m_dep_reg_adj_2_help)

summary(pooled_m_dep_reg_adj_2_help, exponentiate = TRUE)


m_adj_dep_table_2_help <- tbl_regression(m_dep_reg_adj_2_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(m_alwys_help)" ~ 
                                             "The doctor in the clinic is always helpful",
                                           m_age ~ "Mother's age",
                                           m_parity ~ "Mother's parity",
                                           m_mar_status ~ "Mother's marital status",
                                           m_relig ~ "Mother's religion",
                                           m_sep ~ "Mother's socioeconomic status",
                                           m_edu_level ~ "Mother's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           m_eval_hlth ~ "Mother's evaluation of own health",
                                           m_anx_score ~ "Crown-crisp anxiety score",
                                           m_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_2_help)

#-------------------------------------------------------------------------------
# partner adjusted model 2
# adjusted model 2
# no confidence
p_anx_reg_adj_2_conf <- with(
  imp_anx_p_conf,
  glm(
    p_anx ~ as.numeric(p_conf_nhs) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score,
    family = binomial(link = "logit")
  ))

print(p_anx_reg_adj_2_conf)

pooled_p_anx_reg_adj_2_conf <- pool(p_anx_reg_adj_2_conf)

summary(pooled_p_anx_reg_adj_2_conf, exponentiate = TRUE)

p_adj_anx_table_2_conf <- tbl_regression(p_anx_reg_adj_2_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(p_conf_nhs)" ~ 
                                             "I don’t have any confidence in the national health service",
                                           p_age ~ "Partner's age",
                                           m_parity ~ "Mother's parity",
                                           p_mar_status ~ "Partners's marital status",
                                           p_relig ~ "Partner's religion",
                                           p_sep ~ "Partner's socioeconomic status",
                                           p_edu_level ~ "Partner's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           p_eval_hlth ~ "Partner's evaluation of own health",
                                           p_anx_score ~ "Crown-crisp anxiety score",
                                           p_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_2_conf)

# partner quick
p_anx_reg_adj_2_quick <- with(
  imp_anx_p_quick,
  glm(
    p_anx ~ as.numeric(p_ill_quick) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score,
    family = binomial(link = "logit")
  ))

print(p_anx_reg_adj_2_quick)

pooled_p_anx_reg_adj_2_quick <- pool(p_anx_reg_adj_2_quick)

summary(pooled_p_anx_reg_adj_2_quick, exponentiate = TRUE)

p_adj_anx_table_2_quick <- tbl_regression(p_anx_reg_adj_2_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            "as.numeric(p_ill_quick)" ~ 
                                              "I know that if my child was very ill my doctor would come quickly",
                                            p_age ~       "Partner's age",
                                            m_parity ~    "Mother's parity",
                                            p_mar_status ~"Partner's marital status",
                                            p_relig ~     "Partner's religion",
                                            p_sep ~       "Partner's socioeconomic status",
                                            p_edu_level ~ "Partner's highest educational attainment",
                                            m_home_own_status ~ "Mother's home ownership",
                                            m_dr_change ~ "Recent doctor change",
                                            p_eval_hlth ~ "Partner's evaluation of own health",
                                            p_anx_score ~ "Crown-crisp anxiety score",
                                            p_dep_score ~ "Crown-crisp depression score"
                                          )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_2_quick)

# always helpful

p_anx_reg_adj_2_help <- with(
  imp_anx_p_help,
  glm(
    p_anx ~ as.numeric(p_alwys_help) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score,
    family = binomial(link = "logit")
  ))

print(p_anx_reg_adj_2_help)

pooled_p_anx_reg_adj_2_help <- pool(p_anx_reg_adj_2_help)

summary(pooled_p_anx_reg_adj_2_help, exponentiate = TRUE)

p_adj_anx_table_2_help <- tbl_regression(p_anx_reg_adj_2_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(p_alwys_help)" ~ 
                                             "The doctor in the clinic is always helpful",
                                           p_age ~        "Partner's age",
                                           m_parity ~     "Mother's parity",
                                           p_mar_status ~ "Partner's marital status",
                                           p_relig ~      "Partner's religion",
                                           p_sep ~        "Partner's socioeconomic status",
                                           p_edu_level ~  "Partner's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           p_eval_hlth ~ "Partner's evaluation of own health",
                                           p_anx_score ~ "Crown-crisp anxiety score",
                                           p_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_2_help)

# partner depression
p_dep_reg_adj_2_conf <- with(
  imp_dep_p_conf,
  glm(
    p_dep ~ as.numeric(p_conf_nhs) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score,
    family = binomial(link = "logit")
  ))

print(p_dep_reg_adj_2_conf)

pooled_p_dep_reg_adj_2_conf <- pool(p_dep_reg_adj_2_conf)

summary(pooled_p_dep_reg_adj_2_conf, exponentiate = TRUE)

p_adj_dep_table_2_conf <- tbl_regression(p_dep_reg_adj_2_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(p_conf_nhs)" ~   
                                             "I don’t have any confidence in the national health service",
                                           p_age ~        "Partner's age",
                                           m_parity ~     "Mother's parity",
                                           p_mar_status ~ "Partner's marital status",
                                           p_relig ~      "Partner's religion",
                                           p_sep ~        "Partner's socioeconomic status",
                                           p_edu_level ~  "Partner's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           p_eval_hlth ~ "Partner's evaluation of own health",
                                           p_anx_score ~ "Crown-crisp anxiety score",
                                           p_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_2_conf)

# quick
p_dep_reg_adj_2_quick <- with(
  imp_dep_p_quick,
  glm(
    p_dep ~ as.numeric(p_ill_quick) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score,
    family = binomial(link = "logit")
  ))

print(p_dep_reg_adj_2_quick)

pooled_p_dep_reg_adj_2_quick <- pool(p_dep_reg_adj_2_quick)

summary(pooled_p_dep_reg_adj_2_quick, exponentiate = TRUE)

p_adj_dep_table_2_quick <- tbl_regression(p_dep_reg_adj_2_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            "as.numeric(p_ill_quick)" ~  
                                              "I know that if my child was very ill my doctor would come quickly",
                                            p_age ~        "Partner's age",
                                            m_parity ~     "Mother's parity",
                                            p_mar_status ~ "Partner's marital status",
                                            p_relig ~      "Partner's religion",
                                            p_sep ~        "Partner's socioeconomic status",
                                            p_edu_level ~  "Partner's highest educational attainment",
                                            m_home_own_status ~ "Mother's home ownership",
                                            m_dr_change ~ "Recent doctor change",
                                            p_eval_hlth ~ "Partner's evaluation of own health",
                                            p_anx_score ~ "Crown-crisp anxiety score",
                                            p_dep_score ~ "Crown-crisp depression score"
                                          )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_2_quick)

# always helpful

p_dep_reg_adj_2_help <- with(
  imp_dep_p_help,
  glm(
    p_dep ~ as.numeric(p_alwys_help) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score,
    data = df_working,
    family = binomial(link = "logit")
  ))

print(p_dep_reg_adj_2_help)

pooled_p_dep_reg_adj_2_quick <- pool(p_dep_reg_adj_2_quick)

summary(pooled_p_dep_reg_adj_2_quick, exponentiate = TRUE)

p_adj_dep_table_2_help <- tbl_regression(p_dep_reg_adj_2_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(p_alwys_help)" ~  
                                             "The doctor in the clinic is always helpful",
                                           p_age ~         "Partner's age",
                                           m_parity ~      "Mother's parity",
                                           p_mar_status ~  "Partner's marital status",
                                           p_relig ~       "Partner's religion",
                                           p_sep ~         "Partner's socioeconomic status",
                                           p_edu_level ~   "Partner's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           p_eval_hlth ~ "Partner's evaluation of own health",
                                           p_anx_score ~ "Crown-crisp anxiety score",
                                           p_dep_score ~ "Crown-crisp depression score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_2_help)


#-------------------------------------------------------------------------------
# adjusted model 3 
# mother no confidence

m_anx_reg_adj_3_conf <- with(
  imp_anx_m_conf,
  glm(
    m_anx ~ as.numeric(m_conf_nhs) + m_age + m_parity + m_mar_status +
      m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth +
      m_soc_supp + m_weigh_life,
    family = binomial(link = "logit")
  ))

print(m_anx_reg_adj_3_conf)

pooled_m_anx_reg_adj_3_conf <- pool(m_anx_reg_adj_3_conf)

summary(pooled_m_anx_reg_adj_3_conf, exponentiate = TRUE)

m_adj_anx_table_3_conf <- tbl_regression(m_anx_reg_adj_3_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(m_conf_nhs)" ~ "I don’t have any confidence in the national health service",
                                           m_age ~ "Age",
                                           m_parity ~ "Parity",
                                           m_mar_status ~ "Marital status",
                                           m_relig ~ "Religion",
                                           m_sep ~ "Socioeconomic status",
                                           m_edu_level ~ "Educational attainment",
                                           m_home_own_status ~ "Home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           m_eval_hlth ~ "Mother's evaluation of own health",
                                           m_soc_supp ~ "Mother's social support score",
                                           m_weigh_life ~ "Mother's weighted life events score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_3_conf)

# mother quick

m_anx_reg_adj_3_quick <- with(
  imp_anx_m_quick,
  glm(m_anx ~ as.numeric(m_ill_quick) + m_age + m_parity + m_mar_status +
        m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth +
        m_soc_supp + m_weigh_life,
      family = binomial(link = "logit")
  ))

print(m_anx_reg_adj_3_quick)

pooled_m_anx_reg_adj_3_quick <- pool(m_anx_reg_adj_3_quick)

summary(pooled_m_anx_reg_adj_3_quick, exponentiate = TRUE)

m_adj_anx_table_3_quick <- tbl_regression(m_anx_reg_adj_3_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            "as.numeric(m_ill_quick)" ~ 
                                              "I know that if my child was very ill my doctor would come quickly",
                                            m_age ~ "Mother's age",
                                            m_parity ~ "Mother's parity",
                                            m_mar_status ~ "Mother's marital status",
                                            m_relig ~ "Mother's religion",
                                            m_sep ~ "Mother's socioeconomic status",
                                            m_edu_level ~ "Mother's highest educational attainment",
                                            m_home_own_status ~ "Mother's home ownership",
                                            m_dr_change ~ "Recent doctor change",
                                            m_eval_hlth ~ "Mother's evaluation of own health",
                                            m_soc_supp ~ "Mother's social support score",
                                            m_weigh_life ~ "Mother's weighted life events score"
                                          )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_3_quick)

# always helpful

m_anx_reg_adj_3_help <- with(
  imp_anx_m_help,
  glm(m_anx ~ as.numeric(m_alwys_help) + m_age + m_parity + m_mar_status +
        m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth +
        m_soc_supp + m_weigh_life,
      family = binomial(link = "logit")
  ))

print(m_anx_reg_adj_3_help)

pooled_m_anx_reg_adj_3_help <- pool(m_anx_reg_adj_3_help)

summary(pooled_m_anx_reg_adj_3_help, exponentiate = TRUE)

m_adj_anx_table_3_help <- tbl_regression(m_anx_reg_adj_3_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(m_alwys_help)" ~
                                             "The doctor in the clinic is always helpful",
                                           m_age ~ "Mother's age",
                                           m_parity ~ "Mother's parity",
                                           m_mar_status ~ "Mother's marital status",
                                           m_relig ~ "Mother's religion",
                                           m_sep ~ "Mother's socioeconomic status",
                                           m_edu_level ~ "Mother's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           m_eval_hlth ~ "Mother's evaluation of own health",
                                           m_soc_supp ~ "Mother's social support score",
                                           m_weigh_life ~ "Mother's weighted life events score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_3_help)

# mother depression

m_dep_reg_adj_3_conf <- with(
  imp_dep_m_conf,
  glm(m_dep ~ as.numeric(m_conf_nhs) + m_age + m_parity + m_mar_status + 
        m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth +
        m_soc_supp + m_weigh_life,
      family = binomial(link = "logit")
  ))

print(m_dep_reg_adj_3_conf)

pooled_m_dep_reg_adj_3_conf <- pool(m_dep_reg_adj_3_conf)

summary(pooled_m_dep_reg_adj_3_conf, exponentiate = TRUE)

m_adj_dep_table_3_conf <- tbl_regression(m_dep_reg_adj_3_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(m_conf_nhs)" ~ 
                                             "I don’t have any confidence in the national health service",
                                           m_age ~ "Mother's age",
                                           m_parity ~ "Mother's parity",
                                           m_mar_status ~ "Mother's marital status",
                                           m_relig ~ "Mother's religion",
                                           m_sep ~ "Mother's socioeconomic status",
                                           m_edu_level ~ "Mother's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           m_eval_hlth ~ "Mother's evaluation of own health",
                                           m_soc_supp ~ "Mother's social support score",
                                           m_weigh_life ~ "Mother's weighted life events score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_3_conf)

# mother quick

m_dep_reg_adj_3_quick <- with(
  imp_dep_m_quick,
  glm(m_dep ~ as.numeric(m_ill_quick) + m_age + m_parity + m_mar_status +
        m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth +
        m_soc_supp + m_weigh_life,
      family = binomial(link = "logit")
  ))

print(m_dep_reg_adj_3_quick)

pooled_m_dep_reg_adj_3_quick <- pool(m_dep_reg_adj_3_quick)

summary(pooled_m_dep_reg_adj_3_quick, exponentiate = TRUE)


m_adj_dep_table_3_quick <- tbl_regression(m_dep_reg_adj_3_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            "as.numeric(m_ill_quick)" ~ 
                                              "I know that if my child was very ill my doctor would come quickly",
                                            m_age ~ "Mother's age",
                                            m_parity ~ "Mother's parity",
                                            m_mar_status ~ "Mother's marital status",
                                            m_relig ~ "Mother's religion",
                                            m_sep ~ "Mother's socioeconomic status",
                                            m_edu_level ~ "Mother's highest educational attainment",
                                            m_home_own_status ~ "Mother's home ownership",
                                            m_dr_change ~ "Recent doctor change",
                                            m_eval_hlth ~ "Mother's evaluation of own health",
                                            m_soc_supp ~ "Mother's social support score",
                                            m_weigh_life ~ "Mother's weighted life events score"
                                          )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_3_quick)

# always helpful

m_dep_reg_adj_3_help <- with(
  imp_dep_m_help,
  glm(m_dep ~ as.numeric(m_alwys_help) + m_age + m_parity + m_mar_status +
        m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth +
        m_soc_supp + m_weigh_life,
      family = binomial(link = "logit")
  ))

print(m_dep_reg_adj_3_help)

pooled_m_dep_reg_adj_3_help <- pool(m_dep_reg_adj_3_help)

summary(pooled_m_dep_reg_adj_3_help, exponentiate = TRUE)


m_adj_dep_table_3_help <- tbl_regression(m_dep_reg_adj_3_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(m_alwys_help)" ~ 
                                             "The doctor in the clinic is always helpful",
                                           m_age ~ "Mother's age",
                                           m_parity ~ "Mother's parity",
                                           m_mar_status ~ "Mother's marital status",
                                           m_relig ~ "Mother's religion",
                                           m_sep ~ "Mother's socioeconomic status",
                                           m_edu_level ~ "Mother's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           m_eval_hlth ~ "Mother's evaluation of own health",
                                           m_soc_supp ~ "Mother's social support score",
                                           m_weigh_life ~ "Mother's weighted life events score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_3_help)

#-------------------------------------------------------------------------------
# partner adjusted model 3
# adjusted model 3
# no confidence
p_anx_reg_adj_3_conf <- with(
  imp_anx_p_conf,
  glm(
    p_anx ~ as.numeric(p_conf_nhs) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_soc_supp + p_weigh_life,
    family = binomial(link = "logit")
  ))

print(p_anx_reg_adj_3_conf)

pooled_p_anx_reg_adj_3_conf <- pool(p_anx_reg_adj_3_conf)

summary(pooled_p_anx_reg_adj_3_conf, exponentiate = TRUE)

p_adj_anx_table_3_conf <- tbl_regression(p_anx_reg_adj_3_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(p_conf_nhs)" ~ 
                                             "I don’t have any confidence in the national health service",
                                           p_age ~ "Partner's age",
                                           m_parity ~ "Mother's parity",
                                           p_mar_status ~ "Partners's marital status",
                                           p_relig ~ "Partner's religion",
                                           p_sep ~ "Partner's socioeconomic status",
                                           p_edu_level ~ "Partner's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           p_eval_hlth ~ "Partner's evaluation of own health",
                                           p_soc_supp ~ "Partner's social support score",
                                           p_weigh_life ~ "Partner's weighted life events score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_3_conf)

# partner quick
p_anx_reg_adj_3_quick <- with(
  imp_anx_p_quick,
  glm(
    p_anx ~ as.numeric(p_ill_quick) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_soc_supp + p_weigh_life,
    family = binomial(link = "logit")
  ))

print(p_anx_reg_adj_3_quick)

pooled_p_anx_reg_adj_3_quick <- pool(p_anx_reg_adj_3_quick)

summary(pooled_p_anx_reg_adj_3_quick, exponentiate = TRUE)

p_adj_anx_table_3_quick <- tbl_regression(p_anx_reg_adj_3_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            "as.numeric(p_ill_quick)" ~ 
                                              "I know that if my child was very ill my doctor would come quickly",
                                            p_age ~       "Partner's age",
                                            m_parity ~    "Mother's parity",
                                            p_mar_status ~"Partner's marital status",
                                            p_relig ~     "Partner's religion",
                                            p_sep ~       "Partner's socioeconomic status",
                                            p_edu_level ~ "Partner's highest educational attainment",
                                            m_home_own_status ~ "Mother's home ownership",
                                            m_dr_change ~ "Recent doctor change",
                                            p_eval_hlth ~ "Partner's evaluation of own health",
                                            p_soc_supp ~ "Partner's social support score",
                                            p_weigh_life ~ "Partner's weighted life events score"
                                          )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_3_quick)

# always helpful

p_anx_reg_adj_3_help <- with(
  imp_anx_p_help,
  glm(
    p_anx ~ as.numeric(p_alwys_help) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_soc_supp + p_weigh_life,
    family = binomial(link = "logit")
  ))

print(p_anx_reg_adj_3_help)

pooled_p_anx_reg_adj_3_help <- pool(p_anx_reg_adj_3_help)

summary(pooled_p_anx_reg_adj_3_help, exponentiate = TRUE)

p_adj_anx_table_3_help <- tbl_regression(p_anx_reg_adj_3_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(p_alwys_help)" ~ 
                                             "The doctor in the clinic is always helpful",
                                           p_age ~        "Partner's age",
                                           m_parity ~     "Mother's parity",
                                           p_mar_status ~ "Partner's marital status",
                                           p_relig ~      "Partner's religion",
                                           p_sep ~        "Partner's socioeconomic status",
                                           p_edu_level ~  "Partner's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           p_eval_hlth ~ "Partner's evaluation of own health",
                                           p_soc_supp ~ "Partner's social support score",
                                           p_weigh_life ~ "Partner's weighted life events score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_3_help)

# partner depression
p_dep_reg_adj_3_conf <- with(
  imp_dep_p_conf,
  glm(
    p_dep ~ as.numeric(p_conf_nhs) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_soc_supp + p_weigh_life,
    family = binomial(link = "logit")
  ))

print(p_dep_reg_adj_3_conf)

pooled_p_dep_reg_adj_3_conf <- pool(p_dep_reg_adj_3_conf)

summary(pooled_p_dep_reg_adj_3_conf, exponentiate = TRUE)

p_adj_dep_table_3_conf <- tbl_regression(p_dep_reg_adj_3_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(p_conf_nhs)" ~   
                                             "I don’t have any confidence in the national health service",
                                           p_age ~        "Partner's age",
                                           m_parity ~     "Mother's parity",
                                           p_mar_status ~ "Partner's marital status",
                                           p_relig ~      "Partner's religion",
                                           p_sep ~        "Partner's socioeconomic status",
                                           p_edu_level ~  "Partner's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           p_eval_hlth ~ "Partner's evaluation of own health",
                                           p_soc_supp ~ "Partner's social support score",
                                           p_weigh_life ~ "Partner's weighted life events score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_3_conf)

# quick
p_dep_reg_adj_3_quick <- with(
  imp_dep_p_quick,
  glm(
    p_dep ~ as.numeric(p_ill_quick) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_soc_supp + p_weigh_life,
    family = binomial(link = "logit")
  ))

print(p_dep_reg_adj_3_quick)

pooled_p_dep_reg_adj_3_quick <- pool(p_dep_reg_adj_3_quick)

summary(pooled_p_dep_reg_adj_3_quick, exponentiate = TRUE)

p_adj_dep_table_3_quick <- tbl_regression(p_dep_reg_adj_3_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            "as.numeric(p_ill_quick)" ~  
                                              "I know that if my child was very ill my doctor would come quickly",
                                            p_age ~        "Partner's age",
                                            m_parity ~     "Mother's parity",
                                            p_mar_status ~ "Partner's marital status",
                                            p_relig ~      "Partner's religion",
                                            p_sep ~        "Partner's socioeconomic status",
                                            p_edu_level ~  "Partner's highest educational attainment",
                                            m_home_own_status ~ "Mother's home ownership",
                                            m_dr_change ~ "Recent doctor change",
                                            p_eval_hlth ~ "Partner's evaluation of own health",
                                            p_soc_supp ~ "Partner's social support score",
                                            p_weigh_life ~ "Partner's weighted life events score"
                                          )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_3_quick)

# always helpful

p_dep_reg_adj_3_help <- with(
  imp_dep_p_help,
  glm(
    p_dep ~ as.numeric(p_alwys_help) + p_age + m_parity + p_mar_status +
      p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_soc_supp + p_weigh_life,
    data = df_working,
    family = binomial(link = "logit")
  ))

print(p_dep_reg_adj_3_help)

pooled_p_dep_reg_adj_3_quick <- pool(p_dep_reg_adj_3_quick)

summary(pooled_p_dep_reg_adj_3_quick, exponentiate = TRUE)

p_adj_dep_table_3_help <- tbl_regression(p_dep_reg_adj_3_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           "as.numeric(p_alwys_help)" ~  
                                             "The doctor in the clinic is always helpful",
                                           p_age ~         "Partner's age",
                                           m_parity ~      "Mother's parity",
                                           p_mar_status ~  "Partner's marital status",
                                           p_relig ~       "Partner's religion",
                                           p_sep ~         "Partner's socioeconomic status",
                                           p_edu_level ~   "Partner's highest educational attainment",
                                           m_home_own_status ~ "Mother's home ownership",
                                           m_dr_change ~ "Recent doctor change",
                                           p_eval_hlth ~ "Partner's evaluation of own health",
                                           p_soc_supp ~ "Partner's social support score",
                                           p_weigh_life ~ "Partner's weighted life events score"
                                         )) %>%
  modify_caption("**Table 17. Model 1: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_3_help)



 
# Creating regressoion tables
## Anxiety vs I don't have any confidence in the NHS  

### Mother

  
# anxiety - no confidence - mothers
m_adj_table_1_anx_conf <- tbl_merge(
  tbls = list(m_anx_table_conf,
              m_adj_anx_table_1_conf, 
              m_adj_anx_table_2_conf, 
              m_adj_anx_table_3_conf) ,
  tab_spanner = c(glue::glue("**Unadjusted Model)**"), 
                  glue::glue("**Model 1)**"), 
                  glue::glue("**Model 2)**"),
                  glue::glue("**Model 3)**"))
) %>%
  modify_caption(glue::glue("**Table 13. All Models for Question 1: Adjusted Odds Ratios for Anxiety for Mothers**"))

# Print table
m_adj_table_1_anx_conf
 

### Partner

  
# anxiety - no confidence - Partner
p_adj_table_1_anx_conf <- tbl_merge(
  tbls = list(p_anx_table_conf,
              p_adj_anx_table_1_conf, 
              p_adj_anx_table_2_conf, 
              p_adj_anx_table_3_conf) ,
  tab_spanner = c(glue::glue("**Unadjusted Model)**"), 
                  glue::glue("**Model 1)**"), 
                  glue::glue("**Model 2)**"),
                  glue::glue("**Model 3)**"))
)

# Print table
p_adj_table_1_anx_conf
 

## Depression vs I don't have any confidence in the NHS  

### Mother

  
# depression - no confidence - mothers
m_adj_table_1_dep_conf <- tbl_merge(
  tbls = list(m_dep_table_conf,
              m_adj_dep_table_1_conf, 
              m_adj_dep_table_2_conf, 
              m_adj_dep_table_3_conf) ,
  tab_spanner = c(glue::glue("**Unadjusted Model)**"), 
                  glue::glue("**Model 1)**"), 
                  glue::glue("**Model 2)**"),
                  glue::glue("**Model 3)**"))
) %>%
  modify_caption(glue::glue("**Table 15. All Models for Question 1: Adjusted Odds Ratios for Depression for Mothers**"))

# Print table
m_adj_table_1_dep_conf
 

### Partner

  
# depression - no confidence - Partners
p_adj_table_1_dep_conf <- tbl_merge(
  tbls = list(p_dep_table_conf,
              p_adj_dep_table_1_conf, 
              p_adj_dep_table_2_conf, 
              p_adj_dep_table_3_conf) ,
  tab_spanner = c(glue::glue("**Unadjusted Model)**"), 
                  glue::glue("**Model 1)**"), 
                  glue::glue("**Model 2)**"),
                  glue::glue("**Model 3)**"))
) %>%
  modify_caption(glue::glue("**Table 16. All Models for Question 1: Adjusted Odds Ratios for Depression for Partners**"))

# Print table
p_adj_table_1_dep_conf
 

## Anxiety vs I know that if my child was very ill my doctor would come quickly  

### Mother

  
m_adj_table_1_anx_quick <- tbl_merge(
  tbls = list(m_anx_table_quick,
              m_adj_anx_table_1_quick, 
              m_adj_anx_table_2_quick, 
              m_adj_anx_table_3_quick) ,
  tab_spanner = c(glue::glue("**Unadjusted Model)**"), 
                  glue::glue("**Model 1)**"), 
                  glue::glue("**Model 2)**"),
                  glue::glue("**Model 3)**"))
) %>%
  modify_caption(glue::glue("**Table 17. All Models for Question 2: Adjusted Odds Ratios for Anxiety for Mothers**"))

# Print table
m_adj_table_1_anx_quick
 

### Partner

  
p_adj_table_1_anx_quick <- tbl_merge(
  tbls = list(p_anx_table_quick,
              p_adj_anx_table_1_quick, 
              p_adj_anx_table_2_quick, 
              p_adj_anx_table_3_quick) ,
  tab_spanner = c(glue::glue("**Unadjusted Model)**"), 
                  glue::glue("**Model 1)**"), 
                  glue::glue("**Model 2)**"),
                  glue::glue("**Model 3)**"))
) %>%
  modify_caption(glue::glue("**Table 18. All Models for Question 2: Adjusted Odds Ratios for Anxiety for Partners**"))

# Print table
p_adj_table_1_anx_quick
 

## Depression vs I know that if my child was very ill my doctor would come quickly  

### Mother

  
m_adj_table_1_dep_quick <- tbl_merge(
  tbls = list(m_dep_table_quick,
              m_adj_dep_table_1_quick, 
              m_adj_dep_table_2_quick, 
              m_adj_dep_table_3_quick) ,
  tab_spanner = c(glue::glue("**Unadjusted Model)**"), 
                  glue::glue("**Model 1)**"), 
                  glue::glue("**Model 2)**"),
                  glue::glue("**Model 3)**"))
) %>%
  modify_caption(glue::glue("**Table 19. All Models for Question 2: Adjusted Odds Ratios for Depression for Mothers**"))

# Print table
m_adj_table_1_dep_quick

 

### Partner

  
p_adj_table_1_dep_quick <- tbl_merge(
  tbls = list(p_dep_table_quick,
              p_adj_dep_table_1_quick, 
              p_adj_dep_table_2_quick, 
              p_adj_dep_table_3_quick) ,
  tab_spanner = c(glue::glue("**Unadjusted Model)**"), 
                  glue::glue("**Model 1)**"), 
                  glue::glue("**Model 2)**"),
                  glue::glue("**Model 3)**"))
) %>%
  modify_caption(glue::glue("**Table 20. All Models for Question 2: Adjusted Odds Ratios for Depression for Partners**"))

# Print table
p_adj_table_1_dep_quick
 

## Anxiety vs The doctor in the clinic is always helpful  

### Mother

  
# anxiety - always helpful - mothers
m_adj_table_1_anx_help <- tbl_merge(
  tbls = list(m_anx_table_help,
              m_adj_anx_table_1_help, 
              m_adj_anx_table_2_help, 
              m_adj_anx_table_3_help) ,
  tab_spanner = c(glue::glue("**Unadjusted Model)**"), 
                  glue::glue("**Model 1)**"), 
                  glue::glue("**Model 2)**"),
                  glue::glue("**Model 3)**"))
) %>%
  modify_caption(glue::glue("**Table 21. All Models for Question 3: Adjusted Odds Ratios for Anxiety for Mothers**"))

# Print table
m_adj_table_1_anx_help
 

### Partner

  
# anxiety - always help - Partner
p_adj_table_1_anx_help <- tbl_merge(
  tbls = list(p_anx_table_help,
              p_adj_anx_table_1_help, 
              p_adj_anx_table_2_help, 
              p_adj_anx_table_3_help) ,
  tab_spanner = c(glue::glue("**Unadjusted Model)**"), 
                  glue::glue("**Model 1)**"), 
                  glue::glue("**Model 2)**"),
                  glue::glue("**Model 3)**"))
) %>%
  modify_caption(glue::glue("**Table 22. All Models for Question 3: Adjusted Odds Ratios for Anxiety for Partners**"))

# Print table
p_adj_table_1_anx_help
 

## Depression vs The doctor in the clinic is always helpful  

### Mother

  
# depression - always helpful - mothers
m_adj_table_1_dep_help <- tbl_merge(
  tbls = list(m_dep_table_help,
              m_adj_dep_table_1_help, 
              m_adj_dep_table_2_help, 
              m_adj_dep_table_3_help) ,
  tab_spanner = c(glue::glue("**Unadjusted Model)**"), 
                  glue::glue("**Model 1)**"), 
                  glue::glue("**Model 2)**"),
                  glue::glue("**Model 3)**"))
) %>%
  modify_caption(glue::glue("**Table 23. All Models for Question 3: Adjusted Odds Ratios for Depression for Mothers**"))

# Print table
m_adj_table_1_dep_help
 

### Partner

  
# anxiety - always help - Partner
p_adj_table_1_dep_help <- tbl_merge(
  tbls = list(p_dep_table_help,
              p_adj_dep_table_1_help, 
              p_adj_dep_table_2_help, 
              p_adj_dep_table_3_help) ,
  tab_spanner = c(glue::glue("**Unadjusted Model)**"), 
                  glue::glue("**Model 1)**"), 
                  glue::glue("**Model 2)**"),
                  glue::glue("**Model 3)**"))
) %>%
  modify_caption(glue::glue("**Table 24. All Models for Question 3: Adjusted Odds Ratios for Anxiety for Partners**"))

# Print table
p_adj_table_1_dep_help
 
#-------------------------------------------------------------------------------
# Regression tables with main predictors only  

## Anxiety  

### I don't have any confidence in the NHS

filter_anx_conf_m <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "as.numeric(m_conf_nhs)"))
}

m_anx_table_conf_clean <- filter_anx_conf_m(m_anx_table_conf)
m_adj_anx_table_1_conf_clean <- filter_anx_conf_m(m_adj_anx_table_1_conf)
m_adj_anx_table_2_conf_clean <- filter_anx_conf_m(m_adj_anx_table_2_conf)
m_adj_anx_table_3_conf_clean <- filter_anx_conf_m(m_adj_anx_table_3_conf)

m_adj_table_1_anx_conf <- tbl_merge(
  tbls = list(
    m_anx_table_conf_clean,
    m_adj_anx_table_1_conf_clean, 
    m_adj_anx_table_2_conf_clean, 
    m_adj_anx_table_3_conf_clean
  ),
  tab_spanner = c("**Unadjusted Model**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 1: Adjusted Odds Ratios for Anxiety for Mothers**"))

m_adj_table_1_anx_conf

 

  
df_3_anx_m_conf <- m_adj_anx_table_3_conf_clean$table_body %>% mutate(model = "Model 3")
df_2_anx_m_conf <- m_adj_anx_table_2_conf_clean$table_body %>% mutate(model = "Model 2")
df_1_anx_m_conf <- m_adj_anx_table_1_conf_clean$table_body %>% mutate(model = "Model 1")
df_0_anx_m_conf <- m_anx_table_conf_clean$table_body       %>% mutate(model = "Unadjusted")

combined_df_anx_m_conf <- bind_rows(df_3_anx_m_conf,
                                    df_2_anx_m_conf,
                                    df_1_anx_m_conf,
                                    df_0_anx_m_conf)

df_long_anx_m_conf <- combined_df_anx_m_conf %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_anx_m_conf <- df_long_anx_m_conf %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))

ggplot(df_long_anx_m_conf, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",     # blue
      "Model 2" = "#f44973",     # green
      "Model 3" = "#f2923f"      # red
    ) ) +
  scale_x_continuous(
    breaks = seq(0.70, 2.20, by = 0.10),
    limits = c(0.70, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Mothers",
    color = "Model"
  ) +
  theme_minimal()
 


filter_anx_conf_p <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "as.numeric(p_conf_nhs)"))
}

p_anx_table_conf_clean <- filter_anx_conf_p(p_anx_table_conf)
p_adj_anx_table_1_conf_clean <- filter_anx_conf_p(p_adj_anx_table_1_conf)
p_adj_anx_table_2_conf_clean <- filter_anx_conf_p(p_adj_anx_table_2_conf)
p_adj_anx_table_3_conf_clean <- filter_anx_conf_p(p_adj_anx_table_3_conf)

p_adj_table_1_anx_conf <- tbl_merge(
  tbls = list(
    p_anx_table_conf_clean,
    p_adj_anx_table_1_conf_clean, 
    p_adj_anx_table_2_conf_clean, 
    p_adj_anx_table_3_conf_clean
  ),
  tab_spanner = c("**Unadjusted Model**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 1: Adjusted Odds Ratios for Anxiety for Partners**"))

p_adj_table_1_anx_conf

 

  
df_3_anx_p_conf <- p_adj_anx_table_3_conf_clean$table_body %>% mutate(model = "Model 3")
df_2_anx_p_conf <- p_adj_anx_table_2_conf_clean$table_body %>% mutate(model = "Model 2")
df_1_anx_p_conf <- p_adj_anx_table_1_conf_clean$table_body %>% mutate(model = "Model 1")
df_0_anx_p_conf <- p_anx_table_conf_clean$table_body       %>% mutate(model = "Unadjusted")

combined_df_anx_p_conf <- bind_rows(df_3_anx_p_conf,
                                    df_2_anx_p_conf,
                                    df_1_anx_p_conf,
                                    df_0_anx_p_conf)

df_long_anx_p_conf <- combined_df_anx_p_conf %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_anx_p_conf <- df_long_anx_p_conf %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))

ggplot(df_long_anx_p_conf, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",     # blue
      "Model 2" = "#f44973",     # green
      "Model 3" = "#f2923f"      # red
    ) ) +
  scale_x_continuous(
    breaks = seq(0.70, 2.20, by = 0.10),
    limits = c(0.70, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Partners",
    color = "Model"
  ) +
  theme_minimal()

 

  
m_adj_table_1_anx_conf_labeled <- m_adj_table_1_anx_conf %>%
  modify_table_body(~ 
                      .x %>%
                      mutate(
                        participant_type = "Mother",
                        condition = "Anxiety"
                      ) %>%
                      select(condition, participant_type, everything())   
  )

p_adj_table_1_anx_conf_labeled <- p_adj_table_1_anx_conf %>%
  modify_table_body(~ 
                      .x %>%
                      mutate(
                        participant_type = "Partner",
                        condition = "Anxiety"
                      ) %>%
                      select(condition, participant_type, everything())
  )

combined_anx_conf_table <- tbl_stack(
  tbls = list(
    m_adj_table_1_anx_conf_labeled,
    p_adj_table_1_anx_conf_labeled
  )
) %>%
  modify_caption("**Table 25. Combined Anxiety and Confidence Predictor Models for Mothers and Partners**") %>%
  modify_header(
    condition = "**Mental health condition**",
    participant_type = "**Participant Type**"
  )

combined_anx_conf_table

 

### I know that if my child was very ill my doctor would come quickly

filter_anx_quick_m <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "as.numeric(m_ill_quick)"))
}

m_anx_table_quick_clean <- filter_anx_quick_m(m_anx_table_quick)
m_adj_anx_table_1_quick_clean <- filter_anx_quick_m(m_adj_anx_table_1_quick)
m_adj_anx_table_2_quick_clean <- filter_anx_quick_m(m_adj_anx_table_2_quick)
m_adj_anx_table_3_quick_clean <- filter_anx_quick_m(m_adj_anx_table_3_quick)

m_adj_table_1_anx_quick <- tbl_merge(
  tbls = list(
    m_anx_table_quick_clean,
    m_adj_anx_table_1_quick_clean, 
    m_adj_anx_table_2_quick_clean, 
    m_adj_anx_table_3_quick_clean
  ),
  tab_spanner = c("**Model 0**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 2: Adjusted Odds Ratios for Anxiety for Mothers**"))

m_adj_table_1_anx_quick


 

  
df_3_anx_m_quick <- m_adj_anx_table_3_quick_clean$table_body %>% mutate(model = "Model 3")
df_2_anx_m_quick <- m_adj_anx_table_2_quick_clean$table_body %>% mutate(model = "Model 2")
df_1_anx_m_quick <- m_adj_anx_table_1_quick_clean$table_body %>% mutate(model = "Model 1")
df_0_anx_m_quick <- m_anx_table_quick_clean$table_body       %>% mutate(model = "Unadjusted")

combined_df_anx_m_quick <- bind_rows(df_3_anx_m_quick,
                                     df_2_anx_m_quick,
                                     df_1_anx_m_quick,
                                     df_0_anx_m_quick)

df_long_anx_m_quick <- combined_df_anx_m_quick %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_anx_m_quick <- df_long_anx_m_quick %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))

ggplot(df_long_anx_m_quick, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",     # blue
      "Model 2" = "#f44973",     # green
      "Model 3" = "#f2923f"      # red
    ) ) +
  scale_x_continuous(
    breaks = seq(0.70, 2.20, by = 0.10),
    limits = c(0.70, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Mothers",
    color = "Model"
  ) +
  theme_minimal()
 

  
# partners
filter_anx_quick_p <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "as.numeric(p_ill_quick)"))
}

p_anx_table_quick_clean <- filter_anx_quick_p(p_anx_table_quick)
p_adj_anx_table_1_quick_clean <- filter_anx_quick_p(p_adj_anx_table_1_quick)
p_adj_anx_table_2_quick_clean <- filter_anx_quick_p(p_adj_anx_table_2_quick)
p_adj_anx_table_3_quick_clean <- filter_anx_quick_p(p_adj_anx_table_3_quick)

p_adj_table_1_anx_quick <- tbl_merge(
  tbls = list(
    p_anx_table_quick_clean,
    p_adj_anx_table_1_quick_clean, 
    p_adj_anx_table_2_quick_clean, 
    p_adj_anx_table_3_quick_clean
  ),
  tab_spanner = c("**Model 0**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 2: Adjusted Odds Ratios for Anxiety for Partners**"))

p_adj_table_1_anx_quick

 

  
df_3_anx_p_quick <- p_adj_anx_table_3_quick_clean$table_body %>% mutate(model = "Model 3")
df_2_anx_p_quick <- p_adj_anx_table_2_quick_clean$table_body %>% mutate(model = "Model 2")
df_1_anx_p_quick <- p_adj_anx_table_1_quick_clean$table_body %>% mutate(model = "Model 1")
df_0_anx_p_quick <- p_anx_table_quick_clean$table_body       %>% mutate(model = "Unadjusted")

combined_df_anx_p_quick <- bind_rows(df_3_anx_p_quick,
                                     df_2_anx_p_quick,
                                     df_1_anx_p_quick,
                                     df_0_anx_p_quick)

df_long_anx_p_quick <- combined_df_anx_p_quick %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_anx_p_quick <- df_long_anx_p_quick %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))

ggplot(df_long_anx_p_quick, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",     # blue
      "Model 2" = "#f44973",     # green
      "Model 3" = "#f2923f"      # red
    ) ) +
  scale_x_continuous(
    breaks = seq(0.70, 2.20, by = 0.10),
    limits = c(0.70, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Partners",
    color = "Model"
  ) +
  theme_minimal()
 

  
m_adj_table_1_anx_quick_labeled <- m_adj_table_1_anx_quick %>%
  modify_table_body(~ 
                      .x %>%
                      mutate(
                        participant_type = "Mother",
                        condition = "Anxiety"
                      ) %>%        # add column
                      select(condition, participant_type, everything())          # move it to first column
  )

p_adj_table_1_anx_quick_labeled <- p_adj_table_1_anx_quick %>%
  modify_table_body(~ 
                      .x %>%
                      mutate(participant_type = "Partner",
                             condition = "Anxiety"
                      ) %>%       # add column
                      select(participant_type, everything())          # move it first
  )

combined_anx_quick_table <- tbl_stack(
  tbls = list(
    m_adj_table_1_anx_quick_labeled,
    p_adj_table_1_anx_quick_labeled
  )
) %>%
  modify_caption("**Table 25. Combined Mother and Partner outcomes for I know if my child was ill, a doctor would come quickly against Anxiety**") %>%
  modify_header(condition = "**Mental health condition**",
                participant_type = "**Participant Type**"
  )

combined_anx_quick_table

 

### The doctor in the clinic is always helpful

filter_anx_help_m <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "as.numeric(m_alwys_help)"))
}

m_anx_table_help_clean <- filter_anx_help_m(m_anx_table_help)
m_adj_anx_table_1_help_clean <- filter_anx_help_m(m_adj_anx_table_1_help)
m_adj_anx_table_2_help_clean <- filter_anx_help_m(m_adj_anx_table_2_help)
m_adj_anx_table_3_help_clean <- filter_anx_help_m(m_adj_anx_table_3_help)

m_adj_table_1_anx_help <- tbl_merge(
  tbls = list(
    m_anx_table_help_clean,
    m_adj_anx_table_1_help_clean, 
    m_adj_anx_table_2_help_clean, 
    m_adj_anx_table_3_help_clean
  ),
  tab_spanner = c("**Unadjusted Model**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 3: Adjusted Odds Ratios for Anxiety for Mothers**"))

m_adj_table_1_anx_help

 

  
df_3_anx_m_help <- m_adj_anx_table_3_help_clean$table_body %>% mutate(model = "Model 3")
df_2_anx_m_help <- m_adj_anx_table_2_help_clean$table_body %>% mutate(model = "Model 2")
df_1_anx_m_help <- m_adj_anx_table_1_help_clean$table_body %>% mutate(model = "Model 1")
df_0_anx_m_help <- m_anx_table_help_clean$table_body       %>% mutate(model = "Unadjusted")

combined_df_anx_m_help <- bind_rows(df_3_anx_m_help,
                                    df_2_anx_m_help,
                                    df_1_anx_m_help,
                                    df_0_anx_m_help)

df_long_anx_m_help <- combined_df_anx_m_help %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_anx_m_help <- df_long_anx_m_help %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))

ggplot(df_long_anx_m_help, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",     # blue
      "Model 2" = "#f44973",     # green
      "Model 3" = "#f2923f"      # red
    ) ) +
  scale_x_continuous(
    breaks = seq(0.70, 2.20, by = 0.10),
    limits = c(0.70, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Mothers",
    color = "Model"
  ) +
  theme_minimal()

 
filter_anx_help_p <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "as.numeric(p_alwys_help)"))
}


p_anx_table_help_clean <- filter_anx_help_p(p_anx_table_help)
p_adj_anx_table_1_help_clean <- filter_anx_help_p(p_adj_anx_table_1_help)
p_adj_anx_table_2_help_clean <- filter_anx_help_p(p_adj_anx_table_2_help)
p_adj_anx_table_3_help_clean <- filter_anx_help_p(p_adj_anx_table_3_help)

p_adj_table_1_anx_help <- tbl_merge(
  tbls = list(
    p_anx_table_help_clean,
    p_adj_anx_table_1_help_clean, 
    p_adj_anx_table_2_help_clean, 
    p_adj_anx_table_3_help_clean
  ),
  tab_spanner = c("**Model 0**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 3: Adjusted Odds Ratios for Anxiety for Mothers**"))

p_adj_table_1_anx_help

 

  
df_3_anx_p_help <- p_adj_anx_table_3_help_clean$table_body %>% mutate(model = "Model 3")
df_2_anx_p_help <- p_adj_anx_table_2_help_clean$table_body %>% mutate(model = "Model 2")
df_1_anx_p_help <- p_adj_anx_table_1_help_clean$table_body %>% mutate(model = "Model 1")
df_0_anx_p_help <- p_anx_table_help_clean$table_body       %>% mutate(model = "Unadjusted")

combined_df_anx_p_help <- bind_rows(df_3_anx_p_help,
                                    df_2_anx_p_help,
                                    df_1_anx_p_help,
                                    df_0_anx_p_help)

df_long_anx_p_help <- combined_df_anx_p_help %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_anx_p_help <- df_long_anx_p_help %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))

ggplot(df_long_anx_p_help, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",     # blue
      "Model 2" = "#f44973",     # green
      "Model 3" = "#f2923f"      # red
    ) ) +
  scale_x_continuous(
    breaks = seq(0.70, 2.20, by = 0.10),
    limits = c(0.70, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Mothers",
    color = "Model"
  ) +
  theme_minimal()

 

  
m_adj_table_1_anx_help_labeled <- m_adj_table_1_anx_help %>%
  modify_table_body(~ 
                      .x %>%
                      mutate(participant_type = "Mother",
                             condition = "Anxiety") %>%        # add column
                      select(condition, participant_type, everything())          # move it to first column
  )

p_adj_table_1_anx_help_labeled <- p_adj_table_1_anx_help %>%
  modify_table_body(~ 
                      .x %>%
                      mutate(participant_type = "Partner",
                             condition = "Anxiety") %>%       # add column
                      select(condition, participant_type, everything())          # move it first
  )

combined_anx_help_table <- tbl_stack(
  tbls = list(
    m_adj_table_1_anx_help_labeled,
    p_adj_table_1_anx_help_labeled
  )
) %>%
  modify_caption("**Table 25. Combined Mother and Partner outcomes for The doctor in the clinic is always helpful against Anxiety**") %>%
  modify_header(condition = "**Mental health condition**",
                participant_type = "**Participant Type**")

combined_anx_help_table

 

## Depression  

### I don't have any confidence in the NHS

filter_dep_conf_m <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "as.numeric(m_conf_nhs)"))
}

m_dep_table_conf_clean <- filter_dep_conf_m(m_dep_table_conf)
m_adj_dep_table_1_conf_clean <- filter_dep_conf_m(m_adj_dep_table_1_conf)
m_adj_dep_table_2_conf_clean <- filter_dep_conf_m(m_adj_dep_table_2_conf)
m_adj_dep_table_3_conf_clean <- filter_dep_conf_m(m_adj_dep_table_3_conf)

m_adj_table_1_dep_conf <- tbl_merge(
  tbls = list(
    m_dep_table_conf_clean,
    m_adj_dep_table_1_conf_clean, 
    m_adj_dep_table_2_conf_clean, 
    m_adj_dep_table_3_conf_clean
  ),
  tab_spanner = c("**Model 0**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 1: Adjusted Odds Ratios for Depression for Mothers**"))

m_adj_table_1_dep_conf
 

  
df_3_dep_m_conf <- m_adj_dep_table_3_conf_clean$table_body %>% mutate(model = "Model 3")
df_2_dep_m_conf <- m_adj_dep_table_2_conf_clean$table_body %>% mutate(model = "Model 2")
df_1_dep_m_conf <- m_adj_dep_table_1_conf_clean$table_body %>% mutate(model = "Model 1")
df_0_dep_m_conf <- m_dep_table_conf_clean$table_body       %>% mutate(model = "Unadjusted")

combined_df_dep_m_conf <- bind_rows(df_3_dep_m_conf,
                                    df_2_dep_m_conf,
                                    df_1_dep_m_conf,
                                    df_0_dep_m_conf)

df_long_dep_m_conf <- combined_df_dep_m_conf %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_dep_m_conf <- df_long_dep_m_conf %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))

ggplot(df_long_dep_m_conf, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",     # blue
      "Model 2" = "#f44973",     # green
      "Model 3" = "#f2923f"      # red
    ) ) +
  scale_x_continuous(
    breaks = seq(0.70, 2.20, by = 0.10),
    limits = c(0.70, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Mothers",
    color = "Model"
  ) +
  theme_minimal()
 

# p conf nhs
filter_dep_conf_p <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "as.numeric(p_conf_nhs)"))
}

p_dep_table_conf_clean <- filter_dep_conf_p(p_dep_table_conf)
p_adj_dep_table_1_conf_clean <- filter_dep_conf_p(p_adj_dep_table_1_conf)
p_adj_dep_table_2_conf_clean <- filter_dep_conf_p(p_adj_dep_table_2_conf)
p_adj_dep_table_3_conf_clean <- filter_dep_conf_p(p_adj_dep_table_3_conf)

p_adj_table_1_dep_conf <- tbl_merge(
  tbls = list(
    p_dep_table_conf_clean,
    p_adj_dep_table_1_conf_clean, 
    p_adj_dep_table_2_conf_clean, 
    p_adj_dep_table_3_conf_clean
  ),
  tab_spanner = c("**Model 0**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 1: Adjusted Odds Ratios for Depression for Partners**"))

p_adj_table_1_dep_conf
 

  
df_3_dep_p_conf <- p_adj_dep_table_3_conf_clean$table_body %>% mutate(model = "Model 3")
df_2_dep_p_conf <- p_adj_dep_table_2_conf_clean$table_body %>% mutate(model = "Model 2")
df_1_dep_p_conf <- p_adj_dep_table_1_conf_clean$table_body %>% mutate(model = "Model 1")
df_0_dep_p_conf <- p_dep_table_conf_clean$table_body       %>% mutate(model = "Unadjusted")

combined_df_dep_p_conf <- bind_rows(df_3_dep_p_conf,
                                    df_2_dep_p_conf,
                                    df_1_dep_p_conf,
                                    df_0_dep_p_conf)

df_long_dep_p_conf <- combined_df_dep_p_conf %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_dep_p_conf <- df_long_dep_p_conf %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))

ggplot(df_long_dep_p_conf, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",     # blue
      "Model 2" = "#f44973",     # green
      "Model 3" = "#f2923f"      # red
    ) ) +
  scale_x_continuous(
    breaks = seq(0.70, 2.20, by = 0.10),
    limits = c(0.70, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Partners",
    color = "Model"
  ) +
  theme_minimal()

 

  
m_adj_table_1_dep_conf_labeled <- m_adj_table_1_dep_conf %>%
  modify_table_body(~ 
                      .x %>%
                      mutate(participant_type = "Mother",
                             condition = "Depression") %>%        # add column
                      select(condition, participant_type, everything())          # move it to first column
  )

p_adj_table_1_dep_conf_labeled <- p_adj_table_1_dep_conf %>%
  modify_table_body(~ 
                      .x %>%
                      mutate(participant_type = "Partner",
                             condition = "Depression") %>%       # add column
                      select(condition, participant_type, everything())          # move it first
  )

combined_dep_conf_table <- tbl_stack(
  tbls = list(
    m_adj_table_1_dep_conf_labeled,
    p_adj_table_1_dep_conf_labeled
  )
) %>%
  modify_caption("**Table 25. Combined Mother and Partner results for The doctor in the clinic is always helpful against Depression outcomes**") %>%
  modify_header(condition = "**Mental health condition**",
                participant_type = "**Participant Type**")

combined_dep_conf_table

 

### I know that if my child was ill, a doctor would come quickly
filter_dep_quick_m <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "as.numeric(m_ill_quick)"))
}

m_dep_table_quick_clean <- filter_dep_quick_m(m_dep_table_quick)
m_adj_dep_table_1_quick_clean <- filter_dep_quick_m(m_adj_dep_table_1_quick)
m_adj_dep_table_2_quick_clean <- filter_dep_quick_m(m_adj_dep_table_2_quick)
m_adj_dep_table_3_quick_clean <- filter_dep_quick_m(m_adj_dep_table_3_quick)

m_adj_table_1_dep_quick <- tbl_merge(
  tbls = list(
    m_dep_table_quick_clean,
    m_adj_dep_table_1_quick_clean, 
    m_adj_dep_table_2_quick_clean, 
    m_adj_dep_table_3_quick_clean
  ),
  tab_spanner = c("**Model 0**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 2: Adjusted Odds Ratios for Depression for Mothers**"))

m_adj_table_1_dep_quick


 

  
df_3_dep_m_quick <- m_adj_dep_table_3_quick_clean$table_body %>% mutate(model = "Model 3")
df_2_dep_m_quick <- m_adj_dep_table_2_quick_clean$table_body %>% mutate(model = "Model 2")
df_1_dep_m_quick <- m_adj_dep_table_1_quick_clean$table_body %>% mutate(model = "Model 1")
df_0_dep_m_quick <- m_dep_table_quick_clean$table_body       %>% mutate(model = "Unadjusted")

combined_df_dep_m_quick <- bind_rows(df_3_dep_m_quick,
                                     df_2_dep_m_quick,
                                     df_1_dep_m_quick,
                                     df_0_dep_m_quick)

df_long_dep_m_quick <- combined_df_dep_m_quick %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_dep_m_quick <- df_long_dep_m_quick %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))

ggplot(df_long_dep_m_quick, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",     # blue
      "Model 2" = "#f44973",     # green
      "Model 3" = "#f2923f"      # red
    ) ) +
  scale_x_continuous(
    breaks = seq(0.70, 2.20, by = 0.10),
    limits = c(0.70, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Mothers",
    color = "Model"
  ) +
  theme_minimal()
 


filter_dep_quick_p <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "as.numeric(p_ill_quick)"))
}

p_dep_table_quick_clean <- filter_dep_quick_p(p_dep_table_quick)
p_adj_dep_table_1_quick_clean <- filter_dep_quick_p(p_adj_dep_table_1_quick)
p_adj_dep_table_2_quick_clean <- filter_dep_quick_p(p_adj_dep_table_2_quick)
p_adj_dep_table_3_quick_clean <- filter_dep_quick_p(p_adj_dep_table_3_quick)

p_adj_table_1_dep_quick <- tbl_merge(
  tbls = list(
    p_dep_table_quick_clean,
    p_adj_dep_table_1_quick_clean, 
    p_adj_dep_table_2_quick_clean, 
    p_adj_dep_table_3_quick_clean
  ),
  tab_spanner = c("**Model 0**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 2: Adjusted Odds Ratios for Depression for Partners**"))

p_adj_table_1_dep_quick


 

  
df_3_dep_p_quick <- p_adj_dep_table_3_quick_clean$table_body %>% mutate(model = "Model 3")
df_2_dep_p_quick <- p_adj_dep_table_2_quick_clean$table_body %>% mutate(model = "Model 2")
df_1_dep_p_quick <- p_adj_dep_table_1_quick_clean$table_body %>% mutate(model = "Model 1")
df_0_dep_p_quick <- p_dep_table_quick_clean$table_body       %>% mutate(model = "Unadjusted")

combined_df_dep_p_quick <- bind_rows(df_3_dep_p_quick,
                                     df_2_dep_p_quick,
                                     df_1_dep_p_quick,
                                     df_0_dep_p_quick)

df_long_dep_p_quick <- combined_df_dep_p_quick %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_dep_p_quick <- df_long_dep_p_quick %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))

ggplot(df_long_dep_p_quick, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",     # blue
      "Model 2" = "#f44973",     # green
      "Model 3" = "#f2923f"      # red
    ) ) +
  scale_x_continuous(
    breaks = seq(0.70, 2.20, by = 0.10),
    limits = c(0.70, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Partners",
    color = "Model"
  ) +
  theme_minimal()
 

  
m_adj_table_1_dep_quick_labeled <- m_adj_table_1_dep_quick %>%
  modify_table_body(~ 
                      .x %>%
                      mutate(participant_type = "Mother",
                             condition = "Depression"
                      ) %>%        # add column
                      select(condition, participant_type, everything())          # move it to first column
  )

p_adj_table_1_dep_quick_labeled <- p_adj_table_1_dep_quick %>%
  modify_table_body(~ 
                      .x %>%
                      mutate(participant_type = "Partner",
                             condition = "Depression") %>%       # add column
                      select(condition, participant_type, everything())          # move it first
  )

combined_dep_quick_table <- tbl_stack(
  tbls = list(
    m_adj_table_1_dep_quick_labeled,
    p_adj_table_1_dep_quick_labeled
  )
) %>%
  modify_caption("**Table 25. Combined Mother and Partner results for I know that if my child was very ill, a doctor would come quickly against Depression outcomess**") %>%
  modify_header(condition = "**Mental health condition**",
                participant_type = "**Participant Type**")

combined_dep_quick_table


 

### The doctor in the clinic is always helpful

filter_dep_help_m <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "as.numeric(m_alwys_help)"))
}

m_dep_table_help_clean <- filter_dep_help_m(m_dep_table_help)
m_adj_dep_table_1_help_clean <- filter_dep_help_m(m_adj_dep_table_1_help)
m_adj_dep_table_2_help_clean <- filter_dep_help_m(m_adj_dep_table_2_help)
m_adj_dep_table_3_help_clean <- filter_dep_help_m(m_adj_dep_table_3_help)

m_adj_table_1_dep_help <- tbl_merge(
  tbls = list(
    m_dep_table_help_clean,
    m_adj_dep_table_1_help_clean, 
    m_adj_dep_table_2_help_clean, 
    m_adj_dep_table_3_help_clean
  ),
  tab_spanner = c("**Unadjusted Model**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 3: Adjusted Odds Ratios for Depression for Mothers**"))

m_adj_table_1_dep_help


 

  
df_3_dep_m_help <- m_adj_dep_table_3_help_clean$table_body %>% mutate(model = "Model 3")
df_2_dep_m_help <- m_adj_dep_table_2_help_clean$table_body %>% mutate(model = "Model 2")
df_1_dep_m_help <- m_adj_dep_table_1_help_clean$table_body %>% mutate(model = "Model 1")
df_0_dep_m_help <- m_dep_table_help_clean$table_body       %>% mutate(model = "Unadjusted")

combined_df_dep_m_help <- bind_rows(df_3_dep_m_help,
                                    df_2_dep_m_help,
                                    df_1_dep_m_help,
                                    df_0_dep_m_help)

df_long_dep_m_help <- combined_df_dep_m_help %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_dep_m_help <- df_long_dep_m_help %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))

ggplot(df_long_dep_m_help, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",     # blue
      "Model 2" = "#f44973",     # green
      "Model 3" = "#f2923f"      # red
    ) ) +
  scale_x_continuous(
    breaks = seq(0.70, 2.20, by = 0.10),
    limits = c(0.70, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Mothers",
    color = "Model"
  ) +
  theme_minimal()
 


filter_dep_help_p <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "as.numeric(p_alwys_help)"))
}

p_dep_table_help_clean <- filter_dep_help_p(p_dep_table_help)
p_adj_dep_table_1_help_clean <- filter_dep_help_p(p_adj_dep_table_1_help)
p_adj_dep_table_2_help_clean <- filter_dep_help_p(p_adj_dep_table_2_help)
p_adj_dep_table_3_help_clean <- filter_dep_help_p(p_adj_dep_table_3_help)

p_adj_table_1_dep_help <- tbl_merge(
  tbls = list(
    p_dep_table_help_clean,
    p_adj_dep_table_1_help_clean, 
    p_adj_dep_table_2_help_clean, 
    p_adj_dep_table_3_help_clean
  ),
  tab_spanner = c("**Unadjusted Model**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 3: Adjusted Odds Ratios for Depression for Partners**"))

p_adj_table_1_dep_help


 

  
df_3_dep_p_help <- p_adj_dep_table_3_help_clean$table_body %>% mutate(model = "Model 3")
df_2_dep_p_help <- p_adj_dep_table_2_help_clean$table_body %>% mutate(model = "Model 2")
df_1_dep_p_help <- p_adj_dep_table_1_help_clean$table_body %>% mutate(model = "Model 1")
df_0_dep_p_help <- p_dep_table_help_clean$table_body       %>% mutate(model = "Unadjusted")

combined_df_dep_p_help <- bind_rows(df_3_dep_p_help,
                                    df_2_dep_p_help,
                                    df_1_dep_p_help,
                                    df_0_dep_p_help)

df_long_dep_p_help <- combined_df_dep_p_help %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_dep_p_help <- df_long_dep_p_help %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))

ggplot(df_long_dep_p_help, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",     # blue
      "Model 2" = "#f44973",     # green
      "Model 3" = "#f2923f"      # red
    ) ) +
  scale_x_continuous(
    breaks = seq(0.70, 2.20, by = 0.10),
    limits = c(0.70, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Partners",
    color = "Model"
  ) +
  theme_minimal()
 

  
m_adj_table_1_dep_help_labeled <- m_adj_table_1_dep_help %>%
  modify_table_body(~ 
                      .x %>%
                      mutate(participant_type = "Mother",
                             condition = "Depression") %>%        # add column
                      select(condition, participant_type, everything())          # move it to first column
  )

p_adj_table_1_dep_help_labeled <- p_adj_table_1_dep_help %>%
  modify_table_body(~ 
                      .x %>%
                      mutate(participant_type = "Partner",
                             condition = "Depression") %>%       # add column
                      select(condition, participant_type, everything())          # move it first
  )

combined_dep_help_table <- tbl_stack(
  tbls = list(
    m_adj_table_1_dep_help_labeled,
    p_adj_table_1_dep_help_labeled
  )
) %>%
  modify_caption("**Table 25. Combined Mother and Partner results for The doctor in the clinic is always helpful against Depression outcomes**") %>%
  modify_header(condition = "**Mental health condition**",
                participant_type = "**Participant Type**")

combined_dep_help_table

 

# Master tables of model outcomes  

## Anxiety

master_anx_table <- tbl_stack(
  tbls = list(
    combined_anx_conf_table,
    combined_anx_quick_table,
    combined_anx_help_table
  )
) %>%
  modify_caption("**Main predictor outcomes for all models and all questions, for Anxiety**") %>%
  modify_source_note("")


master_anx_table

saveRDS(
  master_anx_table,
  (here::here( "6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "6y_master_anx_mi_table.rds"))
)

 

## Depression

master_dep_table <- tbl_stack(
  tbls = list(
    combined_dep_conf_table,
    combined_dep_quick_table,
    combined_dep_help_table
  )
) %>%
  modify_caption("**Main predictor outcomes for all models and all questions, for Depression**") %>%
  modify_source_note("")


master_dep_table

saveRDS(
  master_dep_table,
  (here::here( "6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "6y_master_dep_mi_table.rds"))
)


# Master tables grouped by question  

## I don't have any confidence in the NHS

  

master_conf_table <- tbl_stack(
  tbls = list(
    combined_anx_conf_table,
    combined_dep_conf_table
  ))

master_conf_table

master_conf_table %>%
  as_gt() %>%
  gt::gtsave("master_conf_table.docx")
 

## I know that if my child was very ill, a doctor would come quickly

  
master_quick_table <- tbl_stack(
  tbls = list(
    combined_anx_quick_table,
    combined_dep_quick_table
  ))

master_quick_table

master_quick_table %>%
  as_gt() %>%
  gt::gtsave("master_quick_table.docx")

 

## The doctor in the clinic is always helpful

  
master_help_table <- tbl_stack(
  tbls = list(
    combined_anx_help_table,
    combined_dep_help_table
  ))

master_help_table


master_help_table %>%
  as_gt() %>%
  gt::gtsave("master_help_table.docx")
 

# Forest plots  


## Anxiety  

### Overall
  

df_long_anx_m_conf$group <- "Mothers"
df_long_anx_p_conf$group <- "Partners"
df_long_anx_m_quick$group <- "Mothers"
df_long_anx_p_quick$group <- "Partners"
df_long_anx_m_help$group <- "Mothers"
df_long_anx_p_help$group <- "Partners"


# Combine datasets
combined_long_anx <- bind_rows(df_long_anx_m_conf,
                               df_long_anx_p_conf,
                               df_long_anx_m_quick,
                               df_long_anx_p_quick,
                               df_long_anx_m_help,
                               df_long_anx_p_help)

combined_long_anx$outcome <- "anx"

combined_long_anx <- combined_long_anx %>%
  mutate(
    model = factor(model, levels = c("Model 3", "Model 2", "Model 1", "Unadjusted"))
  )

# Plot
plot_anx_MI <- ggplot(combined_long_anx, aes(x = estimate, y = variable_label, color = model, group = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.7), 
    height = 0.2
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(
    aes(
      x = conf.high + 0.04,
      label = sprintf("%.2f", estimate)
    ),                                      # Correctly closed aes()
    position = position_dodge(width = 0.7), # Properly placed outside aes()
    hjust = 0,
    size = 3,
    show.legend = FALSE
  ) +                                       # Correctly closed geom_text()
  scale_y_discrete(labels = \(x) stringr::str_wrap(x, width = 25)) +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#415070",
      "Model 1"    = "#c66146",
      "Model 2"    = "#b65266",
      "Model 3"    = "#68527f"
    ),                                      # Correctly closed values vector
    breaks = c("Unadjusted", "Model 1", "Model 2", "Model 3"), 
    labels = c(
      "Unadjusted model",
      "Model 1: Baseline demographics",
      "Model 2: Health experiences",
      "Model 3: Social networks"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0.50, 2.40, by = 0.20),   # Kept at 2.40 so your text numbers don't clip off-screen
    limits = c(0.50, 2.40),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    title = "Association of trust in doctors against healthcare access \n for anxiety, for mothers and partners, at 6 years",
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Item",
    color = "Model",
    caption = paste(
      "Model 1 (Baseline demographics). Adjusted for: age, parity, marital status, religion, socioeconomic position, \n educational attainment, home ownership status, crown crisp anxiety score and crown crisp depression score",
      "Model 2 (Health experiences). Additionally adjusted for: self-rated health and recent doctor change",
      "Model 3 (Social networks). Additionally adjusted for: social support score and weighted life events ",
      sep = "\n"
    )
  ) +
  facet_wrap(~ group, ncol = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

plot_anx_MI


ggsave(
  filename = (here::here("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "6y_mi_anx_forest_plot.png")),
  plot = plot_anx_MI,
  width = 10,
  height = 6,
  dpi = 300
)



 

### I don't have any confidence in the NHS

  

df_long_anx_m_conf$group <- "Mothers"
df_long_anx_p_conf$group <- "Partners"



# Combine datasets
combined_long_anx_conf <- bind_rows(df_long_anx_m_conf,
                                    df_long_anx_p_conf)


# Plot
plot_anx_conf_MI <- ggplot(combined_long_anx_conf, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(
    aes(x = conf.high + 0.05,
        label = round(estimate, 2)),
    position = position_dodge(width = 0.7),
    hjust = -0.3,
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#F1C40F",
      "Model 1" = "#854f99",
      "Model 2" = "#f44973",
      "Model 3" = "#f2923f"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0.50, 2.20, by = 0.20),
    limits = c(0.50, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 25))+
  labs(
    title = "Associations between healthcare access for Anxiety and Confidence in the NHS (mothers vs partners)",
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Item",
    color = "Model"
  ) +
  facet_wrap(~ group, ncol = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

print(plot_anx_conf_MI)

ggsave(
  filename = "l_forest_anx_conf_MI.png",
  plot = plot_anx_conf_MI,
  width = 10,
  height = 6,
  dpi = 300
)

 


### I know that if my child was very ill, a doctor would come quickly

  

df_long_anx_m_quick$group <- "Mothers"
df_long_anx_p_quick$group <- "Partners"



# Combine datasets
combined_long_anx_quick <- bind_rows(df_long_anx_m_quick,
                                     df_long_anx_p_quick)


# Plot
plot_anx_quick_MI <- ggplot(combined_long_anx_quick, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(
    aes(x = conf.high + 0.05,
        label = round(estimate, 2)),
    position = position_dodge(width = 0.7),
    hjust = -0.3,
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#F1C40F",
      "Model 1" = "#854f99",
      "Model 2" = "#f44973",
      "Model 3" = "#E67E22"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0.50, 2.20, by = 0.20),
    limits = c(0.50, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 25))+
  labs(
    title = "Associations between healthcare access for Anxiety and doctor quickness (mothers vs partners)",
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Item",
    color = "Model"
  ) +
  facet_wrap(~ group, ncol = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

print(plot_anx_quick_MI)

ggsave(
  filename = "l_forest_anx_quick_MI.png",
  plot = plot_anx_quick_MI,
  width = 10,
  height = 6,
  dpi = 300
)

 


### The doctor in the clinic is always helpful
  

df_long_anx_m_help$group <- "Mothers"
df_long_anx_p_help$group <- "Partners"



# Combine datasets
combined_long_anx_help <- bind_rows(df_long_anx_m_help,
                                    df_long_anx_p_help)


# Plot
plot_anx_help_MI <- ggplot(combined_long_anx_help, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(
    aes(x = conf.high + 0.05,
        label = round(estimate, 2)),
    position = position_dodge(width = 0.7),
    hjust = -0.3,
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#F1C40F",
      "Model 1" = "#854f99",
      "Model 2" = "#f44973",
      "Model 3" = "#E67E22"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0.50, 2.20, by = 0.20),
    limits = c(0.50, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 25))+
  labs(
    title = "Associations between healthcare access for Anxiety and doctor helpfulness (mothers vs partners)",
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Item",
    color = "Model"
  ) +
  facet_wrap(~ group, ncol = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

print(plot_anx_help_MI)

ggsave(
  filename = "l_forest_anx_help_MI.png",
  plot = plot_anx_help_MI,
  width = 10,
  height = 6,
  dpi = 300
)


 



## Depression  

### Overall

  
df_long_dep_m_conf$group <- "Mothers"
df_long_dep_p_conf$group <- "Partners"
df_long_dep_m_quick$group <- "Mothers"
df_long_dep_p_quick$group <- "Partners"
df_long_dep_m_help$group <- "Mothers"
df_long_dep_p_help$group <- "Partners"


# Combine datasets
combined_long_dep <- bind_rows(df_long_dep_m_conf,
                               df_long_dep_p_conf,
                               df_long_dep_m_quick,
                               df_long_dep_p_quick,
                               df_long_dep_m_help,
                               df_long_dep_p_help)

combined_long_dep$outcome <- "Depression"

combined_long_anx$outcome <- "anx"

combined_long_anx <- combined_long_anx %>%
  mutate(
    model = factor(model, levels = c("Model 3", "Model 2", "Model 1", "Unadjusted"))
  )

# Plot
plot_dep_MI <- ggplot(combined_long_dep, aes(x = estimate, y = variable_label, color = model, group = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.7), 
    height = 0.2
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(
    aes(
      x = conf.high + 0.04,
      label = sprintf("%.2f", estimate)
    ),                                      # Correctly closed aes()
    position = position_dodge(width = 0.7), # Properly placed outside aes()
    hjust = 0,
    size = 3,
    show.legend = FALSE
  ) +                                       # Correctly closed geom_text()
  scale_y_discrete(labels = \(x) stringr::str_wrap(x, width = 25)) +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#415070",
      "Model 1"    = "#c66146",
      "Model 2"    = "#b65266",
      "Model 3"    = "#68527f"
    ),                                      # Correctly closed values vector
    breaks = c("Unadjusted", "Model 1", "Model 2", "Model 3"), 
    labels = c(
      "Unadjusted model",
      "Model 1: Baseline demographics",
      "Model 2: Health experiences",
      "Model 3: Social networks"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0.50, 2.40, by = 0.20),   # Kept at 2.40 so your text numbers don't clip off-screen
    limits = c(0.50, 2.40),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    title = "Association of trust in doctors against healthcare access \n for depression, for mothers and partners, at 6 years",
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Item",
    color = "Model",
    caption = paste(
      "Model 1 (Baseline demographics). Adjusted for: age, parity, marital status, religion, socioeconomic position, \n educational attainment, home ownership status, crown crisp anxiety score and crown crisp depression score",
      "Model 2 (Health experiences). Additionally adjusted for: self-rated health and recent doctor change",
      "Model 3 (Social networks). Additionally adjusted for: social support score and weighted life events ",
      sep = "\n"
    )
  ) +
  facet_wrap(~ group, ncol = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

plot_dep_MI

ggsave(
  filename = (here::here("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "6y_mi_dep_forest_plot.png")),
  plot = plot_dep_MI,
  width = 10,
  height = 6,
  dpi = 300
)

 


### I don't have any confidence in the NHS

  

df_long_dep_m_conf$group <- "Mothers"
df_long_dep_p_conf$group <- "Partners"



# Combine datasets
combined_long_dep_conf <- bind_rows(df_long_dep_m_conf,
                                    df_long_dep_p_conf)


# Plot
plot_dep_conf_MI <- ggplot(combined_long_dep_conf, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(
    aes(x = conf.high + 0.05,
        label = round(estimate, 2)),
    position = position_dodge(width = 0.7),
    hjust = -0.3,
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#F1C40F",
      "Model 1" = "#854f99",
      "Model 2" = "#f44973",
      "Model 3" = "#f2923f"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0.50, 2.20, by = 0.20),
    limits = c(0.50, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 25))+
  labs(
    title = "Associations between healthcare access for Depression and Confidence in the NHS (mothers vs partners)",
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Item",
    color = "Model"
  ) +
  facet_wrap(~ group, ncol = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

print(plot_dep_conf_MI)

ggsave(
  filename = "l_forest_dep_conf_MI.png",
  plot = plot_dep_conf_MI,
  width = 10,
  height = 6,
  dpi = 300
)

 


### I know that if my child was very ill, a doctor would come quickly
  

df_long_dep_m_quick$group <- "Mothers"
df_long_dep_p_quick$group <- "Partners"



# Combine datasets
combined_long_dep_quick <- bind_rows(df_long_dep_m_quick,
                                     df_long_dep_p_quick)


# Plot
plot_dep_quick_MI <- ggplot(combined_long_dep_quick, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(
    aes(x = conf.high + 0.05,
        label = round(estimate, 2)),
    position = position_dodge(width = 0.7),
    hjust = -0.3,
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#F1C40F",
      "Model 1" = "#854f99",
      "Model 2" = "#f44973",
      "Model 3" = "#E67E22"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0.50, 2.20, by = 0.20),
    limits = c(0.50, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 25))+
  labs(
    title = "Associations between healthcare access for Depression and doctor quickness (mothers vs partners)",
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Item",
    color = "Model"
  ) +
  facet_wrap(~ group, ncol = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

print(plot_dep_quick_MI)

ggsave(
  filename = "l_forest_dep_quick_MI.png",
  plot = plot_dep_quick_MI,
  width = 10,
  height = 6,
  dpi = 300
)


 

### The doctor in the clinic is always helpful
  

df_long_dep_m_help$group <- "Mothers"
df_long_dep_p_help$group <- "Partners"



# Combine datasets
combined_long_dep_help <- bind_rows(df_long_dep_m_help,
                                    df_long_dep_p_help)


# Plot
plot_dep_help_MI <- ggplot(combined_long_dep_help, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(
    aes(x = conf.high + 0.05,
        label = round(estimate, 2)),
    position = position_dodge(width = 0.7),
    hjust = -0.3,
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#F1C40F",
      "Model 1" = "#854f99",
      "Model 2" = "#f44973",
      "Model 3" = "#E67E22"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0.50, 2.20, by = 0.20),
    limits = c(0.50, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 25))+
  labs(
    title = "Associations between healthcare access for Anxiety and doctor helpfulness (mothers vs partners)",
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Item",
    color = "Model"
  ) +
  facet_wrap(~ group, ncol = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

print(plot_dep_help_MI)

ggsave(
  filename = "l_forest_dep_help_MI.png",
  plot = plot_dep_help_MI,
  width = 10,
  height = 6,
  dpi = 300
)



 


# Combined
  

combined_all <- bind_rows(combined_long_anx, combined_long_dep)

plot_all_MI <- ggplot(combined_all, aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(
    aes(label = paste0("N=", N), x = conf.high + 0.05),
    position = position_dodge(width = 0.7),
    hjust = -0.3,
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#F1C40F",
      "Model 1" = "#854f99",
      "Model 2" = "#f44973",
      "Model 3" = "#f2923f"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0.50, 2.20, by = 0.20),
    limits = c(0.50, 2.20),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    title = "Trust vs Healthcare access for Anxiety and Depression at 6 years",
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Item",
    color = "Model"
  ) +
  facet_grid(outcome ~ group) +  
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

print(plot_all_MI)

ggsave(
  filename = (here::here("6-year-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "21m_mi_master_forest_plot.png")),
  plot = plot_all_MI,
  width = 10,
  height = 6,
  dpi = 300
)

