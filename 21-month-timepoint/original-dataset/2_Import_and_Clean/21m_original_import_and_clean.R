# script:  for data import and cleaning for original dataset, at 21 month timepoint

library(here) # to create relative paths

 
source(here::here("21-month-timepoint/original-dataset/1_Packages", "21m_packages.R"))

stata_data <- read_dta(here::here("21-month-timepoint/original-dataset/Data", "g_data_v21_mis.dta"))

#-------------------------------------------------------------------------------
# creating a data frame
all_data <- data.frame(stata_data)

#-------------------------------------------------------------------------------
# duplicating and renaming variables
cols_to_duplicate <- c("a006", "b032", "c645a", "d813", "g010",
                       "g020", "g021","g036",
                       "g226", "g227", "g345", "g994",
                       "g950", "g951", "g952", "g953", "g954", "g955",
                       "g517", "g980", "g981", "g948", 
                       "c755", "g994", "g269", "g275")


newcol_names <- c("home_own_status", "parity", "edu_level", "relig", "mum_hlth",
                  "anx", "dep", "per",
                  "soc_supp", "soc_supp_mis", "weigh_life", "age",
                  "supp", "symp", "int", "help", "easy", "time",
                  "mar_status", "mum_comp", "ptner_comp", "dr_change",
                  "soc_class", "age", "anx_score", "dep_score")

covariates <- c("home_own_status", "parity", "edu_level", "relig",
                "mum_hlth", "soc_supp", "soc_supp_mis", "weigh_life", "mar_status",
                "dr_change", "age", "soc_class", "age")

exposures <- c("supp", "symp", "int", "help", "easy", "time")

outcomes <- c("anx", "dep", "per")


must_keep <- c("aln", "mz010a", "mum_in_alsp", "mum_in_core", 
               "mum_enrol_status", 
               "mum_and_preg_enrolled", "mz005l", "mz005m", "mz013", "mz014",
               "mz028b", "a006", "a525", "b032", "b650", "b663", "b665", "b667", 
               "c645a", "c755", "c765", "bestgest")

#-------------------------------------------------------------------------------
#creating a data frame with all variables and all renamed variables
all_data <- all_data
all_data[newcol_names] <- all_data[cols_to_duplicate]

#-------------------------------------------------------------------------------
#creating a data frame with all variables (but replacing orignally named to renamed)

df_working <- all_data %>%
  select("aln", "mz010a", "mum_in_alsp", "mum_in_core", "mum_enrol_status",
         "mum_and_preg_enrolled", "mz005l", "mz005m", "mz013", "mz014",
         "mz028b", "a525", "b650", "b663", "b665", "b667", "c755", "c765",
         "bestgest", "home_own_status", "parity", "edu_level", "relig",
         "mum_hlth", "anx", "dep", "per", "soc_supp", "soc_supp_mis",
         "weigh_life", "mar_status",
         "dr_change", "supp", "symp", "int", "help", "easy", "time",
         "mum_comp", "ptner_comp", "age", "soc_class", "age", "anx_score", "dep_score")

# this data frame contains all variables original to the dataset but my outcomes,
# exposures and covariates have been renamed for clarity and ease
# changing haven.labelled to numeric

df_working <- df_working %>%
  mutate(across(where(~ inherits(., "haven_labelled")), ~ as.numeric(.)))

#---------------------------------------------------------------------------

# filtering so only mothers under 48 are included
df_working <- df_working %>%
  filter(age < 48 | is.na(age))

#also grouping 17>= 
df_working <- df_working %>%
  mutate(
    age = case_when(
      age %in% c(15, 17) ~ 17,
      TRUE ~ age
    )
  )


#-------------------------------------------------------------------------------
# removing all irrelevant numbers
# only including mothers
df_working$mum_comp[df_working$mum_comp == -1] <- NA
df_working$mum_comp[df_working$mum_comp == 2] <- NA

df_working[is.na(df_working$mum_comp), ] <- NA

# exposures
df_working$supp[df_working$supp == -1] <- NA
df_working$symp[df_working$symp == -1] <- NA
df_working$int[df_working$int == -1] <- NA
df_working$help[df_working$help == -1] <- NA
df_working$easy[df_working$easy == -1] <- NA
df_working$time[df_working$time == -1] <- NA

# outcomes
df_working$anx[df_working$anx == -1] <- NA
df_working$dep[df_working$dep == -1] <- NA
df_working$per[df_working$per == -1] <- NA

# covariates
df_working$home_own_status[df_working$home_own_status == -1] <- NA
df_working$home_own_status[df_working$home_own_status == -7] <- NA
df_working$parity[df_working$parity == -1] <- NA
df_working$parity[df_working$parity == -2] <- NA
df_working$parity[df_working$parity == -7] <- NA
df_working$edu_level[df_working$edu_level == -1] <- NA
df_working$mum_hlth[df_working$mum_hlth == -1] <- NA
df_working$soc_supp[df_working$soc_supp == -1] <- NA
df_working$soc_supp_mis[df_working$soc_supp_mis == -1] <- NA
df_working$weigh_life[df_working$weigh_life == -1] <- NA
df_working$dr_change[df_working$dr_change == -1] <- NA
df_working$age[df_working$age == -1] <- NA
df_working$mar_status[df_working$mar_status == -1] <- NA
df_working$relig[df_working$relig == -1] <- NA
df_working$soc_class[df_working$soc_class == -1] <- NA
df_working$soc_class[df_working$soc_class == 65] <- NA
df_working$age[df_working$age == -1] <- NA

df_working$anx_score[df_working$anx_score == -1] <- NA
df_working$dep_score[df_working$dep_score == -1] <- NA


#-------------------------------------------------------------------------------
# recode all relevant variables so that the numbers are a bit more intuitive
# exposures - the more positive the atittudes towards doctors, the higher the score
df_working$supp <- as.numeric(as.character(df_working$supp))

df_working <- df_working %>%
  mutate(supp = ifelse(supp == 1, 4,
                       ifelse(supp == 2, 3,
                              ifelse(supp == 3, 2,
                                     ifelse(supp == 4, 1, NA_real_)))))


df_working$symp <- as.numeric(as.character(df_working$symp))


df_working <- df_working %>%
  mutate(symp = ifelse(symp == 1, 4,
                       ifelse(symp == 2, 3,
                              ifelse(symp == 3, 2,
                                     ifelse(symp == 4, 1, NA_real_)))))


df_working$int <- as.numeric(as.character(df_working$int))


df_working <- df_working %>%
  mutate(int = ifelse(int == 1, 4,
                      ifelse(int == 2, 3,
                             ifelse(int == 3, 2,
                                    ifelse(int == 4, 1, NA_real_)))))

df_working$help <- as.numeric(as.character(df_working$help))


df_working <- df_working %>%
  mutate(help = ifelse(help == 1, 4,
                       ifelse(help == 2, 3,
                              ifelse(help == 3, 2,
                                     ifelse(help == 4, 1, NA_real_)))))

df_working$easy <- as.numeric(as.character(df_working$easy))


df_working <- df_working %>%
  mutate(easy = ifelse(easy == 1, 4,
                       ifelse(easy == 2, 3,
                              ifelse(easy == 3, 2,
                                     ifelse(easy == 4, 1, NA_real_)))))

df_working$time <- as.numeric(as.character(df_working$time))


df_working <- df_working %>%
  mutate(time = ifelse(time == 1, 4,
                       ifelse(time == 2, 3,
                              ifelse(time == 3, 2,
                                     ifelse(time == 4, 1, NA_real_)))))

# covariates

# mum's evaluation of own health - higher number, better health
df_working$mum_hlth <- as.numeric(as.character(df_working$mum_hlth))

df_working <- df_working %>%
  mutate(mum_hlth = case_when(
    mum_hlth == 1 ~ 4,
    mum_hlth == 2 ~ 3,
    mum_hlth == 3 ~ 2,
    mum_hlth == 4 ~ 1,
    TRUE ~ NA_real_
  ))


# whether or not Dr has been changed
df_working$dr_change <- as.numeric(as.character(df_working$dr_change))

df_working <- df_working %>%
  mutate(dr_change = case_when(
    dr_change == 1 ~ 1,
    dr_change == 2 ~ 0,
    TRUE ~ NA_real_
  ))



# marital status - married or not married
df_working$mar_status <- as.numeric(as.character(df_working$mar_status))

df_working <- df_working %>%
  mutate(mar_status = case_when(
    mar_status == 1 ~ 0,
    mar_status == 2 ~ 0,
    mar_status == 3 ~ 0,
    mar_status == 4 ~ 0,
    mar_status == 5 ~ 1,
    mar_status == 6 ~ 1,
    TRUE ~ NA_real_
  ))

# home ownership status - owned, mortgaged, privately rented, publicly rented, other
df_working$home_own_status <- as.numeric(as.character(df_working$home_own_status))

df_working <- df_working %>%
  mutate(home_own_status = case_when(
    home_own_status == 6 ~ 0,
    home_own_status == 2 ~ 1,
    home_own_status == 5 ~ 1,
    home_own_status == 3 ~ 2,
    home_own_status == 4 ~ 2,
    home_own_status == 0 ~ 3,
    home_own_status == 1 ~ 4,
    TRUE ~ NA_real_
  ))


# religion - Christian, non-Christian or none
df_working$relig <- as.numeric(as.character(df_working$relig))

df_working <- df_working %>%
  mutate(relig = case_when(
    relig == 0 ~ 2,
    relig == 1 ~ 0,
    relig == 2 ~ 0,
    relig == 3 ~ 0,
    relig == 4 ~ 0,
    relig == 5 ~ 0,
    relig == 6 ~ 0,
    relig == 7 ~ 1,
    relig == 8 ~ 1,
    relig == 9 ~ 1,
    relig == 10 ~ 1,
    relig == 11 ~ 1,
    relig == 12 ~ 1,
    relig == 13 ~ 1,
    TRUE ~ NA_real_
  ))

# mums education level - O-level, vocational, CSE, GCSE, none or a-levels or university degree
df_working <- df_working %>%
  mutate(edu_level = case_when(
    edu_level == 1 ~ 0,
    edu_level == 2 ~ 0,
    edu_level == 3 ~ 0,
    edu_level == 4  ~ 1,
    edu_level == 5  ~ 2,
    TRUE ~ NA_real_
  ))

# parity
df_working <- df_working %>%
  mutate(parity = case_when(
    parity == 0 ~ "0",
    parity == 1 ~ "1",
    parity == 2 ~ "2",
    parity == 3 ~ "3",
    parity == 4 ~ "4",
    parity == 5 ~ "5",
    parity %in% c(6, 7, 8, 11, 13, 22) ~ "6<=",
    TRUE ~ NA_character_
  ),
  parity = factor(parity, levels = c("0", "1", "2", "3", "4", "5", "6<="))
  )

# socioeconomic position - split into professional managerial/other - reversing the scale 
df_working <- df_working %>%
  mutate(soc_class = case_when(
    soc_class == 1 ~ 1,
    soc_class == 2 ~ 1,
    soc_class == 3 ~ 0,
    soc_class == 4  ~ 0,
    soc_class == 5  ~ 0,
    soc_class == 6  ~ 0,
    TRUE ~ NA_real_
  ))

# outcomes
# anxiety - Yes and saw Dr, yes but did not see Dr, no
df_working$anx <- as.numeric(as.character(df_working$anx))

df_working$anx <- ifelse(df_working$anx == 1, 1,- #yes and saw dr
                           ifelse(df_working$anx == 2, 0, - #yes but no dr
                                    ifelse(df_working$anx == 3, 2, df_working$anx))) #no

#depression - Yes and saw Dr, yes but did not see Dr, no
df_working$dep <- as.numeric(as.character(df_working$dep))

df_working$dep <- ifelse(df_working$dep == 1, 1,
                         ifelse(df_working$dep == 2, 0,
                                ifelse(df_working$dep == 3, 2, df_working$dep)))
#periods - Yes and saw Dr, yes but did not see Dr, no
df_working$per <- as.numeric(as.character(df_working$per))

df_working$per <- ifelse(df_working$per == 1, 1,
                         ifelse(df_working$per == 2, 0,
                                ifelse(df_working$per == 3, 2, df_working$per)))

#-------------------------------------------------------------------------------
#adding labels to values

# mums religion
df_working <- df_working %>%
  mutate(relig_name = case_when(
    relig == 0 ~ "Christian",
    relig == 1 ~ "Non-Christian",
    relig == 2 ~ "None"
  ))

# mums education level
df_working <- df_working %>%
  mutate(educ_level_name = case_when(
    edu_level == 0 ~ "O-level, Vocational, CSE, GCSE or none",
    edu_level == 1 ~ "A-level",
    edu_level == 2 ~ "Degree"
  ))

# mums evaluation of own health
df_working <- df_working %>%
  mutate(mums_hlth_eval = case_when(
    mum_hlth == 1 ~ "Never well",
    mum_hlth == 2 ~ "Often unwell",
    mum_hlth == 3 ~ "Mostly well",
    mum_hlth == 4  ~ "Fit and well"
  ))


# has mum changed Dr?
df_working <- df_working %>%
  mutate(dr_changed_ever = case_when(
    dr_change == 0 ~ "No recent Dr change",
    dr_change == 1 ~ "Recent Dr change"
  ))


# mums marital status
df_working <- df_working %>%
  mutate(mar_status_ans = case_when(
    mar_status == 0 ~ "Not currently married",
    mar_status == 1 ~ "Married"
  ))

# home ownership status
df_working <- df_working %>%
  mutate(home_own_status_ans = case_when(
    home_own_status == 0 ~ "Other",
    home_own_status == 1 ~ "Publicly rented",
    home_own_status == 2 ~ "Privately rented",
    home_own_status == 3 ~ "Mortgaged",
    home_own_status == 4 ~ "Owned"
  ))

df_working <- df_working %>%
  mutate(soc_class_ans = case_when(
    soc_class == 0 ~ "Professional/Managerial",
    soc_class == 1 ~ "Other",
  ))

#outcomes
df_working <- df_working %>%
  mutate(anx_ans = case_when(
    anx == 0 ~ "Yes but saw no Dr",
    anx == 1 ~ "Yes and saw Dr",
    anx == 2 ~ "No"
  ))

df_working <- df_working %>%
  mutate(dep_ans = case_when(
    dep == 0 ~ "Yes but saw no Dr",
    dep == 1 ~ "Yes and saw Dr",
    dep == 2 ~ "No"
  ))

df_working <- df_working %>%
  mutate(per_ans = case_when(
    per == 0 ~ "Yes but saw no Dr",
    per == 1 ~ "Yes and saw Dr",
    per == 2 ~ "No"
  ))


#exposures
df_working <- df_working %>%
  mutate(supp_ans = case_when(
    supp == 1 ~ "Never",
    supp == 2 ~ "Sometimes",
    supp == 3 ~ "Usually",
    supp == 4  ~ "Always"
  ))

df_working <- df_working %>%
  mutate(symp_ans = case_when(
    symp == 1 ~ "Never",
    symp == 2 ~ "Sometimes",
    symp == 3 ~ "Usually",
    symp == 4  ~ "Always"
  ))

df_working <- df_working %>%
  mutate(int_ans = case_when(
    int == 1 ~ "Never",
    int == 2 ~ "Sometimes",
    int == 3 ~ "Usually",
    int == 4  ~ "Always"
  ))

df_working <- df_working %>%
  mutate(help_ans = case_when(
    help == 1 ~ "Never",
    help == 2 ~ "Sometimes",
    help == 3 ~ "Usually",
    help == 4  ~ "Always"
  ))

df_working <- df_working %>%
  mutate(easy_ans = case_when(
    easy == 1 ~ "Never",
    easy == 2 ~ "Sometimes",
    easy == 3 ~ "Usually",
    easy == 4  ~ "Always"
  ))

df_working <- df_working %>%
  mutate(time_ans = case_when(
    time == 1 ~ "Never",
    time == 2 ~ "Sometimes",
    time == 3 ~ "Usually",
    time == 4  ~ "Always"
  ))


col_order <- c("aln", "mz010a", "mum_in_alsp", "mum_in_core", "mum_enrol_status",
               "mum_and_preg_enrolled", "mz005l", "mz005m", "mz013", "mz014",
               "mz028b", "a525", "b650", "b663", "b665", "b667", "c755", "c765",
               "bestgest", "anx", "anx_ans", "dep",
               "dep_ans", "per", "per_ans", "supp",
               "supp_ans", "symp", "symp_ans", "int", "int_ans", "help",
               "help_ans", "easy", "easy_ans", "time", "time_ans",
               "home_own_status", "home_own_status_ans", "parity", "edu_level", "educ_level_name",
               "ethn_group", "ethn_name", "relig", "relig_name", "mum_hlth", "mums_hlth_eval",
               "soc_supp", "soc_supp_mis", "weigh_life", "mar_status", "mar_status_ans",
               "dr_change", "dr_changed_ever", "age", "soc_class", "soc_class_ans", "anx_score", 
               "dep_score", "mum_comp", "ptner_comp")

df_working <- df_working %>%
  select(any_of(col_order))

# adding labels to columns
df_working <- df_working %>%
  set_variable_labels(
    home_own_status = "Home ownership",
    edu_level = "Education",
    mum_hlth = "Mum's evaluation of own health",
    anx = "Have you had anxiety since your toddler was 8mths?",
    dep = "Have you had depression since your toddler was 8mths?",
    per = "Have you had problems with your periods since your toddler was 8mths?",
    soc_supp = "Social support score",
    soc_supp_mis = "Social support score",
    weigh_life = "Weighted life events score",
    dr_change = "Mum changed Dr after child was born",
    mar_status = "Marital status",
    supp = "Would you describe your GP as supportive?",
    symp = "Would you describe your GP as sympathetic?",
    int = "Would you describe your GP as interested?",
    help = "Would you describe your GP as helpful?",
    easy = "Would you describe your GP as easy to talk to?",
    time = "Would you describe your GP as willing to give you time?",
    mum_comp = "Mother completed questionnaire",
    ptner_comp = "Partner completed questionnaire",
    age = "Age",
    parity = "Parity",
    soc_class = "Social class",
    soc_class_ans = "Social class",
    relig = "Mum's religion",
    relig_name = "Mum's religion",
    home_own_status_ans = "Home ownership",
    educ_level_name = "Education",
    mums_hlth_eval = "Mum's evaluation of her health",
    dr_changed_ever = "Recent dr change",
    mar_status_ans = "Marital status",
    anx_ans = "Have you had anxiety since your toddler was 8mths?",
    dep_ans = "Have you had depression since your toddler was 8mths?",
    per_ans = "Have you had problems with your periods since your toddler was 8mths?",
    supp_ans = "Would you describe your GP as supportive?",
    symp_ans = "Would you describe your GP as sympathetic?",
    int_ans = "Would you describe your GP as interested?",
    help_ans = "Would you describe your GP as helpful?",
    easy_ans = "Would you describe your GP as easy to talk to?",
    time_ans = "Would you describe your GP as willing to give you time?",
    anx_score = "Crown crisp anxiety score",
    dep_score = "Crown crisp depression score"
  )


write.csv(df_working, here::here("21-month-timepoint/original-dataset/2_Import_and_Clean", "df_working.csv"))
