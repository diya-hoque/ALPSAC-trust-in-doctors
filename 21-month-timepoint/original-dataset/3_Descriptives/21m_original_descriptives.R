# script: data descriptives for original and complete-case datasets, at 21 month timepoint
#-------------------------------------------------------------------------------
source(here::here("21-month-timepoint/original-dataset/1_Packages", "21m_packages.R"))

df_working <- read.csv(here::here("21-month-timepoint/original-dataset/2_Import_and_Clean", "df_working.csv"))


#-------------------------------------------------------------------------------
# Correlation Matrix ----
#to assess correlatedness of predictor variables 
df_cor <- df_working %>%
  select(supp, symp, int, help, easy, time) %>%
  drop_na()

cor_matrix_num <- cor(df_cor)

cor_labels <- c("Supportive", "Sympathetic", "Interested", "Helpful", "Easy to talk to", "Willing to give time")

col <- c("#9d0053", "#c8257b", "#da4493", "#e164a6",  "#f584c0","#fcb1d9","#fcd7d9", "#fce6d9", "#fce6d9", "#f8ffaf", 
         "#e5f25c", "#dae83d", "#ccda28", "#bccb0b", "#aab809", "#899316", "#737a22", "#5b6110", "#43470c", "#24260f")

breaks <- c(-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

par(mar = c(6, 6, 8, 2), oma = c(4, 0, 4, 0))

corrplot(
  cor_matrix_num,
  method = "circle",
  type = "full",
  col = rev(col),
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

#-------------------------------------------------------------------------------
# Transforming main predictor variable ----

# creating sum attitudes score because these variables are highly correlated
df_subset <- df_working %>% select(supp, symp, int, help, easy, time)

# removing those that answered less than 4/6 of questions and then multiplying the mean scores by 6 (total number of questions)
df_subset <- df_subset %>%
  mutate(
    sum_attitude = rowMeans(select(., everything()), na.rm = TRUE),
    sum_attitudecountNA = rowSums(is.na(select(., everything()))),
    sum_attitude = ifelse(sum_attitudecountNA > 2, NA, sum_attitude),
    sum_attitude = sum_attitude * 6 
  ) %>%
  select(sum_attitude, sum_attitudecountNA) 

if (nrow(df_working) == nrow(df_subset)) {
  df_working <- bind_cols(df_working, df_subset)
} else {
  stop("Error: Mismatch in row count between df_working and df_subset")
}

#-------------------------------------------------------------------------------
# Cleaning for cont tables ----
#creating categorical variables of the continuous variables so that they work in the contingency tables
df_working$dr_changed_ever <- factor(df_working$dr_changed_ever,
                                     levels = c("Recent Dr change", "No recent Dr change"))

df_working$sum_sum_attitude <- cut(df_working$sum_attitude,
                                   breaks = c(3, 9, 14, 19, 24),
                                   labels = c("4-9", "10-14", "15-19", "19-24"))

df_working$sum_age <- cut(df_working$age,
                          breaks = c(15, 20, 25, 30, 35, 40, 46),
                          labels = c("20>=", "21-25", "26-30", "31-35", "36-40", "41-46"))

df_working$sum_soc_supp <- cut(df_working$soc_supp,
                               breaks = c(0, 4, 11, 18, 25, 30),
                               labels = c("0-4", "5-11", "12-18", "19-25", "26-30"))

df_working$sum_soc_supp_mis <- cut(df_working$soc_supp_mis,
                                   breaks = c(0, 4, 11, 18, 25, 30),
                                   labels = c("0-4", "5-11", "12-18", "19-25", "26-30"))

df_working$sum_weigh_life <- cut(df_working$weigh_life,
                                 breaks = c(0, 9, 19, 29, 38),
                                 labels = c("0-9", "10-19", "20-29", "30-38"))

df_working$sum_anx_score <- cut(df_working$anx_score,
                                breaks = c(0, 4, 8, 12, 16),
                                labels = c("0-4", "5-8", "9-12", "13-16"))

df_working$sum_dep_score <- cut(df_working$dep_score,
                                breaks = c(0, 4, 8, 12, 16),
                                labels = c("0-4", "5-8", "9-12", "13-16"))

# rename variables for contingency table
df_working <- df_working %>%
  set_variable_labels(
    home_own_status_ans = "Home ownership",
    sum_age = "Age",
    educ_level_name = "Education",
    mar_status_ans = "Marital status",
    mums_hlth_eval = "Evaluation of own health",
    sum_sum_attitude = "Attitudes towards doctors",
    sum_soc_supp_mis = "Social support score",
    sum_weigh_life = "Weighted life events score",
    dr_changed_ever = "Recent dr change",
    sum_anx_score = "Crown crisp anxiety score",
    sum_dep_score = "Crown crisp depression score"
  )

#-------------------------------------------------------------------------------
# Contingency tables ----

## main predictor variable only
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

cont_tab_sum_all <- tbl_merge(
  tbls = list(cont_tab_sum_anx, cont_tab_sum_dep, cont_tab_sum_per),
  tab_spanner = c("**Anxiety**", "**Depression**", "**Problems with Periods**")
) %>%
  modify_caption("**Table 4. Summary Statistics for Sum Attitudes by Anxiety, Depression, and Menstrual Problems**")

cont_tab_sum_all

## all covariates

# ordering the outcomes so that it produces a better looking contingency table
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


## Combined contingency table - all covariates

### Anxiety
# contingency tables all covariates
cont_tab_all_anx <- df_working %>%
  tbl_summary(
    by = anx_ans,
    include = c(
      sum_sum_attitude, sum_age, parity, soc_class_ans, educ_level_name, home_own_status_ans,  mar_status_ans, relig_name, mums_hlth_eval, dr_changed_ever, sum_soc_supp_mis, sum_weigh_life, sum_anx_score, sum_dep_score 
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
      sum_sum_attitude, sum_age, parity, soc_class, educ_level_name, home_own_status_ans,  mar_status_ans, relig_name, mums_hlth_eval, dr_changed_ever, sum_soc_supp_mis, sum_weigh_life, sum_anx_score, sum_dep_score 
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
      sum_sum_attitude, sum_age, parity, soc_class, educ_level_name, home_own_status_ans,  mar_status_ans, relig_name, mums_hlth_eval, dr_changed_ever, sum_soc_supp_mis, sum_weigh_life, sum_anx_score, sum_dep_score 
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
  (here::here("21-month-timepoint/original-dataset/3_Descriptives", "df_working.csv"))
)

write.csv(df_working, (here::here("21-month-timepoint/original-dataset/3_Descriptives", "df_working.csv")))

