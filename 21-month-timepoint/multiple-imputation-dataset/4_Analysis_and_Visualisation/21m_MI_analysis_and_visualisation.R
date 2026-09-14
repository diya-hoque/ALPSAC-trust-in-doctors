#script: data analysis and visualisation for Multiple imputation at 21 month timepoint
#-------------------------------------------------------------------------------
source(here::here("21-month-timepoint/multiple-imputation-dataset/1_Packages", "21m_MI_packages.R"))

df_working <- read.csv(here::here("21-month-timepoint/multiple-imputation-dataset/3_Descriptives", "df_working_mi.csv" ))


#-------------------------------------------------------------------------------
# recoding to get data ready for logistic regression models - only including 
# 0 = people who experienced a condition and did not go to the dr
# 1 = people who experienced a condition and did go to the dr

# anxiety
df_working$anx <- as.numeric(as.character(df_working$anx))

df_working$anx <- ifelse(df_working$anx == 1, 1,
                         ifelse(df_working$anx == 0, 0,
                                ifelse(df_working$anx == 2, NA_real_, df_working$anx)))
# depression
df_working$dep <- as.numeric(as.character(df_working$dep))

df_working$dep <- ifelse(df_working$dep == 1, 1,
                         ifelse(df_working$dep == 0, 0,
                                ifelse(df_working$dep == 2, NA_real_, df_working$dep)))
# menstrual proble,s
df_working$per <- as.numeric(as.character(df_working$per))

df_working$per <- ifelse(df_working$per == 1, 1,
                         ifelse(df_working$per == 0, 0,
                                ifelse(df_working$per == 2, NA_real_, df_working$per)))


# Subsetting dataframes

#subsetting so each dataframe only includes people I am interested in - those who answered Y to consultation question 

df_anx <- df_working %>% filter (!is.na(anx))

df_dep <- df_working %>% filter (!is.na(dep))

df_per <- df_working %>% filter (!is.na(per))


#-------------------------------------------------------------------------------
# Checking missingness of variables

#missingness for anxiety
missing_table_anx <- data.frame(
  Variable = names(df_anx[c(
    "sum_attitude", "anx", "age", "mar_status",
    "relig", "edu_level", "parity", "home_own_status",
    "mum_hlth", "soc_supp_mis", "sum_weigh_life", "dr_change", "soc_class", "anx_score", "dep_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_anx", "c_anx", "e_anx", "f_anx",  # anxiety scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )]),
  Missing_Percent = colMeans(is.na(df_anx[c(
    "sum_attitude", "anx", "age", "mar_status",
    "relig", "edu_level", "parity", "home_own_status",
    "mum_hlth", "soc_supp_mis", "sum_weigh_life", "dr_change", "soc_class", "anx_score", "dep_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_anx", "c_anx", "e_anx", "f_anx",  # anxiety scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )])) * 100
)

missing_table_anx <- missing_table_anx[order(-missing_table_anx$Missing_Percent), ]

View(missing_table_anx)


#missingness for depression
missing_table_dep <- data.frame(
  Variable = names(df_dep[c(
    "sum_attitude", "dep", "age", "mar_status",
    "relig", "edu_level", "parity", "home_own_status",
    "mum_hlth", "soc_supp_mis", "sum_weigh_life", "dr_change", "soc_class", "dep_score", "anx_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_dep", "c_dep", "e_dep", "f_dep", # depression scores
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )]),
  Missing_Percent = colMeans(is.na(df_dep[c(
    "sum_attitude", "dep", "age", "mar_status",
    "relig", "edu_level", "parity", "home_own_status",
    "mum_hlth", "soc_supp_mis", "sum_weigh_life", "dr_change", "soc_class", "dep_score", "anx_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
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

missing_table_dep <- missing_table_dep[order(-missing_table_dep$Missing_Percent), ]

View(missing_table_dep)


#missingness for menstrual problems
missing_table_per <- data.frame(
  Variable = names(df_per[c(
    "sum_attitude", "per", "age", "mar_status",
    "relig", "edu_level", "parity", "home_own_status",
    "mum_hlth", "soc_supp_mis", "sum_weigh_life", "dr_change", "soc_class", "anx_score", "dep_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_men_ten", #menstrual problems 
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )]),
  Missing_Percent = colMeans(is.na(df_per[c(
    "sum_attitude", "per", "age", "mar_status",
    "relig", "edu_level", "parity", "home_own_status",
    "mum_hlth", "soc_supp_mis", "sum_weigh_life", "dr_change", "soc_class", "anx_score", "dep_score",
    "f_home_own_status", # home ownership aux 
    "a_car_own", # car ownership 
    "g_mar_status", # marital status
    "k_uni", # educational attainment
    "b_soc_class", "c_soc_class", "g_soc_class", #sep
    "g_no_conf", # no confidence in clinic Drs
    "g_helpful", # the dr in the clinic is always helpful
    "b_men_ten", #menstrual problems 
    "d_soc_supp", "e_soc_supp", "f_soc_supp", #social support score
    "b_weigh_life", "c_weigh_life", "e_weigh_life", "f_weigh_life", #weighted life events
    "l_dr", # recent Dr change
    "k_relig", #religion
    "g_parity", # parity 
    "b_age", "e_age", "f_age", # age
    "b_hlth", "f_hlth" #general health
  )])) * 100
)

missing_table_per <- missing_table_per[order(-missing_table_per$Missing_Percent), ]

View(missing_table_per)

#define a complete case indicator - anxiety
complete_m1_anx <- rep(0, nrow(df_anx))
complete_m1_anx[which(missing_table_anx == 0)] <- 1
df_anx$complete_m1_anx <- complete_m1_anx

assoc_comp_anx <- glm(
  complete_m1_anx ~ sum_attitude + anx + age + mar_status + relig + edu_level + parity + home_own_status + mum_hlth + soc_supp_mis + sum_weigh_life + dr_change + soc_class + anx_score,
  data = df_anx, family = binomial
)

summary(assoc_comp_anx)

#define a complete case indicator - depression
complete_m1_dep <- rep(0, nrow(df_dep))
complete_m1_dep[which(missing_table_dep == 0)] <- 1
df_dep$complete_m1_dep <- complete_m1_dep

assoc_comp_dep <- glm(
  complete_m1_dep ~ sum_attitude + dep + age + mar_status + relig + edu_level + parity + home_own_status + mum_hlth + soc_supp_mis + sum_weigh_life + dr_change + soc_class + dep_score,
  data = df_dep, family = binomial
)

summary(assoc_comp_dep)

#define a complete case indicator - menstrual problems
complete_m1_per <- rep(0, nrow(df_per))
complete_m1_per[which(missing_table_per == 0)] <- 1
df_per$complete_m1_per <- complete_m1_per

assoc_comp_per <- glm(
  complete_m1_per ~ sum_attitude + per + age + mar_status + relig + edu_level + parity + home_own_status + mum_hlth + soc_supp_mis + sum_weigh_life + dr_change + soc_class,
  data = df_per, family = binomial
)

summary(assoc_comp_per)


#creating new dataframes to only include variables of interest

## anx
vars_anx <- c(
  "sum_attitude", "anx", "age", "mar_status",
  "relig", "edu_level", "parity", "home_own_status",
  "mum_hlth", "soc_supp_mis", "weigh_life", "dr_change", "soc_class", "anx_score", "dep_score",
  "b_age", "e_age", "f_age", "g_mar_status", "g_parity", "a_car_own", "b_soc_class",
  "e_dep", "f_anx", "f_dep", "e_anx"
)

df_anx_mi <- df_anx[vars_anx]

##dep
vars_dep <- c(
  "sum_attitude", "dep", "age", "mar_status",
  "relig", "edu_level", "parity", "home_own_status",
  "mum_hlth", "soc_supp_mis", "weigh_life", "dr_change", "soc_class", "anx_score", "dep_score",
  "b_age", "e_age", "f_age", "g_mar_status", "g_parity", "a_car_own", "b_soc_class",
  "e_dep", "f_anx", "f_dep", "e_anx"
)

df_dep_mi <- df_dep[vars_dep]

##per
vars_per <- c(
  "sum_attitude", "per", "age", "mar_status",
  "relig", "edu_level", "parity", "home_own_status",
  "mum_hlth", "soc_supp_mis", "weigh_life", "dr_change", "soc_class",
  "b_age", "e_age", "f_age", "g_mar_status", "g_parity", "a_car_own", "b_soc_class", "anx_score", "dep_score", "e_dep", "f_anx", "f_dep", "e_anx"
)

df_per_mi <- df_per[vars_per]

#checking skew

df_working$parity <- as.numeric(as.character(df_working$parity)) # changing parity to numeric

cols <- c("sum_attitude", "age",
          "parity",
          "soc_supp_mis", "weigh_life", "anx_score", "dep_score")

sapply(df_working[cols], skewness, na.rm = TRUE)

# not looking at binary or categorical variables


#------------------------------------------------------------------------------
#mice

## anxiety
# 1. prep variables

# Binary variables
binary_vars <- c("anx", "mar_status", "dr_change", "soc_class")
df_anx_mi[binary_vars] <- lapply(df_anx_mi[binary_vars], factor)

# Continuous variables
numeric_vars <- c(
  "sum_attitude","age","parity","soc_supp_mis",
  "anx_score","weigh_life","dep_score"
)
df_anx_mi[numeric_vars] <- lapply(
  df_anx_mi[numeric_vars],
  function(x) as.numeric(as.character(x))
)

# Categorical variables
factor_vars <- c("relig", "home_own_status", "edu_level", "mum_hlth")
df_anx_mi[factor_vars] <- lapply(df_anx_mi[factor_vars], factor)

# 2. Initial mice setup

ini <- mice(df_anx_mi, maxit = 0, printFlag = FALSE)

method <- ini$method
pm     <- ini$predictorMatrix

# Do not allow self-prediction
diag(pm) <- 0

# 3. Specify imputation methods


# Outcome (not imputed)
method["anx"] <- ""

# Continuous (normal)
method["age"] <- "norm"

# Binary
method[c("mar_status","dr_change", "soc_class")] <- "logreg"

# Ordered categorical
method[c("mum_hlth", "edu_level", "home_own_status")] <- "polr"

# Ordered categorical
method["relig"] <- "polyreg"

# Robust method for most variables (handles skew + categorical safely)
method[c(
  "sum_attitude","soc_supp_mis","parity",
  "weigh_life","anx_score","dep_score"
)] <- "pmm"


# 4. Run imputation


imp_anx <- mice(
data = df_anx_mi,
method = method,
predictorMatrix = pm,
m = 50,
maxit = 20,
seed = 123
)

# 5. Check imputation worked

colSums(is.na(complete(imp_anx, 1)))

# Optional: check for warnings
imp_anx$loggedEvents


## depression

# 1. prep variables

# Binary variables
binary_vars <- c("dep", "mar_status", "dr_change", "soc_class")
df_dep_mi[binary_vars] <- lapply(df_dep_mi[binary_vars], factor)

# Continuous variables
numeric_vars <- c(
  "sum_attitude","age","parity","soc_supp_mis",
  "anx_score","weigh_life","dep_score"
)
df_dep_mi[numeric_vars] <- lapply(
  df_dep_mi[numeric_vars],
  function(x) as.numeric(as.character(x))
)

# Categorical variables
factor_vars <- c("relig", "home_own_status", "edu_level", "mum_hlth")
df_dep_mi[factor_vars] <- lapply(df_dep_mi[factor_vars], factor)

#2. initial mice set up

ini <- mice(df_dep_mi, maxit = 0, printFlag = FALSE)

method <- ini$method
pm     <- ini$predictorMatrix

# Do not allow self-prediction
diag(pm) <- 0

# 3. specify imputation methods

# Outcome (not imputed)
method["dep"] <- ""

# Continuous (normal)
method["age"] <- "norm"

# Binary
method[c("mar_status","dr_change", "soc_class")] <- "logreg"

# Ordered categorical
method[c("mum_hlth", "edu_level", "home_own_status")] <- "polr"

# Ordered categorical
method["relig"] <- "polyreg"

# Robust method for most variables (handles skew + categorical safely)
method[c(
  "sum_attitude","soc_supp_mis","parity",
  "weigh_life","anx_score","dep_score"
)] <- "pmm"


# 4. run

imp_dep <- mice(
  data = df_dep_mi,
  method = method,
  predictorMatrix = pm,
  m = 50,
  maxit = 20,
  seed = 123
)


## 5. Check imputation worked




colSums(is.na(complete(imp_dep, 1)))

# Optional: check for warnings
imp_dep$loggedEvents


## menstrual problems

# 1. Prepare variables


# Binary variables
binary_vars <- c("per", "mar_status", "dr_change", "soc_class")
df_per_mi[binary_vars] <- lapply(df_per_mi[binary_vars], factor)

# Continuous variables
numeric_vars <- c(
  "sum_attitude","age","parity","soc_supp_mis",
  "anx_score","weigh_life","dep_score"
)
df_per_mi[numeric_vars] <- lapply(
  df_per_mi[numeric_vars],
  function(x) as.numeric(as.character(x))
)

# Categorical variables
factor_vars <- c("relig", "home_own_status", "edu_level", "mum_hlth")
df_per_mi[factor_vars] <- lapply(df_per_mi[factor_vars], factor)


# 2. Initial mice setup


ini <- mice(df_per_mi, maxit = 0, printFlag = FALSE)

method <- ini$method
pm     <- ini$predictorMatrix

# Do not allow self-prediction
diag(pm) <- 0


# 3. Specify imputation methods


# Outcome (not imputed)
method["per"] <- ""

# Continuous (normal)
method["age"] <- "norm"

# Binary
method[c("mar_status","dr_change", "soc_class")] <- "logreg"

# Ordered categorical
method[c("mum_hlth", "edu_level", "home_own_status")] <- "polr"

# Ordered categorical
method["relig"] <- "polyreg"

# Robust method for most variables (handles skew + categorical safely)
method[c(
  "sum_attitude","soc_supp_mis","parity",
  "weigh_life","anx_score","dep_score"
)] <- "pmm"

# 4. run

imp_per <- mice(
  data = df_per_mi,
  method = method,
  predictorMatrix = pm,
  m = 50,
  maxit = 20,
  seed = 123
)


## 5. Check imputation worked


colSums(is.na(complete(imp_per, 1)))

# Optional: check for warnings
imp_per$loggedEvents


saveRDS(imp_anx, "X:/Working datasets/g files r/markdown files/Regression files/Git/4_Analysis_and_Visualisation")


saveRDS(imp_dep, "X:/Working datasets/g files r/markdown files/Regression files/Git/4_Analysis_and_Visualisation")


saveRDS(imp_per, "X:/Working datasets/g files r/markdown files/Regression files/Git/4_Analysis_and_Visualisation")

#--------------------------------------------------------------------------------
# Unadjusted model  

## Output table

# import imputations back in
imp_anx <- readRDS("X:/Working datasets/g files r/markdown files/MI/imp_anx.rds")

imp_dep <- readRDS("X:/Working datasets/g files r/markdown files/MI/imp_dep.rds")

imp_per <- readRDS("X:/Working datasets/g files r/markdown files/MI/imp_per.rds")

# Anxiety
anx_reg <- with(imp_anx, glm(anx ~ sum_attitude, family = binomial()))
anx_pooled <- pool(anx_reg)
summary(anx_pooled, exponentiate = TRUE)

# Depression
dep_reg <- with(imp_dep,
                glm(dep ~ sum_attitude, family = binomial()))
dep_pooled <- pool(dep_reg)
summary(dep_pooled, exponentiate = TRUE)

# Menstrual Problems
per_reg <- with(imp_per, glm(per ~ sum_attitude, family = binomial()))
per_pooled <- pool(per_reg)
summary(per_pooled, exponentiate = TRUE)

# unadjusted odds tables with N values
unad_anx_table <- tbl_regression(
  anx_reg, 
  exponentiate = TRUE, 
  label = sum_attitude ~ 
    "Sum of attitudes")

print(unad_anx_table)

unad_dep_table <- tbl_regression(
  dep_reg, 
  exponentiate = TRUE, 
  label = sum_attitude ~ 
    "Sum of attitudes")

print(unad_dep_table)


unad_per_table <- tbl_regression(
  per_reg, 
  exponentiate = TRUE, 
  label = sum_attitude ~ 
    "Sum of attitudes")

print(unad_per_table)

#combined
unad_table <- tbl_stack(
  list(unad_anx_table, unad_dep_table, unad_per_table)
) %>%
  modify_caption(glue::glue("Unadjusted Odds Ratios for the association between trust in doctors and healthcare access for all conditions"))

print(unad_table)


#-------------------------------------------------------------------------------
# Adjusted Model 1  

### Anx
anx_reg_adj <- with(
  imp_anx,
  glm(
    anx ~ sum_attitude + age + parity + mar_status +
      relig + soc_class + edu_level + home_own_status + anx_score + dep_score,
    family = binomial()
  )
)
adj_anx_pooled <- pool(anx_reg_adj)

# Dep
dep_reg_adj <- with(
  imp_dep,
  glm(
    dep ~ sum_attitude + age + parity + mar_status +
      relig + soc_class + edu_level + home_own_status + anx_score +  dep_score,
    family = binomial()
  )
)
adj_dep_pooled <- pool(dep_reg_adj)

#Menstrual problems
per_reg_adj <- with(
  imp_per,
  glm(
    per ~ sum_attitude + age + parity + mar_status +
      relig + soc_class + edu_level + home_own_status + anx_score + dep_score,
    family = binomial()
  )
)
adj_per_pooled <- pool(per_reg_adj)


extract_adjusted <- function(pooled_model, model_name) {
  tidy(pooled_model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(model = model_name)
}

adj_all <- bind_rows(
  extract_adjusted(adj_anx_pooled, "Anxiety"),
  extract_adjusted(adj_dep_pooled, "Depression"),
  extract_adjusted(adj_per_pooled, "Menstrual Problems")
)


adj_all <- adj_all %>%
  mutate(
    term_label = case_when(
      term == "sum_attitude" ~ "Attitudes towards doctors",
      term == "age" ~ "Age",
      term == "parity" ~ "Parity",
      str_starts(term, "soc_class") ~ "Socioeconomic position",
      str_starts(term, "edu_level") ~ "Highest educational attainment",
      str_starts(term, "home_own_status") ~ "Home ownership status",
      str_starts(term, "mar_status") ~ "Marital status",
      str_starts(term, "relig") ~ "Religion",
      term == "anx_score" ~ "Crown crisp anxiety score",
      term == "dep_score" ~ "Crown crisp depression score",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(term_label))

# so factor variables dont produce more than one row in the tables
factor_vars <- c(
  "Socioeconomic position",
  "Highest educational attainment",
  "Home ownership status",
  "Marital status",
  "Religion"
)

adj_collapsed <- adj_all %>%
  mutate(log_or = abs(log(estimate))) %>%
  group_by(model, term_label) %>%
  slice_max(
    order_by = if_else(term_label %in% factor_vars, log_or, Inf),
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(-log_or)


custom_order <- c(
  "Attitudes towards doctors",
  "Age",
  "Parity",
  "Socioeconomic position",
  "Highest educational attainment",
  "Home ownership status",
  "Marital status",
  "Religion",
  "Crown crisp anxiety score",
  "Crown crisp depression score"
)

adj_collapsed <- adj_collapsed %>%
  mutate(
    term_label = factor(term_label, levels = custom_order),
    model = factor(model, levels = c("Anxiety", "Depression", "Menstrual Problems"))
  )  %>%
  arrange(model, term_label)

adj_collapsed %>%
  mutate(
    OR = sprintf("%.2f", estimate),
    CI = sprintf("%.2f, %.2f", conf.low, conf.high),
    p_value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(model, term_label, OR, CI, p_value) %>%
  gt(groupname_col = "model") %>%
  tab_header(
    title = "Model 1: Adjusted Odds Ratios",
  ) %>%
  cols_label(
    term_label = "Variable",
    OR = "OR",
    CI = "95% CI",
    p_value = "p-value"
  ) %>%
  cols_align(
    align = "left",
    columns = term_label
  ) %>%
  cols_align(
    align = "center",
    columns = c(OR, CI, p_value)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

# unadjusted odds tables with N values
adj_anx_table_1 <- tbl_regression(
  anx_reg_adj, 
  exponentiate = TRUE,
  include = "sum_attitude",
  label = sum_attitude ~ 
    "Sum of attitudes")

print(adj_anx_table_1)

adj_dep_table_1 <- tbl_regression(
  dep_reg_adj, 
  exponentiate = TRUE,
  include = "sum_attitude",
  label = sum_attitude ~ 
    "Sum of attitudes")

print(adj_dep_table_1)


adj_per_table_1 <- tbl_regression(
  per_reg_adj, 
  exponentiate = TRUE,
  include = "sum_attitude",
  label = sum_attitude ~ 
    "Sum of attitudes")

print(adj_per_table_1)

#combined
adj_table_1 <- tbl_stack(
  list(adj_anx_table_1, adj_dep_table_1, adj_per_table_1)
) %>%
  modify_caption(glue::glue("Adjusted model 1 Odds Ratios for the association between trust in doctors and healthcare access for all conditions"))

print(adj_table_1)


#--------------------------------------------------------------------------------
# Adjusted Model 2  

# Anx
anx_reg_adj_2 <- with(
  imp_anx,
  glm(
    anx ~ sum_attitude + age + parity + mar_status +
      relig + soc_class + edu_level + home_own_status + dr_change + mum_hlth + anx_score + dep_score,
    family = binomial()
  )
)
adj_anx_pooled_2 <- pool(anx_reg_adj_2)

# Dep
dep_reg_adj_2 <- with(
  imp_dep,
  glm(
    dep ~ sum_attitude + age + parity + mar_status +
      relig + soc_class + edu_level + home_own_status + dr_change + mum_hlth + anx_score + dep_score,
    family = binomial()
  )
)
adj_dep_pooled_2 <- pool(dep_reg_adj_2)

#Menstrual problems
per_reg_adj_2 <- with(
  imp_per,
  glm(
    per ~ sum_attitude + age + parity + mar_status +
      relig + soc_class + edu_level + home_own_status + dr_change + mum_hlth + anx_score + dep_score,
    family = binomial()
  )
)
adj_per_pooled_2 <- pool(per_reg_adj_2)


extract_adjusted_2 <- function(pooled_model, model_name) {
  tidy(pooled_model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(model = model_name)
}

adj_all_2 <- bind_rows(
  extract_adjusted_2(adj_anx_pooled_2, "Anxiety"),
  extract_adjusted_2(adj_dep_pooled_2, "Depression"),
  extract_adjusted_2(adj_per_pooled_2, "Menstrual Problems")
)

adj_all_2 <- adj_all_2 %>%
  mutate(
    term_label = case_when(
      term == "sum_attitude" ~ "Attitudes towards doctors",
      term == "age" ~ "Age",
      term == "parity" ~ "Parity",
      str_starts(term, "soc_class") ~ "Socioeconomic position",
      str_starts(term, "edu_level") ~ "Highest educational attainment",
      str_starts(term, "home_own_status") ~ "Home ownership status",
      str_starts(term, "mar_status") ~ "Marital status",
      str_starts(term, "relig") ~ "Religion",
      term == "anx_score" ~ "Crown crisp anxiety score",
      term == "dep_score" ~ "Crown crisp depression score",
      str_starts(term, "dr_change") ~ "Recent Dr change",
      str_starts(term, "mum_hlth") ~ "Evaluation of own health",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(term_label))

# so factor variables dont produce more than one row in the tables
factor_vars_2 <- c(
  "Socioeconomic position",
  "Highest educational attainment",
  "Home ownership status",
  "Marital status",
  "Religion",
  "Recent Dr change",
  "Evaluation of own health"
)

adj_collapsed_2 <- adj_all_2 %>%
  mutate(log_or = abs(log(estimate))) %>%
  group_by(model, term_label) %>%
  slice_max(
    order_by = if_else(term_label %in% factor_vars, log_or, Inf),
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(-log_or)


custom_order_2 <- c(
  "Attitudes towards doctors",
  "Age",
  "Parity",
  "Socioeconomic position",
  "Highest educational attainment",
  "Home ownership status",
  "Marital status",
  "Religion",
  "Crown crisp anxiety score",
  "Crown crisp depression score",
  "Recent Dr change",
  "Evaluation of own health"
)

adj_collapsed_2 <- adj_collapsed_2 %>%
  mutate(
    term_label = factor(term_label, levels = custom_order_2),
    model = factor(model, levels = c("Anxiety", "Depression", "Menstrual Problems"))
  ) %>%
  arrange(model, term_label)

adj_collapsed_2 %>%
  mutate(
    OR = sprintf("%.2f", estimate),
    CI = sprintf("%.2f, %.2f", conf.low, conf.high),
    p_value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(model, term_label, OR, CI, p_value) %>%
  gt(groupname_col = "model") %>%
  tab_header(
    title = "Model 2: Adjusted Odds Ratios",
  ) %>%
  cols_label(
    term_label = "Variable",
    OR = "OR",
    CI = "95% CI",
    p_value = "p-value"
  ) %>%
  cols_align(
    align = "left",
    columns = term_label
  ) %>%
  cols_align(
    align = "center",
    columns = c(OR, CI, p_value)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

#main predictor only
adj_anx_table_2 <- tbl_regression(
  anx_reg_adj_2, 
  exponentiate = TRUE,
  include = "sum_attitude",
  label = sum_attitude ~ 
    "Sum of attitudes")

print(adj_anx_table_2)

adj_dep_table_2 <- tbl_regression(
  dep_reg_adj_2, 
  exponentiate = TRUE,
  include = "sum_attitude",
  label = sum_attitude ~ 
    "Sum of attitudes")

print(adj_dep_table_2)


adj_per_table_2 <- tbl_regression(
  per_reg_adj_2, 
  exponentiate = TRUE,
  include = "sum_attitude",
  label = sum_attitude ~ 
    "Sum of attitudes")

print(adj_per_table_2)

#combined
adj_table_2 <- tbl_stack(
  list(adj_anx_table_2, adj_dep_table_2, adj_per_table_2)
) %>%
  modify_caption(glue::glue("Adjusted model 2 Odds Ratios for the association between trust in doctors and healthcare access for all conditions"))

print(adj_table_2)

#-------------------------------------------------------------------------------
# Adjusted Model 3 
# Anx
anx_reg_adj_3 <- with(
  imp_anx,
  glm(
    anx ~ sum_attitude + age + parity + mar_status +
      relig + soc_class + edu_level + home_own_status + anx_score + dr_change + mum_hlth + soc_supp_mis + weigh_life + dep_score,
    family = binomial()
  )
)
adj_anx_pooled_3 <- pool(anx_reg_adj_3)

# Dep
dep_reg_adj_3 <- with(
  imp_dep,
  glm(
    dep ~ sum_attitude + age + parity + mar_status +
      relig + soc_class + edu_level + home_own_status + dep_score + dr_change + mum_hlth + soc_supp_mis + weigh_life + anx_score,
    family = binomial()
  )
)
adj_dep_pooled_3 <- pool(dep_reg_adj_3)

#Menstrual problems
per_reg_adj_3 <- with(
  imp_per,
  glm(
    per ~ sum_attitude + age + parity + mar_status +
      relig + soc_class + edu_level + home_own_status + dr_change + mum_hlth + soc_supp_mis + weigh_life + anx_score + dep_score,
    family = binomial()
  )
)
adj_per_pooled_3 <- pool(per_reg_adj_3)


extract_adjusted_3 <- function(pooled_model, model_name) {
  tidy(pooled_model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(model = model_name)
}

adj_all_3 <- bind_rows(
  extract_adjusted_3(adj_anx_pooled_3, "Anxiety"),
  extract_adjusted_3(adj_dep_pooled_3, "Depression"),
  extract_adjusted_3(adj_per_pooled_3, "Menstrual Problems")
)

adj_all_3 <- adj_all_3 %>%
  mutate(
    term_label = case_when(
      term == "sum_attitude" ~ "Attitudes towards doctors",
      term == "age" ~ "Age",
      term == "parity" ~ "Parity",
      str_starts(term, "soc_class") ~ "Socioeconomic position",
      str_starts(term, "edu_level") ~ "Highest educational attainment",
      str_starts(term, "home_own_status") ~ "Home ownership status",
      str_starts(term, "mar_status") ~ "Marital status",
      str_starts(term, "relig") ~ "Religion",
      term == "anx_score" ~ "Crown crisp anxiety score",
      term == "dep_score" ~ "Crown crisp depression score",
      str_starts(term, "dr_change") ~ "Recent Dr change",
      str_starts(term, "mum_hlth") ~ "Evaluation of own health",
      term == "soc_supp_mis" ~ "Social support score",
      term == "weigh_life" ~ "Weighted life events score",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(term_label))

# so factor variables dont produce more than one row in the tables
factor_vars_3 <- c(
  "Socioeconomic position",
  "Highest educational attainment",
  "Home ownership status",
  "Marital status",
  "Religion",
  "Recent Dr change",
  "Evaluation of own health"
)

adj_collapsed_3 <- adj_all_3 %>%
  mutate(log_or = abs(log(estimate))) %>%
  group_by(model, term_label) %>%
  slice_max(
    order_by = if_else(term_label %in% factor_vars, log_or, Inf),
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(-log_or)


custom_order_3 <- c(
  "Attitudes towards doctors",
  "Age",
  "Parity",
  "Socioeconomic position",
  "Highest educational attainment",
  "Home ownership status",
  "Marital status",
  "Religion",
  "Crown crisp anxiety score",
  "Crown crisp depression score",
  "Recent Dr change",
  "Evaluation of own health",
  "Social support score",
  "Weighted life events score"
)

adj_collapsed_3 <- adj_collapsed_3 %>%
  mutate(
    term_label = factor(term_label, levels = custom_order_3),
    model = factor(model, levels = c("Anxiety", "Depression", "Menstrual Problems"))
  ) %>%
  arrange(model, term_label)

adj_collapsed_3 %>%
  mutate(
    OR = sprintf("%.2f", estimate),
    CI = sprintf("%.2f, %.2f", conf.low, conf.high),
    p_value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(model, term_label, OR, CI, p_value) %>%
  gt(groupname_col = "model") %>%
  tab_header(
    title = "Model 3: Adjusted Odds Ratios",
  ) %>%
  cols_label(
    term_label = "Variable",
    OR = "OR",
    CI = "95% CI",
    p_value = "p-value"
  ) %>%
  cols_align(
    align = "left",
    columns = term_label
  ) %>%
  cols_align(
    align = "center",
    columns = c(OR, CI, p_value)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

adj_anx_table_3 <- tbl_regression(
  anx_reg_adj_3, 
  exponentiate = TRUE,
  include = "sum_attitude",
  label = sum_attitude ~ 
    "Sum of attitudes")

print(adj_anx_table_3)

adj_dep_table_3 <- tbl_regression(
  dep_reg_adj_3, 
  exponentiate = TRUE,
  include = "sum_attitude",
  label = sum_attitude ~ 
    "Sum of attitudes")

print(adj_dep_table_3)


adj_per_table_3 <- tbl_regression(
  per_reg_adj_3, 
  exponentiate = TRUE,
  include = "sum_attitude",
  label = sum_attitude ~ 
    "Sum of attitudes")

print(adj_per_table_3)

#combined
adj_table_3 <- tbl_stack(
  list(adj_anx_table_3, adj_dep_table_3, adj_per_table_3)
) %>%
  modify_caption(glue::glue(" Adjusted model 3 Odds Ratios for the association between trust in doctors and healthcare access for all conditions"))

print(adj_table_3)


#------------------------------------------------------------------------------
# Combined Models: Main predictor only  

## Anxiety 

extract_main <- function(pooled_model, model_name) {
  tidy(pooled_model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term == "sum_attitude") %>%
    mutate(model = model_name)
}

anx_results <- bind_rows(
  extract_main(anx_pooled, "Unadjusted"),
  extract_main(adj_anx_pooled, "Model 1"),
  extract_main(adj_anx_pooled_2, "Model 2"),
  extract_main(adj_anx_pooled_3, "Model 3")
)

anx_results %>%
  mutate(
    OR = sprintf("%.2f", estimate),
    CI = sprintf("%.2f, %.2f", conf.low, conf.high),
    p_value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(model, OR, CI, p_value) %>%
  gt() %>%
  tab_header(title = "Anxiety: Association with Attitudes towards Doctors") %>%
  cols_label(
    model = "Model",
    OR = "OR",
    CI = "95% CI",
    p_value = "p-value"
  )

## Depression


extract_main <- function(pooled_model, model_name) {
  tidy(pooled_model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term == "sum_attitude") %>%
    mutate(model = model_name)
}

dep_results <- bind_rows(
  extract_main(dep_pooled, "Unadjusted"),
  extract_main(adj_dep_pooled, "Model 1"),
  extract_main(adj_dep_pooled_2, "Model 2"),
  extract_main(adj_dep_pooled_3, "Model 3")
)

dep_results %>%
  mutate(
    OR = sprintf("%.2f", estimate),
    CI = sprintf("%.2f, %.2f", conf.low, conf.high),
    p_value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(model, OR, CI, p_value) %>%
  gt() %>%
  tab_header(title = "Depression: Association with Attitudes towards Doctors") %>%
  cols_label(
    model = "Model",
    OR = "OR",
    CI = "95% CI",
    p_value = "p-value"
  )


## Menstrual problems
extract_main <- function(pooled_model, model_name) {
  tidy(pooled_model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term == "sum_attitude") %>%
    mutate(model = model_name)
}

per_results <- bind_rows(
  extract_main(per_pooled, "Unadjusted"),
  extract_main(adj_per_pooled, "Model 1"),
  extract_main(adj_per_pooled_2, "Model 2"),
  extract_main(adj_per_pooled_3, "Model 3")
)

per_results %>%
  mutate(
    OR = sprintf("%.2f", estimate),
    CI = sprintf("%.2f, %.2f", conf.low, conf.high),
    p_value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
  ) %>%
  select(model, OR, CI, p_value) %>%
  gt() %>%
  tab_header(title = "Menstrual Problems: Association with Attitudes towards Doctors") %>%
  cols_label(
    model = "Model",
    OR = "OR",
    CI = "95% CI",
    p_value = "p-value"
  )

#-------------------------------------------------------------------------------
## Combined table; all conditions


# Function to extract the main predictor
extract_main <- function(pooled_model, model_name, outcome_name) {
  tidy(pooled_model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term == "sum_attitude") %>%
    mutate(
      model = model_name,
      outcome = outcome_name
    )
}

# Anxiety models
anx_results <- bind_rows(
  extract_main(anx_pooled, "Unadjusted", "Anxiety"),
  extract_main(adj_anx_pooled, "Model 1", "Anxiety"),
  extract_main(adj_anx_pooled_2, "Model 2", "Anxiety"),
  extract_main(adj_anx_pooled_3, "Model 3", "Anxiety")
)

# Depression models
dep_results <- bind_rows(
  extract_main(dep_pooled, "Unadjusted", "Depression"),
  extract_main(adj_dep_pooled, "Model 1", "Depression"),
  extract_main(adj_dep_pooled_2, "Model 2", "Depression"),
  extract_main(adj_dep_pooled_3, "Model 3", "Depression")
)

# Menstrual problems models
per_results <- bind_rows(
  extract_main(per_pooled, "Unadjusted", "Menstrual Problems"),
  extract_main(adj_per_pooled, "Model 1", "Menstrual Problems"),
  extract_main(adj_per_pooled_2, "Model 2", "Menstrual Problems"),
  extract_main(adj_per_pooled_3, "Model 3", "Menstrual Problems")
)

# Combine everything
all_results <- bind_rows(anx_results, dep_results, per_results)

# Format table
final_table <- all_results %>%
  mutate(
    OR = sprintf("%.2f", estimate),
    CI = sprintf("%.2f, %.2f", conf.low, conf.high),
    p_value = ifelse(
      p.value < 0.001,
      "<0.001",
      sprintf("%.3f", p.value)
    )
  ) %>%
  select(outcome, model, OR, CI, p_value) %>%
  gt(groupname_col = "outcome") %>%
  tab_header(
    title = "Associations Between Attitudes Towards Doctors and Health Outcomes"
  ) %>%
  cols_label(
    model = "Model",
    OR = "Odds Ratio",
    CI = "95% CI",
    p_value = "p-value"
  )

saveRDS(
  final_table,
  (here::here( "21-month-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "21m_master_mi_table.rds"))
)

#-------------------------------------------------------------------------------
# Forest plots 

## Anxiety
extract_main <- function(pooled_model, model_name) {
  tidy(pooled_model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term == "sum_attitude") %>%
    mutate(
      model = model_name,
      variable_label = "Attitudes towards doctors"
    )
}

anx_results <- bind_rows(
  extract_main(anx_pooled, "Unadjusted"),
  extract_main(adj_anx_pooled, "Model 1"),
  extract_main(adj_anx_pooled_2, "Model 2"),
  extract_main(adj_anx_pooled_3, "Model 3")
)

anx_results <- anx_results %>%
  mutate(
    model = factor(
      model,
      levels = c("Unadjusted", "Model 1", "Model 2", "Model 3")
    )
  )

anx_results <- anx_results %>%
  mutate(ypos = c(4, 3, 2, 1))  # Unadjusted at top

anx_results %>%
  mutate(
    model = factor(model, levels = c("Model 3", "Model 2", "Model 1", "Unadjusted"))
  ) %>%
  ggplot(aes(x = estimate, y = variable_label, color = model, group = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.7),
    height = 0.2
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",
      "Model 2" = "#f44973",
      "Model 3" = "#f2923f"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1.00, 1.16, by = 0.02),
    limits = c(1.00, 1.16),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11)
  )


## Depression
extract_main <- function(pooled_model, model_name) {
  tidy(pooled_model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term == "sum_attitude") %>%
    mutate(
      model = model_name,
      variable_label = "Attitudes towards doctors"
    )
}

dep_results <- bind_rows(
  extract_main(dep_pooled, "Unadjusted"),
  extract_main(adj_dep_pooled, "Model 1"),
  extract_main(adj_dep_pooled_2, "Model 2"),
  extract_main(adj_dep_pooled_3, "Model 3")
)

dep_results %>%
  mutate(
    model = factor(model, levels = c("Model 3", "Model 2", "Model 1", "Unadjusted"))
  ) %>%
  ggplot(aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.7),
    height = 0.2
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",
      "Model 2" = "#f44973",
      "Model 3" = "#f2923f"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1.00, 1.16, by = 0.02),
    limits = c(1.00, 1.16),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11)
  )


## Menstrual problems
extract_main <- function(pooled_model, model_name) {
  tidy(pooled_model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term == "sum_attitude") %>%
    mutate(
      model = model_name,
      variable_label = "Attitudes towards doctors"
    )
}

per_results <- bind_rows(
  extract_main(per_pooled, "Unadjusted"),
  extract_main(adj_per_pooled, "Model 1"),
  extract_main(adj_per_pooled_2, "Model 2"),
  extract_main(adj_per_pooled_3, "Model 3")
)


per_results %>%
  mutate(
    model = factor(model, levels = c("Model 3", "Model 2", "Model 1", "Unadjusted"))
  ) %>%
  ggplot(aes(x = estimate, y = variable_label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.7),
    height = 0.2
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#fbbb3c",
      "Model 1" = "#854f99",
      "Model 2" = "#f44973",
      "Model 3" = "#f2923f"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1.00, 1.16, by = 0.02),
    limits = c(1.00, 1.16),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11)
  )

## Forest plots - all conditions
all_results <- bind_rows(
  anx_results %>% mutate(condition = "Anxiety"),
  dep_results %>% mutate(condition = "Depression"),
  per_results %>% mutate(condition = "Menstrual problems")
)

all_results <- all_results %>%
  mutate(
    condition = factor(condition, levels = c("Menstrual problems", "Depression", "Anxiety")),
    # Reverse the factor levels here so ggplot stacks them correctly
    model = factor(model, levels = c("Model 3", "Model 2", "Model 1", "Unadjusted"))
  )
g_plot_MI <- ggplot(all_results, aes(x = estimate, y = condition, color = model, group = model)) +
  geom_point(
    position = position_dodge(width = 0.7), 
    size = 3
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.7),
    height = 0.2
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(
    aes(
      x = conf.high,
      label = sprintf("%.2f", estimate)
    ),
    position = position_dodge(width = 0.7),
    hjust = -0.1,
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Unadjusted" = "#415070", 
      "Model 1"    = "#c66146", 
      "Model 2"    = "#b65266", 
      "Model 3"    = "#68527f"
    ),
    # This keeps your legend looking clean and chronological (top-to-bottom)
    breaks = c("Unadjusted", "Model 1", "Model 2", "Model 3"),
    labels = c(
      "Unadjusted model",
      "Model 1: Baseline demographics",
      "Model 2: Health experiences",
      "Model 3: Social networks"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1.00, 1.16, by = 0.02),
    limits = c(1.00, 1.16),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    title = "Association of trust in doctors against healthcare access for anxiety, depression and menstrual problems, at 21 months",
    x = "Odds Ratio (95% Confidence Interval)",
    y = "",
    color = "Model",
    caption = paste(
      "Model 1 (Baseline demographics). Adjusted for: age, parity, marital status, religion, Socioeconomic position, \n educational attainment, home ownership status, crown crisp anxiety score and crown crisp depression score",
      "Model 2 (Health experiences). Additionally adjusted for: self-rated health and recent doctor change",
      "Model 3 (Social networks). Additionally adjusted for: social support score and weighted life events ",
      sep = "\n"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 11),
    plot.caption = element_text(hjust = 0.5)
  )

g_plot_MI


ggsave(
  filename = (here::here("21-month-timepoint/multiple-imputation-dataset/4_Analysis_and_Visualisation", "21m_mi_forest_plot.png")),
  plot = g_plot_MI,
  width = 10,
  height = 6,
  dpi = 300
)


