# Script: data analysis and visualisation for original dataset at 6-year timepoint

#-------------------------------------------------------------------------------
source(here::here("6-year-timepoint/original-dataset/1_Packages", "6y_packages.R"))

df_working <- read.csv(here::here("6-year-timepoint/original-dataset/3_Descriptives", "df_working_6y.csv" ))

#-------------------------------------------------------------------------------

# Regression tables  

# recoding outcomes, so that they only include 0 = people who experienced symptoms and didn't go to the dr
# 1 = people who did experience symptoms and went to the dr

## anxiety
df_working$m_anx <- as.numeric(as.character(df_working$m_anx))

df_working$m_anx <- ifelse(df_working$m_anx == 1, 1,
                           ifelse(df_working$m_anx == 0, 0,
                                  ifelse(df_working$m_anx == 2, NA_real_, df_working$m_anx)))

# depression
df_working$m_dep <- as.numeric(as.character(df_working$m_dep))

df_working$m_dep <- ifelse(df_working$m_dep == 1, 1,
                           ifelse(df_working$m_dep == 0, 0,
                                  ifelse(df_working$m_dep == 2, NA_real_, df_working$m_dep)))

#partner anxiety
df_working$p_anx <- as.numeric(as.character(df_working$p_anx))

df_working$p_anx <- ifelse(df_working$p_anx == 1, 1,
                           ifelse(df_working$p_anx == 0, 0,
                                  ifelse(df_working$p_anx == 2, NA_real_, df_working$p_anx)))
# partner depression
df_working$p_dep <- as.numeric(as.character(df_working$p_dep))

df_working$p_dep <- ifelse(df_working$p_dep == 1, 1,
                           ifelse(df_working$p_dep == 0, 0,
                                  ifelse(df_working$p_dep == 2, NA_real_, df_working$p_dep)))


#-------------------------------------------------------------------------------
# unadjusted regression models
# mother 
m_anx_reg_unad_tab_conf <- glm(m_anx ~ m_conf_nhs, data = df_working, family = binomial())
m_anx_reg_unad_tab_quick  <- glm(m_anx ~ m_ill_quick, data = df_working, family = binomial())
m_anx_reg_unad_tab_help <- glm(m_anx ~ m_alwys_help, data = df_working, family = binomial())

m_dep_reg_unad_tab_conf <- glm(m_dep ~ m_conf_nhs, data = df_working, family = binomial())
m_dep_reg_unad_tab_quick  <- glm(m_dep ~ m_ill_quick, data = df_working, family = binomial())
m_dep_reg_unad_tab_help <- glm(m_dep ~ m_alwys_help, data = df_working, family = binomial())

# partner
p_anx_reg_unad_tab_conf <- glm(p_anx ~ p_conf_nhs, data = df_working, family = binomial())
p_anx_reg_unad_tab_quick  <- glm(p_anx ~ p_ill_quick, data = df_working, family = binomial())
p_anx_reg_unad_tab_help <- glm(p_anx ~ p_alwys_help, data = df_working, family = binomial())

p_dep_reg_unad_tab_conf <- glm(p_dep ~ p_conf_nhs, data = df_working, family = binomial())
p_dep_reg_unad_tab_quick  <- glm(p_dep ~ p_ill_quick, data = df_working, family = binomial())
p_dep_reg_unad_tab_help <- glm(p_dep ~ p_alwys_help, data = df_working, family = binomial())


# sample sizes
n_anx_m_conf <- nobs(m_anx_reg_unad_tab_conf)
n_anx_m_quick  <- nobs(m_anx_reg_unad_tab_quick)
n_anx_m_help <- nobs(m_anx_reg_unad_tab_help)

n_anx_p_conf <- nobs(p_anx_reg_unad_tab_conf)
n_anx_p_quick  <- nobs(p_anx_reg_unad_tab_quick)
n_anx_p_help <- nobs(p_anx_reg_unad_tab_help)

n_dep_m_conf <- nobs(m_dep_reg_unad_tab_conf)
n_dep_m_quick  <- nobs(m_dep_reg_unad_tab_quick)
n_dep_m_help <- nobs(m_dep_reg_unad_tab_help)

n_dep_p_conf <- nobs(p_dep_reg_unad_tab_conf)
n_dep_p_quick  <- nobs(p_dep_reg_unad_tab_quick)
n_dep_p_help <- nobs(p_dep_reg_unad_tab_help)

# unadjusted odds tables with N values
m_anx_table_conf <- tbl_regression(m_anx_reg_unad_tab_conf, exp = TRUE, 
                                   label = list(m_conf_nhs ~ "I don’t have any confidence in the national health service")) %>%
  modify_caption(glue::glue("(N = {n_anx_m_conf})"))

print(m_anx_table_conf)

m_anx_table_quick <- tbl_regression(m_anx_reg_unad_tab_quick, exp = TRUE, 
                                    label = list(m_ill_quick ~ 
                                                   "I know that if my child was very ill my doctor would come quickly")) %>%
  modify_caption(glue::glue("(N = {n_anx_m_quick})"))

print(m_anx_table_quick)

m_anx_table_help <- tbl_regression(m_anx_reg_unad_tab_help, exp = TRUE, 
                                   label = list(m_alwys_help ~ 
                                                  "The doctor in the clinic is always helpful")) %>%
  modify_caption(glue::glue("(N = {n_anx_m_help})"))

print(m_anx_table_help)

# depression
m_dep_table_conf <- tbl_regression(m_dep_reg_unad_tab_conf, exp = TRUE, 
                                   label = list(m_conf_nhs ~ "I don’t have any confidence in the national health service")) %>%
  modify_caption(glue::glue("(N = {n_dep_m_conf})"))

print(m_dep_table_conf)

m_dep_table_quick <- tbl_regression(m_dep_reg_unad_tab_quick, exp = TRUE, 
                                    label = list(m_ill_quick ~ 
                                                   "I know that if my child was very ill my doctor would come quickly")) %>%
  modify_caption(glue::glue("(N = {n_dep_m_quick})"))

print(m_dep_table_quick)

m_dep_table_help <- tbl_regression(m_dep_reg_unad_tab_help, exp = TRUE, 
                                   label = list(m_alwys_help ~ 
                                                  "The doctor in the clinic is always helpful")) %>%
  modify_caption(glue::glue("(N = {n_dep_m_help})"))

print(m_dep_table_help)

# combined
unad_table_m_anx <- tbl_stack(
  list(m_anx_table_conf, m_anx_table_quick, m_anx_table_help),
  group_header = c(glue::glue("No confidence in NHS (N = {n_anx_m_conf})"), 
                   glue::glue("Believe Doctors would come quickly for sick child (N = {n_anx_m_quick})"), 
                   glue::glue("Believe Doctors in the clinic are always helpful (N = {n_anx_m_help})"))
) %>%
  modify_caption(glue::glue("Unadjusted Odds Ratios for Anxiety against all exposures"))

print(unad_table_m_anx)

unad_table_m_dep <- tbl_stack(
  list(m_dep_table_conf, m_dep_table_quick, m_dep_table_help),
  group_header = c(glue::glue("No confidence in NHS (N = {n_dep_m_conf})"), 
                   glue::glue("Believe Doctors would come quickly for sick child (N = {n_dep_m_quick})"), 
                   glue::glue("Believe Doctors in the clinic are always helpful (N = {n_dep_m_help})"))
) %>%
  modify_caption(glue::glue("Unadjusted Odds Ratios for Depression against all exposures"))

print(unad_table_m_dep)

# mother 
m_anx_reg_unad_tab_conf <- glm(m_anx ~ m_conf_nhs, data = df_working, family = binomial())
m_anx_reg_unad_tab_quick  <- glm(m_anx ~ m_ill_quick, data = df_working, family = binomial())
m_anx_reg_unad_tab_help <- glm(m_anx ~ m_alwys_help, data = df_working, family = binomial())

m_dep_reg_unad_tab_conf <- glm(m_dep ~ m_conf_nhs, data = df_working, family = binomial())
m_dep_reg_unad_tab_quick  <- glm(m_dep ~ m_ill_quick, data = df_working, family = binomial())
m_dep_reg_unad_tab_help <- glm(m_dep ~ m_alwys_help, data = df_working, family = binomial())

# partner
p_anx_reg_unad_tab_conf <- glm(p_anx ~ p_conf_nhs, data = df_working, family = binomial())
p_anx_reg_unad_tab_quick  <- glm(p_anx ~ p_ill_quick, data = df_working, family = binomial())
p_anx_reg_unad_tab_help <- glm(p_anx ~ p_alwys_help, data = df_working, family = binomial())

p_dep_reg_unad_tab_conf <- glm(p_dep ~ p_conf_nhs, data = df_working, family = binomial())
p_dep_reg_unad_tab_quick  <- glm(p_dep ~ p_ill_quick, data = df_working, family = binomial())
p_dep_reg_unad_tab_help <- glm(p_dep ~ p_alwys_help, data = df_working, family = binomial())

# sample sizes
n_anx_m_conf <- nobs(m_anx_reg_unad_tab_conf)
n_anx_m_quick <- nobs(m_anx_reg_unad_tab_quick)
n_anx_m_help <- nobs(m_anx_reg_unad_tab_help)

n_anx_p_conf <- nobs(p_anx_reg_unad_tab_conf)
n_anx_p_quick  <- nobs(p_anx_reg_unad_tab_quick)
n_anx_p_help <- nobs(p_anx_reg_unad_tab_help)

n_dep_m_conf <- nobs(m_dep_reg_unad_tab_conf)
n_dep_m_quick  <- nobs(m_dep_reg_unad_tab_quick)
n_dep_m_help <- nobs(m_dep_reg_unad_tab_help)

n_dep_p_conf <- nobs(p_dep_reg_unad_tab_conf)
n_dep_p_quick  <- nobs(p_dep_reg_unad_tab_quick)
n_dep_p_help <- nobs(p_dep_reg_unad_tab_help)

# unadjusted odds tables with N values
m_anx_table_conf <- tbl_regression(m_anx_reg_unad_tab_conf, exp = TRUE, 
                                   label = list(m_conf_nhs ~ "I don’t have any confidence in the national health service")) %>%
  modify_caption(glue::glue("(N = {n_anx_m_conf})"))

print(m_anx_table_conf)

m_anx_table_quick <- tbl_regression(m_anx_reg_unad_tab_quick, exp = TRUE, 
                                    label = list(m_ill_quick ~ 
                                                   "I know that if my child was very ill my doctor would come quickly")) %>%
  modify_caption(glue::glue("(N = {n_anx_m_quick})"))

print(m_anx_table_quick)

m_anx_table_help <- tbl_regression(m_anx_reg_unad_tab_help, exp = TRUE, 
                                   label = list(m_alwys_help ~ 
                                                  "The doctor in the clinic is always helpful")) %>%
  modify_caption(glue::glue("(N = {n_anx_m_help})"))

print(m_anx_table_help)

# depression
m_dep_table_conf <- tbl_regression(m_dep_reg_unad_tab_conf, exp = TRUE, 
                                   label = list(m_conf_nhs ~ "I don’t have any confidence in the national health service")) %>%
  modify_caption(glue::glue("(N = {n_dep_m_conf})"))

print(m_dep_table_conf)

m_dep_table_quick <- tbl_regression(m_dep_reg_unad_tab_quick, exp = TRUE, 
                                    label = list(m_ill_quick ~ 
                                                   "I know that if my child was very ill my doctor would come quickly")) %>%
  modify_caption(glue::glue("(N = {n_dep_m_quick})"))

print(m_dep_table_quick)

m_dep_table_help <- tbl_regression(m_dep_reg_unad_tab_help, exp = TRUE, 
                                   label = list(m_alwys_help ~ 
                                                  "The doctor in the clinic is always helpful")) %>%
  modify_caption(glue::glue("(N = {n_dep_m_help})"))

print(m_dep_table_help)

# combined - mother
unad_table_m_anx <- tbl_stack(
  list(m_anx_table_conf, m_anx_table_quick, m_anx_table_help),
  group_header = c(glue::glue("No confidence in NHS (N = {n_anx_m_conf})"), 
                   glue::glue("Believe Doctors would come quickly for sick child (N = {n_anx_m_quick})"), 
                   glue::glue("Believe Doctors in the clinic are always helpful (N = {n_anx_m_help})"))
) %>%
  modify_caption(glue::glue("Unadjusted Odds Ratios for Anxiety against all exposures"))

print(unad_table_m_anx)

unad_table_m_dep <- tbl_stack(
  list(m_dep_table_conf, m_dep_table_quick, m_dep_table_help),
  group_header = c(glue::glue("No confidence in NHS (N = {n_dep_m_conf})"), 
                   glue::glue("Believe Doctors would come quickly for sick child (N = {n_dep_m_quick})"), 
                   glue::glue("Believe Doctors in the clinic are always helpful (N = {n_dep_m_help})"))
) %>%
  modify_caption(glue::glue("Unadjusted Odds Ratios for Depression against all exposures"))

print(unad_table_m_dep)

# partner unadjusted tables
# unadjusted odds tables with N values
p_anx_table_conf <- tbl_regression(p_anx_reg_unad_tab_conf, exp = TRUE, 
                                   label = list(p_conf_nhs ~ "I don’t have any confidence in the national health service")) %>%
  modify_caption(glue::glue(" (N = {n_anx_p_conf})"))

print(p_anx_table_conf)

p_anx_table_quick <- tbl_regression(p_anx_reg_unad_tab_quick, exp = TRUE, 
                                    label = list(p_ill_quick ~ 
                                                   "I know that if my child was very ill my doctor would come quickly")) %>%
  modify_caption(glue::glue(" (N = {n_anx_p_quick})"))

print(p_anx_table_quick)

p_anx_table_help <- tbl_regression(p_anx_reg_unad_tab_help, exp = TRUE, 
                                   label = list(p_alwys_help ~ 
                                                  "The doctor in the clinic is always helpful")) %>%
  modify_caption(glue::glue(" (N = {n_anx_p_help})"))

print(p_anx_table_help)

# depression
p_dep_table_conf <- tbl_regression(p_dep_reg_unad_tab_conf, exp = TRUE, 
                                   label = list(p_conf_nhs ~ "I don’t have any confidence in the national health service")) %>%
  modify_caption(glue::glue(" (N = {n_dep_m_conf})"))

print(p_dep_table_conf)

p_dep_table_quick <- tbl_regression(p_dep_reg_unad_tab_quick, exp = TRUE, 
                                    label = list(p_ill_quick ~ 
                                                   "I know that if my child was very ill my doctor would come quickly")) %>%
  modify_caption(glue::glue(" (N = {n_dep_p_quick})"))

print(p_dep_table_quick)

p_dep_table_help <- tbl_regression(p_dep_reg_unad_tab_help, exp = TRUE, 
                                   label = list(p_alwys_help ~ 
                                                  "The doctor in the clinic is always helpful")) %>%
  modify_caption(glue::glue(" (N = {n_dep_m_help})"))

print(p_dep_table_help)

# combined - partner
unad_table_p_anx <- tbl_stack(
  list(p_anx_table_conf, p_anx_table_quick, p_anx_table_help),
  group_header = c(glue::glue("No confidence in NHS (N = {n_anx_p_conf})"), 
                   glue::glue("Believe Doctors would come quickly for sick child (N = {n_anx_p_quick})"), 
                   glue::glue("Believe Doctors in the clinic are always helpful (N = {n_anx_p_help})"))
) %>%
  modify_caption(glue::glue("Unadjusted Odds Ratios for Anxiety against all exposures for Partner"))

print(unad_table_p_anx)

unad_table_p_dep <- tbl_stack(
  list(p_dep_table_conf, p_dep_table_quick, p_dep_table_help),
  group_header = c(glue::glue("No confidence in NHS (N = {n_dep_p_conf})"), 
                   glue::glue("Believe Doctors would come quickly for sick child (N = {n_dep_p_quick})"), 
                   glue::glue("Believe Doctors in the clinic are always helpful (N = {n_dep_p_help})"))
) %>%
  modify_caption(glue::glue(" Unadjusted Odds Ratios for Depression against all exposures"))

print(unad_table_p_dep)

#-------------------------------------------------------------------------------
# adjusted model 1 
# mother no confidence
df_working$m_parity <- as.numeric(df_working$m_parity)

m_anx_reg_adj_1_conf <- glm(
  m_anx ~ m_conf_nhs + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_anx_score + m_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_anx_reg_adj_1_conf)

m_adj_anx_table_1_conf <- tbl_regression(m_anx_reg_adj_1_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           m_conf_nhs ~ "I don’t have any confidence in the national health service",
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
  modify_caption("**Model 1: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_1_conf)

# mother quick
df_working$m_parity <- as.numeric(df_working$m_parity)

m_anx_reg_adj_1_quick <- glm(
  m_anx ~ m_ill_quick + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_anx_score + m_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_anx_reg_adj_1_quick)

m_adj_anx_table_1_quick <- tbl_regression(m_anx_reg_adj_1_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            m_ill_quick ~ 
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
  modify_caption("**Model 1: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_1_quick)

# always helpful

m_anx_reg_adj_1_help <- glm(
  m_anx ~ m_alwys_help + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_anx_score + m_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_anx_reg_adj_1_help)

m_adj_anx_table_1_help <- tbl_regression(m_anx_reg_adj_1_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           m_alwys_help ~
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
  modify_caption("**Model 1: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_1_help)

# mother depression
df_working$m_parity <- as.numeric(df_working$m_parity)

m_dep_reg_adj_1_conf <- glm(
  m_dep ~ m_conf_nhs + m_age + m_parity + m_mar_status + 
    m_relig + m_sep + m_edu_level + m_home_own_status + m_anx_score + m_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_dep_reg_adj_1_conf)

m_adj_dep_table_1_conf <- tbl_regression(m_dep_reg_adj_1_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           m_conf_nhs ~ 
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
  modify_caption("**Model 1: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_1_conf)

# mother quick
df_working$m_parity <- as.numeric(df_working$m_parity)

m_dep_reg_adj_1_quick <- glm(
  m_dep ~ m_ill_quick + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_anx_score + m_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_dep_reg_adj_1_quick)

m_adj_dep_table_1_quick <- tbl_regression(m_dep_reg_adj_1_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            m_ill_quick ~ 
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
  modify_caption("**Model 1: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_1_quick)

# always helpful

m_dep_reg_adj_1_help <- glm(
  m_dep ~ m_alwys_help + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_anx_score + m_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_dep_reg_adj_1_help)

m_adj_dep_table_1_help <- tbl_regression(m_dep_reg_adj_1_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           m_alwys_help ~ 
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
  modify_caption("**Model 1: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_1_help)

# partner adjusted model 1
# adjusted model 1 
# no confidence
df_working$m_parity <- as.numeric(df_working$m_parity)

p_anx_reg_adj_1_conf <- glm(
  p_anx ~ p_conf_nhs + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + p_anx_score + p_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_anx_reg_adj_1_conf)

p_adj_anx_table_1_conf <- tbl_regression(p_anx_reg_adj_1_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           p_conf_nhs ~ 
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
  modify_caption("**Model 1: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_1_conf)

# partner quick
df_working$m_parity <- as.numeric(df_working$m_parity)

p_anx_reg_adj_1_quick <- glm(
  p_anx ~ p_ill_quick + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + p_anx_score + p_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_anx_reg_adj_1_quick)

p_adj_anx_table_1_quick <- tbl_regression(p_anx_reg_adj_1_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            p_ill_quick ~ 
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
  modify_caption("**Model 1: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_1_quick)

# always helpful

p_anx_reg_adj_1_help <- glm(
  p_anx ~ p_alwys_help + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + p_anx_score + p_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_anx_reg_adj_1_help)

p_adj_anx_table_1_help <- tbl_regression(p_anx_reg_adj_1_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           p_alwys_help ~ 
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
  modify_caption("**Model 1: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_1_help)

# mother depression
df_working$m_parity <- as.numeric(df_working$m_parity)

p_dep_reg_adj_1_conf <- glm(
  p_dep ~ p_conf_nhs + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + p_anx_score + p_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_dep_reg_adj_1_conf)

p_adj_dep_table_1_conf <- tbl_regression(p_dep_reg_adj_1_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           p_conf_nhs ~   
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
  modify_caption("**Model 1: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_1_conf)

# mother quick
df_working$m_parity <- as.numeric(df_working$m_parity)

p_dep_reg_adj_1_quick <- glm(
  p_dep ~ p_ill_quick + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + p_anx_score + p_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_dep_reg_adj_1_quick)

p_adj_dep_table_1_quick <- tbl_regression(p_dep_reg_adj_1_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            p_ill_quick ~  
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
  modify_caption("**Model 1: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_1_quick)

# always helpful

p_dep_reg_adj_1_help <- glm(
  p_dep ~ p_alwys_help + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + p_anx_score + p_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_dep_reg_adj_1_help)

p_adj_dep_table_1_help <- tbl_regression(p_dep_reg_adj_1_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           p_alwys_help ~  
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
  modify_caption("**Model 1: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_1_help)



#-------------------------------------------------------------------------------
# adjusted model 2 
# mother no confidence
df_working$m_parity <- as.numeric(df_working$m_parity)

m_anx_reg_adj_2_conf <- glm(
  m_anx ~ m_conf_nhs + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth + m_anx_score + m_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_anx_reg_adj_2_conf)

m_adj_anx_table_2_conf <- tbl_regression(m_anx_reg_adj_2_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           m_conf_nhs ~ "I don’t have any confidence in the national health service",
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
  modify_caption("**Model 2: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_2_conf)

# mother quick

m_anx_reg_adj_2_quick <- glm(
  m_anx ~ m_ill_quick + m_age + m_parity + m_mar_status + 
    m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth + m_anx_score + m_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_anx_reg_adj_2_quick)

m_adj_anx_table_2_quick <- tbl_regression(m_anx_reg_adj_2_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            m_ill_quick ~ 
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
  modify_caption("**Model 2: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_2_quick)

# always helpful

m_anx_reg_adj_2_help <- glm(
  m_anx ~ m_alwys_help + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth + m_anx_score + m_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_anx_reg_adj_2_help)

m_adj_anx_table_2_help <- tbl_regression(m_anx_reg_adj_2_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           m_alwys_help ~ 
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
  modify_caption("**Model 2: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_2_help)

# mother depression

m_dep_reg_adj_2_conf <- glm(
  m_dep ~ m_conf_nhs + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth + m_anx_score + m_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_dep_reg_adj_2_conf)

m_adj_dep_table_2_conf <- tbl_regression(m_dep_reg_adj_2_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           m_conf_nhs ~ "I don’t have any confidence in the national health service",
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
  modify_caption("**Model 2: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_2_conf)

# mother quick
m_dep_reg_adj_2_quick <- glm(
  m_dep ~ m_ill_quick + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth + m_anx_score + m_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_dep_reg_adj_2_quick)

m_adj_dep_table_2_quick <- tbl_regression(m_dep_reg_adj_2_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            m_ill_quick ~ 
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
  modify_caption("**Model 2: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_2_quick)

# always helpful

m_dep_reg_adj_2_help <- glm(
  m_dep ~ m_alwys_help + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth + m_anx_score + m_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_dep_reg_adj_2_help)

m_adj_dep_table_2_help <- tbl_regression(m_dep_reg_adj_2_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           m_alwys_help ~ 
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
  modify_caption("**Model 2: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_2_help)


# partner adjusted model 2
# no confidence
p_anx_reg_adj_2_conf <- glm(
  p_anx ~ p_conf_nhs + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_anx_reg_adj_2_conf)

p_adj_anx_table_2_conf <- tbl_regression(p_anx_reg_adj_2_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           p_conf_nhs ~ "I don’t have any confidence in the national health service",
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
  modify_caption("**Model 2: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_2_conf)

# partner quick

p_anx_reg_adj_2_quick <- glm(
  p_anx ~ p_ill_quick + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_anx_reg_adj_2_quick)

p_adj_anx_table_2_quick <- tbl_regression(p_anx_reg_adj_2_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            p_ill_quick ~ 
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
  modify_caption("**Model 2: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_2_quick)

# always helpful

p_anx_reg_adj_2_help <- glm(
  p_anx ~ p_alwys_help + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_anx_reg_adj_2_help)

p_adj_anx_table_2_help <- tbl_regression(p_anx_reg_adj_2_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           p_alwys_help ~ 
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
  modify_caption("**Model 2: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_2_help)

# partner depression
p_dep_reg_adj_2_conf <- glm(
  p_dep ~ p_conf_nhs + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_dep_reg_adj_2_conf)

p_adj_dep_table_2_conf <- tbl_regression(p_dep_reg_adj_2_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           p_conf_nhs ~   "I don’t have any confidence in the national health service",
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
  modify_caption("**Model 2: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_2_conf)

# mother quick
p_dep_reg_adj_2_quick <- glm(
  p_dep ~ p_ill_quick + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_dep_reg_adj_2_quick)

p_adj_dep_table_2_quick <- tbl_regression(p_dep_reg_adj_2_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            p_ill_quick ~  
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
  modify_caption("**Model 2: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_2_quick)

# always helpful

p_dep_reg_adj_2_help <- glm(
  p_dep ~ p_alwys_help + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_dep_reg_adj_2_help)

p_adj_dep_table_2_help <- tbl_regression(p_dep_reg_adj_2_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           p_alwys_help ~  
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
  modify_caption("**Model 2: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_2_help)

#-------------------------------------------------------------------------------
# adjusted model 3
# mother no confidence
m_anx_reg_adj_3_conf <- glm(
  m_anx ~ m_conf_nhs + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth + m_anx_score + m_dep_score +
    m_soc_supp + m_weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_anx_reg_adj_3_conf)

m_adj_anx_table_3_conf <- tbl_regression(m_anx_reg_adj_3_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           m_conf_nhs ~ "I don’t have any confidence in the national health service",
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
                                           m_dep_score ~ "Crown-crisp depression score",
                                           m_soc_supp ~ "Mother's social support score",
                                           m_weigh_life ~ "Mother's weighted life events score"
                                         )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_3_conf)

# mother quick

m_anx_reg_adj_3_quick <- glm(
  m_anx ~ m_ill_quick + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth + m_anx_score + m_dep_score 
  + m_soc_supp + m_weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_anx_reg_adj_3_quick)

m_adj_anx_table_3_quick <- tbl_regression(m_anx_reg_adj_3_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            m_ill_quick ~ "I know that if my child was very ill my doctor would come quickly",
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
                                            m_dep_score ~ "Crown-crisp depression score",
                                            m_soc_supp ~ "Mother's social support score",
                                            m_weigh_life ~ "Mother's weighted life events score"
                                          )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_3_quick)

# always helpful

m_anx_reg_adj_3_help <- glm(
  m_anx ~ m_alwys_help + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth
  + m_anx_score + m_dep_score + m_soc_supp + m_weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_anx_reg_adj_3_help)

m_adj_anx_table_3_help <- tbl_regression(m_anx_reg_adj_3_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           m_alwys_help ~ "The doctor in the clinic is always helpful",
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
                                           m_dep_score ~ "Crown-crisp depression score",
                                           m_soc_supp ~ "Mother's social support score",
                                           m_weigh_life ~ "Mother's weighted life events score"
                                         )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for Anxiety**")

print(m_adj_anx_table_3_help)

# mother depression

m_dep_reg_adj_3_conf <- glm(
  m_dep ~ m_conf_nhs + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth
  + m_anx_score + m_dep_score + m_soc_supp + m_weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_dep_reg_adj_3_conf)

m_adj_dep_table_3_conf <- tbl_regression(m_dep_reg_adj_3_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           m_conf_nhs ~ "I don’t have any confidence in the national health service",
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
                                           m_dep_score ~ "Crown-crisp depression score",
                                           m_soc_supp ~ "Mother's social support score",
                                           m_weigh_life ~ "Mother's weighted life events score"
                                         )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_3_conf)

# mother quick
m_dep_reg_adj_3_quick <- glm(
  m_dep ~ m_ill_quick + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth
  + m_anx_score + m_dep_score + m_soc_supp + m_weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_dep_reg_adj_3_quick)

m_adj_dep_table_3_quick <- tbl_regression(m_dep_reg_adj_3_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            m_ill_quick ~ "I know that if my child was very ill my doctor would come quickly",
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
                                            m_dep_score ~ "Crown-crisp depression score",
                                            m_soc_supp ~ "Mother's social support score",
                                            m_weigh_life ~ "Mother's weighted life events score"
                                          )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_3_quick)

# always helpful

m_dep_reg_adj_3_help <- glm(
  m_dep ~ m_alwys_help + m_age + m_parity + m_mar_status +
    m_relig + m_sep + m_edu_level + m_home_own_status + m_dr_change + m_eval_hlth
  + m_anx_score + m_dep_score + m_soc_supp + m_weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

print(m_dep_reg_adj_3_help)

m_adj_dep_table_3_help <- tbl_regression(m_dep_reg_adj_3_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           m_alwys_help ~ "The doctor in the clinic is always helpful",
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
                                           m_dep_score ~ "Crown-crisp depression score",
                                           m_soc_supp ~ "Mother's social support score",
                                           m_weigh_life ~ "Mother's weighted life events score"
                                         )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for Depression**")

print(m_adj_dep_table_3_help)


# partner adjusted model 3
# no confidence
p_anx_reg_adj_3_conf <- glm(
  p_anx ~ p_conf_nhs + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score
  + p_soc_supp + p_weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_anx_reg_adj_3_conf)

p_adj_anx_table_3_conf <- tbl_regression(p_anx_reg_adj_3_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           p_conf_nhs ~ "I don’t have any confidence in the national health service",
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
                                           p_dep_score ~ "Crown-crisp depression score",
                                           p_soc_supp ~ "Partner's social support score",
                                           p_weigh_life ~ "Partner's weighted life events score"
                                         )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_3_conf)

# partner quick

p_anx_reg_adj_3_quick <- glm(
  p_anx ~ p_ill_quick + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score
  + p_soc_supp + p_weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_anx_reg_adj_3_quick)

p_adj_anx_table_3_quick <- tbl_regression(p_anx_reg_adj_3_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            p_ill_quick ~ 
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
                                            p_dep_score ~ "Crown-crisp depression score",
                                            p_soc_supp ~ "Partner's social support score",
                                            p_weigh_life ~ "Partner's weighted life events score"
                                          )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_3_quick)

# always helpful

p_anx_reg_adj_3_help <- glm(
  p_anx ~ p_alwys_help + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score
  + p_soc_supp + p_weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_anx_reg_adj_3_help)

p_adj_anx_table_3_help <- tbl_regression(p_anx_reg_adj_3_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           p_alwys_help ~ "The doctor in the clinic is always helpful",
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
                                           p_dep_score ~ "Crown-crisp depression score",
                                           p_soc_supp ~ "Partner's social support score",
                                           p_weigh_life ~ "Partner's weighted life events score"
                                         )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for Anxiety**")

print(p_adj_anx_table_3_help)

# partner depression
p_dep_reg_adj_3_conf <- glm(
  p_dep ~ p_conf_nhs + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score
  + p_soc_supp + p_weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_dep_reg_adj_3_conf)

p_adj_dep_table_3_conf <- tbl_regression(p_dep_reg_adj_3_conf,
                                         exponentiate = TRUE,
                                         label = list(
                                           p_conf_nhs ~   "I don’t have any confidence in the national health service",
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
                                           p_dep_score ~ "Crown-crisp depression score",
                                           p_soc_supp ~ "Partner's social support score",
                                           p_weigh_life ~ "Partner's weighted life events score"
                                         )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_3_conf)

# mother quick
p_dep_reg_adj_3_quick <- glm(
  p_dep ~ p_ill_quick + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score
  + p_soc_supp + p_weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_dep_reg_adj_3_quick)

p_adj_dep_table_3_quick <- tbl_regression(p_dep_reg_adj_3_quick,
                                          exponentiate = TRUE,
                                          label = list(
                                            p_ill_quick ~  
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
                                            p_dep_score ~ "Crown-crisp depression score",
                                            p_soc_supp ~ "Partner's social support score",
                                            p_weigh_life ~ "Partner's weighted life events score"
                                          )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_3_quick)

# always helpful

p_dep_reg_adj_3_help <- glm(
  p_dep ~ p_alwys_help + p_age + m_parity + p_mar_status +
    p_relig + p_sep + p_edu_level + m_home_own_status + m_dr_change + p_eval_hlth + p_anx_score + p_dep_score
  + p_soc_supp + p_weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

print(p_dep_reg_adj_3_help)

p_adj_dep_table_3_help <- tbl_regression(p_dep_reg_adj_3_help,
                                         exponentiate = TRUE,
                                         label = list(
                                           p_alwys_help ~  
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
                                           p_dep_score ~ "Crown-crisp depression score",
                                           p_soc_supp ~ "Partner's social support score",
                                           p_weigh_life ~ "Partner's weighted life events score"
                                         )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for Depression**")

print(p_adj_dep_table_3_help)

# merged tables 
# sample sizes for each model
n_anx_adj_0_conf_m <- nobs(m_anx_reg_unad_tab_conf)
n_anx_adj_1_conf_m <- nobs(m_anx_reg_adj_1_conf)
n_anx_adj_2_conf_m <- nobs(m_anx_reg_adj_2_conf)
n_anx_adj_3_conf_m <- nobs(m_anx_reg_adj_3_conf)

n_anx_adj_0_quick_m <- nobs(m_anx_reg_unad_tab_quick)
n_anx_adj_1_quick_m <- nobs(m_anx_reg_adj_1_quick)
n_anx_adj_2_quick_m <- nobs(m_anx_reg_adj_2_quick)
n_anx_adj_3_quick_m <- nobs(m_anx_reg_adj_3_quick)

n_anx_adj_0_help_m <- nobs(m_anx_reg_unad_tab_help)
n_anx_adj_1_help_m <- nobs(m_anx_reg_adj_1_help)
n_anx_adj_2_help_m <- nobs(m_anx_reg_adj_2_help)
n_anx_adj_3_help_m <- nobs(m_anx_reg_adj_3_help)

n_anx_adj_0_conf_p <- nobs(p_anx_reg_unad_tab_conf)
n_anx_adj_1_conf_p <- nobs(p_anx_reg_adj_1_conf)
n_anx_adj_2_conf_p <- nobs(p_anx_reg_adj_2_conf)
n_anx_adj_3_conf_p <- nobs(p_anx_reg_adj_3_conf)

n_anx_adj_0_quick_p <- nobs(p_anx_reg_unad_tab_quick)
n_anx_adj_1_quick_p <- nobs(p_anx_reg_adj_1_quick)
n_anx_adj_2_quick_p <- nobs(p_anx_reg_adj_2_quick)
n_anx_adj_3_quick_p <- nobs(p_anx_reg_adj_3_quick)


n_anx_adj_1_help_p <- nobs(p_anx_reg_adj_1_help)
n_anx_adj_2_help_p <- nobs(p_anx_reg_adj_2_help)
n_anx_adj_3_help_p <- nobs(p_anx_reg_adj_3_help)
n_anx_adj_0_help_p <- nobs(p_anx_reg_unad_tab_help)

##
n_dep_adj_0_conf_m <-  nobs(m_dep_reg_unad_tab_conf)
n_dep_adj_1_conf_m <-  nobs(m_dep_reg_adj_1_conf)
n_dep_adj_2_conf_m <-  nobs(m_dep_reg_adj_2_conf)
n_dep_adj_3_conf_m <-  nobs(m_dep_reg_adj_3_conf)

n_dep_adj_0_quick_m <- nobs(m_dep_reg_unad_tab_quick)
n_dep_adj_1_quick_m <- nobs(m_dep_reg_adj_1_quick)
n_dep_adj_2_quick_m <- nobs(m_dep_reg_adj_2_quick)
n_dep_adj_3_quick_m <- nobs(m_dep_reg_adj_3_quick)

n_dep_adj_0_help_m <-  nobs(m_dep_reg_unad_tab_help)
n_dep_adj_1_help_m <-  nobs(m_dep_reg_adj_1_help)
n_dep_adj_2_help_m <-  nobs(m_dep_reg_adj_2_help)
n_dep_adj_3_help_m <-  nobs(m_dep_reg_adj_3_help)

n_dep_adj_0_conf_p <-  nobs(p_dep_reg_unad_tab_conf)
n_dep_adj_1_conf_p <-  nobs(p_dep_reg_adj_1_conf)
n_dep_adj_2_conf_p <-  nobs(p_dep_reg_adj_2_conf)
n_dep_adj_3_conf_p <-  nobs(p_dep_reg_adj_3_conf)

n_dep_adj_0_quick_p <- nobs(p_dep_reg_unad_tab_quick)
n_dep_adj_1_quick_p <- nobs(p_dep_reg_adj_1_quick)
n_dep_adj_2_quick_p <- nobs(p_dep_reg_adj_2_quick)
n_dep_adj_3_quick_p <- nobs(p_dep_reg_adj_3_quick)

n_dep_adj_0_help_p <-  nobs(p_dep_reg_unad_tab_help)
n_dep_adj_1_help_p <-  nobs(p_dep_reg_adj_1_help)
n_dep_adj_2_help_p <-  nobs(p_dep_reg_adj_2_help)
n_dep_adj_3_help_p <-  nobs(p_dep_reg_adj_3_help)

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
  modify_caption(glue::glue("**All Models for Question 1: Adjusted Odds Ratios for Anxiety for Mothers, (N = {n_anx_m_conf}, {n_anx_adj_1_conf_m}, {n_anx_adj_2_conf_m}, {n_anx_adj_3_conf_m}))**"))

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
) %>%
  modify_caption(glue::glue("**All Models for Question 1: Adjusted Odds Ratios for Anxiety for Partners, (N = {n_anx_p_conf}, {n_anx_adj_1_conf_p}, {n_anx_adj_2_conf_p}, {n_anx_adj_3_conf_p}))**"))

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
  modify_caption(glue::glue("**All Models for Question 1: Adjusted Odds Ratios for Depression for Mothers, (N = {n_dep_m_conf}, {n_dep_adj_1_conf_m}, {n_dep_adj_2_conf_m}, {n_dep_adj_3_conf_m}))**"))

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
  modify_caption(glue::glue("**All Models for Question 1: Adjusted Odds Ratios for Depression for Partners, (N = {n_dep_p_conf}, {n_dep_adj_1_conf_p}, {n_dep_adj_2_conf_p}, {n_dep_adj_3_conf_p}))**"))

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
  modify_caption(glue::glue("**All Models for Question 2: Adjusted Odds Ratios for Anxiety for Mothers, (N = {n_anx_m_quick}, {n_anx_adj_1_quick_m}, {n_anx_adj_2_quick_m}, {n_anx_adj_3_quick_m}))**"))

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
  modify_caption(glue::glue("**All Models for Question 2: Adjusted Odds Ratios for Anxiety for Partners, (N = {n_anx_p_conf}, {n_anx_adj_1_conf_p}, {n_anx_adj_2_conf_p}, {n_anx_adj_3_conf_p}))**"))

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
  modify_caption(glue::glue("**All Models for Question 2: Adjusted Odds Ratios for Depression for Mothers, (N = {n_dep_m_quick}, {n_dep_adj_1_quick_m}, {n_dep_adj_2_quick_m}, {n_dep_adj_3_quick_m}))**"))

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
  modify_caption(glue::glue("All Models for Question 2: Adjusted Odds Ratios for Depression for Partners, (N = {n_dep_p_quick}, {n_dep_adj_1_quick_p}, {n_dep_adj_2_quick_p}, {n_dep_adj_3_quick_p}))**"))

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
  modify_caption(glue::glue("All Models for Question 3: Adjusted Odds Ratios for Anxiety for Mothers, (N = {n_anx_m_conf}, {n_anx_adj_1_conf_m}, {n_anx_adj_2_conf_m}, {n_anx_adj_3_conf_m}))**"))

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
  modify_caption(glue::glue("All Models for Question 3: Adjusted Odds Ratios for Anxiety for Partners, (N = {n_anx_p_conf}, {n_anx_adj_1_conf_p}, {n_anx_adj_2_conf_p}, {n_anx_adj_3_conf_p}))**"))

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
  modify_caption(glue::glue("All Models for Question 3: Adjusted Odds Ratios for Depression for Mothers, (N = {n_dep_m_conf}, {n_dep_adj_1_conf_m}, {n_dep_adj_2_conf_m}, {n_dep_adj_3_conf_m}))**"))

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
  modify_caption(glue::glue("All Models for Question 3: Adjusted Odds Ratios for Anxiety for Partners, (N = {n_dep_p_conf}, {n_dep_adj_1_conf_p}, {n_dep_adj_2_conf_p}, {n_dep_adj_3_conf_p}))**"))

# Print table
p_adj_table_1_dep_help

# Regression tables with main predictors only  

## Anxiety 

### I don't have any confidence in the NHS

# Function to keep only the row(s) for m_conf_nhs
filter_anx_conf_m <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "m_conf_nhs"))
}

# Apply filtering to each table
m_anx_table_conf_clean <- filter_anx_conf_m(m_anx_table_conf)
m_adj_anx_table_1_conf_clean <- filter_anx_conf_m(m_adj_anx_table_1_conf)
m_adj_anx_table_2_conf_clean <- filter_anx_conf_m(m_adj_anx_table_2_conf)
m_adj_anx_table_3_conf_clean <- filter_anx_conf_m(m_adj_anx_table_3_conf)

# Now merge only the predictor rows
m_adj_table_1_anx_conf <- tbl_merge(
  tbls = list(
    m_anx_table_conf_clean,
    m_adj_anx_table_1_conf_clean, 
    m_adj_anx_table_2_conf_clean, 
    m_adj_anx_table_3_conf_clean
  ),
  tab_spanner = c("**Unadjusted Model**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 1: Adjusted Odds Ratios for Anxiety for Mothers, (N = {n_anx_m_conf}, {n_anx_adj_1_conf_m}, {n_anx_adj_2_conf_m}, {n_anx_adj_3_conf_m})**"))

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
      "Model 1" = "#854f99",    
      "Model 2" = "#f44973",    
      "Model 3" = "#f2923f"      
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

# Function to keep only the row(s) for p_conf_nhs
filter_anx_conf_p <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "p_conf_nhs"))
}

# Apply filtering to each table
p_anx_table_conf_clean <- filter_anx_conf_p(p_anx_table_conf)
p_adj_anx_table_1_conf_clean <- filter_anx_conf_p(p_adj_anx_table_1_conf)
p_adj_anx_table_2_conf_clean <- filter_anx_conf_p(p_adj_anx_table_2_conf)
p_adj_anx_table_3_conf_clean <- filter_anx_conf_p(p_adj_anx_table_3_conf)

# Now merge only the predictor rows
p_adj_table_1_anx_conf <- tbl_merge(
  tbls = list(
    p_anx_table_conf_clean,
    p_adj_anx_table_1_conf_clean, 
    p_adj_anx_table_2_conf_clean, 
    p_adj_anx_table_3_conf_clean
  ),
  tab_spanner = c("**Unadjusted Model**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 1: Adjusted Odds Ratios for Anxiety for Partners, (N = {n_anx_p_conf}, {n_anx_adj_1_conf_p}, {n_anx_adj_2_conf_p}, {n_anx_adj_3_conf_p})**"))

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
      "Model 1" = "#854f99",     
      "Model 2" = "#f44973",     
      "Model 3" = "#f2923f"      
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
  modify_caption("Combined Mother and Partner outcomes for I don't have any confidence in the NHS, against Anxiety and Depression**") %>%
  modify_header(
    condition = "**Mental health condition**",
    participant_type = "**Participant Type**"
  )

combined_anx_conf_table


### I know that if my child was very ill my doctor would come quickly

filter_anx_quick_m <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "m_ill_quick"))
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
  tab_spanner = c("**Unadjusted model**", "**Model 1**", "**Model 2**", "**Model 3**")
)  %>%
  modify_caption(glue::glue("**All Models for Question 2: Adjusted Odds Ratios for Anxiety for Mothers, (N = {n_anx_m_quick}, {n_anx_adj_1_quick_m}, {n_anx_adj_2_quick_m}, {n_anx_adj_3_quick_m})**"))

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
      "Model 1" = "#854f99",     
      "Model 2" = "#f44973",    
      "Model 3" = "#f2923f"      
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
    modify_table_body(~ filter(.x, variable == "p_ill_quick"))
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
  tab_spanner = c("**Unadjusted model**", "**Model 1**", "**Model 2**", "**Model 3**")
)  %>%
  modify_caption(glue::glue("**All Models for Question 2: Adjusted Odds Ratios for Anxiety for Partners, (N = {n_anx_p_quick}, {n_anx_adj_1_quick_p}, {n_anx_adj_2_quick_p}, {n_anx_adj_3_quick_p})**"))

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
      "Model 1" = "#854f99",     
      "Model 2" = "#f44973",     
      "Model 3" = "#f2923f"      
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
  modify_caption("**Table 25. Combined Mother and Partner outcomes for I know if my child was ill, a doctor would come quickly against Anxiety and Depression**") %>%
  modify_header(condition = "**Mental health condition**",
                participant_type = "**Participant Type**"
  )

combined_anx_quick_table


### The doctor in the clinic is always helpful

filter_anx_help_m <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "m_alwys_help"))
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
  modify_caption(glue::glue("**All Models for Question 3: Adjusted Odds Ratios for Anxiety for Mothers, (N = {n_anx_m_help}, {n_anx_adj_1_help_m}, {n_anx_adj_2_help_m}, {n_anx_adj_3_help_m})**"))

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
    modify_table_body(~ filter(.x, variable == "p_alwys_help"))
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
  tab_spanner = c("**Unadjusted model**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 3: Adjusted Odds Ratios for Anxiety for Mothers, (N = {n_anx_p_help}, {n_anx_adj_1_help_p}, {n_anx_adj_2_help_p}, {n_anx_adj_3_help_p})**"))

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
  modify_caption("Combined Mother and Partner outcomes for The doctor in the clinic is always helpful against Anxiety and Depression**") %>%
  modify_header(condition = "**Mental health condition**",
                participant_type = "**Participant Type**")

combined_anx_help_table


## Depression  

### I don't have any confidence in the NHS


# Function to keep only the row(s) for m_conf_nhs
filter_dep_conf_m <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "m_conf_nhs"))
}

# Apply filtering to each table
m_dep_table_conf_clean <- filter_dep_conf_m(m_dep_table_conf)
m_adj_dep_table_1_conf_clean <- filter_dep_conf_m(m_adj_dep_table_1_conf)
m_adj_dep_table_2_conf_clean <- filter_dep_conf_m(m_adj_dep_table_2_conf)
m_adj_dep_table_3_conf_clean <- filter_dep_conf_m(m_adj_dep_table_3_conf)

# Now merge only the predictor rows
m_adj_table_1_dep_conf <- tbl_merge(
  tbls = list(
    m_dep_table_conf_clean,
    m_adj_dep_table_1_conf_clean, 
    m_adj_dep_table_2_conf_clean, 
    m_adj_dep_table_3_conf_clean
  ),
  tab_spanner = c("**Unadjusted model**", "**Model 1**", "**Model 2**", "**Model 3**")
)  %>%
  modify_caption(glue::glue("**All Models for Question 1: Adjusted Odds Ratios for Depression for Mothers, (N = {n_dep_m_conf}, {n_dep_adj_1_conf_m}, {n_dep_adj_2_conf_m}, {n_dep_adj_3_conf_m})**"))

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

# Function to keep only the row(s) for m_conf_nhs
filter_dep_conf_p <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "p_conf_nhs"))
}

# Apply filtering to each table
p_dep_table_conf_clean <- filter_dep_conf_p(p_dep_table_conf)
p_adj_dep_table_1_conf_clean <- filter_dep_conf_p(p_adj_dep_table_1_conf)
p_adj_dep_table_2_conf_clean <- filter_dep_conf_p(p_adj_dep_table_2_conf)
p_adj_dep_table_3_conf_clean <- filter_dep_conf_p(p_adj_dep_table_3_conf)

# Now merge only the predictor rows
p_adj_table_1_dep_conf <- tbl_merge(
  tbls = list(
    p_dep_table_conf_clean,
    p_adj_dep_table_1_conf_clean, 
    p_adj_dep_table_2_conf_clean, 
    p_adj_dep_table_3_conf_clean
  ),
  tab_spanner = c("**Unadjusted model**", "**Model 1**", "**Model 2**", "**Model 3**")
) %>%
  modify_caption(glue::glue("**All Models for Question 1: Adjusted Odds Ratios for Depression for Partners, (N = {n_dep_p_conf}, {n_dep_adj_1_conf_p}, {n_dep_adj_2_conf_p}, {n_dep_adj_3_conf_p})**"))

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
  modify_caption("Combined Mother and Partner results for The doctor in the clinic is always helpful against Depression outcomes**") %>%
  modify_header(condition = "**Mental health condition**",
                participant_type = "**Participant Type**")

combined_dep_conf_table



### I know that if my child was ill, a doctor would come quickly


filter_dep_quick_m <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "m_ill_quick"))
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
  tab_spanner = c("**Unadjusted model**", "**Model 1**", "**Model 2**", "**Model 3**")
)  %>%
  modify_caption(glue::glue("**All Models for Question 2: Adjusted Odds Ratios for Depression for Mothers, (N = {n_dep_m_quick}, {n_dep_adj_1_quick_m}, {n_dep_adj_2_quick_m}, {n_dep_adj_3_quick_m})**"))

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
      "Model 1" = "#854f99",     
      "Model 2" = "#f44973",     
      "Model 3" = "#f2923f"     
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

filter_dep_quick_p <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "p_ill_quick"))
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
  tab_spanner = c("**Unadjusted model**", "**Model 1**", "**Model 2**", "**Model 3**")
)  %>%
  modify_caption(glue::glue("**All Models for Question 2: Adjusted Odds Ratios for Depression for Partners, (N = {n_dep_p_quick}, {n_dep_adj_1_quick_p}, {n_dep_adj_2_quick_p}, {n_dep_adj_3_quick_p})**"))

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
      "Model 1" = "#854f99",     
      "Model 2" = "#f44973",     
      "Model 3" = "#f2923f"     
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
  modify_caption("Combined Mother and Partner results for I know that if my child was very ill, a doctor would come quickly against Depression outcomess**") %>%
  modify_header(condition = "**Mental health condition**",
                participant_type = "**Participant Type**")

combined_dep_quick_table


### The doctor in the clinic is always helpful


filter_dep_help_m <- function(gtsummary_obj) {
  gtsummary_obj %>%
    modify_table_body(~ filter(.x, variable == "m_alwys_help"))
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
  modify_caption(glue::glue("**All Models for Question 3: Adjusted Odds Ratios for Depression for Mothers, (N = {n_dep_m_conf}, {n_dep_adj_1_conf_m}, {n_dep_adj_2_conf_m}, {n_dep_adj_3_conf_m})**"))

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
    modify_table_body(~ filter(.x, variable == "p_alwys_help"))
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
  modify_caption(glue::glue("**All Models for Question 3: Adjusted Odds Ratios for Depression for Partners, (N = {n_dep_p_conf}, {n_dep_adj_1_conf_p}, {n_dep_adj_2_conf_p}, {n_dep_adj_3_conf_p})**"))

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
      "Model 1" = "#854f99",     
      "Model 2" = "#f44973",     
      "Model 3" = "#f2923f"     
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
  modify_caption("**Combined Mother and Partner results for The doctor in the clinic is always helpful against Depression outcomes**") %>%
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
  (here::here("6-year-timepoint/original-dataset/4_Analysis_and_Visualisation", "6y_master_anx_reg_table.rds"))
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
  (here::here("6-year-timepoint/original-dataset/4_Analysis_and_Visualisation", "6y_master_dep_reg_table.rds"))
)
# Master tables grouped by question 

## I don't have any confidence in the NHS

master_conf_table <- tbl_stack(
  tbls = list(
    combined_anx_conf_table,
    combined_dep_conf_table
  ))

master_conf_table

## I know that if my child was very ill, a doctor would come quickly

master_quick_table <- tbl_stack(
  tbls = list(
    combined_anx_quick_table,
    combined_dep_quick_table
  ))

master_quick_table



## The doctor in the clinic is always helpful

master_help_table <- tbl_stack(
  tbls = list(
    combined_anx_help_table,
    combined_dep_help_table
  ))

master_help_table

# Forest plots  

n_anx_m_conf <- nobs(m_anx_reg_unad_tab_conf)
n_anx_m_quick  <- nobs(m_anx_reg_unad_tab_quick)
n_anx_m_help <- nobs(m_anx_reg_unad_tab_help)

n_anx_p_conf <- nobs(p_anx_reg_unad_tab_conf)
n_anx_p_quick  <- nobs(p_anx_reg_unad_tab_quick)
n_anx_p_help <- nobs(p_anx_reg_unad_tab_help)

n_dep_m_conf <- nobs(m_dep_reg_unad_tab_conf)
n_dep_m_quick  <- nobs(m_dep_reg_unad_tab_quick)
n_dep_m_help <- nobs(m_dep_reg_unad_tab_help)

n_dep_p_conf <- nobs(p_dep_reg_unad_tab_conf)
n_dep_p_quick  <- nobs(p_dep_reg_unad_tab_quick)
n_dep_p_help <- nobs(p_dep_reg_unad_tab_help)



## Anxiety  

### Overall


df_long_anx_m_conf$N <-  c(1148, 1151, 1214, 1856)
df_long_anx_p_conf$N <-  c(407, 442, 457, 725)
df_long_anx_m_quick$N <- c(1153, 1156, 1220, 1867)
df_long_anx_p_quick$N <- c(406, 441, 456, 724)
df_long_anx_m_help$N <-  c(1126, 1129, 1192, 1825)
df_long_anx_p_help$N <-  c(406, 441, 456, 722)

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

combined_long_anx$outcome <- "Anxiety"

# Plot
plot_anx <- ggplot(combined_long_anx, aes(x = estimate, y = variable_label, color = model)) +
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
    title = "Trust vs Healthcare access for Anxiety, at 6 years",
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Attitudes towards doctors",
    color = "Model"
  ) +
  facet_wrap(~ group, nrow = 1) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

ggsave(
  filename = (here::here("6-year-timepoint/original-dataset/4_Analysis_and_Visualisation", "6y_master_anx_reg_forest_plot.png")),
  plot = plot_anx,
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
plot_anx_conf <- ggplot(combined_long_anx_conf, aes(x = estimate, y = variable_label, color = model)) +
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

print(plot_anx_conf)

ggsave(
  filename = "l_forest_anx_conf.png",
  plot = plot_anx_conf,
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
plot_anx_quick <- ggplot(combined_long_anx_quick, aes(x = estimate, y = variable_label, color = model)) +
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

print(plot_anx_quick)

ggsave(
  filename = "l_forest_anx_quick.png",
  plot = plot_anx_quick,
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
plot_anx_help <- ggplot(combined_long_anx_help, aes(x = estimate, y = variable_label, color = model)) +
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

print(plot_anx_help)

ggsave(
  filename = "l_forest_anx_help.png",
  plot = plot_anx_help,
  width = 10,
  height = 6,
  dpi = 300
)



## Depression  

### Overall

df_long_dep_m_conf$N <-  c(1207, 1211, 1285, 1957)
df_long_dep_p_conf$N <-  c(291, 325, 335, 569)
df_long_dep_m_quick$N <- c(1203, 1207, 1283, 1960)
df_long_dep_p_quick$N <- c(289, 323, 333, 567)
df_long_dep_m_help$N <-  c(1183, 1187, 1262, 1929)
df_long_dep_p_help$N <-  c(291, 325, 335, 567)

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


# Plot
plot_dep <- ggplot(combined_long_dep, aes(x = estimate, y = variable_label, color = model)) +
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
      "Unadjusted" = "#fbbb3c",
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
    title = "Trust vs Healthcare access for Depression, at 6 years",
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Attitudes towards doctors",
    color = "Model"
  ) +
  facet_wrap(~ group, nrow = 1) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

ggsave(
  filename = (here::here("6-year-timepoint/original-dataset/4_Analysis_and_Visualisation", "6y_master_dep_reg_forest_plot.png")),
  plot = plot_dep,
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
plot_dep_conf <- ggplot(combined_long_dep_conf, aes(x = estimate, y = variable_label, color = model)) +
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

print(plot_dep_conf)

ggsave(
  filename = "l_forest_dep_conf.png",
  plot = plot_anx_conf,
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
plot_dep_quick <- ggplot(combined_long_dep_quick, aes(x = estimate, y = variable_label, color = model)) +
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

print(plot_dep_quick)

ggsave(
  filename = "l_forest_dep_quick.png",
  plot = plot_dep_quick,
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

print(combined_long_dep_help)
# Plot
plot_dep_help <- ggplot(combined_long_dep_help, aes(x = estimate, y = variable_label, color = model)) +
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

print(plot_dep_help)

ggsave(
  filename = "l_forest_dep_help.png",
  plot = plot_dep_help,
  width = 10,
  height = 6,
  dpi = 300
)





# Combined


combined_all <- bind_rows(combined_long_anx, combined_long_dep) 

plot_all <- ggplot(combined_all, aes(x = estimate, y = variable_label, color = model)) +
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

print(plot_all)

ggsave(
  filename = (here::here("6-year-timepoint/original-dataset/4_Analysis_and_Visualisation", "6y_master_forest_plot.png")),
  plot = plot_all,
  width = 10,
  height = 6,
  dpi = 300
)


