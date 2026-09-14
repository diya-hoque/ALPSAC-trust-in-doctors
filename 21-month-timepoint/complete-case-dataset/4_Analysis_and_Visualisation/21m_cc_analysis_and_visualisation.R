# script: data analysis and visualisation 
#-------------------------------------------------------------------------------
source(here::here("21-month-timepoint/complete-case-dataset/1_Packages", "21m_packages.R"))

df_working <- read.csv(here::here("21-month-timepoint/complete-case-dataset/3_Descriptives", "df_working.csv" ))


#-------------------------------------------------------------------------------
# recoding to get data ready for logistic regression models - only including 
# 0 = people who experienced a condition and did not go to the dr
# 1 = people who experienced a condition and did go to the dr

# outcomes
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
# problems with periods
df_working$per <- as.numeric(as.character(df_working$per))

df_working$per <- ifelse(df_working$per == 1, 1,
                         ifelse(df_working$per == 0, 0,
                                ifelse(df_working$per == 2, NA_real_, df_working$per)))

#-------------------------------------------------------------------------------
# Regressions ---- 
# recoding for log regressions 
## anxiety
df_working$anx <- as.numeric(as.character(df_working$anx))

df_working$anx <- ifelse(df_working$anx == 1, 1,
                         ifelse(df_working$anx == 0, 0,
                                ifelse(df_working$anx == 2, NA_real_, df_working$anx)))
## depression
df_working$dep <- as.numeric(as.character(df_working$dep))

df_working$dep <- ifelse(df_working$dep == 1, 1,
                         ifelse(df_working$dep == 0, 0,
                                ifelse(df_working$dep == 2, NA_real_, df_working$dep)))
## menstrual problems 
df_working$per <- as.numeric(as.character(df_working$per))

df_working$per <- ifelse(df_working$per == 1, 1,
                         ifelse(df_working$per == 0, 0,
                                ifelse(df_working$per == 2, NA_real_, df_working$per)))


# unadjusted regression model
anx_reg_unad_tab <- glm(anx ~ sum_attitude, data = df_working, family = binomial())
dep_reg_unad_tab <- glm(dep ~ sum_attitude, data = df_working, family = binomial())
per_reg_unad_tab <- glm(per ~ sum_attitude, data = df_working, family = binomial())

anx_table <- tbl_regression(anx_reg_unad_tab, exponentiate = TRUE)
dep_table <- tbl_regression(dep_reg_unad_tab, exponentiate = TRUE)
per_table <- tbl_regression(per_reg_unad_tab, exponentiate = TRUE)


n_anx <- nobs(anx_reg_unad_tab)
n_dep <- nobs(dep_reg_unad_tab)
n_per <- nobs(per_reg_unad_tab)



anx_table <- anx_table %>%
  modify_table_body(~ .x %>% mutate(N = n_anx))

anx_table <- anx_table %>%
  modify_table_body(~.x %>% mutate(Condition = "Anxiety"))

dep_table <- dep_table %>%
  modify_table_body(~ .x %>% mutate(N = n_dep))

dep_table <- dep_table %>%
  modify_table_body(~.x %>% mutate(Condition = "Depression"))

per_table <- per_table %>%
  modify_table_body(~ .x %>% mutate(N = n_per))

per_table <- per_table %>%
  modify_table_body(~.x %>% mutate(Condition = "Menstrual Problems"))


unad_table <- tbl_stack(
  list(anx_table, dep_table, per_table))

unad_table$table_body <- unad_table$table_body %>%
  relocate(Condition, .after = label)

unad_table$table_body <- unad_table$table_body %>%
  relocate(N, .after = Condition)  

unad_table <- unad_table %>%
  modify_column_hide(columns = label) %>%
  modify_header(
    Condition = "**Condition**",
    N = "**N**",
    estimate = "**OR**",
    p.value = "**p-value**"
  ) %>%
  modify_caption("Unadjusted Odds Ratios for Mental Health Outcomes and Attitudes towards doctors")

unad_table

# unadjusted model - forest plot
forest_plot_unad <- bind_rows(
  tidy(anx_reg_unad_tab, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "sum_attitude") %>%
    mutate(Model = "Anxiety"),
  
  tidy(dep_reg_unad_tab, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "sum_attitude") %>%
    mutate(Model = "Depression"),
  
  tidy(per_reg_unad_tab, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "sum_attitude") %>%
    mutate(Model = "Menstrual Problems")
) %>%
  select(Model, estimate, conf.low, conf.high, p.value) %>%
  mutate(Model = factor(Model, levels = rev(c("Anxiety", "Depression", "Menstrual Problems"))))  

ggplot(forest_plot_unad, aes(x = estimate, y = Model, color = Model)) +  
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkgreen") +
  scale_color_manual(values = c(
    "Anxiety" = "#fc4a1a", 
    "Depression" = "#f7b733", 
    "Menstrual Problems" = "#008080"
  )) +
  labs(
    title = "Graph 1: Forest Plot of Unadjusted Odds Ratios",
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Condition",
    color = "Condition" 
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

#-------------------------------------------------------------------------------
# adjusted model 1
# changing parity and recent dr change from a factor (for cont tables) to numeric
df_working$parity <- as.numeric(df_working$parity)
df_working$dr_change <- as.numeric(df_working$dr_change)

anx_reg_adj_1 <- glm(
  anx ~ sum_attitude + age + parity + mar_status +
    relig + soc_class + edu_level + home_own_status + anx_score + dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

adj_anx_table_1 <- tbl_regression(anx_reg_adj_1,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score"                                                        )) %>%
  modify_caption("Model 1: Adjusted Odds Ratios for Anxiety**")


dep_reg_adj_1 <- glm(
  dep ~ sum_attitude + age + parity + mar_status +
    relig + soc_class + edu_level + home_own_status + anx_score + dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

adj_dep_table_1 <- tbl_regression(dep_reg_adj_1,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score" 
                                  )) %>%
  modify_caption("Model 1: Adjusted Odds Ratios for Depression**")


per_reg_adj_1 <- glm(
  per ~ sum_attitude + age + parity + mar_status +
    relig + soc_class + edu_level + home_own_status + anx_score + dep_score,
  data = df_working,
  family = binomial(link = "logit")
)

adj_per_table_1 <- tbl_regression(per_reg_adj_1,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score"
                                  )) %>%
  modify_caption("Model 1: Adjusted Odds Ratios for Problems with Periods**")

n_anx_1 <- nobs(anx_reg_adj_1)
n_dep_1 <- nobs(dep_reg_adj_1)
n_per_1 <- nobs(per_reg_adj_1)

adj_anx_table_1 <- tbl_regression(anx_reg_adj_1,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score" 
                                  )) %>%
  modify_caption(glue::glue("Model 1: Adjusted Odds Ratios for Anxiety (N = {n_anx_1})**"))

adj_dep_table_1 <- tbl_regression(dep_reg_adj_1,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score" 
                                  )) %>%
  modify_caption(glue::glue("Model 1: Adjusted Odds Ratios for Depression (N = {n_dep_1})**"))

adj_per_table_1 <- tbl_regression(per_reg_adj_1,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score"
                                  )) %>%
  modify_caption(glue::glue("Model 1: Adjusted Odds Ratios for Problems with Periods (N = {n_per_1})**"))

adj_table_1_all <- tbl_merge(
  tbls = list(adj_anx_table_1, adj_dep_table_1, adj_per_table_1),
  tab_spanner = c(glue::glue("**Anxiety (N = {n_anx_1})**"), 
                  glue::glue("**Depression (N = {n_dep_1})**"), 
                  glue::glue("**Problems with Periods (N = {n_per_1})**"))
) %>%
  modify_caption(glue::glue("Model 1: Adjusted Odds Ratios for Anxiety, Depression, and Problems with Periods (N = {n_anx_1}, {n_dep_1}, {n_per_1})**"))

adj_table_1_all


term_labels <- c(
  "sum_attitude" = "Attitudes towards doctors",
  "soc_class" = "Socioeconomic position",
  "relig" = "Religion",
  "parity" = "Parity",
  "mar_status" = "Marital status",
  "home_own_status" = "Home ownership status",
  "edu_level" = "Highest educational attainment",
  "age" = "Age",
  "anx_score" ~ "Crown crisp anxiety score",
  "dep_score" ~ "Crown crisp depression score"
)

custom_order_1 <- c(
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


anx_regression_results <- adj_anx_table_1$table_body
dep_regression_results <- adj_dep_table_1$table_body
per_regression_results <- adj_per_table_1$table_body


anx_regression_results$model <- "Anxiety"
dep_regression_results$model <- "Depression"
per_regression_results$model <- "Menstrual Problems"


combined_regression_results <- bind_rows(anx_regression_results, dep_regression_results, per_regression_results)

combined_regression_results <- combined_regression_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    model = factor(model, levels = c("Anxiety", "Depression", "Menstrual Problems")),
    term_label = term_labels[term],
    term_label = factor(term_label, levels = rev(custom_order_1))
  )

## adjusted model 1 forest plot
ggplot(combined_regression_results, aes(x = estimate, y = term_label, xmin = conf.low, xmax = conf.high, color = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(position = position_dodge(width = 0.5), height = 0.3) +
  labs(
    title = "Graph 2: Forest Plot of Adjusted Odds Ratios",
    x = "Odds Ratio (95% CI)",
    y = "",
    subtitle = "Based on Regression Model 1",
    color = "Condition"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
  scale_x_continuous(
    breaks = seq(0.00, 2.50, by = 0.50),
    limits = c(0.00, 2.50),
    labels = scales::label_number(accuracy = 0.01)
  ) + 
  scale_color_manual(values = c("Anxiety" = "#fc4a1a", "Depression" = "#f7b733", "Menstrual Problems" = "#008080"))

#-------------------------------------------------------------------------------
#adjusted model 2
anx_reg_adj_2 <- glm(
  anx ~ sum_attitude + age + parity + mar_status +
    relig + soc_class + edu_level + home_own_status + anx_score + dep_score + mum_hlth + dr_change,
  data = df_working,
  family = binomial(link = "logit")
)

adj_anx_table_2 <- tbl_regression(anx_reg_adj_2,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score", 
                                    mum_hlth ~ "Evaluation of own health",
                                    dr_change ~ "Recent doctor change"
                                  )) %>%
  modify_caption("Model 2: Adjusted Odds Ratios for the Likelihood of going to the Doctor for Anxiety**")


dep_reg_adj_2 <- glm(
  dep ~ sum_attitude + age + parity + mar_status +
    relig + soc_class + edu_level + home_own_status + anx_score + dep_score + mum_hlth + dr_change,
  data = df_working,
  family = binomial(link = "logit")
)

adj_dep_table_2 <- tbl_regression(dep_reg_adj_2,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score", 
                                    mum_hlth ~ "Evaluation of own health",
                                    dr_change ~ "Recent doctor change"
                                  )) %>%
  modify_caption("Model 2: Adjusted Odds Ratios for the Likelihood of going to the Doctor for Depression**")


per_reg_adj_2 <- glm(
  per ~ sum_attitude + age + parity + mar_status +
    relig + soc_class + edu_level + home_own_status + anx_score + dep_score + mum_hlth + dr_change,
  data = df_working,
  family = binomial(link = "logit")
)

adj_per_table_2 <- tbl_regression(per_reg_adj_2,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score", 
                                    mum_hlth ~ "Evaluation of own health",
                                    dr_change ~ "Recent doctor change"
                                  )) %>%
  modify_caption("Model 2: Adjusted Odds Ratios for the Likelihood of going to the Doctor for Problems with Periods**")

n_anx_2 <- nobs(anx_reg_adj_2)
n_dep_2 <- nobs(dep_reg_adj_2)
n_per_2 <- nobs(per_reg_adj_2)


adj_anx_table_2 <- tbl_regression(anx_reg_adj_2,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score", 
                                    mum_hlth ~ "Evaluation of own health",
                                    dr_change ~ "Recent doctor change"
                                  )) %>%
  modify_caption(glue::glue("Model 2: Adjusted Odds Ratios for Anxiety (N = {n_anx_2})**"))

adj_dep_table_2 <- tbl_regression(dep_reg_adj_2,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score", 
                                    mum_hlth ~ "Evaluation of own health",
                                    dr_change ~ "Recent doctor change"
                                  )) %>%
  modify_caption(glue::glue("Model 2: Adjusted Odds Ratios for Depression (N = {n_dep_2})**"))

adj_per_table_2 <- tbl_regression(per_reg_adj_2,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score", 
                                    mum_hlth ~ "Evaluation of own health",
                                    dr_change ~ "Recent doctor change"
                                  )) %>%
  modify_caption(glue::glue("Model 2: Adjusted Odds Ratios for Problems with Periods (N = {n_per_2})**"))


adj_table_2_all <- tbl_merge(
  tbls = list(adj_anx_table_2, adj_dep_table_2, adj_per_table_2),
  tab_spanner = c(glue::glue("**Anxiety (N = {n_anx_2})**"), 
                  glue::glue("**Depression (N = {n_dep_2})**"), 
                  glue::glue("**Problems with Periods (N = {n_per_2})**"))
) %>%
  modify_caption(glue::glue("Model 2: Adjusted Odds Ratios for Anxiety, Depression, and Problems with Periods (N = {n_anx_2}, {n_dep_2}, {n_per_2})**"))


adj_table_2_all

term_labels_2 <- c(
  "sum_attitude" = "Attitudes towards doctors",
  "age" = "Age",
  "soc_class" = "Socioeconomic position",
  "parity" = "Parity",
  "mar_status" = "Marital status",
  "home_own_status" = "Home ownership status",
  "relig" = "Religion",
  "edu_level" = "Highest educational attainment",
  "dr_change" = "Recent Doctor change",
  "mum_hlth" = "Evaluation of own health",
  "anx_score" ~ "Crown crisp anxiety score",
  "dep_score" ~ "Crown crisp depression score" 
)

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
  "Evaluation of own health",
  "Recent Doctor change"
)


anx_regression_results_2 <- adj_anx_table_2$table_body
dep_regression_results_2 <- adj_dep_table_2$table_body
per_regression_results_2 <- adj_per_table_2$table_body


anx_regression_results_2$model <- "Anxiety"
dep_regression_results_2$model <- "Depression"
per_regression_results_2$model <- "Menstrual Problems"


combined_regression_results_2 <- bind_rows(anx_regression_results_2, dep_regression_results_2, per_regression_results_2)


combined_regression_results_2 <- combined_regression_results_2 %>%
  filter(term != "(Intercept)") %>%
  mutate(
    model = factor(model, levels = c("Anxiety", "Depression", "Menstrual Problems")),
    term_label = term_labels_2[term],
    term_label = factor(term_label, levels = rev(custom_order_2))
  )

# adjusted model 2 forest plot
ggplot(combined_regression_results_2, aes(x = estimate, y = term_label, xmin = conf.low, xmax = conf.high, color = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(position = position_dodge(width = 0.5), height = 0.3) +
  labs(
    title = "Graph 3: Forest Plot of Adjusted Odds Ratios",
    x = "Odds Ratio (95% CI)",
    y = "",
    subtitle = "Based on Regression Model 2",
    color = "Condition"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
  scale_x_continuous(
    breaks = seq(0.00, 3.00, by = 0.50),
    limits = c(0.00, 3.00),
    labels = scales::label_number(accuracy = 0.01)
  ) + 
  scale_color_manual(values = c("Anxiety" = "#fc4a1a", "Depression" = "#f7b733", "Menstrual Problems" = "#008080"))

#-------------------------------------------------------------------------------
# adjusted model 3
anx_reg_adj_3 <- glm(
  anx ~ sum_attitude + age + parity + mar_status +
    relig + soc_class + edu_level + home_own_status + anx_score + dep_score + mum_hlth + dr_change
  + soc_supp_mis + weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

adj_anx_table_3 <- tbl_regression(anx_reg_adj_3,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score", 
                                    mum_hlth ~ "Evaluation of own health",
                                    dr_change ~ "Recent doctor change",
                                    soc_supp_mis ~ "Social support score",
                                    weigh_life ~ "Weighted life events score"
                                  )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for the Likelihood of going to the Doctor for Anxiety**")


dep_reg_adj_3 <- glm(
  dep ~ sum_attitude + age + parity + mar_status +
    relig + soc_class + edu_level + home_own_status + anx_score + dep_score + mum_hlth + dr_change
  + soc_supp_mis + weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

adj_dep_table_3 <- tbl_regression(dep_reg_adj_3,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score", 
                                    mum_hlth ~ "Evaluation of own health",
                                    dr_change ~ "Recent doctor change",
                                    soc_supp_mis ~ "Social support score",
                                    weigh_life ~ "Weighted life events score"
                                  )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for the Likelihood of going to the Doctor for Depression**")


per_reg_adj_3 <- glm(
  per ~ sum_attitude + age + parity + mar_status +
    relig + soc_class + edu_level + home_own_status + anx_score + dep_score+ mum_hlth + dr_change
  + soc_supp_mis + weigh_life,
  data = df_working,
  family = binomial(link = "logit")
)

adj_per_table_3 <- tbl_regression(per_reg_adj_3,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score", 
                                    mum_hlth ~ "Evaluation of own health",
                                    dr_change ~ "Recent doctor change",
                                    soc_supp_mis ~ "Social support score",
                                    weigh_life ~ "Weighted life events score"
                                  )) %>%
  modify_caption("**Model 3: Adjusted Odds Ratios for the Likelihood of going to the Doctor for Problems with Periods**")


n_anx_3 <- nobs(anx_reg_adj_3)
n_dep_3 <- nobs(dep_reg_adj_3)
n_per_3 <- nobs(per_reg_adj_3)

adj_anx_table_3 <- tbl_regression(anx_reg_adj_3,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    mum_hlth ~ "Evaluation of own health",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score", 
                                    dr_change ~ "Recent doctor change",
                                    soc_supp_mis ~ "Social support score",
                                    weigh_life ~ "Weighted life events score"
                                  )) %>%
  modify_caption(glue::glue("**Model 3: Adjusted Odds Ratios for Anxiety (N = {n_anx_3})**"))

adj_dep_table_3 <- tbl_regression(dep_reg_adj_3,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score", 
                                    mum_hlth ~ "Evaluation of own health",
                                    dr_change ~ "Recent doctor change",
                                    soc_supp_mis ~ "Social support score",
                                    weigh_life ~ "Weighted life events score"
                                  )) %>%
  modify_caption(glue::glue("**Model 3: Adjusted Odds Ratios for Depression (N = {n_dep_3})**"))

adj_per_table_3 <- tbl_regression(per_reg_adj_3,
                                  exponentiate = TRUE,
                                  label = list(
                                    sum_attitude ~ "Attitudes towards doctors",
                                    age ~ "Age",
                                    parity ~ "Parity",
                                    mar_status ~ "Marital status",
                                    relig ~ "Religion",
                                    soc_class ~ "Socioeconomic position",
                                    edu_level ~ "Highest educational attainment",
                                    home_own_status ~ "Home ownership status",
                                    anx_score ~ "Crown crisp anxiety score",
                                    dep_score ~ "Crown crisp depression score", 
                                    mum_hlth ~ "Evaluation of own health",
                                    dr_change ~ "Recent doctor change",
                                    soc_supp_mis ~ "Social support score",
                                    weigh_life ~ "Weighted life events score"
                                  )) %>%
  modify_caption(glue::glue("**Model 3: Adjusted Odds Ratios for Problems with Periods (N = {n_per_3})**"))

adj_table_3_all <- tbl_merge(
  tbls = list(adj_anx_table_3, adj_dep_table_3, adj_per_table_3),
  tab_spanner = c(glue::glue("**Anxiety (N = {n_anx_3})**"), 
                  glue::glue("**Depression (N = {n_dep_3})**"), 
                  glue::glue("**Problems with Periods (N = {n_per_3})**"))
) %>%
  modify_caption(glue::glue("**Model 3: Adjusted Odds Ratios for Anxiety, Depression, and Problems with Periods (N = {n_anx_3}, {n_dep_3}, {n_per_3})**"))


adj_table_3_all

term_labels_3 <- c(
  "sum_attitude" = "Attitudes towards doctors",
  "soc_class" = "Socioeconomic position",
  "relig" = "Religion",
  "parity" = "Parity",
  "mar_status" = "Marital status",
  "home_own_status" = "Home ownership status",
  "edu_level" = "Highest educational attainment",
  "age" = "Age",
  "anx_score" = "Crown crisp anxiety score",
  "dep_score" = "Crown crisp depression score", 
  "dr_change" = "Recent Doctor change",
  "mum_hlth" = "Evaluation of own health",
  "soc_supp_mis" = "Social support score",
  "weigh_life" = "Weighted life events score"
)

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
  "Evaluation of own health",
  "Recent Doctor change",
  "Social support score",
  "Weighted life events score"
)


anx_regression_results_3 <- adj_anx_table_3$table_body
dep_regression_results_3 <- adj_dep_table_3$table_body
per_regression_results_3 <- adj_per_table_3$table_body


anx_regression_results_3$model <- "Anxiety"
dep_regression_results_3$model <- "Depression"
per_regression_results_3$model <- "Menstrual Problems"


combined_regression_results_3 <- bind_rows(anx_regression_results_3, dep_regression_results_3, per_regression_results_3)

combined_regression_results_3 <- combined_regression_results_3 %>%
  filter(term != "(Intercept)") %>%
  mutate(
    model = factor(model, levels = c("Anxiety", "Depression", "Menstrual Problems")),
    term_label = term_labels_3[term],
    term_label = factor(term_label, levels = rev(custom_order_3))
    
  )

# adjusted model 3 forest plot
ggplot(combined_regression_results_3, aes(x = estimate, y = term_label, xmin = conf.low, xmax = conf.high, color = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(position = position_dodge(width = 0.5), height = 0.3) +
  labs(
    title = "Graph 4: Forest Plot of Adjusted Odds Ratios",
    x = "Odds Ratio (95% CI)",
    y = "",
    subtitle = "Based on Regression Model 3",
    color = "Condition"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
  scale_x_continuous(
    breaks = seq(0.00, 3.00, by = 0.50),
    limits = c(0.00, 3.00),
    labels = scales::label_number(accuracy = 0.01)
  ) + 
  scale_color_manual(values = c("Anxiety" = "#fc4a1a", "Depression" = "#f7b733", "Menstrual Problems" = "#008080"))



#-------------------------------------------------------------------------------
# Combine all tables into one
all_anx_tables <- tbl_merge(
  tbls = list(anx_table, adj_anx_table_1, adj_anx_table_2, adj_anx_table_3),
  tab_spanner = c(glue::glue("**Unadjusted Model (N = {n_anx})**"), 
                  glue::glue("**Adjusted Model 1 (N = {n_anx_1})**"), 
                  glue::glue("**Adjusted Model 2 (N = {n_anx_2})**"), 
                  glue::glue("**Adjusted Model 3 (N = {n_anx_3})**")
  )) %>%
  modify_caption("**Odds Ratios for Anxiety across Models**")


all_anx_tables



all_dep_tables <- tbl_merge(
  tbls = list(dep_table, adj_dep_table_1, adj_dep_table_2, adj_dep_table_3),
  tab_spanner = c(glue::glue("**Unadjusted Model (N = {n_dep})**"), 
                  glue::glue("**Adjusted Model 1 (N = {n_dep_1})**"), 
                  glue::glue("**Adjusted Model 2 (N = {n_dep_2})**"), 
                  glue::glue("**Adjusted Model 3 (N = {n_dep_3})**")
  )) %>%
  modify_caption("**Odds Ratios for Depression across Models**")


all_dep_tables



all_per_tables <- tbl_merge(
  tbls = list(per_table, adj_per_table_1, adj_per_table_2, adj_per_table_3),
  tab_spanner = c(glue::glue("**Unadjusted Model (N = {n_per})**"), 
                  glue::glue("**Adjusted Model 1 (N = {n_per_1})**"), 
                  glue::glue("**Adjusted Model 2 (N = {n_per_2})**"), 
                  glue::glue("**Adjusted Model 3 (N = {n_per_3})**")
  )) %>%
  modify_caption("**Odds Ratios for Menstrual Problems across different Models**")


all_per_tables


## Anxiety

# Regression tables with only sum_attitude
adj_anx_table_3_sum <- tbl_regression(anx_reg_adj_3, exponentiate = TRUE, include = sum_attitude) 
adj_anx_table_2_sum <- tbl_regression(anx_reg_adj_2, exponentiate = TRUE, include = sum_attitude) 
adj_anx_table_1_sum <- tbl_regression(anx_reg_adj_1, exponentiate = TRUE, include = sum_attitude) 
anx_table_sum <- tbl_regression(anx_reg_unad_tab, exponentiate = TRUE, include = sum_attitude) 

final_table_anx <- tbl_merge(
  tbls = list(anx_table_sum, adj_anx_table_1_sum, adj_anx_table_2_sum, adj_anx_table_3_sum),
  tab_spanner = c(glue::glue("**Unadjusted Model (N = {n_anx})**"), 
                  glue::glue("**Adjusted Model 1 (N = {n_anx_1})**"), 
                  glue::glue("**Adjusted Model 2 (N = {n_anx_2})**"), 
                  glue::glue("**Adjusted Model 3 (N = {n_anx_3})**")
  )
) %>%
  modify_caption("**Odds Ratios Across Models for Anxiety**") %>%
  modify_source_note("Model 1: adjusted for age, parity, marital status, religion, Socioeconomic position, education, home ownership status, crown crisp anxiety score and crown crisp depression score; Model 2: additionally adjusted for participants' evaluation of own health and recent primary care physician changes; Model 3: additionally adjusted for social support score and weighted life events score.")

final_table_anx <- final_table_anx %>%
  modify_table_body(~ .x %>% select(-var_label))


final_table_anx

### Forest plot
df_3_anx <- adj_anx_table_3_sum$table_body %>% mutate(model = "Model 3")
df_2_anx <- adj_anx_table_2_sum$table_body %>% mutate(model = "Model 2")
df_1_anx <- adj_anx_table_1_sum$table_body %>% mutate(model = "Model 1")
df_0_anx <- anx_table_sum$table_body        %>% mutate(model = "Unadjusted")

combined_df_anx <- bind_rows(df_3_anx, df_2_anx, df_1_anx, df_0_anx)


df_long_anx <- combined_df_anx %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_anx <- df_long_anx %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))


ggplot(df_long_anx, aes(x = estimate, y = variable_label, color = model)) +
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
    breaks = seq(1.00, 1.16, by = 0.02),
    limits = c(1.00, 1.16),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Main predictor",
    color = "Model"
  ) +
  theme_minimal()

## Depression 
### Combined Models

# Regression tables but only keep "sum_attitude"
adj_dep_table_3_sum <- tbl_regression(dep_reg_adj_3, exponentiate = TRUE, include = sum_attitude) 
adj_dep_table_2_sum <- tbl_regression(dep_reg_adj_2, exponentiate = TRUE, include = sum_attitude) 
adj_dep_table_1_sum <- tbl_regression(dep_reg_adj_1, exponentiate = TRUE, include = sum_attitude) 
dep_table_sum <- tbl_regression(dep_reg_unad_tab, exponentiate = TRUE, include = sum_attitude) 

final_table_dep <- tbl_merge(
  tbls = list(dep_table_sum, adj_dep_table_1_sum, adj_dep_table_2_sum, adj_dep_table_3_sum),
  tab_spanner = c(glue::glue("**Unadjusted Model (N = {n_dep})**"), 
                  glue::glue("**Adjusted Model 1 (N = {n_dep_1})**"), 
                  glue::glue("**Adjusted Model 2 (N = {n_dep_2})**"), 
                  glue::glue("**Adjusted Model 3 (N = {n_dep_3})**")
  )
) %>%
  modify_caption("**Odds Ratios Across Models for Depression**") %>%
  modify_source_note("Model 1: adjusted for age, parity, marital status, religion, socioeconomic position, education, home ownership status, crown crisp anxiety score and crown crisp depression score; Model 2: additionally adjusted for participants' evaluation of own health and recent primary care physician changes; Model 3: additionally adjusted for social support score and weighted life events score.")

final_table_dep

### Forest plot
df_3_dep <- adj_dep_table_3_sum$table_body %>% mutate(model = "Model 3" )
df_2_dep <- adj_dep_table_2_sum$table_body %>% mutate(model = "Model 2")
df_1_dep <- adj_dep_table_1_sum$table_body %>% mutate(model = "Model 1")
df_0_dep <- dep_table_sum$table_body        %>% mutate(model = "Unadjusted")

combined_df_dep <- bind_rows(df_3_dep, df_2_dep, df_1_dep, df_0_dep)


df_long_dep <- combined_df_dep %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_dep <- df_long_dep %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))


ggplot(df_long_dep, aes(x = estimate, y = variable_label, color = model)) +
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
    breaks = seq(1.00, 1.16, by = 0.02),
    limits = c(1.00, 1.16),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Main predictor",
    color = "Model"
  ) +
  theme_minimal()

## Menstrual Problems
### Combined Model

# Regression tables but only keep "sum_attitude"
adj_per_table_3_sum <- tbl_regression(per_reg_adj_3, exponentiate = TRUE, include = sum_attitude) 
adj_per_table_2_sum <- tbl_regression(per_reg_adj_2, exponentiate = TRUE, include = sum_attitude) 
adj_per_table_1_sum <- tbl_regression(per_reg_adj_1, exponentiate = TRUE, include = sum_attitude) 
per_table_sum <- tbl_regression(per_reg_unad_tab, exponentiate = TRUE, include = sum_attitude) 

final_table_per <- tbl_merge(
  tbls = list(per_table_sum, adj_per_table_1_sum, adj_per_table_2_sum, adj_per_table_3_sum),
  tab_spanner = c(glue::glue("**Unadjusted Model (N = {n_per})**"), 
                  glue::glue("**Adjusted Model 1 (N = {n_per_1})**"), 
                  glue::glue("**Adjusted Model 2 (N = {n_per_2})**"), 
                  glue::glue("**Adjusted Model 3 (N = {n_per_3})**")
  )
) %>%
  modify_caption("**Odds Ratios Across Models for Problems with Periods**") %>%
  modify_source_note("Model 1: adjusted for age, parity, marital status, religion, socioeconomic position, education, home ownership status, crown crisp anxiety score and crown crisp depression score; Model 2: additionally adjusted for participants' evaluation of own health and recent primary care physician changes; Model 3: additionally adjusted for social support score and weighted life events score.")

final_table_per

### Forest plot
df_3_per <- adj_per_table_3_sum$table_body %>% mutate(model = "Model 3")
df_2_per <- adj_per_table_2_sum$table_body %>% mutate(model = "Model 2")
df_1_per <- adj_per_table_1_sum$table_body %>% mutate(model = "Model 1")
df_0_per <- per_table_sum$table_body        %>% mutate(model = "Unadjusted")

combined_df_per <- bind_rows(df_3_per, df_2_per, df_1_per, df_0_per)


df_long_per <- combined_df_per %>%
  pivot_longer(
    cols = c(estimate, conf.low, conf.high),
    names_to = "stat",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value)

df_long_per <- df_long_per %>%
  mutate(variable_label = ifelse(is.na(label), variable, label)) %>%
  mutate(variable_label = factor(variable_label, levels = rev(unique(variable_label))))

ggplot(df_long_per, aes(x = estimate, y = variable_label, color = model)) +
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
    breaks = seq(0.98, 1.16, by = 0.02),
    limits = c(0.98, 1.16),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Main predictor",
    color = "Model"
  ) +
  theme_minimal()


## Anxiety ----
anx_table_sum_tbl <- tbl_regression(anx_reg_unad_tab, exponentiate = TRUE, include = sum_attitude, label = list(sum_attitude ~ "Attitudes towards doctors"))
adj_anx_table_1_sum_tbl <- tbl_regression(anx_reg_adj_1, exponentiate = TRUE, include = sum_attitude, label = list(sum_attitude ~ "Attitudes towards doctors"))
adj_anx_table_2_sum_tbl <- tbl_regression(anx_reg_adj_2, exponentiate = TRUE, include = sum_attitude, label = list(sum_attitude ~ "Attitudes towards doctors"))
adj_anx_table_3_sum_tbl <- tbl_regression(anx_reg_adj_3, exponentiate = TRUE, include = sum_attitude, label = list(sum_attitude ~ "Attitudes towards doctors"))

anx_table_sum_tbl <- anx_table_sum_tbl %>%
  modify_table_body(~ .x %>% mutate(N = n_anx, Condition = "Anxiety"))
adj_anx_table_1_sum_tbl <- adj_anx_table_1_sum_tbl %>%
  modify_table_body(~ .x %>% mutate(N = n_anx_1, Condition = "Anxiety"))
adj_anx_table_2_sum_tbl <- adj_anx_table_2_sum_tbl %>%
  modify_table_body(~ .x %>% mutate(N = n_anx_2, Condition = "Anxiety"))
adj_anx_table_3_sum_tbl <- adj_anx_table_3_sum_tbl %>%
  modify_table_body(~ .x %>% mutate(N = n_anx_3, Condition = "Anxiety"))

final_table_anx_comb <- tbl_merge(
  tbls = list(anx_table_sum_tbl, adj_anx_table_1_sum_tbl, adj_anx_table_2_sum_tbl, adj_anx_table_3_sum_tbl),
  tab_spanner = c(glue::glue("**Unadjusted Model**"), 
                  glue::glue("**Adjusted Model 1**"), 
                  glue::glue("**Adjusted Model 2**"), 
                  glue::glue("**Adjusted Model 3**")
  ),
) 

## Depression ----
dep_table_sum_tbl <- tbl_regression(dep_reg_unad_tab, exponentiate = TRUE, include = sum_attitude, label = list(sum_attitude ~ "Attitudes towards doctors"))
adj_dep_table_1_sum_tbl <- tbl_regression(dep_reg_adj_1, exponentiate = TRUE, include = sum_attitude, label = list(sum_attitude ~ "Attitudes towards doctors"))
adj_dep_table_2_sum_tbl <- tbl_regression(dep_reg_adj_2, exponentiate = TRUE, include = sum_attitude, label = list(sum_attitude ~ "Attitudes towards doctors"))
adj_dep_table_3_sum_tbl <- tbl_regression(dep_reg_adj_3, exponentiate = TRUE, include = sum_attitude, label = list(sum_attitude ~ "Attitudes towards doctors"))

dep_table_sum_tbl <- dep_table_sum_tbl %>%
  modify_table_body(~ .x %>% mutate(N = n_dep, Condition = "Depression"))
adj_dep_table_1_sum_tbl <- adj_dep_table_1_sum_tbl %>%
  modify_table_body(~ .x %>% mutate(N = n_dep_1, Condition = "Depression"))
adj_dep_table_2_sum_tbl <- adj_dep_table_2_sum_tbl %>%
  modify_table_body(~ .x %>% mutate(N = n_dep_2, Condition = "Depression"))
adj_dep_table_3_sum_tbl <- adj_dep_table_3_sum_tbl %>%
  modify_table_body(~ .x %>% mutate(N = n_dep_3, Condition = "Depression"))

final_table_dep_comb <- tbl_merge(
  tbls = list(dep_table_sum_tbl, adj_dep_table_1_sum_tbl, adj_dep_table_2_sum_tbl, adj_dep_table_3_sum_tbl),
  tab_spanner = c(glue::glue("**Unadjusted Model**"), 
                  glue::glue("**Adjusted Model 1**"), 
                  glue::glue("**Adjusted Model 2**"), 
                  glue::glue("**Adjusted Model 3**")
  ),
) 

## Menstrual Problems ----
per_table_sum_tbl <- tbl_regression(per_reg_unad_tab, exponentiate = TRUE, include = sum_attitude, label = list(sum_attitude ~ "Attitudes towards doctors"))
adj_per_table_1_sum_tbl <- tbl_regression(per_reg_adj_1, exponentiate = TRUE, include = sum_attitude, label = list(sum_attitude ~ "Attitudes towards doctors"))
adj_per_table_2_sum_tbl <- tbl_regression(per_reg_adj_2, exponentiate = TRUE, include = sum_attitude, label = list(sum_attitude ~ "Attitudes towards doctors"))
adj_per_table_3_sum_tbl <- tbl_regression(per_reg_adj_3, exponentiate = TRUE, include = sum_attitude, label = list(sum_attitude ~ "Attitudes towards doctors"))

per_table_sum_tbl <- per_table_sum_tbl %>%
  modify_table_body(~ .x %>% mutate(N = n_per, Condition = "Menstrual problems"))
adj_per_table_1_sum_tbl <- adj_per_table_1_sum_tbl %>%
  modify_table_body(~ .x %>% mutate(N = n_per_1, Condition = "Menstrual problems"))
adj_per_table_2_sum_tbl <- adj_per_table_2_sum_tbl %>%
  modify_table_body(~ .x %>% mutate(N = n_per_2, Condition = "Menstrual problems"))
adj_per_table_3_sum_tbl <- adj_per_table_3_sum_tbl %>%
  modify_table_body(~ .x %>% mutate(N = n_per_3, Condition = "Menstrual problems"))

## Master table ----
final_table_per_comb <- tbl_merge(
  tbls = list(per_table_sum_tbl, adj_per_table_1_sum_tbl, adj_per_table_2_sum_tbl, adj_per_table_3_sum_tbl),
  tab_spanner = c(glue::glue("**Unadjusted Model**"), 
                  glue::glue("**Adjusted Model 1**"), 
                  glue::glue("**Adjusted Model 2**"), 
                  glue::glue("**Adjusted Model 3**")
  ),
)

final_table <- tbl_stack(
  tbls = list(
    final_table_anx_comb,
    final_table_dep_comb,
    final_table_per_comb
  )
)

group_header_labels <- c(
  glue::glue("Anxiety")[1],
  glue::glue("Depression")[1],
  glue::glue("Menstrual problems")[1]
)

final_table <- tbl_stack(
  tbls = list(
    final_table_anx_comb,
    final_table_dep_comb,
    final_table_per_comb
  ),
  group_header = group_header_labels
)

final_table

saveRDS(
  final_table,
  (here::here( "21-month-timepoint/complete-case-dataset/4_Analysis_and_Visualisation", "21m_master_reg_table.rds"))
)


# Forest plots ----

df_long_anx$N <- c(1265, 1267, 1272, 1827) 
df_long_dep$N <- c(1592, 1598, 1610, 2380)
df_long_per$N <- c(1110, 1115, 1127, 1712)

# Add outcome/measure column
df_long_anx <- df_long_anx %>% mutate(Outcome = "Anxiety")
df_long_dep <- df_long_dep %>% mutate(Outcome = "Depression")
df_long_per <- df_long_per %>% mutate(Outcome = "Menstrual problems")


# Combine datasets
combined_long <- bind_rows(df_long_anx, df_long_dep, df_long_per)

# Create a combined label to stack by outcome and predictor
combined_long <- combined_long %>%
  mutate(variable_label_full = paste(Outcome)) %>%
  mutate(variable_label_full = factor(variable_label_full, levels = rev(unique(variable_label_full))))

# Plot
reg_forest_plot <- ggplot(combined_long, aes(x = estimate, y = variable_label_full, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(
    aes(label = paste0("N=", N), x = conf.high + 0.05),
    position = position_dodge(width = 0.7),
    hjust = -0.3,
    size = 3
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
    breaks = seq(0.98, 1.40, by = 0.02),
    limits = c(0.98, 1.40),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    title = "Anxiety, Depression, Menstrual Problems - 21-month timepoint",
    x = "Odds Ratio (95% Confidence Interval)",
    y = "Attitudes towards doctors",
    color = "Model"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

ggsave(
  filename = (here::here("21-month-timepoint/complete-case-dataset/4_Analysis_and_Visualisation", "21m_reg_forest_plot.png")),
  plot = reg_forest_plot,
  width = 10,
  height = 6,
  dpi = 300
)

