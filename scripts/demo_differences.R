library(here)
library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(quantreg)
library(stargazer)

source("utils.R")

data_path <- here("../data", "ind_data.csv")

data <- read.csv(data_path) %>%
  mutate(
    women = ifelse(gender_lab == "Female", 1, 0),
    age = 2022 - birthyr,
    mal_visitor = ifelse(malicious_visits > 0, 1, 0),  # Binary variable: 1 if malicious_duration = 0, 0 otherwise
    private_hrs = ifelse(tod_mean >= 19 | tod_mean < 4, 1, 0),  # Private hours: 7 PM to 4 AM
    office_hrs = ifelse(tod_mean >= 7 & tod_mean < 19, 1, 0),
    private_hrs_mal = ifelse(tod_mean_mal >= 19 | tod_mean_mal < 4, 1, 0),
    office_hrs_mal = ifelse(tod_mean_mal >= 7 & tod_mean_mal < 19, 1, 0),
    private_hrs_nonmal = ifelse(tod_mean_nonmal >= 19 | tod_mean_nonmal < 4, 1, 0),
    office_hrs_nonmal = ifelse(tod_mean_nonmal >= 7 & tod_mean_nonmal < 19, 1, 0)
  ) %>%
  # rescale
  mutate(
    visits = visits / 100,  # Scale visits
    # to 0 to 1 to 0 to 100
    gini = 100 * gini, 
    gini_mal = 100 * gini_mal,
    gini_nonmal = 100 * gini_nonmal, 
    perc_mal = 100 * n_uniques_mal / n_uniques
  ) %>%
  # fillna to 0 for mal.
  mutate(
    singleton_mal = replace_na(singleton_mal, 0),
    gini_mal = replace_na(gini_mal, 0),
    private_hrs_mal = replace_na(private_hrs_mal, 0),
    office_hrs_mal = replace_na(office_hrs_mal, 0),
    nonoffice_hrs_mal = 1 - office_hrs_mal,
    duration_mean_mal = replace_na(duration_mean_mal, 0)
  ) %>%
  # Mean-center continuous variables and create new columns with suffix "_mc"
  mutate(
    across(
      where(~ is.numeric(.) && !all(. %in% c(0, 1))),  # Select numeric variables that are not binary
      ~ . - mean(., na.rm = TRUE),                    # Mean-center
      .names = "{.col}_mc"                      # Create new columns with prefix "centered_"
    )
  ) %>%
  # Scale to 0-1
  mutate(
    age_scaled = scales::rescale(age, to = c(0, 1)),
    visits_scaled = scales::rescale(visits, to = c(0, 1))
  )


# Median regs -------------------------------------------------------------
set.seed(0)
# https://stackoverflow.com/questions/28393176/error-in-summary-quantreg-backsolve

run_median_reg <- function(formula, data, tau = 0.5, R = 1000) {
  model <- rq(formula, tau = tau, data = data)
  summary(model, se = "boot", R = R)$coef
}

# outcome --- num. mal sites
model_list <- list(
  Model1  = run_median_reg(n_uniques_mal ~ I(women), data),
  Model2  = run_median_reg(n_uniques_mal ~ I(women) + visits_scaled + I(visits_scaled^2), data),
  Model3  = run_median_reg(n_uniques_mal ~ i(race_lab, ref = "White"), data),
  Model4  = run_median_reg(n_uniques_mal ~ i(race_lab, ref = "White") + visits_scaled + I(visits_scaled^2), data),
  Model5  = run_median_reg(n_uniques_mal ~ i(educ_lab, ref = "HS or Below"), data),
  Model6  = run_median_reg(n_uniques_mal ~ i(educ_lab, ref = "HS or Below") + visits_scaled + I(visits_scaled^2), data),
  Model7  = run_median_reg(n_uniques_mal ~ i(agegroup_lab, ref = "<25"), data),
  Model8  = run_median_reg(n_uniques_mal ~ i(agegroup_lab, ref = "<25") + visits_scaled + I(visits_scaled^2), data),
  Model9  = run_median_reg(n_uniques_mal ~ I(women) + i(race_lab, ref = "White") + i(educ_lab, ref = "HS or Below") + i(agegroup_lab, ref = "<25"), data),
  Model10 = run_median_reg(n_uniques_mal ~ I(women) + i(race_lab, ref = "White") + i(educ_lab, ref = "HS or Below") + i(agegroup_lab, ref = "<25") + visits_scaled + I(visits_scaled^2), data)
)
latex_output <- generate_latex_table(model_list, COEF_LABELS2)
writeLines(latex_output, "../tabs/demo_differences_median_reg.tex")
print_tex("../tabs/demo_differences_median_reg.tex")

# outcome --- perc. mal sites
model_list_perc <- list(
  Model1  = run_median_reg(perc_mal ~ I(women), data),
  Model2  = run_median_reg(perc_mal ~ I(women) + visits_scaled + I(visits_scaled^2), data),
  Model3  = run_median_reg(perc_mal ~ i(race_lab, ref = "White"), data),
  Model4  = run_median_reg(perc_mal ~ i(race_lab, ref = "White") + visits_scaled + I(visits_scaled^2), data),
  Model5  = run_median_reg(perc_mal ~ i(educ_lab, ref = "HS or Below"), data),
  Model6  = run_median_reg(perc_mal ~ i(educ_lab, ref = "HS or Below") + visits_scaled + I(visits_scaled^2), data),
  Model7  = run_median_reg(perc_mal ~ i(agegroup_lab, ref = "<25"), data),
  Model8  = run_median_reg(perc_mal ~ i(agegroup_lab, ref = "<25") + visits_scaled + I(visits_scaled^2), data),
  Model9  = run_median_reg(perc_mal ~ I(women) + i(race_lab, ref = "White") + i(educ_lab, ref = "HS or Below") + i(agegroup_lab, ref = "<25"), data),
  Model10 = run_median_reg(perc_mal ~ I(women) + i(race_lab, ref = "White") + i(educ_lab, ref = "HS or Below") + i(agegroup_lab, ref = "<25") + visits_scaled + I(visits_scaled^2), data)
)

latex_output_perc <- generate_latex_table(model_list_perc, COEF_LABELS2)
writeLines(latex_output_perc, "../tabs/demo_differences_perc_malsites_median_reg.tex")
print_tex("../tabs/demo_differences_perc_malsites_median_reg.tex")


# OLS ---------------------------------------------------------------------
run_ols_reg <- function(formula, data, vcov_type = "hetero") {
  feols(formula, data = data, vcov = vcov_type)
}

# outcome --- num. mal sites
model_list_ols <- list(
  Model1  = run_ols_reg(n_uniques_mal ~ I(women), data),
  Model2  = run_ols_reg(n_uniques_mal ~ I(women) + visits_scaled + I(visits_scaled^2), data),
  Model3  = run_ols_reg(n_uniques_mal ~ i(race_lab, ref = "White"), data),
  Model4  = run_ols_reg(n_uniques_mal ~ i(race_lab, ref = "White") + visits_scaled + I(visits_scaled^2), data),
  Model5  = run_ols_reg(n_uniques_mal ~ i(educ_lab, ref = "HS or Below"), data),
  Model6  = run_ols_reg(n_uniques_mal ~ i(educ_lab, ref = "HS or Below") + visits_scaled + I(visits_scaled^2), data),
  Model7  = run_ols_reg(n_uniques_mal ~ i(agegroup_lab, ref = "<25"), data),
  Model8  = run_ols_reg(n_uniques_mal ~ i(agegroup_lab, ref = "<25") + visits_scaled + I(visits_scaled^2), data),
  Model9  = run_ols_reg(n_uniques_mal ~ I(women) + i(race_lab, ref = "White") + i(educ_lab, ref = "HS or Below") + i(agegroup_lab, ref = "<25"), data),
  Model10 = run_ols_reg(n_uniques_mal ~ I(women) + i(race_lab, ref = "White") + i(educ_lab, ref = "HS or Below") + i(agegroup_lab, ref = "<25") + visits_scaled + I(visits_scaled^2), data)
)

etable(
  model_list_ols,
  digits = 3,
  digits.stats = 3,
  dict = COEF_LABELS,
  order = COEF_ORDER,
  signif.code = "letters",
  fitstat = c("r2", "n"),
  se.row = FALSE,
  tex = TRUE,
  file = "../tabs/demo_differences_ols.tex",
  replace=TRUE,
  style.tex = style.tex("aer")
)
print_tex("../tabs/demo_differences_ols.tex")

# outcome --- 1(have encountered mal. sites) --- extensive margin
model_list_lpm_ext <- list(
  Model1  = run_ols_reg(mal_visitor ~ I(women), data),
  Model2  = run_ols_reg(mal_visitor ~ I(women) + visits_scaled + I(visits_scaled^2), data),
  Model3  = run_ols_reg(mal_visitor ~ i(race_lab, ref = "White"), data),
  Model4  = run_ols_reg(mal_visitor ~ i(race_lab, ref = "White") + visits_scaled + I(visits_scaled^2), data),
  Model5  = run_ols_reg(mal_visitor ~ i(educ_lab, ref = "HS or Below"), data),
  Model6  = run_ols_reg(mal_visitor ~ i(educ_lab, ref = "HS or Below") + visits_scaled + I(visits_scaled^2), data),
  Model7  = run_ols_reg(mal_visitor ~ i(agegroup_lab, ref = "<25"), data),
  Model8  = run_ols_reg(mal_visitor ~ i(agegroup_lab, ref = "<25") + visits_scaled + I(visits_scaled^2), data),
  Model9  = run_ols_reg(mal_visitor ~ I(women) + i(race_lab, ref = "White") + i(educ_lab, ref = "HS or Below") + i(agegroup_lab, ref = "<25"), data),
  Model10 = run_ols_reg(mal_visitor ~ I(women) + i(race_lab, ref = "White") + i(educ_lab, ref = "HS or Below") + i(agegroup_lab, ref = "<25") + visits_scaled + I(visits_scaled^2), data)
)

etable(
  model_list_lpm_ext,
  digits = 3,
  digits.stats = 3,
  dict = COEF_LABELS,
  order = COEF_ORDER,
  signif.code = "letters",
  fitstat = c("r2", "n"),
  se.row = FALSE,
  tex = TRUE,
  file="../tabs/demo_differences_prob_mal_extensive_margin_lpm.tex",
  replace=TRUE,
  style.tex = style.tex("aer")
)
print_tex("../tabs/demo_differences_prob_mal_extensive_margin_lpm.tex")
