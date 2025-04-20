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

# coef labels ---------------------------------------------------------------
COEF_LABELS = c(
  visits = "Total domain visits",
  visits_scaled = "Total visits (scaled)",
  "I(visits_scaled^2)" = "Total visits$^2$ (scaled)",
  "I(women)" = "Woman",
  "race_lab::Black" = "Race: African American",
  "race_lab::White" = "Race: White",
  "race_lab::Asian" = "Race: Asian",
  "race_lab::Hispanic" = "Race: Hispanic",
  "race_lab::Other" = "Race: Other",
  "educ_lab::College" = "Educ: College",
  "educ_lab::Postgrad" = "Educ: Postgraduate",
  "educ_lab::Somecollege" = "Educ: Some college",
  "agegroup_lab::<25" = "Age: 18--25",
  "agegroup_lab::25-34" = "Age: 25--34",
  "agegroup_lab::35-49" = "Age: 35--49",
  "agegroup_lab::50-64" = "Age: 50--64",
  "agegroup_lab::65+" = "Age: 65+",
  age_mc = "Age",
  age_scaled="Age (scaled)",
  "I(age_scaled^2)" = "Age$^2$ (scaled)",
  age = "Age",
  "I(age_mc^2)" = "Age$^2$ (scaled)",
  duration_mean="Mean dwelling time",
  singleton="Proportion of singleton visits",
  gini="Concentration of domain visits",
  "I(private_hrs)"="Mean time of day: Private Hours"
)

COEF_ORDER = c(
  "Woman",
  "Race: African American",
  "Race: Asian",
  "Race: Hispanic",
  "Race: Other",
  "Educ: Some college",
  "Educ: College",
  "Educ: Postgraduate",
  "Age",
  "Mean dwelling time",
  "Proportion of singleton visits",
  "Concentration of domain visits",
  "Mean time of day: Private Hours",
  "Total visits (scaled)",
  "Constant"
)

# Intensive margin (means) --------------------------------------------------
m_gender_int = feols(
  n_uniques_mal ~ I(women),
  vcov = "hetero",
  data = data
)

m_gender_visits_int = feols(
  n_uniques_mal ~ visits_scaled + I(visits_scaled^2) + I(women),
  vcov = "hetero",
  data = data
)

m_race_int = feols(
  n_uniques_mal ~ i(race_lab, ref = "White"),
  vcov = "hetero",
  data = data
)

m_race_visits_int = feols(
  n_uniques_mal ~ visits_scaled + I(visits_scaled^2) + i(race_lab, ref = "White"),
  vcov = "hetero",
  data = data
)

m_educ_int = feols(
  n_uniques_mal ~ i(educ_lab, ref = "HS or Below"),
  vcov = "hetero",
  data = data
)

m_educ_visits_int = feols(
  n_uniques_mal ~ visits_scaled + I(visits_scaled^2) + i(educ_lab, ref = "HS or Below"),
  vcov = "hetero",
  data = data
)

m_age_int = feols(
  n_uniques_mal ~ age_scaled + I(age_scaled^2),
  vcov = "hetero",
  data = data
)
m_age_visits_int = feols(
  n_uniques_mal ~ visits_scaled + I(visits_scaled^2) + age_scaled + I(age_scaled^2),
  vcov = "hetero",
  data = data
)

m_agegroup_int = feols(
  n_uniques_mal ~ i(agegroup_lab, ref = "<25"),
  vcov = "hetero",
  data = data
)
m_agegroup_visits_int = feols(
  n_uniques_mal ~ visits_scaled + I(visits_scaled^2) + i(agegroup_lab, ref = "<25"),
  vcov = "hetero",
  data = data
)

m_demo_int = feols(
  n_uniques_mal ~  I(women) +
    i(race_lab, ref = "White") +
    i(educ_lab, ref = "HS or Below") +
    i(agegroup_lab, ref = "<25"),
  # age_scaled + I(age_scaled^2),
  vcov = "hetero",
  data = data
)


m_demo_visits_int = feols(
  n_uniques_mal ~ visits_scaled + I(visits_scaled^2) + I(women) +
    i(race_lab, ref = "White") +
    i(educ_lab, ref = "HS or Below") +
    i(agegroup_lab, ref = "<25"),
    # age_scaled + I(age_scaled^2),
  vcov = "hetero",
  data = data
)
# 
# m_demo_bev_visits_int = feols(
#   n_uniques_mal ~ visits_scaled + I(visits_scaled^2) + I(women) +
#     i(race_lab, ref = "White") +
#     i(educ_lab, ref = "HS or Below") +
#     i(agegroup_lab, ref = "<25") +
#     duration + gini + I(private_hrs) + singleton,    
#   # age_scaled + I(age_scaled^2),
#   vcov = "hetero",
#   data = data
# )

etable(
  m_gender_int,
  m_gender_visits_int,
  m_race_int,
  m_race_visits_int,
  m_educ_int,
  m_educ_visits_int,
  m_agegroup_int,
  m_agegroup_visits_int,
  m_demo_int,
  m_demo_visits_int,
  # m_demo_bev_visits_int,
  digits = 3,
  digits.stats = 3,
  dict = COEF_LABELS,
  order = COEF_ORDER,
  signif.code = "letters",
  fitstat = c("r2", "n"),
  se.row = FALSE,
  tex = TRUE,
  style.tex = style.tex("aer")
)

# Intensive margin (medians) ----------------------------------------------
data$race_lab <- relevel(as.factor(data$race_lab), ref = "White")
data$educ_lab <- relevel(as.factor(data$educ_lab), ref = "HS or Below")
data$agegroup_lab <- relevel(as.factor(data$agegroup_lab), ref = "<25")

set.seed(0)
tau <- 0.5

# https://stackoverflow.com/questions/28393176/error-in-summary-quantreg-backsolve

# Gender models
m_gender_int_qr <- rq(
  n_uniques_mal ~ women,
  tau = tau,
  data = data
)
summary(m_gender_int_qr, se = "boot")

m_gender_visits_int_qr <- rq(
  n_uniques_mal ~ visits_scaled + I(visits_scaled^2) + women,
  tau = tau,
  data = data
)
summary(m_gender_visits_int_qr, se = "boot")

# Race models
m_race_int_qr <- rq(
  n_uniques_mal ~ race_lab,
  tau = tau,
  data = data,
)
summary(m_race_int_qr, se = "boot")

m_race_visits_int_qr <- rq(
  n_uniques_mal ~ visits_scaled + I(visits_scaled^2) + race_lab,
  tau = tau,
  data = data
)
summary(m_race_visits_int_qr, se = "boot")

# Education models
m_educ_int_qr <- rq(
  n_uniques_mal ~ educ_lab,
  tau = tau,
  data = data
)
summary(m_educ_int_qr, se = "boot")

m_educ_visits_int_qr <- rq(
  n_uniques_mal ~ visits_scaled + I(visits_scaled^2) + educ_lab,
  tau = tau,
  data = data
)
summary(m_educ_visits_int_qr, se = "boot")

# Age models
m_age_int_qr <- rq(
  n_uniques_mal ~ age_scaled + I(age_scaled^2),
  tau = tau,
  data = data
)
summary(m_age_int_qr, se = "boot")

m_age_visits_int_qr <- rq(
  n_uniques_mal ~ visits_scaled + I(visits_scaled^2) + age_scaled + I(age_scaled^2),
  tau = tau,
  data = data
)
summary(m_age_visits_int_qr, se = "boot")

# Age group models
m_agegroup_int_qr <- rq(
  n_uniques_mal ~ agegroup_lab,
  tau = tau,
  data = data
)
summary(m_agegroup_int_qr, se = "boot")

m_agegroup_visits_int_qr <- rq(
  n_uniques_mal ~ visits_scaled + I(visits_scaled^2) + agegroup_lab,
  tau = tau,
  data = data
)
summary(m_agegroup_visits_int_qr, se = "boot")

# Demographics model
m_demo_int_qr <- rq(
  n_uniques_mal ~ women +
    race_lab +
    educ_lab +
    agegroup_lab,
  tau = tau,
  data = data
)
summary(m_demo_int_qr, se = "boot")

m_demo_visits_int_qr <- rq(
  n_uniques_mal ~ visits_scaled + I(visits_scaled^2) + women +
    race_lab +
    educ_lab +
    agegroup_lab,
  tau = tau,
  data = data
)
summary(m_demo_visits_int_qr, se = "boot")

# stargazer(
#   m_gender_int_qr,
#   m_gender_visits_int_qr,
#   m_race_visits_int_qr,
#   # m_educ_visits_int_qr,
#   # m_agegroup_visits_int_qr,
#   model.names=FALSE,
#   rq.se="boot",
#   # covariate.labels = COEF_LABELS,
#   type="text"
# )
# stargazer(
#   # m_gender_int_qr,
#   # m_gender_visits_int_qr,
#   # m_race_visits_int_qr,
#   m_educ_visits_int_qr,
#   m_agegroup_visits_int_qr,
#   model.names=FALSE,
#   rq.se="boot",
#   # covariate.labels = COEF_LABELS,
#   type="text"
# )

model_list <- list(
  Model1 = summary(m_gender_int_qr, se = "boot", R=1000)$coef,
  Model2 = summary(m_gender_visits_int_qr, se = "boot", R=1000)$coef,
  Model3 = summary(m_race_int_qr, se = "boot", R=1000)$coef,
  Model4 = summary(m_race_visits_int_qr, se = "boot", R=1000)$coef,
  Model5 = summary(m_educ_int_qr, se = "boot", R=1000)$coef,
  Model6 = summary(m_educ_visits_int_qr, se = "boot", R=1000)$coef,
  Model7 = summary(m_agegroup_int_qr, se = "boot", R=1000)$coef,
  Model8 = summary(m_agegroup_visits_int_qr, se = "boot", R=1000)$coef,
  Model9 = summary(m_demo_int_qr, se = "boot", R=1000)$coef,
  Model10 = summary(m_demo_visits_int_qr, se = "boot", R=1000)$coef
)

COEF_LABELS2 = c(
  visits_scaled = "Total visits (scaled)",
  "I(visits_scaled^2)" = "Total visits$^2$ (scaled)",
  "(Intercept)" = "Constant",
  "women" = "Woman",
  "race_labBlack" = "Race: African American",
  "race_labAsian" = "Race: Asian",
  "race_labHispanic" = "Race: Hispanic",
  "race_labOther" = "Race: Other",
  "educ_labCollege" = "Educ: College",
  "educ_labPostgrad" = "Educ: Postgraduate",
  "educ_labSome college" = "Educ: Some college",
  "agegroup_lab::<25" = "Age: 18--25",
  "agegroup_lab25-34" = "Age: 25--34",
  "agegroup_lab35-49" = "Age: 35--49",
  "agegroup_lab50-64" = "Age: 50--64",
  "agegroup_lab65+" = "Age: 65+"
)

latex_output <- generate_latex_table(model_list, COEF_LABELS2)

cat(latex_output)


# Extensive margin --------------------------------------------------------
m_gender_ext = feols(
  mal_visitor ~ I(women),
  vcov = "hetero",
  data = data
)

m_gender_visits_ext = feols(
  mal_visitor ~ visits_scaled + I(visits_scaled^2) + I(women),
  vcov = "hetero",
  data = data
)

m_race_ext = feols(
  mal_visitor ~ i(race_lab, ref = "White"),
  vcov = "hetero",
  data = data
)

m_race_visits_ext = feols(
  mal_visitor ~ visits_scaled + I(visits_scaled^2) + i(race_lab, ref = "White"),
  vcov = "hetero",
  data = data
)

m_educ_ext = feols(
  mal_visitor ~ i(educ_lab, ref = "HS or Below"),
  vcov = "hetero",
  data = data
)

m_educ_visits_ext = feols(
  mal_visitor ~ visits_scaled + I(visits_scaled^2) + i(educ_lab, ref = "HS or Below"),
  vcov = "hetero",
  data = data
)

m_age_ext = feols(
  mal_visitor ~ age_scaled + I(age_scaled^2),
  vcov = "hetero",
  data = data
)
m_age_visits_ext = feols(
  mal_visitor ~ visits_scaled + I(visits_scaled^2) + age_scaled + I(age_scaled^2),
  vcov = "hetero",
  data = data
)
m_agegroup_ext = feols(
  mal_visitor ~ i(agegroup_lab, ref = "<25"),
  vcov = "hetero",
  data = data
)
m_agegroup_visits_ext = feols(
  mal_visitor ~ visits_scaled + I(visits_scaled^2) + i(agegroup_lab, ref = "<25"),
  vcov = "hetero",
  data = data
)


m_demo_ext = feols(
  mal_visitor ~  I(women) +
    i(race_lab, ref = "White") +
    i(educ_lab, ref = "HS or Below") +
    i(agegroup_lab, ref = "<25"),    
    # age_scaled + I(age_scaled^2),
  vcov = "hetero",
  data = data
)


m_demo_visits_ext = feols(
  mal_visitor ~ visits_scaled + I(visits_scaled^2) + I(women) +
    i(race_lab, ref = "White") +
    i(educ_lab, ref = "HS or Below") +
    i(agegroup_lab, ref = "<25"),    
    # age_scaled + I(age_scaled^2),
  vcov = "hetero",
  data = data
)

# m_demo_bev_visits_ext = feols(
#   mal_visitor ~ visits_scaled + I(visits_scaled^2) + I(women) +
#     i(race_lab, ref = "White") +
#     i(educ_lab, ref = "HS or Below") +
#     i(agegroup_lab, ref = "<25") +
#     duration + gini + I(private_hrs) + singleton,    
#   # age_scaled + I(age_scaled^2),
#   vcov = "hetero",
#   data = data
# )


etable(
  m_gender_ext,
  m_gender_visits_ext,
  m_race_ext,
  m_race_visits_ext,
  m_educ_ext,
  m_educ_visits_ext,
  m_agegroup_ext,
  m_agegroup_visits_ext,
  m_demo_ext,
  m_demo_visits_ext,
  # m_demo_bev_visits_ext,
  digits = 3,
  digits.stats = 3,
  dict = COEF_LABELS,
  order = COEF_ORDER,
  signif.code = "letters",
  fitstat = c("r2", "n"),
  se.row = FALSE,
  tex = TRUE,
  style.tex = style.tex("aer")
)



# Intensive margin (perc mal_domains / domains) -----------------------------
# Gender models
m_gender_int_qr_perc <- rq(
  perc_mal ~ women,
  tau = tau,
  data = data
)
summary(m_gender_int_qr_perc, se = "boot")

m_gender_visits_int_qr_perc <- rq(
  perc_mal ~ visits_scaled + I(visits_scaled^2) + women,
  tau = tau,
  data = data
)
summary(m_gender_visits_int_qr_perc, se = "boot")

# Race models
m_race_int_qr_perc <- rq(
  perc_mal ~ race_lab,
  tau = tau,
  data = data
)
summary(m_race_int_qr_perc, se = "boot")

m_race_visits_int_qr_perc <- rq(
  perc_mal ~ visits_scaled + I(visits_scaled^2) + race_lab,
  tau = tau,
  data = data
)
summary(m_race_visits_int_qr_perc, se = "boot")

# Education models
m_educ_int_qr_perc <- rq(
  perc_mal ~ educ_lab,
  tau = tau,
  data = data
)
summary(m_educ_int_qr_perc, se = "boot")

m_educ_visits_int_qr_perc <- rq(
  perc_mal ~ visits_scaled + I(visits_scaled^2) + educ_lab,
  tau = tau,
  data = data
)
summary(m_educ_visits_int_qr_perc, se = "boot")

# Age group models
m_agegroup_int_qr_perc <- rq(
  perc_mal ~ agegroup_lab,
  tau = tau,
  data = data
)
summary(m_agegroup_int_qr_perc, se = "boot")

m_agegroup_visits_int_qr_perc <- rq(
  perc_mal ~ visits_scaled + I(visits_scaled^2) + agegroup_lab,
  tau = tau,
  data = data
)
summary(m_agegroup_visits_int_qr_perc, se = "boot")

# Demographics model
m_demo_int_qr_perc <- rq(
  perc_mal ~ women +
    race_lab +
    educ_lab +
    agegroup_lab,
  tau = tau,
  data = data
)
summary(m_demo_int_qr_perc, se = "boot")

m_demo_visits_int_qr_perc <- rq(
  perc_mal ~ visits_scaled + I(visits_scaled^2) + women +
    race_lab +
    educ_lab +
    agegroup_lab,
  tau = tau,
  data = data
)
summary(m_demo_visits_int_qr_perc, se = "boot")

model_list_perc <- list(
  Model1 = summary(m_gender_int_qr_perc, se = "boot", R = 1000)$coef,
  Model2 = summary(m_gender_visits_int_qr_perc, se = "boot", R = 1000)$coef,
  Model3 = summary(m_race_int_qr_perc, se = "boot", R = 1000)$coef,
  Model4 = summary(m_race_visits_int_qr_perc, se = "boot", R = 1000)$coef,
  Model5 = summary(m_educ_int_qr_perc, se = "boot", R = 1000)$coef,
  Model6 = summary(m_educ_visits_int_qr_perc, se = "boot", R = 1000)$coef,
  Model7 = summary(m_agegroup_int_qr_perc, se = "boot", R = 1000)$coef,
  Model8 = summary(m_agegroup_visits_int_qr_perc, se = "boot", R = 1000)$coef,
  Model9 = summary(m_demo_int_qr_perc, se = "boot", R = 1000)$coef,
  Model10 = summary(m_demo_visits_int_qr_perc, se = "boot", R = 1000)$coef
)

latex_output_perc <- generate_latex_table(model_list_perc, COEF_LABELS2)

cat(latex_output_perc)
