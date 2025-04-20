generate_latex_table <- function(model_list, coef_names = NULL) {
  library(dplyr)
  library(tidyr)
  library(kableExtra)
  
  # Define a function for formatting coefficients with standard errors and significance levels
  format_coef_latex <- function(coef, se, p) {
    sig <- case_when(
      p < 0.01 ~ "\\sym{a}",
      p < 0.05 ~ "\\sym{b}",
      p < 0.01 ~ "\\sym{c}",
      # p < 0.001 ~ "\\textsuperscript{***}",
      # p < 0.01 ~ "\\textsuperscript{**}",
      # p < 0.05 ~ "\\textsuperscript{*}",
      TRUE ~ ""
    )
    paste0(
      formatC(coef, digits = 3, format = "f"), sig, "{(", 
      formatC(se, digits = 3, format = "f"), ")}"
    )
  }
  
  # Combine model summaries into a single table
  output_table <- bind_rows(
    lapply(model_list, function(df) {
      as.data.frame(df) %>%
        mutate(Variable = row.names(df)) %>%
        select(Variable, everything())
    }),
    .id = "Model"
  )
  
  # If coef_names is provided, join to replace Variable names
  if (!is.null(coef_names)) {
    coef_df <- tibble(Variable = names(coef_names), OfficialName = coef_names)
    output_table <- output_table %>%
      left_join(coef_df, by = "Variable") %>%
      mutate(Variable = coalesce(OfficialName, Variable)) %>%
      select(-OfficialName)
  }
  
  # Reshape the table and format the coefficients
  output_table_formatted <- output_table %>%
    rename(
      Coefficient = Value,
      `Std. Error` = `Std. Error`,
      `p-value` = `Pr(>|t|)`
    ) %>%
    mutate(Formatted = format_coef_latex(Coefficient, `Std. Error`, `p-value`)) %>%
    select(Model, Variable, Formatted) %>%
    pivot_wider(
      names_from = Model,
      values_from = Formatted
    ) %>%
    replace_na(list(Formatted = ""))  # Replace NA with empty strings
  
  # Generate the LaTeX table
  latex_table <- output_table_formatted %>%
    kable(
      align = c("l", rep("c", length(model_list))), 
      format = "latex", 
      escape = FALSE, 
      col.names = c("Variable", paste0("Model ", seq_along(model_list)))
    ) %>%
    kable_styling(full_width = FALSE, position = "center", latex_options = c("hold_position"))
  
  return(latex_table)
}
# 
# # Example Usage
# # Pass a list of models with their summaries
# model_list <- list(
#   Model1 = summary(m_demo_int_qr, se = "boot")$coef,
#   Model2 = summary(m_demo_int_qr, se = "boot")$coef
# )
# 
# # Pass coefficient names (optional)
# coef_names <- c(
#   "(Intercept)" = "Intercept",
#   "race_labAsian" = "Asian",
#   "race_labBlack" = "Black",
#   "race_labHispanic" = "Hispanic",
#   "race_labOther" = "Other Race"
# )
# 
# # Generate the LaTeX table
# latex_output <- generate_latex_table(model_list, coef_names)
# 
# # Print the table for integration into LaTeX
# cat(latex_output)


print_tex <- function(path) {
  if (file.exists(path)) {
    cat(readLines(path), sep = "\n")
  } else {
    message("File not found: ", path)
  }
}


# =========================================================================
# =========================================================================
# =========================================================================
# Constants ---------------------------------------------------------------
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

