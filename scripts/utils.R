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