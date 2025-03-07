#' Compute Sum of Squares Between Treatments (SSB)
#'
#' @description Computes the sum of squares between treatments for ANOVA.
#' @param df A dataframe containing the dataset.
#' @param treatment_col A character string specifying the treatment column.
#' @param value_col A character string specifying the dependent variable column.
#' @param message A logical indicating whether to print the result.
#' @return The SSB value.
#' @export
SSB <- function(df, treatment_col, value_col, message = FALSE) {
  warning("It is required that all groups have the same sample size 
")
  
  df <- df[, c(treatment_col, value_col)]
  overall_mean <- mean(df[[value_col]])
  n_per_group <- table(df[[treatment_col]])[1]  
  group_means <- tapply(df[[value_col]], df[[treatment_col]], mean)

  SSB_value <- sum(n_per_group * (group_means - overall_mean)^2)

  if (message) {
    paste("The sum of squares between treatments: ", SSB_value)
  } else {
    return(SSB_value)
  }
}
