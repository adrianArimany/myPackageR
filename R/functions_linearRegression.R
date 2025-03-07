#' Compute Sum of Squares for a Linear Model
#'
#' @description Computes SST (Total Sum of Squares), SSR (Regression Sum of Squares), and SSE (Error Sum of Squares).
#' @param model A linear model object (`lm`).
#' @return A list with SST, SSR, and SSE.
#' @export
compute_sums_squares <- function(model) {
  y <- model$model[[1]]  
  y_hat <- predict(model) 
  y_mean <- mean(y) 
  
  SST <- sum((y - y_mean)^2)
  SSR <- sum((y_hat - y_mean)^2)
  SSE <- sum((y - y_hat)^2)
  
  return(list(SST = SST, SSR = SSR, SSE = SSE))
}

#' Create a Full Linear Model with All Numeric Predictors
#'
#' @description Builds a linear model using all numeric predictors in a dataset.
#' @param data A dataframe containing the dataset.
#' @param y A character string specifying the dependent variable.
#' @return A fitted linear model (`lm` object).
#' @export
fullModelCreation <- function(data, y) {
  colnames(data) <- make.names(colnames(data))
  y <- make.names(y)  
  numeric_data <- data[, sapply(data, is.numeric) | names(data) == y]

  if (!(y %in% colnames(numeric_data))) {
    stop("The dependent variable is missing after filtering numeric columns.")
  }

  formula <- as.formula(paste(y, "~ ."))
  model <- lm(formula, data = numeric_data)
  return(model)
}
