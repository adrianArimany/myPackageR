#USE 
#source("/home/adrianarimany/Documents/UVG/Year_2/Stats_2/functions_linearRegression.R") 
#to use function

compute_sums_squares <- function(model) {
  y <- model$model[[1]]  
  y_hat <- predict(model) 
  y_mean <- mean(y) 
  
  # Compute SST
  SST <- sum((y - y_mean)^2)
  
  # Compute SSR
  SSR <- sum((y_hat - y_mean)^2)
  
  # Compute SSE
  SSE <- sum((y - y_hat)^2)
  
  
  return(list(SST = SST, SSR = SSR, SSE = SSE))
}


fullModelCreation <- function(data, y) {
  colnames(data) <- make.names(colnames(data))
  
  # Ensure y is properly formatted
  y <- make.names(y)  
  
  # Remove non-numeric columns except for y
  numeric_data <- data[, sapply(data, is.numeric) | names(data) == y]
  
  # Check if y is still in the dataset
  if (!(y %in% colnames(numeric_data))) {
    stop("The dependent variable is missing after filtering numeric columns.")
  }
  
  # Fit the linear model using a safe formula
  formula <- as.formula(paste(y, "~ ."))
  model <- lm(formula, data = numeric_data)
  return(model)
}

fullModelSummary <- function(data, y) {
  # Rename columns to be formula-friendly
  colnames(data) <- make.names(colnames(data))
  
  # Ensure y is properly formatted
  y <- make.names(y)  
  
  # Remove non-numeric columns except for y
  numeric_data <- data[, sapply(data, is.numeric) | names(data) == y]
  
  # Check if y is still in the dataset
  if (!(y %in% colnames(numeric_data))) {
    stop("The dependent variable is missing after filtering numeric columns.")
  }
  
  # Fit the linear model using a safe formula
  formula <- as.formula(paste(y, "~ ."))
  model <- lm(formula, data = numeric_data)
  result <- summary(model)
  
  return(result)
}

modelObject <- function(data, y, x = list(), interaction = list(), summary = FALSE) {
  warning("To add the interaction term you need interaction = list(c(var1, var2), c(var3,var4)) \n")
  warning("This function runs without the need to have an interaction assigned. \n")
  
  # Rename columns to be formula-friendly
  colnames(data) <- make.names(colnames(data))  # Fix column names
  
  # Ensure y and x are properly formatted
  y <- make.names(y)  
  x <- make.names(x)
  
  # Validate input: x must have at least one predictor
  if (length(x) == 0) stop("Error: The list of regressors (x) is empty. Please specify at least one regressor.")
  if (!all(x %in% colnames(data))) stop("Error: Some regressors in x are not found in the dataset.")
  if (!(y %in% colnames(data))) stop("Error: The dependent variable y is not found in the dataset.")
  
  # Ensure interaction terms exist in dataset and are valid
  if (length(interaction) > 0) {
    for (pair in interaction) {
      pair <- make.names(pair)  # Ensure names are valid
      if (length(pair) != 2) {
        stop("Error: Each interaction must be between exactly two variables.")
      }
      if (!all(pair %in% x)) {
        stop(paste("Error: Interaction terms", paste(pair, collapse = ":"), "must be included in x."))
      }
    }
  }
  
  # Remove non-numeric columns except for y
  numeric_data <- data[, c(y, x)]
  numeric_data <- numeric_data[, sapply(numeric_data, is.numeric)]
  
  # Check if y is still in the dataset
  if (!(y %in% colnames(numeric_data))) {
    stop("Error: The dependent variable is missing after filtering numeric columns.")
  }
  
  # Build the regression formula
  formula_parts <- x  # Start with main effects
  
  # Add interaction terms if specified
  if (length(interaction) > 0) {
    interaction_strings <- sapply(interaction, function(pair) {
      paste0(pair[1], "*", pair[2])  # Correctly format interactions
    })
    formula_parts <- c(formula_parts, interaction_strings)
  }
  
  # Construct full formula string
  formula_str <- paste(y, "~", paste(formula_parts, collapse = " + "))
  formula <- as.formula(formula_str)
  
  # Debugging: Print the formula being used
  print(paste("Model formula:", formula_str))
  
  # Fit the linear model
  model <- lm(formula, data = numeric_data)
  
  # Return either summary or model object
  if (summary) {
    result <- summary(model)
    return(result)  
  } else {
    return(model)
  }
}



#To test if the model for the f test is significant.
check_model_significance <- function(model, alpha) {
  f_stat <- summary(model)$fstatistic[1]  # F-value
  df1 <- summary(model)$fstatistic[2]     # Numerator DF
  df2 <- summary(model)$fstatistic[3]     # Denominator DF
  
  p_value <- pf(f_stat, df1, df2, lower.tail = FALSE)
  
  if (f_stat < 1) {
    interpretation <- cat("El modelo tiene un poder explicativo extremadamente bajo (F < 1).
                          \n No es mejor que un modelo con solo el intercepto, 
                          \n lo que indica que las variables predictoras no aportan información útil.")
  } else if (f_stat >= 1 & f_stat < 4) {
    interpretation <- cat("El modelo tiene un poder explicativo débil (1 <= F < 4).
                          \n Puede no ser útil en conjuntos de datos grandes, 
                          \n ya que la varianza explicada es baja.")
  } else {
    if (p_value < alpha) {
      interpretation <- cat("Hay evidencia para rechazar la hipótesis nula con un nivel de significancia:", alpha,
                              "\n Esto indica que el modelo de regresión es significativo y que",
                              "\n al menos una de las variables predictoras contribuye a explicar la variable dependiente.")
    } else {
      interpretation <- cat("No hay evidencia suficiente para rechazar la hipótesis nula con un nivel de significancia:", alpha,
                              "\n Esto sugiere que el modelo de regresión no es significativo y que",
                              "\n ninguna de las variables predictoras contribuye de manera sustancial a explicar la variable dependiente.")
    }
  }
  
  return(cat("F-Statistic:", round(f_stat, 3), 
               "| p-value:", format.pval(p_value, digits = 3, eps = 0.001), 
               "\n", interpretation))
  
  #example
  #check_model_significance(model, alpha = 0.05) 
}

#Compare the Rsquare with Adjusted R-square
compareRsquare <- function(model, threshold = 0.10) {
  Rsquare <- summary(model)$r.squared
  adjRsquare <- summary(model)$adj.r.squared
  
  difference <- Rsquare - adjRsquare
  
  if (difference < threshold) {
    cat("La diferencia entre R^2 y R^2 ajustado es", round(difference, 4),
                 "\n El modelo no parece estar sobreajustado.")
  } else {
    cat("La diferencia entre R^2 y R^2 ajustado es", round(difference, 4),
              "\n Esto sugiere que el modelo podría estar sobreajustado,",
              "\n lo que indica que algunas variables podrían no aportar información útil.")
  }
  # Example usage:
  #compareRsquare(model, threshold = 0.100)
  #you need to define some threshhold.
}


#shapiroTest to test normality
shapiroTest <- function(updatedModel, alpha = 0.05) {
  result <- stats::shapiro.test(updatedModel$residuals)
  p_value <- result$p.value
  print(result)  
  if (p_value < alpha) {
    cat("Hay evidencia para rechazar la hipótesis nula con un nivel de significancia:", alpha, 
                            "\nPor ende, los residuos no son normalmente distribuidos.\n")
  } else {
    cat("No hay evidencia para rechazar la hipótesis nula con un nivel de significancia:", alpha,
                            "\nPor ende, los residuos son normalmente distribuidos. \n")
  }
  #example
  #shapiroTest(model, alpha = 0.05)
  #the predefined alpha is 0.05 but you can change it  
}

#Test if the epsilons have expectation 0
Eepsilon <- function(updatedModel,categorical = TRUE, alpha = 0.05, message = TRUE) {
  promedio <- mean(updatedModel$residuals)
  if (!categorical) {
    sigma <- sqrt(sum(residuals(updatedModel)^2) / df.residual(updatedModel))
  } else {
    # Manually calculate sigma (residual standard deviation)
    sigma <- summary(updatedModel)$sigma  
  }
  n <- length(updatedModel$residuals)
  SE <- sigma / sqrt(n)
  z_alpha <- qnorm(1 - alpha / 2)
  CIupper <- promedio + z_alpha * SE
  CIlower <- promedio - z_alpha * SE
  
  cat("Confidence Interval:", "[", CIlower, ",", CIupper, "]\n")
  
  if (message) { # Only print if message is TRUE
    if (CIupper < 0 || CIlower > 0) {
      cat("There is sufficient evidence to reject the null hypothesis (p <", alpha, ")\n")
      cat("The expected value of the residuals is likely different from 0.\n")
    } else {
      cat("There is not sufficient evidence to reject the null hypothesis (p >=", alpha, ")\n")
      cat("The expected value of the residuals could be 0.\n")
    }
  } # End of the if (message) block
}


#Test if the elpsilons have variance equals to \sigma^2 using BreushPaga
BreushPaga <- function(updatedModel, alpha = 0.05) {
  result <- lmtest::bptest(updatedModel)
  p_value <- result$p.value
  print(result)
  if (p_value < alpha) {
    cat("Hay evidencia para rechazar la hipótesis nula con un nivel de significancia:", alpha,
        "\n Por ende los residuos tienen heteroscedasticidad. \n")
  } else {
    cat("No hay evidencia para rechazar la hipótesis nula con un nivel de significancia:", alpha, 
        "\n Por ende los residuos tienen homoscedasticity.\n")
  }
  #Example
  #BreushPaga(model, alpha = 0.05)
}

#Test if the epsilons are independent using DurbinWatson 
DurbinWatson <- function(updatedModel, alpha = 0.05) {
  result <- car::durbinWatsonTest(updatedModel)
  p_value <- result$p
  
  print(result)
  
  if (p_value < alpha) {
    cat("Hay evidencia para rechazar la hipótesis nula con un nivel de significancia:", alpha, 
        "\n Por ende los residuos son dependientes.\n")
  } else {
    cat("No hay evidencia para rechazar la hipótesis nula con un nivel de significancia:", alpha, 
        "\n Por ende los residuos son independientes.\n")
  }
  #Example
  #DurbinWatson(model, alpha)
}


#Analyze the results from a ggpairs, 
#prints most corrolated variable between regressors
#prints the regressors that are corrolated with the dependent variable
#prints the regressors that best fit a normal distribution.
analyze_ggpairs <- function(data, alpha = 0.05, message = TRUE) {
  warning("Please have dependent variable in the first column of the data.frame \n")
  warning("Do not have an Index or Colnames() as a variable in the data.frame \n")
  warning("Do notice that the corrrolation tests use Pearsons test,\n so recall the assumptions of this test are x1 and x2 are normal and linear\n")
  ggp <- GGally::ggpairs(data)
  
  # Compute correlation matrix
  correlation_matrix <- cor(data, use = "pairwise.complete.obs")
  
  # Extract correlation of independent variable (assuming first column is dependent variable y)
  dependent_var <- colnames(data)[1]  # Assumes the first column is the dependent variable
  correlations <- correlation_matrix[, dependent_var]  # Get correlations
  
  # Identify the most correlated variable
  best_regressor <- names(which.max(abs(correlations[-1])))  # Exclude y itself
  
  # Compute significance of correlations using Pearson test
  significant_regressors <- c()
  for (regressor in colnames(data)[-1]) {  # Exclude dependent variable
    test <- cor.test(data[[dependent_var]], data[[regressor]], method = "pearson")
    if (test$p.value < alpha) {
      significant_regressors <- c(significant_regressors, regressor)
    }
  }
  
  # Identify which regressor is best fitted to a normal distribution using Shapiro-Wilk test
  normality_scores <- sapply(data[-1], function(x) shapiro.test(x)$p.value)  # Exclude dependent variable
  best_normal_regressor <- names(which.max(normality_scores))
  
  # Identify regressors with the best linear fit to the dependent variable
  linear_fit_scores <- sapply(colnames(data)[-1], function(x) {
    model <- lm(as.formula(paste(dependent_var, "~", x)), data = data)
    return(summary(model)$r.squared)
  })
  
  best_linear_regressor <- names(which.max(linear_fit_scores))
  
  # Identify significant correlations between independent variables
  significant_inter_regressors <- list()
  independent_vars <- colnames(data)[-1]  # Exclude dependent variable
  
  for (i in seq_along(independent_vars)) {
    for (j in seq(i + 1, length(independent_vars))) {
      var1 <- independent_vars[i]
      var2 <- independent_vars[j]
      
      if (var2 %in% colnames(data)) {  # Ensure column exists
        test <- cor.test(data[[var1]], data[[var2]], method = "pearson")
        if (test$p.value < alpha) {
          significant_inter_regressors <- c(significant_inter_regressors, paste(var1, "&", var2))
        }
      }
    }
  }
  #Note that this corrolation uses Pearsons Corrolation test which, assumes that:
  # 1. X1 and X2 have a linear corrolation,
  # 2. X1 and X2 are normally distriuted.
  # Create output summary
  alpha <- alpha #Stores signficant levle so that is can be printed.
  result <- list(
    Significant_level_used = alpha,
    Most_Correlated_Variable_With_Dependent_Variable = best_regressor,
    Significant_Correlations_With_Dependent_Variable = significant_regressors,
    Best_Normal_Distribution_For_Regressors = best_normal_regressor,
    Best_Linearity_With_Dependent_Variable = best_linear_regressor,
    Significant_Correlations_Between_Regressors = significant_inter_regressors
  )
  if (message) {
    print(ggp)
    print("Returning correlation analysis results:")
    print(result)
  } else {
    print(ggp)
  }
  
  # Notes:
  # It is important that the dependent variable is the first column
  # It is important that no Index or ColNames is present in the data.
}

significant_regressors <- function(model, alpha = 0.05, units = list(), intercept = TRUE) {
  warning("If variable has no units, then do not add to the list of units. \n")
  # Extract coefficients and p-values from summary
  summary_model <- summary(model)
  coeffs <- summary_model$coefficients  # Get coefficients and p-values
  
  # Extract dependent variable name from formula
  dependent_var <- as.character(formula(model))[2]
  
  # Identify significant regressors (p-value < alpha)
  significant_regressors <- rownames(coeffs)[coeffs[, 4] < alpha]  # p-values are in column 4
  
  if (!intercept) {
    significant_regressors <- setdiff(significant_regressors, "(Intercept)")
  }
  
  # Generate interpretations
  interpretations <- lapply(significant_regressors, function(regressor) {
    coef_value <- coeffs[regressor, 1]  # Extract coefficient
    sign <- ifelse(coef_value > 0, "aumenta", "disminuye")
    
    # Get the unit of the regressor and dependent variable (default: "unidad")
    regressor_unit <- ifelse(regressor %in% names(units), units[[regressor]], "unidad")
    dependent_unit <- ifelse(dependent_var %in% names(units), units[[dependent_var]], "unidad")
    
    # Construct interpretation string
    interpretation <- paste(
      "Un aumento en", regressor, "por unidad",
      paste0("(", regressor_unit, ")"),
      sign, dependent_var, "en", round(abs(coef_value), 4),
      paste0("(", dependent_unit, ")"),
      "en promedio, manteniendo las demás variables constantes."
    )
    return(interpretation)
  })
  
  # Format output for significant regressors
  regressors_text <- if (length(significant_regressors) > 0) {
    paste("Regresores significantes con un nivel de significancia", alpha)
  } else {
    paste("No hay regresores significantes con un nivel de significancia", alpha)
  }
  
  # Create output summary
  result <- list(
    Dependent_Variable = paste(dependent_var, "(", ifelse(dependent_var %in% names(units), units[[dependent_var]], "unidad"), ")"),
    Any_Significant_Regressors = regressors_text,
    The_Significant_Regressors =  significant_regressors,
    Interpretations = interpretations
  )
  
  return(result)
  #Example
  #units_list <- list(
  #"y" = "comida",
  #"x1" = "años",
  #"x2" = "años de estudio",
  #"x4" = "fwef",
  #"x5" = "fewf",
  #"x6" = "fewew")
  #So have the regressor with their respective unit, if no unit is defined, don't add it in the list
  #significant_regressors(model, alpha = 0.05, units = units_list)
}

significant_regressors_withoutSummary <- function(model, alpha = 0.05, units = list(), intercept = TRUE) {
  warning("If variable has no units, then do not add to the list of units. \n")
  # Extract coefficients and p-values from summary
  #summary_model <- summary(model) //in case it has a model
  
  coeffs <- model$coefficients  # Get coefficients and p-values
  
  
  # Extract dependent variable name from formula
  dependent_var <- as.character(formula(model))[2]
  
  # Identify significant regressors (p-value < alpha)
  significant_regressors <- rownames(coeffs)[coeffs[, 4] < alpha]  # p-values are in column 4
  
  if (!intercept) {
    significant_regressors <- setdiff(significant_regressors, "(Intercept)")
  }
  
  # Generate interpretations
  interpretations <- lapply(significant_regressors, function(regressor) {
    coef_value <- coeffs[regressor, 1]  # Extract coefficient
    sign <- ifelse(coef_value > 0, "aumenta", "disminuye")
    
    # Get the unit of the regressor and dependent variable (default: "unidad")
    regressor_unit <- ifelse(regressor %in% names(units), units[[regressor]], "unidad")
    dependent_unit <- ifelse(dependent_var %in% names(units), units[[dependent_var]], "unidad")
    
    # Construct interpretation string
    interpretation <- paste(
      "Un aumento en", regressor, "por unidad",
      paste0("(", regressor_unit, ")"),
      sign, dependent_var, "en", round(abs(coef_value), 4),
      paste0("(", dependent_unit, ")"),
      "en promedio, manteniendo las demás variables constantes."
    )
    return(interpretation)
  })
  
  # Format output for significant regressors
  regressors_text <- if (length(significant_regressors) > 0) {
    paste("Regresores significantes con un nivel de significancia", alpha)
  } else {
    paste("No hay regresores significantes con un nivel de significancia", alpha)
  }
  
  # Create output summary
  result <- list(
    Dependent_Variable = paste(dependent_var, "(", ifelse(dependent_var %in% names(units), units[[dependent_var]], "unidad"), ")"),
    Any_Significant_Regressors = regressors_text,
    The_Significant_Regressors =  significant_regressors,
    Interpretations = interpretations
  )
  
  return(result)
  #Example
  #units_list <- list(
  #"y" = "comida",
  #"x1" = "años",
  #"x2" = "años de estudio",
  #"x4" = "fwef",
  #"x5" = "fewf",
  #"x6" = "fewew")
  #So have the regressor with their respective unit, if no unit is defined, don't add it in the list
  #significant_regressors(model, alpha = 0.05, units = units_list)
}

create_interaction_terms <- function(data) {
  data <- as.data.frame(data)
  regressors <- data[, -1, drop = FALSE]
  interaction_terms <- combn(colnames(regressors), 2, FUN = function(x) paste(x, collapse = " * "), simplify = TRUE)
  interaction_formula <- paste(interaction_terms, collapse = " + ")
  interaction_data <- model.matrix(as.formula(paste("~", interaction_formula)), data = data)
  interaction_data <- interaction_data[, -1, drop = FALSE]
  return(as.data.frame(interaction_data))
}

