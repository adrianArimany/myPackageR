#' sum of squares between treatments.
#' @param df the data frame.
#' @param treatment_col the column with the treatment names
#' @param value_col the column with the values.
#' @param message TRUE if you want to see the result, FALSE if the you don't want the result.
#' @export
SSB <- function(df, treatment_col, value_col, message = FALSE) {
  warning("Is require that all the groups have the same sample size \n")
  # Ensure the columns are correctly referenced
  df <- df[, c(treatment_col, value_col)]
  
  # Compute the overall mean
  overall_mean <- mean(df[[value_col]])
  
  # Compute the number of observations per group
  n_per_group <- table(df[[treatment_col]])[1]  # Assumes equal sample sizes
  
  # Compute group means
  group_means <- tapply(df[[value_col]], df[[treatment_col]], mean)
  
  # Compute SSB
  SSB_value <- sum(n_per_group * (group_means - overall_mean)^2)
  
  if (message) {
    paste("The sum of squares between treatments: ", SSB_value)
  } else {
    return(SSB_value)
  }
  
  
}

#' mean square between treatments.
#' @param df the data frame.
#' @param treatment_col the column with the treatment names
#' @param value_col the column with the values.
#' @param message TRUE if you want to see the result, FALSE if the you don't want the result. 
#' @export
MSB <- function(df, treatment_col, value_col, message = FALSE) {
  #Computes the SSB with the SSB function
  SSB_result <- SSB(df, treatment_col, value_col)
  
  # Check if the treatment coloumn is character or factor.
  if (is.character(df[[treatment_col]])) {
    num_groups <- length(unique(df[[treatment_col]]))
  } else if (is.factor(df[[treatment_col]])) {
    num_groups <- nlevels(factor(df[[treatment_col]]))
  } else {
    warning("Error: can't find the number of groups because either has to be a factor or numeric \n")
  }
  
  #Calculates the MSB result
  MSB_result <- SSB_result /(num_groups - 1)
  
  #Prints the result or retuns it
  if (message) {
    print(paste("The Mean Square between Treatments: ", round(MSB_result, 2)))
  } else {
    return(MSB_result)
  }
}

#'Compute the sum of squares due to error (SSE)
#' @param df the data frame.
#' @param treatment_col the column with the treatment names
#' @param value_col the column with the values.
#' @param message TRUE if you want to see the result, FALSE if the you don't want the result.
#' @export
SSE <- function(df, treatment_col, value_col, message = FALSE) {
  warning("It is required that all groups have the same sample size \n")
  
  #Compute the means as  a matrix
  group_means <- tapply(df[[value_col]], df[[treatment_col]],  mean)
  
  
  # Check if the treatment coloumn is character or factor. then Cumpte the SSW
  if (is.character(df[[treatment_col]])) {
    SSE_result <- sum((df[[value_col]] - group_means[as.character(df[[treatment_col]])])^2)
  } else if (is.factor(df[[treatment_col]])) {
    SSE_result <- sum((df[[value_col]] - group_means[as.factor(df[[treatment_col]])])^2)
  } else {
    #IF is neither character or factor, we convert it as character.
    SSE_result <- sum((df[[value_col]] - group_means[as.character(df[[treatment_col]])])^2)
  }
  
  if (message) {
    print(paste("The value for the Within groups sum of square: ", round(SSE_result, 2)))
  } else {
    return(SSE_result)
  }
}

#' Mean Square error MSE
#' @param df the data frame.
#' @param treatment_col the column with the treatment names
#' @param value_col the column with the values.
#' @param message TRUE if you want to see the result, FALSE if the you don't want the result.
#' @export
MSE <- function(df, treatment_col, value_col, message = FALSE) {
  # Ensure no missing values in treatment_col (debugging)
  df <- df[complete.cases(df[, c(treatment_col, value_col)]), ]
  
  # Takes SSE (Sum of Squares Error) from another myPackage method
  SSE_result <- SSE(df, treatment_col, value_col)
  
  # Check if the treatment column is character or factor
  if (is.character(df[[treatment_col]])) {
    num_groups <- length(unique(df[[treatment_col]]))
  } else if (is.factor(df[[treatment_col]])) {
    num_groups <- nlevels(df[[treatment_col]])
  } else {
    stop("Error: The treatment column must be either character or factor.")
  }
  
  # Compute total number of observations
  N <- length(df[[value_col]])
  
  # Compute MSE (Mean Square Error)
  MSE_result <- SSE_result / (N - num_groups)
  
  # Display message if required
  if (message) {
    print(paste("The Mean Square Error: ", round(MSE_result, 2)))
  } else {
    return(MSE_result)
  }
}

#' manual creation of Anova test.
#' @description
#' Manually creates the table for the anova table using the defined parameters. 
#' @param SST the total sum of squares
#' @param SSR the total sum of error squares
#' @param nT the total number of observations for every treatment
#' @param k the number of levels or factors in the treatment
#' @param alpha the rejection level
#' @param message False if you want the conclusion hypothesis, TRUE if you you don't want the conclusion hypothesis. 
#' @returns The Anova table or the conclusion for the hypothesis.
#' @export
anova_test_manual <- function(SST, SSTR, nT, k, alpha = 0.05, message = FALSE) {
  # Determines the Degrees of Freedom
  DF_Total <- nT - 1 
  DF_Treatment <- k - 1
  DF_Error <- DF_Total - DF_Treatment
  
  # Determines Sum of Squares for Error
  SSE <- SST - SSTR
  
  # Determines Mean Squares
  MS_Treatment <- SSTR / DF_Treatment
  MS_Error <- SSE / DF_Error
  
  # F-Ratio
  F_value <- MS_Treatment / MS_Error
  
  # Compute p-value via an F-test
  p_value <- pf(F_value, DF_Treatment, DF_Error, lower.tail = FALSE)
  
  # Display ANOVA Table
  anova_table <- data.frame(
    Source = c("Treatment", "Error", "Total"),
    SS = c(SSTR, SSE, SST),
    DF = c(DF_Treatment, DF_Error, DF_Total),
    MS = c(MS_Treatment, MS_Error, NA),
    F = c(F_value, NA, NA),
    p_value = c(p_value, NA, NA)
  )
  
  if (message) {
    print(anova_table)
    if (p_value < alpha) {
      cat("\nConclution: Reject the null hypothesis. There is a significant difference between the treatment means.\n")
    } else {
      cat("\nConclution: Fail to reject the null hypothesis. No significant difference detected between the treatment means.\n")
    }
  } else {
    print(anova_table)  
  }
  
}



#' Anova with just one factor.
#' @param df data frame
#' @param treatment_col the treatment coloumn's name (where the treatments name are)
#' @param value_col the observations coloumn from the levels.
#' @export
singleAOV <- function(df, treatment_col, value_col, summary = FALSE) {
  warning("This function only works with a one way ANOVA \n")
  
  df[[treatment_col]] <- as.factor(df[[treatment_col]])
  
  #Gets the anova model
  model <- aov(as.formula(paste(value_col, "~", treatment_col)), data = df)
  
  
  #If summary true then prints summary table, otherwise returns the model
  if (summary) {
    return(summary(model))
  } else {
    return(model)  
  }
} 

#' Fisher Least Square Difference, direct confidence interval for two specific levels.
#' @param name description
#' @param group2 first group 
#' @param alpha the confidence level for rejection.
#' @param message TRUE if you want to see a message, FALSE if you just want the CI
#' @export
flsd_CI <- function(modelAnova, levelVar, group1, group2, alpha = 0.05, message = FALSE) {
  lsd_result <- LSD.test(modelAnova, levelVar, alpha = alpha, console = FALSE)
  
  mean_diff <- abs(lsd_result$means[group1, "Value"] - lsd_result$means[group2, "Value"])
  LSD_value <- lsd_result$statistics$LSD
  
  LCL <- mean_diff - LSD_value
  UCL <- mean_diff + LSD_value
  
  if (message) {
    cat("\n95% Confidence Interval for the Difference between Means:", "(", LCL, ",", UCL, ")\n")
  } else {
    return(c(LCL, UCL))  
  }
  
}