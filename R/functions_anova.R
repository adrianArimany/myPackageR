#' sum of squares between treatments.
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

#'Mean Square error MSE
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

#' Anova with just one factor.
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