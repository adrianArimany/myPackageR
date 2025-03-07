#source("/home/adrianarimany/Documents/UVG/Year_2/Stats_2/functions_graphs.R") 
scatterdiagram <- function(data, x, y, indexsize = 4, label_column = FALSE) {
  if (!(x %in% colnames(data))) stop(paste("Column", x, "not found in dataset"))
  if (!(y %in% colnames(data))) stop(paste("Column", y, "not found in dataset"))
  
  # If label_column is FALSE, use row names (if available) by creating a new column
  if (isFALSE(label_column)) {
    if (!is.null(rownames(data))) {
      data$.rownames <- rownames(data)  # Create a new column for row names
      label_column <- ".rownames"  # Use this column as labels
    } else {
      label_column <- FALSE  # No labels if row names don't exist
    }
  }
  
  # Ensure label_column exists in data if provided as a column
  if (!isFALSE(label_column) && !(label_column %in% colnames(data))) {
    stop(paste("Label column", label_column, "not found in dataset"))
  }
  
  # Create base scatter plot
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_point(color = "blue", size = 3, alpha = 0.7) +  # Scatter points
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Slope trend line
    ggplot2::labs(
      title = paste("Scatter Plot of", x, "vs", y),
      x = x,
      y = y
    ) +
    ggplot2::theme_minimal()
  
  # Add labels if label_column exists
  if (!isFALSE(label_column)) {
    plot <- plot + ggplot2::geom_text(ggplot2::aes(label = .data[[label_column]]), 
                                      hjust = 1.2, vjust = 1.2, size = indexsize)
  }
  
  return(plot)
}

histogram_normal_residuals <- function(model, binwidth = 0.5, bins = 5) {
  
  residuals_df <- tidyr::tibble(
    Standardized_Residuals = rstandard(model)
  ) 
   
  
  ggplot2::ggplot(residuals_df, aes(x = Standardized_Residuals)) +
    ggplot2::geom_histogram(aes(y = after_stat(density)),binwidth = binwidth, bins = bins, fill = "blue", alpha = 0.2, color = "black") + 
    ggplot2::stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                  color = "red", linetype = "dashed", size = 1) +
    ggplot2::labs(
      title = "Histograma de Residuos Estadarizados con bondad a la Normal",
      x = "Residuos Estadarizados",
      y = "Densidad"
    ) +
    ggplot2::theme_minimal()
}



qqplot_normal_residual <- function(model, label_column = FALSE) {
  residuals_df <- tidyr::tibble(
    Standardized_Residuals = rstandard(model)
  )
  qq_data <- qqnorm(residuals_df$Standardized_Residuals, plot.it = FALSE)
  residuals_df <- dplyr::mutate(residuals_df,
                         Theoretical_Quantiles = qq_data$x,
                         Sample_Quantiles = qq_data$y)
  
  # If label_column is FALSE, use row names if available
  if (isFALSE(label_column)) {
    if (!is.null(rownames(residuals_df))) {
      residuals_df$.rownames <- rownames(residuals_df)  # Create a new column for row names
      label_column <- ".rownames"  # Use this column as labels
    } else {
      label_column <- FALSE  # No labels if row names don't exist
    }
  }
  
  plot <- ggplot2::ggplot(residuals_df, ggplot2::aes(x = Theoretical_Quantiles, y = Sample_Quantiles)) +
    ggplot2::geom_point(color = "blue") +  # Q-Q points
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Q-Q reference line
    ggplot2::labs(
      title = "Q-Q Residuos Estandarizados",
      x = "Quantiles Teóricos",
      y = "Quantiles de la Muestra"
    ) +
    ggplot2::theme_minimal()
  
  # Add labels if specified
  if (!isFALSE(label_column)) {
    plot <- plot + ggplot2::geom_text(aes(label = ifelse(abs(Standardized_Residuals) > 2, .data[[label_column]], "")), 
                                      hjust = -0.3, vjust = 0.5, size = 4, color = "black")
  }
  
  return(plot)
}


homoscedasticity_plot <- function(model, label_column = FALSE) { 
  residuals_df <- data.frame(
    Fitted = fitted(model),
    Standardized_Residuals = rstandard(model)
  )
  # If label_column is FALSE, use row names if available
  if (isFALSE(label_column)) {
    if (!is.null(rownames(residuals_df))) {
      residuals_df$.rownames <- rownames(residuals_df)  # Create a new column for row names
      label_column <- ".rownames"  # Use this column as labels
    } else {
      label_column <- FALSE  # No labels if row names don't exist
    }
  }
  
  plot <- ggplot2::ggplot(residuals_df, aes(x = Fitted, y = Standardized_Residuals)) +
    ggplot2::geom_point(color = "blue", alpha = 0.6) + 
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
    ggplot2::labs(
      title = "Residuos Standarizados contra Valores Ajustados",
      x = "Valores Ajustados",
      y = "Residuos Standarizados"
    ) +
    ggplot2::theme_minimal()
  
  
  if (!isFALSE(label_column)) {
    plot <- plot + ggplot2::geom_text(aes(label = ifelse(abs(Standardized_Residuals) > 2, .data[[label_column]], "")), 
                                      hjust = -0.3, vjust = 0.5, size = 4, color = "black")
  }
  
  return(plot)
}



boxPlotComparative <- function(data, x, y) {
  if (!(x %in% colnames(data))) stop(paste("Column", x, "not found in dataset"))
  if (!(y %in% colnames(data))) stop(paste("Column", y, "not found in dataset"))
  
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_boxplot() +  # boxplot
    ggplot2::labs(
      title = paste("Box plot of", x, "vs", y),
      x = x,
      y = y
    ) +
    ggplot2::theme_minimal()
  return(plot)
}

boxPlotComparativeBlocks <- function(df, x, y, Block, xlab = x, ylab = y, block_labels = NULL) {
  plot <- ggplot2::ggplot(df, aes(x = !!sym(x), y = !!sym(y))) + 
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(as.formula(paste("~", Block)), 
                        labeller = ggplot2::as_labeller(block_labels)) +  # Change facet labels
    ggplot2::labs(
      title = paste("Boxplot para", y, "por", x, "a lo largo de los", Block),
      x = xlab,
      y = ylab
    ) +
    ggplot2::theme_minimal()
  return(plot)
  #block_labels =  c("block A" = "Peso 75 (g)", "block B" = "Peso 170 (g)", "block C" = "Peso 250 (g)")
}
