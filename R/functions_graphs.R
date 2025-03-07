#' Scatter Plot with Optional Labels
#'
#' @description Creates a scatter plot between two variables with an optional linear regression trend line.
#' @param data A dataframe containing the dataset.
#' @param x A character string specifying the x-axis variable.
#' @param y A character string specifying the y-axis variable.
#' @param indexsize Numeric value controlling the size of point labels.
#' @param label_column A logical or character string. If TRUE, row names are used as labels.
#' @return A ggplot object.
#' @import ggplot2
#' @export
scatterdiagram <- function(data, x, y, indexsize = 4, label_column = FALSE) {
  if (!(x %in% colnames(data))) stop(paste("Column", x, "not found in dataset"))
  if (!(y %in% colnames(data))) stop(paste("Column", y, "not found in dataset"))
  
  if (isFALSE(label_column)) {
    if (!is.null(rownames(data))) {
      data$.rownames <- rownames(data)
      label_column <- ".rownames"
    } else {
      label_column <- FALSE
    }
  }

  if (!isFALSE(label_column) && !(label_column %in% colnames(data))) {
    stop(paste("Label column", label_column, "not found in dataset"))
  }

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_point(color = "blue", size = 3, alpha = 0.7) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
    ggplot2::labs(title = paste("Scatter Plot of", x, "vs", y), x = x, y = y) +
    ggplot2::theme_minimal()

  if (!isFALSE(label_column)) {
    plot <- plot + ggplot2::geom_text(ggplot2::aes(label = .data[[label_column]]), hjust = 1.2, vjust = 1.2, size = indexsize)
  }
  
  return(plot)
}
