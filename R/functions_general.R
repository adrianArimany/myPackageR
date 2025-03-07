#' Read an Excel File
#'
#' @description This function reads an Excel file and loads it as a dataframe.
#' @param path A character string specifying the path to the Excel file.
#' @param removeSpace A logical indicating whether to remove spaces in column names (default is TRUE).
#' @param sheet An optional sheet name or index to read (default is NULL, which reads the first sheet).
#' @return A dataframe containing the contents of the Excel sheet.
#' @import readxl
#' @export
excelPath <- function(path, removeSpace = TRUE, sheet = NULL) {
  
  if (!is.null(sheet)) {
    data <- readxl::read_excel(path, sheet = sheet)
  } else {
    data <- readxl::read_excel(path)
  }
  
  return(data)  
}

#' List Sheets in an Excel File
#'
#' @description This function lists all the sheet names in an Excel file.
#' @param path A character string specifying the path to the Excel file.
#' @return A character vector containing the names of the sheets in the Excel file.
#' @import readxl
#' @export
listExcelSheets <- function(path) {
  if (!file.exists(path)) {
    stop("File does not exist. Please provide a valid file path.")
  }
  
  sheets <- readxl::excel_sheets(path)
  return(sheets)
}

#' Read a CSV File
#'
#' @description This function reads a CSV file and loads it as a dataframe.
#' @param path A character string specifying the path to the CSV file.
#' @param removeSpace A logical indicating whether to remove spaces in column names (default is TRUE).
#' @return A dataframe containing the contents of the CSV file.
#' @import readr
#' @export
csvPath <- function(path, removeSpace = TRUE) {
  ## read_csv stores the csv as a tibble
  data <- readr::read_csv(path, show_col_types = FALSE)
  
  if (removeSpace) {
    colnames(data) <- gsub("\\s+", "_", trimws(colnames(data)))  # Replace spaces & trim
  }
  
  return(data)
}

#' Compute Descriptive Statistics
#'
#' @description Computes basic descriptive statistics (count, mean, median, SD, min, max, IQR) for numeric columns.
#' @param data A dataframe containing the data.
#' @return A dataframe containing descriptive statistics for numeric variables.
#' @import dplyr
#' @export
descriptive_stats <- function(data) {
  if (!is.data.frame(data)) stop("Input must be a dataframe")
  
  stats <- data %>%
    summarise(across(where(is.numeric), list(
      Count = ~sum(!is.na(.)),
      Mean = ~mean(., na.rm = TRUE),
      Median = ~median(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      Min = ~min(., na.rm = TRUE),
      Max = ~max(., na.rm = TRUE),
      IQR = ~IQR(., na.rm = TRUE)
    )))
  
  return(stats)
}

#' Compute Descriptive Statistics by Factor
#'
#' @description Computes descriptive statistics for numeric variables grouped by a factor column.
#' @param data A dataframe containing the data.
#' @param factor_col A character string specifying the factor column to group by.
#' @return A dataframe containing descriptive statistics for numeric variables grouped by the factor.
#' @import dplyr
#' @export
descriptive_stats_by_factor <- function(data, factor_col) {
  if (!(factor_col %in% colnames(data))) stop("Factor column not found in dataset")
  if (!is.factor(data[[factor_col]])) stop("Selected column must be a factor")
  
  stats <- data %>%
    group_by(.data[[factor_col]]) %>%
    summarise(across(where(is.numeric), list(
      Count = ~sum(!is.na(.)),
      Mean = ~mean(., na.rm = TRUE),
      Median = ~median(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      Min = ~min(., na.rm = TRUE),
      Max = ~max(., na.rm = TRUE),
      IQR = ~IQR(., na.rm = TRUE)
    ), .names = "{.col}_{.fn}")) %>%
    ungroup()
  
  return(stats)
}
