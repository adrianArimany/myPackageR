#source("/home/adrianarimany/Documents/UVG/Year_2/Stats_2/functions_general.R") 
excelPath <- function(path, removeSpace = TRUE, sheet = NULL) {
  
  if (!is.null(sheet)) {
    data <- readxl::read_excel(path, sheet = sheet)
  } else {
    data <- readxl::read_excel(path)
  }
  
  return(data)  
}

listExcelSheets <- function(path) {
  if (!file.exists(path)) {
    stop("File does not exist. Please provide a valid file path.")
  }
  
  sheets <- readxl::excel_sheets(path)
  return(sheets)
}

csvPath <- function(path, removeSpace = TRUE) {
  ##read_csv stores the csv as a tibble
  data <- readr::read_csv(path, show_col_types = FALSE)
  
  if (removeSpace) {
    colnames(data) <- gsub("\\s+", "_", trimws(colnames(data)))  # Replace spaces & trim
  }

  return(data)
}

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