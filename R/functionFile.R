#' Analyze a dataframe for common structural issues and information
#'
#' @param df A data.frame (or tibble)
#' @param check_outliers Whether to calculate and flag outliers (default TRUE)
#' @param check_bad_names Whether to flag suspicious column names (default TRUE)
#' @param check_mixed_types Whether to detect mixed types (default TRUE)
#' @param check_empty_strings Whether to count empty strings (default TRUE)
#' @return A tibble with one row for each column of the original dataframe
#' @export
analyze_table <- function(df,
                          check_outliers = TRUE,
                          check_bad_names = TRUE,
                          check_mixed_types = TRUE,
                          check_empty_strings = TRUE){
  #security for input dataframe
  stopifnot(is.data.frame(df))

  col_names <- names(df)
  rows_list <- vector("list", length(col_names))

  for(i in seq_along(col_names)){
    colname <- col_names[i]
    col_data <- df[[colname]]
    col_type <- class(col_data)[1]

    n_missing <- sum(is.na(col_data))
    percent_missing <- round(n_missing / length(col_data) * 100, 2)

    #constants
    n_unique_non_na <- length(unique(na.omit(col_data)))
    all_missing <- all(is.na(col_data))
    constant <- (!all_missing) && (n_unique_non_na == 1) && (sum(!is.na(col_data)) == length(col_data))

    suspected_id <- !anyDuplicated(col_data) && !any(is.na(col_data))

    #outliers
    has_outliers <- FALSE
    if(check_outliers && is.numeric(col_data)){
      z_scores <- scale(col_data)
      has_outliers <- any(abs(z_scores) > 3, na.rm = TRUE)
    }

    #empty strings
    empty_string_count <- if (check_empty_strings && is.character(col_data)) {
      sum(col_data == "", na.rm = TRUE)
    } else 0

    #numeric-like strings
    is_numeric_str <- FALSE
    if(is.character(col_data)){
      is_numeric_str <- all(grepl("^\\s*-?\\d*\\.?\\d+\\s*$", na.omit(col_data)))
    }

    #levels
    n_levels <- if (is.character(col_data) || is.factor(col_data)) length(unique(na.omit(col_data))) else NA

    #mixed types
    mixed_types_detected <- FALSE
    if(check_mixed_types){
      if(is.list(col_data)){
        elem_classes <- sapply(col_data, function(x) class(x)[1])
        mixed_types_detected <- length(unique(elem_classes)) > 1
      } else if(is.character(col_data)){
        non_na <- na.omit(col_data)
        mixed_types_detected <- any(is.na(suppressWarnings(as.numeric(non_na)))) &&
          any(!is.na(suppressWarnings(as.numeric(non_na))))
      }
    }

    #bad names
    bad_column_name <- FALSE
    if(check_bad_names){
      bad_column_name <- grepl("[^A-Za-z0-9_]|^\\d|\\s", colname)
    }

    #grouping the information
    issues <- character()
    if (n_missing > 0) issues <- c(issues, "has missing")
    if (empty_string_count > 0) issues <- c(issues, "empty strings")
    if (constant) issues <- c(issues, "constant")
    if (suspected_id) issues <- c(issues, "unique id?")
    if (has_outliers) issues <- c(issues, "possible outliers")
    if (is_numeric_str) issues <- c(issues, "numeric-like strings")
    if (mixed_types_detected) issues <- c(issues, "mixed types")
    if (bad_column_name) issues <- c(issues, "bad column name")

    rows_list[[i]] <- tibble::tibble(
      column = colname,
      type = col_type,
      n_missing = n_missing,
      percent_missing = percent_missing,
      empty_string_count = empty_string_count,
      is_numeric_str = is_numeric_str,
      n_levels = n_levels,
      constant = constant,
      suspected_id = suspected_id,
      has_outliers = has_outliers,
      mixed_types_detected = mixed_types_detected,
      bad_column_name = bad_column_name,
      issues = ifelse(length(issues) > 0, paste(issues, collapse = "; "), "")
    )
  }

  summary_df <- dplyr::bind_rows(rows_list)
  return(summary_df)
}


#' Summarize numeric columns with stats and suggested fill
#'
#' @param df A data.frame or tibble
#' @return A tibble of summary stats and recommended fill method per numeric column
#' @export
summarize_numeric <- function(df){
  #security check
  stopifnot(is.data.frame(df))

  numeric_cols <- df[, sapply(df, is.numeric), drop = FALSE]

  #if no numeric columns
  if(ncol(numeric_cols) == 0){
    warning("No numeric columns found.")
    return(tibble::tibble())
  }

  num_names <- names(numeric_cols)
  rows_list <- vector("list", length(num_names))

  for(i in seq_along(num_names)){
    colname <- num_names[i]
    x <- numeric_cols[[colname]]
    x_no_na <- na.omit(x)

    mean_val <- mean(x_no_na)
    median_val <- median(x_no_na)
    sd_val <- sd(x_no_na)
    min_val <- min(x_no_na)
    max_val <- max(x_no_na)
    n_missing <- sum(is.na(x))

    skewness <- if (length(x_no_na) > 2){
      mean((x_no_na - mean_val)^3) / sd_val^3
    } else NA

    kurtosis <- if (length(x_no_na) > 3){
      mean((x_no_na - mean_val)^4) / sd_val^4
    } else NA

    #recommended replacement for NA
    if(is.na(skewness)){
      recommended_fill <- "NA"
    }else if (abs(skewness) < 0.5){
      recommended_fill <- "mean"
    }else if (abs(skewness) < 1.5){
      recommended_fill <- "median"
    } else{
      recommended_fill <- "model/robust"
    }

    rows_list[[i]] <- tibble::tibble(
      column = colname,
      mean = round(mean_val, 2),
      median = round(median_val, 2),
      sd = round(sd_val, 2),
      min = round(min_val, 2),
      max = round(max_val, 2),
      skewness = round(skewness, 2),
      kurtosis = round(kurtosis, 2),
      n_missing = n_missing,
      recommended_fill = recommended_fill
    )
  }

  summary_df <- dplyr::bind_rows(rows_list)
  return(summary_df)
}


#' Suggest cleaning actions based on diagnostics
#'
#' @param df A data.frame or tibble
#' @param na_threshold Percentage threshold of missing data before removal is suggested (default 90)
#' @param suggest_constant Whether to suggest dropping constant columns (default TRUE)
#' @param suggest_numeric_str Whether to suggest converting numeric-like strings (default TRUE)
#' @param suggest_impute Whether to suggest imputation for numeric columns (default TRUE)
#' @return A tibble with suggested actions.
#' @export
suggest_cleaning <- function(df,
                             na_threshold = 90,
                             suggest_constant = TRUE,
                             suggest_numeric_str = TRUE,
                             suggest_impute = TRUE){
  #security check
  stopifnot(is.data.frame(df))

  diag <- analyze_table(df)
  num_diag <- summarize_numeric(df)

  suggestions <- list()

  for(i in seq_len(nrow(diag))){
    row <- diag[i, ]
    col <- row$column
    col_data <- df[[col]]

    #mostly missing
    if(row$percent_missing >= na_threshold){
      suggestions[[length(suggestions)+1]] <- list(
        column = col,
        action = "remove_column",
        reason = paste0("missingness above ", na_threshold, "%"),
        value = paste0(row$percent_missing, "% missing")
      )
      next
    }

    #constant
    if(suggest_constant && isTRUE(row$constant)){
      suggestions[[length(suggestions)+1]] <- list(
        column = col,
        action = "remove_column",
        reason = "constant column",
        value = NA
      )
      next
    }

    #convert numeric-like
    if(suggest_numeric_str && isTRUE(row$is_numeric_str)){
      suggestions[[length(suggestions)+1]] <- list(
        column = col,
        action = "convert_type",
        reason = "numeric-like strings detected",
        value = "character → numeric"
      )
    }

    #impute
    if(suggest_impute && is.numeric(col_data) && any(is.na(col_data))){
      fill_method <- num_diag$recommended_fill[num_diag$column == col]
      fill_value <- switch(
        fill_method,
        mean = mean(col_data, na.rm = TRUE),
        median = median(col_data, na.rm = TRUE),
        "model/robust" = median(col_data, na.rm = TRUE),
        NA
      )
      if(!is.na(fill_value)){
        suggestions[[length(suggestions)+1]] <- list(
          column = col,
          action = "impute",
          reason = paste("missing values +", fill_method, "fill"),
          value = round(fill_value, 2)
        )
      }
    }
  }

  n <- length(suggestions)
  columns <- character(n)
  actions <- character(n)
  reasons <- character(n)
  values <- character(n)

  for(i in seq_len(n)){
    columns[i] <- suggestions[[i]]$column
    actions[i] <- suggestions[[i]]$action
    reasons[i] <- suggestions[[i]]$reason
    values[i] <- suggestions[[i]]$value
  }

  tibble::tibble(
    step = seq_len(n),
    column = columns,
    action = actions,
    reason = reasons,
    value = values
  )
}



#' Apply cleaning actions to selected columns with configurable rules
#'
#' @param df A data.frame or tibble
#' @param columns Character vector of column names to clean
#' @param na_threshold Percentage threshold of missing data before removal is suggested (default 90)
#' @param drop_constant Whether to drop constant columns (default TRUE)
#' @param convert_numeric_str Whether to convert numeric-like strings (default TRUE)
#' @param impute Whether to impute missing numeric values (default TRUE)
#' @param impute_method_override Optional string ("mean" or "median") to force imputation method
#' @param verbose Whether to print actions (default TRUE)
#' @return Cleaned data.frame
#' @export
apply_cleaning <- function(df,
                           columns,
                           na_threshold = 90,
                           drop_constant = TRUE,
                           convert_numeric_str = TRUE,
                           impute = TRUE,
                           impute_method_override = NULL,
                           verbose = TRUE){
  #security checks
  stopifnot(is.data.frame(df))
  stopifnot(is.character(columns))

  diag <- analyze_table(df)
  num_diag <- summarize_numeric(df)

  diag <- diag[diag$column %in% columns, , drop = FALSE]
  cleaned_df <- df
  actions <- c()

  for(i in seq_len(nrow(diag))){
    row <- diag[i, ]
    col <- row$column
    col_data <- cleaned_df[[col]]

    #mostly missing
    if(row$percent_missing >= na_threshold){
      cleaned_df[[col]] <- NULL
      actions <- c(actions, paste0("Dropped ", col, " (", row$percent_missing, "% missing)"))
      next
    }

    #constant
    if(drop_constant && isTRUE(row$constant)){
      cleaned_df[[col]] <- NULL
      actions <- c(actions, paste0("Removed constant column: ", col))
      next
    }

    #convert
    if(convert_numeric_str && isTRUE(row$is_numeric_str)){
      cleaned_df[[col]] <- as.numeric(cleaned_df[[col]])
      actions <- c(actions, paste0("Converted ", col, " to numeric"))
    }

    #impute
    if(impute && is.numeric(col_data) && any(is.na(col_data))) {
      fill_method <- if (!is.null(impute_method_override)){
        impute_method_override
      } else{
        num_diag$recommended_fill[num_diag$column == col]
      }
      fill_value <- switch(
        fill_method,
        mean = mean(col_data, na.rm = TRUE),
        median = median(col_data, na.rm = TRUE),
        "model/robust" = median(col_data, na.rm = TRUE),
        NA
      )
      if(!is.na(fill_value)){
        col_data[is.na(col_data)] <- fill_value
        cleaned_df[[col]] <- col_data
        actions <- c(actions, paste0("Imputed ", col, " with ", fill_method, " (", round(fill_value, 2), ")"))
      }
    }
  }

  if(verbose && length(actions) > 0){
    message("Cleaning actions performed:")
    for(i in seq_along(actions)){
      message(" - ", actions[i])
    }
  }

  return(cleaned_df)
}
