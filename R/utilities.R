# =============================================================================
# UTILITIES
# File: R/utilities.R
# Part of CovariateSearcher Package
# Utility functions and helpers
# =============================================================================



#' Read Model Information from YAML
#'
#' @param model_name Character. Model name
#' @param models_folder Character. Models folder path
#' @return List with model information
#' @export
read_model_yaml <- function(model_name, models_folder = "models") {

  possible_yaml_paths <- c(
    file.path(models_folder, paste0(model_name, ".yaml")),
    file.path(models_folder, paste0(model_name, ".yml")),
    file.path(models_folder, model_name, paste0(model_name, ".yaml")),
    file.path(models_folder, model_name, paste0(model_name, ".yml")),
    paste0(model_name, ".yaml"),
    paste0(model_name, ".yml")
  )

  yaml_file <- NULL
  for (path in possible_yaml_paths) {
    if (file.exists(path)) {
      yaml_file <- path
      break
    }
  }

  if (is.null(yaml_file)) {
    return(list(
      found_yaml = FALSE,
      tags = character(0),
      based_on = character(0),
      model_type = "unknown"
    ))
  }

  tryCatch({
    yaml_content <- yaml::read_yaml(yaml_file)

    model_info <- list(
      found_yaml = TRUE,
      yaml_file = yaml_file,
      tags = character(0),
      based_on = character(0),
      model_type = if (is.null(yaml_content$model_type)) "unknown" else yaml_content$model_type
    )

    if (!is.null(yaml_content$tags)) {
      if (is.character(yaml_content$tags)) {
        model_info$tags <- yaml_content$tags
      } else if (is.list(yaml_content$tags)) {
        model_info$tags <- unlist(yaml_content$tags)
      }
    }

    if (!is.null(yaml_content$based_on)) {
      if (is.character(yaml_content$based_on)) {
        model_info$based_on <- yaml_content$based_on
      } else if (is.list(yaml_content$based_on)) {
        model_info$based_on <- unlist(yaml_content$based_on)
      }
    }

    return(model_info)

  }, error = function(e) {
    return(list(
      found_yaml = FALSE,
      error = e$message,
      tags = character(0),
      based_on = character(0),
      model_type = "unknown"
    ))
  })
}



#' Analyze Model Covariates from YAML
#'
#' @param model_name Character. Model name
#' @param models_folder Character. Models folder path
#' @param tags_list List. Available tags mapping
#' @return Character vector of detected covariates
#' @export
analyze_model_covariates_yaml <- function(model_name, models_folder = "models", tags_list = list()) {

  yaml_info <- read_model_yaml(model_name, models_folder)

  if (!yaml_info$found_yaml) {
    return(analyze_model_from_logic(model_name))
  }

  if (model_name == "run1" || (length(yaml_info$based_on) == 0 && length(yaml_info$tags) == 0)) {
    return("BASE_MODEL")
  }

  if (grepl("\\d{3}$", model_name)) {
    return("RETRY_MODEL")
  }

  model_tags <- yaml_info$tags

  if (length(model_tags) == 0) {
    return("NO_COVARIATES")
  }

  return(model_tags)
}



#' Get Model Parent from YAML
#'
#' @param model_name Character. Model name
#' @param models_folder Character. Models folder path
#' @return Character. Parent model name or NA
#' @export
get_model_parent_yaml <- function(model_name, models_folder = "models") {
  yaml_info <- read_model_yaml(model_name, models_folder)
  if (!yaml_info$found_yaml || length(yaml_info$based_on) == 0) {
    return(NA_character_)
  }
  return(yaml_info$based_on[1])
}

#' Analyze Model Using Logical Rules
#'
#' @param model_name Character. Model name
#' @return Character vector indicating model type
analyze_model_from_logic <- function(model_name) {

  if (model_name == "run1") {
    return("BASE_MODEL")
  }

  if (grepl("\\d{3}$", model_name)) {
    return("RETRY_MODEL")
  }

  return("UNKNOWN")
}


#' Clean Directory - Remove NONMEM Working Files
#'
#' @title Remove all files starting with "WK_" from all model subdirectories
#' @param models_folder Character. Path to models directory (default: "models")
#' @return Number of files deleted
#' @export
clean_dir <- function(models_folder = "models") {

  # Get all model subdirectories (run1, run2, etc.)
  model_dirs <- list.dirs(models_folder, recursive = FALSE)

  if (length(model_dirs) == 0) {
    cat("No model directories found\n")
    return(0)
  }

  total_deleted <- 0

  # Process each model directory
  for (model_dir in model_dirs) {
    wk_files <- list.files(
      path = model_dir,
      pattern = "^WK_",
      full.names = TRUE
    )

    if (length(wk_files) > 0) {
      deleted <- sum(file.remove(wk_files))
      total_deleted <- total_deleted + deleted
    }
  }

  if (total_deleted == 0) {
    cat("No WK_ files found\n")
  } else {
    cat(sprintf("Deleted %d WK_ files from %d model directories\n", total_deleted, length(model_dirs)))
  }

  return(total_deleted)
}

#' Extract Covariate Name from Tag
#'
#' @title Parse covariate name from tag string
#' @description Extracts the covariate name from a tag in the format "beta_COVARIATE_PARAMETER"
#'   (e.g., "beta_AGE_CL" → "AGE", "beta_WT_V" → "WT")
#' @param tag Character. Tag string in format "beta_COVARIATE_PARAMETER"
#' @return Character. Covariate name, or NA if parsing fails
#' @examples
#' extract_covariate_name_from_tag("beta_AGE_CL")  # Returns "AGE"
#' extract_covariate_name_from_tag("beta_WT_V")    # Returns "WT"
#' extract_covariate_name_from_tag("beta_SEX_CL")  # Returns "SEX"
#' @export
extract_covariate_name_from_tag <- function(tag) {

  if (is.na(tag) || !is.character(tag) || length(tag) != 1) {
    return(NA_character_)
  }

  # Expected format: beta_COVARIATE_PARAMETER
  # Split by underscore
  parts <- strsplit(tag, "_")[[1]]

  # Should have 3 parts: "beta", "COVARIATE", "PARAMETER"
  if (length(parts) != 3) {
    warning(sprintf("Tag format unexpected: %s (expected beta_COVARIATE_PARAMETER)", tag))
    return(NA_character_)
  }

  # Return the middle part (covariate name)
  return(parts[2])
}


#' Convert P-Value to Chi-Square ΔOFV Threshold
#'
#' @title Calculate ΔOFV threshold from p-value for likelihood ratio test
#' @description Converts a p-value to the corresponding chi-square ΔOFV threshold
#'   for model comparison in stepwise covariate modeling. ΔOFV follows a chi-square
#'   distribution with degrees of freedom equal to the difference in parameters.
#'
#'   For covariate modeling:
#'   - Continuous covariates: df = 1 (one parameter added)
#'   - Categorical covariates: df = number of levels - 1
#'
#' @param p_value Numeric. Significance level (e.g., 0.05, 0.01)
#' @param df Integer. Degrees of freedom for chi-square test (default: 1)
#' @return Numeric. Chi-square critical value (ΔOFV threshold)
#' @examples
#' # Standard forward selection (p = 0.05, df = 1)
#' pvalue_to_threshold(0.05, df = 1)  # Returns 3.84
#'
#' # Standard backward elimination (p = 0.01, df = 1)
#' pvalue_to_threshold(0.01, df = 1)  # Returns 6.63
#'
#' # Categorical covariate with 3 levels (df = 2)
#' pvalue_to_threshold(0.05, df = 2)  # Returns 5.99
#' @export
pvalue_to_threshold <- function(p_value, df = 1) {

  # Input validation
  if (!is.numeric(p_value) || length(p_value) != 1) {
    stop("p_value must be a single numeric value")
  }
  if (p_value <= 0 || p_value >= 1) {
    stop("p_value must be between 0 and 1")
  }
  if (!is.numeric(df) || length(df) != 1 || df < 1) {
    stop("df must be a positive integer")
  }

  # Convert p-value to chi-square critical value
  threshold <- qchisq(1 - p_value, df = df)

  return(threshold)
}


#' Calculate Degrees of Freedom for Covariate
#'
#' @title Determine degrees of freedom based on covariate type
#' @description Calculates the degrees of freedom for a covariate based on whether
#'   it is continuous or categorical. For categorical covariates, df equals the
#'   number of levels minus 1.
#' @param covariate_name Character. Name of the covariate
#' @param covariate_search Data frame. Covariate search configuration
#' @return Integer. Degrees of freedom (1 for continuous, n_levels-1 for categorical)
#' @export
calculate_covariate_df <- function(covariate_name, covariate_search) {

  # Find covariate in search table
  cov_row <- covariate_search[covariate_search$COVARIATE == covariate_name, ]

  if (nrow(cov_row) == 0) {
    warning(sprintf("Covariate %s not found in covariate_search, defaulting to df=1",
                    covariate_name))
    return(1L)
  }

  # Get first matching row (in case of multiple parameters)
  cov_row <- cov_row[1, ]

  # Check if categorical
  if (cov_row$STATUS == "cat") {
    # Parse levels (format: "0;1;2")
    levels_str <- cov_row$LEVELS
    if (is.na(levels_str) || levels_str == "") {
      warning(sprintf("Categorical covariate %s has no levels specified, defaulting to df=1",
                      covariate_name))
      return(1L)
    }

    # Count levels
    levels_vec <- strsplit(levels_str, ";")[[1]]
    n_levels <- length(levels_vec)

    # df = number of levels - 1
    df <- n_levels - 1L

    if (df < 1) {
      warning(sprintf("Categorical covariate %s has only 1 level, defaulting to df=1", covariate_name))
      return(1L)
    }

    return(df)

  } else {
    # Continuous covariate: df = 1
    return(1L)
  }
}

