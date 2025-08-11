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

