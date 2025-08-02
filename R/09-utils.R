#' @title Utility Functions
#' @description Utility functions and helpers
#' @name utility-functions
NULL

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

#' Update Database with YAML Analysis
#'
#' @param search_state List. Current search state
#' @return Updated search state with analyzed covariates
#' @export
analyze_existing_models_yaml <- function(search_state) {

  cat("[CHECK] Analyzing existing models using YAML files...\n")

  existing_models <- search_state$search_database[
    search_state$search_database$phase == "discovered", ]

  if (nrow(existing_models) == 0) {
    cat("  No existing models to analyze\n")
    return(search_state)
  }

  for (i in 1:nrow(existing_models)) {
    model_name <- existing_models$model_name[i]
    cat(sprintf("  [LIST] Analyzing %s... ", model_name))

    yaml_info <- read_model_yaml(model_name, search_state$models_folder)
    covariates <- analyze_model_covariates_yaml(model_name, search_state$models_folder, search_state$tags)
    yaml_parent <- get_model_parent_yaml(model_name, search_state$models_folder)

    db_idx <- which(search_state$search_database$model_name == model_name)

    if (length(db_idx) > 0) {

      if ("BASE_MODEL" %in% covariates) {
        search_state$search_database$covariate_tested[db_idx] <- "Base Model"
        search_state$search_database$step_description[db_idx] <- "Base Model"
        search_state$search_database$phase[db_idx] <- "base"
        search_state$search_database$parent_model[db_idx] <- NA_character_
        cat("Base Model\n")

      } else if ("RETRY_MODEL" %in% covariates) {
        search_state$search_database$covariate_tested[db_idx] <- "Retry Model"
        search_state$search_database$step_description[db_idx] <- "Retry Model"
        search_state$search_database$phase[db_idx] <- "retry"
        search_state$search_database$retry_attempt[db_idx] <- 1L

        # Fixed retry parent logic
        if (!is.na(yaml_parent)) {
          original_parent <- get_model_parent_yaml(yaml_parent, search_state$models_folder)
          if (!is.na(original_parent)) {
            search_state$search_database$parent_model[db_idx] <- original_parent
          } else {
            search_state$search_database$parent_model[db_idx] <- "run1"
          }
        } else {
          search_state$search_database$parent_model[db_idx] <- "run1"
        }
        cat("Retry Model\n")

      } else if (length(covariates) > 0 && !"UNKNOWN" %in% covariates) {
        cov_string <- paste(covariates, collapse = ", ")
        search_state$search_database$covariate_tested[db_idx] <- cov_string
        search_state$search_database$step_description[db_idx] <- sprintf("Has: %s", cov_string)
        search_state$search_database$phase[db_idx] <- "existing_with_covariates"

        if (!is.na(yaml_parent)) {
          search_state$search_database$parent_model[db_idx] <- yaml_parent
        }

        cat(sprintf("Found: %s\n", cov_string))
      } else {
        search_state$search_database$covariate_tested[db_idx] <- "Unknown"
        search_state$search_database$step_description[db_idx] <- "Unknown"
        cat("Unknown\n")
      }
    }
  }

  cat("[OK] YAML analysis complete\n")
  return(search_state)
}
