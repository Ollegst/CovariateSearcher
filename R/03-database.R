#' @title Database Functions
#' @description Search database operations and management
#' @name database-functions
NULL

#' Initialize Search Database
#'
#' @param models_folder Character. Path to models directory
#' @return List with database and counter
#' @export
initialize_search_database <- function(models_folder) {

  cat(sprintf("🔍 Initializing database with models folder: %s\n", models_folder))

  # Create empty database with required columns
  search_database <- tibble::tibble(
    model_name = character(0),
    step_description = character(0),
    phase = character(0),
    step_number = integer(0),
    parent_model = character(0),
    covariate_tested = character(0),
    action = character(0),
    ofv = numeric(0),
    delta_ofv = numeric(0),
    rse_max = numeric(0),
    status = character(0),
    tags = list(),
    submission_time = as.POSIXct(character(0)),
    completion_time = as.POSIXct(character(0)),
    retry_attempt = integer(0),
    original_model = character(0),
    estimation_issue = character(0),
    excluded_from_step = logical(0)
  )

  # Discover existing models if folder exists
  if (dir.exists(models_folder)) {
    cat(sprintf("✅ Models folder '%s' found\n", models_folder))
    existing_models <- discover_existing_models_simple(models_folder)
    if (nrow(existing_models) > 0) {
      search_database <- existing_models
      cat(sprintf("📊 Discovered %d existing models\n", nrow(existing_models)))
    }
  } else {
    cat(sprintf("⚠️  Models folder '%s' not found\n", models_folder))
  }

  # Set counter
  if (nrow(search_database) > 0) {
    model_numbers <- as.numeric(gsub("run", "", search_database$model_name))
    model_numbers <- model_numbers[!is.na(model_numbers) & model_numbers < 1000]
    counter <- if (length(model_numbers) > 0) max(model_numbers) else 1
  } else {
    counter <- 1
  }

  cat(sprintf("🔢 Model counter set to: %d\n", counter))

  return(list(
    database = search_database,
    counter = counter
  ))
}

#' Discover Existing Models Simple
#'
#' @param models_folder Character. Path to models directory
#' @return Tibble with discovered models
discover_existing_models_simple <- function(models_folder) {

  # Find model files
  ctl_files <- list.files(models_folder, pattern = "^run\\d+\\.(ctl|mod)$")
  yaml_files <- list.files(models_folder, pattern = "^run\\d+\\.yaml$")
  
  # Get model names
  ctl_models <- gsub("\\.(ctl|mod)$", "", ctl_files)
  yaml_models <- gsub("\\.yaml$", "", yaml_files)
  
  # Combine and get unique models
  all_models <- unique(c(ctl_models, yaml_models))
  all_models <- all_models[grepl("^run\\d+$", all_models)]
  
  if (length(all_models) == 0) {
    return(tibble::tibble(
      model_name = character(0), step_description = character(0),
      phase = character(0), step_number = integer(0),
      parent_model = character(0), covariate_tested = character(0),
      action = character(0), ofv = numeric(0), delta_ofv = numeric(0),
      rse_max = numeric(0), status = character(0), tags = list(),
      submission_time = as.POSIXct(character(0)),
      completion_time = as.POSIXct(character(0)),
      retry_attempt = integer(0), original_model = character(0),
      estimation_issue = character(0), excluded_from_step = logical(0)
    ))
  }
  
  # Sort models
  model_numbers <- as.numeric(gsub("run", "", all_models))
  sorted_indices <- order(model_numbers)
  all_models <- all_models[sorted_indices]
  
  # Create database entries
  tibble::tibble(
    model_name = all_models,
    step_description = "Existing Model",
    phase = "discovered",
    step_number = 0L,
    parent_model = NA_character_,
    covariate_tested = NA_character_,
    action = "existing",
    ofv = NA_real_,
    delta_ofv = NA_real_,
    rse_max = NA_real_,
    status = "unknown",
    tags = rep(list(character(0)), length(all_models)),
    submission_time = as.POSIXct(rep(NA, length(all_models))),
    completion_time = as.POSIXct(rep(NA, length(all_models))),
    retry_attempt = rep(0L, length(all_models)),
    original_model = rep(NA_character_, length(all_models)),
    estimation_issue = rep(NA_character_, length(all_models)),
    excluded_from_step = rep(FALSE, length(all_models))
  )
}

#' Create Model Summary Table
#'
#' @param search_state List. Current search state
#' @return Data frame with model summary
#' @export
create_comprehensive_table <- function(search_state) {

  db <- search_state$search_database

  # Handle NULL database
  if (is.null(db) || nrow(db) == 0) {
    return(tibble::tibble(
      model_name = character(0), parent_display = character(0),
      model_type = character(0), changes = character(0),
      status = character(0), ofv_display = character(0),
      delta_display = character(0), param_display = character(0)
    ))
  }

  # Create summary
  db %>%
    dplyr::mutate(
      parent_display = ifelse(is.na(parent_model) | parent_model == "", "-", parent_model),
      model_type = "Development",
      changes = dplyr::case_when(
        covariate_tested == "Base Model" ~ "Base model",
        covariate_tested == "Retry Model" ~ "+ Retry",
        is.na(covariate_tested) | covariate_tested == "Unknown" ~ "Unknown",
        TRUE ~ paste("+", covariate_tested)
      ),
      status_display = "not_submitted",
      ofv_display = "pending",
      delta_display = "-",
      param_display = ifelse(covariate_tested == "Base Model", "2", "3")
    ) %>%
    dplyr::select(model_name, parent_display, model_type, changes,
                  status = status_display, ofv_display, delta_display, param_display)
}

#' View Model Summary Table
#'
#' @param search_state List. Current search state
#' @export
view_comprehensive_table <- function(search_state) {
  comprehensive <- create_comprehensive_table(search_state)
  if (nrow(comprehensive) == 0) {
    cat("📊 No models found\n")
    return(invisible(NULL))
  }
  cat(sprintf("📊 Model Table (%d models)\n", nrow(comprehensive)))
  print(comprehensive)
  return(invisible(comprehensive))
}
