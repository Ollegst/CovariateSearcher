# =============================================================================
# DATABASE MANAGEMENT
# File: R/database-management.R
# Part of CovariateSearcher Package
# Search database operations and management
# =============================================================================



#' Initialize Search Database
#'
#' @title Initialize the search database with retry tracking columns
#' @description Creates empty search database with all required columns
#' @param search_state List containing search state
#' @return List with updated search_state
#' @export
initialize_search_database_core <- function(search_state) {
  search_state$search_database <- data.frame(
    model_name = character(),
    step_description = character(),
    phase = character(),
    step_number = integer(),
    parent_model = character(),
    covariate_tested = character(),
    action = character(),
    ofv = numeric(),
    delta_ofv = numeric(),
    rse_max = numeric(),
    status = character(),
    tags = I(list()),
    submission_time = as.POSIXct(character()),
    completion_time = as.POSIXct(character()),
    retry_attempt = integer(),
    original_model = character(),
    estimation_issue = character(),
    excluded_from_step = logical(),
    stringsAsFactors = FALSE
  )
  cat("Search database initialized with retry tracking columns\n")
  return(list(search_state = search_state))
}



#' Get Model Status
#'
#' @title Get current status of a model from database
#' @description Checks model completion status from search database
#' @param search_state List containing search state
#' @param model_name Character. Model name to check
#' @return Character. Status: "completed", "failed", "in_progress", etc.
#' @export
get_model_status <- function(search_state, model_name) {

  model_row <- search_state$search_database[
    search_state$search_database$model_name == model_name, ]

  if (nrow(model_row) == 0) {
    return("not_found")
  }

  return(model_row$status[1])
}



#' Get Model OFV
#'
#' @title Extract OFV from completed model database entry
#' @description Extracts the objective function value from search database
#' @param search_state List containing search state
#' @param model_name Character. Model name
#' @return Numeric. OFV value or NA if not available
#' @export
get_model_ofv <- function(search_state, model_name) {

  model_row <- search_state$search_database[
    search_state$search_database$model_name == model_name, ]

  if (nrow(model_row) == 0) {
    return(NA_real_)
  }

  return(model_row$ofv[1])
}



#' Get Model Covariates
#'
#' @title Get list of covariates in a model from database
#' @description Returns covariates currently in the specified model
#' @param search_state List containing search state
#' @param model_name Character. Model name
#' @return Character vector. Covariate names in the model
#' @export
get_model_covariates <- function(search_state, model_name) {

  model_row <- search_state$search_database[
    search_state$search_database$model_name == model_name, ]

  if (nrow(model_row) == 0) {
    return(character(0))
  }

  # For now, return the single covariate tested
  # In full implementation, this would track cumulative covariates
  covariate <- model_row$covariate_tested[1]
  if (is.na(covariate)) {
    return(character(0))
  }

  return(covariate)
}



#' Get Model Covariates from Database
#'
#' @title Get cumulative covariates by tracing model history
#' @description Traces model hierarchy to get all covariates in a model
#' @param search_state List containing search state
#' @param model_name Character. Model name
#' @return Character vector. All covariate names in the model
#' @export
get_model_covariates_from_db <- function(search_state, model_name) {
  # Trace back through model hierarchy to collect all covariates
  current_model <- model_name
  covariates <- character(0)

  while (!is.na(current_model) && current_model != "" && current_model != search_state$base_model) {
    model_row <- search_state$search_database[
      search_state$search_database$model_name == current_model, ]

    if (nrow(model_row) == 0) break

    cov_tested <- model_row$covariate_tested[1]
    if (!is.na(cov_tested) && cov_tested != "" && cov_tested != "Base Model") {
      covariates <- c(cov_tested, covariates)
    }

    current_model <- model_row$parent_model[1]
  }

  return(unique(covariates))
}



#' Update Model Counter
#'
#' @title Update model counter excluding retry models
#' @description Sets the model counter based on existing models
#' @param search_state List containing search state
#' @return List with updated search_state
#' @export
update_model_counter <- function(search_state) {
  if (nrow(search_state$search_database) > 0) {
    all_model_names <- search_state$search_database$model_name

    # Filter out retry models (ending with 3 digits)
    main_models <- all_model_names[!grepl("\\d{3}$", all_model_names)]

    if (length(main_models) > 0) {
      model_numbers <- as.numeric(gsub("run", "", main_models))
      sorted_numbers <- sort(model_numbers, na.last = TRUE)
      search_state$model_counter <- utils::tail(sorted_numbers, 1)

      cat("Model counter set to:", search_state$model_counter,
          "(last in sequence, excluding retry models)\n")

      if (length(all_model_names) > length(main_models)) {
        retry_models <- setdiff(all_model_names, main_models)
        cat("Retry models excluded from counter:",
            paste(retry_models, collapse = ", "), "\n")
      }
    } else {
      base_number <- as.numeric(gsub("run", "", search_state$base_model))
      search_state$model_counter <- base_number
      cat("Model counter set to base model:", search_state$model_counter, "\n")
    }
  } else {
    base_number <- as.numeric(gsub("run", "", search_state$base_model))
    search_state$model_counter <- base_number
    cat("Model counter set to base model:", search_state$model_counter, "\n")
  }

  return(list(search_state = search_state))
}



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

  # Create summary using the actual database fields
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
      param_display = dplyr::case_when(
        covariate_tested == "Base Model" ~ "2",
        covariate_tested == "Retry Model" ~ "3",
        !is.na(covariate_tested) ~ "3",
        TRUE ~ "NA"
      )
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
