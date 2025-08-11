# =============================================================================
# DATABASE MANAGEMENT
# File: R/database-management.R
# Part of CovariateSearcher Package
# Search database operations and management
# =============================================================================


#' Load Existing Covariate Search
#'
#' @title Load existing models and recreate search state
#' @description Discovers existing models in the models folder and recreates
#'   the search database. Use this to continue work on an existing project.
#' @param base_model_path Character. Path to base model (e.g., "run1")
#' @param data_file_path Character. Path to NONMEM dataset CSV
#' @param covariate_search_path Character. Path to covariate search CSV
#' @param models_folder Character. Directory containing models (default: "models")
#' @param timecol Character. Time column name (default: "TIME")
#' @param idcol Character. ID column name (default: "ID")
#' @param threads Integer. Number of threads for execution (default: 60)
#' @return List containing search state with discovered models
#' @export
load_existing_search <- function(base_model_path,
                                 data_file_path,
                                 covariate_search_path,
                                 models_folder = "models",
                                 timecol = "TIME",
                                 idcol = "ID",
                                 threads = 60) {

  cat("📂 Loading EXISTING Covariate Search (Discovery Mode)...\n")

  # First check if models folder exists and has models
  if (!dir.exists(models_folder)) {
    stop("Models folder '", models_folder, "' not found. Use initialize_new_search() instead.")
  }

  model_files <- list.files(models_folder, pattern = "^run\\d+\\.(ctl|mod|yaml)$")
  if (length(model_files) <= 2) {  # Only base model files
    cat("⚠️  Few/no existing models found. Consider using initialize_new_search()\n")
  }

  cat(sprintf("🔍 Found %d model files in %s\n", length(model_files), models_folder))

  # Call with discovery enabled, but with error handling
  tryCatch({
    search_state <- initialize_covariate_search(
      base_model_path = base_model_path,
      data_file_path = data_file_path,
      covariate_search_path = covariate_search_path,
      models_folder = models_folder,
      timecol = timecol,
      idcol = idcol,
      threads = threads,
      discover_existing = TRUE
    )

    cat("✅ EXISTING search loaded successfully\n")
    cat(sprintf("📊 Database: %d models discovered, Counter: %d\n",
                nrow(search_state$search_database), search_state$model_counter))

    # Show discovered models
    if (nrow(search_state$search_database) > 0) {
      cat("📋 Discovered models:\n")
      discovered <- search_state$search_database[, c("model_name", "covariate_tested")]
      for (i in 1:min(5, nrow(discovered))) {
        cat(sprintf("  - %s: %s\n", discovered$model_name[i],
                    ifelse(is.na(discovered$covariate_tested[i]), "Base", discovered$covariate_tested[i])))
      }
      if (nrow(discovered) > 5) {
        cat(sprintf("  ... and %d more models\n", nrow(discovered) - 5))
      }
    }

    return(search_state)

  }, error = function(e) {
    cat("❌ Discovery failed:", e$message, "\n")
    cat("💡 Try using initialize_new_search() instead, or check your models folder\n")
    stop("Model discovery failed. Use initialize_new_search() for a fresh start.")
  })
}



#' Save Search State to File
#'
#' @title Save current search state for later loading
#' @description Saves the complete search state to an RDS file for backup
#'   or to resume work later.
#' @param search_state List. Current search state
#' @param filename Character. Filename to save to (default: "search_state_backup.rds")
#' @return Invisible TRUE if successful
#' @export
save_search_state <- function(search_state, filename = "search_state_backup.rds") {
  saveRDS(search_state, file = filename)
  cat(sprintf("💾 Search state saved to %s\n", filename))
  cat(sprintf("📊 Saved: %d models, counter: %d\n",
              nrow(search_state$search_database), search_state$model_counter))
  return(invisible(TRUE))
}

#' Ensure Base Model in Database
#' @param search_state List containing search state
#' @return Updated search_state with base model added if missing
ensure_base_model_in_database <- function(search_state) {
  # Check if base model already exists in database
  if (search_state$base_model %in% search_state$search_database$model_name) {
    cat(sprintf("Base model '%s' already in database\n", search_state$base_model))
    return(search_state)
  }

  # Add base model with minimal required information
  base_row <- data.frame(
    model_name = search_state$base_model,
    step_description = "Base Model",
    phase = "base",
    step_number = 0L,
    parent_model = NA_character_,
    covariate_tested = "Base Model",
    action = "base_model",
    ofv = NA_real_,  # Will be updated when needed
    delta_ofv = NA_real_,
    rse_max = NA_real_,
    status = "unknown",  # Can be updated later
    tags = I(list(character(0))),
    submission_time = as.POSIXct(NA),
    completion_time = as.POSIXct(NA),
    retry_attempt = 0L,
    original_model = NA_character_,
    estimation_issue = NA_character_,
    excluded_from_step = FALSE,
    stringsAsFactors = FALSE
  )

  search_state$search_database <- rbind(search_state$search_database, base_row)
  cat(sprintf("✅ Base model '%s' added to database\n", search_state$base_model))

  return(search_state)
}

#' Load Search State from File
#'
#' @title Load previously saved search state
#' @description Loads a complete search state from an RDS file.
#' @param filename Character. Filename to load from
#' @return List containing loaded search state
#' @export
load_search_state <- function(filename) {
  if (!file.exists(filename)) {
    stop("File '", filename, "' not found")
  }

  search_state <- readRDS(filename)
  cat(sprintf("📁 Search state loaded from %s\n", filename))
  cat(sprintf("📊 Loaded: %d models, counter: %d\n",
              nrow(search_state$search_database), search_state$model_counter))

  return(search_state)
}


#' Initialize Search Database
#'
#' @title Initialize the search database with retry tracking columns
#' @description Creates empty search database with all required columns
#' @param search_state List containing search state
#' @return Updated search_state (initialization function - returns directly)
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
  return(search_state)
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



#' Get Model OFV from Database
#'
#' @title Extract OFV from completed model database entry
#' @description Fast lookup of cached OFV value from search database
#' @param search_state Search state object
#' @param model_name Model name
#' @return Numeric OFV or NA if not found
#' @note For authoritative values, use get_model_ofv_from_files()
#' @export
get_model_ofv_from_database  <- function(search_state, model_name) {

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
#' @return Updated search_state (initialization function - returns directly)
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

  return(search_state)
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
