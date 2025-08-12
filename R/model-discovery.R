# =============================================================================
# MODEL DISCOVERY
# File: R/model-discovery.R
# Part of CovariateSearcher Package
# Existing model discovery and analysis
# =============================================================================



#' Discover Existing Models
#'
#' @title Discover existing models and set up relationships
#' @description Scans the models folder and catalogs existing models
#' @param search_state List containing search state
#' @return Updated search_state with discovered models
#' @export
discover_existing_models <- function(search_state) {

  cat("Discovering existing models...\n")

  # Clear existing database
  search_state <- initialize_search_database_core(search_state)

  # Get all model files in models folder
  model_files <- list.files(search_state$models_folder, pattern = "^run\\d+\\.(ctl|mod)$",
                            full.names = FALSE)
  model_names <- gsub("\\.(ctl|mod)$", "", model_files)
  model_names <- unique(model_names)
  model_names <- model_names[order(as.numeric(gsub("run", "", model_names)))]

  if (length(model_names) == 0) {
    cat("No existing models found.\n")
    search_state$discovered_models <- character(0)
    return(search_state)
  }

  cat("Found", length(model_names), "existing models:",
      paste(model_names, collapse = ", "), "\n")

  search_state$discovered_models <- model_names

  # Add each model to database
  for (model_name in model_names) {

    # Simple parent relationship logic
    if (model_name == search_state$base_model) {
      parent_model <- NA_character_
      step_desc <- "Base Model"
      phase <- "base"
      action <- "base"
      step_num <- 0L
    } else {
      # Try to get parent from BBR
      tryCatch({
        model_path <- file.path(search_state$models_folder, model_name)
        mod <- bbr::read_model(model_path)

        if (!is.null(mod$based_on) && length(mod$based_on) > 0) {
          parent_model <- mod$based_on[1]
          notes <- tryCatch({
            if (length(mod$notes) > 0) mod$notes[1] else ""
          }, error = function(e) "")

          if (notes != "" && grepl("^[+-]", notes)) {
            # Parse notes like "+ WT_CL" or "- RACE_CL"
            action <- if (startsWith(notes, "+")) "Add" else "Remove"
            covariate <- gsub("^[+-]\\s*", "", notes)
            step_desc <- paste(action, covariate)

          } else {
            step_desc <- "Added Covariate"  # fallback for models without notes


          }
          phase <- "individual_testing"
          action <- "add_single_covariate"
        } else {
          # Fallback logic
          parent_model <- search_state$base_model
          step_desc <- "Manual/External"
          phase <- "manual"
          action <- "manual_modification"
        }



      }, error = function(e) {
        # If BBR fails, use simple logic
        parent_model <- search_state$base_model
        step_desc <- "Manual/External"
        phase <- "manual"
        action <- "manual_modification"

      })
      step_num <- if (model_name == search_state$base_model) {
        0L  # Base model
      } else if (!is.na(parent_model) && parent_model == search_state$base_model) {
        1L  # Direct children of base model (univariate tests)
      } else {
        1L  # Default to step 1 for discovered models
      }
    }

    # Get model information - using functions from other files
    model_path <- file.path(search_state$models_folder, model_name)
    status <- get_model_status_from_files(model_path)
    ofv <- if (status == "completed") get_model_ofv_from_files(search_state, model_name) else NA
    covariates <- get_model_covariates_from_files(search_state, model_name)
    model_tags <- tryCatch({
      mod <- bbr::read_model(model_path)
      mod$tags
    }, error = function(e) {
      character(0)
    })

    # Add to database with retry tracking columns
    new_row <- data.frame(
      model_name = model_name,
      step_description = step_desc,
      phase = phase,
      step_number = step_num,
      parent_model = parent_model,
      covariate_tested = if (exists("covariate")) covariate else paste(covariates, collapse = ";"),
      action = action,
      ofv = ofv,
      delta_ofv = NA_real_,
      rse_max = NA_real_,
      status = status,
      tags = I(list(model_tags)),
      submission_time = as.POSIXct(NA),
      completion_time = if (status == "completed") Sys.time() else as.POSIXct(NA),
      retry_attempt = ifelse(grepl("\\d{3}$", model_name), 1L, NA_integer_),
      original_model = NA_character_,
      estimation_issue = NA_character_,
      excluded_from_step = FALSE,
      stringsAsFactors = FALSE
    )

    search_state$search_database <- dplyr::bind_rows(search_state$search_database, new_row)
  }

  cat("Added", length(model_names), "models to search database.\n")
  return(search_state)
}



#' Discover Existing Models with YAML Metadata
#'
#' @param models_folder Character. Path to models directory
#' @return Tibble with discovered models and their metadata
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

  # Create database entries with YAML metadata
  model_entries <- list()

  for (i in seq_along(all_models)) {
    model_name <- all_models[i]
    yaml_path <- file.path(models_folder, paste0(model_name, ".yaml"))

    # Default values
    parent_model <- NA_character_
    covariate_tested <- NA_character_
    model_tags <- character(0)
    step_description <- "Existing Model"
    phase <- "discovered"

    # Read YAML metadata if available
    if (file.exists(yaml_path)) {
      tryCatch({
        yaml_data <- yaml::read_yaml(yaml_path)

        # Extract parent model
        if (!is.null(yaml_data$based_on)) {
          parent_model <- yaml_data$based_on
        }

        # Extract covariate information from tags
        if (!is.null(yaml_data$tags) && length(yaml_data$tags) > 0) {
          model_tags <- yaml_data$tags
          if (length(model_tags) > 0) {
            covariate_tested <- model_tags[1]  # Use first tag as primary covariate
            step_description <- paste("Add", covariate_tested)
            phase <- "forward_selection"
          }
        }

        # Special handling for base model
        if (model_name == "run1" || is.na(parent_model)) {
          step_description <- "Base Model"
          covariate_tested <- "Base Model"
          phase <- "base"
        }

        # Special handling for retry models (model numbers > 2000)
        model_num <- as.numeric(gsub("run", "", model_name))
        if (!is.na(model_num) && model_num > 2000) {
          step_description <- "Retry Model"
          covariate_tested <- "Retry Model"
          phase <- "retry"
        }

      }, error = function(e) {
        warning("Could not read YAML for ", model_name, ": ", e$message)
      })
    }

    # Create model entry
    model_entry <- tibble::tibble(
      model_name = model_name,
      step_description = step_description,
      phase = phase,
      step_number = if (phase == "base") 0L else 1L,
      parent_model = parent_model,
      covariate_tested = covariate_tested,
      action = if (phase == "base") "base_model" else "add_single_covariate",
      ofv = NA_real_,
      delta_ofv = NA_real_,
      rse_max = NA_real_,
      status = "unknown",
      tags = list(model_tags),
      submission_time = as.POSIXct(NA),
      completion_time = as.POSIXct(NA),
      retry_attempt = if (phase == "retry") 1L else 0L,
      original_model = if (phase == "retry") parent_model else NA_character_,
      estimation_issue = NA_character_,
      excluded_from_step = FALSE
    )

    model_entries[[i]] <- model_entry
  }

  # Combine all entries
  result <- dplyr::bind_rows(model_entries)
  return(result)
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

