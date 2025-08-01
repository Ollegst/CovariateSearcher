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
