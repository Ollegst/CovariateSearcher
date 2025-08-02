# =============================================================================
# CORE FUNCTIONS - MAIN INITIALIZATION AND MODEL MANAGEMENT
# File: R/01-core-functions.R
# Part of CovariateSearcher Package
# Enhanced with functions from 04-models.R (DUPLICATES REMOVED)
# =============================================================================

#' Initialize Covariate Search
#'
#' @title Initialize covariate search state with validation and setup
#' @description Main initialization function that sets up the search state,
#'   loads data files, validates configuration, and discovers existing models.
#' @param base_model_path Character. Path to base model (e.g., "run1")
#' @param data_file_path Character. Path to NONMEM dataset CSV
#' @param covariate_search_path Character. Path to covariate search CSV
#' @param models_folder Character. Directory containing models (default: "models")
#' @param timecol Character. Time column name (default: "TIME")
#' @param idcol Character. ID column name (default: "ID")
#' @param threads Integer. Number of threads for execution (default: 60)
#' @param discover_existing Logical. Discover existing models (default: TRUE)
#' @return List containing complete search state configuration
#' @export
initialize_covariate_search <- function(base_model_path,
                                        data_file_path,
                                        covariate_search_path,
                                        models_folder = "models",
                                        timecol = "TIME",
                                        idcol = "ID",
                                        threads = 60,
                                        discover_existing = TRUE) {

  cat("Initializing CovariateSearcher (Core Module)...\n")

  # Initialize search state structure
  search_state <- list(
    base_model = base_model_path,
    data_file = NULL,
    covariate_search = NULL,
    models_folder = models_folder,
    timecol = timecol,
    idcol = idcol,
    threads = threads,
    tags = NULL,
    model_counter = NULL,
    search_database = NULL,
    search_config = NULL,
    discovered_models = NULL
  )

  # Load data files
  cat("Loading data files...\n")
  search_state$data_file <- readr::read_csv(data_file_path, show_col_types = FALSE)
  search_state$covariate_search <- readr::read_csv(covariate_search_path, show_col_types = FALSE)

  # Add cov_to_test column if missing
  if (!"cov_to_test" %in% names(search_state$covariate_search)) {
    cat("Adding cov_to_test column...\n")
    search_state$covariate_search$cov_to_test <- paste0("beta_",
                                                        search_state$covariate_search$COVARIATE, "_",
                                                        search_state$covariate_search$PARAMETER)
  }

  # Load tags and run validations
  search_state <- load_tags(search_state)
  search_state <- validate_setup(search_state)

  # Initialize search database and configuration
  search_state <- initialize_search_database_core(search_state)
  search_state <- initialize_search_config(search_state)

  # Discover existing models if requested
  if (discover_existing) {
    search_state <- discover_existing_models(search_state)
  }

  # Set model counter based on existing models
  search_state <- update_model_counter(search_state)

  cat("CovariateSearcher (Core) initialized successfully!\n")
  return(search_state)
}

#' Load Tags from YAML File
#'
#' @title Load covariate tags from YAML configuration
#' @description Loads the tags.yaml file containing covariate mappings
#' @param search_state List containing search state
#' @return Updated search_state with tags loaded
#' @export
load_tags <- function(search_state) {
  tags_file <- file.path("data", "spec", "tags.yaml")
  if (file.exists(tags_file)) {
    search_state$tags <- yaml::read_yaml(tags_file)
    cat("Tags loaded from:", tags_file, "\n")
  } else {
    stop("tags.yaml file not found at: ", tags_file)
  }
  return(search_state)
}

#' Validate Setup Configuration
#'
#' @title Validate covariate search setup and data consistency
#' @description Performs validation checks on data files and configuration
#' @param search_state List containing search state
#' @return Updated search_state (unchanged if validation passes)
#' @export
validate_setup <- function(search_state) {
  cat("Running validation checks...\n")

  # Check required columns in covariate_search
  required_cols <- c("PARAMETER", "COVARIATE", "STATUS", "FORMULA",
                     "LEVELS", "REFERENCE", "TIME_DEPENDENT")
  missing_cols <- setdiff(required_cols, names(search_state$covariate_search))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in covariate_search: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check that covariates exist in dataset
  missing_covs <- setdiff(search_state$covariate_search$COVARIATE,
                          names(search_state$data_file))
  if (length(missing_covs) > 0) {
    stop("Covariates not found in dataset: ", paste(missing_covs, collapse = ", "))
  }

  cat("All validation checks passed!\n")
  return(search_state)
}

#' Initialize Search Database
#'
#' @title Initialize the search database with retry tracking columns
#' @description Creates empty search database with all required columns
#' @param search_state List containing search state
#' @return Updated search_state with initialized database
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

#' Initialize Search Configuration
#'
#' @title Initialize search configuration parameters
#' @description Sets up default configuration for SCM workflow
#' @param search_state List containing search state
#' @return Updated search_state with initialized configuration
#' @export
initialize_search_config <- function(search_state) {
  search_state$search_config <- list(
    forward_ofv_threshold = 3.84,
    backward_ofv_threshold = 6.63,
    max_rse_threshold = 50,
    max_forward_steps = 10,
    max_backward_steps = 10,
    timeout_minutes = 60,
    threads = search_state$threads,
    current_phase = "initialization",
    current_step = 0
  )
  cat("Search configuration initialized\n")
  return(search_state)
}

#' Add Covariate to Model
#'
#' @title Creates a new model by adding a single covariate to an existing base model
#' @description This is the core function for stepwise covariate modeling.
#'   Enhanced version with detailed logging and error handling.
#' @param search_state List. Current search state from initialize_covariate_search()
#' @param base_model_id Character. Base model identifier (e.g., "run1")
#' @param covariate_tag Character. Covariate tag to add (e.g., "cov_cl_wt")
#' @return Updated search state with new model added to database
#' @export
add_covariate_to_model <- function(search_state, base_model_id, covariate_tag) {
  cat(sprintf("[+] Adding covariate %s to model %s\n", covariate_tag, base_model_id))

  # Get covariate information
  if (!covariate_tag %in% names(search_state$tags)) {
    stop("Covariate tag not found: ", covariate_tag)
  }

  covariate_name <- search_state$tags[[covariate_tag]]
  cat(sprintf("  Covariate: %s\n", covariate_name))

  # Calculate new model name (don't increment counter yet)
  new_model_name <- sprintf("run%d", search_state$model_counter + 1)
  cat(sprintf("  New model: %s\n", new_model_name))

  # Find covariate definition in search database
  matching_cov <- search_state$covariate_search[
    grepl(paste0("_", covariate_name, "$"), search_state$covariate_search$cov_to_test), ]

  if (nrow(matching_cov) == 0) {
    warning("No matching covariate definition found for: ", covariate_name)
    cov_definition <- "Unknown"
  } else {
    cov_definition <- matching_cov$cov_to_test[1]
  }

  tryCatch({
    # Step 1: Create BBR model (creates physical file)
    cat("  Creating BBR model...\n")
    parent_path <- file.path(search_state$models_folder, base_model_id)

    new_mod <- bbr::copy_model_from(
      .parent_mod = bbr::read_model(parent_path),
      .new_model = new_model_name,
      .inherit_tags = TRUE,
      .overwrite = TRUE
    ) %>%
      bbr::add_tags(.tags = search_state$tags[[covariate_tag]]) %>%
      bbr::add_notes(.notes = paste0("+ ", search_state$tags[[covariate_tag]]))

    cat("  [OK] BBR model created\n")

    # Step 2: INCREMENT COUNTER NOW (physical file exists)
    search_state$model_counter <<- search_state$model_counter + 1
    cat(sprintf("  [OK] Model counter updated to: %d\n", search_state$model_counter))

    # Step 3: Try to add covariate (may fail, but file already exists)
    cat("  Adding covariate to model file...\n")

    # Get covariate information for model modification
    cov_info <- matching_cov[1, ]
    cov_name <- cov_info$COVARIATE  # e.g., "WT"
    param_name <- cov_info$PARAMETER # e.g., "CL"
    cov_on_param <- paste0(cov_name, "_", param_name)  # e.g., "WT_CL"

    model_add_cov(
      search_state = search_state,
      ref_model = new_model_name,
      cov_on_param = cov_on_param,
      id_var = search_state$idcol,
      data_file = search_state$data_file,
      covariate_search = search_state$covariate_search
    )

    cat("  [OK] Covariate added to model file\n")

    # Step 4: Add to database (may fail, but file exists and counter updated)
    cat("  Adding to database...\n")

    new_row <- data.frame(
      model_name = new_model_name,
      step_description = sprintf("Add %s", covariate_name),
      phase = "forward_selection",
      step_number = 1L,
      parent_model = base_model_id,
      covariate_tested = covariate_name,
      action = "add_single_covariate",
      ofv = NA_real_,
      delta_ofv = NA_real_,
      rse_max = NA_real_,
      status = "created",
      tags = I(list(c(covariate_name))),
      submission_time = as.POSIXct(NA),
      completion_time = as.POSIXct(NA),
      retry_attempt = 0L,
      original_model = NA_character_,
      estimation_issue = NA_character_,
      excluded_from_step = FALSE,
      stringsAsFactors = FALSE
    )

    search_state$search_database <<- rbind(search_state$search_database, new_row)

    cat(sprintf("[OK] Model %s added to database\n", new_model_name))

    return(list(status = "success", model_name = new_model_name, covariate_added = covariate_name))

  }, error = function(e) {
    cat(sprintf("[X] Model creation failed: %s\n", e$message))

    # Check if physical model file was created
    model_file_path <- file.path(search_state$models_folder, paste0(new_model_name, ".ctl"))
    file_exists <- file.exists(model_file_path)

    if (file_exists) {
      # File was created but later steps failed
      cat(sprintf("  [INFO] Physical model file exists: %s\n", basename(model_file_path)))

      # Make sure counter was incremented (it should have been)
      if (search_state$model_counter < as.numeric(gsub("run", "", new_model_name))) {
        search_state$model_counter <<- as.numeric(gsub("run", "", new_model_name))
        cat(sprintf("  [FIX] Updated counter to match existing file: %d\n", search_state$model_counter))
      }
    } else {
      # File creation failed completely
      cat(sprintf("  [INFO] No physical model file created\n"))
    }

    return(list(
      status = "error",
      error_message = e$message,
      attempted_model = new_model_name,
      file_exists = file_exists
    ))
  })
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
          ###debugging for removal !!!###
          cat("Model:", model_name, "\n")
          cat("Notes length:", length(mod$notes), "\n")
          cat("Notes content:", if(length(mod$notes) > 0) mod$notes[1] else "EMPTY", "\n")
          cat("Notes variable:", notes, "\n")
          cat("Grep result:", grepl("^[+-]", notes), "\n")
          cat("---\n")
          ##############
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

        step_num <- as.numeric(gsub("run", "", model_name))

      }, error = function(e) {
        # If BBR fails, use simple logic
        parent_model <- search_state$base_model
        step_desc <- "Manual/External"
        phase <- "manual"
        action <- "manual_modification"
        step_num <- as.numeric(gsub("run", "", model_name))
      })
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
      covariate_tested = paste(covariates, collapse = ";"),
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

    search_state$search_database <- rbind(search_state$search_database, new_row)
  }

  cat("Added", length(model_names), "models to search database.\n")
  return(search_state)
}

#' Update Model Counter
#'
#' @title Update model counter excluding retry models
#' @description Sets the model counter based on existing models
#' @param search_state List containing search state
#' @return Updated search_state with model counter set
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

#' Update All Model Statuses
#'
#' @title Update status for all models in database
#' @description Refreshes status information for all tracked models
#' @param search_state List containing search state
#' @return Updated search_state with refreshed model statuses
#' @export
update_all_model_statuses <- function(search_state) {
  if (nrow(search_state$search_database) == 0) {
    return(search_state)
  }

  for (i in 1:nrow(search_state$search_database)) {
    model_name <- search_state$search_database$model_name[i]
    model_path <- file.path(search_state$models_folder, model_name)
    new_status <- get_model_status_from_files(model_path)
    search_state$search_database$status[i] <- new_status

    # Update OFV if completed and not already set
    if (new_status == "completed" && is.na(search_state$search_database$ofv[i])) {
      search_state$search_database$ofv[i] <- get_model_ofv_from_files(search_state, model_name)
      search_state$search_database$completion_time[i] <- Sys.time()
    }
  }

  return(search_state)
}

#' Get Model OFV from Files
#'
#' @title Extract OFV from model output files
#' @description Extracts OFV value from NONMEM output files
#' @param search_state List containing search state
#' @param model_name Character. Model name
#' @return Numeric. OFV value or NA
#' @export
get_model_ofv_from_files <- function(search_state, model_name) {

  model_path <- file.path(search_state$models_folder, model_name)
  status <- get_model_status_from_files(model_path)
  if (status != "completed") {
    return(NA)
  }

  tryCatch({
    # Try .lst file parsing first
    lst_file <- file.path(search_state$models_folder, model_name, paste0(model_name, ".lst"))
    if (file.exists(lst_file)) {
      lst_content <- readLines(lst_file, warn = FALSE)

      ofv_lines <- grep("OBJECTIVE FUNCTION VALUE", lst_content, value = TRUE)
      if (length(ofv_lines) > 0) {
        final_ofv_line <- utils::tail(ofv_lines, 1)
        ofv_match <- regmatches(final_ofv_line,
                                regexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", final_ofv_line))
        if (length(ofv_match) > 0) {
          return(as.numeric(ofv_match[1]))
        }
      }
    }

    return(NA)
  }, error = function(e) {
    return(NA)
  })
}

#' Get Model Covariates from Files
#'
#' @title Extract covariates from model using BBR tags
#' @description Gets covariate information from BBR model tags
#' @param search_state List containing search state
#' @param model_name Character. Model name
#' @return Character vector. Covariate names
#' @export
get_model_covariates_from_files <- function(search_state, model_name) {

  tryCatch({
    model_path <- file.path(search_state$models_folder, model_name)
    mod <- bbr::read_model(model_path)
    mod_tags <- mod$tags
    cov_tags <- names(search_state$tags)[grepl("^cov_", names(search_state$tags))]
    cov_tag_values <- unlist(search_state$tags[cov_tags])
    present_cov_values <- intersect(cov_tag_values, mod_tags)
    present_cov_names <- names(search_state$tags)[search_state$tags %in% present_cov_values]
    return(present_cov_names)
  }, error = function(e) {
    return(character(0))
  })
}
