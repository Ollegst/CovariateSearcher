# =============================================================================
# CORE FUNCTIONS - MAIN INITIALIZATION AND MODEL MANAGEMENT
# File: R/01-core-functions.R
# Part of CovariateSearcher Package
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
  search_state <- initialize_search_database(search_state)
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
initialize_search_database <- function(search_state) {
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
#' @title Add single covariate to parent model
#' @description Creates a new model by adding one covariate to the parent model
#' @param search_state List containing search state
#' @param parent_model Character. Parent model name (e.g., "run1")
#' @param cov_tag Character. Covariate tag to add (e.g., "cov_cl_wt")
#' @return List with updated search_state and model information
#' @export
add_covariate_to_model <- function(search_state, parent_model, cov_tag) {

  cat("=== Adding Covariate ===\n")
  cat("Parent model:", parent_model, "\n")
  cat("Covariate tag:", cov_tag, "\n")

  # Get covariate info
  cov_value <- search_state$tags[[cov_tag]]
  if (is.null(cov_value)) {
    stop("Covariate tag not found: ", cov_tag)
  }

  matching_row <- search_state$covariate_search[
    grepl(paste0("_", cov_value, "$"), search_state$covariate_search$cov_to_test), ]

  if (nrow(matching_row) == 0) {
    stop("No matching covariate found for tag: ", cov_tag)
  }

  cov_info <- matching_row[1, ]
  cov_name <- cov_info$COVARIATE  # e.g., "WT"
  param_name <- cov_info$PARAMETER # e.g., "CL"
  cov_on_param <- paste0(cov_name, "_", param_name)  # e.g., "WT_CL"

  cat("Adding covariate:", cov_value, "\n")
  cat("STATUS:", cov_info$STATUS, "FORMULA:", cov_info$FORMULA, "\n")

  # Create new model name
  new_model_name <- paste0("run", search_state$model_counter + 1)
  search_state$model_counter <- search_state$model_counter + 1

  tryCatch({
    # Step 1: Create BBR model
    cat("Creating BBR model...\n")
    parent_path <- file.path(search_state$models_folder, parent_model)

    new_mod <- bbr::copy_model_from(
      .parent_mod = bbr::read_model(parent_path),
      .new_model = new_model_name,
      .inherit_tags = TRUE,
      .overwrite = TRUE
    ) %>%
      bbr::add_tags(search_state$tags[[cov_tag]])

    cat("✓ BBR model created:", new_model_name, "\n")

    # Step 2: Apply model_add_cov function
    cat("Adding covariate to model file...\n")

    search_state <- model_add_cov(
      search_state = search_state,
      ref_model = new_model_name,
      cov_on_param = cov_on_param,
      id_var = search_state$idcol,
      data_file = search_state$data_file,
      covariate_search = search_state$covariate_search
    )

    cat("✓ Covariate added to model file\n")

    # Step 3: Add to database
    new_row <- data.frame(
      model_name = new_model_name,
      step_description = "Individual Covariate Addition",
      phase = "individual_testing",
      step_number = as.integer(gsub("run", "", new_model_name)),
      parent_model = parent_model,
      covariate_tested = cov_value,
      action = "add_single_covariate",
      ofv = NA_real_,
      delta_ofv = NA_real_,
      rse_max = NA_real_,
      status = "created",
      tags = I(list(c(get_model_covariates(search_state, parent_model), cov_tag))),
      submission_time = as.POSIXct(NA),
      completion_time = as.POSIXct(NA),
      retry_attempt = NA_integer_,
      original_model = NA_character_,
      estimation_issue = NA_character_,
      excluded_from_step = FALSE,
      stringsAsFactors = FALSE
    )

    search_state$search_database <- rbind(search_state$search_database, new_row)

    return(list(
      search_state = search_state,
      model_name = new_model_name,
      log_file = NULL
    ))

  }, error = function(e) {
    cat("✗ Model creation failed:", e$message, "\n")
    return(list(
      search_state = search_state,
      model_name = NULL,
      error = e$message
    ))
  })
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
  search_state <- initialize_search_database(search_state)

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
          step_desc <- "Added Covariate"
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

    # Get model information
    status <- get_model_status(search_state, model_name)
    ofv <- if (status == "completed") get_model_ofv(search_state, model_name) else NA
    covariates <- get_model_covariates(search_state, model_name)

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
      tags = I(list(covariates)),
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

#' Get Model Status
#'
#' @title Get current status of a model
#' @description Checks model completion status by examining output files
#' @param search_state List containing search state
#' @param model_name Character. Model name to check
#' @return Character. Status: "completed", "failed", "in_progress", etc.
#' @export
get_model_status <- function(search_state, model_name) {

  model_path <- file.path(search_state$models_folder, model_name)

  # Check if model file exists
  if (!any(file.exists(paste0(model_path, c(".ctl", ".mod"))))) {
    return("not_found")
  }

  # Check if output folder exists
  output_folder <- file.path(search_state$models_folder, model_name)
  if (!dir.exists(output_folder)) {
    return("not_submitted")
  }

  # Check .lst file for completion
  lst_file <- file.path(output_folder, paste0(model_name, ".lst"))

  if (!file.exists(lst_file)) {
    return("in_progress")
  }

  # Check completion status
  tryCatch({
    lst_content <- readLines(lst_file, warn = FALSE)

    if (any(grepl("Stop Time:", lst_content))) {
      # Look for failure patterns
      failure_patterns <- c(
        "CONVERGENCE NOT ACHIEVED",
        "OPTIMIZATION WAS TERMINATED",
        "NONMEM STOP",
        "FATAL ERROR",
        "EXECUTION ERROR"
      )

      if (any(sapply(failure_patterns, function(pattern) any(grepl(pattern, lst_content))))) {
        return("failed")
      } else {
        return("completed")
      }
    } else {
      return("in_progress")
    }
  }, error = function(e) {
    return("in_progress")
  })
}

#' Get Model OFV
#'
#' @title Extract OFV from completed model
#' @description Extracts the objective function value from model output
#' @param search_state List containing search state
#' @param model_name Character. Model name
#' @return Numeric. OFV value or NA if not available
#' @export
get_model_ofv <- function(search_state, model_name) {

  status <- get_model_status(search_state, model_name)
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
        final_ofv_line <- tail(ofv_lines, 1)
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

#' Get Model Covariates
#'
#' @title Get list of covariates in a model
#' @description Extracts covariate tags from model using BBR tags
#' @param search_state List containing search state
#' @param model_name Character. Model name
#' @return Character vector. Covariate tag names
#' @export
get_model_covariates <- function(search_state, model_name) {

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
      search_state$model_counter <- tail(sorted_numbers, 1)

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
    new_status <- get_model_status(search_state, model_name)
    search_state$search_database$status[i] <- new_status

    # Update OFV if completed and not already set
    if (new_status == "completed" && is.na(search_state$search_database$ofv[i])) {
      search_state$search_database$ofv[i] <- get_model_ofv(search_state, model_name)
      search_state$search_database$completion_time[i] <- Sys.time()
    }
  }

  return(search_state)
}
