# =============================================================================
# INITIALIZATION
# File: R/initialization.R
# Part of CovariateSearcher Package
# Package initialization and setup
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
  search_state <- ensure_base_model_in_database(search_state)  # ADD THIS LINE
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

