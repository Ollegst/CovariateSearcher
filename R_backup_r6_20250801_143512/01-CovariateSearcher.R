#' CovariateSearcher R6 Class
#'
#' @description
#' Automated stepwise covariate modeling for NONMEM with recovery systems,
#' parallel execution, and comprehensive monitoring.
#'
#' @details
#' This class provides a complete framework for automated covariate search
#' in pharmacometric modeling. It integrates with BBR for model management
#' and includes sophisticated recovery mechanisms for failed estimations.
#'
#' \subsection{Main Capabilities}{
#' \itemize{
#'   \item R6 class initialization with validation and setup
#'   \item BBR-integrated model management and discovery
#'   \item Single covariate addition to existing models
#'   \item Basic model status and OFV extraction
#'   \item Search database initialization with retry tracking columns
#'   \item YAML-based tags and configuration management
#'   \item Model file reading/writing with proper path handling
#' }
#' }
#'
#' \subsection{Key Methods (Core Module)}{
#' \itemize{
#'   \item \code{initialize()} - Constructor with full setup and validation
#'   \item \code{add_covariate()} - Add single covariate to parent model
#'   \item \code{discover_existing_models()} - Scan and catalog existing models
#'   \item \code{get_model_status()} - Check model completion status
#'   \item \code{get_model_ofv()} - Extract OFV from completed models
#'   \item \code{get_model_covariates()} - Get covariate list from model tags
#'   \item \code{view_search_database()} - Display database with model relationships
#' }
#' }
#'
#' \subsection{Database Structure}{
#' Creates comprehensive \code{search_database} with columns:
#' \itemize{
#'   \item \strong{Basic info:} model_name, step_description, phase, parent_model
#'   \item \strong{Results:} ofv, delta_ofv, rse_max, status, tags
#'   \item \strong{Timing:} submission_time, completion_time
#'   \item \strong{Recovery:} retry_attempt, original_model, estimation_issue, excluded_from_step
#' }
#' }
#'
#' \subsection{Supported Covariate Types}{
#' \itemize{
#'   \item \strong{Continuous Linear:} (1 + (COV-REF) * THETA)
#'   \item \strong{Continuous Power:} (COV/REF)**THETA
#'   \item \strong{Fixed Power:} (COV/REF)**1 or (COV/REF)**0.75
#'   \item \strong{Categorical Binary:} (1 + THETA * COV)
#'   \item \strong{Categorical Multi-level:} IF-THEN-ELSE blocks
#'   \item \strong{Time-dependent:} Automatic detection and handling
#' }
#' }
#'
#' \subsection{Required Input Files}{
#' \itemize{
#'   \item \strong{Base model file:} .ctl or .mod file in models folder
#'   \item \strong{Data file:} CSV with all covariates and standard NONMEM columns
#'   \item \strong{Covariate search:} CSV with PARAMETER, COVARIATE, STATUS, FORMULA, etc.
#'   \item \strong{Tags file:} YAML at data/spec/tags.yaml with covariate mappings
#' }
#' }
#'
#' \subsection{Extension Modules}{
#' This core module is extended by:
#' \itemize{
#'   \item \strong{Recovery Module:} Automatic retry and exclusion system
#'   \item \strong{SCM Module:} Complete stepwise covariate modeling workflow
#'   \item \strong{Monitoring Module:} Progress tracking and reporting
#'   \item \strong{Utility Module:} Helper functions and advanced features
#' }
#' }
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom dplyr %>% mutate select filter bind_rows count arrange desc
#' @importFrom readr read_csv
#' @importFrom yaml read_yaml
#' @importFrom bbr read_model copy_model_from add_tags
#' @examples
#' \dontrun{
#' # Initialize searcher
#' searcher <- CovariateSearcher$new(
#'   base_model_path = "run1",
#'   data_file_path = "data/derived/synthetic_nonmem_data.csv",
#'   covariate_search_path = "data/derived/synthetic_covariate_search.csv"
#' )
#'
#' # View current database
#' searcher$view_search_database()
#'
#' # Add covariate to model
#' result <- searcher$add_covariate("run1", "cov_cl_wt")
#'
#' # Check model status
#' status <- searcher$get_model_status("run2")
#' }
CovariateSearcher <- R6Class("CovariateSearcher",
                             public = list(
                               # =============================================================================
                               # CORE PROPERTIES
                               # =============================================================================

                               #' @field base_model Character. Base model name (e.g., "run1")
                               base_model = NULL,

                               #' @field data_file Data.frame. NONMEM dataset with covariates
                               data_file = NULL,

                               #' @field covariate_search Data.frame. Covariate search configuration
                               covariate_search = NULL,

                               #' @field models_folder Character. Directory containing model files
                               models_folder = NULL,

                               #' @field timecol Character. Time column name in dataset
                               timecol = NULL,

                               #' @field idcol Character. ID column name in dataset
                               idcol = NULL,

                               #' @field threads Integer. Number of threads for parallel execution
                               threads = NULL,

                               #' @field tags List. Covariate tag mappings from YAML
                               tags = NULL,

                               #' @field model_counter Integer. Current model counter for naming
                               model_counter = NULL,

                               #' @field search_database Data.frame. Database tracking all models
                               search_database = NULL,

                               #' @field search_config List. Search configuration parameters
                               search_config = NULL,

                               # =============================================================================
                               # CONSTRUCTOR
                               # =============================================================================

                               #' @description Initialize CovariateSearcher instance
                               #' @param base_model_path Character. Path to base model (e.g., "run1")
                               #' @param data_file_path Character. Path to NONMEM dataset CSV
                               #' @param covariate_search_path Character. Path to covariate search CSV
                               #' @param models_folder Character. Directory containing models (default: "models")
                               #' @param timecol Character. Time column name (default: "TIME")
                               #' @param idcol Character. ID column name (default: "ID")
                               #' @param threads Integer. Number of threads for execution (default: 60)
                               #' @param discover_existing Logical. Discover existing models (default: TRUE)
                               #' @return New CovariateSearcher instance
                               initialize = function(base_model_path,
                                                     data_file_path,
                                                     covariate_search_path,
                                                     models_folder = "models",
                                                     timecol = "TIME",
                                                     idcol = "ID",
                                                     threads = 60,
                                                     discover_existing = TRUE) {

                                 cat("Initializing CovariateSearcher (Core Module)...\n")

                                 # Store inputs
                                 self$base_model <- base_model_path
                                 self$data_file <- readr::read_csv(data_file_path, show_col_types = FALSE)
                                 self$covariate_search <- readr::read_csv(covariate_search_path, show_col_types = FALSE)

                                 # Add cov_to_test column if missing
                                 if (!"cov_to_test" %in% names(self$covariate_search)) {
                                   cat("Adding cov_to_test column...\n")
                                   self$covariate_search$cov_to_test <- paste0("beta_",
                                                                               self$covariate_search$COVARIATE, "_",
                                                                               self$covariate_search$PARAMETER)
                                 }

                                 self$models_folder <- models_folder
                                 self$timecol <- timecol
                                 self$idcol <- idcol
                                 self$threads <- threads

                                 # Load tags and run validations
                                 private$load_tags()
                                 private$validate_setup()

                                 # Initialize search database and configuration
                                 private$initialize_search_database()
                                 private$initialize_search_config()

                                 # Discover existing models if requested
                                 if (discover_existing) {
                                   self$discover_existing_models()
                                 }

                                 # Set model counter based on existing models
                                 private$update_model_counter()

                                 cat("CovariateSearcher (Core) initialized successfully!\n")
                               },

                               # =============================================================================
                               # ESSENTIAL MODEL MANAGEMENT METHODS
                               # =============================================================================

                               #' @description Add covariate to parent model
                               #' @param parent_model Character. Parent model name (e.g., "run1")
                               #' @param cov_tag Character. Covariate tag to add (e.g., "cov_cl_wt")
                               #' @return List with model_name and log_file, or NULL if failed
                               add_covariate = function(parent_model, cov_tag) {

                                 # Initialize log
                                 log_messages <- c()
                                 log_msg <- function(msg) {
                                   log_messages <<- c(log_messages, paste(Sys.time(), "-", msg))
                                   cat(".", sep = "")  # Progress indicator
                                 }

                                 cat("=== Adding Covariate ===\n")
                                 log_msg("=== Starting Covariate Addition Process ===")
                                 log_msg(paste("Parent model:", parent_model))
                                 log_msg(paste("Covariate tag:", cov_tag))

                                 # Get covariate info
                                 cov_value <- self$tags[[cov_tag]]
                                 log_msg(paste("Tag value found:", cov_value))

                                 matching_row <- self$covariate_search[grepl(paste0("_", cov_value, "$"),
                                                                             self$covariate_search$cov_to_test), ]

                                 if (nrow(matching_row) == 0) {
                                   log_msg("ERROR: No matching covariate found")
                                   stop("No matching covariate found for tag: ", cov_tag)
                                 }

                                 cov_info <- matching_row[1, ]
                                 log_msg(paste("Matching covariate found:", cov_info$COVARIATE))

                                 # Extract the actual covariate name and parameter from the row
                                 cov_name <- cov_info$COVARIATE  # e.g., "WT"
                                 param_name <- cov_info$PARAMETER # e.g., "CL"

                                 # Create the cov_on_param string that model_add_cov expects
                                 cov_on_param <- paste0(cov_name, "_", param_name)  # e.g., "WT_CL"

                                 cat("Adding covariate:", cov_value, "\n")
                                 cat("STATUS:", cov_info$STATUS, "FORMULA:", cov_info$FORMULA, "\n")

                                 log_msg(paste("Combined parameter name:", cov_on_param))

                                 # Create new model name
                                 new_model_name <- paste0("run", self$model_counter + 1)
                                 self$model_counter <- self$model_counter + 1
                                 log_msg(paste("New model name:", new_model_name))

                                 tryCatch({
                                   # Step 1: Create BBR model
                                   log_msg("Step 1: Creating BBR model...")
                                   parent_path <- file.path(self$models_folder, parent_model)

                                   new_mod <- bbr::copy_model_from(
                                     .parent_mod = bbr::read_model(parent_path),
                                     .new_model = new_model_name,
                                     .inherit_tags = TRUE,
                                     .overwrite = TRUE
                                   ) %>%
                                     bbr::add_tags(self$tags[[cov_tag]])

                                   log_msg("BBR model created successfully")
                                   cat("✓ BBR model created:", new_model_name, "\n")

                                   # Step 2: Apply model_add_cov function
                                   log_msg("Step 2: Adding covariate to model file...")

                                   private$model_add_cov(
                                     ref_model = new_model_name,
                                     cov_on_param = cov_on_param,
                                     id_var = self$idcol,
                                     data_file = self$data_file,
                                     covariate_search = self$covariate_search
                                   )

                                   log_msg("Covariate successfully added to model file")
                                   cat("✓ Covariate added to model file\n")

                                   # Step 3: Save log file
                                   log_file <- file.path(self$models_folder,
                                                         paste0(new_model_name, "_add_", cov_name, "_", param_name, "_log.txt"))
                                   log_msg(paste("Saving log to:", basename(log_file)))
                                   writeLines(log_messages, log_file)
                                   cat("✓ Log saved to:", basename(log_file), "\n")

                                   return(list(model_name = new_model_name, log_file = log_file))

                                 }, error = function(e) {
                                   log_msg(paste("ERROR occurred:", e$message))

                                   # Save error log
                                   error_log_file <- file.path(self$models_folder,
                                                               paste0("ERROR_add_", cov_name, "_", param_name, "_log.txt"))
                                   writeLines(log_messages, error_log_file)

                                   cat("✗ Model creation failed:", e$message, "\n")
                                   return(NULL)
                                 })
                               },

                               #' @description Discover existing models and set up relationships
                               #' @return Invisible NULL
                               discover_existing_models = function() {

                                 cat("Discovering existing models...\n")

                                 # Clear existing database
                                 private$initialize_search_database()

                                 # Get all model files in models folder
                                 model_files <- list.files(self$models_folder, pattern = "^run\\d+\\.(ctl|mod)$",
                                                           full.names = FALSE)
                                 model_names <- gsub("\\.(ctl|mod)$", "", model_files)
                                 model_names <- unique(model_names)
                                 model_names <- model_names[order(as.numeric(gsub("run", "", model_names)))]

                                 if (length(model_names) == 0) {
                                   cat("No existing models found.\n")
                                   return(invisible(NULL))
                                 }

                                 cat("Found", length(model_names), "existing models:",
                                     paste(model_names, collapse = ", "), "\n")

                                 # Add each model to database
                                 for (model_name in model_names) {

                                   # Simple parent relationship logic
                                   if (model_name == self$base_model) {
                                     parent_model <- NA_character_
                                     step_desc <- "Base Model"
                                     phase <- "base"
                                     action <- "base"
                                     step_num <- 0L
                                   } else {
                                     # Try to get parent from BBR
                                     tryCatch({
                                       model_path <- file.path(self$models_folder, model_name)
                                       mod <- bbr::read_model(model_path)

                                       if (!is.null(mod$based_on) && length(mod$based_on) > 0) {
                                         parent_model <- mod$based_on[1]
                                         step_desc <- "Added Covariate"
                                         phase <- "individual_testing"
                                         action <- "add_single_covariate"
                                       } else {
                                         # Fallback logic
                                         model_num <- as.numeric(gsub("run", "", model_name))
                                         parent_model <- self$base_model
                                         step_desc <- "Manual/External"
                                         phase <- "manual"
                                         action <- "manual_modification"
                                       }

                                       step_num <- as.numeric(gsub("run", "", model_name))

                                     }, error = function(e) {
                                       # If BBR fails, use simple logic
                                       model_num <- as.numeric(gsub("run", "", model_name))
                                       parent_model <- self$base_model
                                       step_desc <- "Manual/External"
                                       phase <- "manual"
                                       action <- "manual_modification"
                                       step_num <- model_num
                                     })
                                   }

                                   # Get model information
                                   status <- self$get_model_status(model_name)
                                   ofv <- if (status == "completed") self$get_model_ofv(model_name) else NA
                                   covariates <- self$get_model_covariates(model_name)

                                   # Add to database with retry tracking columns
                                   new_row <- tibble::tibble(
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
                                     tags = list(covariates),
                                     submission_time = as.POSIXct(NA),
                                     completion_time = if (status == "completed") Sys.time() else as.POSIXct(NA),

                                     # Retry tracking columns
                                     retry_attempt = ifelse(grepl("\\d{3}$", model_name), 1L, NA_integer_),
                                     original_model = NA_character_,
                                     estimation_issue = NA_character_,
                                     excluded_from_step = FALSE
                                   )

                                   self$search_database <- dplyr::bind_rows(self$search_database, new_row)
                                 }

                                 cat("Added", length(model_names), "models to search database.\n")
                               },

                               # =============================================================================
                               # BASIC MODEL INFORMATION METHODS
                               # =============================================================================

                               #' @description Get current status of a model
                               #' @param model_name Character. Model name to check
                               #' @return Character. Status: "completed", "failed", "in_progress", etc.
                               get_model_status = function(model_name) {

                                 model_path <- file.path(self$models_folder, model_name)

                                 # Check if model file exists
                                 if (!any(file.exists(paste0(model_path, c(".ctl", ".mod"))))) {
                                   return("not_found")
                                 }

                                 # Check if output folder exists
                                 output_folder <- file.path(self$models_folder, model_name)
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
                               },

                               #' @description Extract OFV from completed model
                               #' @param model_name Character. Model name
                               #' @return Numeric. OFV value or NA if not available
                               get_model_ofv = function(model_name) {

                                 status <- self$get_model_status(model_name)
                                 if (status != "completed") {
                                   return(NA)
                                 }

                                 tryCatch({
                                   # Try .lst file parsing first
                                   lst_file <- file.path(self$models_folder, model_name, paste0(model_name, ".lst"))
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
                               },

                               #' @description Get list of covariates in a model
                               #' @param model_name Character. Model name
                               #' @return Character vector. Covariate tag names
                               get_model_covariates = function(model_name) {

                                 tryCatch({
                                   model_path <- file.path(self$models_folder, model_name)
                                   mod <- bbr::read_model(model_path)
                                   mod_tags <- mod$tags
                                   cov_tags <- names(self$tags)[grepl("^cov_", names(self$tags))]
                                   cov_tag_values <- unlist(self$tags[cov_tags])
                                   present_cov_values <- intersect(cov_tag_values, mod_tags)
                                   present_cov_names <- names(self$tags)[self$tags %in% present_cov_values]
                                   return(present_cov_names)
                                 }, error = function(e) {
                                   return(character(0))
                                 })
                               },

                               # =============================================================================
                               # DATABASE VIEWING
                               # =============================================================================

                               #' @description View the search database
                               #' @param detailed Logical. Show detailed view (default: TRUE)
                               #' @return Invisible search database
                               view_search_database = function(detailed = TRUE) {

                                 if (nrow(self$search_database) == 0) {
                                   cat("Search database is empty.\n")
                                   return(invisible(NULL))
                                 }

                                 cat("=== Covariate Search Database ===\n")
                                 cat("Total models:", nrow(self$search_database), "\n\n")

                                 if (detailed) {
                                   db <- self$search_database %>%
                                     dplyr::mutate(
                                       ofv_display = ifelse(is.na(ofv), "pending", sprintf("%.2f", ofv)),
                                       parent_display = ifelse(is.na(parent_model), "-", parent_model),
                                       delta_display = ifelse(is.na(delta_ofv), "-", sprintf("%.2f", delta_ofv))
                                     )

                                   # Add changes column
                                   db$changes <- sapply(1:nrow(db), function(i) {
                                     model_name <- db$model_name[i]
                                     parent_model <- db$parent_display[i]

                                     if (model_name == self$base_model) {
                                       return("Base model")
                                     } else if (parent_model == "-" || is.na(parent_model)) {
                                       return("No parent info")
                                     } else {
                                       # Simple covariate difference
                                       current_covs <- self$get_model_covariates(model_name)
                                       if (length(current_covs) > 0) {
                                         cov_names <- sapply(current_covs, function(x) {
                                           if (x %in% names(self$tags)) self$tags[[x]] else x
                                         })
                                         return(paste0("+ ", paste(cov_names, collapse = ", ")))
                                       } else {
                                         return("No covariates")
                                       }
                                     }
                                   })

                                   # Select key columns
                                   db <- db %>%
                                     dplyr::select(model_name, parent_display, changes, status, ofv_display, delta_display)

                                   print(db, n = Inf)
                                 }

                                 return(invisible(self$search_database))
                               }
                             ),

                             # =============================================================================
                             # PRIVATE METHODS
                             # =============================================================================
                             private = list(

                               #' @description Load tags from YAML file
                               load_tags = function() {
                                 tags_file <- file.path("data", "spec", "tags.yaml")
                                 if (file.exists(tags_file)) {
                                   self$tags <- yaml::read_yaml(tags_file)
                                 } else {
                                   stop("tags.yaml file not found at: ", tags_file)
                                 }
                               },

                               #' @description Validate setup
                               validate_setup = function() {
                                 cat("Running validation checks...\n")

                                 required_cols <- c("PARAMETER", "COVARIATE", "STATUS", "FORMULA",
                                                    "LEVELS", "REFERENCE", "TIME_DEPENDENT")
                                 missing_cols <- setdiff(required_cols, names(self$covariate_search))
                                 if (length(missing_cols) > 0) {
                                   stop("Missing required columns in covariate_search: ",
                                        paste(missing_cols, collapse = ", "))
                                 }

                                 missing_covs <- setdiff(self$covariate_search$COVARIATE, names(self$data_file))
                                 if (length(missing_covs) > 0) {
                                   stop("Covariates not found in dataset: ", paste(missing_covs, collapse = ", "))
                                 }

                                 cat("All validation checks passed!\n")
                               },

                               #' @description Initialize search database with retry tracking columns
                               initialize_search_database = function() {
                                 self$search_database <- tibble::tibble(
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
                                   tags = list(),
                                   submission_time = as.POSIXct(character()),
                                   completion_time = as.POSIXct(character()),

                                   # Retry tracking columns
                                   retry_attempt = integer(),
                                   original_model = character(),
                                   estimation_issue = character(),
                                   excluded_from_step = logical()
                                 )
                                 cat("Search database initialized with retry tracking columns\n")
                               },

                               #' @description Initialize search configuration
                               initialize_search_config = function() {
                                 self$search_config <- list(
                                   forward_ofv_threshold = 3.84,
                                   backward_ofv_threshold = 6.63,
                                   max_rse_threshold = 50,
                                   max_forward_steps = 10,
                                   max_backward_steps = 10,
                                   timeout_minutes = 60,
                                   current_phase = "initialization",
                                   current_step = 0
                                 )
                                 cat("Search configuration initialized\n")
                               },

                               #' @description Update model counter excluding retry models
                               update_model_counter = function() {
                                 if (nrow(self$search_database) > 0) {
                                   all_model_names <- self$search_database$model_name

                                   # Filter out retry models (ending with 3 digits)
                                   main_models <- all_model_names[!grepl("\\d{3}$", all_model_names)]

                                   if (length(main_models) > 0) {
                                     model_numbers <- as.numeric(gsub("run", "", main_models))
                                     sorted_numbers <- sort(model_numbers, na.last = TRUE)
                                     self$model_counter <- tail(sorted_numbers, 1)

                                     cat("Model counter set to:", self$model_counter,
                                         "(last in sequence, excluding retry models)\n")

                                     if (length(all_model_names) > length(main_models)) {
                                       retry_models <- setdiff(all_model_names, main_models)
                                       cat("Retry models excluded from counter:",
                                           paste(retry_models, collapse = ", "), "\n")
                                     }
                                   } else {
                                     base_number <- as.numeric(gsub("run", "", self$base_model))
                                     self$model_counter <- base_number
                                     cat("Model counter set to base model:", self$model_counter, "\n")
                                   }
                                 } else {
                                   base_number <- as.numeric(gsub("run", "", self$base_model))
                                   self$model_counter <- base_number
                                   cat("Model counter set to base model:", self$model_counter, "\n")
                                 }
                               },

                               #' @description Read model file content
                               read_model_file = function(run_name, extensions = c(".ctl", ".mod")) {
                                 base_path <- file.path(self$models_folder, run_name)

                                 for (ext in extensions) {
                                   file_path <- paste0(base_path, ext)
                                   if (file.exists(file_path)) {
                                     lines <- readLines(file_path)
                                     attr(lines, "file_path") <- file_path
                                     return(lines)
                                   }
                                 }

                                 stop("No file found for ", run_name, " with extensions: ",
                                      paste(extensions, collapse = ", "))
                               },

                               #' @description Write model file content
                               write_model_file = function(lines) {
                                 file_path <- attr(lines, "file_path")

                                 if (is.null(file_path)) {
                                   stop("No file path found. Make sure the lines were read using read_model_file()")
                                 }

                                 writeLines(lines, file_path)
                                 cat("File saved to:", basename(file_path), "\n")
                               },

                               #' @description Add covariate to model file (core functionality)
                               model_add_cov = function(ref_model, cov_on_param, id_var = "ID", data_file, covariate_search) {

                                 modelcode <- private$read_model_file(ref_model)
                                 original_file_path <- attr(modelcode, "file_path")

                                 cov_on_param <- paste0("beta_", cov_on_param)

                                 cova <- covariate_search$COVARIATE[covariate_search$cov_to_test == cov_on_param]
                                 param <- covariate_search$PARAMETER[covariate_search$cov_to_test == cov_on_param]
                                 ref <- covariate_search$REFERENCE[covariate_search$cov_to_test == cov_on_param]

                                 # Get max THETA number
                                 thetas <- modelcode[grepl('THETA\\(..?\\)', modelcode)] %>%
                                   gsub(pattern = '.*THETA\\(', replacement = '') %>%
                                   gsub(pattern = '\\).*', replacement = '') %>%
                                   as.double()

                                 newtheta <- max(thetas) + 1
                                 temp_cov <- dplyr::filter(covariate_search, cov_to_test == cov_on_param)

                                 # Determine covariate type and formula
                                 cov_status <- temp_cov$STATUS[temp_cov$COVARIATE == cova]
                                 cov_formula <- temp_cov$FORMULA[temp_cov$COVARIATE == cova]

                                 FLAG <- dplyr::case_when(
                                   length(cov_status) == 0 | length(cov_formula) == 0 ~ "ERROR: Missing covariate info",
                                   cov_status == "cat" & cov_formula == "linear" ~ "1",
                                   cov_status == "cat" & cov_formula == "power" ~ "2",
                                   cov_status == "con" & cov_formula == "linear" ~ "3",
                                   cov_status == "con" & cov_formula == "power" ~ "2",
                                   cov_status == "con" & cov_formula == "power1" ~ "5",
                                   cov_status == "con" & cov_formula == "power0.75" ~ "6",
                                   .default = "Please check covariate status and formula"
                                 )

                                 initialValuethetacov <- dplyr::case_when(
                                   FLAG == "5" ~ "1 FIX",
                                   FLAG == "6" ~ "0.75 FIX",
                                   .default = "0.1"
                                 )

                                 # Generate formula based on FLAG
                                 if(FLAG == "2") formule <- paste0(' * (',cova,'/',ref,')**THETA(', newtheta ,')')
                                 if(FLAG == "3") formule <- paste0(' * (1 + (',cova,'-',ref, ') * THETA(',newtheta ,'))')
                                 if(FLAG == "5") formule <- paste0(' * (',cova,'/',ref,')** THETA(', newtheta ,')')
                                 if(FLAG == "6") formule <- paste0(' * (',cova,'/',ref,')** THETA(', newtheta ,')')

                                 # Handle categorical covariates (simplified for core module)
                                 if(FLAG == "1") {
                                   formule <- paste0(' * (1 + THETA(', newtheta ,') * ',cova ,')')
                                 }

                                 # Check if time-dependent
                                 max_levels <- max(tapply(data_file[[cova]], data_file[[id_var]],
                                                          function(x) length(unique(x))), na.rm = TRUE)
                                 time_varying <- max_levels > 1

                                 if(time_varying == FALSE){
                                   linetu <- grep(paste0('^\\s*TV_', param), modelcode)
                                 } else {
                                   linetu <- grep(paste0('^\\s*', param), modelcode)
                                 }

                                 # Add covariate to parameter line
                                 if(grepl(";", modelcode[linetu])){
                                   modelcode[linetu] <- sub("(.*?)(\\s*;)", paste0("\\1", formule, "\\2"), modelcode[linetu])
                                 } else {
                                   modelcode[linetu] <- paste0(modelcode[linetu], formule)
                                 }

                                 # Add THETA line
                                 newthetaline <- paste0(initialValuethetacov, ' ; ', cov_on_param, ' ;  ; RATIO')
                                 lineomeg <- grep('\\$OMEGA', modelcode)[1]

                                 modelcode <- c(
                                   modelcode[1:(lineomeg - 1)],
                                   newthetaline,
                                   modelcode[lineomeg:length(modelcode)]
                                 )

                                 # Write back
                                 attr(modelcode, "file_path") <- original_file_path
                                 private$write_model_file(modelcode)
                               }
                             )
)
