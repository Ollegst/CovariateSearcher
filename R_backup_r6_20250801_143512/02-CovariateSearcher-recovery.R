# =============================================================================
# RECOVERY SYSTEM METHODS - ESTIMATION ERROR HANDLING
# File: R/CovariateSearcher-recovery.R
# Extends CovariateSearcher R6 Class with Module 2 functionality
# =============================================================================

#' Recovery System Methods for CovariateSearcher
#'
#' @name CovariateSearcher-recovery
#' @title Recovery System Methods
#' @description Extension methods for handling NONMEM estimation failures,
#'   creating retry models, and managing covariate exclusions.
#' @keywords internal
NULL

#
# PURPOSE:
# This module extends the CovariateSearcher class with automatic recovery
# functionality for NONMEM estimation errors. When models fail to converge or
# encounter numerical issues, the system automatically creates retry models
# with adjusted initial parameter values and excludes problematic covariates
# if retries fail.
#
# MAIN CAPABILITIES:
# • Real-time monitoring of .ext files for estimation problems
# • Automatic retry model creation with THETA adjustments (0.1 → -0.1)
# • Covariate exclusion system for persistent failures
# • Comprehensive logging and recovery statistics
# • Integration with BBR model management
#
# KEY METHODS ADDED (PUBLIC):
# • read_ext_file()                    - Parse .ext files, detect issues
# • detect_estimation_problems()       - Monitor multiple models for problems
# • create_retry_model()              - Create retry with adjusted parameters
# • process_estimation_issues()       - Orchestrate automatic recovery
# • update_model_status()             - Database status management
# • get_excluded_covariates()         - Track excluded covariates
# • generate_recovery_report()        - Recovery statistics and reporting
#
# KEY METHODS ADDED (PRIVATE):
# • adjust_theta_for_covariate()      - Modify THETA values in model files
# • handle_failed_retry()             - Handle failed retry model exclusion
#
# RECOVERY WORKFLOW:
# 1. Monitor running models via .ext file analysis
# 2. Detect estimation issues (infinite OFV, NaN parameters, etc.)
# 3. For original models: Create retry with adjusted THETA values
# 4. For retry models: Exclude covariate from current search step
# 5. Update database with exclusion tracking for final phase testing
# 6. Generate comprehensive logs and recovery statistics
#
# USAGE EXAMPLES:
# # Monitor models and trigger recovery
# issues <- searcher$detect_estimation_problems(c("run25", "run26"))
# recovery <- searcher$process_estimation_issues(issues)
#
# # Create specific retry model
# retry_result <- searcher$create_retry_model("run25", "high_ofv")
#
# # Continuous monitoring with automatic recovery
# results <- searcher$monitor_models_with_recovery(model_names,
#                                                  check_interval_minutes = 30)
#
# DEPENDENCIES:
# • Core module (CovariateSearcher.R) - Basic model management and database
# • BBR package - Model copying and management
# • File system access - Reading/writing .ext and model files
#
# =============================================================================

# Ensure CovariateSearcher class exists before extending
if (!exists("CovariateSearcher")) {
  stop("CovariateSearcher class not found. Please load the core module first.")
}

# =============================================================================
# PUBLIC RECOVERY METHODS
# =============================================================================

#' Read NONMEM .ext file and detect estimation issues
#'
#' @param model_name Character. Model name (e.g., "run25")
#' @return List with status, current_ofv, iterations, and issue detection
CovariateSearcher$set("public", "read_ext_file", function(model_name) {
  ext_file <- file.path(self$models_folder, model_name, paste0(model_name, ".ext"))

  if (!file.exists(ext_file)) {
    return(list(
      status = "not_started",
      current_ofv = NA,
      iterations = 0,
      last_iteration_time = NA,
      has_estimation_issues = FALSE,
      issue_type = NA,
      estimation_method = NA
    ))
  }

  tryCatch({
    ext_lines <- readLines(ext_file, warn = FALSE)

    if (length(ext_lines) == 0) {
      return(list(
        status = "empty_file",
        current_ofv = NA,
        iterations = 0,
        last_iteration_time = file.info(ext_file)$mtime,
        has_estimation_issues = FALSE,
        issue_type = NA,
        estimation_method = NA
      ))
    }

    # Find all TABLE headers to identify multiple estimation methods
    table_lines <- which(grepl("^TABLE", ext_lines))

    if (length(table_lines) == 0) {
      table_lines <- 1
    }

    # Process each table and find the final (last) table
    final_table_data <- NULL
    final_method <- "Unknown"

    for (i in seq_along(table_lines)) {
      table_start <- table_lines[i]
      table_end <- if (i < length(table_lines)) table_lines[i + 1] - 1 else length(ext_lines)

      # Extract method name from table header
      if (table_start <= length(ext_lines)) {
        method_line <- ext_lines[table_start]
        if (grepl("First Order", method_line)) {
          method_name <- "FOCE"
        } else if (grepl("Importance", method_line)) {
          method_name <- "IMP"
        } else if (grepl("Stochastic", method_line)) {
          method_name <- "SAEM"
        } else if (grepl("Laplacian", method_line)) {
          method_name <- "LAPLACE"
        } else {
          method_name <- paste("Method", i)
        }
      } else {
        method_name <- "Unknown"
      }

      # Find data lines in this table
      table_section <- ext_lines[table_start:table_end]
      data_lines <- table_section[!grepl("^#|^TABLE|^\\s*ITERATION\\s+", table_section) &
                                    nchar(trimws(table_section)) > 0]

      if (length(data_lines) > 0) {
        final_table_data <- data_lines
        final_method <- method_name
      }
    }

    if (is.null(final_table_data) || length(final_table_data) == 0) {
      return(list(
        status = "no_data",
        current_ofv = NA,
        iterations = 0,
        last_iteration_time = file.info(ext_file)$mtime,
        has_estimation_issues = FALSE,
        issue_type = NA,
        estimation_method = final_method
      ))
    }

    # Parse the last (most recent) data line from final table
    last_line <- tail(final_table_data, 1)
    values <- as.numeric(strsplit(trimws(last_line), "\\s+")[[1]])

    # Handle potential parsing errors
    if (length(values) < 2) {
      return(list(
        status = "parse_error",
        current_ofv = NA,
        iterations = 0,
        last_iteration_time = file.info(ext_file)$mtime,
        has_estimation_issues = TRUE,
        issue_type = "parse_error",
        estimation_method = final_method
      ))
    }

    # Extract iteration and OFV
    current_iteration <- values[1]
    current_ofv <- values[length(values)]  # Last column is typically OBJ

    # Detect estimation issues
    has_issues <- FALSE
    issue_type <- NA

    if (!is.na(current_ofv)) {
      if (is.infinite(current_ofv)) {
        has_issues <- TRUE
        issue_type <- "infinite_ofv"
      } else if (is.nan(current_ofv)) {
        has_issues <- TRUE
        issue_type <- "nan_ofv"
      } else if (abs(current_ofv) > 1e10) {
        has_issues <- TRUE
        issue_type <- "high_ofv"
      }
    } else {
      has_issues <- TRUE
      issue_type <- "missing_ofv"
    }

    # Check for other problematic values in the iteration
    if (!has_issues) {
      problematic_values <- values[is.infinite(values) | is.nan(values) | abs(values) > 1e10]
      if (length(problematic_values) > 0) {
        has_issues <- TRUE
        issue_type <- "problematic_parameters"
      }
    }

    # Determine status based on iteration number
    model_status <- if (current_iteration < 0) "completed" else "progressing"

    return(list(
      status = model_status,
      current_ofv = current_ofv,
      iterations = abs(current_iteration),
      last_iteration_time = file.info(ext_file)$mtime,
      has_estimation_issues = has_issues,
      issue_type = issue_type,
      estimation_method = final_method
    ))

  }, error = function(e) {
    return(list(
      status = "read_error",
      current_ofv = NA,
      iterations = 0,
      last_iteration_time = file.info(ext_file)$mtime,
      has_estimation_issues = TRUE,
      issue_type = paste("read_error:", e$message),
      estimation_method = "Unknown"
    ))
  })
})

#' Monitor multiple models for estimation problems
#'
#' @param model_names Character vector. Model names to monitor
#' @param check_interval_minutes Numeric. How often to check (default: 30)
#' @return List of models with issues detected
CovariateSearcher$set("public", "detect_estimation_problems", function(model_names, check_interval_minutes = 30) {
  cat(sprintf("🔍 Monitoring %d models for estimation issues (checking every %d minutes)\n",
              length(model_names), check_interval_minutes))

  models_with_issues <- list()

  for (model_name in model_names) {
    cat(sprintf("  Checking %s... ", model_name))

    ext_data <- self$read_ext_file(model_name)

    if (ext_data$has_estimation_issues) {
      cat(sprintf("⚠️  Issue detected: %s (OFV: %s)\n", ext_data$issue_type,
                  ifelse(is.na(ext_data$current_ofv), "NA",
                         sprintf("%.2e", ext_data$current_ofv))))

      models_with_issues[[model_name]] <- list(
        model_name = model_name,
        issue_type = ext_data$issue_type,
        current_ofv = ext_data$current_ofv,
        iterations = ext_data$iterations,
        detection_time = Sys.time()
      )

      # Update database with issue information
      idx <- which(self$search_database$model_name == model_name)
      if (length(idx) > 0) {
        self$search_database$estimation_issue[idx] <- ext_data$issue_type
        self$search_database$status[idx] <- "estimation_error"
      }

    } else {
      cat(sprintf("✅ OK (OFV: %s, Iter: %d)\n",
                  ifelse(is.na(ext_data$current_ofv), "NA",
                         sprintf("%.2f", ext_data$current_ofv)),
                  ext_data$iterations))
    }
  }

  if (length(models_with_issues) > 0) {
    cat(sprintf("\n⚠️  Found %d models with estimation issues\n", length(models_with_issues)))
  } else {
    cat("\n✅ All models progressing normally\n")
  }

  return(models_with_issues)
})

#' Create retry model with adjusted THETA values
#'
#' @param original_model_name Character. Name of problematic model
#' @param issue_type Character. Type of estimation issue detected
#' @return List with retry model information
CovariateSearcher$set("public", "create_retry_model", function(original_model_name, issue_type = "estimation_error") {
  cat(sprintf("🔧 Creating retry model for %s (issue: %s)\n", original_model_name, issue_type))

  # Generate retry model name by adding "001" to original number
  model_number <- gsub("run", "", original_model_name)
  retry_model_name <- paste0("run", model_number, "001")

  cat(sprintf("  Original: %s → Retry: %s\n", original_model_name, retry_model_name))

  # Detailed logging
  log_messages <- c()
  log_msg <- function(msg) {
    log_messages <<- c(log_messages, paste(Sys.time(), "-", msg))
  }

  log_msg("=== Starting Retry Model Creation Process ===")
  log_msg(paste("Original model:", original_model_name))
  log_msg(paste("Issue type:", issue_type))
  log_msg(paste("Retry model name:", retry_model_name))

  tryCatch({
    # Step 1: Get information about the original model
    original_row <- self$search_database[self$search_database$model_name == original_model_name, ]

    if (nrow(original_row) == 0) {
      stop("Original model not found in database: ", original_model_name)
    }

    log_msg("Original model found in database")
    cat("  ✓ Original model found in database\n")

    # Step 2: Create BBR model copy
    cat("  Creating BBR model copy...")
    log_msg("Step 1: Creating BBR model copy...")

    original_model_path <- file.path(self$models_folder, original_model_name)
    log_msg(paste("Original model path:", original_model_path))

    retry_mod <- bbr::copy_model_from(
      .parent_mod = bbr::read_model(original_model_path),
      .new_model = retry_model_name,
      .inherit_tags = TRUE,
      .overwrite = TRUE
    )

    log_msg("BBR model copy created successfully")
    cat(" ✓\n")

    # Step 3: Modify THETA values in the model file
    cat("  Adjusting THETA values for latest covariate...")
    log_msg("Step 2: Adjusting THETA values...")

    # Find the latest added covariate from the original model
    parent_model <- original_row$parent_model[1]

    if (!is.na(parent_model) && nchar(parent_model) > 0) {
      # Get covariates in parent vs original to find what was added
      parent_covs <- self$get_model_covariates(parent_model)
      original_covs <- self$get_model_covariates(original_model_name)
      added_covs <- setdiff(original_covs, parent_covs)

      if (length(added_covs) > 0) {
        latest_covariate <- added_covs[1]
        cat(sprintf(" (covariate: %s)", latest_covariate))
        log_msg(paste("Latest covariate identified:", latest_covariate))

        # Call the THETA adjustment method
        success <- private$adjust_theta_for_covariate(retry_model_name, latest_covariate)

        if (success) {
          cat(" ✓\n")
          log_msg("THETA adjustment completed successfully")
        } else {
          cat(" ⚠️  THETA adjustment failed - using original values\n")
          log_msg("WARNING: THETA adjustment failed - using original values")
        }

      } else {
        cat(" ⚠️  No added covariate found - using original values\n")
        log_msg("WARNING: No added covariate found")
      }
    } else {
      cat(" ⚠️  No parent model info - using original values\n")
      log_msg("WARNING: No parent model information available")
    }

    # Step 4: Add retry model to database
    cat("  Adding to database...")
    log_msg("Step 3: Adding retry model to database...")

    new_row <- tibble::tibble(
      model_name = retry_model_name,
      step_description = paste(original_row$step_description[1], "- Retry"),
      phase = original_row$phase[1],
      step_number = original_row$step_number[1],
      parent_model = original_row$parent_model[1],
      covariate_tested = original_row$covariate_tested[1],
      action = "retry_with_theta_adjustment",
      ofv = NA_real_,
      delta_ofv = NA_real_,
      rse_max = NA_real_,
      status = "created",
      tags = original_row$tags,
      submission_time = as.POSIXct(NA),
      completion_time = as.POSIXct(NA),

      # Retry-specific columns
      retry_attempt = 1L,
      original_model = original_model_name,
      estimation_issue = issue_type,
      excluded_from_step = FALSE
    )

    self$search_database <- dplyr::bind_rows(self$search_database, new_row)
    log_msg("Retry model added to database successfully")

    # Update original model status
    orig_idx <- which(self$search_database$model_name == original_model_name)
    if (length(orig_idx) > 0) {
      self$search_database$status[orig_idx] <- "error"
      self$search_database$estimation_issue[orig_idx] <- issue_type
      log_msg("Original model status updated to 'error'")
    }

    cat(" ✓\n")

    # Step 5: Save detailed log file
    covariate_name <- original_row$covariate_tested[1]
    log_file <- file.path(self$models_folder,
                          paste0(retry_model_name, "_retry_",
                                 gsub("\\s+", "_", covariate_name), "_log.txt"))

    log_msg("Step 4: Saving detailed log...")
    log_msg(paste("Log file path:", log_file))

    # Add summary to log
    log_msg("=== RETRY MODEL CREATION SUMMARY ===")
    log_msg(paste("Retry model created:", retry_model_name))
    log_msg(paste("Based on original:", original_model_name))
    log_msg(paste("Issue that triggered retry:", issue_type))
    log_msg(paste("Covariate being retried:", covariate_name))
    log_msg("THETA values adjusted from 0.1 to -0.1")
    log_msg("=== END SUMMARY ===")

    writeLines(log_messages, log_file)

    cat(sprintf("✅ Retry model %s created successfully\n", retry_model_name))
    cat(sprintf("✅ Detailed log saved: %s\n", basename(log_file)))

    return(list(
      retry_model_name = retry_model_name,
      original_model_name = original_model_name,
      issue_type = issue_type,
      status = "created",
      database_updated = TRUE,
      bbr_model_created = TRUE,
      log_file = log_file
    ))

  }, error = function(e) {
    log_msg(paste("ERROR occurred:", e$message))

    # Save error log
    error_log_file <- file.path(self$models_folder,
                                paste0("ERROR_retry_", model_number, "_log.txt"))
    log_msg(paste("Saving error log to:", basename(error_log_file)))
    writeLines(log_messages, error_log_file)

    cat(sprintf("❌ Failed to create retry model: %s\n", e$message))
    cat(sprintf("✅ Error log saved: %s\n", basename(error_log_file)))

    return(list(
      retry_model_name = NULL,
      original_model_name = original_model_name,
      issue_type = issue_type,
      status = "failed",
      error = e$message,
      log_file = error_log_file
    ))
  })
})

#' Process detected estimation issues and trigger recovery
#'
#' @param models_with_issues List. Models with issues from detect_estimation_problems
#' @return List with recovery actions taken
CovariateSearcher$set("public", "process_estimation_issues", function(models_with_issues) {
  if (length(models_with_issues) == 0) {
    return(list(
      retry_models_created = character(0),
      excluded_covariates = character(0),
      recovery_actions = list()
    ))
  }

  cat(sprintf("\n🔧 Processing %d models with estimation issues\n", length(models_with_issues)))

  retry_models_created <- character(0)
  excluded_covariates <- character(0)
  recovery_actions <- list()

  for (model_name in names(models_with_issues)) {
    issue_info <- models_with_issues[[model_name]]

    cat(sprintf("\n🔍 Processing %s (issue: %s)\n", model_name, issue_info$issue_type))

    # Check if this is already a retry model (ends with 3 digits)
    is_retry <- grepl("\\d{3}$", model_name)

    if (is_retry) {
      cat("  This is a retry model - excluding covariate from step\n")

      # Call private method to handle failed retry
      exclusion_result <- private$handle_failed_retry(model_name, issue_info$issue_type)

      if (exclusion_result$status == "excluded") {
        excluded_covariates <- c(excluded_covariates, exclusion_result$excluded_covariate)

        recovery_actions[[model_name]] <- list(
          action = "exclude_covariate",
          covariate = exclusion_result$excluded_covariate,
          result = exclusion_result
        )

      } else {
        recovery_actions[[model_name]] <- list(
          action = "exclusion_failed",
          result = exclusion_result
        )
        cat("  ❌ Failed to exclude covariate\n")
      }

    } else {
      cat("  This is original model - creating retry with adjusted THETA\n")

      # Create retry model using existing public method
      retry_result <- self$create_retry_model(model_name, issue_info$issue_type)

      if (retry_result$status == "created") {
        retry_models_created <- c(retry_models_created, retry_result$retry_model_name)

        recovery_actions[[model_name]] <- list(
          action = "create_retry",
          retry_model = retry_result$retry_model_name,
          result = retry_result
        )

        cat(sprintf("  ✅ Retry model '%s' created\n", retry_result$retry_model_name))
      } else {
        recovery_actions[[model_name]] <- list(
          action = "retry_creation_failed",
          result = retry_result
        )
        cat("  ❌ Failed to create retry model\n")
      }
    }
  }

  # Summary
  cat(sprintf("\n📊 Recovery Summary:\n"))
  cat(sprintf("  Retry models created: %d\n", length(retry_models_created)))
  cat(sprintf("  Covariates excluded: %d\n", length(excluded_covariates)))

  if (length(retry_models_created) > 0) {
    cat(sprintf("  New retry models: %s\n", paste(retry_models_created, collapse = ", ")))
  }

  if (length(excluded_covariates) > 0) {
    cat(sprintf("  Excluded covariates: %s\n", paste(excluded_covariates, collapse = ", ")))
  }

  return(list(
    retry_models_created = retry_models_created,
    excluded_covariates = excluded_covariates,
    recovery_actions = recovery_actions
  ))
})

#' Update model status in database
#'
#' @param model_name Character. Specific model to update (NULL for all models)
#' @return Invisible NULL
CovariateSearcher$set("public", "update_model_status", function(model_name = NULL) {
  if (is.null(model_name)) {
    # Update all models in database
    for (i in 1:nrow(self$search_database)) {
      model_name <- self$search_database$model_name[i]
      new_status <- self$get_model_status(model_name)
      self$search_database$status[i] <- new_status

      # Update OFV if completed and not already set
      if (new_status == "completed" && is.na(self$search_database$ofv[i])) {
        self$search_database$ofv[i] <- self$get_model_ofv(model_name)
        self$search_database$completion_time[i] <- Sys.time()
      }
    }
  } else {
    # Update specific model
    idx <- which(self$search_database$model_name == model_name)
    if (length(idx) > 0) {
      new_status <- self$get_model_status(model_name)
      self$search_database$status[idx] <- new_status

      if (new_status == "completed" && is.na(self$search_database$ofv[idx])) {
        self$search_database$ofv[idx] <- self$get_model_ofv(model_name)
        self$search_database$completion_time[idx] <- Sys.time()
      }
    }
  }
})

#' Get list of excluded covariates from database
#'
#' @return Character vector of excluded covariate names
CovariateSearcher$set("public", "get_excluded_covariates", function() {
  cat("Retrieving excluded covariates from database...\n")

  tryCatch({
    excluded_models <- self$search_database[self$search_database$excluded_from_step == TRUE, ]

    if (nrow(excluded_models) > 0) {
      excluded_covs <- unique(excluded_models$covariate_tested)
      excluded_covs <- excluded_covs[!is.na(excluded_covs) & excluded_covs != ""]

      cat(sprintf("Found %d excluded covariates: %s\n",
                  length(excluded_covs),
                  paste(excluded_covs, collapse = ", ")))

      return(excluded_covs)
    } else {
      cat("No excluded covariates found\n")
      return(character(0))
    }
  }, error = function(e) {
    cat(sprintf("Error retrieving excluded covariates: %s\n", e$message))
    return(character(0))
  })
})

#' Generate recovery statistics and report
#'
#' @return List with recovery statistics
CovariateSearcher$set("public", "generate_recovery_report", function() {
  cat("Generating recovery system report...\n")

  tryCatch({
    # Get overall statistics
    status_stats <- self$search_database %>%
      dplyr::count(status, .drop = FALSE) %>%
      dplyr::mutate(percentage = n * 100.0 / nrow(self$search_database)) %>%
      dplyr::arrange(desc(n))

    # Get retry statistics
    retry_models <- self$search_database[grepl("\\d{3}$", self$search_database$model_name), ]
    retry_stats <- list(
      total_retries = nrow(retry_models),
      successful_retries = sum(retry_models$status == "completed", na.rm = TRUE),
      failed_retries = sum(retry_models$status %in% c("failed", "retry_failed"), na.rm = TRUE)
    )

    # Get excluded covariates
    excluded_covs <- self$get_excluded_covariates()

    # Create report
    report <- list(
      timestamp = Sys.time(),
      status_distribution = status_stats,
      retry_statistics = retry_stats,
      excluded_covariates = excluded_covs,
      recovery_success_rate = if(retry_stats$total_retries > 0) {
        retry_stats$successful_retries / retry_stats$total_retries * 100
      } else { 0 }
    )

    # Log summary
    cat(sprintf("Recovery Report Summary:\n"))
    cat(sprintf("- Total models: %d\n", nrow(self$search_database)))
    cat(sprintf("- Retry models: %d\n", retry_stats$total_retries))
    cat(sprintf("- Excluded covariates: %d\n", length(excluded_covs)))
    cat(sprintf("- Recovery success rate: %.1f%%\n", report$recovery_success_rate))

    return(report)
  }, error = function(e) {
    cat(sprintf("Failed to generate recovery report: %s\n", e$message))
    return(list(error = e$message))
  })
})

# =============================================================================
# PRIVATE RECOVERY METHODS
# =============================================================================

#' Modify THETA values from 0.1 to -0.1 for specified covariate
#'
#' @param model_name Character. Model to modify (e.g., "run2001")
#' @param covariate_tag Character. Covariate tag that was added (e.g., "cov_cl_wt")
#' @return Logical. TRUE if successful, FALSE otherwise
CovariateSearcher$set("private", "adjust_theta_for_covariate", function(model_name, covariate_tag) {
  tryCatch({
    # Read the model file
    modelcode <- private$read_model_file(model_name)
    original_file_path <- attr(modelcode, "file_path")

    # Find the covariate value from tags
    covariate_value <- self$tags[[covariate_tag]]
    if (is.null(covariate_value)) {
      cat(sprintf("    Warning: Covariate tag %s not found in tags\n", covariate_tag))
      return(FALSE)
    }

    # Find matching covariate in covariate_search to get the beta name
    matching_row <- self$covariate_search[grepl(paste0("_", covariate_value, "$"),
                                                self$covariate_search$cov_to_test), ]

    if (nrow(matching_row) == 0) {
      cat(sprintf("    Warning: No matching covariate found for %s\n", covariate_value))
      return(FALSE)
    }

    cov_info <- matching_row[1, ]
    cov_to_test <- cov_info$cov_to_test  # e.g., "beta_WT_CL"

    cat(sprintf("    Looking for THETA with: %s\n", cov_to_test))

    # Find and modify THETA lines
    theta_lines_modified <- 0

    for (i in 1:length(modelcode)) {
      line <- modelcode[i]

      # Check if this line contains the covariate reference and starts with 0.1
      if (grepl(paste0("\\b", cov_to_test, "\\b"), line) && grepl("^\\s*0\\.1", line)) {

        original_line <- line

        # Replace 0.1 with -0.1 at the beginning of the line
        modified_line <- gsub("^(\\s*)0\\.1", "\\1-0.1", line)

        # Only update if something actually changed
        if (modified_line != original_line) {
          modelcode[i] <- modified_line
          theta_lines_modified <- theta_lines_modified + 1

          cat(sprintf("    THETA line %d modified:\n", i))
          cat(sprintf("      Before: %s\n", trimws(original_line)))
          cat(sprintf("      After:  %s\n", trimws(modified_line)))
        }
      }
    }

    if (theta_lines_modified == 0) {
      cat("    Warning: No THETA lines found for covariate - no changes made\n")
      return(FALSE)
    }

    # Write the modified model file
    attr(modelcode, "file_path") <- original_file_path
    private$write_model_file(modelcode)

    cat(sprintf("    ✓ Modified %d THETA lines for covariate %s\n",
                theta_lines_modified, covariate_value))

    return(TRUE)

  }, error = function(e) {
    cat(sprintf("    Error adjusting THETA: %s\n", e$message))
    return(FALSE)
  })
})

#' Handle failed retry model by excluding covariate from step
#'
#' @param retry_model_name Character. Name of failed retry model (e.g., "run25001")
#' @param exclusion_reason Character. Reason for exclusion
#' @return List with exclusion information and actions taken
CovariateSearcher$set("private", "handle_failed_retry", function(retry_model_name, exclusion_reason = "retry_failed") {
  cat(sprintf("🚫 Handling failed retry model: %s\n", retry_model_name))

  # Find the retry model in database
  retry_idx <- which(self$search_database$model_name == retry_model_name)

  if (length(retry_idx) == 0) {
    cat(sprintf("❌ Error: Retry model %s not found in database\n", retry_model_name))
    return(list(
      status = "error",
      excluded_covariate = NA,
      message = "Retry model not found in database"
    ))
  }

  retry_row <- self$search_database[retry_idx, ]
  original_model_name <- retry_row$original_model[1]
  covariate_tested <- retry_row$covariate_tested[1]
  current_step <- retry_row$step_number[1]

  cat(sprintf("   Original: %s, Covariate: %s, Step: %d\n",
              original_model_name, covariate_tested, current_step))

  # Step 1: Update retry model status to "failed"
  self$search_database$status[retry_idx] <- "failed"
  self$search_database$excluded_from_step[retry_idx] <- TRUE
  self$search_database$completion_time[retry_idx] <- Sys.time()

  # Step 2: Update original model exclusion
  original_idx <- which(self$search_database$model_name == original_model_name)
  if (length(original_idx) > 0) {
    self$search_database$excluded_from_step[original_idx] <- TRUE
  }

  # Step 3: Create exclusion log
  log_messages <- c(
    paste("=== COVARIATE EXCLUSION RECORD ==="),
    paste("Timestamp:", Sys.time()),
    paste("Retry model:", retry_model_name, "→ FAILED"),
    paste("Original model:", original_model_name, "→ EXCLUDED"),
    paste("Covariate:", covariate_tested, "→ EXCLUDED FROM STEP", current_step),
    paste("Reason:", exclusion_reason),
    paste("Available for final phase: TRUE"),
    paste("=== END EXCLUSION RECORD ===")
  )

  # Save exclusion log
  log_file <- file.path(self$models_folder,
                        paste0("EXCLUSION_", gsub("run", "", retry_model_name),
                               "_", gsub("\\s+", "_", covariate_tested), ".txt"))
  writeLines(log_messages, log_file)

  cat(sprintf("✅ Excluded covariate '%s' from step %d\n", covariate_tested, current_step))
  cat(sprintf("   Log saved: %s\n", basename(log_file)))

  return(list(
    status = "excluded",
    excluded_covariate = covariate_tested,
    retry_model = retry_model_name,
    original_model = original_model_name,
    step_number = current_step,
    log_file = log_file
  ))
})
