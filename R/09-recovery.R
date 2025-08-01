# =============================================================================
# RECOVERY SYSTEM - ESTIMATION ERROR HANDLING AND AUTOMATIC RETRY
# File: R/09-recovery.R
# Part of CovariateSearcher Package
# Enhanced with functions from 05-recovery.R and 06-recovery.R
# =============================================================================

#' Read NONMEM .ext File and Detect Estimation Issues
#'
#' @title Parse .ext files and detect OFV > 10^10 and other estimation problems
#' @description Reads NONMEM .ext files to extract current OFV and detect
#'   estimation issues including boundary failures (OFV > 10^10), infinite values,
#'   and other numerical problems.
#' @param search_state List containing covariate search state and configuration
#' @param model_name Character. Model name (e.g., "run25")
#' @return List with status, current_ofv, iterations, and issue detection
#' @export
read_ext_file <- function(search_state, model_name) {
  ext_file <- file.path(search_state$models_folder, model_name, paste0(model_name, ".ext"))

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
          final_method <- "FOCE"
        } else if (grepl("Importance", method_line)) {
          final_method <- "IMP"
        } else if (grepl("Stochastic", method_line)) {
          final_method <- "SAEM"
        } else if (grepl("Laplacian", method_line)) {
          final_method <- "LAPLACE"
        } else {
          final_method <- paste("Method", i)
        }
      }

      # Find data lines in this table
      table_section <- ext_lines[table_start:table_end]
      data_lines <- table_section[!grepl("^#|^TABLE|^\\s*ITERATION\\s+", table_section) &
                                    nchar(trimws(table_section)) > 0]

      if (length(data_lines) > 0) {
        final_table_data <- data_lines
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
      } else if (abs(current_ofv) > 1e10) {  # KEY: OFV > 10^10 detection
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
}

#' Read NONMEM EXT File with Issue Detection (BBR Enhanced)
#'
#' @title Enhanced .ext file reader that detects estimation problems
#' @description Enhanced version from 06-recovery.R with BBR integration
#' @param model_name Character. Model name (e.g., "run25")
#' @param models_folder Character. Models folder path
#' @return List with detailed status and issue detection
#' @export
read_ext_file_with_issues <- function(model_name, models_folder = "models") {

  ext_file <- file.path(models_folder, model_name, paste0(model_name, ".ext"))

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

    # Find TABLE headers
    table_lines <- which(grepl("^TABLE", ext_lines))
    if (length(table_lines) == 0) table_lines <- 1

    # Process final table
    final_table_data <- NULL
    final_method <- "Unknown"

    for (i in seq_along(table_lines)) {
      table_start <- table_lines[i]
      table_end <- if (i < length(table_lines)) table_lines[i + 1] - 1 else length(ext_lines)

      # Extract method name
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

      # Find data lines
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

    # Parse last data line
    last_line <- tail(final_table_data, 1)
    values <- as.numeric(strsplit(trimws(last_line), "\\s+")[[1]])

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
    current_ofv <- values[length(values)]

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

    # Check for problematic values
    if (!has_issues) {
      problematic_values <- values[is.infinite(values) | is.nan(values) | abs(values) > 1e10]
      if (length(problematic_values) > 0) {
        has_issues <- TRUE
        issue_type <- "problematic_parameters"
      }
    }

    # Determine status
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
}

#' Detect Estimation Problems in Multiple Models
#'
#' @title Monitor multiple models for estimation problems
#' @description Checks a list of models for estimation issues by reading their
#'   .ext files and detecting boundary failures, convergence problems, etc.
#' @param search_state List containing covariate search state and configuration
#' @param model_names Character vector. Model names to monitor
#' @param check_interval_minutes Numeric. Check interval (default 30)
#' @return List with updated search_state and models with issues detected
#' @export
detect_estimation_problems <- function(search_state, model_names, check_interval_minutes = 30) {
  cat(sprintf("🔍 Monitoring %d models for estimation issues\n", length(model_names)))

  models_with_issues <- list()

  for (model_name in model_names) {
    cat(sprintf("  Checking %s... ", model_name))

    ext_data <- read_ext_file(search_state, model_name)

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
      db_idx <- which(search_state$search_database$model_name == model_name)
      if (length(db_idx) > 0) {
        search_state$search_database$estimation_issue[db_idx] <- ext_data$issue_type
        search_state$search_database$status[db_idx] <- "estimation_error"
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

  return(list(
    search_state = search_state,
    models_with_issues = models_with_issues
  ))
}

#' Create Retry Model with Adjusted THETA Values
#'
#' @title Create retry model with modified initial estimates
#' @description Creates a retry model (e.g., run2001 from run2) with THETA values
#'   adjusted from 0.1 to -0.1 for the problematic covariate.
#' @param search_state List containing covariate search state and configuration
#' @param original_model_name Character. Name of problematic model
#' @param issue_type Character. Type of estimation issue detected
#' @return List with retry model information and updated search_state
#' @export
create_retry_model <- function(search_state, original_model_name, issue_type = "estimation_error") {
  cat(sprintf("🔧 Creating retry model for %s (issue: %s)\n", original_model_name, issue_type))

  # Generate retry model name by adding "001" to original number
  model_number <- gsub("run", "", original_model_name)
  retry_model_name <- paste0("run", model_number, "001")

  cat(sprintf("  Original: %s → Retry: %s\n", original_model_name, retry_model_name))

  tryCatch({
    # Step 1: Get information about the original model
    original_row <- search_state$search_database[
      search_state$search_database$model_name == original_model_name, ]

    if (nrow(original_row) == 0) {
      stop("Original model not found in database: ", original_model_name)
    }

    cat("  ✓ Original model found in database\n")

    # Step 2: Create BBR model copy
    cat("  Creating BBR model copy...")

    original_model_path <- file.path(search_state$models_folder, original_model_name)

    retry_mod <- bbr::copy_model_from(
      .parent_mod = bbr::read_model(original_model_path),
      .new_model = retry_model_name,
      .inherit_tags = TRUE,
      .overwrite = TRUE
    )

    cat(" ✓\n")

    # Step 3: Modify THETA values in the model file
    cat("  Adjusting THETA values for latest covariate...")

    # Find the latest added covariate from the original model
    parent_model <- original_row$parent_model[1]

    if (!is.na(parent_model) && nchar(parent_model) > 0) {
      # Get covariates in parent vs original to find what was added
      parent_covs <- get_model_covariates(search_state, parent_model)
      original_covs <- get_model_covariates(search_state, original_model_name)
      added_covs <- setdiff(original_covs, parent_covs)

      if (length(added_covs) > 0) {
        latest_covariate <- added_covs[1]
        cat(sprintf(" (covariate: %s)", latest_covariate))

        # Call the THETA adjustment function
        adjustment_result <- adjust_theta_for_covariate(search_state, retry_model_name, latest_covariate)

        if (adjustment_result$success) {
          cat(" ✓\n")
        } else {
          cat(" ⚠️  THETA adjustment failed - using original values\n")
        }

      } else {
        cat(" ⚠️  No added covariate found - using original values\n")
      }
    } else {
      cat(" ⚠️  No parent model info - using original values\n")
    }

    # Step 4: Add retry model to database
    cat("  Adding to database...")

    new_row <- data.frame(
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
      tags = original_row$tags[1],
      submission_time = as.POSIXct(NA),
      completion_time = as.POSIXct(NA),
      retry_attempt = 1L,
      original_model = original_model_name,
      estimation_issue = issue_type,
      excluded_from_step = FALSE,
      stringsAsFactors = FALSE
    )

    search_state$search_database <- rbind(search_state$search_database, new_row)

    # Update original model status
    orig_idx <- which(search_state$search_database$model_name == original_model_name)
    if (length(orig_idx) > 0) {
      search_state$search_database$status[orig_idx] <- "error"
      search_state$search_database$estimation_issue[orig_idx] <- issue_type
    }

    cat(" ✓\n")

    cat(sprintf("✅ Retry model %s created successfully\n", retry_model_name))

    return(list(
      search_state = search_state,
      retry_model_name = retry_model_name,
      original_model_name = original_model_name,
      issue_type = issue_type,
      status = "created"
    ))

  }, error = function(e) {
    cat(sprintf("❌ Failed to create retry model: %s\n", e$message))

    return(list(
      search_state = search_state,
      retry_model_name = NULL,
      original_model_name = original_model_name,
      issue_type = issue_type,
      status = "failed",
      error = e$message
    ))
  })
}

#' Adjust THETA Values for Covariate
#'
#' @title Modify THETA values from 0.1 to -0.1 for specified covariate
#' @description Reads the model file and adjusts THETA initial estimates
#'   from 0.1 to -0.1 for the problematic covariate to improve convergence.
#' @param search_state List containing covariate search state and configuration
#' @param model_name Character. Model to modify (e.g., "run2001")
#' @param covariate_tag Character. Covariate tag that was added (e.g., "cov_cl_wt")
#' @return List with success status and details
#' @export
adjust_theta_for_covariate <- function(search_state, model_name, covariate_tag) {
  tryCatch({
    # Read the model file
    model_file_path <- file.path(search_state$models_folder, model_name, paste0(model_name, ".ctl"))

    if (!file.exists(model_file_path)) {
      return(list(success = FALSE, message = "Model file not found"))
    }

    modelcode <- readLines(model_file_path)

    # Find the covariate value from tags
    covariate_value <- search_state$tags[[covariate_tag]]
    if (is.null(covariate_value)) {
      return(list(success = FALSE, message = paste("Covariate tag", covariate_tag, "not found in tags")))
    }

    # Find matching covariate in covariate_search to get the beta name
    matching_row <- search_state$covariate_search[
      grepl(paste0("_", covariate_value, "$"), search_state$covariate_search$cov_to_test), ]

    if (nrow(matching_row) == 0) {
      return(list(success = FALSE, message = paste("No matching covariate found for", covariate_value)))
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
      return(list(success = FALSE, message = "No THETA lines found for covariate"))
    }

    # Write the modified model file
    writeLines(modelcode, model_file_path)

    cat(sprintf("    ✓ Modified %d THETA lines for covariate %s\n",
                theta_lines_modified, covariate_value))

    return(list(
      success = TRUE,
      theta_lines_modified = theta_lines_modified,
      covariate = covariate_value
    ))

  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error adjusting THETA:", e$message)))
  })
}

#' Process Estimation Issues and Trigger Recovery
#'
#' @title Process detected estimation issues and trigger appropriate recovery
#' @description Orchestrates the recovery process for models with estimation
#'   issues. Creates retries for original models or excludes covariates for failed retries.
#' @param search_state List containing covariate search state and configuration
#' @param models_with_issues List. Models with issues from detect_estimation_problems
#' @return List with recovery actions taken and updated search_state
#' @export
process_estimation_issues <- function(search_state, models_with_issues) {
  if (length(models_with_issues) == 0) {
    return(list(
      search_state = search_state,
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

      # Handle failed retry by excluding covariate
      exclusion_result <- handle_failed_retry(search_state, model_name, issue_info$issue_type)
      search_state <- exclusion_result$search_state

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

      # Create retry model
      retry_result <- create_retry_model(search_state, model_name, issue_info$issue_type)
      search_state <- retry_result$search_state

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
    search_state = search_state,
    retry_models_created = retry_models_created,
    excluded_covariates = excluded_covariates,
    recovery_actions = recovery_actions
  ))
}

#' Handle Failed Retry Model
#'
#' @title Handle failed retry model by excluding covariate from step
#' @description When a retry model fails, exclude the associated covariate
#'   from the current step but keep it available for final phase testing.
#' @param search_state List containing covariate search state and configuration
#' @param retry_model_name Character. Name of failed retry model (e.g., "run25001")
#' @param exclusion_reason Character. Reason for exclusion
#' @return List with exclusion information and updated search_state
#' @export
handle_failed_retry <- function(search_state, retry_model_name, exclusion_reason = "retry_failed") {
  cat(sprintf("🚫 Handling failed retry model: %s\n", retry_model_name))

  # Find the retry model in database
  retry_idx <- which(search_state$search_database$model_name == retry_model_name)

  if (length(retry_idx) == 0) {
    cat(sprintf("❌ Error: Retry model %s not found in database\n", retry_model_name))
    return(list(
      search_state = search_state,
      status = "error",
      excluded_covariate = NA,
      message = "Retry model not found in database"
    ))
  }

  retry_row <- search_state$search_database[retry_idx, ]
  original_model_name <- retry_row$original_model[1]
  covariate_tested <- retry_row$covariate_tested[1]
  current_step <- retry_row$step_number[1]

  cat(sprintf("   Original: %s, Covariate: %s, Step: %d\n",
              original_model_name, covariate_tested, current_step))

  # Step 1: Update retry model status to "failed"
  search_state$search_database$status[retry_idx] <- "failed"
  search_state$search_database$excluded_from_step[retry_idx] <- TRUE
  search_state$search_database$completion_time[retry_idx] <- Sys.time()

  # Step 2: Update original model exclusion
  original_idx <- which(search_state$search_database$model_name == original_model_name)
  if (length(original_idx) > 0) {
    search_state$search_database$excluded_from_step[original_idx] <- TRUE
  }

  cat(sprintf("✅ Excluded covariate '%s' from step %d\n", covariate_tested, current_step))

  return(list(
    search_state = search_state,
    status = "excluded",
    excluded_covariate = covariate_tested,
    retry_model = retry_model_name,
    original_model = original_model_name,
    step_number = current_step
  ))
}

#' Get Excluded Covariates
#'
#' @title Get list of covariates excluded from current step
#' @description Returns covariates that have been excluded due to estimation issues
#' @param search_state List containing covariate search state and configuration
#' @return Character vector of excluded covariate names
#' @export
get_excluded_covariates <- function(search_state) {

  excluded_models <- search_state$search_database[search_state$search_database$excluded_from_step == TRUE, ]

  if (nrow(excluded_models) > 0) {
    excluded_covs <- unique(excluded_models$covariate_tested)
    excluded_covs <- excluded_covs[!is.na(excluded_covs) & excluded_covs != ""]
    return(excluded_covs)
  } else {
    return(character(0))
  }
}

#' Generate Recovery Report
#'
#' @title Generate comprehensive recovery statistics and summary
#' @description Creates detailed recovery report showing retry success rates,
#'   excluded covariates, and overall system performance.
#' @param search_state List containing covariate search state and configuration
#' @return List with recovery statistics and summary information
#' @export
generate_recovery_report <- function(search_state) {

  # Overall statistics
  status_stats <- search_state$search_database %>%
    dplyr::count(status, .drop = FALSE) %>%
    dplyr::mutate(percentage = n * 100.0 / nrow(search_state$search_database))

  # Retry statistics
  retry_models <- search_state$search_database[grepl("\\d{3}$", search_state$search_database$model_name), ]
  retry_stats <- list(
    total_retries = nrow(retry_models),
    successful_retries = sum(retry_models$status == "completed", na.rm = TRUE),
    failed_retries = sum(retry_models$status %in% c("failed", "retry_failed"), na.rm = TRUE)
  )

  # Excluded covariates
  excluded_covs <- get_excluded_covariates(search_state)

  return(list(
    timestamp = Sys.time(),
    status_distribution = status_stats,
    retry_statistics = retry_stats,
    excluded_covariates = excluded_covs,
    recovery_success_rate = if(retry_stats$total_retries > 0) {
      retry_stats$successful_retries / retry_stats$total_retries * 100
    } else { 0 }
  ))
}
