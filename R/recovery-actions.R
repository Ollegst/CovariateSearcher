# =============================================================================
# RECOVERY ACTIONS
# File: R/recovery-actions.R
# Part of CovariateSearcher Package
# Recovery actions and retry model creation
# =============================================================================



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

    # Step 2.5: Update BBR YAML metadata FIRST (before any file modifications)
    cat("  Updating BBR YAML metadata...")

    # Get parent model info
    parent_model <- original_row$parent_model[1]

    if (!is.na(parent_model) && nchar(parent_model) > 0) {
      # Update based_on to point to original parent (run1), not failed model (run2)
      retry_mod$based_on <- parent_model
    }

    # Add initial retry note
    retry_mod <- bbr::add_notes(retry_mod, "Retry model with adjusted initial estimates")

    cat(" ✓\n")

    # Step 3: Modify THETA values in the model file
    cat("  Adjusting THETA values for latest covariate...")


    if (!is.na(parent_model) && nchar(parent_model) > 0) {
      # Get covariates in parent vs original to find what was added
      parent_covs <- get_model_covariates(search_state, parent_model)
      original_covs <- get_model_covariates(search_state, original_model_name)
      added_covs <- setdiff(original_covs, parent_covs)

      if (length(added_covs) > 0) {
        latest_covariate_name <- added_covs[1]  # e.g., "WT_CL"

        # Convert covariate name back to tag
        latest_covariate_tag <- NULL
        for (tag_name in names(search_state$tags)) {
          if (search_state$tags[[tag_name]] == latest_covariate_name) {
            latest_covariate_tag <- tag_name
            break
          }
        }

        if (!is.null(latest_covariate_tag)) {
          cat(sprintf(" (covariate: %s)", latest_covariate_name))

          # Call the THETA adjustment function
          adjustment_result <- adjust_theta_for_covariate(search_state, retry_model_name, latest_covariate_tag)

          if (adjustment_result$success) {
            cat(" ✓\n")
          } else {
            cat(" ⚠️  THETA adjustment failed - using original values\n")
          }
        } else {
          cat(" ⚠️  Could not find covariate tag - using original values\n")
        }

      } else {
        cat(" ⚠️  No added covariate found - using original values\n")
      }
    } else {
      cat(" ⚠️  No parent model info - using original values\n")
    }

    # Step 4: Create info log file with standard naming format
    cat("  Creating model info log...")

    if (exists("latest_covariate_name")) {
      # Get covariate info for the log
      matching_cov_for_log <- search_state$covariate_search[
        grepl(paste0("_", latest_covariate_name, "$"), search_state$covariate_search$cov_to_test), ]

      if (nrow(matching_cov_for_log) > 0) {
        create_model_info_log(
          search_state = search_state,
          model_name = retry_model_name,
          parent_model = parent_model,  # Use original parent (run1), not failed model (run2)
          covariate_name = latest_covariate_name,
          cov_info = matching_cov_for_log[1, ]
        )
      }
      cat(" ✓\n")
    } else {
      cat(" ⚠️  No covariate info for log\n")
    }

    # Step 5: Add retry model to database
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
    # FIXED: Correct file path construction
    model_file_path <- file.path(search_state$models_folder, paste0(model_name, ".ctl"))

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

