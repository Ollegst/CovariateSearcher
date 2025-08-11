# =============================================================================
# VALIDATION
# File: R/validation.R
# Part of CovariateSearcher Package
# Model validation and quality assessment
# =============================================================================



#' Validate Model Quality
#'
#' Comprehensive model quality assessment
#'
#' @param search_state List containing search state
#' @param model_name Character. Model name
#' @param rse_threshold Numeric. RSE threshold (default 50)
#' @param ofv_threshold Numeric. OFV significance threshold (default 3.84)
#' @return List with validation results
#' @export
validate_model_quality <- function(search_state, model_name, rse_threshold = 50, ofv_threshold = 3.84) {

  model_path <- file.path(search_state, model_name)

  # Extract basic results - CORRECTED ARGUMENTS
  results <- extract_model_results(search_state, model_name)

  validation <- list(#' Update Model Status from Files (Working Version)

    model_path = model_path,
    overall_status = "unknown",
    converged = FALSE,
    acceptable_rse = FALSE,
    significant_ofv = FALSE,
    issues = character(0),
    recommendations = character(0)
  )


  # Check convergence
  if (results$status == "completed") {
    validation$converged <- TRUE
  } else if (results$status == "completed_with_issues") {
    validation$converged <- TRUE
    validation$issues <- c(validation$issues, "has_estimation_issues")
  } else {
    validation$converged <- FALSE
    validation$issues <- c(validation$issues, "did_not_converge")
    validation$recommendations <- c(validation$recommendations, "retry_with_different_initial_estimates")
  }

  # Check OFV availability
  if (!is.na(results$ofv)) {
    validation$ofv <- results$ofv
  } else {
    validation$issues <- c(validation$issues, "no_ofv_available")
  }

  # Overall assessment
  if (validation$converged && length(validation$issues) == 0) {
    validation$overall_status <- "acceptable"
  } else if (validation$converged && length(validation$issues) <= 2) {
    validation$overall_status <- "acceptable_with_warnings"
  } else {
    validation$overall_status <- "not_acceptable"
  }

  return(validation)
}


#'
#' @param search_state List. Current search state
#' @param model_name Character. Model name to update
#' @return List with updated search_state
#' @export
update_model_status_from_files <- function(search_state, model_name) {

  # Extract results
  results <- extract_model_results(search_state, model_name)
  validation <- validate_model_quality(search_state, model_name)

  # Find model in database
  db_idx <- which(search_state$search_database$model_name == model_name)

  if (length(db_idx) == 0) {
    cat(sprintf("⚠️  Model %s not found in database\n", model_name))
    return(search_state)
  }

  # Update database
  search_state$search_database$status[db_idx] <- results$status
  search_state$search_database$ofv[db_idx] <- results$ofv
  search_state$search_database$completion_time[db_idx] <- Sys.time()

  # Add error information for failed models
  if (results$status == "failed") {
    search_state$search_database$estimation_issue[db_idx] <- "Model failed - check LST file"
  }

  cat(sprintf("✅ Updated %s: %s (OFV: %s)\n",
              model_name, results$status,
              ifelse(is.na(results$ofv), "NA", round(results$ofv, 2))))

  return(search_state)
}
#' Calculate Delta OFV
#'
#' Calculate OFV difference between models
#'
#' @param base_ofv Numeric. Base model OFV
#' @param test_ofv Numeric. Test model OFV
#' @param significance_threshold Numeric. Significance threshold (default 3.84)
#' @return List with delta OFV and significance
#' @export
calculate_delta_ofv <- function(base_ofv, test_ofv, significance_threshold = 3.84) {

  if (is.na(base_ofv) || is.na(test_ofv)) {
    return(list(
      delta_ofv = NA_real_,
      significant = FALSE,
      direction = NA_character_,
      error = "Missing OFV values"
    ))
  }

  delta_ofv <- base_ofv - test_ofv
  significant <- abs(delta_ofv) > significance_threshold

  direction <- if (delta_ofv > 0) "improvement" else "worse"

  return(list(
    delta_ofv = delta_ofv,
    significant = significant,
    direction = direction,
    threshold = significance_threshold,
    meets_threshold = significant && direction == "improvement"
  ))
}


#' Update Model Status from Files with Enhanced Error Detection
#'
#' @title Updates search database with enhanced results from NONMEM output
#' @description Enhanced version with detailed error reporting and LST excerpts
#' @param search_state List. Current search state
#' @param model_name Character. Model name to update
#' @return List with updated search_state
#' @export
update_model_status_from_files <- function(search_state, model_name) {

  model_path <- file.path(search_state$models_folder, model_name)

  # Get enhanced LST analysis
  lst_info <- read_nonmem_lst(model_path)

  # Get basic results
  results <- extract_model_results(search_state, model_name)

  # Find model in database
  db_idx <- which(search_state$search_database$model_name == model_name)

  if (length(db_idx) == 0) {
    cat(sprintf("⚠️  Model %s not found in database\n", model_name))
    return(list(search_state = search_state))
  }

  # Update database with enhanced information
  search_state$search_database$status[db_idx] <- lst_info$status
  search_state$search_database$ofv[db_idx] <- results$ofv
  search_state$search_database$estimation_issue[db_idx] <- lst_info$error_message
  search_state$search_database$completion_time[db_idx] <- Sys.time()

  # Print detailed status with error information
  if (lst_info$has_issues) {
    cat(sprintf("❌ Model %s FAILED: %s\n", model_name, lst_info$error_message))
    if (nchar(lst_info$error_excerpt) > 0) {
      cat("📄 LST file excerpt:\n")
      cat(paste0("  ", gsub("\n", "\n  ", lst_info$error_excerpt)), "\n")
    }
  } else {
    cat(sprintf("✅ Model %s completed: %s (OFV: %s)\n",
                model_name, lst_info$status,
                ifelse(is.na(results$ofv), "NA", round(results$ofv, 2))))
  }

  # Calculate delta OFV if parent exists and both models successful
  parent_model <- search_state$search_database$parent_model[db_idx]
  if (!is.na(parent_model)) {
    parent_idx <- which(search_state$search_database$model_name == parent_model)
    if (length(parent_idx) > 0) {
      parent_ofv <- search_state$search_database$ofv[parent_idx[1]]
      parent_status <- search_state$search_database$status[parent_idx[1]]
      current_ofv <- results$ofv
      current_status <- lst_info$status

      if (parent_status == "completed" && current_status == "completed") {
        # Both completed - normal delta OFV
        delta_result <- calculate_delta_ofv(parent_ofv, current_ofv)
        search_state$search_database$delta_ofv[db_idx] <- delta_result$delta_ofv

      } else if (parent_status == "failed" && current_status == "completed") {
        # Fixed a failed model - mark as major improvement
        search_state$search_database$delta_ofv[db_idx] <- 999999  # Large positive number
        cat(sprintf("🎉 Model %s FIXED failed parent %s!\n", model_name, parent_model))

      } else if (parent_status == "completed" && current_status == "failed") {
        # Broke a working model - mark as major deterioration
        search_state$search_database$delta_ofv[db_idx] <- -999999  # Large negative number
        cat(sprintf("💥 Model %s BROKE working parent %s\n", model_name, parent_model))

      } else {
        # Both failed or other combinations
        search_state$search_database$delta_ofv[db_idx] <- NA_real_
        cat(sprintf("⚠️ No meaningful OFV comparison: %s (%s) vs %s (%s)\n",
                    model_name, current_status, parent_model, parent_status))
      }
    }
  }
  return(search_state)
}


#' Update All Model Statuses
#'
#' Updates all models in database from NONMEM output files
#'
#' @param search_state List. Current search state
#' @return List with updated search_state
#' @export
update_all_model_statuses <- function(search_state) {

  cat("📊 Updating all model statuses from NONMEM output files...\n")

  models_to_update <- search_state$search_database$model_name

  for (model_name in models_to_update) {
    update_result <- update_model_status_from_files(search_state, model_name)
    search_state <- update_result$search_state
  }

  cat(sprintf("✅ Updated %d models\n", length(models_to_update)))
  return(search_state)
}
