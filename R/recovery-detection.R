# =============================================================================
# RECOVERY DETECTION
# File: R/recovery-detection.R
# Part of CovariateSearcher Package
# Estimation problem detection
# =============================================================================



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

