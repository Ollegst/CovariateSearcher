# =============================================================================
# SCM MONITORING AND PROGRESS TRACKING
# File: R/08-monitoring.R
# Part of CovariateSearcher Package
# =============================================================================

#' Get SCM Progress Summary
#'
#' @title Generate comprehensive progress summary for SCM workflow
#' @description Creates a detailed summary of current SCM progress including
#'   model counts, completion rates, and timing information.
#' @param search_state List containing covariate search state and configuration
#' @return List with detailed progress information
#' @export
get_scm_progress_summary <- function(search_state) {
  db <- search_state$search_database

  # Overall counts
  total_models <- nrow(db)
  completed_models <- sum(db$status == "completed", na.rm = TRUE)
  failed_models <- sum(db$status == "failed", na.rm = TRUE)
  in_progress_models <- sum(db$status == "in_progress", na.rm = TRUE)

  # Step-wise breakdown
  step_summary <- db %>%
    dplyr::group_by(step_number, step_description) %>%
    dplyr::summarise(
      total = dplyr::n(),
      completed = sum(status == "completed", na.rm = TRUE),
      failed = sum(status == "failed", na.rm = TRUE),
      in_progress = sum(status == "in_progress", na.rm = TRUE),
      completion_rate = round(100 * completed / total, 1),
      .groups = "drop"
    ) %>%
    dplyr::arrange(step_number)

  # Timing analysis
  timing_summary <- db %>%
    dplyr::filter(!is.na(submission_time) & !is.na(completion_time)) %>%
    dplyr::mutate(
      runtime_minutes = as.numeric(difftime(completion_time, submission_time, units = "mins"))
    ) %>%
    dplyr::summarise(
      models_with_timing = dplyr::n(),
      avg_runtime_minutes = round(mean(runtime_minutes, na.rm = TRUE), 1),
      median_runtime_minutes = round(median(runtime_minutes, na.rm = TRUE), 1),
      max_runtime_minutes = round(max(runtime_minutes, na.rm = TRUE), 1),
      .groups = "drop"
    )

  # Covariate performance
  covariate_performance <- db %>%
    dplyr::filter(!is.na(covariate_tested) & status == "completed") %>%
    dplyr::group_by(covariate_tested) %>%
    dplyr::summarise(
      times_tested = dplyr::n(),
      avg_delta_ofv = round(mean(delta_ofv, na.rm = TRUE), 2),
      best_delta_ofv = round(max(delta_ofv, na.rm = TRUE), 2),
      times_significant = sum(delta_ofv > search_state$search_config$forward_ofv_threshold, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(best_delta_ofv))

  return(list(
    overall = list(
      total_models = total_models,
      completed = completed_models,
      failed = failed_models,
      in_progress = in_progress_models,
      completion_rate = round(100 * completed_models / total_models, 1)
    ),
    by_step = step_summary,
    timing = timing_summary,
    covariate_performance = covariate_performance,
    last_updated = Sys.time()
  ))
}

#' Print SCM Progress Report
#'
#' @title Print formatted progress report to console
#' @description Displays a nicely formatted progress report with emoji indicators
#'   and key metrics for the current SCM workflow.
#' @param search_state List containing covariate search state and configuration
#' @param show_details Logical. Whether to show detailed step breakdown (default: TRUE)
#' @return Invisible NULL (prints to console)
#' @export
print_scm_progress_report <- function(search_state, show_details = TRUE) {
  progress <- get_scm_progress_summary(search_state)

  cat(paste0("\n", paste(rep("=", 60), collapse=""), "\n"))
  cat("📊 SCM PROGRESS REPORT\n")
  cat(paste0(paste(rep("=", 60), collapse=""), "\n"))

  # Overall status
  overall <- progress$overall
  cat(sprintf("🎯 Overall Status: %d models (%d completed, %d failed, %d running)\n",
              overall$total_models, overall$completed, overall$failed, overall$in_progress))
  cat(sprintf("📈 Completion Rate: %.1f%%\n", overall$completion_rate))

  # Timing information
  if (nrow(progress$timing) > 0) {
    timing <- progress$timing
    cat(sprintf("⏱️  Average Runtime: %.1f minutes (median: %.1f, max: %.1f)\n",
                timing$avg_runtime_minutes, timing$median_runtime_minutes, timing$max_runtime_minutes))
  }

  # Step breakdown
  if (show_details && nrow(progress$by_step) > 0) {
    cat("\n📋 Step Breakdown:\n")
    for (i in 1:nrow(progress$by_step)) {
      step <- progress$by_step[i, ]
      status_icon <- if (step$completion_rate == 100) "✅"
      else if (step$completion_rate > 0) "🔄"
      else "⏳"

      cat(sprintf("  %s Step %d: %s\n", status_icon, step$step_number, step$step_description))
      cat(sprintf("     %d/%d completed (%.1f%%), %d failed, %d running\n",
                  step$completed, step$total, step$completion_rate,
                  step$failed, step$in_progress))
    }
  }

  # Top performing covariates
  if (nrow(progress$covariate_performance) > 0) {
    cat("\n🏆 Top Performing Covariates:\n")
    top_covs <- head(progress$covariate_performance, 5)
    for (i in 1:nrow(top_covs)) {
      cov <- top_covs[i, ]
      significance_icon <- if (cov$times_significant > 0) "⭐" else "📊"
      cat(sprintf("  %s %s: ΔOFV=%.2f (tested %d times, significant %d times)\n",
                  significance_icon, cov$covariate_tested, cov$best_delta_ofv,
                  cov$times_tested, cov$times_significant))
    }
  }

  cat(sprintf("\n🕐 Last Updated: %s\n", format(progress$last_updated, "%Y-%m-%d %H:%M:%S")))
  cat(paste0(paste(rep("=", 60), collapse=""), "\n"))

  invisible(NULL)
}

#' Monitor SCM Progress in Real-Time
#'
#' @title Monitor SCM progress with periodic updates
#' @description Continuously monitors and prints SCM progress updates at
#'   specified intervals until all models are complete or timeout is reached.
#' @param search_state List containing covariate search state and configuration
#' @param update_interval_seconds Numeric. Seconds between updates (default: 60)
#' @param max_monitor_minutes Numeric. Maximum monitoring time (default: 300)
#' @param show_detailed_reports Logical. Whether to show full reports (default: FALSE)
#' @return Updated search_state with latest model statuses
#' @export
monitor_scm_progress <- function(search_state, update_interval_seconds = 60,
                                 max_monitor_minutes = 300, show_detailed_reports = FALSE) {

  cat("🔍 Starting SCM progress monitoring...\n")
  cat(sprintf("Update interval: %d seconds, Max monitoring time: %d minutes\n",
              update_interval_seconds, max_monitor_minutes))

  start_time <- Sys.time()
  max_monitor_time <- start_time + (max_monitor_minutes * 60)

  while (Sys.time() < max_monitor_time) {
    # Update all model statuses
    search_state <- update_all_model_statuses(search_state)

    # Get current progress
    progress <- get_scm_progress_summary(search_state)

    # Print update
    elapsed_mins <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1)

    if (show_detailed_reports) {
      print_scm_progress_report(search_state, show_details = TRUE)
    } else {
      # Compact progress update
      overall <- progress$overall
      cat(sprintf("[%.1f min] 📊 Status: %d/%d completed (%.1f%%), %d failed, %d running\n",
                  elapsed_mins, overall$completed, overall$total_models,
                  overall$completion_rate, overall$failed, overall$in_progress))
    }

    # Check if all models are done
    if (overall$in_progress == 0) {
      cat("✅ All models completed - monitoring complete\n")
      break
    }

    # Wait for next update
    Sys.sleep(update_interval_seconds)
  }

  if (Sys.time() >= max_monitor_time) {
    cat("⏰ Monitoring timeout reached\n")
  }

  # Final status update
  search_state <- update_all_model_statuses(search_state)
  print_scm_progress_report(search_state, show_details = TRUE)

  return(search_state)
}

#' Get Model Status Dashboard
#'
#' @title Create a comprehensive model status dashboard
#' @description Generates a detailed view of all models with their current
#'   status, timing, and key metrics for debugging and monitoring.
#' @param search_state List containing covariate search state and configuration
#' @param filter_status Character vector. Status types to include (default: all)
#' @param sort_by Character. Column to sort by (default: "model_name")
#' @return Data frame with model dashboard information
#' @export
get_model_status_dashboard <- function(search_state, filter_status = NULL, sort_by = "model_name") {
  db <- search_state$search_database

  # Filter by status if specified
  if (!is.null(filter_status)) {
    db <- db[db$status %in% filter_status, ]
  }

  # Create dashboard view
  dashboard <- db %>%
    dplyr::select(
      model_name, status, step_description, covariate_tested,
      parent_model, ofv, delta_ofv, rse_max,
      submission_time, completion_time
    ) %>%
    dplyr::mutate(
      runtime_minutes = ifelse(
        !is.na(submission_time) & !is.na(completion_time),
        round(as.numeric(difftime(completion_time, submission_time, units = "mins")), 1),
        NA_real_
      ),
      status_icon = dplyr::case_when(
        status == "completed" ~ "✅",
        status == "failed" ~ "❌",
        status == "in_progress" ~ "🔄",
        status == "created" ~ "📝",
        status == "submission_failed" ~ "⚠️",
        TRUE ~ "❓"
      ),
      significance = dplyr::case_when(
        is.na(delta_ofv) ~ "",
        delta_ofv > search_state$search_config$forward_ofv_threshold ~ "⭐ Significant",
        delta_ofv > 0 ~ "📈 Improvement",
        TRUE ~ "📉 No improvement"
      )
    )

  # Sort the results
  if (sort_by %in% names(dashboard)) {
    dashboard <- dashboard[order(dashboard[[sort_by]]), ]
  }

  return(dashboard)
}

#' Print Model Status Dashboard
#'
#' @title Print formatted model status dashboard to console
#' @description Displays a nicely formatted table showing all model statuses
#'   with key metrics and visual indicators.
#' @param search_state List containing covariate search state and configuration
#' @param filter_status Character vector. Status types to include (default: all)
#' @param max_models Numeric. Maximum number of models to show (default: 20)
#' @return Invisible NULL (prints to console)
#' @export
print_model_status_dashboard <- function(search_state, filter_status = NULL, max_models = 20) {
  dashboard <- get_model_status_dashboard(search_state, filter_status)

  if (nrow(dashboard) == 0) {
    cat("No models found matching the specified criteria.\n")
    return(invisible(NULL))
  }

  # Limit number of models shown
  if (nrow(dashboard) > max_models) {
    dashboard <- dashboard[1:max_models, ]
    showing_subset <- TRUE
  } else {
    showing_subset <- FALSE
  }

  cat(paste0("\n", paste(rep("=", 80), collapse=""), "\n"))
  cat("🔧 MODEL STATUS DASHBOARD\n")
  cat(paste0(paste(rep("=", 80), collapse=""), "\n"))

  # Print table header
  cat(sprintf("%-12s %-4s %-15s %-12s %-8s %-12s %s\n",
              "Model", "Stat", "Covariate", "ΔOFV", "Runtime", "Significance", "Step"))
  cat(paste(rep("-", 80), collapse=""), "\n")

  # Print each model
  for (i in 1:nrow(dashboard)) {
    row <- dashboard[i, ]

    cat(sprintf("%-12s %-4s %-15s %-12s %-8s %-12s %s\n",
                row$model_name,
                row$status_icon,
                ifelse(is.na(row$covariate_tested), "-", substr(row$covariate_tested, 1, 15)),
                ifelse(is.na(row$delta_ofv), "-", sprintf("%.2f", row$delta_ofv)),
                ifelse(is.na(row$runtime_minutes), "-", paste0(row$runtime_minutes, "m")),
                substr(row$significance, 1, 12),
                ifelse(is.na(row$step_description), "-", substr(row$step_description, 1, 20))))
  }

  if (showing_subset) {
    cat(sprintf("\n... showing first %d models out of %d total\n", max_models, nrow(dashboard) + max_models))
  }

  cat(paste0(paste(rep("=", 80), collapse=""), "\n"))

  invisible(NULL)
}
