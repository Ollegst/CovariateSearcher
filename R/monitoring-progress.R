# =============================================================================
# MONITORING PROGRESS
# File: R/monitoring-progress.R
# Part of CovariateSearcher Package
# Progress monitoring and reporting
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



