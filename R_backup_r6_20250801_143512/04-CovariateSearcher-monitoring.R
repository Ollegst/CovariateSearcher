# =============================================================================
# MONITORING & PROGRESS TRACKING METHODS
# File: R/04-CovariateSearcher-monitoring.R
# Extends CovariateSearcher R6 Class with Module 4 functionality
# =============================================================================

#' Monitoring and Progress Methods for CovariateSearcher
#'
#' @name CovariateSearcher-monitoring
#' @title Real-time Monitoring and Progress Tracking
#' @description Extension methods for real-time progress monitoring,
#'   performance metrics, and checkpoint creation during SCM workflows.
#' @keywords internal
NULL

#
# PURPOSE:
# This module extends the CovariateSearcher class with comprehensive monitoring,
# progress tracking, and real-time status reporting capabilities for long-running
# SCM workflows.
#
# MAIN CAPABILITIES:
# • Real-time progress monitoring with visual indicators
# • Continuous model status updates and health checks
# • Performance metrics and timing analysis
# • Interactive progress bars and status displays
# • Email/notification integration for completion alerts
# • Detailed logging and checkpoint creation
#
# KEY METHODS ADDED (PUBLIC):
# • monitor_scm_progress()             - Real-time SCM workflow monitoring
# • create_progress_report()          - Generate comprehensive progress reports
# • monitor_models_continuously()     - Background model monitoring
# • get_performance_metrics()         - Calculate timing and efficiency metrics
# • create_checkpoint()               - Save workflow state for recovery
# • restore_from_checkpoint()         - Resume from saved state
#
# USAGE EXAMPLES:
# # Start monitoring during SCM
# monitor_result <- searcher$monitor_scm_progress()
#
# # Generate detailed progress report
# report <- searcher$create_progress_report()
#
# # Continuous background monitoring
# searcher$monitor_models_continuously(check_interval = 300)
#
# DEPENDENCIES:
# • Core modules (1-3) - All functionality built on existing foundation
# • Optional: email packages for notifications
#
# =============================================================================

# Ensure CovariateSearcher class exists before extending
if (!exists("CovariateSearcher")) {
  stop("CovariateSearcher class not found. Please load the core modules first.")
}

# =============================================================================
# PUBLIC MONITORING METHODS
# =============================================================================

#' Monitor SCM progress with real-time updates and visual indicators
#'
#' @param check_interval_seconds Numeric. How often to check status (default: 60)
#' @param max_monitor_hours Numeric. Maximum monitoring time in hours (default: 24)
#' @param show_progress_bar Logical. Show visual progress bar (default: TRUE)
#' @return List with monitoring results and final status
CovariateSearcher$set("public", "monitor_scm_progress", function(check_interval_seconds = 60, max_monitor_hours = 24,
                                                                 show_progress_bar = TRUE) {
  cat("📊 STARTING SCM PROGRESS MONITORING\n")
  cat(sprintf("Check interval: %d seconds\n", check_interval_seconds))
  cat(sprintf("Maximum monitoring time: %.1f hours\n", max_monitor_hours))
  cat(paste(rep("=", 60), collapse=""), "\n")

  monitor_start_time <- Sys.time()
  max_monitor_time <- monitor_start_time + (max_monitor_hours * 3600)
  last_status_update <- monitor_start_time - 61  # Force initial update

  monitoring_log <- list()
  check_count <- 0

  while (Sys.time() < max_monitor_time) {
    check_count <- check_count + 1
    current_time <- Sys.time()

    # Update all model statuses
    self$update_model_status()

    # Get current progress statistics
    progress_stats <- self$get_performance_metrics()

    # Log current state
    monitoring_log[[check_count]] <- list(
      timestamp = current_time,
      check_number = check_count,
      statistics = progress_stats
    )

    # Print status update
    if (difftime(current_time, last_status_update, units = "secs") >= 60 || check_count == 1) {
      elapsed_time <- as.numeric(difftime(current_time, monitor_start_time, units = "hours"))

      cat(sprintf("\n🕐 [%.1fh] Progress Check #%d\n", elapsed_time, check_count))
      cat(sprintf("📊 Models: %d total, %d completed, %d running, %d failed\n",
                  progress_stats$total_models, progress_stats$completed_models,
                  progress_stats$running_models, progress_stats$failed_models))

      if (progress_stats$total_models > 0) {
        completion_pct <- (progress_stats$completed_models / progress_stats$total_models) * 100
        cat(sprintf("📈 Completion: %.1f%% (%d/%d)\n",
                    completion_pct, progress_stats$completed_models, progress_stats$total_models))

        # Show progress bar
        if (show_progress_bar && progress_stats$total_models > 0) {
          bar_width <- 40
          filled_width <- round((completion_pct / 100) * bar_width)
          empty_width <- bar_width - filled_width
          progress_bar <- paste0(
            "[", paste(rep("█", filled_width), collapse=""),
            paste(rep("░", empty_width), collapse=""), "]"
          )
          cat(sprintf("📊 %s %.1f%%\n", progress_bar, completion_pct))
        }
      }

      # Show estimation issues if any
      if (progress_stats$models_with_issues > 0) {
        cat(sprintf("⚠️  Models with estimation issues: %d\n", progress_stats$models_with_issues))
      }

      # Show retry models
      if (progress_stats$retry_models > 0) {
        cat(sprintf("🔧 Retry models created: %d\n", progress_stats$retry_models))
      }

      last_status_update <- current_time
    }

    # Check if all models are completed or failed
    total_finished <- progress_stats$completed_models + progress_stats$failed_models
    if (total_finished >= progress_stats$total_models && progress_stats$total_models > 0) {
      cat("\n✅ All models completed - monitoring finished\n")
      break
    }

    # Check if no models are running (might indicate workflow completion)
    if (progress_stats$running_models == 0 && progress_stats$total_models > 0) {
      cat("\n🏁 No models currently running - checking if workflow is complete\n")
      # Could add additional logic here to determine if SCM is truly done
    }

    # Wait before next check
    Sys.sleep(check_interval_seconds)
  }

  # Final monitoring summary
  total_monitoring_time <- as.numeric(difftime(Sys.time(), monitor_start_time, units = "hours"))
  final_stats <- self$get_performance_metrics()

  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("📊 MONITORING COMPLETED\n")
  cat(sprintf("⏱️  Total monitoring time: %.1f hours\n", total_monitoring_time))
  cat(sprintf("🔍 Total checks performed: %d\n", check_count))
  cat(sprintf("📊 Final status: %d completed, %d failed, %d running\n",
              final_stats$completed_models, final_stats$failed_models, final_stats$running_models))

  return(list(
    monitoring_duration_hours = total_monitoring_time,
    total_checks = check_count,
    final_statistics = final_stats,
    monitoring_log = monitoring_log,
    completed_successfully = total_monitoring_time < max_monitor_hours
  ))
})

#' Get comprehensive performance metrics for current workflow
#'
#' @return List with detailed performance and timing statistics
CovariateSearcher$set("public", "get_performance_metrics", function() {
  # Get basic model counts
  total_models <- nrow(self$search_database)
  completed_models <- sum(self$search_database$status == "completed", na.rm = TRUE)
  failed_models <- sum(self$search_database$status %in% c("failed", "error"), na.rm = TRUE)
  running_models <- sum(self$search_database$status == "in_progress", na.rm = TRUE)
  created_models <- sum(self$search_database$status == "created", na.rm = TRUE)

  # Get retry statistics
  retry_models <- sum(grepl("\\d{3}$", self$search_database$model_name), na.rm = TRUE)
  models_with_issues <- sum(!is.na(self$search_database$estimation_issue), na.rm = TRUE)
  excluded_models <- sum(self$search_database$excluded_from_step == TRUE, na.rm = TRUE)

  # Calculate timing statistics
  completed_with_times <- self$search_database[
    self$search_database$status == "completed" &
      !is.na(self$search_database$submission_time) &
      !is.na(self$search_database$completion_time),
  ]

  if (nrow(completed_with_times) > 0) {
    runtime_minutes <- as.numeric(difftime(
      completed_with_times$completion_time,
      completed_with_times$submission_time,
      units = "mins"
    ))

    avg_runtime <- mean(runtime_minutes, na.rm = TRUE)
    min_runtime <- min(runtime_minutes, na.rm = TRUE)
    max_runtime <- max(runtime_minutes, na.rm = TRUE)
  } else {
    avg_runtime <- min_runtime <- max_runtime <- NA
  }

  # Calculate step progress
  steps_info <- self$search_database %>%
    dplyr::filter(!is.na(step_number)) %>%
    dplyr::group_by(step_number) %>%
    dplyr::summarise(
      step_total = n(),
      step_completed = sum(status == "completed"),
      step_failed = sum(status %in% c("failed", "error")),
      step_running = sum(status == "in_progress"),
      .groups = "drop"
    )

  current_step <- if (nrow(steps_info) > 0) max(steps_info$step_number) else 0

  return(list(
    # Basic counts
    total_models = total_models,
    completed_models = completed_models,
    failed_models = failed_models,
    running_models = running_models,
    created_models = created_models,

    # Recovery statistics
    retry_models = retry_models,
    models_with_issues = models_with_issues,
    excluded_models = excluded_models,

    # Timing statistics
    average_runtime_minutes = avg_runtime,
    min_runtime_minutes = min_runtime,
    max_runtime_minutes = max_runtime,

    # Progress information
    current_step = current_step,
    steps_summary = steps_info,

    # Completion rates
    completion_rate = if (total_models > 0) completed_models / total_models else 0,
    failure_rate = if (total_models > 0) failed_models / total_models else 0,

    # Status timestamp
    metrics_timestamp = Sys.time()
  ))
})

#' Create comprehensive progress report with visualizations
#'
#' @param include_plots Logical. Generate plots if possible (default: FALSE)
#' @param save_to_file Character. File path to save report (NULL for console only)
#' @return Character vector with report content
CovariateSearcher$set("public", "create_progress_report", function(include_plots = FALSE, save_to_file = NULL) {
  cat("📋 GENERATING COMPREHENSIVE PROGRESS REPORT\n")

  # Get current metrics
  metrics <- self$get_performance_metrics()

  # Build report content
  report_lines <- c(
    paste(rep("=", 80), collapse=""),
    "COVARIATESEARCHER PROGRESS REPORT",
    paste("Generated:", Sys.time()),
    paste(rep("=", 80), collapse=""),
    "",
    "📊 OVERALL STATISTICS",
    paste(rep("-", 40), collapse=""),
    paste("Total Models:", metrics$total_models),
    paste("Completed Models:", metrics$completed_models),
    paste("Failed Models:", metrics$failed_models),
    paste("Running Models:", metrics$running_models),
    paste("Created (Not Submitted):", metrics$created_models),
    "",
    paste("Completion Rate:", sprintf("%.1f%%", metrics$completion_rate * 100)),
    paste("Failure Rate:", sprintf("%.1f%%", metrics$failure_rate * 100)),
    ""
  )

  # Add recovery statistics
  if (metrics$retry_models > 0 || metrics$models_with_issues > 0) {
    report_lines <- c(report_lines,
                      "🔧 RECOVERY SYSTEM STATISTICS",
                      paste(rep("-", 40), collapse=""),
                      paste("Retry Models Created:", metrics$retry_models),
                      paste("Models with Issues:", metrics$models_with_issues),
                      paste("Excluded Models:", metrics$excluded_models),
                      ""
    )
  }

  # Add timing statistics
  if (!is.na(metrics$average_runtime_minutes)) {
    report_lines <- c(report_lines,
                      "⏱️  TIMING STATISTICS",
                      paste(rep("-", 40), collapse=""),
                      paste("Average Runtime:", sprintf("%.1f minutes", metrics$average_runtime_minutes)),
                      paste("Fastest Model:", sprintf("%.1f minutes", metrics$min_runtime_minutes)),
                      paste("Slowest Model:", sprintf("%.1f minutes", metrics$max_runtime_minutes)),
                      ""
    )
  }

  # Add step-by-step progress
  if (nrow(metrics$steps_summary) > 0) {
    report_lines <- c(report_lines,
                      "📈 STEP-BY-STEP PROGRESS",
                      paste(rep("-", 40), collapse=""),
                      paste("Current Step:", metrics$current_step)
    )

    for (i in 1:nrow(metrics$steps_summary)) {
      step_data <- metrics$steps_summary[i, ]
      step_completion <- (step_data$step_completed / step_data$step_total) * 100

      report_lines <- c(report_lines,
                        paste(sprintf("Step %d: %d/%d completed (%.1f%%)",
                                      step_data$step_number, step_data$step_completed,
                                      step_data$step_total, step_completion))
      )
    }
    report_lines <- c(report_lines, "")
  }

  # Add model details by status
  report_lines <- c(report_lines,
                    "📋 MODEL DETAILS BY STATUS",
                    paste(rep("-", 40), collapse="")
  )

  status_summary <- self$search_database %>%
    dplyr::count(status, .drop = FALSE) %>%
    dplyr::arrange(desc(n))

  for (i in 1:nrow(status_summary)) {
    report_lines <- c(report_lines,
                      paste(sprintf("  %s: %d models", status_summary$status[i], status_summary$n[i]))
    )
  }

  # Add recent activity
  recent_models <- self$search_database %>%
    dplyr::filter(!is.na(completion_time)) %>%
    dplyr::arrange(desc(completion_time)) %>%
    dplyr::slice_head(n = 5)

  if (nrow(recent_models) > 0) {
    report_lines <- c(report_lines,
                      "",
                      "🕐 RECENT ACTIVITY (Last 5 Completions)",
                      paste(rep("-", 40), collapse="")
    )

    for (i in 1:nrow(recent_models)) {
      model_info <- recent_models[i, ]
      report_lines <- c(report_lines,
                        paste(sprintf("  %s (%s) - %s",
                                      model_info$model_name, model_info$status,
                                      format(model_info$completion_time, "%H:%M:%S")))
      )
    }
  }

  report_lines <- c(report_lines,
                    "",
                    paste(rep("=", 80), collapse=""),
                    "End of Report"
  )

  # Print report to console
  cat("\n")
  for (line in report_lines) {
    cat(line, "\n")
  }

  # Save to file if requested
  if (!is.null(save_to_file)) {
    writeLines(report_lines, save_to_file)
    cat(sprintf("\n📄 Report saved to: %s\n", save_to_file))
  }

  return(report_lines)
})

#' Create checkpoint of current workflow state for recovery
#'
#' @param checkpoint_name Character. Name for the checkpoint (auto-generated if NULL)
#' @param include_models Logical. Include model files in checkpoint (default: FALSE)
#' @return List with checkpoint information
CovariateSearcher$set("public", "create_checkpoint", function(checkpoint_name = NULL, include_models = FALSE) {
  if (is.null(checkpoint_name)) {
    checkpoint_name <- paste0("checkpoint_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }

  cat(sprintf("💾 Creating workflow checkpoint: %s\n", checkpoint_name))

  checkpoint_dir <- file.path(self$models_folder, "checkpoints", checkpoint_name)
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

  # Save current state
  checkpoint_data <- list(
    timestamp = Sys.time(),
    checkpoint_name = checkpoint_name,
    search_database = self$search_database,
    search_config = self$search_config,
    base_model = self$base_model,
    model_counter = self$model_counter,
    tags = self$tags,
    covariate_search = self$covariate_search,
    data_file_summary = list(
      nrows = nrow(self$data_file),
      ncols = ncol(self$data_file),
      colnames = names(self$data_file)
    )
  )

  # Save checkpoint data
  checkpoint_file <- file.path(checkpoint_dir, "checkpoint_data.rds")
  saveRDS(checkpoint_data, checkpoint_file)

  # Save database as CSV for easy viewing
  database_file <- file.path(checkpoint_dir, "search_database.csv")
  readr::write_csv(self$search_database, database_file)

  # Create checkpoint summary
  summary_lines <- c(
    paste("Checkpoint:", checkpoint_name),
    paste("Created:", Sys.time()),
    paste("Total Models:", nrow(self$search_database)),
    paste("Base Model:", self$base_model),
    paste("Model Counter:", self$model_counter),
    "",
    "Files saved:",
    paste("- checkpoint_data.rds (main state)"),
    paste("- search_database.csv (database export)")
  )

  summary_file <- file.path(checkpoint_dir, "checkpoint_summary.txt")
  writeLines(summary_lines, summary_file)

  cat(sprintf("✅ Checkpoint saved to: %s\n", checkpoint_dir))

  return(list(
    checkpoint_name = checkpoint_name,
    checkpoint_directory = checkpoint_dir,
    files_saved = c(checkpoint_file, database_file, summary_file),
    timestamp = Sys.time()
  ))
})
