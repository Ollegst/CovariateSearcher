# =============================================================================
# SCM EXECUTION
# File: R/scm-execution.R
# Part of CovariateSearcher Package
# SCM step execution and model submission
# =============================================================================



#' Run Univariate Step
#'
#' @title Run univariate analysis: test each covariate individually from base model
#' @description Creates individual covariate models for testing in parallel.
#'   Each covariate is added to the base model separately.
#' @param search_state List containing covariate search state and configuration
#' @param base_model_id Character. Base model to test from
#' @param covariates_to_test Character vector. Covariate tags to test
#' @param step_name Character. Description for this step
#' @return List with created model information and updated search_state
#' @export
run_univariate_step <- function(search_state, base_model_id, covariates_to_test, step_name) {
  if (length(covariates_to_test) == 0) {
    cat("❌ No covariates to test\n")
    return(list(
      search_state = search_state,
      step_name = step_name,
      base_model = base_model_id,
      models_created = character(0),
      status = "no_covariates"
    ))
  }

  cat(sprintf("\n🔬 %s\n", step_name))
  cat(sprintf("Base model: %s\n", base_model_id))

  # FIX: Pre-compute the covariate names to avoid scoping issues
  covariate_names <- character(length(covariates_to_test))
  for (i in seq_along(covariates_to_test)) {
    covariate_names[i] <- search_state$tags[[covariates_to_test[i]]]
  }

  cat(sprintf("Testing %d covariates: %s\n",
              length(covariates_to_test),
              paste(covariate_names, collapse = ", ")))

  # Create models for each covariate
  created_models <- list()
  step_start_time <- Sys.time()
  if (nrow(search_state$search_database) == 0) {
    step_number <- 1
  } else {
    step_number <- max(search_state$search_database$step_number, na.rm = TRUE) + 1
  }

  cat("🔧 Creating test models...\n")

  for (i in seq_along(covariates_to_test)) {
    cov_tag <- covariates_to_test[i]
    cov_name <- search_state$tags[[cov_tag]]

    cat(sprintf("  [%d/%d] Testing %s (%s)... ", i, length(covariates_to_test), cov_tag, cov_name))

    tryCatch({
      # Use detailed logging function for comprehensive model creation logs
      result <- add_covariate_with_detailed_logging(search_state, base_model_id, cov_tag)

      # FUNCTIONAL FIX: Use returned search_state
      search_state <- result$search_state

      if (!is.null(result$model_name)) {
        model_name <- result$model_name
        created_models[[cov_tag]] <- model_name

        # Add step-specific information to database
        db_idx <- which(search_state$search_database$model_name == model_name)
        if (length(db_idx) > 0) {
          search_state$search_database$step_description[db_idx] <- step_name
          search_state$search_database$phase[db_idx] <- "forward_selection"
          search_state$search_database$step_number[db_idx] <- step_number
          search_state$search_database$covariate_tested[db_idx] <- cov_name
          search_state$search_database$action[db_idx] <- "add_single_covariate"
        }

        cat("✓\n")
      } else {
        cat("✗ Failed\n")
      }

    }, error = function(e) {
      cat(sprintf("✗ Error: %s\n", e$message))
    })
  }

  creation_time <- as.numeric(difftime(Sys.time(), step_start_time, units = "mins"))
  cat(sprintf("✅ Created %d test models in %.1f minutes\n",
              length(created_models), creation_time))

  return(list(
    search_state = search_state,
    step_name = step_name,
    base_model = base_model_id,
    models_created = unlist(created_models),
    covariate_tags = names(created_models),
    status = "models_created",
    creation_time = creation_time
  ))
}



#' Submit Models and Wait for Completion with Auto-Updates
#'
#' @title Submit models and wait for all to complete with automatic status updates
#' @description Submits a batch of models in parallel using bbr and monitors
#'   their completion status with 1-minute updates and timeout protection.
#' @param search_state List containing covariate search state and configuration
#' @param model_names Character vector. Model names to submit and monitor
#' @param step_name Character. Description of current step
#' @param max_wait_minutes Numeric. Maximum time to wait for completion (default: 60)
#' @param threads Numeric. Number of threads per model (uses config if NULL)
#' @param auto_submit Logical. Whether to automatically submit models (default: TRUE)
#' @return List with completion results and updated search_state
#' @export
submit_and_wait_for_step <- function(search_state, model_names, step_name,
                                     max_wait_minutes = 120, threads = NULL,
                                     auto_submit = TRUE) {
  if (length(model_names) == 0) {
    return(list(
      search_state = search_state,
      completed_models = character(0),
      failed_models = character(0),
      status = "no_models"
    ))
  }

  if (!auto_submit) {
    cat("⏭️  Skipping submission (auto_submit = FALSE)\n")
    return(list(
      search_state = search_state,
      completed_models = model_names,
      failed_models = character(0),
      status = "submission_skipped"
    ))
  }

  if (is.null(threads)) {
    threads <- search_state$search_config$threads
  }

  cat(sprintf("\n🚀 SUBMITTING %s\n", step_name))
  cat(sprintf("Models: %s\n", paste(model_names, collapse = ", ")))
  cat(sprintf("Threads per model: %d\n", threads))

  # Submit all models
  submission_results <- list()
  failed_submissions <- character()

  for (model_name in model_names) {
    cat(sprintf("  Submitting %s... ", model_name))

    tryCatch({
      model_path <- file.path(search_state$models_folder, model_name)

      # Check if model file exists
      if (!file.exists(paste0(model_path, ".ctl"))) {
        stop(sprintf("Model file %s.ctl not found", model_path))
      }

      # Read and submit model using bbr
      mod <- bbr::read_model(model_path)
      bbr::submit_model(mod, .bbi_args = list(threads = threads), .overwrite = TRUE)

      # Initially set submission time to current time (will be updated with actual start time)
      db_idx <- which(search_state$search_database$model_name == model_name)
      if (length(db_idx) > 0) {
        search_state$search_database$submission_time[db_idx] <- Sys.time()
        search_state$search_database$status[db_idx] <- "in_progress"
      }

      submission_results[[model_name]] <- "submitted"
      cat("✓\n")

    }, error = function(e) {
      failed_submissions <- c(failed_submissions, model_name)
      submission_results[[model_name]] <- paste("failed:", e$message)
      cat(sprintf("✗ %s\n", e$message))

      # Update database status for failed submission
      db_idx <- which(search_state$search_database$model_name == model_name)
      if (length(db_idx) > 0) {
        search_state$search_database$status[db_idx] <- "submission_failed"
      }
    })
  }

  successful_submissions <- setdiff(model_names, failed_submissions)
  cat(sprintf("📊 Submitted: %d successful, %d failed\n",
              length(successful_submissions), length(failed_submissions)))

  if (length(successful_submissions) == 0) {
    return(list(
      search_state = search_state,
      completed_models = character(0),
      failed_models = failed_submissions,
      status = "all_submissions_failed"
    ))
  }

  # Wait for all models to complete with 1-minute updates
  cat(sprintf("\n⏳ MONITORING WITH 1-MINUTE UPDATES (%d minutes max)\n", max_wait_minutes))

  start_time <- Sys.time()
  max_wait_time <- start_time + (max_wait_minutes * 60)
  update_count <- 0

  completed_models <- character(0)
  failed_models <- character(0)

  while (Sys.time() < max_wait_time) {
    update_count <- update_count + 1

    # 1-MINUTE UPDATE CYCLE
    cat(sprintf("\n[Update %d] Checking model statuses...\n", update_count))

    # Update all model statuses from NONMEM files
    search_state <- update_all_model_statuses(search_state)

    # Update submission and completion times with actual NONMEM timestamps
    for (model_name in successful_submissions) {
      db_idx <- which(search_state$search_database$model_name == model_name)
      if (length(db_idx) > 0) {

        # Extract actual timestamps from LST file
        timestamps <- extract_nonmem_timestamps(model_name, search_state$models_folder)

        # Update submission_time with actual start time if available
        if (!is.na(timestamps$start_time)) {
          search_state$search_database$submission_time[db_idx] <- timestamps$start_time
        }

        # Update completion_time with actual stop time if available
        if (!is.na(timestamps$stop_time)) {
          search_state$search_database$completion_time[db_idx] <- timestamps$stop_time
        }
      }
    }

    # Get current status of submitted models
    current_status <- search_state$search_database %>%
      filter(model_name %in% successful_submissions) %>%
      select(model_name, covariate_tested, status, ofv, delta_ofv, estimation_issue) %>%
      arrange(model_name)

    # Update tracking
    newly_completed <- current_status %>%
      filter(status == "completed", !model_name %in% completed_models) %>%
      pull(model_name)

    newly_failed <- current_status %>%
      filter(status %in% c("failed", "estimation_error"), !model_name %in% failed_models) %>%
      pull(model_name)

    completed_models <- unique(c(completed_models, newly_completed))
    failed_models <- unique(c(failed_models, newly_failed))

    # Count status types
    status_counts <- current_status %>% count(status)
    in_progress_count <- status_counts$n[status_counts$status == "in_progress"]
    if (is.na(in_progress_count)) in_progress_count <- 0

    # Print progress update
    elapsed_mins <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1)
    cat(sprintf("[%.1f min] Status: %d completed, %d failed, %d running\n",
                elapsed_mins, length(completed_models), length(failed_models), in_progress_count))

    # Show individual model status (if not too many models)
    if (nrow(current_status) <= 10) {
      for (i in 1:nrow(current_status)) {
        row <- current_status[i, ]
        status_icon <- case_when(
          row$status == "completed" ~ "✅",
          row$status == "in_progress" ~ "🔄",
          row$status %in% c("failed", "estimation_error") ~ "❌",
          TRUE ~ "❓"
        )

        cat(sprintf("  %s %s (%s)", status_icon, row$model_name, row$covariate_tested))
        if (!is.na(row$ofv)) {
          cat(sprintf(" - OFV: %.2f", row$ofv))
          if (!is.na(row$delta_ofv)) {
            cat(sprintf(", ΔOFV: %.2f", row$delta_ofv))
          }
        }
        cat("\n")
      }
    }

    # Check if all models are done
    if (in_progress_count == 0) {
      cat(sprintf("\n🏁 ALL MODELS FINISHED! (%d completed, %d failed)\n",
                  length(completed_models), length(failed_models)))
      break
    }

    # Save intermediate state every 5 updates
    if (update_count %% 5 == 0) {
      save_search_state(search_state, sprintf("monitoring_update_%d.rds", update_count))
      cat("💾 Progress saved\n")
    }

    # Wait 1 minute before next update
    if (in_progress_count > 0) {
      cat("⏱️  Waiting 1 minute for next update...\n")
      Sys.sleep(60)
    }
  }

  # Final status
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  all_failed <- c(failed_models, failed_submissions)

  if (Sys.time() >= max_wait_time) {
    still_running <- current_status %>%
      filter(status == "in_progress") %>%
      pull(model_name)

    cat(sprintf("⏰ Timeout reached after %.1f minutes\n", total_time))
    cat(sprintf("📊 Final Status: %d completed, %d failed, %d still running\n",
                length(completed_models), length(all_failed), length(still_running)))
  } else {
    cat(sprintf("✅ All models finished in %.1f minutes\n", total_time))
    cat(sprintf("📊 Final Status: %d completed, %d failed\n",
                length(completed_models), length(all_failed)))
  }

  return(list(
    search_state = search_state,
    completed_models = completed_models,
    failed_models = all_failed,
    still_running = setdiff(successful_submissions, c(completed_models, failed_models)),
    total_time_minutes = total_time,
    timed_out = Sys.time() >= max_wait_time,
    updates_performed = update_count,
    status = "completed"
  ))
}
