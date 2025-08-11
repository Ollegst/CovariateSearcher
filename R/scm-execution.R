# =============================================================================
# SCM EXECUTION
# File: R/scm-execution.R
# Part of CovariateSearcher Package
# SCM step execution and model submission
# =============================================================================



#' Run Univariate Step (SIMPLIFIED DATABASE VERSION)
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
      attempted_covariates = character(0),
      successful_covariates = character(0),
      failed_covariates = character(0),
      status = "no_covariates"
    ))
  }

  cat(sprintf("\n🔬 %s\n", step_name))
  cat(sprintf("Base model: %s\n", base_model_id))

  # STEP 1: Validate all covariate tags BEFORE starting
  invalid_tags <- covariates_to_test[!covariates_to_test %in% names(search_state$tags)]
  if (length(invalid_tags) > 0) {
    stop("Invalid covariate tags found: ", paste(invalid_tags, collapse = ", "))
  }

  # Pre-compute the covariate names for display
  covariate_names <- sapply(covariates_to_test, function(tag) search_state$tags[[tag]])

  cat(sprintf("Testing %d covariates: %s\n",
              length(covariates_to_test),
              paste(covariate_names, collapse = ", ")))

  # STEP 2: Calculate the step number for THIS entire step
  step_number <- if (is.null(search_state$search_database) || nrow(search_state$search_database) == 0) {
    1L
  } else {
    current_max <- max(search_state$search_database$step_number, na.rm = TRUE)
    if (is.na(current_max) || is.infinite(current_max)) {
      1L
    } else {
      current_max + 1L
    }
  }

  cat(sprintf("🔢 Step number for all models in this step: %d\n", step_number))

  # STEP 3: Initialize tracking variables
  created_models <- character(0)        # model_name vector
  successful_covariates <- character(0) # covariate_tag vector
  failed_covariates <- character(0)     # covariate_tag vector
  failure_reasons <- list()             # detailed error messages
  step_start_time <- Sys.time()

  cat("🔧 Creating test models...\n")

  # STEP 4: Create models for each covariate
  for (i in seq_along(covariates_to_test)) {
    cov_tag <- covariates_to_test[i]
    cov_name <- covariate_names[i]

    cat(sprintf("  [%d/%d] Testing %s (%s)... ",
                i, length(covariates_to_test), cov_tag, cov_name))

    tryCatch({
      # Call add_covariate_to_model with the calculated step_number
      result <- add_covariate_to_model(
        search_state = search_state,
        base_model_id = base_model_id,
        covariate_tag = cov_tag,
        step_number = step_number
      )

      # Validate result structure
      if (is.null(result) || is.null(result$search_state)) {
        stop("add_covariate_to_model returned invalid result")
      }

      # Update search_state with the returned state
      search_state <- result$search_state

      if (result$status == "success") {
        model_name <- result$model_name
        created_models <- c(created_models, model_name)
        successful_covariates <- c(successful_covariates, cov_tag)
        cat("✓\n")
      } else {
        failed_covariates <- c(failed_covariates, cov_tag)
        failure_reasons[[cov_tag]] <- result$error_message
        cat("✗ Failed\n")
      }

    }, error = function(e) {
      cat(sprintf("✗ Error: %s\n", e$message))
      failed_covariates <- c(failed_covariates, cov_tag)
      failure_reasons[[cov_tag]] <- e$message
    })
  }

  # STEP 5: Calculate timing and generate summary
  creation_time <- as.numeric(difftime(Sys.time(), step_start_time, units = "mins"))

  successful_count <- length(successful_covariates)
  failed_count <- length(failed_covariates)

  cat(sprintf("✅ Step complete: %d models created, %d failed in %.1f minutes\n",
              successful_count, failed_count, creation_time))

  # Show failures if any
  if (failed_count > 0) {
    cat("❌ Failed models:\n")
    for (tag in failed_covariates) {
      cat(sprintf("  - %s: %s\n", tag, failure_reasons[[tag]]))
    }
  }

  # STEP 6: Validate database consistency (this should NEVER fail if our logic is correct)
  if (successful_count > 0) {
    db_step_numbers <- search_state$search_database$step_number[
      search_state$search_database$model_name %in% created_models
    ]

    # This is an ASSERTION - if it fails, our code has a bug
    inconsistent_steps <- db_step_numbers[db_step_numbers != step_number]
    if (length(inconsistent_steps) > 0) {
      stop(sprintf("INTERNAL ERROR: Inconsistent step numbers in database. Expected %d, found: %s",
                   step_number, paste(unique(inconsistent_steps), collapse = ", ")))
    }

    cat(sprintf("✅ Database consistency verified: All %d models have step_number = %d\n",
                successful_count, step_number))
  }

  return(list(
    search_state = search_state,
    step_name = step_name,
    base_model = base_model_id,
    step_number = step_number,
    models_created = created_models,                    # model names
    attempted_covariates = covariates_to_test,         # all attempted covariate tags
    successful_covariates = successful_covariates,     # successful covariate tags
    failed_covariates = failed_covariates,             # failed covariate tags
    failure_reasons = failure_reasons,                 # detailed error messages
    creation_time = creation_time,
    successful_count = successful_count,
    failed_count = failed_count,
    status = if (successful_count > 0) "models_created" else "all_failed"
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
                                     auto_submit = TRUE, auto_retry = TRUE) {
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
  if (auto_retry) {
    cat("🔄 Auto-retry: ENABLED\n")
  }

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

  # Initialize retry tracking before the monitoring loop
  already_processed_for_retry <- character(0)  # Track models we've already created retries for

  while (Sys.time() < max_wait_time) {
    update_count <- update_count + 1

    # 1-MINUTE UPDATE CYCLE
    cat(sprintf("\n[Update %d] Checking model statuses...\n", update_count))

    # Update all model statuses from NONMEM files
    search_state <- update_all_model_statuses(search_state)

    # ===================================================================
    # NEW: IMMEDIATE AUTO-RETRY LOGIC (FIXED VERSION)
    # ===================================================================
    if (auto_retry) {
      # Check for newly failed models this cycle (that we haven't processed yet)
      newly_failed_this_cycle <- setdiff(newly_failed, already_processed_for_retry)

      if (length(newly_failed_this_cycle) > 0) {
        cat(sprintf("🔧 IMMEDIATE RETRY: %d models just failed - creating retries\n",
                    length(newly_failed_this_cycle)))

        tryCatch({
          # Create models_with_issues for newly failed models
          models_with_issues <- list()
          for (model_name in newly_failed_this_cycle) {
            # Get the specific error from database
            model_row <- search_state$search_database[search_state$search_database$model_name == model_name, ]
            issue_type <- if (nrow(model_row) > 0 && !is.na(model_row$estimation_issue[1]) && model_row$estimation_issue[1] != "") {
              model_row$estimation_issue[1]
            } else {
              "failed_model"
            }

            models_with_issues[[model_name]] <- list(
              model_name = model_name,
              issue_type = issue_type,
              detection_time = Sys.time()
            )

            cat(sprintf("  %s: %s\n", model_name, issue_type))
          }

          # Use existing process_estimation_issues() function
          recovery_result <- process_estimation_issues(search_state, models_with_issues)
          search_state <- recovery_result$search_state

          # Handle retry models if created
          if (length(recovery_result$retry_models_created) > 0) {
            cat(sprintf("✅ Created %d retry models: %s\n",
                        length(recovery_result$retry_models_created),
                        paste(recovery_result$retry_models_created, collapse = ", ")))

            # Add retry models to monitoring list
            successful_submissions <- c(successful_submissions, recovery_result$retry_models_created)

            # Check if retry models need to be submitted (they might already be submitted by process_estimation_issues)
            for (retry_model in recovery_result$retry_models_created) {
              retry_row <- search_state$search_database[search_state$search_database$model_name == retry_model, ]

              # Only submit if status is "created" (not already submitted)
              if (nrow(retry_row) > 0 && retry_row$status[1] == "created") {
                cat(sprintf("🚀 Submitting retry model %s... ", retry_model))

                tryCatch({
                  model_path <- file.path(search_state$models_folder, retry_model)
                  mod <- bbr::read_model(model_path)
                  bbr::submit_model(mod, .bbi_args = list(threads = threads), .overwrite = TRUE)

                  # Update database status
                  db_idx <- which(search_state$search_database$model_name == retry_model)
                  if (length(db_idx) > 0) {
                    search_state$search_database$submission_time[db_idx] <- Sys.time()
                    search_state$search_database$status[db_idx] <- "in_progress"
                  }

                  cat("✓\n")

                }, error = function(submit_error) {
                  cat(sprintf("✗ Submission failed: %s\n", submit_error$message))

                  # Update status to reflect submission failure
                  db_idx <- which(search_state$search_database$model_name == retry_model)
                  if (length(db_idx) > 0) {
                    search_state$search_database$status[db_idx] <- "submission_failed"
                  }
                })
              } else {
                cat(sprintf("ℹ️  Retry model %s already submitted or has status: %s\n",
                            retry_model, if(nrow(retry_row) > 0) retry_row$status[1] else "unknown"))
              }
            }
          }

          # Mark these models as processed to avoid re-processing
          already_processed_for_retry <- c(already_processed_for_retry, newly_failed_this_cycle)

        }, error = function(retry_error) {
          cat(sprintf("❌ Error in retry processing: %s\n", retry_error$message))
          cat("Continuing with normal monitoring...\n")

          # Still mark as processed to avoid infinite retry attempts
          already_processed_for_retry <- c(already_processed_for_retry, newly_failed_this_cycle)
        })
      }
    }
    # ===================================================================
    # END IMMEDIATE AUTO-RETRY LOGIC
    # ===================================================================

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
