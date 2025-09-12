# =============================================================================
# SCM EXECUTION
# File: R/scm-execution.R
# Part of CovariateSearcher Package
# SCM step execution and model submission
# =============================================================================



#' Run Univariate Step (ENHANCED WITH EXCLUSION FILTERING)
#' @param search_state List containing covariate search state and configuration
#' @param base_model_id Character. Base model to test from
#' @param covariates_to_test Character vector. Covariate tags to test (optional)
#' @param step_name Character. Description for this step
#' @param include_excluded Logical. Whether to include excluded covariates (default: TRUE for SCM)
#' @return List with created model information and updated search_state
#' @export
run_univariate_step <- function(search_state, base_model_id, covariates_to_test = NULL,
                                step_name, include_excluded = TRUE) {

  # If no covariates specified, get remaining ones with exclusion filtering
  if (is.null(covariates_to_test)) {
    covariates_to_test <- get_remaining_covariates(search_state, base_model_id, include_excluded)

    if (include_excluded) {
      cat(sprintf("🔍 Auto-selected %d covariates (including previously excluded)\n",
                  length(covariates_to_test)))
    } else {
      cat(sprintf("🔍 Auto-selected %d active covariates (excluding problematic ones)\n",
                  length(covariates_to_test)))
    }
  } else {
    # Filter provided covariates by exclusion status if not including excluded
    if (!include_excluded) {
      original_count <- length(covariates_to_test)
      available_covariates <- get_remaining_covariates(search_state, base_model_id, include_excluded = FALSE)
      covariates_to_test <- intersect(covariates_to_test, available_covariates)

      if (length(covariates_to_test) < original_count) {
        cat(sprintf("🔍 Filtered from %d to %d covariates (excluded problematic ones)\n",
                    original_count, length(covariates_to_test)))
      }
    }
  }

  if (length(covariates_to_test) == 0) {
    if (include_excluded) {
      cat("❌ No covariates available for testing (including excluded ones)\n")
    } else {
      cat("❌ No active covariates available for testing\n")
      cat("   💡 Use include_excluded=TRUE to test previously excluded covariates\n")
    }

    return(list(
      search_state = search_state,
      step_name = step_name,
      base_model = base_model_id,
      models_created = character(0),
      attempted_covariates = character(0),
      successful_covariates = character(0),
      failed_covariates = character(0),
      include_excluded = include_excluded,
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

  # Show exclusion status
  if (include_excluded) {
    excluded_in_this_step <- intersect(covariate_names, get_excluded_covariates(search_state))
    if (length(excluded_in_this_step) > 0) {
      cat(sprintf("📋 Including %d previously excluded covariates: %s\n",
                  length(excluded_in_this_step), paste(excluded_in_this_step, collapse = ", ")))
    }
  }

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
    include_excluded = include_excluded,
    status = if (successful_count > 0) "models_created" else "all_failed"
  ))
}


#' Submit Models and Wait for Completion with Auto-Updates (FIXED MONITORING)
#'
#' @title Submit models and wait for all to complete with corrected status tracking
#' @description Fixed version that properly handles retry model creation and status locking
#' @param search_state List containing covariate search state and configuration
#' @param model_names Character vector. Model names to submit and monitor
#' @param step_name Character. Description of current step
#' @param threads Numeric. Number of threads per model (uses config if NULL)
#' @param max_wait_minutes  Optional timeout - NULL means no limit
#' @param auto_submit Logical. Whether to automatically submit models (default: TRUE)
#' @param auto_retry Logical. Whether to enable automatic retry for failed models (default: TRUE)
#' @return List with completion results and updated search_state
#' @export
submit_and_wait_for_step <- function(search_state, model_names, step_name,
                                     max_wait_minutes = NULL,  # Optional timeout - NULL means no limit
                                     threads = NULL,
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
    cat("⭐️ Skipping submission (auto_submit = FALSE)\n")
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

      if (!file.exists(paste0(model_path, ".ctl"))) {
        stop(sprintf("Model file %s.ctl not found", model_path))
      }

      mod <- bbr::read_model(model_path)
      bbr::submit_model(mod, .bbi_args = list(threads = threads), .overwrite = TRUE)

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
  if (!is.null(max_wait_minutes)) {
    cat(sprintf("\n⏳ MONITORING WITH 1-MINUTE UPDATES (%d minutes max)\n", max_wait_minutes))
  } else {
    cat("\n⏳ MONITORING WITH 1-MINUTE UPDATES (no timeout)\n")
  }

  start_time <- Sys.time()
  update_count <- 0

  # Track models that are definitively completed/failed
  definitively_completed <- character(0)
  definitively_failed <- character(0)
  already_processed_for_retry <- character(0)
  active_monitoring_list <- successful_submissions

  # ADD THE MONITORING LOOP - this was missing!
  while (TRUE) {  # Or use: repeat {

    update_count <- update_count + 1

    # Check if we've exceeded max wait time (if set)
    elapsed_mins <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    if (!is.null(max_wait_minutes) && elapsed_mins > max_wait_minutes) {
      cat(sprintf("\n⏱️ TIMEOUT: Exceeded maximum wait time of %d minutes\n", max_wait_minutes))
      break
    }

    cat(sprintf("\n[Update %d] Checking model statuses...\n", update_count))

    # Update all model statuses from NONMEM files
    search_state <- update_all_model_statuses(search_state)

    # Update submission and completion times with actual NONMEM timestamps
    for (model_name in active_monitoring_list) {
      db_idx <- which(search_state$search_database$model_name == model_name)
      if (length(db_idx) > 0) {
        timestamps <- extract_nonmem_timestamps(model_name, search_state$models_folder)

        if (!is.na(timestamps$start_time)) {
          search_state$search_database$submission_time[db_idx] <- timestamps$start_time
        }

        if (!is.na(timestamps$stop_time)) {
          search_state$search_database$completion_time[db_idx] <- timestamps$stop_time
        }
      }
    }

    current_status <- search_state$search_database %>%
      dplyr::filter(!is.na(model_name)) %>%  # Get ALL models, not just current step
      dplyr::select(model_name, covariate_tested, status, ofv, delta_ofv, estimation_issue, step_number, parent_model) %>%
      dplyr::mutate(model_num = as.numeric(gsub("^run", "", model_name))) %>%  # Extract model number for sorting
      dplyr::arrange(step_number, model_num) %>%  # Sort by step FIRST, then model number
      dplyr::select(-model_num)  # Remove the temporary column

    # Display ALL models grouped by status
    cat("📊 Model Status Summary:\n")

    # Show completed models
    completed_display <- current_status[current_status$status == "completed", , drop = FALSE]
    if (!is.null(completed_display) && nrow(completed_display) > 0) {
      cat("--- Completed Models ---\n")
      for (i in seq_len(nrow(completed_display))) {
        row <- completed_display[i, ]
        step_prefix <- if (!is.null(row$step_number) && !is.na(row$step_number)) {
          sprintf("[Step %d] ", row$step_number)
        } else ""

        # Get parent OFV for better display
        parent_ofv <- NA
        if (!is.null(row$parent_model) && !is.na(row$parent_model)) {
          parent_idx <- which(search_state$search_database$model_name == row$parent_model)
          if (length(parent_idx) > 0) {
            parent_ofv <- search_state$search_database$ofv[parent_idx[1]]
          }
        }

        delta_display <- if (!is.null(row$delta_ofv) && !is.na(row$delta_ofv)) {
          if (!is.na(parent_ofv)) {
            sprintf("OFV %.2f → %.2f (ΔOFV: %+.2f)", parent_ofv, row$ofv, row$delta_ofv)
          } else {
            sprintf("ΔOFV: %+.2f", row$delta_ofv)
          }
        } else if (!is.null(row$ofv) && !is.na(row$ofv)) {
          sprintf("OFV: %.2f", row$ofv)
        } else {
          "OFV: N/A"
        }

        cat(sprintf("%s✅ Model %s (%s): %s\n",
                    step_prefix, row$model_name, row$covariate_tested, delta_display))
      }
    }

    # Show failed models
    failed_display <- current_status[current_status$status %in% c("failed", "estimation_error"), , drop = FALSE]
    if (!is.null(failed_display) && nrow(failed_display) > 0) {
      cat("--- Failed Models ---\n")
      for (i in seq_len(nrow(failed_display))) {
        row <- failed_display[i, ]
        step_prefix <- if (!is.null(row$step_number) && !is.na(row$step_number)) {
          sprintf("[Step %d] ", row$step_number)
        } else ""

        issue_display <- if (!is.null(row$estimation_issue) && !is.na(row$estimation_issue) && row$estimation_issue != "") {
          sprintf(" - %s", row$estimation_issue)
        } else ""

        cat(sprintf("%s❌ Model %s (%s)%s\n",
                    step_prefix, row$model_name, row$covariate_tested, issue_display))
      }
    }

    # Show running models
    running_display <- current_status[!(current_status$status %in% c("completed", "failed", "estimation_error")), , drop = FALSE]
    if (!is.null(running_display) && nrow(running_display) > 0) {
      cat("--- Running Models ---\n")
      for (i in seq_len(nrow(running_display))) {
        row <- running_display[i, ]
        step_prefix <- if (!is.null(row$step_number) && !is.na(row$step_number)) {
          sprintf("[Step %d] ", row$step_number)
        } else ""

        # Calculate elapsed time
        db_idx <- which(search_state$search_database$model_name == row$model_name)
        elapsed_display <- ""
        if (length(db_idx) > 0) {
          submission_time <- search_state$search_database$submission_time[db_idx[1]]
          if (!is.null(submission_time) && !is.na(submission_time)) {
            elapsed_mins <- as.numeric(difftime(Sys.time(), submission_time, units = "mins"))
            if (!is.na(elapsed_mins)) {
              elapsed_display <- sprintf(" (%.1f min)", elapsed_mins)
            }
          }
        }

        cat(sprintf("%s🔄 Model %s (%s)%s\n",
                    step_prefix, row$model_name, row$covariate_tested, elapsed_display))
      }
    }

    # Calculate summary stats
    completed_count <- if (!is.null(completed_display)) nrow(completed_display) else 0
    failed_count <- if (!is.null(failed_display)) nrow(failed_display) else 0
    running_count <- if (!is.null(running_display)) nrow(running_display) else 0

    cat(sprintf("\n📈 Summary: %d/%d completed, %d failed, %d running\n",
                completed_count, length(model_names), failed_count, running_count))


    # Identify NEW completions and failures
    newly_completed <- tryCatch({
      completed_models <- current_status$model_name[current_status$status == "completed"]
      setdiff(completed_models, definitively_completed)
    }, error = function(e) {
      character(0)
    })

    newly_failed <- tryCatch({
      failed_models <- current_status$model_name[current_status$status %in% c("failed", "estimation_error")]
      candidates <- setdiff(failed_models, definitively_failed)
      setdiff(candidates, already_processed_for_retry)
    }, error = function(e) {
      character(0)
    })

    # Update definitive tracking
    if (length(newly_completed) > 0) {
      definitively_completed <- c(definitively_completed, newly_completed)
      cat(sprintf("✅ NEW completions: %s\n", paste(newly_completed, collapse = ", ")))
    }

    if (length(newly_failed) > 0) {
      cat(sprintf("❌ NEW failures: %s\n", paste(newly_failed, collapse = ", ")))

      # Lock failed model status
      for (failed_model in newly_failed) {
        db_idx <- which(search_state$search_database$model_name == failed_model)
        if (length(db_idx) > 0) {
          search_state$search_database$status[db_idx] <- "failed"
          cat(sprintf("🔒 Locked %s status as 'failed'\n", failed_model))
        }
      }

      definitively_failed <- c(definitively_failed, newly_failed)
    }

    # AUTO-RETRY LOGIC
    if (auto_retry && length(newly_failed) > 0) {
      cat(sprintf("🔧 IMMEDIATE RETRY: %d models just failed - creating retries\n", length(newly_failed)))

      tryCatch({
        models_with_issues <- list()
        for (model_name in newly_failed) {
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

        # Process estimation issues to create retry models
        recovery_result <- process_estimation_issues(search_state, models_with_issues)
        search_state <- recovery_result$search_state

        if (length(recovery_result$retry_models_created) > 0) {
          cat(sprintf("✅ Created %d retry models: %s\n",
                      length(recovery_result$retry_models_created),
                      paste(recovery_result$retry_models_created, collapse = ", ")))

          # Add retry models to monitoring list
          active_monitoring_list <- c(active_monitoring_list, recovery_result$retry_models_created)

          # Submit retry models
          for (retry_model in recovery_result$retry_models_created) {
            retry_row <- search_state$search_database[search_state$search_database$model_name == retry_model, ]

            if (nrow(retry_row) > 0 && retry_row$status[1] == "created") {
              cat(sprintf("🚀 Submitting retry model %s... ", retry_model))

              tryCatch({
                model_path <- file.path(search_state$models_folder, retry_model)
                mod <- bbr::read_model(model_path)
                bbr::submit_model(mod, .bbi_args = list(threads = threads), .overwrite = TRUE)

                db_idx <- which(search_state$search_database$model_name == retry_model)
                if (length(db_idx) > 0) {
                  search_state$search_database$submission_time[db_idx] <- Sys.time()
                  search_state$search_database$status[db_idx] <- "in_progress"
                }

                cat("✓\n")

              }, error = function(submit_error) {
                cat(sprintf("✗ Submission failed: %s\n", submit_error$message))

                db_idx <- which(search_state$search_database$model_name == retry_model)
                if (length(db_idx) > 0) {
                  search_state$search_database$status[db_idx] <- "submission_failed"
                }
              })
            }
          }
        }

        already_processed_for_retry <- c(already_processed_for_retry, newly_failed)

      }, error = function(retry_error) {
        cat(sprintf("❌ Error in retry processing: %s\n", retry_error$message))
        already_processed_for_retry <- c(already_processed_for_retry, newly_failed)
      })
    }

    # Get updated status after retry model creation
    final_current_status <- tryCatch({
      search_state$search_database %>%
        dplyr::filter(model_name %in% active_monitoring_list) %>%
        dplyr::select(model_name, covariate_tested, status, ofv, delta_ofv, estimation_issue, step_number) %>%
        dplyr::arrange(model_name)
    }, error = function(e) {
      db_subset <- search_state$search_database[search_state$search_database$model_name %in% active_monitoring_list, ]
      db_subset[order(db_subset$model_name), c("model_name", "covariate_tested", "status", "ofv", "delta_ofv", "estimation_issue", "step_number")]
    })

    # Calculate progress
    final_status_counts <- tryCatch({
      completed_status <- final_current_status$status[final_current_status$status == "completed"]
      failed_status <- final_current_status$status[final_current_status$status %in% c("failed", "estimation_error")]

      list(
        completed = length(completed_status),
        failed = length(failed_status)
      )
    }, error = function(e) {
      list(completed = 0, failed = 0)
    })

    completed_count <- final_status_counts$completed
    failed_count <- final_status_counts$failed
    models_still_running <- length(active_monitoring_list) - completed_count - failed_count

    # Print progress update
    cat(sprintf("[%.1f min] Status: %d completed, %d failed, %d still running\n",
                elapsed_mins, completed_count, failed_count, models_still_running))

    # Show individual model status (if not too many)
    display_models <- tryCatch({
      display_subset <- final_current_status[!(final_current_status$model_name %in% definitively_failed &
                                                 final_current_status$status == "failed"), ]
      display_subset[order(display_subset$model_name), ]
    }, error = function(e) {
      final_current_status
    })

    if (nrow(display_models) <= 10) {
      for (i in 1:nrow(display_models)) {
        row <- display_models[i, ]

        status_icon <- if (row$status == "completed") {
          "✅"
        } else if (row$status %in% c("failed", "estimation_error")) {
          "❌"
        } else {
          "🔄"
        }

        step_prefix <- if (length(row$step_number) > 0 && !is.na(row$step_number)) {
          sprintf("[Step %d] ", row$step_number)
        } else {
          ""
        }

        cat(sprintf("  %s%s %s (%s)", step_prefix, status_icon, row$model_name, row$covariate_tested))
        if (!is.na(row$ofv)) {
          cat(sprintf(" - OFV: %.2f", row$ofv))
          if (!is.na(row$delta_ofv)) {
            cat(sprintf(", ΔOFV: %.2f", row$delta_ofv))
          }
        }
        cat("\n")
      }
    }

    # EXIT CONDITION - all active models finished
    if (models_still_running == 0) {
      cat(sprintf("\n🏁 ALL ACTIVE MODELS FINISHED! (%d completed, %d failed)\n",
                  completed_count, failed_count))
      break  # THIS BREAK IS NOW INSIDE A LOOP!
    }

    # Save intermediate state periodically
    if (update_count %% 5 == 0) {
      save_search_state(search_state, sprintf("monitoring_update_%d.rds", update_count))
      cat("💾 Progress saved\n")
    }

    # Wait 1 minute before next update
    cat("⏱️  Waiting 1 minute for next update...\n")
    Sys.sleep(60)

  }  # END OF WHILE LOOP

  # Final status calculation
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))

  final_all_status <- tryCatch({
    search_state$search_database %>%
      dplyr::filter(model_name %in% active_monitoring_list) %>%
      dplyr::select(model_name, status)
  }, error = function(e) {
    subset_db <- search_state$search_database[search_state$search_database$model_name %in% active_monitoring_list, ]
    subset_db[, c("model_name", "status")]
  })

  final_completed <- tryCatch({
    completed_models <- final_all_status$model_name[final_all_status$status == "completed"]
    if (is.null(completed_models)) character(0) else completed_models
  }, error = function(e) {
    character(0)
  })

  final_failed <- tryCatch({
    failed_models <- final_all_status$model_name[final_all_status$status %in% c("failed", "estimation_error")]
    if (is.null(failed_models)) character(0) else failed_models
  }, error = function(e) {
    character(0)
  })

  all_failed <- c(final_failed, failed_submissions)

  cat(sprintf("✅ All models finished in %.1f minutes\n", total_time))
  cat(sprintf("📊 Final Status: %d completed, %d failed\n",
              length(final_completed), length(all_failed)))

  # Make sure recovery_result exists before accessing it
  retry_models_created <- if (exists("recovery_result") && !is.null(recovery_result)) {
    recovery_result$retry_models_created
  } else {
    character(0)
  }

  return(list(
    search_state = search_state,
    completed_models = final_completed,
    failed_models = all_failed,
    still_running = setdiff(active_monitoring_list, c(final_completed, final_failed)),
    total_time_minutes = total_time,
    timed_out = !is.null(max_wait_minutes) && elapsed_mins > max_wait_minutes,
    updates_performed = update_count,
    retry_models_created = retry_models_created,
    status = if (!is.null(max_wait_minutes) && elapsed_mins > max_wait_minutes) "timed_out" else "completed"
  ))
}


