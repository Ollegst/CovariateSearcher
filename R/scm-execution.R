tryCatch({
  # Use detailed logging function for comprehensive# =============================================================================
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
    step_number <- max(search_state$search_database$step_number, na.rm = TRUE) + 1

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



  #' Submit Models and Wait for Completion
  #'
  #' @title Submit models and wait for all to complete before proceeding
  #' @description Submits a batch of models in parallel using bbr and monitors
  #'   their completion status with timeout protection.
  #' @param search_state List containing covariate search state and configuration
  #' @param model_names Character vector. Model names to submit and monitor
  #' @param step_name Character. Description of current step
  #' @param max_wait_minutes Numeric. Maximum time to wait for completion (default: 60)
  #' @param threads Numeric. Number of threads per model (uses config if NULL)
  #' @param auto_submit Logical. Whether to automatically submit models (default: TRUE)
  #' @return List with completion results and updated search_state
  #' @export
  submit_and_wait_for_step <- function(search_state, model_names, step_name,
                                       max_wait_minutes = 60, threads = NULL,
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
    failed_submissions <- character(0)

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
        bbr::submit_model(mod, .mode = "local", .threads = threads)

        # Update database status
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

    # Wait for all models to complete
    cat(sprintf("\n⏳ WAITING FOR COMPLETION (%d minutes max)\n", max_wait_minutes))

    start_time <- Sys.time()
    max_wait_time <- start_time + (max_wait_minutes * 60)
    last_status_print <- start_time - 61  # Force initial status print

    completed_models <- character(0)
    failed_models <- character(0)

    while (Sys.time() < max_wait_time) {

      # Update status for all models
      update_result <- update_all_model_statuses(search_state)
      search_state <- update_result$search_state

      # Check current status
      current_status <- search_state$search_database[
        search_state$search_database$model_name %in% successful_submissions,
        c("model_name", "status")]

      # Count status types
      completed_now <- current_status$model_name[current_status$status == "completed"]
      failed_now <- current_status$model_name[current_status$status == "failed"]
      in_progress <- current_status$model_name[current_status$status == "in_progress"]

      # Update tracking
      completed_models <- unique(c(completed_models, completed_now))
      failed_models <- unique(c(failed_models, failed_now))

      # Print status update every minute
      if (difftime(Sys.time(), last_status_print, units = "secs") >= 60) {
        elapsed_mins <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1)
        cat(sprintf("  [%.1f min] Status: %d completed, %d failed, %d running\n",
                    elapsed_mins, length(completed_models), length(failed_models),
                    length(in_progress)))
        last_status_print <- Sys.time()
      }

      # Check if all models are done (including submission failures)
      total_done <- length(completed_models) + length(failed_models) + length(failed_submissions)
      if (total_done >= length(model_names)) {
        break
      }

      # Wait before next check
      Sys.sleep(30)  # Check every 30 seconds
    }

    # Final status
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    all_failed <- c(failed_models, failed_submissions)

    if (Sys.time() >= max_wait_time) {
      still_running <- setdiff(successful_submissions, c(completed_models, failed_models))
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
      status = "completed"
    ))
  }
