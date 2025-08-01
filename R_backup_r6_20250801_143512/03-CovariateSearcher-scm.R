# =============================================================================
# SCM ORCHESTRATION METHODS - STEPWISE COVARIATE MODELING
# File: R/03-CovariateSearcher-scm.R
# Extends CovariateSearcher R6 Class with Module 3 functionality
# =============================================================================

#' SCM Orchestration Methods for CovariateSearcher
#'
#' @name CovariateSearcher-scm
#' @title Stepwise Covariate Modeling Methods
#' @description Extension methods for complete automated stepwise covariate
#'   modeling workflows with parallel execution and statistical evaluation.
#' @keywords internal
NULL

#
# PURPOSE:
# This module extends the CovariateSearcher class with complete stepwise
# covariate modeling (SCM) workflow orchestration. It implements the classic
# SCM algorithm with parallel execution, statistical evaluation, and integration
# with the recovery system.
#
# MAIN CAPABILITIES:
# • Complete automated SCM workflow execution
# • Parallel model creation and submission within steps
# • Sequential step execution with completion waiting
# • Statistical significance evaluation (ΔOFV, RSE thresholds)
# • Integration with recovery system for failed models
# • Dropped covariate retesting in final phase
#
# KEY METHODS ADDED (PUBLIC):
# • run_stepwise_covariate_modeling()  - Complete automated SCM workflow
# • run_univariate_step()             - Create models for one step
# • submit_and_wait_for_step()        - Parallel submission with monitoring
# • select_best_model()               - Statistical evaluation and selection
# • get_remaining_covariates()        - Find untested covariates
# • get_dropped_covariates()          - Find previously dropped covariates
#
# SCM ALGORITHM WORKFLOW:
# 1. Initial univariate analysis on base model (test all covariates)
# 2. Select best model based on statistical criteria (ΔOFV > 3.84)
# 3. Iterative forward steps: test remaining covariates from best model
# 4. Continue until no significant improvement found
# 5. Final phase: retest dropped covariates on final best model
# 6. All models within each step run in parallel
# 7. Wait for step completion before proceeding to next step
#
# STATISTICAL CRITERIA:
# • ΔOFV threshold: 3.84 (p < 0.05) for forward selection
# • RSE threshold: 50% maximum for parameter precision
# • Configurable thresholds via search_config
#
# USAGE EXAMPLES:
# # Run complete automated SCM
# results <- searcher$run_stepwise_covariate_modeling("run1")
#
# # Run single step manually
# step_result <- searcher$run_univariate_step("run1", covariates, "Step 1")
# completion <- searcher$submit_and_wait_for_step(models, "Step 1")
# selection <- searcher$select_best_model(completed_models)
#
# DEPENDENCIES:
# • Core module (Module 1) - Basic model management and database
# • Recovery module (Module 2) - Automatic error handling
# • BBR package - Model submission and monitoring
#
# =============================================================================

# Ensure CovariateSearcher class exists before extending
if (!exists("CovariateSearcher")) {
  stop("CovariateSearcher class not found. Please load the core module first.")
}

# =============================================================================
# PUBLIC SCM ORCHESTRATION METHODS
# =============================================================================

#' Get list of covariate tags that haven't been tested from base model
#'
#' @param base_model_id Character. Model to check current covariates against
#' @return Character vector of covariate tag names that can still be tested
CovariateSearcher$set("public", "get_remaining_covariates", function(base_model_id) {
  # Get current covariates in the base model
  current_covs <- self$get_model_covariates(base_model_id)

  # Get all available covariate tags
  all_cov_tags <- names(self$tags)[grepl("^cov_", names(self$tags))]

  # Return tags not currently in the model
  remaining_tags <- setdiff(all_cov_tags, current_covs)

  return(remaining_tags)
})

#' Get covariates that were tested but not selected in previous steps
#'
#' @param current_model_id Character. Current best model
#' @param tested_covariates Character vector. All covariate tags tested so far
#' @return Character vector of covariate tags that were dropped
CovariateSearcher$set("public", "get_dropped_covariates", function(current_model_id, tested_covariates) {
  current_covs <- self$get_model_covariates(current_model_id)
  dropped_covs <- setdiff(tested_covariates, current_covs)

  return(dropped_covs)
})

#' Run univariate analysis: test each covariate individually from base model
#'
#' @param base_model_id Character. Base model to test from
#' @param covariates_to_test Character vector. Covariate tags to test
#' @param step_name Character. Description for this step
#' @return List with created model information
CovariateSearcher$set("public", "run_univariate_step", function(base_model_id, covariates_to_test, step_name) {
  if (length(covariates_to_test) == 0) {
    cat("❌ No covariates to test\n")
    return(list(
      step_name = step_name,
      base_model = base_model_id,
      models_created = character(0),
      status = "no_covariates"
    ))
  }

  cat(sprintf("\n🔬 %s\n", step_name))
  cat(sprintf("Base model: %s\n", base_model_id))
  cat(sprintf("Testing %d covariates: %s\n",
              length(covariates_to_test),
              paste(sapply(covariates_to_test, function(x) self$tags[[x]]), collapse = ", ")))

  # Create models for each covariate
  created_models <- list()
  step_start_time <- Sys.time()
  step_number <- max(self$search_database$step_number, na.rm = TRUE) + 1

  cat("🔧 Creating test models...\n")

  for (i in seq_along(covariates_to_test)) {
    cov_tag <- covariates_to_test[i]
    cov_name <- self$tags[[cov_tag]]

    cat(sprintf("  [%d/%d] Testing %s (%s)... ", i, length(covariates_to_test), cov_tag, cov_name))

    tryCatch({
      # Use existing add_covariate method
      result <- self$add_covariate(base_model_id, cov_tag)

      if (!is.null(result)) {
        model_name <- result$model_name
        created_models[[cov_tag]] <- model_name

        # Add to search database with step information
        new_row <- tibble::tibble(
          model_name = model_name,
          step_description = step_name,
          phase = "forward_selection",
          step_number = step_number,
          parent_model = base_model_id,
          covariate_tested = cov_name,
          action = "add_single_covariate",
          ofv = NA_real_,
          delta_ofv = NA_real_,
          rse_max = NA_real_,
          status = "created",
          tags = list(c(self$get_model_covariates(base_model_id), cov_name)),
          submission_time = as.POSIXct(NA),
          completion_time = as.POSIXct(NA),
          retry_attempt = 0L,
          original_model = NA_character_,
          estimation_issue = NA_character_,
          excluded_from_step = FALSE
        )

        self$search_database <- dplyr::bind_rows(self$search_database, new_row)
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
    step_name = step_name,
    base_model = base_model_id,
    models_created = unlist(created_models),
    covariate_tags = names(created_models),
    status = "models_created",
    creation_time = creation_time
  ))
})

#' Submit models and wait for all to complete before proceeding
#'
#' @param model_names Character vector. Model names to submit and monitor
#' @param step_name Character. Description of current step
#' @param max_wait_minutes Numeric. Maximum time to wait for completion
#' @param threads Numeric. Number of threads per model (uses self$threads if NULL)
#' @param auto_submit Logical. Whether to automatically submit models
#' @return List with completion results
CovariateSearcher$set("public", "submit_and_wait_for_step", function(model_names, step_name, max_wait_minutes = 60,
                                                                     threads = NULL, auto_submit = TRUE) {
  if (length(model_names) == 0) {
    return(list(
      completed_models = character(0),
      failed_models = character(0),
      status = "no_models"
    ))
  }

  if (!auto_submit) {
    cat("⏭️  Skipping submission (auto_submit = FALSE)\n")
    return(list(
      completed_models = model_names,
      failed_models = character(0),
      status = "submission_skipped"
    ))
  }

  if (is.null(threads)) {
    threads <- self$threads
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
      model_path <- file.path(self$models_folder, model_name)
      mod <- bbr::read_model(model_path)

      # Submit using BBR
      bbr::submit_model(mod, .mode = "local", .threads = threads)

      # Update database status
      idx <- which(self$search_database$model_name == model_name)
      if (length(idx) > 0) {
        self$search_database$submission_time[idx] <- Sys.time()
        self$search_database$status[idx] <- "in_progress"
      }

      submission_results[[model_name]] <- "submitted"
      cat("✓\n")

    }, error = function(e) {
      failed_submissions <- c(failed_submissions, model_name)
      submission_results[[model_name]] <- paste("failed:", e$message)
      cat(sprintf("✗ %s\n", e$message))

      # Update database status for failed submission
      idx <- which(self$search_database$model_name == model_name)
      if (length(idx) > 0) {
        self$search_database$status[idx] <- "submission_failed"
      }
    })
  }

  successful_submissions <- setdiff(model_names, failed_submissions)
  cat(sprintf("📊 Submitted: %d successful, %d failed\n",
              length(successful_submissions), length(failed_submissions)))

  if (length(successful_submissions) == 0) {
    return(list(
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
    self$update_model_status()

    # Check current status
    current_status <- self$search_database[self$search_database$model_name %in% successful_submissions,
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
    completed_models = completed_models,
    failed_models = all_failed,
    still_running = setdiff(successful_submissions, c(completed_models, failed_models)),
    total_time_minutes = total_time,
    timed_out = Sys.time() >= max_wait_time,
    status = "completed"
  ))
})

#' Evaluate models and select the best one based on statistical criteria
#'
#' @param model_names Character vector. Model names to evaluate
#' @param ofv_threshold Numeric. OFV improvement threshold (uses config if NULL)
#' @param rse_threshold Numeric. Maximum RSE threshold (uses config if NULL)
#' @return List with best model and evaluation details
CovariateSearcher$set("public", "select_best_model", function(model_names, ofv_threshold = NULL, rse_threshold = NULL) {
  # Use config defaults if not specified
  if (is.null(ofv_threshold)) {
    ofv_threshold <- self$search_config$forward_ofv_threshold
  }
  if (is.null(rse_threshold)) {
    rse_threshold <- self$search_config$max_rse_threshold
  }

  cat(sprintf("\n📊 EVALUATING MODELS (ΔOFV > %.2f, RSE < %d%%)\n",
              ofv_threshold, rse_threshold))

  # Update all model information
  self$update_model_status()

  # Get completed models
  model_data <- self$search_database[self$search_database$model_name %in% model_names &
                                       self$search_database$status == "completed", ]

  if (nrow(model_data) == 0) {
    cat("❌ No completed models to evaluate\n")
    return(list(
      best_model = NULL,
      significant_models = character(0),
      evaluation_results = data.frame(),
      status = "no_completed_models"
    ))
  }

  # Calculate delta OFV for models without it
  for (i in 1:nrow(model_data)) {
    if (is.na(model_data$delta_ofv[i]) && !is.na(model_data$parent_model[i])) {
      parent_name <- model_data$parent_model[i]
      parent_ofv <- self$search_database$ofv[self$search_database$model_name == parent_name]

      if (length(parent_ofv) > 0 && !is.na(parent_ofv) && !is.na(model_data$ofv[i])) {
        delta_ofv <- parent_ofv - model_data$ofv[i]  # Positive = improvement
        model_data$delta_ofv[i] <- delta_ofv

        # Update in main database
        db_idx <- which(self$search_database$model_name == model_data$model_name[i])
        self$search_database$delta_ofv[db_idx] <- delta_ofv
      }
    }
  }

  # Evaluate each model
  evaluation_results <- model_data %>%
    dplyr::mutate(
      delta_ofv_significant = !is.na(delta_ofv) & delta_ofv > ofv_threshold,
      rse_acceptable = is.na(rse_max) | rse_max < rse_threshold,
      overall_significant = delta_ofv_significant & rse_acceptable,
      evaluation_notes = dplyr::case_when(
        is.na(delta_ofv) ~ "Delta OFV not available",
        !delta_ofv_significant & !rse_acceptable ~ "Poor OFV and high RSE",
        !delta_ofv_significant ~ "Insufficient OFV improvement",
        !rse_acceptable ~ "RSE too high",
        overall_significant ~ "Meets all criteria",
        TRUE ~ "Unknown issue"
      )
    ) %>%
    dplyr::arrange(desc(delta_ofv))

  # Identify significant models and best model
  significant_models <- evaluation_results$model_name[evaluation_results$overall_significant]

  best_model <- NULL
  if (length(significant_models) > 0) {
    # Best model is the one with highest delta OFV among significant models
    best_idx <- which.max(evaluation_results$delta_ofv[evaluation_results$overall_significant])
    best_model <- significant_models[best_idx]
  }

  # Print results
  cat("📋 Evaluation Results:\n")
  for (i in 1:nrow(evaluation_results)) {
    row <- evaluation_results[i, ]
    status_icon <- if (row$overall_significant) "✅" else "❌"
    best_icon <- if (row$model_name == best_model) " 🏆" else ""

    cat(sprintf("  %s %s: ΔOFV=%.2f, %s%s\n",
                status_icon, row$model_name,
                ifelse(is.na(row$delta_ofv), 0, row$delta_ofv),
                row$evaluation_notes, best_icon))
  }

  cat(sprintf("\n🎯 Summary: %d significant models found\n", length(significant_models)))
  if (!is.null(best_model)) {
    best_delta <- evaluation_results$delta_ofv[evaluation_results$model_name == best_model]
    cat(sprintf("🏆 Best model selected: %s (ΔOFV = %.2f)\n", best_model, best_delta))
  } else {
    cat("❌ No significant improvement found - keeping current base model\n")
  }

  return(list(
    best_model = best_model,
    significant_models = significant_models,
    evaluation_results = evaluation_results,
    criteria_used = list(
      ofv_threshold = ofv_threshold,
      rse_threshold = rse_threshold
    ),
    status = if (!is.null(best_model)) "best_model_found" else "no_improvement"
  ))
})

#' Execute complete stepwise covariate modeling algorithm
#'
#' @param base_model_id Character. Starting base model
#' @param max_forward_steps Numeric. Maximum number of forward steps
#' @param max_wait_minutes Numeric. Maximum wait time per step
#' @param auto_submit Logical. Whether to automatically submit models
#' @param ofv_threshold Numeric. OFV improvement threshold
#' @param rse_threshold Numeric. Maximum RSE threshold
#' @return List with complete SCM results
CovariateSearcher$set("public", "run_stepwise_covariate_modeling", function(base_model_id, max_forward_steps = NULL,
                                                                            max_wait_minutes = 60, auto_submit = TRUE,
                                                                            ofv_threshold = NULL, rse_threshold = NULL) {
  # Auto-calculate max_forward_steps based on available covariates
  if (is.null(max_forward_steps)) {
    available_covariates <- length(names(self$tags)[grepl("^cov_", names(self$tags))])
    max_forward_steps <- available_covariates + 2  # Buffer for edge cases
  }

  cat(paste0("\n", paste(rep("=", 80), collapse=""), "\n"))
  cat("🎯 STARTING STEPWISE COVARIATE MODELING (SCM)\n")
  cat(sprintf("Base model: %s\n", base_model_id))
  cat(sprintf("Max forward steps: %d\n", max_forward_steps))
  cat(sprintf("OFV threshold: %.2f\n", ofv_threshold %||% self$search_config$forward_ofv_threshold))
  cat(paste0(paste(rep("=", 80), collapse=""), "\n"))

  scm_start_time <- Sys.time()

  # Initialize tracking variables
  current_base_model <- base_model_id
  step_count <- 0
  all_tested_covariates <- character(0)
  step_results <- list()

  # STEP 1: Initial univariate analysis on base model
  cat(paste0("\n", "📍 STEP 1: INITIAL UNIVARIATE ANALYSIS\n"))
  step_count <- step_count + 1

  remaining_covariates <- self$get_remaining_covariates(current_base_model)

  if (length(remaining_covariates) == 0) {
    cat("❌ No covariates available for testing\n")
    return(list(
      status = "no_covariates",
      final_model = current_base_model,
      steps_completed = 0
    ))
  }

  # Step 1a: Create univariate models
  step1_creation <- self$run_univariate_step(
    base_model_id = current_base_model,
    covariates_to_test = remaining_covariates,
    step_name = "Step 1: Initial Univariate Analysis"
  )

  if (step1_creation$status != "models_created") {
    cat("❌ Failed to create initial models\n")
    return(list(
      status = "step1_creation_failed",
      final_model = current_base_model,
      steps_completed = 0
    ))
  }

  # Step 1b: Submit and wait for completion
  step1_completion <- self$submit_and_wait_for_step(
    model_names = step1_creation$models_created,
    step_name = "Step 1 Models",
    max_wait_minutes = max_wait_minutes,
    auto_submit = auto_submit
  )

  # Step 1c: Select best model
  step1_selection <- self$select_best_model(
    model_names = step1_completion$completed_models,
    ofv_threshold = ofv_threshold,
    rse_threshold = rse_threshold
  )

  step_results[["step_1"]] <- list(
    creation = step1_creation,
    completion = step1_completion,
    selection = step1_selection
  )

  # Update tracking
  all_tested_covariates <- c(all_tested_covariates, step1_creation$covariate_tags)

  if (is.null(step1_selection$best_model)) {
    cat("🏁 No significant improvement found in initial analysis\n")
    cat(sprintf("Final model: %s (no covariates added)\n", current_base_model))

    scm_time <- as.numeric(difftime(Sys.time(), scm_start_time, units = "mins"))
    return(list(
      status = "no_improvement_step1",
      final_model = current_base_model,
      steps_completed = 1,
      step_results = step_results,
      total_time_minutes = scm_time
    ))
  }

  # Update current base model
  current_base_model <- step1_selection$best_model
  cat(sprintf("🎯 Step 1 complete - new base model: %s\n", current_base_model))

  # ITERATIVE FORWARD STEPS (Steps 2, 3, 4, ...)
  for (forward_step in 2:max_forward_steps) {

    cat(sprintf("\n📍 STEP %d: FORWARD SELECTION ITERATION\n", forward_step))

    # Get remaining covariates (not yet in current model)
    remaining_covariates <- self$get_remaining_covariates(current_base_model)

    if (length(remaining_covariates) == 0) {
      cat("✅ No more covariates to test - forward selection complete\n")
      break
    }

    # Create univariate models from current best model
    step_creation <- self$run_univariate_step(
      base_model_id = current_base_model,
      covariates_to_test = remaining_covariates,
      step_name = sprintf("Step %d: Forward Selection", forward_step)
    )

    if (step_creation$status != "models_created") {
      cat(sprintf("❌ Failed to create Step %d models\n", forward_step))
      break
    }

    # Submit and wait for completion
    step_completion <- self$submit_and_wait_for_step(
      model_names = step_creation$models_created,
      step_name = sprintf("Step %d Models", forward_step),
      max_wait_minutes = max_wait_minutes,
      auto_submit = auto_submit
    )

    # Select best model
    step_selection <- self$select_best_model(
      model_names = step_completion$completed_models,
      ofv_threshold = ofv_threshold,
      rse_threshold = rse_threshold
    )

    # Store results
    step_results[[sprintf("step_%d", forward_step)]] <- list(
      creation = step_creation,
      completion = step_completion,
      selection = step_selection
    )

    # Update tracking
    all_tested_covariates <- c(all_tested_covariates, step_creation$covariate_tags)

    # Check if we found improvement
    if (is.null(step_selection$best_model)) {
      cat(sprintf("🏁 No significant improvement found in Step %d\n", forward_step))
      break
    }

    # Update current base model
    current_base_model <- step_selection$best_model
    cat(sprintf("🎯 Step %d complete - new base model: %s\n", forward_step, current_base_model))
    step_count <- forward_step
  }

  # FINAL STEP: Test dropped covariates
  cat(paste0("\n", "📍 FINAL STEP: TESTING DROPPED COVARIATES\n"))

  dropped_covariates <- self$get_dropped_covariates(current_base_model, all_tested_covariates)

  if (length(dropped_covariates) > 0) {
    cat(sprintf("Found %d previously dropped covariates to retest\n", length(dropped_covariates)))

    # Test dropped covariates
    final_creation <- self$run_univariate_step(
      base_model_id = current_base_model,
      covariates_to_test = dropped_covariates,
      step_name = "Final Step: Dropped Covariate Testing"
    )

    if (final_creation$status == "models_created") {
      # Submit and wait
      final_completion <- self$submit_and_wait_for_step(
        model_names = final_creation$models_created,
        step_name = "Final Step Models",
        max_wait_minutes = max_wait_minutes,
        auto_submit = auto_submit
      )

      # Select best model
      final_selection <- self$select_best_model(
        model_names = final_completion$completed_models,
        ofv_threshold = ofv_threshold,
        rse_threshold = rse_threshold
      )

      step_results[["final_step"]] <- list(
        creation = final_creation,
        completion = final_completion,
        selection = final_selection
      )

      # Update final model if improvement found
      if (!is.null(final_selection$best_model)) {
        current_base_model <- final_selection$best_model
        cat(sprintf("🎯 Final improvement found - final model: %s\n", current_base_model))
      }
    }
  } else {
    cat("No dropped covariates to retest\n")
  }

  # FINAL SUMMARY
  scm_time <- as.numeric(difftime(Sys.time(), scm_start_time, units = "mins"))

  cat(paste0("\n", paste(rep("=", 80), collapse=""), "\n"))
  cat("🏁 STEPWISE COVARIATE MODELING COMPLETE\n")
  cat(sprintf("⏱️  Total time: %.1f minutes\n", scm_time))
  cat(sprintf("📊 Steps completed: %d\n", step_count))
  cat(sprintf("🎯 Final model: %s\n", current_base_model))

  # Show final model covariates
  final_covariates <- self$get_model_covariates(current_base_model)
  if (length(final_covariates) > 0) {
    final_cov_names <- sapply(final_covariates, function(x) {
      if (x %in% names(self$tags)) self$tags[[x]] else x
    })
    cat(sprintf("📋 Final covariates: %s\n", paste(final_cov_names, collapse = ", ")))
  } else {
    cat("📋 Final covariates: None (base model retained)\n")
  }
  cat(paste0(paste(rep("=", 80), collapse=""), "\n"))

  return(list(
    status = "completed",
    final_model = current_base_model,
    base_model = base_model_id,
    steps_completed = step_count,
    step_results = step_results,
    total_time_minutes = scm_time,
    final_covariates = final_covariates
  ))
})
