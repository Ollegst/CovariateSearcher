# =============================================================================
# SCM FORWARD SELECTION
# File: R/scm-forward.R
# Part of CovariateSearcher Package
# Forward selection covariate modeling
# =============================================================================

#' Run SCM Forward Selection
#'
#' @title Execute complete forward selection covariate modeling workflow
#' @description Runs dynamic forward selection starting from base model,
#'   continuing until no significant improvements found or no covariates remain.
#'   Updates search database and configuration with results.
#' @param search_state List containing covariate search state and configuration
#' @param base_model_id Character. Starting base model (default: "run1")
#' @param max_steps Integer. Maximum steps (NULL = unlimited, bounded by covariates)
#' @param ofv_threshold Numeric. OFV improvement threshold (uses config if NULL)
#' @param rse_threshold Numeric. Maximum RSE threshold (uses config if NULL)
#' @param auto_submit Logical. Whether to automatically submit models (default: TRUE)
#' @param auto_retry Logical. Whether to enable automatic retry (default: TRUE)
#' @return List with updated search_state and forward selection results
run_scm_covariate_search_forward <- function(search_state,
                                             base_model_id = "run1",
                                             max_steps = NULL,
                                             ofv_threshold = NULL,
                                             rse_threshold = NULL,
                                             auto_submit = TRUE,
                                             auto_retry = TRUE) {

  # Use config defaults if not specified
  if (is.null(ofv_threshold)) {
    ofv_threshold <- search_state$search_config$forward_ofv_threshold
  }
  if (is.null(rse_threshold)) {
    rse_threshold <- search_state$search_config$max_rse_threshold
  }

  cat("🚀 STARTING SCM FORWARD SELECTION WORKFLOW\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  cat(sprintf("Base model: %s\n", base_model_id))
  cat(sprintf("OFV threshold: %.2f\n", ofv_threshold))
  cat(sprintf("RSE threshold: %d%%\n", rse_threshold))


  # Initialize workflow variables
  forward_start_time <- Sys.time()
  current_step <- 1
  current_best_model <- base_model_id
  forward_selection_active <- TRUE
  step_results <- list()

  # Main forward selection loop
  while (forward_selection_active) {

    # Check maximum steps limit
    if (!is.null(max_steps) && current_step > max_steps) {
      cat(sprintf("🛑 Maximum steps (%d) reached - stopping forward selection\n", max_steps))
      break
    }

    cat(sprintf("\n🎯 STEP %d: FORWARD SELECTION\n", current_step))
    cat(paste(rep("-", 40), collapse=""), "\n")

    # Find best model from previous step (or use base model for Step 1)
    if (current_step == 1) {
      current_best_model <- base_model_id
      cat(sprintf("Starting from base model: %s\n", current_best_model))
    } else {
      # Find best model from previous step
      cat(sprintf("🔍 Finding best model from Step %d...\n", current_step - 1))

      previous_step_models <- search_state$search_database[
        search_state$search_database$step_number == (current_step - 1) &
          search_state$search_database$status == "completed" &
          !is.na(search_state$search_database$delta_ofv), ]

      if (nrow(previous_step_models) == 0) {
        cat(sprintf("❌ No completed models found from Step %d\n", current_step - 1))
        forward_selection_active <- FALSE
        break
      }

      # Find model with highest ΔOFV
      best_idx <- which.max(previous_step_models$delta_ofv)
      current_best_model <- previous_step_models$model_name[best_idx]
      best_delta_ofv <- previous_step_models$delta_ofv[best_idx]
      best_covariate <- previous_step_models$covariate_tested[best_idx]

      cat(sprintf("🏆 Best Step %d Model: %s\n", current_step - 1, current_best_model))
      cat(sprintf("📈 ΔOFV improvement: %.2f\n", best_delta_ofv))
      cat(sprintf("🧬 Covariate: %s\n", best_covariate))
    }

    # Get remaining covariates for this step
    cat(sprintf("\n📋 Getting remaining covariates for Step %d...\n", current_step))

    remaining_covariates <- get_remaining_covariates(
      search_state = search_state,
      base_model_id = current_best_model,
      include_excluded = FALSE  # Exclude failed covariates
    )

    cat(sprintf("Remaining covariates: %d\n", length(remaining_covariates)))

    if (length(remaining_covariates) == 0) {
      cat("🏁 No more covariates to test - forward selection complete!\n")
      forward_selection_active <- FALSE
      break
    }

    cat("Covariates to test:", paste(remaining_covariates, collapse = ", "), "\n")

    # Create models for this step
    cat(sprintf("\n🔬 Creating Step %d models...\n", current_step))

    step_result <- run_univariate_step(
      search_state = search_state,
      base_model_id = current_best_model,
      covariates_to_test = NULL,  # Auto-select all remaining covariates
      step_name = sprintf("Step %d: Forward Selection", current_step),
      include_excluded = TRUE    # Exclude problematic covariates
    )

    # Update search_state
    search_state <- step_result$search_state

    if (step_result$status != "models_created") {
      cat(sprintf("❌ Step %d model creation failed: %s\n", current_step, step_result$status))
      forward_selection_active <- FALSE
      break
    }

    cat(sprintf("✅ Step %d: Created %d models successfully\n", current_step, step_result$successful_count))
    cat("Models created:", paste(step_result$models_created, collapse = ", "), "\n")

    # Submit and monitor models
    cat(sprintf("\n🚀 Submitting Step %d models...\n", current_step))

    step_submission <- submit_and_wait_for_step(
      search_state = search_state,
      model_names = step_result$models_created,
      step_name = sprintf("Step %d Models", current_step),
      auto_submit = auto_submit,
      auto_retry = auto_retry
    )

    # Update search_state
    search_state <- step_submission$search_state

    cat(sprintf("\n📊 Step %d Submission Results:\n", current_step))
    cat(sprintf("Completed: %d models\n", length(step_submission$completed_models)))
    cat(sprintf("Failed: %d models\n", length(step_submission$failed_models)))

    if (length(step_submission$completed_models) == 0) {
      cat(sprintf("❌ No models completed successfully in Step %d\n", current_step))
      forward_selection_active <- FALSE
      break
    }

    # Select best model from this step
    cat(sprintf("\n🏆 Selecting best Step %d model...\n", current_step))

    step_selection <- select_best_model(
      search_state = search_state,
      model_names = step_submission$completed_models,
      ofv_threshold = ofv_threshold,
      rse_threshold = rse_threshold
    )

    # Update search_state
    search_state <- step_selection$search_state

    # Store step results
    step_results[[sprintf("step_%d", current_step)]] <- list(
      creation = step_result,
      submission = step_submission,
      selection = step_selection,
      base_model = current_best_model,
      models_tested = length(step_result$models_created),
      models_completed = length(step_submission$completed_models)
    )

    if (is.null(step_selection$best_model)) {
      cat(sprintf("❌ No significant improvement found in Step %d\n", current_step))
      cat("🏁 Forward selection complete - no more improvements!\n")
      forward_selection_active <- FALSE
      break
    }

    # Show step results
    step_best_model <- step_selection$best_model
    cat(sprintf("🎯 Step %d Best Model: %s\n", current_step, step_best_model))

    # Get improvement information
    best_row <- search_state$search_database[
      search_state$search_database$model_name == step_best_model, ]

    if (nrow(best_row) > 0 && !is.na(best_row$delta_ofv)) {
      cat(sprintf("📈 ΔOFV improvement: %.2f\n", best_row$delta_ofv))
      cat(sprintf("🧬 Covariate added: %s\n", best_row$covariate_tested))
    }

    cat(sprintf("\n✅ Step %d completed successfully!\n", current_step))

    # Update for next iteration
    current_step <- current_step + 1

    # Save progress after each step
    save_search_state(search_state, sprintf("scm_forward_step_%d.rds", current_step - 1))
    cat(sprintf("💾 Progress saved to: scm_forward_step_%d.rds\n", current_step - 1))
  }

  # Calculate total time
  forward_time <- as.numeric(difftime(Sys.time(), forward_start_time, units = "mins"))

  # ===================================================================
  # FORWARD SELECTION SUMMARY AND RESULTS
  # ===================================================================

  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("🏁 FORWARD SELECTION COMPLETE!\n")
  cat(paste(rep("=", 60), collapse=""), "\n")

  # Find the final best model
  cat("🔍 Determining final best model...\n")

  # Get all completed models with valid ΔOFV from all steps
  all_completed_models <- search_state$search_database[
    search_state$search_database$status == "completed" &
      !is.na(search_state$search_database$delta_ofv) &
      search_state$search_database$step_number > 0, ]  # Exclude base model

  final_best_model <- base_model_id  # Fallback to base model
  final_best_step <- 0
  final_best_delta <- 0
  final_best_covariate <- "None"
  step_progression <- list()

  if (nrow(all_completed_models) > 0) {

    # Find overall best model (highest cumulative improvement)
    final_best_model <- current_best_model
    final_best_idx <- which(all_completed_models$model_name == final_best_model)
    final_best_step <- all_completed_models$step_number[final_best_idx]
    final_best_delta <- all_completed_models$delta_ofv[final_best_idx]
    final_best_covariate <- all_completed_models$covariate_tested[final_best_idx]

    cat(sprintf("🏆 FINAL BEST MODEL: %s (from Step %d)\n", final_best_model, final_best_step))
    cat(sprintf("📈 Best ΔOFV improvement: %.2f\n", final_best_delta))
    cat(sprintf("🧬 Final covariate: %s\n", final_best_covariate))

    # Show step-by-step progression
    cat("\n📊 Forward Selection Progression:\n")

    # Get best model from each step
    for (step in 1:(current_step - 1)) {
      step_models <- search_state$search_database[
        search_state$search_database$step_number == step &
          search_state$search_database$status == "completed" &
          !is.na(search_state$search_database$delta_ofv), ]

      if (nrow(step_models) > 0) {
        best_step_idx <- which.max(step_models$delta_ofv)
        step_best <- step_models[best_step_idx, ]

        icon <- if (step_best$model_name == final_best_model) "🏆" else "📊"
        cat(sprintf("  %s Step %d: %s → ΔOFV=%.2f (%s)\n",
                    icon, step, step_best$model_name,
                    step_best$delta_ofv, step_best$covariate_tested))

        # Store step progression
        step_progression[[step]] <- list(
          step = step,
          model = step_best$model_name,
          delta_ofv = step_best$delta_ofv,
          covariate = step_best$covariate_tested,
          is_final_best = (step_best$model_name == final_best_model)
        )
      }
    }

  } else {
    cat("❌ No successful models found in forward selection!\n")
    cat(sprintf("Keeping base model: %s\n", base_model_id))
  }

  # Show excluded covariates
  excluded_covariates <- get_excluded_covariates(search_state)
  if (length(excluded_covariates) > 0) {
    cat(sprintf("\n🚫 Excluded covariates (%d): %s\n",
                length(excluded_covariates), paste(excluded_covariates, collapse = ", ")))
    cat("   These covariates failed during forward selection and are available for final testing\n")
  }

  # Update search configuration with forward selection results
  cat("\n🔧 Updating search configuration...\n")

  search_state$search_config$forward_selection_complete <- TRUE
  search_state$search_config$forward_final_model <- final_best_model
  search_state$search_config$forward_steps_completed <- current_step - 1
  search_state$search_config$forward_time_minutes <- forward_time
  search_state$search_config$forward_excluded_covariates <- excluded_covariates
  search_state$search_config$current_phase <- "forward_complete"

  # Final summary
  cat(sprintf("\n🎯 FORWARD SELECTION SUMMARY:\n"))
  cat(sprintf("   Steps completed: %d\n", current_step - 1))
  cat(sprintf("   Final best model: %s\n", final_best_model))
  cat(sprintf("   Total time: %.1f minutes\n", forward_time))
  cat(sprintf("   Models created: %d\n", nrow(search_state$search_database) - 1))  # Exclude base model
  cat(sprintf("   Excluded covariates: %d\n", length(excluded_covariates)))

  # Save final state
  save_search_state(search_state, "scm_forward_selection_complete.rds")
  cat("\n💾 Final forward selection state saved to: scm_forward_selection_complete.rds\n")

  cat("\n🔄 Ready for backward elimination or final testing phase\n")
  cat(paste(rep("=", 60), collapse=""), "\n")

  # Return comprehensive results
  return(list(
    search_state = search_state,
    status = "completed",
    final_best_model = final_best_model,
    final_best_step = final_best_step,
    final_best_delta_ofv = final_best_delta,
    final_best_covariate = final_best_covariate,
    base_model = base_model_id,
    steps_completed = current_step - 1,
    step_results = step_results,
    step_progression = step_progression,
    total_time_minutes = forward_time,
    excluded_covariates = excluded_covariates,
    models_created = nrow(search_state$search_database) - 1,
    forward_selection_complete = TRUE
  ))
}
