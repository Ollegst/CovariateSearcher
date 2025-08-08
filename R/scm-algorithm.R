# =============================================================================
# SCM ALGORITHM
# File: R/scm-algorithm.R
# Part of CovariateSearcher Package
# Core SCM algorithm and workflow
# =============================================================================



#' Get Remaining Covariates for Testing
#'
#' @title Get list of covariate tags that haven't been tested from base model
#' @description Identifies which covariates from the search definition haven't
#'   been added to the specified base model yet.
#' @param search_state List containing covariate search state and configuration
#' @param base_model_id Character. Model to check current covariates against
#' @return Character vector of covariate tag names that can still be tested
#' @export
get_remaining_covariates <- function(search_state, base_model_id) {
  # Get current covariates in the base model
  current_covs <- get_model_covariates(search_state, base_model_id)

  # Get all available covariate tags
  all_cov_tags <- names(search_state$tags)[grepl("^cov_", names(search_state$tags))]

  # Return tags not currently in the model
  remaining_tags <- setdiff(all_cov_tags, current_covs)

  return(remaining_tags)
}



#' Get Dropped Covariates from Previous Steps
#'
#' @title Get covariates that were tested but not selected in previous steps
#' @description Identifies covariates that were tested in earlier SCM steps
#'   but not included in the current best model.
#' @param search_state List containing covariate search state and configuration
#' @param current_model_id Character. Current best model
#' @param tested_covariates Character vector. All covariate tags tested so far
#' @return Character vector of covariate tags that were dropped
#' @export
get_dropped_covariates <- function(search_state, current_model_id, tested_covariates) {
  current_covs <- get_model_covariates(search_state, current_model_id)
  dropped_covs <- setdiff(tested_covariates, current_covs)

  return(dropped_covs)
}



#' Get Excluded Covariates
#'
#' @title Get list of covariates excluded from current step
#' @description Returns covariates that have been excluded due to estimation issues
#' @param search_state List containing covariate search state and configuration
#' @return Character vector of excluded covariate names
#' @export
get_excluded_covariates <- function(search_state) {

  excluded_models <- search_state$search_database[search_state$search_database$excluded_from_step == TRUE, ]

  if (nrow(excluded_models) > 0) {
    excluded_covs <- unique(excluded_models$covariate_tested)
    excluded_covs <- excluded_covs[!is.na(excluded_covs) & excluded_covs != ""]
    return(excluded_covs)
  } else {
    return(character(0))
  }
}



#' Run Complete Stepwise Covariate Modeling
#'
#' @title Execute complete stepwise covariate modeling algorithm
#' @description Main orchestration function that runs the complete SCM workflow:
#'   1. Initial univariate analysis on base model
#'   2. Iterative forward selection steps
#'   3. Final testing of dropped covariates
#'   All models within each step run in parallel.
#' @param search_state List containing covariate search state and configuration
#' @param base_model_id Character. Starting base model
#' @param max_forward_steps Numeric. Maximum number of forward steps (default: 10)
#' @param max_wait_minutes Numeric. Maximum wait time per step (default: 60)
#' @param auto_submit Logical. Whether to automatically submit models (default: TRUE)
#' @param ofv_threshold Numeric. OFV improvement threshold (uses config if NULL)
#' @param rse_threshold Numeric. Maximum RSE threshold (uses config if NULL)
#' @return List with complete SCM results and updated search_state
#' @export
run_stepwise_covariate_modeling <- function(search_state, base_model_id,
                                            max_forward_steps = 10,
                                            max_wait_minutes = 60,
                                            auto_submit = TRUE,
                                            ofv_threshold = NULL,
                                            rse_threshold = NULL) {

  cat(paste0("\n", paste(rep("=", 80), collapse=""), "\n"))
  cat("🎯 STARTING STEPWISE COVARIATE MODELING (SCM)\n")
  cat(sprintf("Base model: %s\n", base_model_id))
  cat(sprintf("Max forward steps: %d\n", max_forward_steps))
  cat(sprintf("OFV threshold: %.2f\n",
              ofv_threshold %||% search_state$search_config$forward_ofv_threshold))
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

  remaining_covariates <- get_remaining_covariates(search_state, current_base_model)

  if (length(remaining_covariates) == 0) {
    cat("❌ No covariates available for testing\n")
    return(list(
      search_state = search_state,
      status = "no_covariates",
      final_model = current_base_model,
      steps_completed = 0
    ))
  }

  # Step 1a: Create univariate models
  step1_creation <- run_univariate_step(
    search_state = search_state,
    base_model_id = current_base_model,
    covariates_to_test = remaining_covariates,
    step_name = "Step 1: Initial Univariate Analysis"
  )

  search_state <- step1_creation$search_state

  if (step1_creation$status != "models_created") {
    cat("❌ Failed to create initial models\n")
    return(list(
      search_state = search_state,
      status = "step1_creation_failed",
      final_model = current_base_model,
      steps_completed = 0
    ))
  }

  # Step 1b: Submit and wait for completion
  step1_completion <- submit_and_wait_for_step(
    search_state = search_state,
    model_names = step1_creation$models_created,
    step_name = "Step 1 Models",
    max_wait_minutes = max_wait_minutes,
    auto_submit = auto_submit
  )

  search_state <- step1_completion$search_state

  # Step 1c: Select best model
  step1_selection <- select_best_model(
    search_state = search_state,
    model_names = step1_completion$completed_models,
    ofv_threshold = ofv_threshold,
    rse_threshold = rse_threshold
  )

  search_state <- step1_selection$search_state

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
      search_state = search_state,
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
    remaining_covariates <- get_remaining_covariates(search_state, current_base_model)

    if (length(remaining_covariates) == 0) {
      cat("✅ No more covariates to test - forward selection complete\n")
      break
    }

    # Create univariate models from current best model
    step_creation <- run_univariate_step(
      search_state = search_state,
      base_model_id = current_base_model,
      covariates_to_test = remaining_covariates,
      step_name = sprintf("Step %d: Forward Selection", forward_step)
    )

    search_state <- step_creation$search_state

    if (step_creation$status != "models_created") {
      cat(sprintf("❌ Failed to create Step %d models\n", forward_step))
      break
    }

    # Submit and wait for completion
    step_completion <- submit_and_wait_for_step(
      search_state = search_state,
      model_names = step_creation$models_created,
      step_name = sprintf("Step %d Models", forward_step),
      max_wait_minutes = max_wait_minutes,
      auto_submit = auto_submit
    )

    search_state <- step_completion$search_state

    # Select best model
    step_selection <- select_best_model(
      search_state = search_state,
      model_names = step_completion$completed_models,
      ofv_threshold = ofv_threshold,
      rse_threshold = rse_threshold
    )

    search_state <- step_selection$search_state

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

  dropped_covariates <- get_dropped_covariates(search_state, current_base_model, all_tested_covariates)

  if (length(dropped_covariates) > 0) {
    cat(sprintf("Found %d previously dropped covariates to retest\n", length(dropped_covariates)))

    # Test dropped covariates
    final_creation <- run_univariate_step(
      search_state = search_state,
      base_model_id = current_base_model,
      covariates_to_test = dropped_covariates,
      step_name = "Final Step: Dropped Covariate Testing"
    )

    search_state <- final_creation$search_state

    if (final_creation$status == "models_created") {
      # Submit and wait
      final_completion <- submit_and_wait_for_step(
        search_state = search_state,
        model_names = final_creation$models_created,
        step_name = "Final Step Models",
        max_wait_minutes = max_wait_minutes,
        auto_submit = auto_submit
      )

      search_state <- final_completion$search_state

      # Select best model
      final_selection <- select_best_model(
        search_state = search_state,
        model_names = final_completion$completed_models,
        ofv_threshold = ofv_threshold,
        rse_threshold = rse_threshold
      )

      search_state <- final_selection$search_state

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
  final_covariates <- get_model_covariates(search_state, current_base_model)
  if (length(final_covariates) > 0) {
    final_cov_names <- sapply(final_covariates, function(x) {
      if (x %in% names(search_state$tags)) search_state$tags[[x]] else x
    })
    cat(sprintf("📋 Final covariates: %s\n", paste(final_cov_names, collapse = ", ")))
  } else {
    cat("📋 Final covariates: None (base model retained)\n")
  }
  cat(paste0(paste(rep("=", 80), collapse=""), "\n"))

  return(list(
    search_state = search_state,
    status = "completed",
    final_model = current_base_model,
    base_model = base_model_id,
    steps_completed = step_count,
    step_results = step_results,
    total_time_minutes = scm_time,
    final_covariates = final_covariates
  ))
}

