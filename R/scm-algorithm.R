# =============================================================================
# SCM ALGORITHM
# File: R/scm-algorithm.R
# Part of CovariateSearcher Package
# Core SCM algorithm and workflow
# =============================================================================



#' Get Remaining Covariates for Testing (ENHANCED WITH EXCLUSION)
#'
#' @title Get list of covariate tags that haven't been tested from base model
#' @description Identifies which covariates from the search definition haven't
#'   been added to the specified base model yet, with exclusion filtering.
#' @param search_state List containing covariate search state and configuration
#' @param base_model_id Character. Model to check current covariates against
#' @param include_excluded Logical. Whether to include excluded covariates (default: FALSE)
#' @return Character vector of covariate tag names that can still be tested
#' @export
get_remaining_covariates <- function(search_state, base_model_id, include_excluded = FALSE) {
  # Get current covariates in the base model
  current_covs <- get_model_covariates(search_state, base_model_id)

  # Get all available covariate tags
  all_cov_tags <- names(search_state$tags)[grepl("^cov_", names(search_state$tags))]

  # Get excluded covariates (if not including them)
  if (!include_excluded) {
    excluded_cov_names <- get_excluded_covariates(search_state)
    excluded_tags <- character(0)

    # Convert excluded names back to tags
    for (tag in names(search_state$tags)) {
      if (search_state$tags[[tag]] %in% excluded_cov_names) {
        excluded_tags <- c(excluded_tags, tag)
      }
    }

    # Remove excluded tags
    all_cov_tags <- setdiff(all_cov_tags, excluded_tags)

    if (length(excluded_tags) > 0) {
      cat(sprintf("🚫 Excluding %d covariates from testing: %s\n",
                  length(excluded_tags), paste(excluded_cov_names, collapse = ", ")))
    }
  } else {
    cat("📋 Including previously excluded covariates for testing\n")
  }

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



#' Get Excluded Covariates (ENHANCED VERSION - FIXED COLUMN REFERENCE)
#'
#' @title Get list of covariates excluded from current step with details
#' @description Returns covariates that have been excluded due to estimation issues
#' @param search_state List containing covariate search state and configuration
#' @param return_details Logical. Whether to return detailed exclusion info (default: FALSE)
#' @return Character vector of excluded covariate names, or data.frame if return_details=TRUE
#' @export
get_excluded_covariates <- function(search_state, return_details = FALSE) {

  excluded_models <- search_state$search_database[search_state$search_database$excluded_from_step == TRUE, ]

  if (nrow(excluded_models) == 0) {
    if (return_details) {
      return(data.frame(
        covariate_tested = character(0),
        original_model = character(0),
        retry_model = character(0),
        exclusion_reason = character(0),
        step_number = integer(0),
        stringsAsFactors = FALSE
      ))
    } else {
      return(character(0))
    }
  }

  if (return_details) {
    # FIXED: Handle the original_model column properly - check if it exists
    if ("original_model" %in% names(excluded_models)) {
      # Use original_model column if it exists
      exclusion_details <- excluded_models %>%
        dplyr::filter(!is.na(covariate_tested) & covariate_tested != "") %>%
        dplyr::select(covariate_tested, model_name, original_model, estimation_issue, step_number) %>%
        dplyr::rename(
          retry_model = model_name,
          exclusion_reason = estimation_issue
        ) %>%
        dplyr::arrange(covariate_tested)
    } else {
      # Fallback if original_model column doesn't exist
      exclusion_details <- excluded_models %>%
        dplyr::filter(!is.na(covariate_tested) & covariate_tested != "") %>%
        dplyr::select(covariate_tested, model_name, estimation_issue, step_number) %>%
        dplyr::mutate(original_model = NA_character_) %>%
        dplyr::rename(
          retry_model = model_name,
          exclusion_reason = estimation_issue
        ) %>%
        dplyr::select(covariate_tested, original_model, retry_model, exclusion_reason, step_number) %>%
        dplyr::arrange(covariate_tested)
    }

    return(exclusion_details)
  } else {
    # Return simple character vector of excluded covariate names
    excluded_covs <- unique(excluded_models$covariate_tested)
    excluded_covs <- excluded_covs[!is.na(excluded_covs) & excluded_covs != ""]
    return(excluded_covs)
  }
}



#' Run Complete Stepwise Covariate Modeling (FIXED CRITICAL FIELD NAME ERRORS)
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

  # VALIDATION: Check that step1_creation is valid
  if (is.null(step1_creation) || is.null(step1_creation$search_state)) {
    cat("❌ Failed to create initial models - invalid result from run_univariate_step\n")
    return(list(
      search_state = search_state,
      status = "step1_creation_failed",
      final_model = current_base_model,
      steps_completed = 0,
      error = "Invalid result from run_univariate_step"
    ))
  }

  search_state <- step1_creation$search_state

  if (step1_creation$status != "models_created") {
    cat("❌ Failed to create initial models\n")
    return(list(
      search_state = search_state,
      status = "step1_creation_failed",
      final_model = current_base_model,
      steps_completed = 0,
      error = paste("Step1 creation status:", step1_creation$status)
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

  # VALIDATION: Check that step1_completion is valid
  if (is.null(step1_completion) || is.null(step1_completion$search_state)) {
    cat("❌ Failed in step1 completion - invalid result from submit_and_wait_for_step\n")
    return(list(
      search_state = search_state,
      status = "step1_completion_failed",
      final_model = current_base_model,
      steps_completed = 0,
      error = "Invalid result from submit_and_wait_for_step"
    ))
  }

  search_state <- step1_completion$search_state

  # Step 1c: Select best model
  step1_selection <- select_best_model(
    search_state = search_state,
    model_names = step1_completion$completed_models,
    ofv_threshold = ofv_threshold,
    rse_threshold = rse_threshold
  )

  # VALIDATION: Check that step1_selection is valid
  if (is.null(step1_selection) || is.null(step1_selection$search_state)) {
    cat("❌ Failed in step1 selection - invalid result from select_best_model\n")
    return(list(
      search_state = search_state,
      status = "step1_selection_failed",
      final_model = current_base_model,
      steps_completed = 0,
      error = "Invalid result from select_best_model"
    ))
  }

  search_state <- step1_selection$search_state

  step_results[["step_1"]] <- list(
    creation = step1_creation,
    completion = step1_completion,
    selection = step1_selection
  )

  # FIXED: Update tracking with correct field name
  all_tested_covariates <- c(all_tested_covariates, step1_creation$successful_covariates)

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
    remaining_covariates <- get_remaining_covariates(search_state, current_base_model, include_excluded = TRUE)

    if (length(remaining_covariates) == 0) {
      cat("✅ No more covariates to test - forward selection complete\n")
      break
    }

    # Create univariate models from current best model
    step_creation <- run_univariate_step(
      search_state = search_state,
      base_model_id = current_base_model,
      covariates_to_test = remaining_covariates,
      step_name = sprintf("Step %d: Forward Selection", forward_step),
      include_excluded = TRUE
    )

    # VALIDATION: Check step_creation result
    if (is.null(step_creation) || is.null(step_creation$search_state)) {
      cat(sprintf("❌ Failed to create Step %d models - invalid result\n", forward_step))
      break
    }

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

    # VALIDATION: Check step_completion result
    if (is.null(step_completion) || is.null(step_completion$search_state)) {
      cat(sprintf("❌ Failed in Step %d completion - invalid result\n", forward_step))
      break
    }

    search_state <- step_completion$search_state

    # Select best model
    step_selection <- select_best_model(
      search_state = search_state,
      model_names = step_completion$completed_models,
      ofv_threshold = ofv_threshold,
      rse_threshold = rse_threshold
    )

    # VALIDATION: Check step_selection result
    if (is.null(step_selection) || is.null(step_selection$search_state)) {
      cat(sprintf("❌ Failed in Step %d selection - invalid result\n", forward_step))
      break
    }

    search_state <- step_selection$search_state

    # Store results
    step_results[[sprintf("step_%d", forward_step)]] <- list(
      creation = step_creation,
      completion = step_completion,
      selection = step_selection
    )

    # FIXED: Update tracking with correct field name
    all_tested_covariates <- c(all_tested_covariates, step_creation$successful_covariates)

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

    # VALIDATION: Check final_creation result
    if (!is.null(final_creation) && !is.null(final_creation$search_state)) {
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

        # VALIDATION: Check final_completion result
        if (!is.null(final_completion) && !is.null(final_completion$search_state)) {
          search_state <- final_completion$search_state

          # Select best model
          final_selection <- select_best_model(
            search_state = search_state,
            model_names = final_completion$completed_models,
            ofv_threshold = ofv_threshold,
            rse_threshold = rse_threshold
          )

          # VALIDATION: Check final_selection result
          if (!is.null(final_selection) && !is.null(final_selection$search_state)) {
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
        }
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

#' View Exclusion Status Summary
#'
#' @title Display current exclusion status with details
#' @description Shows which covariates are excluded and why
#' @param search_state List containing search state
#' @return Invisible NULL (prints to console)
#' @export
view_exclusion_status <- function(search_state) {
  cat("\n📋 COVARIATE EXCLUSION STATUS\n")
  cat(paste(rep("=", 60), collapse=""), "\n")

  exclusion_details <- get_excluded_covariates(search_state, return_details = TRUE)

  if (nrow(exclusion_details) == 0) {
    cat("✅ No covariates currently excluded\n")
  } else {
    cat(sprintf("🚫 %d covariates excluded from forward steps:\n\n", nrow(exclusion_details)))

    for (i in 1:nrow(exclusion_details)) {
      row <- exclusion_details[i, ]
      cat(sprintf("  ❌ %s (Step %d)\n", row$covariate_tested, row$step_number))
      cat(sprintf("     Original: %s → Retry: %s\n", row$original_model, row$retry_model))
      cat(sprintf("     Reason: %s\n", row$exclusion_reason))
      cat("\n")
    }

    cat("💡 These covariates will be available for final testing before backward elimination\n")
  }

  cat(paste(rep("=", 60), collapse=""), "\n")
  return(invisible(NULL))
}
