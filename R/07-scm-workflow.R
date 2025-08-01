#' @title SCM Workflow Functions
#' @description Complete stepwise covariate modeling workflow automation
#' @name scm-workflow-functions
NULL

#' Run Complete Stepwise Covariate Modeling
#'
#' Execute complete stepwise covariate modeling algorithm
#'
#' @param search_state List. Current search state
#' @param base_model_id Character. Starting base model
#' @param max_forward_steps Numeric. Maximum number of forward steps
#' @param max_wait_minutes Numeric. Maximum wait time per step (default 60)
#' @param auto_submit Logical. Whether to automatically submit models (default TRUE)
#' @param ofv_threshold Numeric. OFV improvement threshold
#' @param rse_threshold Numeric. Maximum RSE threshold
#' @return List with complete SCM results and updated search state
#' @export
run_stepwise_covariate_modeling <- function(search_state, base_model_id, max_forward_steps = NULL,
                                           max_wait_minutes = 60, auto_submit = TRUE,
                                           ofv_threshold = NULL, rse_threshold = NULL) {

  # Set defaults from config
  if (is.null(ofv_threshold)) ofv_threshold <- search_state$search_config$forward_ofv_threshold
  if (is.null(rse_threshold)) rse_threshold <- search_state$search_config$max_rse_threshold
  
  # Auto-calculate max_forward_steps
  if (is.null(max_forward_steps)) {
    available_covariates <- length(names(search_state$tags)[grepl("^cov_", names(search_state$tags))])
    max_forward_steps <- available_covariates + 2
  }

  cat(paste0("\n", paste(rep("=", 80), collapse=""), "\n"))
  cat("🎯 STARTING STEPWISE COVARIATE MODELING (SCM)\n")
  cat(sprintf("Base model: %s\n", base_model_id))
  cat(sprintf("Max forward steps: %d\n", max_forward_steps))
  cat(sprintf("OFV threshold: %.2f\n", ofv_threshold))
  cat(paste0(paste(rep("=", 80), collapse=""), "\n"))

  scm_start_time <- Sys.time()
  
  # Initialize tracking
  current_base_model <- base_model_id
  step_count <- 0
  all_tested_covariates <- character(0)
  step_results <- list()

  # STEP 1: Initial univariate analysis
  cat("\n📍 STEP 1: INITIAL UNIVARIATE ANALYSIS\n")
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
  step1_result <- run_univariate_step(
    search_state = search_state,
    base_model_id = current_base_model,
    covariates_to_test = remaining_covariates,
    step_name = "Step 1: Initial Univariate Analysis"
  )
  search_state <- step1_result$search_state

  if (step1_result$status != "models_created") {
    cat("❌ Failed to create initial models\n")
    return(list(
      search_state = search_state,
      status = "step1_creation_failed",
      final_model = current_base_model,
      steps_completed = 0
    ))
  }

  # Step 1b: Submit and wait
  step1_completion <- submit_and_wait_for_step(
    search_state = search_state,
    model_names = step1_result$models_created,
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

  step_results[["step_1"]] <- list(
    creation = step1_result,
    completion = step1_completion,
    selection = step1_selection
  )

  all_tested_covariates <- c(all_tested_covariates, step1_result$covariate_tags)

  if (is.null(step1_selection$best_model)) {
    cat("🏁 No significant improvement found in initial analysis\n")
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

  current_base_model <- step1_selection$best_model
  cat(sprintf("🎯 Step 1 complete - new base model: %s\n", current_base_model))

  # ITERATIVE FORWARD STEPS
  for (forward_step in 2:max_forward_steps) {
    cat(sprintf("\n📍 STEP %d: FORWARD SELECTION ITERATION\n", forward_step))

    remaining_covariates <- get_remaining_covariates(search_state, current_base_model)

    if (length(remaining_covariates) == 0) {
      cat("✅ No more covariates to test - forward selection complete\n")
      break
    }

    # Create models
    step_result <- run_univariate_step(
      search_state = search_state,
      base_model_id = current_base_model,
      covariates_to_test = remaining_covariates,
      step_name = sprintf("Step %d: Forward Selection", forward_step)
    )
    search_state <- step_result$search_state

    if (step_result$status != "models_created") {
      cat(sprintf("❌ Failed to create Step %d models\n", forward_step))
      break
    }

    # Submit and wait
    step_completion <- submit_and_wait_for_step(
      search_state = search_state,
      model_names = step_result$models_created,
      step_name = sprintf("Step %d Models", forward_step),
      max_wait_minutes = max_wait_minutes,
      auto_submit = auto_submit
    )
    search_state <- step_completion$search_state

    # Select best
    step_selection <- select_best_model(
      search_state = search_state,
      model_names = step_completion$completed_models,
      ofv_threshold = ofv_threshold,
      rse_threshold = rse_threshold
    )

    step_results[[sprintf("step_%d", forward_step)]] <- list(
      creation = step_result,
      completion = step_completion,
      selection = step_selection
    )

    all_tested_covariates <- c(all_tested_covariates, step_result$covariate_tags)

    if (is.null(step_selection$best_model)) {
      cat(sprintf("🏁 No significant improvement found in Step %d\n", forward_step))
      break
    }

    current_base_model <- step_selection$best_model
    cat(sprintf("🎯 Step %d complete - new base model: %s\n", forward_step, current_base_model))
    step_count <- forward_step
  }

  # FINAL STEP: Test dropped covariates
  cat("\n📍 FINAL STEP: TESTING DROPPED COVARIATES\n")

  dropped_covariates <- get_dropped_covariates(search_state, current_base_model, all_tested_covariates)

  if (length(dropped_covariates) > 0) {
    cat(sprintf("Found %d previously dropped covariates to retest\n", length(dropped_covariates)))

    final_result <- run_univariate_step(
      search_state = search_state,
      base_model_id = current_base_model,
      covariates_to_test = dropped_covariates,
      step_name = "Final Step: Dropped Covariate Testing"
    )
    search_state <- final_result$search_state

    if (final_result$status == "models_created") {
      final_completion <- submit_and_wait_for_step(
        search_state = search_state,
        model_names = final_result$models_created,
        step_name = "Final Step Models",
        max_wait_minutes = max_wait_minutes,
        auto_submit = auto_submit
      )
      search_state <- final_completion$search_state

      final_selection <- select_best_model(
        search_state = search_state,
        model_names = final_completion$completed_models,
        ofv_threshold = ofv_threshold,
        rse_threshold = rse_threshold
      )

      step_results[["final_step"]] <- list(
        creation = final_result,
        completion = final_completion,
        selection = final_selection
      )

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

  # Show final covariates
  final_covariates <- get_model_covariates_from_db(search_state, current_base_model)
  if (length(final_covariates) > 0) {
    cat(sprintf("📋 Final covariates: %s\n", paste(final_covariates, collapse = ", ")))
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

# Helper functions needed for SCM workflow

#' Get Remaining Covariates
#'
#' @param search_state List. Current search state
#' @param model_name Character. Model to check
#' @return Character vector of remaining covariate tags
#' @export
get_remaining_covariates <- function(search_state, model_name) {
  # All available covariate tags
  all_cov_tags <- names(search_state$tags)[grepl("^cov_", names(search_state$tags))]
  
  # Get covariates already in the model
  model_covariates <- get_model_covariates_from_db(search_state, model_name)
  
  # Find tags for existing covariates
  existing_tags <- character(0)
  for (cov in model_covariates) {
    matching_tags <- names(search_state$tags)[search_state$tags == cov]
    existing_tags <- c(existing_tags, matching_tags)
  }
  
  # Return remaining
  remaining <- setdiff(all_cov_tags, existing_tags)
  return(remaining)
}

#' Get Dropped Covariates
#'
#' @param search_state List. Current search state
#' @param model_name Character. Current model
#' @param tested_covariates Character. Previously tested covariate tags
#' @return Character vector of dropped covariate tags
#' @export
get_dropped_covariates <- function(search_state, model_name, tested_covariates) {
  # Get excluded covariates
  excluded_covariates <- get_excluded_covariates(search_state)
  
  # Remove excluded from tested list
  dropped <- setdiff(tested_covariates, excluded_covariates)
  
  # Remove currently included covariates
  current_covariates <- get_model_covariates_from_db(search_state, model_name)
  current_tags <- character(0)
  for (cov in current_covariates) {
    matching_tags <- names(search_state$tags)[search_state$tags == cov]
    current_tags <- c(current_tags, matching_tags)
  }
  
  dropped <- setdiff(dropped, current_tags)
  return(dropped)
}

# Placeholder functions for now - need to be implemented

#' Run Univariate Step
#' @param search_state List. Current search state
#' @param base_model_id Character. Base model
#' @param covariates_to_test Character. Covariate tags to test
#' @param step_name Character. Step description
#' @return List with creation results
#' @export
run_univariate_step <- function(search_state, base_model_id, covariates_to_test, step_name) {
  cat(sprintf("📝 %s: Creating %d models\n", step_name, length(covariates_to_test)))
  
  models_created <- character(0)
  covariate_tags <- character(0)
  
  for (cov_tag in covariates_to_test) {
    result <- add_covariate_to_model(search_state, base_model_id, cov_tag)
    search_state <- result
    
    # Get the new model name (last added)
    new_model <- tail(search_state$search_database$model_name, 1)
    models_created <- c(models_created, new_model)
    covariate_tags <- c(covariate_tags, cov_tag)
  }
  
  return(list(
    search_state = search_state,
    status = "models_created",
    models_created = models_created,
    covariate_tags = covariate_tags
  ))
}

#' Submit and Wait for Step
#' @param search_state List. Current search state
#' @param model_names Character. Models to submit
#' @param step_name Character. Step description
#' @param max_wait_minutes Numeric. Maximum wait time
#' @param auto_submit Logical. Auto-submit flag
#' @return List with completion results
#' @export
submit_and_wait_for_step <- function(search_state, model_names, step_name, max_wait_minutes, auto_submit) {
  cat(sprintf("🚀 %s: Submitting %d models\n", step_name, length(model_names)))
  
  if (auto_submit) {
    cat("   Auto-submission enabled (placeholder)\n")
  } else {
    cat("   Manual submission required\n")
  }
  
  # Placeholder - in real implementation would use bbr to submit
  completed_models <- model_names  # Assume all complete for now
  
  return(list(
    search_state = search_state,
    status = "completed",
    completed_models = completed_models,
    failed_models = character(0)
  ))
}

#' Select Best Model
#' @param search_state List. Current search state
#' @param model_names Character. Models to evaluate
#' @param ofv_threshold Numeric. OFV threshold
#' @param rse_threshold Numeric. RSE threshold
#' @return List with selection results
#' @export
select_best_model <- function(search_state, model_names, ofv_threshold, rse_threshold) {
  cat(sprintf("📊 Evaluating %d models for best selection\n", length(model_names)))
  
  # Placeholder - would implement statistical selection logic
  # For now, return first model as "best"
  best_model <- if (length(model_names) > 0) model_names[1] else NULL
  
  if (!is.null(best_model)) {
    cat(sprintf("🏆 Selected best model: %s\n", best_model))
  } else {
    cat("❌ No suitable model found\n")
  }
  
  return(list(
    best_model = best_model,
    selection_criteria = "placeholder",
    all_results = model_names
  ))
}
