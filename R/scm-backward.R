# File: R/scm-backward.R
# Part of CovariateSearcher Package
# Backward elimination functionality for stepwise covariate modeling
# =============================================================================


#' Run Backward Elimination
#'
#' @title Execute backward elimination from a forward selection result
#' @description Iteratively removes covariates that have minimal impact on model fit.
#'   Removes the covariate with smallest ΔOFV increase if below threshold.
#' @param search_state List containing covariate search state and configuration
#' @param starting_model Character. Model to start backward elimination from
#' @param ofv_threshold Numeric. ΔOFV threshold for removal (default: 10.83)
#' @param auto_submit Logical. Whether to automatically submit models (default: TRUE)
#' @param auto_retry Logical. Whether to enable automatic retry (default: TRUE)
#' @return List with backward elimination results and updated search_state
#' @export
run_backward_elimination <- function(search_state,
                                     starting_model,
                                     ofv_threshold = 10.83,
                                     auto_submit = TRUE,
                                     auto_retry = TRUE) {

  cat("\n🔙 STARTING BACKWARD ELIMINATION\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  cat(sprintf("Starting model: %s\n", starting_model))
  cat(sprintf("ΔOFV threshold: %.2f (for keeping covariate)\n", ofv_threshold))
  cat(sprintf("Strategy: Remove covariate with smallest impact if ΔOFV < %.2f\n", ofv_threshold))
  cat(paste(rep("=", 60), collapse=""), "\n")

  # Initialize tracking variables
  backward_start_time <- Sys.time()
  current_base_model <- starting_model
  backward_active <- TRUE
  step_results <- list()
  removed_covariates <- character(0)

  # Get starting step number (continue from forward selection)
  last_step <- max(search_state$search_database$step_number, na.rm = TRUE)
  if (is.na(last_step) || is.infinite(last_step)) {
    last_step <- 0
  }
  current_step <- last_step + 1

  # Get starting model info
  starting_row <- search_state$search_database[
    search_state$search_database$model_name == starting_model, ]

  if (nrow(starting_row) == 0) {
    stop(sprintf("Starting model %s not found in database", starting_model))
  }

  starting_ofv <- starting_row$ofv[1]
  if (is.na(starting_ofv)) {
    stop(sprintf("Starting model %s does not have OFV value", starting_model))
  }

  cat(sprintf("\n📊 Starting model OFV: %.2f\n", starting_ofv))

  # Main backward elimination loop
  while (backward_active) {

    cat(sprintf("\n🎯 STEP %d: BACKWARD ELIMINATION\n", current_step))
    cat(paste(rep("-", 40), collapse=""), "\n")

    # Get current model's covariates
    current_covariates <- get_model_covariates_from_db(search_state, current_base_model)

    if (length(current_covariates) == 0) {
      cat("📋 No covariates left in model - backward elimination complete\n")
      backward_active <- FALSE
      break
    }

    cat(sprintf("📋 Current model has %d covariates: %s\n",
                length(current_covariates),
                paste(current_covariates, collapse = " + ")))

    # Identify FIX status covariates
    fixed_covariates <- get_fixed_covariates(search_state, current_covariates)
    removable_covariates <- setdiff(current_covariates, fixed_covariates)

    if (length(fixed_covariates) > 0) {
      cat(sprintf("🔒 Fixed covariates (not removable): %s\n",
                  paste(fixed_covariates, collapse = ", ")))
    }

    if (length(removable_covariates) == 0) {
      cat("⚠️  No removable covariates (all are FIXED) - backward elimination complete\n")
      backward_active <- FALSE
      break
    }

    cat(sprintf("🔓 Testing removal of %d covariates: %s\n",
                length(removable_covariates),
                paste(removable_covariates, collapse = ", ")))

    # Create removal test models
    cat("\n🔬 Creating removal test models...\n")

    removal_models <- list()
    model_creation_results <- list()

    for (covariate_name in removable_covariates) {
      # Convert covariate name back to tag
      covariate_tag <- get_covariate_tag_from_name(search_state, covariate_name)

      if (is.null(covariate_tag)) {
        cat(sprintf("  ⚠️  Cannot find tag for covariate %s - skipping\n", covariate_name))
        next
      }

      cat(sprintf("  Testing removal of %s (%s)... ", covariate_name, covariate_tag))

      # Use existing remove_covariate_from_model function
      # Suppress its console output to keep our formatting clean
      removal_result <- tryCatch({
        suppressMessages(
          remove_covariate_from_model(
            search_state = search_state,
            model_name = current_base_model,
            covariate_tag = covariate_tag,
            save_as_new_model = TRUE
          )
        )
      }, error = function(e) {
        list(status = "error", error_message = e$message)
      })

      if (removal_result$status == "success") {
        new_model_name <- removal_result$model_name
        search_state <- removal_result$search_state

        # Update database entry for backward elimination
        db_idx <- which(search_state$search_database$model_name == new_model_name)
        if (length(db_idx) > 0) {
          search_state$search_database$step_number[db_idx] <- current_step
          search_state$search_database$phase[db_idx] <- "backward_elimination"
          search_state$search_database$action[db_idx] <- "remove_covariate"
          search_state$search_database$step_description[db_idx] <-
            sprintf("Step %d: Test removing %s", current_step, covariate_name)
        }

        removal_models[[covariate_name]] <- new_model_name
        model_creation_results[[covariate_name]] <- removal_result
        cat("✓\n")
      } else {
        cat(sprintf("✗ %s\n", removal_result$error_message))
      }
    }

    if (length(removal_models) == 0) {
      cat("❌ Failed to create any removal test models\n")
      backward_active <- FALSE
      break
    }

    cat(sprintf("✅ Created %d removal test models\n", length(removal_models)))

    # Submit and monitor models
    cat(sprintf("\n🚀 Submitting Step %d models...\n", current_step))

    model_names_to_submit <- unlist(removal_models)

    step_submission <- submit_and_wait_for_step(
      search_state = search_state,
      model_names = model_names_to_submit,
      step_name = sprintf("Step %d: Backward Elimination", current_step),
      auto_submit = auto_submit,
      auto_retry = auto_retry
    )

    search_state <- step_submission$search_state

    cat(sprintf("\n📊 Step %d Results:\n", current_step))
    cat(sprintf("  Completed: %d models\n", length(step_submission$completed_models)))
    cat(sprintf("  Failed: %d models\n", length(step_submission$failed_models)))

    if (length(step_submission$completed_models) == 0) {
      cat(sprintf("❌ No models completed in Step %d\n", current_step))
      backward_active <- FALSE
      break
    }

    # Evaluate removal impacts
    cat(sprintf("\n📈 Evaluating removal impacts...\n"))

    removal_evaluation <- evaluate_removal_impacts(
      search_state = search_state,
      base_model = current_base_model,
      removal_models = removal_models,
      completed_models = step_submission$completed_models,
      ofv_threshold = ofv_threshold
    )

    search_state <- removal_evaluation$search_state

    # Store step results
    step_results[[sprintf("step_%d", current_step)]] <- list(
      base_model = current_base_model,
      removal_models = removal_models,
      submission = step_submission,
      evaluation = removal_evaluation,
      step_number = current_step
    )

    # Check if any covariate can be removed
    if (is.null(removal_evaluation$covariate_to_remove)) {
      cat("\n🏁 No covariate can be removed without exceeding threshold\n")
      cat(sprintf("All removal attempts had ΔOFV > %.2f\n", ofv_threshold))
      backward_active <- FALSE
      break
    }

    # Remove the selected covariate
    removed_covariate <- removal_evaluation$covariate_to_remove
    new_base_model <- removal_evaluation$new_base_model
    delta_ofv <- removal_evaluation$delta_ofv

    cat(sprintf("\n✂️  REMOVING: %s\n", removed_covariate))
    cat(sprintf("📊 ΔOFV: %.2f (below threshold of %.2f)\n", delta_ofv, ofv_threshold))
    cat(sprintf("🎯 New base model: %s\n", new_base_model))

    # Update tracking
    removed_covariates <- c(removed_covariates, removed_covariate)
    current_base_model <- new_base_model
    current_step <- current_step + 1

    # Update step description for the winning model
    db_idx <- which(search_state$search_database$model_name == new_base_model)
    if (length(db_idx) > 0) {
      search_state$search_database$step_description[db_idx] <-
        sprintf("Step %d: Remove %s", current_step - 1, removed_covariate)
    }

    # Save progress
    save_search_state(search_state, sprintf("backward_step_%d.rds", current_step - 1))
    cat(sprintf("💾 Progress saved to: backward_step_%d.rds\n", current_step - 1))
  }

  # Calculate total time
  backward_time <- as.numeric(difftime(Sys.time(), backward_start_time, units = "mins"))

  # Final summary
  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("🏁 BACKWARD ELIMINATION COMPLETE!\n")
  cat(paste(rep("=", 60), collapse=""), "\n")

  cat(sprintf("⏱️  Total time: %.1f minutes\n", backward_time))
  cat(sprintf("📊 Steps completed: %d\n", current_step - last_step - 1))
  cat(sprintf("🎯 Final model: %s\n", current_base_model))

  # Get final model info
  final_row <- search_state$search_database[
    search_state$search_database$model_name == current_base_model, ]

  if (nrow(final_row) > 0 && !is.na(final_row$ofv[1])) {
    cat(sprintf("📊 Final OFV: %.2f\n", final_row$ofv[1]))
    cat(sprintf("📊 Total ΔOFV from start: %.2f\n", final_row$ofv[1] - starting_ofv))
  }

  # Show removed covariates
  if (length(removed_covariates) > 0) {
    cat(sprintf("\n✂️  Removed covariates (%d): %s\n",
                length(removed_covariates),
                paste(removed_covariates, collapse = " → ")))
  } else {
    cat("\n✅ No covariates removed (all were significant)\n")
  }

  # Show final model composition
  final_covariates <- get_model_covariates_from_db(search_state, current_base_model)
  if (length(final_covariates) > 0) {
    cat(sprintf("\n📋 Final model contains: %s\n",
                paste(final_covariates, collapse = " + ")))
  } else {
    cat("\n📋 Final model: Base model (no covariates)\n")
  }

  # Save final state
  save_search_state(search_state, "backward_elimination_complete.rds")
  cat("\n💾 Final state saved to: backward_elimination_complete.rds\n")

  cat(paste(rep("=", 60), collapse=""), "\n")

  return(list(
    search_state = search_state,
    status = "completed",
    starting_model = starting_model,
    final_model = current_base_model,
    removed_covariates = removed_covariates,
    steps_completed = current_step - last_step - 1,
    step_results = step_results,
    total_time_minutes = backward_time,
    starting_ofv = starting_ofv,
    final_ofv = if (nrow(final_row) > 0) final_row$ofv[1] else NA,
    final_covariates = final_covariates
  ))
}


#' Evaluate Removal Impacts
#'
#' @title Evaluate the impact of removing each covariate
#' @description Calculates ΔOFV for each removal and identifies the covariate
#'   with smallest impact that meets the threshold for removal
#' @param search_state List containing search state
#' @param base_model Character. Current base model name
#' @param removal_models List. Named list of removal test models
#' @param completed_models Character vector. Successfully completed models
#' @param ofv_threshold Numeric. ΔOFV threshold for removal
#' @return List with evaluation results and covariate to remove
#' @export
evaluate_removal_impacts <- function(search_state, base_model, removal_models,
                                     completed_models, ofv_threshold) {

  # Get base model OFV
  base_row <- search_state$search_database[
    search_state$search_database$model_name == base_model, ]

  if (nrow(base_row) == 0 || is.na(base_row$ofv[1])) {
    stop(sprintf("Base model %s OFV not found", base_model))
  }

  base_ofv <- base_row$ofv[1]

  cat(sprintf("\n📊 Base model %s OFV: %.2f\n", base_model, base_ofv))
  cat("📈 Removal impacts:\n")

  # Calculate ΔOFV for each removal
  removal_impacts <- data.frame(
    covariate = character(0),
    model_name = character(0),
    ofv = numeric(0),
    delta_ofv = numeric(0),
    meets_threshold = logical(0),
    stringsAsFactors = FALSE
  )

  for (covariate_name in names(removal_models)) {
    model_name <- removal_models[[covariate_name]]

    if (!(model_name %in% completed_models)) {
      cat(sprintf("  ⚠️  %s: Model %s did not complete\n", covariate_name, model_name))
      next
    }

    # Get model OFV
    model_row <- search_state$search_database[
      search_state$search_database$model_name == model_name, ]

    if (nrow(model_row) == 0 || is.na(model_row$ofv[1])) {
      cat(sprintf("  ⚠️  %s: OFV not available\n", covariate_name))
      next
    }

    model_ofv <- model_row$ofv[1]
    delta_ofv <- model_ofv - base_ofv  # Positive means OFV increased (model got worse)

    # Update database with delta_ofv
    db_idx <- which(search_state$search_database$model_name == model_name)
    if (length(db_idx) > 0) {
      search_state$search_database$delta_ofv[db_idx] <- delta_ofv
    }

    meets_threshold <- delta_ofv < ofv_threshold

    removal_impacts <- rbind(removal_impacts, data.frame(
      covariate = covariate_name,
      model_name = model_name,
      ofv = model_ofv,
      delta_ofv = delta_ofv,
      meets_threshold = meets_threshold,
      stringsAsFactors = FALSE
    ))

    status_icon <- if (meets_threshold) "✅" else "❌"
    cat(sprintf("  %s %s removed → OFV: %.2f (ΔOFV: %+.2f) %s\n",
                status_icon, covariate_name, model_ofv, delta_ofv,
                if (meets_threshold) "< threshold" else "> threshold"))
  }

  # Find covariate with smallest ΔOFV that meets threshold
  removable <- removal_impacts[removal_impacts$meets_threshold == TRUE, ]

  covariate_to_remove <- NULL
  new_base_model <- NULL
  selected_delta_ofv <- NULL

  if (nrow(removable) > 0) {
    # Sort by delta_ofv (smallest first - least important covariate)
    removable <- removable[order(removable$delta_ofv), ]

    covariate_to_remove <- removable$covariate[1]
    new_base_model <- removable$model_name[1]
    selected_delta_ofv <- removable$delta_ofv[1]

    cat(sprintf("\n🎯 Selected for removal: %s (ΔOFV = %.2f)\n",
                covariate_to_remove, selected_delta_ofv))
  } else {
    cat(sprintf("\n⚠️  No removals meet threshold (all ΔOFV > %.2f)\n", ofv_threshold))
  }

  return(list(
    search_state = search_state,
    removal_impacts = removal_impacts,
    covariate_to_remove = covariate_to_remove,
    new_base_model = new_base_model,
    delta_ofv = selected_delta_ofv,
    removable_count = nrow(removable)
  ))
}


#' Get Fixed Covariates
#'
#' @title Identify covariates with FIX status
#' @description Checks both covariate_search data and model THETA parameters
#'   to identify covariates that should not be removed
#' @param search_state List containing search state
#' @param covariate_names Character vector. Covariate names to check
#' @return Character vector of fixed covariate names
#' @export
get_fixed_covariates <- function(search_state, covariate_names) {

  fixed_covariates <- character(0)

  # Check FIX_STATUS column in covariate_search if it exists
  if ("FIX_STATUS" %in% names(search_state$covariate_search)) {
    # Get covariates marked as FIX
    fixed_rows <- search_state$covariate_search[
      search_state$covariate_search$FIX_STATUS == TRUE |
        search_state$covariate_search$FIX_STATUS == "TRUE" |
        search_state$covariate_search$FIX_STATUS == 1, ]

    if (nrow(fixed_rows) > 0) {
      # Convert COVARIATE column values to the format used in models
      for (i in 1:nrow(fixed_rows)) {
        cov_name <- paste0(fixed_rows$COVARIATE[i], "_", fixed_rows$PARAMETER[i])
        if (cov_name %in% covariate_names) {
          fixed_covariates <- c(fixed_covariates, cov_name)
        }
      }
    }
  }

  # TODO: Also check model file for THETA FIX parameters
  # This would require parsing the model file to identify which THETAs
  # are associated with which covariates and have FIX status

  return(unique(fixed_covariates))
}


#' Get Covariate Tag from Name
#'
#' @title Convert covariate name back to tag
#' @description Finds the tag in search_state$tags that corresponds to
#'   the given covariate name (e.g., "WT_CL" → "cov_cl_wt")
#' @param search_state List containing search state
#' @param covariate_name Character. Covariate name (e.g., "WT_CL")
#' @return Character. Covariate tag or NULL if not found
#' @export
get_covariate_tag_from_name <- function(search_state, covariate_name) {

  # Search through tags to find matching value
  for (tag_name in names(search_state$tags)) {
    tag_value <- search_state$tags[[tag_name]]
    if (!is.null(tag_value) && tag_value == covariate_name) {
      return(tag_name)
    }
  }

  # If not found, try to construct it (fallback)
  # This assumes a pattern like "WT_CL" → "cov_cl_wt"
  parts <- strsplit(covariate_name, "_")[[1]]
  if (length(parts) == 2) {
    # Try both possible patterns
    constructed_tag1 <- paste0("cov_", tolower(parts[2]), "_", tolower(parts[1]))
    constructed_tag2 <- paste0("cov_", tolower(parts[1]), "_", tolower(parts[2]))

    if (constructed_tag1 %in% names(search_state$tags)) {
      return(constructed_tag1)
    } else if (constructed_tag2 %in% names(search_state$tags)) {
      return(constructed_tag2)
    }
  }

  return(NULL)
}
