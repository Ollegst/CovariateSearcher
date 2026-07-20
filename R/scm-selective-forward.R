# =============================================================================
# SCM forward
# File: R/scm-selective-forward.R
# Part of CovariateSearcher Package
# =============================================================================
#' Run SCM Forward Selection (CORRECTED VERSION)
#'
#' Get Significant Models from Step
#'
#' @title Extract models that showed significant improvement in a specific step
#' @description Returns model names from a step that have ΔOFV above threshold.
#'   Threshold is calculated per model based on covariate degrees of freedom.
#' @param search_state List containing search state
#' @param step_number Integer. Step number to check
#' @param p_value Numeric. P-value for significance testing (e.g., 0.05 for forward, 0.01 for backward)
#' @param rse_threshold Numeric. RSE threshold for significance
#' @return Character vector of significant model names
#' @export
get_significant_models_from_step <- function(search_state, step_number, p_value, rse_threshold = NULL) {
  # Validate inputs
  if (is.null(search_state$search_database) || nrow(search_state$search_database) == 0) {
    return(character(0))
  }

  # Check required columns exist
  required_cols <- c("step_number", "status", "delta_ofv", "rse_max", "model_name", "covariate_tested")
  if (!all(required_cols %in% names(search_state$search_database))) {
    warning("Missing required columns in search database")
    return(character(0))
  }

  # Use config default if RSE threshold not specified
  if (is.null(rse_threshold)) {
    rse_threshold <- search_state$search_config$max_rse_threshold %||% 50
  }

  # Get models from specified step
  step_models <- search_state$search_database[
    search_state$search_database$step_number == step_number &
      search_state$search_database$status == "completed" &
      !is.na(search_state$search_database$delta_ofv), ]

  if (nrow(step_models) == 0) {
    return(character(0))
  }

  # Calculate threshold per model based on covariate df
  significant_model_names <- character(0)

  for (i in 1:nrow(step_models)) {
    # Extract covariate name from tag
    cov_tag <- step_models$covariate_tested[i]

    # Calculate df for this covariate
    if (!is.na(cov_tag) && nchar(cov_tag) > 0) {
      cov_name <- extract_covariate_name_from_tag(cov_tag)
      if (!is.na(cov_name)) {
        df <- calculate_covariate_df(cov_name, search_state$covariate_search)
      } else {
        df <- 1L
      }
    } else {
      df <- 1L
    }

    # Calculate threshold for this specific covariate
    ofv_threshold <- pvalue_to_threshold(p_value, df)

    # Check if model meets both criteria
    delta_ofv <- step_models$delta_ofv[i]
    rse_max <- step_models$rse_max[i]

    meets_ofv <- !is.na(delta_ofv) && delta_ofv > ofv_threshold
    meets_rse <- is.na(rse_max) || rse_max < rse_threshold

    if (meets_ofv && meets_rse) {
      significant_model_names <- c(significant_model_names, step_models$model_name[i])
    }
  }

  return(significant_model_names)
}


#' Identify the Models Tested in an SCM Step (with their base model)
#'
#' @title Reconstruct one SCM step from the database
#' @description Supportive lookup used by both selective forward selection and
#'   \code{\link{continue_search}}. Given a step number, it returns the models
#'   tested in that step, the base (parent) model they were built from, which
#'   completed, which were significant, and the step winner — all read from the
#'   search database, so a step can be reconstructed with no in-memory run state.
#'   This is what makes selective forward resume-safe: the "test only covariates
#'   from the previous step's significant models" narrowing is recovered from the
#'   database rather than a local variable.
#' @param search_state List containing covariate search state and configuration
#' @param step_number Integer. Step number to reconstruct
#' @param p_value Numeric. P-value for significance (uses forward config if NULL)
#' @param rse_threshold Numeric. RSE threshold (uses config if NULL)
#' @return List with: \code{exists} (logical), \code{step_number},
#'   \code{base_model} (common parent), \code{models} (all tested),
#'   \code{completed_models}, \code{significant_models}, and \code{best_model}
#'   (highest-ΔOFV significant model, or NULL).
#' @export
get_step_models <- function(search_state, step_number, p_value = NULL, rse_threshold = NULL) {
  db <- search_state$search_database
  empty <- list(
    exists = FALSE, step_number = step_number, base_model = NA_character_,
    models = character(0), completed_models = character(0),
    significant_models = character(0), best_model = NULL
  )
  if (is.null(db) || nrow(db) == 0) return(empty)

  rows <- db[!is.na(db$step_number) & db$step_number == step_number, , drop = FALSE]

  # Exclude the base model: it carries phase "base" and can share a step number,
  # but it is not a covariate model tested during this step.
  if ("phase" %in% names(rows)) {
    rows <- rows[is.na(rows$phase) | rows$phase != "base", , drop = FALSE]
  }
  if (nrow(rows) == 0) return(empty)

  if (is.null(p_value)) {
    p_value <- search_state$search_config$forward_p_value %||% 0.05
  }

  # Base model = the common parent of this step's models
  parents <- rows$parent_model[!is.na(rows$parent_model)]
  base_model <- if (length(parents) > 0) {
    names(sort(table(parents), decreasing = TRUE))[1]
  } else {
    NA_character_
  }

  completed_models <- rows$model_name[!is.na(rows$status) & rows$status == "completed"]

  significant_models <- get_significant_models_from_step(
    search_state, step_number, p_value, rse_threshold
  )

  best_model <- NULL
  if (length(significant_models) > 0) {
    sig_rows <- db[db$model_name %in% significant_models, , drop = FALSE]
    if (nrow(sig_rows) > 0 && any(!is.na(sig_rows$delta_ofv))) {
      best_model <- sig_rows$model_name[which.max(sig_rows$delta_ofv)]
    }
  }

  list(
    exists = TRUE,
    step_number = step_number,
    base_model = base_model,
    models = rows$model_name,
    completed_models = completed_models,
    significant_models = significant_models,
    best_model = best_model
  )
}


#' Get Covariates from Models
#'
#' @title Extract covariate names from specific models
#' @description Returns unique covariate names that were tested in the specified models
#' @param search_state List containing search state
#' @param model_names Character vector. Model names to extract covariates from
#' @return Character vector of unique covariate tag names
#' @export
get_covariates_from_models <- function(search_state, model_names) {

  cat(paste("  Models requested:", paste(model_names, collapse=", "), "\n"))

  if (length(model_names) == 0) {
    cat("  ❌ No models provided\n")
    return(character(0))
  }

  # Validate search_state and database
  if (is.null(search_state$search_database) || nrow(search_state$search_database) == 0) {
    cat("  ❌ Database is null or empty\n")
    return(character(0))
  }

  # Check required columns exist
  if (!"covariate_tested" %in% names(search_state$search_database)) {
    warning("covariate_tested column missing from search database")
    return(character(0))
  }

  # Get rows for specified models
  model_rows <- search_state$search_database[
    search_state$search_database$model_name %in% model_names, ]

  cat(paste("  Found", nrow(model_rows), "rows in database\n"))

  if (nrow(model_rows) == 0) {
    cat("  ❌ No rows found for these models\n")
    return(character(0))
  }

  # Extract covariate tags (covariate_tested column already contains tags like "beta_BALB_CL")
  covariate_tags <- unique(model_rows$covariate_tested)
  cat(paste("  Extracted tags:", paste(covariate_tags, collapse=", "), "\n"))

  covariate_tags <- covariate_tags[!is.na(covariate_tags) & covariate_tags != ""]
  cat(paste("  After filtering NA/empty:", paste(covariate_tags, collapse=", "), "\n"))

  # Filter to only valid tags that exist in search_state$tags
  if (!is.null(search_state$tags) && length(search_state$tags) > 0) {
    valid_tags <- names(search_state$tags)[grepl("^beta_", names(search_state$tags))]
    cat(paste("  Valid tags count:", length(valid_tags), "\n"))
    cat(paste("  First 5 valid tags:", paste(head(valid_tags, 5), collapse=", "), "\n"))

    covariate_tags <- intersect(covariate_tags, valid_tags)
    cat(paste("  After intersect:", paste(covariate_tags, collapse=", "), "\n"))
  }

  cat(paste("  ✅ Returning:", length(covariate_tags), "tags\n"))
  return(covariate_tags)
}


#' @title Execute proper stepwise forward selection with cumulative model building
#' @description Runs true forward selection where each step builds from the best model
#'   of the previous step, testing only remaining (untested) covariates. Implements
#'   standard pharmacometrics SCM methodology.
#' @param search_state List containing covariate search state and configuration
#' @param base_model_id Character. Starting base model (default: "run1")
#' @param forward_p_value Numeric. P-value for forward selection (uses config if NULL)
#' @param rse_threshold Numeric. Maximum RSE threshold (uses config if NULL)
#' @param auto_submit Logical. Whether to automatically submit models (default: TRUE)
#' @param auto_retry Logical. Whether to enable automatic retry (default: TRUE)
#' @param resume Logical. If TRUE, treat this call as a continuation (used by
#'   \code{\link{continue_search}}): the first loop pass skips the "test all"
#'   step and goes straight to selective narrowing, reconstructing the previous
#'   step from the database via \code{\link{get_step_models}}. Fresh runs use
#'   FALSE (default) and begin by testing all available covariates.
#' @return List with updated search_state and forward selection results
#' @export
run_scm_selective_forward <- function(search_state,
                                      base_model_id = NULL,
                                      forward_p_value = NULL,
                                      rse_threshold = NULL,
                                      auto_submit = TRUE,
                                      auto_retry = TRUE,
                                      resume = FALSE) {
  if (is.null(base_model_id)) {
    base_model_id <- search_state$base_model
  }

  if (is.null(forward_p_value)) {
    forward_p_value <- search_state$search_config$forward_p_value %||% 0.05
  }
  if (is.null(rse_threshold)) {
    rse_threshold <- search_state$search_config$max_rse_threshold
  }

  cat("🚀 STARTING SCM SELECTIVE FORWARD SELECTION WORKFLOW\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  cat(sprintf("Base model: %s\n", base_model_id))
  ofv_threshold_display <- pvalue_to_threshold(forward_p_value, df = 1)
  cat(sprintf("📊 Forward OFV threshold: %.2f\n", ofv_threshold_display))
  cat(sprintf("RSE threshold: %d%%\n", rse_threshold))
  cat(sprintf("Strategy: Test only covariates from significant models\n"))

  # Initialize workflow variables
  forward_start_time <- Sys.time()
  last_step <- max(search_state$search_database$step_number, na.rm = TRUE)
  if (is.na(last_step) || is.infinite(last_step)) {
    last_step <- 0
  }
  current_step <- last_step + 1
  current_best_model <- base_model_id
  forward_selection_active <- TRUE
  step_results <- list()
  last_step_covariates_tested <- character(0)  # Track for deduplication

  # Fresh run (resume = FALSE): first step tests ALL covariates.
  # Resume (resume = TRUE): first step continues selectively from the previous
  # step reconstructed from the database. Every later pass is a continuation.
  is_continuation <- isTRUE(resume)

  # Main selective forward selection loop
  while (forward_selection_active) {

    cat(sprintf("\n🎯 SELECTIVE FORWARD - Step %d\n", current_step))
    cat(paste(rep("-", 40), collapse=""), "\n")

    # Initialize significant_models outside if/else
    significant_models <- NULL

    # Determine covariates to test for this step.
    #   is_continuation == FALSE -> first step of a fresh run: test ALL covariates.
    #   is_continuation == TRUE  -> a continuation (a later step, or a
    #     continue_search resume): narrow to covariates from the previous step's
    #     significant models, reconstructed from the database via get_step_models().
    if (!is_continuation) {
      # First step: Test ALL available covariates
      current_best_model <- base_model_id
      cat(sprintf("First selective forward step: Starting from base model %s\n", current_best_model))
      cat("Strategy: Test ALL available covariates\n")

      covariates_to_test <- get_remaining_covariates(
        search_state = search_state,
        base_model_id = current_best_model,
        include_excluded = FALSE
      )

      step_base_model <- current_best_model

    } else {
      # Continuation: test only covariates from the previous step's significant models
      prev_step <- get_step_models(
        search_state, current_step - 1,
        p_value = forward_p_value, rse_threshold = rse_threshold
      )

      if (!isTRUE(prev_step$exists)) {
        cat(sprintf("❌ No covariate models found for previous Step %d\n", current_step - 1))
        forward_selection_active <- FALSE
        break
      }

      cat(sprintf("Selective testing from significant Step %d models\n", current_step - 1))

      # Base model = previous step's winner (reconstructed from the database)
      current_best_model <- prev_step$best_model

      if (is.null(current_best_model) || is.na(current_best_model)) {
        # Fallback: overall best (highest ΔOFV) among completed previous models
        all_previous_models <- search_state$search_database[
          !is.na(search_state$search_database$step_number) &
            search_state$search_database$step_number < current_step &
            search_state$search_database$status == "completed" &
            !is.na(search_state$search_database$delta_ofv), ]

        if (nrow(all_previous_models) == 0) {
          cat(sprintf("❌ No completed models found from previous steps\n"))
          forward_selection_active <- FALSE
          break
        }

        overall_best_idx <- which.max(all_previous_models$delta_ofv)
        current_best_model <- all_previous_models$model_name[overall_best_idx]
        cat(sprintf("📍 Using overall best model as base: %s (from Step %d)\n",
                    current_best_model, all_previous_models$step_number[overall_best_idx]))
      } else {
        cat(sprintf("📍 Using Step %d winner as base: %s\n", current_step - 1, current_best_model))
      }

      # Significant models from the previous step (read from the database)
      significant_models <- prev_step$significant_models

      if (length(significant_models) == 0) {
        cat(sprintf("❌ No significant models found in Step %d\n", current_step - 1))
        cat("🏁 Selective forward selection complete - no significant models to continue!\n")
        forward_selection_active <- FALSE
        break
      }

      cat(sprintf("📊 Found %d significant models from Step %d:\n",
                  length(significant_models), current_step - 1))
      for (model in significant_models) {
        model_row <- search_state$search_database[search_state$search_database$model_name == model, ]
        if (nrow(model_row) > 0) {
          cat(sprintf("  - %s: ΔOFV=%.2f (%s)\n",
                      model, model_row$delta_ofv[1], model_row$covariate_tested[1]))
        }
      }

      # Extract covariates from significant models
      covariates_to_test <- get_covariates_from_models(search_state, significant_models)

      cat(sprintf("📋 Extracted %d covariates from significant models\n", length(covariates_to_test)))

      # CRITICAL: Filter out covariates already in current best model
      if (length(covariates_to_test) > 0) {
        # Get covariates already in current best model
        current_model_covariates <- tryCatch({
          get_model_covariates_from_db(search_state, current_best_model)
        }, error = function(e) {
          cat(sprintf("⚠️  Warning: Could not get covariates for %s: %s\n",
                      current_best_model, e$message))
          character(0)
        })

        cat(sprintf("📋 Current best model has %d covariates\n", length(current_model_covariates)))

        # Convert model covariates (names) back to tags for comparison
        current_model_tags <- character(0)
        for (cov_name in current_model_covariates) {
          matching_tags <- names(search_state$tags)[sapply(search_state$tags, function(x) x == cov_name)]
          if (length(matching_tags) > 0) {
            current_model_tags <- c(current_model_tags, matching_tags)
          }
        }

        # Log what we're filtering
        initial_count <- length(covariates_to_test)

        # Filter out covariates already in the model
        covariates_to_test <- setdiff(covariates_to_test, current_model_tags)

        if (length(current_model_tags) > 0) {
          filtered_count <- initial_count - length(covariates_to_test)
          cat(sprintf("📋 Filtered out %d covariates already in best model\n", filtered_count))
          if (filtered_count > 0) {
            filtered_names <- sapply(current_model_tags, function(tag) {
              if (tag %in% names(search_state$tags)) search_state$tags[[tag]] else tag
            })
            cat(sprintf("   Filtered: %s\n", paste(filtered_names, collapse = ", ")))
          }
        }
      }

      step_base_model <- current_best_model  # Use current best as base
    }

    # Always update last_step_covariates_tested
    last_step_covariates_tested <- covariates_to_test

    cat(sprintf("\n📋 Covariates to test in Step %d: %d\n", current_step, length(covariates_to_test)))

    if (length(covariates_to_test) == 0) {
      cat("🏁 No covariates to test - selective forward selection complete!\n")
      forward_selection_active <- FALSE
      break
    }

    # Convert tags to names for display
    covariate_names <- sapply(covariates_to_test, function(tag) {
      if (tag %in% names(search_state$tags)) {
        search_state$tags[[tag]]
      } else {
        tag
      }
    })

    cat("Covariates:", paste(covariate_names, collapse = ", "), "\n")
    cat(sprintf("Base model for testing: %s\n", step_base_model))

    # Create models for this step
    cat(sprintf("\n🔬 Creating Step %d models...\n", current_step))

    step_result <- run_univariate_step(
      search_state = search_state,
      base_model_id = step_base_model,
      covariates_to_test = covariates_to_test,
      step_name = sprintf("Step %d: Selective Forward Selection", current_step),
      include_excluded = FALSE
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

    # Checkpoint the freshly-created step BEFORE submitting, so a crash while the
    # models are running still leaves this step's rows on disk for continue_search().
    save_search_state(search_state, sprintf("scm_selective_step_%d_created.rds", current_step))

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
    cat(sprintf("\n🏆 Evaluating Step %d models...\n", current_step))

    step_selection <- select_best_model(
      search_state = search_state,
      model_names = step_submission$completed_models,
      p_value = forward_p_value,
      rse_threshold = rse_threshold
    )

    # Update search_state
    search_state <- step_selection$search_state

    # Store step results BEFORE updating current_best_model
    step_results[[sprintf("step_%d", current_step)]] <- list(
      creation = step_result,
      submission = step_submission,
      selection = step_selection,
      base_model = step_base_model,
      models_tested = length(step_result$models_created),
      models_completed = length(step_submission$completed_models),
      significant_models_tested = significant_models,
      covariates_tested = covariates_to_test
    )

    # Check for significant models in this step

    step_significant_models <- get_significant_models_from_step(
      search_state = search_state,
      step_number = current_step,
      p_value = forward_p_value
    )

    if (length(step_significant_models) == 0) {
      cat(sprintf("❌ No significant models found in Step %d\n", current_step))
      cat("🏁 Selective forward selection complete - no significant improvements!\n")
      forward_selection_active <- FALSE
      break
    }

    cat(sprintf("✅ Step %d: Found %d significant models\n", current_step, length(step_significant_models)))


    if (!is.null(step_selection$best_model)) {
      cat(sprintf("🏆 Step %d winner: %s\n", current_step, step_selection$best_model))

      # Show what was added
      best_model_row <- search_state$search_database[
        search_state$search_database$model_name == step_selection$best_model, ]
      if (nrow(best_model_row) > 0) {
        cat(sprintf("   Added: %s (ΔOFV = %.2f)\n",
                    best_model_row$covariate_tested[1],
                    best_model_row$delta_ofv[1]))
      }
    }

    cat(sprintf("\n✅ Step %d completed successfully!\n", current_step))

    # Update for next iteration (every pass after the first is a continuation)
    current_step <- current_step + 1
    is_continuation <- TRUE
    # Save progress after each step
    save_search_state(search_state, sprintf("scm_selective_step_%d.rds", current_step - 1))
  }

  # Store last step number from main loop
  last_main_step <- current_step - 1

  # ===================================================================
  # REDEMPTION PHASE - Test remaining covariates
  # ===================================================================

  # Find the final best model from main selective forward steps (including base model)
  # Scope: base model + all main-loop steps, but exclude redemption/future steps
  all_main_models <- search_state$search_database[
    search_state$search_database$status == "completed" &
      !is.na(search_state$search_database$ofv) &
      (is.na(search_state$search_database$step_number) |  # Base model has NA step
       search_state$search_database$step_number <= last_main_step), ]  # or main-loop step

  if (nrow(all_main_models) > 0) {
    # Find model with LOWEST absolute OFV (best fit)
    final_best_model <- current_best_model
    final_best_idx <- which(all_main_models$model_name == final_best_model)
    
    # GUARD 1: Handle case where current best model not found in filtered table
    if (length(final_best_idx) == 0) {
      cat(sprintf("\n⚠️  WARNING: Current best model '%s' not found in completed models\n", final_best_model))
      cat("This may occur if the model failed, is still running, or has an invalid OFV\n")
      cat("Attempting fallback: Finding best model from available completed rows...\n")
      
      # Fallback: Find model with best (lowest) OFV from all available main models
      if (nrow(all_main_models) > 0) {
        best_ofv_idx <- which.min(all_main_models$ofv)
        final_best_model <- all_main_models$model_name[best_ofv_idx]
        final_best_idx <- best_ofv_idx
        cat(sprintf("✅ Fallback model selected: %s\n", final_best_model))
      } else {
        cat("❌ No valid completed models available for redemption - skipping redemption phase\n")
        final_best_idx <- integer(0)
      }
    }
    
    # GUARD 2: If still no valid model found, skip redemption gracefully
    if (length(final_best_idx) == 0) {
      cat("🛑 Redemption phase preconditions not met - skipping\n")
      # Redemption phase will be skipped
      
    } else {
      # Proceed with redemption using valid model
      final_best_step <- all_main_models$step_number[final_best_idx]
      final_best_ofv <- all_main_models$ofv[final_best_idx]

      cat("\n", paste(rep("=", 60), collapse=""), "\n")
      cat("🔄 REDEMPTION PHASE CHECK\n")
      cat(paste(rep("=", 60), collapse=""), "\n")
      cat(sprintf("Best model: %s (from Step %d, OFV=%.2f)\n",
                  final_best_model, final_best_step, final_best_ofv))
      cat(sprintf("Last completed step: %d\n", last_main_step))

      # Determine redemption strategy based on where best model came from
      redemption_needed <- FALSE
      redemption_covariates <- character(0)

      # Get all available covariates including excluded
      all_available_tags <- names(search_state$tags)[grepl("^beta_", names(search_state$tags))]

      # Get covariates in best model with error handling
      best_model_covariates <- tryCatch({
        get_model_covariates_from_db(search_state, final_best_model)
      }, error = function(e) {
        cat(sprintf("⚠️  Warning: Could not get covariates for %s: %s\n",
                    final_best_model, e$message))
        character(0)
      })

      # Convert to tags
      best_model_tags <- character(0)
      for (cov_name in best_model_covariates) {
        matching_tags <- names(search_state$tags)[sapply(search_state$tags, function(x) x == cov_name)]
        if (length(matching_tags) > 0) {
          best_model_tags <- c(best_model_tags, matching_tags)
        }
      }

      # GUARD 3: Ensure final_best_step is non-empty before comparison
      if (length(final_best_step) == 0) {
        cat("⚠️  WARNING: Unable to determine step number for best model - skipping scenario branching\n")
        redemption_needed <- FALSE
      } else if (final_best_step < last_main_step) {
        # Scenario B: Best model from earlier step
        cat("📋 Scenario B: Best model from earlier step - avoiding duplicates\n")

        # Remove covariates already in best model
        redemption_covariates <- setdiff(all_available_tags, best_model_tags)

        # Ensure last_step_covariates_tested is valid
        if (length(last_step_covariates_tested) > 0) {
          # Remove covariates tested in last step to avoid duplicates
          redemption_covariates <- setdiff(redemption_covariates, last_step_covariates_tested)
          cat(sprintf("Excluding %d covariates tested in Step %d\n",
                      length(last_step_covariates_tested), last_main_step))
        }

        cat(sprintf("Covariates to test: All except those in best model and tested in Step %d\n",
                    last_main_step))
        redemption_needed <- length(redemption_covariates) > 0

      } else if (final_best_step == last_main_step) {
        # Scenario A: Best model from last step
        cat("📋 Scenario A: Best model from last step - test all remaining\n")

        # Test all covariates not in best model
        redemption_covariates <- setdiff(all_available_tags, best_model_tags)

        cat("Covariates to test: All not in best model (including excluded)\n")
        redemption_needed <- length(redemption_covariates) > 0
      }

      if (redemption_needed) {
        cat(sprintf("\n🎯 STARTING REDEMPTION TESTING\n"))
        cat(sprintf("Found %d covariates for redemption testing\n", length(redemption_covariates)))

      # Convert to names for display
      redemption_names <- sapply(redemption_covariates, function(tag) {
        if (tag %in% names(search_state$tags)) search_state$tags[[tag]] else tag
      })
      cat("Covariates:", paste(redemption_names, collapse = ", "), "\n")

      redemption_active <- TRUE
      redemption_step <- 1
      current_redemption_base <- final_best_model  # Track current base for redemption

      while (redemption_active) {
        current_step_number <- last_main_step + redemption_step

        cat(sprintf("\n🔄 REDEMPTION STEP %d (Overall Step %d)\n",
                    redemption_step, current_step_number))
        cat(paste(rep("-", 40), collapse=""), "\n")

        if (redemption_step == 1) {
          # First redemption: Test all identified redemption covariates
          covariates_to_test <- redemption_covariates
          cat("Testing all redemption covariates\n")
        } else {
          # Update best model BEFORE determining covariates
          # Find current best model across ALL steps
          all_models_so_far <- search_state$search_database[
            search_state$search_database$status == "completed" &
              !is.na(search_state$search_database$ofv), ]  # Check ofv not delta_ofv

          if (nrow(all_models_so_far) > 0) {
            best_idx <- which.min(all_models_so_far$ofv)  # Use MIN OFV (best fit)
            current_redemption_base <- all_models_so_far$model_name[best_idx]
            best_ofv <- all_models_so_far$ofv[best_idx]
            cat(sprintf("Current best model updated to: %s (OFV=%.2f)\n",
                        current_redemption_base, best_ofv))
          }

          # Get significant models from previous redemption step

          prev_step_significant <- get_significant_models_from_step(
            search_state = search_state,
            step_number = current_step_number - 1,
            p_value = forward_p_value
          )

          if (length(prev_step_significant) == 0) {
            cat("❌ No significant models in previous redemption step\n")
            redemption_active <- FALSE
            break
          }

          cat(sprintf("Found %d significant models from previous redemption step\n",
                      length(prev_step_significant)))

          # Get covariates from significant models
          covariates_to_test <- get_covariates_from_models(search_state, prev_step_significant)

          # Filter with updated best model
          current_model_covariates <- tryCatch({
            get_model_covariates_from_db(search_state, current_redemption_base)
          }, error = function(e) {
            character(0)
          })

          current_model_tags <- character(0)
          for (cov_name in current_model_covariates) {
            matching_tags <- names(search_state$tags)[sapply(search_state$tags, function(x) x == cov_name)]
            if (length(matching_tags) > 0) {
              current_model_tags <- c(current_model_tags, matching_tags)
            }
          }

          initial_count <- length(covariates_to_test)
          covariates_to_test <- setdiff(covariates_to_test, current_model_tags)

          if (initial_count > length(covariates_to_test)) {
            cat(sprintf("Filtered out %d covariates already in current best model\n",
                        initial_count - length(covariates_to_test)))
          }
        }

        if (length(covariates_to_test) == 0) {
          cat("✅ No more covariates to test in redemption\n")
          redemption_active <- FALSE
          break
        }

        cat(sprintf("Testing %d covariates on %s\n",
                    length(covariates_to_test), current_redemption_base))

        # Create redemption models
        redemption_result <- run_univariate_step(
          search_state = search_state,
          base_model_id = current_redemption_base,  # Use updated base
          covariates_to_test = covariates_to_test,
          step_name = sprintf("Step %d: Redemption Testing", current_step_number),
          include_excluded = TRUE  # Include excluded covariates
        )

        search_state <- redemption_result$search_state

        if (redemption_result$status != "models_created" ||
            redemption_result$successful_count == 0) {
          cat("❌ Failed to create redemption models\n")
          redemption_active <- FALSE
          break
        }

        cat(sprintf("Created %d redemption models\n", redemption_result$successful_count))

        # Checkpoint the freshly-created step BEFORE submitting (resume/reconstruct point).
        save_search_state(search_state, sprintf("scm_selective_step_%d_created.rds", current_step_number))

        # Submit redemption models
        redemption_submission <- submit_and_wait_for_step(
          search_state = search_state,
          model_names = redemption_result$models_created,
          step_name = sprintf("Redemption Step %d Models", redemption_step),
          auto_submit = auto_submit,
          auto_retry = auto_retry
        )

        search_state <- redemption_submission$search_state

        if (length(redemption_submission$completed_models) == 0) {
          cat("❌ No redemption models completed\n")
          redemption_active <- FALSE
          break
        }

        # Evaluate redemption models

        redemption_selection <- select_best_model(
          search_state = search_state,
          model_names = redemption_submission$completed_models,
          p_value = forward_p_value,
          rse_threshold = rse_threshold
        )

        search_state <- redemption_selection$search_state

        # Check for significant improvements
        redemption_significant <- get_significant_models_from_step(
          search_state = search_state,
          step_number = current_step_number,
          p_value = forward_p_value,
          rse_threshold = rse_threshold
        )

        if (length(redemption_significant) == 0) {
          cat(sprintf("❌ No significant improvements in Redemption Step %d\n", redemption_step))
          redemption_active <- FALSE
          break
        }

        cat(sprintf("✅ Redemption Step %d found %d significant models\n",
                    redemption_step, length(redemption_significant)))

        # Update current_best_model so the final return value reflects the redemption winner
        if (!is.null(redemption_selection$best_model)) {
          current_redemption_base <- redemption_selection$best_model
          current_best_model <- redemption_selection$best_model
          cat(sprintf("📍 Redemption winner: %s (new best)\n", current_best_model))
        }

        # Store redemption results
        step_results[[sprintf("redemption_%d", redemption_step)]] <- list(
          creation = redemption_result,
          submission = redemption_submission,
          selection = redemption_selection,
          base_model = current_redemption_base,
          step_number = current_step_number
        )

        redemption_step <- redemption_step + 1

        # Save progress
        save_search_state(search_state, sprintf("scm_redemption_%d.rds", redemption_step - 1))
      }

      cat("\n✅ Redemption phase complete!\n")
      } else {
        cat("\n✅ No redemption needed - no remaining covariates to test\n")
      }
    }
  }

  # After redemption, update current_best_model to the model with lowest absolute OFV
  # that also passed the forward selection OFV criteria (using per-covariate df)
  all_completed_candidates <- search_state$search_database[
    search_state$search_database$status == "completed" &
      !is.na(search_state$search_database$ofv) &
      !is.na(search_state$search_database$delta_ofv) &
      search_state$search_database$step_number > 0, ]
  if (nrow(all_completed_candidates) > 0) {
    passed <- vapply(seq_len(nrow(all_completed_candidates)), function(i) {
      row <- all_completed_candidates[i, ]
      cov_name <- tryCatch(
        extract_covariate_name_from_tag(row$covariate_tested),
        error = function(e) NA_character_
      )
      cov_df <- tryCatch(
        calculate_covariate_df(cov_name, search_state$covariate_search),
        error = function(e) 1L
      )
      threshold <- pvalue_to_threshold(forward_p_value, df = cov_df)
      isTRUE(row$delta_ofv >= threshold)
    }, logical(1))
    all_completed <- all_completed_candidates[passed, ]
  } else {
    all_completed <- all_completed_candidates
  }
  if (nrow(all_completed) > 0) {
    current_best_model <- all_completed$model_name[which.min(all_completed$ofv)]
    cat(sprintf("🏆 Overall best model after forward+redemption: %s (OFV=%.2f)\n",
                current_best_model,
                all_completed$ofv[which.min(all_completed$ofv)]))
  }

  # ===================================================================
  # FINAL SUMMARY
  # ===================================================================

  # Calculate total time
  forward_time <- as.numeric(difftime(Sys.time(), forward_start_time, units = "mins"))

  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("🏁 SELECTIVE FORWARD SELECTION WITH REDEMPTION COMPLETE!\n")
  cat(paste(rep("=", 60), collapse=""), "\n")

  # Find absolute best model across all steps
  absolute_best_model <- current_best_model
# Get details for the final best model
  # Get details for the final best model
  final_model_info <- search_state$search_database[
    search_state$search_database$model_name == absolute_best_model, ]

  if (nrow(final_model_info) > 0) {
    absolute_best_step <- final_model_info$step_number[1]
    absolute_best_ofv <- final_model_info$ofv[1]
    absolute_best_delta <- final_model_info$delta_ofv[1]
    absolute_best_covariate <- final_model_info$covariate_tested[1]

    cat(sprintf("🏆 ABSOLUTE BEST MODEL: %s (Step %d)\n",
                absolute_best_model, absolute_best_step))
    cat(sprintf("📈 Best OFV: %.2f (ΔOFV from parent: %.2f)\n",
                absolute_best_ofv, absolute_best_delta))  # Show both OFV and delta
    cat(sprintf("🧬 Best covariate: %s\n", absolute_best_covariate))

    # Show final model composition
    final_covariates <- tryCatch({
      get_model_covariates_from_db(search_state, absolute_best_model)
    }, error = function(e) {
      character(0)
    })

    if (length(final_covariates) > 0) {
      best_covariates <- get_model_covariates_from_db(search_state, absolute_best_model)
      cat(sprintf("\n📋 Final model contains: %s\n",
                  paste(best_covariates, collapse = " + ")))
    }
  }

  cat(sprintf("\n⏱️  Total time: %.1f minutes\n", forward_time))

  # Save final state
  save_search_state(search_state, "scm_selective_complete.rds")
  cat("\n💾 Final state saved to: scm_selective_complete.rds\n")

  return(list(
    search_state = search_state,
    status = "completed",
    final_best_model = absolute_best_model,
    step_results = step_results,
    total_time_minutes = forward_time
  ))
}
