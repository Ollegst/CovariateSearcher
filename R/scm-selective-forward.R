#' Run SCM Forward Selection (CORRECTED VERSION)
#'
#' @title Execute proper stepwise forward selection with cumulative model building
#' @description Runs true forward selection where each step builds from the best model
#'   of the previous step, testing only remaining (untested) covariates. Implements
#'   standard pharmacometrics SCM methodology.
#' @param search_state List containing covariate search state and configuration
#' @param base_model_id Character. Starting base model (default: "run1")
#' @param ofv_threshold Numeric. OFV improvement threshold (uses config if NULL)
#' @param rse_threshold Numeric. Maximum RSE threshold (uses config if NULL)
#' @param auto_submit Logical. Whether to automatically submit models (default: TRUE)
#' @param auto_retry Logical. Whether to enable automatic retry (default: TRUE)
#' @return List with updated search_state and forward selection results
#' @export
run_scm_selective_forward <- function(search_state,
                                      base_model_id = NULL,
                                      ofv_threshold = NULL,
                                      rse_threshold = NULL,
                                      auto_submit = TRUE,
                                      auto_retry = TRUE) {
  if (is.null(base_model_id)) {
    base_model_id <- search_state$base_model
  }

  # Use config defaults if not specified
  if (is.null(ofv_threshold)) {
    ofv_threshold <- search_state$search_config$forward_ofv_threshold
  }
  if (is.null(rse_threshold)) {
    rse_threshold <- search_state$search_config$max_rse_threshold
  }

  cat("🚀 STARTING SCM SELECTIVE FORWARD SELECTION WORKFLOW\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  cat(sprintf("Base model: %s\n", base_model_id))
  cat(sprintf("OFV threshold: %.2f (for significance)\n", ofv_threshold))
  cat(sprintf("RSE threshold: %d%%\n", rse_threshold))
  cat(sprintf("Strategy: Test only covariates from significant models\n"))

  # Initialize workflow variables
  forward_start_time <- Sys.time()
  current_step <- 1
  current_best_model <- base_model_id
  forward_selection_active <- TRUE
  step_results <- list()
  last_step_covariates_tested <- character(0)  # Track for deduplication

  # Main selective forward selection loop
  while (forward_selection_active) {

    cat(sprintf("\n🎯 STEP %d: SELECTIVE FORWARD SELECTION\n", current_step))
    cat(paste(rep("-", 40), collapse=""), "\n")

    # Initialize significant_models outside if/else
    significant_models <- NULL

    # Determine covariates to test for this step
    if (current_step == 1) {
      # Step 1: Test ALL available covariates
      current_best_model <- base_model_id
      cat(sprintf("Step 1: Starting from base model %s\n", current_best_model))
      cat("Strategy: Test ALL available covariates\n")

      covariates_to_test <- get_remaining_covariates(
        search_state = search_state,
        base_model_id = current_best_model,
        include_excluded = FALSE
      )

      step_base_model <- current_best_model

    } else {
      # Step 2+: Test only covariates from significant models in previous step
      cat(sprintf("Step %d: Selective testing from significant Step %d models\n",
                  current_step, current_step - 1))

      # CRITICAL FIX: Use the best model from the PREVIOUS step, not overall best
      # This prevents infinite loops and duplicate models
      previous_step_results <- step_results[[sprintf("step_%d", current_step - 1)]]

      if (!is.null(previous_step_results) &&
          !is.null(previous_step_results$selection) &&
          !is.null(previous_step_results$selection$best_model)) {
        # Use the winner from the previous step
        current_best_model <- previous_step_results$selection$best_model
        cat(sprintf("📍 Using Step %d winner as base: %s\n", current_step - 1, current_best_model))
      } else {
        # Fallback: find best model from all previous steps
        all_previous_models <- search_state$search_database[
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
        best_step <- all_previous_models$step_number[overall_best_idx]
        cat(sprintf("📍 Using overall best model as base: %s (from Step %d)\n",
                    current_best_model, best_step))
      }

      # Get significant models from previous step only
      significant_models <- get_significant_models_from_step(
        search_state = search_state,
        step_number = current_step - 1,
        ofv_threshold = ofv_threshold
      )

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
      ofv_threshold = ofv_threshold,
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
      ofv_threshold = ofv_threshold
    )

    if (length(step_significant_models) == 0) {
      cat(sprintf("❌ No significant models found in Step %d\n", current_step))
      cat("🏁 Selective forward selection complete - no significant improvements!\n")
      forward_selection_active <- FALSE
      break
    }

    cat(sprintf("✅ Step %d: Found %d significant models\n", current_step, length(step_significant_models)))

    # CRITICAL FIX: Update current_best_model if this step found a better model
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

    # Update for next iteration
    current_step <- current_step + 1

    # Save progress after each step
    save_search_state(search_state, sprintf("scm_selective_step_%d.rds", current_step - 1))
  }

  # Store last step number from main loop
  last_main_step <- current_step - 1

  # ===================================================================
  # REDEMPTION PHASE - Test remaining covariates
  # ===================================================================

  # Find the final best model from main selective forward steps
  all_main_models <- search_state$search_database[
    search_state$search_database$status == "completed" &
      !is.na(search_state$search_database$ofv) &  # Changed from delta_ofv to ofv
      search_state$search_database$step_number > 0 &
      search_state$search_database$step_number <= last_main_step, ]

  if (nrow(all_main_models) > 0) {
    # Find model with LOWEST absolute OFV (best fit)
    final_best_model <- current_best_model
    final_best_idx <- which(all_main_models$model_name == final_best_model)
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
    all_available_tags <- names(search_state$tags)[grepl("^cov_", names(search_state$tags))]

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

    if (final_best_step < last_main_step) {
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
            ofv_threshold = ofv_threshold
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
          ofv_threshold = ofv_threshold,
          rse_threshold = rse_threshold
        )

        search_state <- redemption_selection$search_state

        # Check for significant improvements
        redemption_significant <- get_significant_models_from_step(
          search_state = search_state,
          step_number = current_step_number,
          ofv_threshold = ofv_threshold
        )

        if (length(redemption_significant) == 0) {
          cat(sprintf("❌ No significant improvements in Redemption Step %d\n", redemption_step))
          redemption_active <- FALSE
          break
        }

        cat(sprintf("✅ Redemption Step %d found %d significant models\n",
                    redemption_step, length(redemption_significant)))

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

#' Get Significant Models from Step
#'
#' @title Extract models that showed significant improvement in a specific step
#' @description Returns model names from a step that have ΔOFV above threshold
#' @param search_state List containing search state
#' @param step_number Integer. Step number to check
#' @param ofv_threshold Numeric. OFV threshold for significance
#' @return Character vector of significant model names
#' @export
get_significant_models_from_step <- function(search_state, step_number, ofv_threshold) {

  # Validate inputs
  if (is.null(search_state$search_database) || nrow(search_state$search_database) == 0) {
    return(character(0))
  }

  # Check required columns exist
  required_cols <- c("step_number", "status", "delta_ofv", "model_name")
  if (!all(required_cols %in% names(search_state$search_database))) {
    warning("Missing required columns in search database")
    return(character(0))
  }

  # Get models from specified step
  step_models <- search_state$search_database[
    search_state$search_database$step_number == step_number &
      search_state$search_database$status == "completed" &
      !is.na(search_state$search_database$delta_ofv), ]

  if (nrow(step_models) == 0) {
    return(character(0))
  }

  # Filter for significant models
  significant_models <- step_models[step_models$delta_ofv > ofv_threshold, ]

  if (nrow(significant_models) == 0) {
    return(character(0))
  }

  return(significant_models$model_name)
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

  if (length(model_names) == 0) {
    return(character(0))
  }

  # Validate search_state and database
  if (is.null(search_state$search_database) || nrow(search_state$search_database) == 0) {
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

  if (nrow(model_rows) == 0) {
    return(character(0))
  }

  # Extract covariate names and convert back to tags
  covariate_names <- unique(model_rows$covariate_tested)
  covariate_names <- covariate_names[!is.na(covariate_names) & covariate_names != ""]

  if (length(covariate_names) == 0) {
    return(character(0))
  }

  # Validate tags exist
  if (is.null(search_state$tags) || length(search_state$tags) == 0) {
    warning("No tags found in search_state")
    return(character(0))
  }

  # Convert covariate names back to tags
  covariate_tags <- character(0)
  for (cov_name in covariate_names) {
    # Safe comparison with validation
    matching_tags <- names(search_state$tags)[sapply(search_state$tags, function(x) {
      identical(as.character(x), as.character(cov_name))
    })]

    if (length(matching_tags) > 0) {
      covariate_tags <- c(covariate_tags, matching_tags[1])
    }
  }

  return(unique(covariate_tags))
}






#' Resume Selective Forward Selection from Checkpoint
#'
#' @title Resume SCM selective forward selection workflow from a saved checkpoint
#' @description Loads a previously saved search state and continues selective forward
#'   selection from where it left off. Can handle incomplete models, update statuses,
#'   and determine the appropriate next step (continue forward selection or start
#'   redemption phase). Provides flexible resumption options for interrupted workflows.
#' @param checkpoint_file Character. Path to the RDS file containing saved search state
#'   (e.g., "scm_selective_step_3.rds", "scm_forward_selection_complete.rds")
#' @param ofv_threshold Numeric. OFV improvement threshold for significance testing.
#'   If NULL, uses value from search_state$search_config$forward_ofv_threshold (default: 3.84)
#' @param rse_threshold Numeric. Maximum acceptable RSE threshold as percentage.
#'   If NULL, uses value from search_state$search_config$max_rse_threshold (default: 50)
#' @param auto_submit Logical. Whether to automatically resubmit incomplete models
#'   found in the last step (default: TRUE)
#' @param auto_retry Logical. Whether to enable automatic retry creation for failed
#'   models during resubmission (default: TRUE)
#' @param continue_forward Logical. Whether to automatically continue forward selection
#'   if possible, or just load and prepare the state (default: TRUE)
#' @return List containing:
#'   \itemize{
#'     \item \code{search_state} - Updated search state with current model statuses
#'     \item \code{status} - Resume status: "ready", "ready_to_continue", "forward_complete", "no_completed_models"
#'     \item \code{best_model} - Current best model across all completed steps
#'     \item \code{best_model_step} - Step number where best model was found
#'     \item \code{best_model_ofv} - OFV of the best model
#'     \item \code{last_step} - Last step number found in database
#'     \item \code{next_step} - Suggested next step number (if continuing)
#'     \item \code{redemption_suggested} - Whether redemption phase is recommended
#'     \item \code{message} - Descriptive message about resume status
#'   }
#' @details
#' This function performs the following operations:
#' \enumerate{
#'   \item Loads the checkpoint file and validates search state structure
#'   \item Determines the last completed step and identifies incomplete models
#'   \item Updates model statuses from NONMEM output files
#'   \item Optionally resubmits incomplete models if auto_submit=TRUE
#'   \item Identifies the current best model (lowest OFV) across all steps
#'   \item Determines appropriate next action:
#'     \itemize{
#'       \item Continue forward selection if significant models exist in last step
#'       \item Suggest redemption phase if best model is from earlier step
#'       \item Mark as complete if no significant improvements possible
#'     }
#'   \item Provides recommendations for next steps based on current state
#' }
#' @section Checkpoint Files:
#' Common checkpoint files created by selective forward selection:
#' \itemize{
#'   \item \code{scm_selective_step_N.rds} - Saved after each completed step
#'   \item \code{scm_selective_complete.rds} - Final state after completion
#'   \item \code{scm_redemption_N.rds} - Saved during redemption phase
#' }
#' @section Resume Scenarios:
#' \describe{
#'   \item{Incomplete Models}{If models from the last step are still running or failed,
#'     they will be updated and optionally resubmitted}
#'   \item{Continue Forward}{If significant models exist in last step and best model
#'     is from that step, forward selection can continue}
#'   \item{Redemption Phase}{If best model is from an earlier step, redemption
#'     testing of remaining covariates is suggested}
#'   \item{Complete}{If no significant models exist in last step and best model
#'     is from that step, forward selection is complete}
#' }
#' @examples
#' \dontrun{
#' # Resume from step 3 checkpoint and continue automatically
#' result <- resume_selective_forward("scm_selective_step_3.rds")
#' search_state <- result$search_state
#'
#' # Resume with custom thresholds, just load state without continuing
#' result <- resume_selective_forward(
#'   checkpoint_file = "scm_selective_step_5.rds",
#'   ofv_threshold = 4.0,
#'   rse_threshold = 40,
#'   continue_forward = FALSE
#' )
#'
#' # Resume and resubmit incomplete models without auto-retry
#' result <- resume_selective_forward(
#'   checkpoint_file = "scm_selective_step_2.rds",
#'   auto_submit = TRUE,
#'   auto_retry = FALSE
#' )
#'
#' # Check what would happen next without actually continuing
#' status <- resume_selective_forward(
#'   checkpoint_file = "scm_selective_complete.rds",
#'   continue_forward = FALSE
#' )
#' cat("Status:", status$status)
#' cat("Best model:", status$best_model)
#' cat("Redemption suggested:", status$redemption_suggested)
#' }
#' @seealso
#' \code{\link{run_scm_selective_forward}} for starting selective forward selection,
#' \code{\link{save_search_state}} for creating checkpoint files,
#' \code{\link{get_significant_models_from_step}} for analyzing step results
#' @export

resume_selective_forward <- function(checkpoint_file,
                                     ofv_threshold = NULL,
                                     rse_threshold = NULL,
                                     auto_submit = TRUE,
                                     auto_retry = TRUE,
                                     continue_forward = TRUE) {

  # Load saved state
  cat(sprintf("\n📂 RESUMING FROM CHECKPOINT: %s\n", checkpoint_file))
  cat(paste(rep("=", 60), collapse=""), "\n")

  # Check if file exists
  if (!file.exists(checkpoint_file)) {
    stop(sprintf("Checkpoint file not found: %s", checkpoint_file))
  }

  # Load the search state
  search_state <- tryCatch({
    readRDS(checkpoint_file)
  }, error = function(e) {
    stop(sprintf("Error loading checkpoint file: %s", e$message))
  })

  cat(sprintf("✅ Loaded search state with %d models\n", nrow(search_state$search_database)))

  # Use config defaults if not specified
  if (is.null(ofv_threshold)) {
    ofv_threshold <- search_state$search_config$forward_ofv_threshold
  }
  if (is.null(rse_threshold)) {
    rse_threshold <- search_state$search_config$max_rse_threshold
  }

  cat(sprintf("📊 OFV threshold: %.2f\n", ofv_threshold))
  cat(sprintf("📊 RSE threshold: %d%%\n", rse_threshold))

  # Determine where we left off
  last_step <- max(search_state$search_database$step_number, na.rm = TRUE)
  if (is.na(last_step) || is.infinite(last_step)) {
    last_step <- 0
  }

  cat(sprintf("\n📍 Last step in database: %d\n", last_step))

  # Check models in last step
  if (last_step > 0) {
    last_step_models <- search_state$search_database %>%
      dplyr::filter(step_number == last_step)

    incomplete_models <- last_step_models %>%
      dplyr::filter(status %in% c("in_progress", "created", "submitted", "unknown"))

    completed_models <- last_step_models %>%
      dplyr::filter(status == "completed")

    failed_models <- last_step_models %>%
      dplyr::filter(status == "failed")

    cat(sprintf("Step %d status:\n", last_step))
    cat(sprintf("  ✅ Completed: %d\n", nrow(completed_models)))
    cat(sprintf("  🔄 Incomplete: %d\n", nrow(incomplete_models)))
    cat(sprintf("  ❌ Failed: %d\n", nrow(failed_models)))

    # Handle incomplete models if any
    if (nrow(incomplete_models) > 0) {
      cat(sprintf("\n⚠️  Step %d has %d incomplete models\n", last_step, nrow(incomplete_models)))

      # Update status from files first
      cat("📊 Updating model statuses from files...\n")
      for (i in 1:nrow(incomplete_models)) {
        model_name <- incomplete_models$model_name[i]
        cat(sprintf("  Checking %s...\n", model_name))
        search_state <- update_model_status_from_files(search_state, model_name)
      }

      # Re-check after update
      still_incomplete <- search_state$search_database %>%
        dplyr::filter(model_name %in% incomplete_models$model_name,
                      status %in% c("in_progress", "created", "submitted", "unknown"))

      if (nrow(still_incomplete) > 0) {
        cat(sprintf("\n📊 %d models still incomplete. Options:\n", nrow(still_incomplete)))
        cat("  1. Resubmit these models (if auto_submit = TRUE)\n")
        cat("  2. Continue without them\n")

        if (auto_submit) {
          cat("\n🚀 Resubmitting incomplete models...\n")

          step_submission <- submit_and_wait_for_step(
            search_state = search_state,
            model_names = still_incomplete$model_name,
            step_name = sprintf("Step %d Models (Resumed)", last_step),
            auto_submit = auto_submit,
            auto_retry = auto_retry
          )
          search_state <- step_submission$search_state

          # Save updated state
          save_search_state(search_state, sprintf("scm_resumed_step_%d.rds", last_step))
        } else {
          cat("⏭️  Skipping resubmission (auto_submit = FALSE)\n")
        }
      } else {
        cat("✅ All models from Step %d are now complete\n", last_step)
      }
    }
  }

  # Find the best model across all completed steps
  all_completed <- search_state$search_database %>%
    dplyr::filter(status == "completed", !is.na(ofv))

  if (nrow(all_completed) == 0) {
    cat("\n❌ No completed models found in the database\n")
    cat("Cannot continue without at least one completed model\n")
    return(list(
      search_state = search_state,
      status = "no_completed_models",
      message = "No completed models to continue from"
    ))
  }

  # Get best model (lowest OFV)
  best_model_idx <- which.min(all_completed$ofv)
  best_model <- all_completed$model_name[best_model_idx]
  best_model_step <- all_completed$step_number[best_model_idx]
  best_model_ofv <- all_completed$ofv[best_model_idx]

  cat(sprintf("\n🏆 Best model overall: %s\n", best_model))
  cat(sprintf("   From Step: %d\n", best_model_step))
  cat(sprintf("   OFV: %.2f\n", best_model_ofv))

  # Get covariates in best model
  best_model_covariates <- tryCatch({
    get_model_covariates_from_db(search_state, best_model)
  }, error = function(e) {
    character(0)
  })

  if (length(best_model_covariates) > 0) {
    cat(sprintf("   Covariates: %s\n", paste(best_model_covariates, collapse = " + ")))
  }

  # Determine next action
  cat("\n📋 DETERMINING NEXT ACTION...\n")

  if (!continue_forward) {
    cat("✅ Resume complete (continue_forward = FALSE)\n")
    cat("Search state loaded and updated. Ready for manual continuation.\n")

    return(list(
      search_state = search_state,
      status = "ready",
      best_model = best_model,
      best_model_step = best_model_step,
      best_model_ofv = best_model_ofv,
      last_step = last_step,
      message = "State loaded and ready for manual continuation"
    ))
  }

  # Check if we should continue forward selection or start redemption
  if (last_step > 0) {
    last_step_significant <- get_significant_models_from_step(
      search_state = search_state,
      step_number = last_step,
      ofv_threshold = ofv_threshold
    )

    if (length(last_step_significant) == 0) {
      cat(sprintf("❌ No significant models found in Step %d\n", last_step))

      if (best_model_step < last_step) {
        cat("📊 Best model is from an earlier step - redemption phase may be needed\n")
        cat("\n💡 Suggestion: Run redemption testing with:\n")
        cat("   - All remaining covariates not in best model\n")
        cat("   - Including previously excluded covariates\n")

        # Get remaining covariates for redemption
        all_available_tags <- names(search_state$tags)[grepl("^cov_", names(search_state$tags))]

        # Convert best model covariates to tags
        best_model_tags <- character(0)
        for (cov_name in best_model_covariates) {
          matching_tags <- names(search_state$tags)[sapply(search_state$tags, function(x) x == cov_name)]
          if (length(matching_tags) > 0) {
            best_model_tags <- c(best_model_tags, matching_tags)
          }
        }

        redemption_covariates <- setdiff(all_available_tags, best_model_tags)

        if (length(redemption_covariates) > 0) {
          redemption_names <- sapply(redemption_covariates, function(tag) {
            if (tag %in% names(search_state$tags)) search_state$tags[[tag]] else tag
          })
          cat(sprintf("\n📋 %d covariates available for redemption:\n", length(redemption_covariates)))
          cat(sprintf("   %s\n", paste(redemption_names, collapse = ", ")))
        }
      } else {
        cat("✅ Best model is from the last step - forward selection complete\n")
      }

      return(list(
        search_state = search_state,
        status = "forward_complete",
        best_model = best_model,
        best_model_step = best_model_step,
        best_model_ofv = best_model_ofv,
        last_step = last_step,
        redemption_suggested = (best_model_step < last_step),
        message = "Forward selection complete, ready for redemption phase if needed"
      ))
    }

    cat(sprintf("✅ Found %d significant models in Step %d\n",
                length(last_step_significant), last_step))
    cat("📊 Can continue with forward selection\n")
  }

  # Prepare to continue forward selection
  cat("\n🚀 READY TO CONTINUE FORWARD SELECTION\n")
  cat(sprintf("Next step will be: Step %d\n", last_step + 1))
  cat(sprintf("Base model: %s\n", best_model))

  # Get covariates from last step's significant models for next step
  if (last_step > 0) {
    last_step_significant_models <- get_significant_models_from_step(
      search_state = search_state,
      step_number = last_step,
      ofv_threshold = ofv_threshold
    )

    if (length(last_step_significant_models) > 0) {
      next_covariates <- get_covariates_from_models(search_state, last_step_significant_models)

      # Filter out covariates already in best model
      best_model_tags <- character(0)
      for (cov_name in best_model_covariates) {
        matching_tags <- names(search_state$tags)[sapply(search_state$tags, function(x) x == cov_name)]
        if (length(matching_tags) > 0) {
          best_model_tags <- c(best_model_tags, matching_tags)
        }
      }

      next_covariates <- setdiff(next_covariates, best_model_tags)

      if (length(next_covariates) > 0) {
        next_names <- sapply(next_covariates, function(tag) {
          if (tag %in% names(search_state$tags)) search_state$tags[[tag]] else tag
        })
        cat(sprintf("\nCovariates for next step: %s\n", paste(next_names, collapse = ", ")))
      } else {
        cat("\n⚠️  No new covariates to test in next step\n")
      }
    }
  }

  cat("\n💡 To continue, run:\n")
  cat("   result <- run_scm_selective_forward(search_state, ...)\n")
  cat("   Or use the manual step-by-step approach\n")

  return(list(
    search_state = search_state,
    status = "ready_to_continue",
    best_model = best_model,
    best_model_step = best_model_step,
    best_model_ofv = best_model_ofv,
    last_step = last_step,
    next_step = last_step + 1,
    message = sprintf("Ready to continue from Step %d with model %s", last_step + 1, best_model)
  ))
}

