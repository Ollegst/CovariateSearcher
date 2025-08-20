#' Run Complete Automated SCM Testing from Base Model (FIXED)
#'
#' @title Execute full automated stepwise covariate modeling workflow from scratch
#' @description Runs complete end-to-end SCM testing starting from base model through
#'   forward selection and final model validation. Includes automatic retry, error
#'   recovery, progress monitoring, and comprehensive reporting. Designed for hands-off
#'   execution with intelligent decision making.
#' @param search_state List containing covariate search state and configuration
#' @param base_model_id Character. Starting base model name (default: "run1")
#' @param scm_type Character. Type of SCM algorithm to use:
#'   \itemize{
#'     \item "standard" - Traditional SCM testing all covariates each step
#'     \item "selective" - Selective SCM testing only significant model covariates
#'   }
#' @param starting_phase Character. Which phase to start with:
#'   \itemize{
#'     \item "forward" - Start with forward selection (SCM)
#'     \item "backward" - Start with backward elimination (SCM+)
#'   }
#' @param full_scm Logical. Whether to run complete SCM workflow.
#'   If TRUE: Always runs Forward selection → Backward elimination (regardless of starting_phase)
#'   If FALSE: Runs only the specified starting_phase
#'   Note: True SCM should always include both forward and backward phases (default: TRUE)
#' @param forward_ofv_threshold Numeric. OFV improvement threshold for forward selection.
#'   If NULL, uses search_state$search_config$forward_ofv_threshold (default: 3.84)
#' @param backward_ofv_threshold Numeric. OFV threshold for backward elimination.
#'   If NULL, uses 6.63 (more stringent than forward). Only used when starting_phase = "backward".
#' @param rse_threshold Numeric. Maximum RSE threshold as percentage.
#'   If NULL, uses search_state$search_config$max_rse_threshold (default: 50)
#' @param auto_submit Logical. Whether to automatically submit models to cluster (default: TRUE)
#' @param auto_retry Logical. Whether to enable automatic retry for failed models (default: TRUE)
#' @param save_checkpoints Logical. Whether to save state after each major step (default: TRUE)
#' @param checkpoint_prefix Character. Prefix for checkpoint filenames (default: "scm_auto")
#' @param final_testing Logical. Whether to test excluded covariates on final model (default: TRUE)
#' @return List containing comprehensive SCM results
#' @export
run_automated_scm_testing <- function(search_state,
                                      base_model_id = "run1",
                                      scm_type = c("standard", "selective"),
                                      starting_phase = c("forward", "backward"),
                                      full_scm = TRUE,
                                      forward_ofv_threshold = NULL,
                                      backward_ofv_threshold = NULL,
                                      rse_threshold = NULL,
                                      auto_submit = TRUE,
                                      auto_retry = TRUE,
                                      save_checkpoints = TRUE,
                                      checkpoint_prefix = "scm_auto",
                                      final_testing = TRUE) {

  # FIXED: Input validation with proper match.arg()
  if (is.null(search_state) || !is.list(search_state)) {
    stop("search_state must be a valid list")
  }

  scm_type <- match.arg(scm_type)
  starting_phase <- match.arg(starting_phase)  # FIXED: Added missing match.arg()

  # FIXED: Consistent workflow logic variables
  run_forward <- FALSE
  run_initial_backward <- FALSE
  run_final_backward <- FALSE

  # FIXED: Clear workflow logic
  if (full_scm) {
    if (starting_phase == "forward") {
      # Standard SCM: Forward → Backward
      run_forward <- TRUE
      run_final_backward <- TRUE
      cat("🔄 Full SCM workflow: Forward selection → Backward elimination\n")
    } else if (starting_phase == "backward") {
      # SCM+ starting with backward: Backward → Forward → Backward
      run_initial_backward <- TRUE
      run_forward <- TRUE
      run_final_backward <- TRUE
      cat("🔄 Full SCM+ workflow: Backward elimination → Forward selection → Backward elimination\n")
    }
  } else {
    # Single phase only
    if (starting_phase == "forward") {
      run_forward <- TRUE
      cat("📊 Forward selection only\n")
    } else {
      run_initial_backward <- TRUE
      cat("📉 Backward elimination only\n")
    }
  }

  # FIXED: Enhanced input validation
  if (!is.null(forward_ofv_threshold) && (forward_ofv_threshold <= 0)) {
    stop("forward_ofv_threshold must be positive")
  }
  if (!is.null(backward_ofv_threshold) && (backward_ofv_threshold <= 0)) {
    stop("backward_ofv_threshold must be positive")
  }
  if (!is.null(rse_threshold) && (rse_threshold <= 0 || rse_threshold > 100)) {
    stop("rse_threshold must be between 0 and 100")
  }


  # Set defaults with proper null coalescing
  if (is.null(forward_ofv_threshold)) {
    forward_ofv_threshold <- search_state$search_config$forward_ofv_threshold %||% 3.84
  }
  if (is.null(backward_ofv_threshold)) {
    backward_ofv_threshold <- 6.63  # More stringent than forward
  }
  if (is.null(rse_threshold)) {
    rse_threshold <- search_state$search_config$max_rse_threshold %||% 50
  }

  cat("🚀 STARTING AUTOMATED SCM TESTING\n")
  cat(paste(rep("=", 80), collapse=""), "\n")
  cat(sprintf("SCM type: %s\n", scm_type))
  if (full_scm) {
    if (starting_phase == "forward") {
      cat("Workflow: Full SCM (Forward → Backward)\n")
    } else {
      cat("Workflow: Full SCM+ (Backward → Forward → Backward)\n")
    }
  } else {
    cat(sprintf("Single phase: %s only\n", starting_phase))
  }
  cat(sprintf("Base model: %s\n", base_model_id))
  cat(sprintf("Forward OFV threshold: %.2f\n", forward_ofv_threshold))
  cat(sprintf("Backward OFV threshold: %.2f\n", backward_ofv_threshold))
  cat(sprintf("RSE threshold: %d%%\n", rse_threshold))
  cat(sprintf("Auto-retry enabled: %s\n", auto_retry))
  cat(sprintf("Checkpoints enabled: %s\n", save_checkpoints))
  cat(sprintf("Final testing enabled: %s\n", final_testing))
  cat(paste(rep("=", 80), collapse=""), "\n")

  # Initialize tracking
  scm_start_time <- Sys.time()
  checkpoint_files <- character(0)
  forward_results <- NULL
  initial_backward_results <- NULL
  final_backward_results <- NULL
  current_model <- base_model_id

  # ===================================================================
  # PHASE 1: INITIAL BACKWARD ELIMINATION (if starting with backward)
  # ===================================================================

  if (run_initial_backward) {
    cat("\n📉 PHASE 1: INITIAL BACKWARD ELIMINATION\n")
    cat(paste(rep("-", 50), collapse=""), "\n")

    cat(sprintf("Starting backward elimination from: %s\n", current_model))
    cat(sprintf("Using backward OFV threshold: %.2f\n", backward_ofv_threshold))

    # Note: Backward elimination function would be implemented here
    cat("⚠️ Backward elimination not yet implemented\n")

    # Placeholder for future backward elimination implementation
    initial_backward_results <- list(
      status = "not_implemented",
      message = "Backward elimination will be implemented in future versions",
      starting_model = current_model,
      final_model = current_model
    )

    # For now, no change to current_model since backward not implemented
    # current_model would be updated to the result of backward elimination

    if (save_checkpoints) {
      checkpoint_file <- paste0(checkpoint_prefix, "_initial_backward_complete.rds")
      save_search_state(search_state, checkpoint_file)
      checkpoint_files <- c(checkpoint_files, checkpoint_file)
    }
  }

  # ===================================================================
  # PHASE 2: FORWARD SELECTION
  # ===================================================================

  if (run_forward) {
    phase_num <- if (run_initial_backward) "2" else "1"
    cat(sprintf("\n📊 PHASE %s: FORWARD SELECTION\n", phase_num))
    cat(paste(rep("-", 50), collapse=""), "\n")

    forward_error <- NULL

    forward_results <- tryCatch({
      if (scm_type == "standard") {
        # Standard forward selection
        run_stepwise_covariate_modeling(
          search_state = search_state,
          base_model_id = current_model,
          auto_submit = auto_submit,
          ofv_threshold = forward_ofv_threshold,
          rse_threshold = rse_threshold
        )

      } else if (scm_type == "selective") {
        # Selective forward selection
        run_scm_selective_forward(
          search_state = search_state,
          base_model_id = current_model,
          ofv_threshold = forward_ofv_threshold,
          rse_threshold = rse_threshold,
          auto_submit = auto_submit,
          auto_retry = auto_retry
        )
      }

    }, error = function(e) {
      forward_error <<- e$message
      cat(sprintf("❌ Forward selection failed: %s\n", e$message))
      NULL
    })

    # Handle forward selection failure
    if (is.null(forward_results)) {
      if (save_checkpoints) {
        error_file <- paste0(checkpoint_prefix, "_forward_error.rds")
        save_search_state(search_state, error_file)
        checkpoint_files <- c(checkpoint_files, error_file)
      }

      total_time <- as.numeric(difftime(Sys.time(), scm_start_time, units = "mins"))

      return(list(
        search_state = search_state,
        status = "failed",
        scm_type = scm_type,
        starting_phase = starting_phase,
        full_scm = full_scm,
        error_phase = "forward_selection",
        error_message = forward_error,
        final_model = current_model,
        base_model = base_model_id,
        forward_results = NULL,
        initial_backward_results = initial_backward_results,
        final_backward_results = NULL,
        final_covariates = character(0),
        excluded_covariates = get_excluded_covariates(search_state),
        total_models_created = nrow(search_state$search_database) - 1,
        total_time_minutes = total_time,
        checkpoint_files = checkpoint_files,
        final_summary = sprintf("SCM failed during forward selection after %.1f minutes", total_time)
      ))
    }

    # FIXED: Update search state and current model from forward results with proper field names
    search_state <- forward_results$search_state

    # FIXED: Access correct field names from different SCM functions
    current_model <- forward_results$final_best_model %||%  # from run_scm_selective_forward
      forward_results$final_model %||%                      # from run_stepwise_covariate_modeling
      current_model                                         # fallback to original

    # Save checkpoint after forward selection
    if (save_checkpoints) {
      checkpoint_file <- paste0(checkpoint_prefix, "_forward_complete.rds")
      save_search_state(search_state, checkpoint_file)
      checkpoint_files <- c(checkpoint_files, checkpoint_file)
      cat(sprintf("💾 Forward selection checkpoint saved: %s\n", checkpoint_file))
    }

    cat("✅ Forward selection completed successfully\n")
    cat(sprintf("Best forward model: %s\n", current_model))
  }

  # ===================================================================
  # PHASE: INITIAL BACKWARD ELIMINATION (if starting with backward)
  # ===================================================================

  if (run_initial_backward) {
    cat("\n📉 INITIAL BACKWARD ELIMINATION PHASE\n")
    cat(paste(rep("-", 50), collapse=""), "\n")

    cat(sprintf("Starting backward elimination from: %s\n", current_model))
    cat(sprintf("Using backward OFV threshold: %.2f\n", backward_ofv_threshold))

    initial_backward_error <- NULL

    initial_backward_results <- tryCatch({
      run_backward_elimination(
        search_state = search_state,
        starting_model = current_model,
        ofv_threshold = backward_ofv_threshold,
        auto_submit = auto_submit,
        auto_retry = auto_retry
      )
    }, error = function(e) {
      initial_backward_error <<- e$message
      cat(sprintf("❌ Initial backward elimination failed: %s\n", e$message))
      NULL
    })

    if (!is.null(initial_backward_results)) {
      search_state <- initial_backward_results$search_state

      # Update current model from backward elimination results
      current_model <- initial_backward_results$final_model %||%
        initial_backward_results$best_model %||%
        current_model

      cat(sprintf("✅ Initial backward elimination completed\n"))
      cat(sprintf("🎯 Model after backward elimination: %s\n", current_model))

      if (save_checkpoints) {
        checkpoint_file <- paste0(checkpoint_prefix, "_initial_backward_complete.rds")
        save_search_state(search_state, checkpoint_file)
        checkpoint_files <- c(checkpoint_files, checkpoint_file)
      }
    } else {
      cat("❌ Initial backward elimination failed - continuing with current model\n")
    }
  }

  # ===================================================================
  # PHASE: FINAL BACKWARD ELIMINATION (for full SCM)
  # ===================================================================

  if (run_final_backward) {
    phase_num <- if (run_initial_backward) "FINAL" else ""
    cat(sprintf("\n📉 %s BACKWARD ELIMINATION PHASE\n", phase_num))
    cat(paste(rep("-", 50), collapse=""), "\n")

    cat(sprintf("Starting final backward elimination from: %s\n", current_model))
    cat(sprintf("Using backward OFV threshold: %.2f\n", backward_ofv_threshold))

    final_backward_error <- NULL

    final_backward_results <- tryCatch({
      run_backward_elimination(
        search_state = search_state,
        starting_model = current_model,
        ofv_threshold = backward_ofv_threshold,
        auto_submit = auto_submit,
        auto_retry = auto_retry
      )
    }, error = function(e) {
      final_backward_error <<- e$message
      cat(sprintf("❌ Final backward elimination failed: %s\n", e$message))
      NULL
    })

    if (!is.null(final_backward_results)) {
      search_state <- final_backward_results$search_state

      # Update current model from backward elimination results
      current_model <- final_backward_results$final_model %||%
        final_backward_results$best_model %||%
        current_model

      cat(sprintf("✅ Final backward elimination completed\n"))
      cat(sprintf("🎯 Final model after backward elimination: %s\n", current_model))

      if (save_checkpoints) {
        checkpoint_file <- paste0(checkpoint_prefix, "_final_backward_complete.rds")
        save_search_state(search_state, checkpoint_file)
        checkpoint_files <- c(checkpoint_files, checkpoint_file)
      }
    } else {
      cat("❌ Final backward elimination failed - using model from forward selection\n")
    }
  }

  # ===================================================================
  # DETERMINE FINAL MODEL
  # ===================================================================

  cat(sprintf("\n🎯 FINAL MODEL DETERMINATION\n"))
  cat(paste(rep("-", 50), collapse=""), "\n")

  # Final model is the current_model after all phases
  final_model <- current_model
  cat(sprintf("Final model determined: %s\n", final_model))

  # FIXED: Get final model composition with proper error handling
  final_covariates <- tryCatch({
    get_model_covariates_from_db(search_state, final_model)
  }, error = function(e) {
    cat(sprintf("Warning: Could not get covariates for %s: %s\n", final_model, e$message))
    character(0)
  })

  # Get excluded covariates
  excluded_covariates <- tryCatch({
    get_excluded_covariates(search_state)
  }, error = function(e) {
    cat(sprintf("Warning: Could not get excluded covariates: %s\n", e$message))
    character(0)
  })

  # ===================================================================
  # FINAL TESTING OF EXCLUDED COVARIATES (only for forward-ending workflows)
  # ===================================================================

  # Only do final testing if enabled, we ran forward selection, and not doing full SCM
  if (final_testing && run_forward && !run_final_backward) {
    if (length(excluded_covariates) > 0) {
      cat("\n🔬 FINAL TESTING OF EXCLUDED COVARIATES\n")
      cat(paste(rep("-", 50), collapse=""), "\n")

      cat(sprintf("Testing %d excluded covariates on final model %s\n",
                  length(excluded_covariates), final_model))
      cat(sprintf("Excluded covariates: %s\n", paste(excluded_covariates, collapse = ", ")))

      # FIXED: Convert excluded covariate names back to tags with proper error handling
      excluded_tags <- character(0)
      for (cov_name in excluded_covariates) {
        tryCatch({
          matching_tags <- names(search_state$tags)[sapply(search_state$tags, function(x) x == cov_name)]
          if (length(matching_tags) > 0) {
            excluded_tags <- c(excluded_tags, matching_tags[1])
          }
        }, error = function(e) {
          cat(sprintf("Warning: Could not convert covariate %s to tag: %s\n", cov_name, e$message))
        })
      }

      if (length(excluded_tags) > 0) {
        final_testing_result <- tryCatch({
          run_univariate_step(
            search_state = search_state,
            base_model_id = final_model,
            covariates_to_test = excluded_tags,
            step_name = "Final Testing: Excluded Covariates",
            include_excluded = TRUE
          )
        }, error = function(e) {
          cat(sprintf("❌ Final testing failed: %s\n", e$message))
          NULL
        })

        if (!is.null(final_testing_result) && final_testing_result$status == "models_created") {
          search_state <- final_testing_result$search_state

          # Submit and wait for final testing models
          final_submission <- tryCatch({
            submit_and_wait_for_step(
              search_state = search_state,
              model_names = final_testing_result$models_created,
              step_name = "Final Testing Models",
              auto_submit = auto_submit,
              auto_retry = auto_retry
            )
          }, error = function(e) {
            cat(sprintf("❌ Final testing submission failed: %s\n", e$message))
            NULL
          })

          if (!is.null(final_submission)) {
            search_state <- final_submission$search_state

            # Check if any final testing models are better
            if (length(final_submission$completed_models) > 0) {
              final_evaluation <- tryCatch({
                select_best_model(
                  search_state = search_state,
                  model_names = final_submission$completed_models,
                  ofv_threshold = forward_ofv_threshold,
                  rse_threshold = rse_threshold
                )
              }, error = function(e) {
                cat(sprintf("❌ Final evaluation failed: %s\n", e$message))
                NULL
              })

              # FIXED: Access correct field name - select_best_model() returns 'best_model'
              if (!is.null(final_evaluation) && !is.null(final_evaluation$best_model)) {
                search_state <- final_evaluation$search_state
                cat(sprintf("🎉 Final testing found better model: %s\n", final_evaluation$best_model))
                final_model <- final_evaluation$best_model

                # Update final covariates
                final_covariates <- tryCatch({
                  get_model_covariates_from_db(search_state, final_model)
                }, error = function(e) {
                  character(0)
                })
              } else {
                cat("✅ Final testing complete - no significant improvements found\n")
              }
            }
          }
        } else {
          cat("⚠️ Final testing model creation failed\n")
        }
      } else {
        cat("⚠️ Could not convert excluded covariate names to tags\n")
      }
    } else {
      cat("\n✅ PHASE 3: No excluded covariates to test\n")
    }
  } else {
    if (!final_testing) {
      cat("\n⏭️ PHASE 3: Final testing disabled\n")
    } else if (run_final_backward) {
      cat("\n⏭️ PHASE 3: Final testing not applicable for backward elimination\n")
      cat("(Excluded covariates would be handled during backward elimination process)\n")
    } else {
      cat("\n⏭️ PHASE 3: No forward selection performed\n")
    }
  }

  # ===================================================================
  # FINAL SUMMARY AND REPORTING
  # ===================================================================

  cat("\n📊 FINAL ANALYSIS\n")
  cat(paste(rep("-", 50), collapse=""), "\n")

  # Calculate final statistics
  total_time <- as.numeric(difftime(Sys.time(), scm_start_time, units = "mins"))
  total_models <- nrow(search_state$search_database) - 1  # Exclude base model

  # Save final checkpoint
  if (save_checkpoints) {
    final_checkpoint <- paste0(checkpoint_prefix, "_final_complete.rds")
    save_search_state(search_state, final_checkpoint)
    checkpoint_files <- c(checkpoint_files, final_checkpoint)
    cat(sprintf("💾 Final checkpoint saved: %s\n", final_checkpoint))
  }

  # Create final summary
  if (full_scm) {
    if (starting_phase == "forward") {
      workflow_desc <- "Full SCM (Forward → Backward)"
    } else {
      workflow_desc <- "Full SCM+ (Backward → Forward → Backward)"
    }
  } else {
    workflow_desc <- sprintf("%s %s only",
                             tools::toTitleCase(starting_phase),
                             if(starting_phase == "forward") "selection" else "elimination")
  }

  final_summary <- sprintf(
    "%s (%s) completed in %.1f minutes. Final model: %s with %d covariates (%s). %d total models created, %d excluded covariates.",
    workflow_desc,
    scm_type,
    total_time,
    final_model,
    length(final_covariates),
    if (length(final_covariates) > 0) paste(final_covariates, collapse = " + ") else "none",
    total_models,
    length(excluded_covariates)
  )

  # ===================================================================
  # FINAL CONSOLE SUMMARY
  # ===================================================================

  cat("\n", paste(rep("=", 80), collapse=""), "\n")
  cat("🎉 AUTOMATED SCM TESTING COMPLETE!\n")
  cat(paste(rep("=", 80), collapse=""), "\n")
  cat(sprintf("⏱️  Total time: %.1f minutes\n", total_time))
  cat(sprintf("🏗️  Total models created: %d\n", total_models))
  cat(sprintf("🎯 Final model: %s\n", final_model))

  if (length(final_covariates) > 0) {
    cat(sprintf("🧬 Final covariates: %s\n", paste(final_covariates, collapse = " + ")))
  } else {
    cat("🧬 Final covariates: None (base model retained)\n")
  }

  if (length(excluded_covariates) > 0) {
    cat(sprintf("🚫 Excluded covariates: %s\n", paste(excluded_covariates, collapse = ", ")))
  }

  cat(sprintf("💾 Checkpoints saved: %d files\n", length(checkpoint_files)))
  cat(paste(rep("=", 80), collapse=""), "\n")

  # FIXED: Return comprehensive results with consistent naming
  return(list(
    search_state = search_state,
    status = "completed",
    scm_type = scm_type,
    starting_phase = starting_phase,
    full_scm = full_scm,
    final_model = final_model,
    base_model = base_model_id,
    forward_results = forward_results,
    initial_backward_results = initial_backward_results,
    final_backward_results = final_backward_results,
    final_covariates = final_covariates,
    excluded_covariates = excluded_covariates,
    forward_ofv_threshold = forward_ofv_threshold,
    backward_ofv_threshold = backward_ofv_threshold,
    rse_threshold = rse_threshold,
    total_models_created = total_models,
    total_time_minutes = total_time,
    checkpoint_files = checkpoint_files,
    final_summary = final_summary
  ))
}
