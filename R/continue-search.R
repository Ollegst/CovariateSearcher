# =============================================================================
# CONTINUE SEARCH
# File: R/continue-search.R
# Part of CovariateSearcher Package
# Resume an interrupted SCM run (forward or backward) from the last full step.
# =============================================================================


#' Classify a phase string as backward elimination
#' @keywords internal
.scm_phase_is_backward <- function(phase) {
  !is.na(phase) & grepl("backward|removal", phase, ignore.case = TRUE)
}


#' Resume the backward-elimination phase from the last full step
#'
#' @description Re-evaluates the removal models already created in the last
#'   backward step (reconstructed from the database via \code{get_step_models}),
#'   picks the winner with \code{evaluate_removal_impacts}, then continues normal
#'   backward elimination from that winner. No models are re-created for the
#'   last step, so there is no duplication.
#' @keywords internal
.scm_continue_backward <- function(search_state, last_step,
                                   backward_p_value, rse_threshold,
                                   auto_submit, auto_retry) {
  step <- get_step_models(search_state, last_step,
                          p_value = backward_p_value, rse_threshold = rse_threshold)

  if (!isTRUE(step$exists) || is.na(step$base_model)) {
    cat("⚠️  Could not reconstruct the last backward step — nothing to continue.\n")
    return(list(search_state = search_state, final_model = search_state$base_model))
  }

  base_model <- step$base_model
  cat(sprintf("Backward base at step %d: %s\n", last_step, base_model))

  if (length(step$completed_models) == 0) {
    cat("⚠️  No completed removal models in the last backward step — elimination stops here.\n")
    return(list(search_state = search_state, final_model = base_model))
  }

  # Rebuild the covariate-name -> model-name map that evaluate_removal_impacts expects
  db <- search_state$search_database
  step_rows <- db[db$model_name %in% step$models, , drop = FALSE]
  removal_models <- list()
  for (i in seq_len(nrow(step_rows))) {
    tag <- step_rows$covariate_tested[i]
    cov_name <- if (!is.na(tag) && tag %in% names(search_state$tags)) {
      search_state$tags[[tag]]
    } else {
      tryCatch(extract_covariate_name_from_tag(tag), error = function(e) tag)
    }
    if (is.na(cov_name) || cov_name == "") cov_name <- tag
    removal_models[[cov_name]] <- step_rows$model_name[i]
  }

  evaluation <- evaluate_removal_impacts(
    search_state     = search_state,
    base_model       = base_model,
    removal_models   = removal_models,
    completed_models = step$completed_models,
    backward_p_value = backward_p_value,
    rse_threshold    = rse_threshold
  )
  search_state <- evaluation$search_state

  if (is.null(evaluation$covariate_to_remove)) {
    cat("🏁 No further removals meet the backward criteria — elimination complete.\n")
    return(list(search_state = search_state, final_model = base_model))
  }

  winner <- evaluation$new_base_model
  cat(sprintf("✂️  Removing %s → new base %s; continuing backward elimination...\n",
              evaluation$covariate_to_remove, winner))

  db_idx <- which(search_state$search_database$model_name == winner)
  if (length(db_idx) > 0) {
    search_state$search_database$step_description[db_idx] <-
      sprintf("Remove %s", evaluation$covariate_to_remove)
  }

  result <- run_backward_elimination(
    search_state     = search_state,
    starting_model   = winner,
    backward_p_value = backward_p_value,
    auto_submit      = auto_submit,
    auto_retry       = auto_retry,
    rse_threshold    = rse_threshold
  )

  list(search_state = result$search_state,
       final_model  = result$final_model %||% winner)
}


#' Continue an Interrupted SCM Search
#'
#' @title Resume a stepwise covariate search from the last completed step
#' @description Resumes an SCM run that stopped part-way (e.g. a model run failed
#'   and was rerun manually). It re-reads the last step's model outputs from disk,
#'   confirms that step is complete, determines the current best model, and
#'   continues the search using the same machinery as a fresh run:
#'   \itemize{
#'     \item \strong{Forward} — re-enters the forward method from the current best
#'       model. Selective forward reconstructs the previous step's significant
#'       covariates from the database (via \code{\link{get_step_models}}), so the
#'       selective narrowing is preserved. When \code{full_scm}, backward
#'       elimination follows.
#'     \item \strong{Backward} — re-evaluates the last step's removal models and
#'       continues elimination from the winner.
#'   }
#'   The forward method (\code{scm_type}) and \code{full_scm} are passed as
#'   arguments — you choose them when you launch a run, and the database does not
#'   record which forward method was used. Thresholds default to the state's
#'   \code{search_config}. The interrupted phase (forward vs backward) and the
#'   current best model are detected from the database.
#' @param search_state List. In-memory search state to resume (takes precedence).
#'   Typically the state you just refreshed with
#'   \code{\link{update_model_status_from_files}}.
#' @param checkpoint Character or NULL. Full path to a per-step checkpoint
#'   \code{.rds} to load when \code{search_state} is NULL (per-step files live in
#'   \code{<models_folder>/scm_rds/}, e.g.
#'   \code{models/scm_rds/scm_selective_step_3.rds},
#'   \code{models/scm_rds/backward_step_5.rds},
#'   \code{models/scm_rds/scm_forward_step_2.rds}).
#' @param scm_type Character or NULL. "standard" or "selective". Required for a
#'   forward resume (the database does not record which forward method was used);
#'   ignored for a backward resume.
#' @param full_scm Logical. Whether to continue into backward elimination after
#'   forward selection (default TRUE). Set FALSE if the original run was
#'   forward-only.
#' @param forward_p_value,backward_p_value,rse_threshold Numeric or NULL.
#'   Thresholds; default to the state's \code{search_config}.
#' @param auto_submit Logical. Whether to submit newly created models (default TRUE).
#' @param auto_retry Logical. Whether to auto-retry failed models (default TRUE).
#' @return Invisibly, a list with \code{search_state}, \code{status}
#'   ("completed" or "incomplete_step"), \code{resumed_phase}, \code{final_model}
#'   and \code{final_covariates}.
#' @export
continue_search <- function(search_state = NULL,
                            checkpoint = NULL,
                            scm_type = NULL,
                            full_scm = TRUE,
                            forward_p_value = NULL,
                            backward_p_value = NULL,
                            rse_threshold = NULL,
                            auto_submit = TRUE,
                            auto_retry = TRUE) {

  # ---- 1. Obtain the search state ----------------------------------------
  if (is.null(search_state)) {
    if (is.null(checkpoint)) {
      stop("continue_search(): provide either `search_state` or a `checkpoint` .rds path")
    }
    if (!file.exists(checkpoint)) {
      stop(sprintf("continue_search(): checkpoint file not found: %s\n  ",
                   checkpoint),
           "Pass a full path (e.g. models/scm_rds/scm_selective_step_3.rds) ",
           "or the in-memory search_state.")
    }
    search_state <- load_search_state(checkpoint)
  }
  if (is.null(search_state$search_database) || nrow(search_state$search_database) == 0) {
    stop("continue_search(): search_state has no models to resume from")
  }

  # ---- 2. Resolve thresholds (default to the state's search_config) --------
  forward_p_value  <- forward_p_value  %||% search_state$search_config$forward_p_value  %||% 0.05
  backward_p_value <- backward_p_value %||% search_state$search_config$backward_p_value %||% 0.001
  rse_threshold    <- rse_threshold    %||% search_state$search_config$max_rse_threshold %||% 50

  # Keep config coherent for the downstream methods
  search_state$search_config$forward_p_value   <- forward_p_value
  search_state$search_config$backward_p_value  <- backward_p_value
  search_state$search_config$max_rse_threshold <- rse_threshold

  cat("🔁 CONTINUE SEARCH\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")

  # ---- 3. Find the last recorded step and re-sync it from files -----------
  last_step <- suppressWarnings(max(search_state$search_database$step_number, na.rm = TRUE))
  if (!is.finite(last_step) || last_step < 1) {
    stop("continue_search(): no completed SCM steps found in the database ",
         "(only the base model?); run the search first before resuming.")
  }

  last_step_models <- search_state$search_database$model_name[
    !is.na(search_state$search_database$step_number) &
      search_state$search_database$step_number == last_step]

  cat(sprintf("Last recorded step: %d (%d models)\n", last_step, length(last_step_models)))
  cat("🔄 Re-reading last-step model outputs from files...\n")
  for (m in last_step_models) {
    search_state <- update_model_status_from_files(search_state, m, force = TRUE)
  }
  search_state <- update_all_model_statuses(search_state)

  # ---- 4. Require the last step to be full (nothing still running) --------
  ls_rows <- search_state$search_database[
    !is.na(search_state$search_database$step_number) &
      search_state$search_database$step_number == last_step, , drop = FALSE]

  blocking <- ls_rows$model_name[ls_rows$status %in% c("created", "in_progress", "submitted")]
  if (length(blocking) > 0) {
    cat(sprintf("\n⛔ Step %d is not complete — %d model(s) still pending:\n",
                last_step, length(blocking)))
    for (m in blocking) {
      st <- search_state$search_database$status[search_state$search_database$model_name == m][1]
      cat(sprintf("   - %s (%s)\n", m, st))
    }
    cat("Finish/rerun these models, then call continue_search() again.\n")
    return(invisible(list(
      search_state   = search_state,
      status         = "incomplete_step",
      step           = last_step,
      pending_models = blocking
    )))
  }

  # ---- 5. Classify the interrupted phase ----------------------------------
  in_backward <- any(.scm_phase_is_backward(ls_rows$phase))
  cat(sprintf("SCM type: %s | full_scm: %s\n", scm_type %||% "(unknown)", full_scm))
  cat(sprintf("Interrupted phase: %s\n",
              if (in_backward) "backward elimination" else "forward selection"))

  final_model <- NULL

  if (!in_backward) {
    # ======================= FORWARD RESUME ===============================
    if (is.null(scm_type)) {
      stop("continue_search(): scm_type is unknown from the checkpoint. ",
           "Pass scm_type = 'standard' or 'selective'.")
    }
    scm_type <- match.arg(scm_type, c("standard", "selective"))

    # Winner + significance for the last completed step come from the SAME
    # evaluator the live selective-forward loop uses (get_step_models →
    # get_significant_models_from_step), so RSE and per-df thresholds are applied
    # identically. If the step produced no significant model, forward has
    # converged and we keep the model the step built on.
    step_eval    <- get_step_models(search_state, last_step,
                                    p_value = forward_p_value, rse_threshold = rse_threshold)
    current_best <- step_eval$best_model %||% step_eval$base_model
    if (is.null(current_best) || is.na(current_best)) current_best <- search_state$base_model
    sig_last     <- step_eval$significant_models
    cat(sprintf("Current best forward model: %s\n", current_best))
    cat(sprintf("Significant models in step %d: %d\n", last_step, length(sig_last)))

    if (length(sig_last) > 0 && last_step >= 1) {
      cat("▶️  Resuming forward selection...\n")
      forward_result <- if (scm_type == "selective") {
        run_scm_selective_forward(
          search_state    = search_state,
          base_model_id   = current_best,
          forward_p_value = forward_p_value,
          rse_threshold   = rse_threshold,
          auto_submit     = auto_submit,
          auto_retry      = auto_retry,
          resume          = TRUE
        )
      } else {
        run_stepwise_covariate_modeling(
          search_state    = search_state,
          base_model_id   = current_best,
          auto_submit     = auto_submit,
          forward_p_value = forward_p_value,
          rse_threshold   = rse_threshold
        )
      }
      search_state <- forward_result$search_state
      current_best <- forward_result$final_best_model %||%
        forward_result$final_model %||% current_best
    } else {
      cat("✅ Forward selection already converged (no significant models in the last step).\n")
    }

    if (isTRUE(full_scm)) {
      cat("\n▶️  Proceeding to backward elimination...\n")
      backward_result <- run_backward_elimination(
        search_state     = search_state,
        starting_model   = current_best,
        backward_p_value = backward_p_value,
        auto_submit      = auto_submit,
        auto_retry       = auto_retry,
        rse_threshold    = rse_threshold
      )
      search_state <- backward_result$search_state
      final_model  <- backward_result$final_model %||% current_best
    } else {
      final_model <- current_best
    }

  } else {
    # ======================= BACKWARD RESUME ==============================
    cat("▶️  Resuming backward elimination...\n")
    backward_result <- .scm_continue_backward(
      search_state     = search_state,
      last_step        = last_step,
      backward_p_value = backward_p_value,
      rse_threshold    = rse_threshold,
      auto_submit      = auto_submit,
      auto_retry       = auto_retry
    )
    search_state <- backward_result$search_state
    final_model  <- backward_result$final_model
  }

  # ---- 6. Wrap up ---------------------------------------------------------
  # (The forward/backward methods invoked above already persist their own
  #  per-step and completion .rds files; nothing extra is saved here.)
  final_covariates <- tryCatch(
    get_model_covariates_from_db(search_state, final_model),
    error = function(e) character(0)
  )

  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("✅ CONTINUE SEARCH COMPLETE\n")
  cat(sprintf("🎯 Final model: %s\n", final_model))
  if (length(final_covariates) > 0) {
    cat(sprintf("🧬 Final covariates: %s\n", paste(final_covariates, collapse = " + ")))
  }
  cat(paste(rep("=", 60), collapse = ""), "\n")

  invisible(list(
    search_state     = search_state,
    status           = "completed",
    scm_type         = scm_type,
    resumed_phase    = if (in_backward) "backward" else "forward",
    final_model      = final_model,
    final_covariates = final_covariates
  ))
}
