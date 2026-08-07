# =============================================================================
# SCM EVALUATION
# File: R/scm-evaluation.R
# Part of CovariateSearcher Package
# Statistical model evaluation and selection
# =============================================================================


#' Does a model meet the search's acceptance criteria?
#'
#' @description The single answer to "may the search use this model", for every
#'   phase and every caller. A model is acceptable when it completed, its ΔOFV
#'   clears the chi-square threshold for its own covariate's degrees of freedom,
#'   and its worst parameter RSE is within the limit.
#'
#'   Forward and backward are not the same test with a different p-value. The two
#'   phases store \code{delta_ofv} with opposite sign conventions
#'   (forward \code{parent - model}, positive = better; backward
#'   \code{model - base}, positive = worse) AND compare it in opposite directions,
#'   so \code{phase} selects both. The RSE test is identical in both, though what
#'   passing it authorises differs: forward it accepts the covariate, backward it
#'   accepts the covariate's REMOVAL.
#'
#' @param search_state List containing covariate search state and configuration.
#' @param model_names Character vector of model names to judge. Names with no row
#'   in the database are dropped from the result.
#' @param phase Character. \code{"forward"} or \code{"backward"}.
#' @param p_value Numeric or NULL. Defaults to the phase's configured p-value.
#' @param rse_threshold Numeric or NULL. Resolved via
#'   \code{.resolve_rse_threshold()}.
#' @return A \code{data.frame}, one row per model found, in the order given:
#'   \code{model_name}, \code{status}, \code{ofv}, \code{delta_ofv},
#'   \code{rse_max}, \code{covariate_df}, \code{ofv_threshold}, \code{meets_ofv},
#'   \code{meets_rse}, \code{meets_threshold}, \code{note}. The column names are
#'   the ones \code{evaluate_removal_impacts()} reports, so its result frame is
#'   assembled straight from these.
#' @keywords internal
#' @noRd
.evaluate_model_criteria <- function(search_state,
                                     model_names,
                                     phase = c("forward", "backward"),
                                     p_value = NULL,
                                     rse_threshold = NULL) {
  phase <- match.arg(phase)

  if (is.null(p_value)) {
    p_value <- if (phase == "forward") {
      search_state$search_config$forward_p_value %||% 0.05
    } else {
      search_state$search_config$backward_p_value %||% 0.001
    }
  }
  rse_threshold <- .resolve_rse_threshold(search_state, rse_threshold)

  empty <- data.frame(
    model_name = character(), status = character(), ofv = numeric(),
    delta_ofv = numeric(), rse_max = numeric(), covariate_df = integer(),
    ofv_threshold = numeric(), meets_ofv = logical(), meets_rse = logical(),
    meets_threshold = logical(), note = character(),
    stringsAsFactors = FALSE
  )

  db <- search_state$search_database
  if (is.null(db) || !is.data.frame(db) || nrow(db) == 0 ||
      length(model_names) == 0) {
    return(empty)
  }

  idx <- match(model_names, db$model_name)
  if (!any(!is.na(idx))) return(empty)
  rows <- db[idx[!is.na(idx)], , drop = FALSE]

  # A database that predates a column reads as all-NA rather than erroring, so a
  # checkpoint saved before rse_max existed still evaluates (on OFV alone).
  col <- function(nm, default) if (nm %in% names(rows)) rows[[nm]] else default
  status    <- as.character(col("status", NA_character_))
  ofv       <- as.numeric(col("ofv", NA_real_))
  delta_ofv <- as.numeric(col("delta_ofv", NA_real_))
  rse_max   <- as.numeric(col("rse_max", NA_real_))
  cov_tag   <- as.character(col("covariate_tested", NA_character_))

  # Degrees of freedom per covariate: a 3-level categorical is a df=2 test, not
  # df=1, and a fully-FIX covariate is df=0 (threshold 0 -> direct OFV comparison).
  covariate_df <- vapply(cov_tag, function(tg) {
    if (is.na(tg) || !nzchar(tg)) return(1L)
    cov_name <- tryCatch(extract_covariate_name_from_tag(tg),
                         error = function(e) NA_character_)
    if (is.na(cov_name)) return(1L)
    tryCatch(
      as.integer(calculate_covariate_df(cov_name, search_state$covariate_search)),
      error = function(e) 1L
    )
  }, integer(1), USE.NAMES = FALSE)
  covariate_df[is.na(covariate_df)] <- 1L

  ofv_threshold <- vapply(covariate_df,
                          function(d) pvalue_to_threshold(p_value, df = d),
                          numeric(1))

  # The phase fork: forward keeps a covariate when it BUYS enough OFV; backward
  # accepts a removal when it COSTS little enough.
  meets_ofv <- if (phase == "forward") {
    !is.na(delta_ofv) & delta_ofv > ofv_threshold
  } else {
    !is.na(delta_ofv) & delta_ofv < ofv_threshold
  }
  # An unreadable RSE means precision was not demonstrated. Whether that
  # disqualifies the model follows require_cov_step, because the two settings
  # are the same question asked twice: a search that DEMANDS a covariance step
  # cannot accept a model whose precision it could not read, while one that does
  # not demand it has no standard errors to judge and must not reject a model
  # for the absence of something it never asked for.
  # Only an explicit FALSE relaxes the rule. NA, a string, a vector - anything
  # else is a configuration fault, and a fault must not quietly switch a
  # criterion off. initialize_search_config() writes whatever it is given, so an
  # isTRUE() test here would send every malformed value down the lenient branch.
  require_cov <- search_state$search_config$require_cov_step %||% TRUE
  meets_rse <- if (identical(require_cov, FALSE)) {
    is.na(rse_max) | rse_max < rse_threshold
  } else {
    !is.na(rse_max) & rse_max < rse_threshold
  }

  completed <- !is.na(status) & status == "completed"
  meets_threshold <- completed & meets_ofv & meets_rse

  # These three are the function's contract and every consumer indexes on them.
  # `x[NA]` returns NA rather than dropping the element, so a single NA here
  # becomes an NA *model name* downstream - a model that does not exist. An
  # unusable threshold is the way that happens, and it is a configuration fault,
  # so fail closed rather than admit a row nothing could judge.
  meets_ofv[is.na(meets_ofv)] <- FALSE
  meets_rse[is.na(meets_rse)] <- FALSE
  meets_threshold[is.na(meets_threshold)] <- FALSE

  need <- if (phase == "forward") ">" else "<"
  # An RSE that could not be read is a different fact from one that is too high,
  # and reporting it as "NA% > 50%" hides which of the two happened.
  rse_reason <- ifelse(
    is.na(rse_max),
    "RSE unavailable (a successful covariance step is required)",
    sprintf("RSE too high (%.1f%%, limit %g%%)", rse_max, rse_threshold)
  )
  note <- ifelse(
    !completed,
    paste("Model", ifelse(is.na(status), "status unknown", status)),
    ifelse(
      is.na(delta_ofv),
      "Delta OFV not available",
      ifelse(
        !meets_ofv & !meets_rse,
        sprintf("Insufficient OFV (%.2f, need %s%.2f for df=%d) and %s",
                delta_ofv, need, ofv_threshold, covariate_df, rse_reason),
        ifelse(
          !meets_ofv,
          sprintf("Insufficient OFV (%.2f, need %s%.2f for df=%d)",
                  delta_ofv, need, ofv_threshold, covariate_df),
          ifelse(
            !meets_rse,
            rse_reason,
            sprintf("Meets all criteria (OFV=%.2f%s%.2f, RSE=%s)",
                    delta_ofv, need, ofv_threshold,
                    ifelse(is.na(rse_max), "not required", sprintf("%.1f%%", rse_max)))
          )
        )
      )
    )
  )

  data.frame(
    model_name = as.character(rows$model_name),
    status = status,
    ofv = ofv,
    delta_ofv = delta_ofv,
    rse_max = rse_max,
    covariate_df = covariate_df,
    ofv_threshold = ofv_threshold,
    meets_ofv = meets_ofv,
    meets_rse = meets_rse,
    meets_threshold = meets_threshold,
    note = note,
    stringsAsFactors = FALSE
  )
}


#' Pick the acceptable model with the lowest OFV
#'
#' @description Shared by every "which model should the search carry forward"
#'   decision that is not a single step's winner - the redemption re-base, the
#'   post-redemption overall pick, and the fallbacks either side of them.
#'   Ranking on fit alone would let a model failing the search's own criteria
#'   become the base for backward elimination purely by having the best OFV, so
#'   the candidates are filtered before the minimum is taken.
#'
#'   Only models that were tested as a covariate step are candidates: the base
#'   model carries no \code{delta_ofv} of its own and is therefore never
#'   returned. It is the incumbent, which is a different thing - callers that
#'   need a fallback when nothing qualifies name it themselves.
#'
#' @param search_state List containing covariate search state and configuration.
#' @param model_names Character vector of candidate model names.
#' @param p_value,rse_threshold Passed to \code{.evaluate_model_criteria()}.
#' @return A list: \code{model} (name, or NULL when nothing qualifies) and
#'   \code{rejected} (a data.frame of the candidates that had the OFV to win but
#'   failed on RSE, for logging).
#' @keywords internal
#' @noRd
.best_acceptable_model <- function(search_state, model_names,
                                   p_value = NULL, rse_threshold = NULL) {
  ev <- .evaluate_model_criteria(search_state, model_names, phase = "forward",
                                 p_value = p_value, rse_threshold = rse_threshold)
  if (nrow(ev) == 0) return(list(model = NULL, rejected = ev))

  ok <- ev[ev$meets_threshold & !is.na(ev$ofv), , drop = FALSE]
  # Reported so a model dropped purely for imprecision is visible in the log
  # rather than silently absent from the shortlist. Restricted to completed
  # models, so a model excluded for some other reason is not announced as an
  # RSE rejection.
  rejected <- ev[ev$status %in% "completed" & ev$meets_ofv & !ev$meets_rse &
                   !is.na(ev$ofv), , drop = FALSE]

  list(
    model = if (nrow(ok) > 0) ok$model_name[which.min(ok$ofv)] else NULL,
    rejected = rejected
  )
}


#' Announce models that had the fit to win but failed the RSE limit
#'
#' @description A selection that passes over the best-fitting model reads as
#'   arbitrary unless the reason is stated, so models dropped purely for
#'   imprecision are named where the choice is made rather than only in a later
#'   report.
#' @param rejected The \code{rejected} element of \code{.best_acceptable_model()}.
#' @param context Character. Where the rejection happened, for the message.
#' @return Invisibly \code{NULL}; called for its console output.
#' @keywords internal
#' @noRd
.report_rse_rejections <- function(rejected, context) {
  if (is.null(rejected) || nrow(rejected) == 0) return(invisible(NULL))
  for (i in seq_len(nrow(rejected))) {
    cat(sprintf("   ⚠️  %s excluded from %s: RSE %.1f%% exceeds the limit (OFV %.2f would otherwise qualify)\n",
                rejected$model_name[i], context, rejected$rse_max[i], rejected$ofv[i]))
  }
  invisible(NULL)
}


#' Select Best Model from Statistical Evaluation
#'
#' @title Evaluate models and select the best one based on statistical criteria
#' @description Evaluates completed models using delta OFV and RSE thresholds
#'   to identify the best performing model. ΔOFV threshold is calculated based
#'   on p-value and covariate degrees of freedom (df=1 for continuous,
#'   df=n_levels-1 for categorical).
#' @param search_state List containing covariate search state and configuration
#' @param model_names Character vector. Model names to evaluate
#' @param p_value Numeric. P-value for forward selection (uses config if NULL)
#' @param rse_threshold Numeric. Maximum RSE threshold (uses config if NULL)
#' @return List with best model, evaluation details, and updated search_state
#' @export
select_best_model <- function(search_state, model_names, p_value = NULL, rse_threshold = NULL) {
  # Use config defaults if not specified
  if (is.null(p_value)) {
    p_value <- search_state$search_config$forward_p_value %||% 0.05
  }
  rse_threshold <- .resolve_rse_threshold(search_state, rse_threshold)

  # Calculate display threshold for df=1 (most common case)
  ofv_threshold_display <- pvalue_to_threshold(p_value, df = 1)

  cat(sprintf("\n📊 EVALUATING MODELS (Forward OFV threshold: %.2f for df=1, RSE < %g%%)\n",
              ofv_threshold_display, rse_threshold))

  # Update all model information first
  search_state <- update_all_model_statuses(search_state)

  # Get completed models
  model_data <- search_state$search_database[
    search_state$search_database$model_name %in% model_names &
      search_state$search_database$status == "completed", ]

  if (nrow(model_data) == 0) {
    cat("❌ No completed models to evaluate\n")
    return(list(
      search_state = search_state,
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
      parent_ofv <- search_state$search_database$ofv[search_state$search_database$model_name == parent_name]

      # This function fixes the acceptance test's direction (phase = "forward"
      # below), so it must fix the sign the same way. Deriving it instead would
      # store a removal row as model - parent and then test that same row with
      # delta_ofv > threshold, so a removal that cost 70 OFV points would read as
      # a 70-point improvement and be selected.
      delta_ofv <- .signed_delta_ofv(search_state, model_data$model_name[i],
                                     model_data$ofv[i], parent_ofv,
                                     direction = "forward")

      # A removal reaching the forward evaluator means the caller assembled
      # `model_names` wrongly. Say so rather than silently recording a forward
      # sign on a row that every backward reader will take the other way.
      if (.is_removal_model(search_state, model_data$model_name[i])) {
        warning("select_best_model(): ", model_data$model_name[i],
                " is a removal model; its ΔOFV is recorded with the forward ",
                "sign but will be read as backward elsewhere.", call. = FALSE)
      }

      if (!is.na(delta_ofv)) {
        model_data$delta_ofv[i] <- delta_ofv

        # Update in main database
        db_idx <- which(search_state$search_database$model_name == model_data$model_name[i])
        search_state$search_database$delta_ofv[db_idx] <- delta_ofv
      }
    }
  }

  # Acceptance is decided in one place for every phase and caller. The delta-OFV
  # backfill above has already written to the database, so the evaluator reads
  # the same numbers this function would have used.
  ev <- .evaluate_model_criteria(
    search_state  = search_state,
    model_names   = model_data$model_name,
    phase         = "forward",
    p_value       = p_value,
    rse_threshold = rse_threshold
  )
  m <- match(model_data$model_name, ev$model_name)
  model_data$covariate_df          <- ev$covariate_df[m]
  model_data$ofv_threshold         <- ev$ofv_threshold[m]
  model_data$delta_ofv_significant <- ev$meets_ofv[m]
  model_data$rse_acceptable        <- ev$meets_rse[m]
  model_data$overall_significant   <- ev$meets_threshold[m]
  model_data$evaluation_notes      <- ev$note[m]

  evaluation_results <- model_data %>%
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
  for (i in seq_len(nrow(evaluation_results))) {
    row <- evaluation_results[i, ]
    status_icon <- if (isTRUE(row$overall_significant)) "✅" else "❌"
    best_icon <- if (length(row$model_name) > 0 && !is.na(row$model_name) &&
                     !is.null(best_model) && !is.na(best_model)) {
      if (row$model_name == best_model) " 🏆" else ""
    } else {
      ""
    }

    # Show threshold used for this model. An unknown RSE prints as "NA", never as
    # 0% - a model whose RSE could not be read must not read as a perfect one.
    cat(sprintf("  %s %s: OFV=%.2f (threshold=%.2f for df=%d), RSE=%s - %s%s\n",
                status_icon,
                row$model_name,
                ifelse(is.na(row$delta_ofv), 0, row$delta_ofv),
                row$ofv_threshold,
                row$covariate_df,
                ifelse(is.na(row$rse_max), "NA", sprintf("%.1f%%", row$rse_max)),
                row$evaluation_notes,
                best_icon))
  }

  cat(sprintf("\n🎯 Summary: %d significant models found\n", length(significant_models)))
  if (!is.null(best_model)) {
    best_delta <- evaluation_results$delta_ofv[evaluation_results$model_name == best_model]
    cat(sprintf("🏆 Best model selected: %s (OFV = %.2f)\n", best_model, best_delta))
  } else {
    cat("❌ No significant improvement found - keeping current base model\n")
  }

  return(list(
    search_state = search_state,
    best_model = best_model,
    significant_models = significant_models,
    evaluation_results = evaluation_results,
    criteria_used = list(
      forward_p_value = p_value,
      rse_threshold = rse_threshold
    ),
    status = if (!is.null(best_model)) "best_model_found" else "no_improvement"
  ))
}

