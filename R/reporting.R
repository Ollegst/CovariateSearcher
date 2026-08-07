#' Generate SCM Report from Existing Database
#'
#' @title Create SCM report using existing search database
#' @description Simple function that reads your search_database and generates
#'   a formatted report showing all steps, similar to console output
#' @param search_state List containing search state with populated database
#' @param output_file Character. Path for report file (default: "scm_report.txt")
#' @param print_console Logical. Also print to console (default: TRUE)
#' @return Invisible NULL (writes to file)
#' @export
generate_scm_report <- function(search_state,
                                output_file = "scm_report.txt",
                                print_console = TRUE) {

  # Add the %||% operator if not available
  `%||%` <- function(x, y) if (is.null(x)) y else x

  db <- search_state$search_database
  if (is.null(db) || nrow(db) == 0) {
    stop("No models in database")
  }

  # Sort by step number and model name
  db <- db[order(db$step_number, db$model_name), ]

  # Initialize report
  lines <- character()

  # Header
  lines <- c(lines,
             paste(rep("=", 80), collapse = ""),
             "STEPWISE COVARIATE MODELING (SCM) REPORT",
             paste(rep("=", 80), collapse = ""),
             sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
             sprintf("Total Models: %d", nrow(db)),
             "",
             paste(rep("-", 80), collapse = ""),
             ""
  )

  # Get unique steps
  steps <- unique(db$step_number[!is.na(db$step_number) & db$step_number > 0])

  # The limit every RSE comparison below is made against, resolved once.
  rse_threshold <- .resolve_rse_threshold(search_state)

  # Backward/forward is decided by the same predicate the engine uses, keyed by
  # model name so it survives the reordering above.
  is_backward_by_model <- stats::setNames(
    .scm_backward_rows(search_state),
    search_state$search_database$model_name
  )

  # The model the steps have arrived at so far. It advances only when a step
  # produces an accepted winner, so a step where nothing qualifies holds it.
  current_model <- NA_character_

  # Process each step
  for (step in sort(steps)) {
    step_data <- db[db$step_number == step & !is.na(db$step_number), ]

    if (nrow(step_data) == 0) next

    # Step header
    lines <- c(lines,
               "",
               paste(rep("=", 70), collapse = ""),
               sprintf("STEP %d", step),
               paste(rep("=", 70), collapse = "")
    )

    # Determine step type and add description
    is_backward <- any(is_backward_by_model[step_data$model_name], na.rm = TRUE)

    is_redemption <- any(grepl("redemption|final_test|retry", step_data$phase, ignore.case = TRUE))

    # Add step type description
    if (is_redemption) {
      step_type_desc <- "REDEMPTION/FINAL TESTING"
    } else if (is_backward) {
      step_type_desc <- "BACKWARD ELIMINATION"
    } else {
      step_type_desc <- "FORWARD SELECTION"
    }

    lines <- c(lines, step_type_desc)

    # Get parent/base model for this step
    base_model <- unique(step_data$parent_model)[1]
    # Reset per step. Assigned only when this step names a parent, but read
    # further down regardless - without the reset a step whose rows carry no
    # parent_model would report the PREVIOUS step's base OFV as its own, or fail
    # outright on the first step.
    base_ofv <- NA_real_
    # The first step's parent is where the search started, and stays the answer
    # until some step produces an accepted winner.
    if (is.na(current_model) && !is.na(base_model)) current_model <- base_model
    if (!is.na(base_model)) {
      # Get OFV from database
      base_row <- db[db$model_name == base_model, ]
      if (nrow(base_row) > 0) {
        base_ofv <- base_row$ofv[1]
      } else {
        base_ofv <- NA
      }
      if (!is.na(base_ofv)) {
        lines <- c(lines,
                   sprintf("\n📊 Base model: %s (OFV: %.2f)", base_model, base_ofv)
        )
      }
    }

    # Count completed vs failed
    n_completed <- sum(step_data$status == "completed")
    n_failed <- sum(step_data$status == "failed")

    lines <- c(lines,
               sprintf("\n Step %d Results:", step),
               sprintf("  Completed: %d models", n_completed),
               sprintf("  Failed: %d models", n_failed)
    )

    # Get completed models only for evaluation
    completed <- step_data[step_data$status == "completed", ]

    if (nrow(completed) > 0) {
      # One evaluator for the whole package: per-covariate degrees of freedom,
      # the phase's own p-value and comparison direction, and the RSE limit. The
      # report states the decision the search made rather than re-deriving a
      # different one from the same numbers.
      ev <- .evaluate_model_criteria(
        search_state  = search_state,
        model_names   = completed$model_name,
        phase         = if (is_backward) "backward" else "forward",
        rse_threshold = rse_threshold
      )
      m <- match(completed$model_name, ev$model_name)
      completed$meets_ofv       <- ev$meets_ofv[m]
      completed$meets_rse       <- ev$meets_rse[m]
      completed$meets_threshold <- ev$meets_threshold[m]
      completed$ofv_threshold   <- ev$ofv_threshold[m]
      completed$covariate_df    <- ev$covariate_df[m]

      # Why a model was not accepted, in the caller's vocabulary.
      reason_for <- function(row) {
        if (isTRUE(row$meets_threshold)) {
          sprintf("threshold %.2f (df=%d)", row$ofv_threshold, row$covariate_df)
        } else if (!isTRUE(row$meets_ofv)) {
          sprintf("ΔOFV vs threshold %.2f (df=%d)", row$ofv_threshold, row$covariate_df)
        } else {
          sprintf("RSE %.1f%% > %g%%", row$rse_max, rse_threshold)
        }
      }

      if (is_backward) {
        # Backward elimination format
        lines <- c(lines,
                   "\n📈 Evaluating removal impacts...",
                   if (!is.na(base_ofv)) {
                     sprintf("\n📊 Base model %s OFV: %.2f", base_model, base_ofv)
                   } else {
                     "\n📊 Base model OFV not available"
                   },
                   "📈 Removal impacts:"
        )

        # Sort by delta OFV (smallest first - the least costly removal)
        completed <- completed[order(completed$delta_ofv), ]

        for (i in seq_len(nrow(completed))) {
          row <- completed[i, ]
          lines <- c(lines,
                     sprintf("  %s %s (%s) removed → OFV: %.2f (ΔOFV: %+.2f) %s",
                             if (isTRUE(row$meets_threshold)) "✅" else "❌",
                             row$covariate_tested,
                             row$model_name,
                             row$ofv,
                             row$delta_ofv,
                             reason_for(row))
          )
        }

        # The removal actually accepted: cheapest among those meeting BOTH
        # criteria, not merely the cheapest.
        acceptable <- completed[which(completed$meets_threshold), , drop = FALSE]
        if (nrow(acceptable) > 0) {
          winner <- acceptable[which.min(acceptable$delta_ofv), ]
          current_model <- winner$model_name
          lines <- c(lines,
                     sprintf("\n✂️ REMOVING: %s", winner$covariate_tested),
                     sprintf("📊 ΔOFV: %+.2f (below threshold of %.2f for df=%d)",
                             winner$delta_ofv, winner$ofv_threshold, winner$covariate_df),
                     sprintf("🎯 New base model: %s", winner$model_name)
          )
        } else {
          blocked <- sum(completed$meets_ofv & !completed$meets_rse, na.rm = TRUE)
          lines <- c(lines,
                     "\n⚠️ No removals meet the criteria",
                     if (blocked > 0) {
                       sprintf("   %d removal(s) were cheap enough but blocked by RSE > %g%%",
                               blocked, rse_threshold)
                     } else {
                       "   Every removal would worsen OFV by more than its threshold"
                     },
                     "\n🏁 No covariate can be removed"
          )
        }

      } else {
        # Forward selection format
        lines <- c(lines,
                   "\n📈 Forward selection results:"
        )

        # Sort by delta OFV (largest first)
        completed <- completed[order(completed$delta_ofv, decreasing = TRUE), ]

        for (i in seq_len(nrow(completed))) {
          row <- completed[i, ]
          lines <- c(lines,
                     sprintf("  %s %s (%s) added → OFV: %.2f (ΔOFV: %.2f) %s",
                             if (isTRUE(row$meets_threshold)) "✅" else "❌",
                             row$covariate_tested,
                             row$model_name,
                             row$ofv,
                             row$delta_ofv,
                             reason_for(row))
          )
        }

        # Winner: largest ΔOFV among those meeting BOTH criteria.
        acceptable <- completed[which(completed$meets_threshold), , drop = FALSE]
        if (nrow(acceptable) > 0) {
          winner <- acceptable[which.max(acceptable$delta_ofv), ]
          current_model <- winner$model_name
          lines <- c(lines,
                     sprintf("\n🏆 Winner: %s (%s) with ΔOFV: %.2f (threshold %.2f for df=%d)",
                             winner$model_name,
                             winner$covariate_tested,
                             winner$delta_ofv,
                             winner$ofv_threshold,
                             winner$covariate_df)
          )
        } else {
          blocked <- sum(completed$meets_ofv & !completed$meets_rse, na.rm = TRUE)
          lines <- c(lines,
                     "\n⚠️ No model meets the criteria",
                     if (blocked > 0) {
                       sprintf("   %d model(s) had sufficient ΔOFV but were blocked by RSE > %g%%",
                               blocked, rse_threshold)
                     } else {
                       "   No ΔOFV reached its threshold"
                     }
          )
        }
      }
    }
  }

  # Final summary
  lines <- c(lines,
             "",
             paste(rep("=", 80), collapse = ""),
             "FINAL SUMMARY",
             paste(rep("=", 80), collapse = "")
  )

  # The final model is the one the steps arrived at - seeded from the first
  # step's base and advanced only by an accepted winner. Naming the highest step
  # number instead reports whatever ran last, accepted or not, which is how a
  # model the search rejected can be presented as the result.
  if (is.na(current_model)) current_model <- search_state$base_model %||% NA_character_

  if (!is.na(current_model)) {
    final_row <- db[db$model_name == current_model, , drop = FALSE]
    final_ofv <- if (nrow(final_row) > 0) final_row$ofv[1] else NA_real_

    lines <- c(lines,
               sprintf("Final Model: %s", current_model),
               if (!is.na(final_ofv)) {
                 sprintf("Final OFV: %.2f", final_ofv)
               } else {
                 "Final OFV: not available"
               }
    )
  } else {
    lines <- c(lines, "Final Model: not determined (no step produced a winner)")
  }

  # The counts describe the database itself, so they are reported whether or not
  # a final model could be named.
  lines <- c(lines,
             sprintf("Total Models Created: %d", nrow(db)),
             sprintf("Completed Models: %d", sum(db$status == "completed", na.rm = TRUE)),
             sprintf("Failed Models: %d", sum(db$status == "failed", na.rm = TRUE))
  )

  # Get excluded covariates if available
  if ("excluded_covariates" %in% names(search_state)) {
    excluded <- search_state$excluded_covariates
    if (length(excluded) > 0) {
      lines <- c(lines,
                 sprintf("Excluded Covariates: %s", paste(excluded, collapse = ", "))
      )
    }
  }

  lines <- c(lines,
             "",
             paste(rep("=", 80), collapse = ""),
             "End of Report"
  )

  # Write to file
  writeLines(lines, output_file)

  # Print to console if requested
  if (print_console) {
    cat(paste(lines, collapse = "\n"), "\n")
  }

  cat(sprintf("\n📄 Report saved to: %s\n", output_file))

  invisible(NULL)
}
