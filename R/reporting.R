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
    is_backward <- any(grepl("remove", step_data$action, ignore.case = TRUE)) ||
      any(grepl("backward", step_data$phase, ignore.case = TRUE))

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
      # Get thresholds from search_state config
      forward_threshold <- search_state$search_config$forward_ofv_threshold %||% 3.84
      backward_threshold <- search_state$search_config$backward_ofv_threshold %||% 10.83

      if (is_backward) {
        # Backward elimination format
        lines <- c(lines,
                   "\n📈 Evaluating removal impacts...",
                   sprintf("\n📊 Base model %s OFV: %.2f", base_model, base_ofv),
                   "📈 Removal impacts:"
        )

        # Sort by delta OFV (smallest first)
        completed <- completed[order(completed$delta_ofv), ]
        threshold <- backward_threshold

        for (i in 1:nrow(completed)) {
          row <- completed[i, ]
          symbol <- if (!is.na(row$delta_ofv) && row$delta_ofv <= threshold) "✅" else "❌"
          status <- if (!is.na(row$delta_ofv) && row$delta_ofv <= threshold) "< threshold" else "> threshold"

          lines <- c(lines,
                     sprintf("  %s %s (%s) removed → OFV: %.2f (ΔOFV: +%.2f) %s",
                             symbol,
                             row$covariate_tested,
                             row$model_name,
                             row$ofv,
                             abs(row$delta_ofv),
                             status)
          )
        }

        # Check if any removal was accepted
        if (any(completed$delta_ofv <= threshold, na.rm = TRUE)) {
          winner <- completed[which.min(completed$delta_ofv), ]
          lines <- c(lines,
                     sprintf("\n✂️ REMOVING: %s", winner$covariate_tested),
                     sprintf("📊 ΔOFV: +%.2f (below threshold of %.2f)",
                             abs(winner$delta_ofv), threshold),
                     sprintf("🎯 New base model: %s", winner$model_name)
          )
        } else {
          lines <- c(lines,
                     sprintf("\n⚠️ No removals meet threshold (all ΔOFV > %.2f)", threshold),
                     "\n🏁 No covariate can be removed without exceeding threshold",
                     sprintf("All removal attempts had ΔOFV > %.2f", threshold)
          )
        }

      } else {
        # Forward selection format
        lines <- c(lines,
                   "\n📈 Forward selection results:"
        )

        # Sort by delta OFV (largest first)
        completed <- completed[order(completed$delta_ofv, decreasing = TRUE), ]
        threshold <- forward_threshold

        for (i in 1:nrow(completed)) {
          row <- completed[i, ]
          symbol <- if (!is.na(row$delta_ofv) && row$delta_ofv > threshold) "✅" else "❌"

          lines <- c(lines,
                     sprintf("  %s %s (%s) added → OFV: %.2f (ΔOFV: %.2f)",
                             symbol,
                             row$covariate_tested,
                             row$model_name,
                             row$ofv,
                             row$delta_ofv)
          )
        }

        # Winner
        if (any(completed$delta_ofv > threshold, na.rm = TRUE)) {
          winner <- completed[which.max(completed$delta_ofv), ]
          lines <- c(lines,
                     sprintf("\n🏆 Winner: %s (%s) with ΔOFV: %.2f",
                             winner$model_name,
                             winner$covariate_tested,
                             winner$delta_ofv)
          )
        } else {
          lines <- c(lines,
                     sprintf("\n⚠️ No significant improvements found (all ΔOFV < %.2f)", threshold)
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

  # Find final model
  completed_models <- db[db$status == "completed", ]
  if (nrow(completed_models) > 0) {
    final_model <- completed_models[which.max(completed_models$step_number), ]

    lines <- c(lines,
               sprintf("Final Model: %s", final_model$model_name),
               sprintf("Final OFV: %.2f", final_model$ofv),
               sprintf("Total Models Created: %d", nrow(db)),
               sprintf("Completed Models: %d", sum(db$status == "completed")),
               sprintf("Failed Models: %d", sum(db$status == "failed"))
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
