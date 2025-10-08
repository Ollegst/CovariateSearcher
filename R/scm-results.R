#' Create Enhanced SCM Results Table with RSE Evaluation Comments
#'
#' @title Generate comprehensive SCM results table with evaluation comments
#' @description Creates a detailed table showing phase/step, model name, all covariates
#'   currently in the model, OFV, delta OFV, RSE, selection status, and comments
#'   explaining why models were or weren't selected
#' @param search_state List containing search state with database and tags
#' @return Data frame with comprehensive SCM results including evaluation comments
#' @export
create_scm_results_table <- function(search_state) {

  db <- search_state$search_database

  # Get thresholds from search_state config
  forward_threshold <- search_state$search_config$forward_ofv_threshold %||% 3.84
  backward_threshold <- search_state$search_config$backward_ofv_threshold %||% 6.63
  rse_threshold <- search_state$search_config$max_rse_threshold %||% 50

  cat(sprintf("Using thresholds - Forward: %.2f, Backward: %.2f, RSE: %d%%\n",
              forward_threshold, backward_threshold, rse_threshold))

  # Initialize results data frame
  results <- data.frame(
    Phase_Step = character(),
    Model = character(),
    Parent_Model = character(),
    Description = character(),
    Status = character(),
    Covariates_in_Model = character(),
    OFV = numeric(),
    Delta_OFV = numeric(),
    RSE_Max = numeric(),
    Selected = character(),
    Comment = character(),
    stringsAsFactors = FALSE
  )

  # Helper function to get covariates for a model from YAML tags
  get_model_covariates_display <- function(model_name) {
    yaml_file <- file.path(search_state$models_folder, paste0(model_name, ".yaml"))
    if (!file.exists(yaml_file)) {
      return("Unknown")
    }

    tryCatch({
      yaml_data <- yaml::read_yaml(yaml_file)

      if (length(yaml_data$tags) == 0) {
        return("Base")
      }

      # Convert tags to display format
      covariates_display <- character()

      for (tag in yaml_data$tags) {
        # Find matching covariate in search definition
        matching_rows <- search_state$covariate_search[
          grepl(paste0("_", tag, "$"), search_state$covariate_search$cov_to_test),
        ]

        if (nrow(matching_rows) > 0) {
          cov_name <- matching_rows$COVARIATE[1]
          param_name <- matching_rows$PARAMETER[1]
          covariates_display <- c(covariates_display, paste0(cov_name, "_on_", param_name))
        }
      }

      if (length(covariates_display) == 0) {
        return("Base")
      } else {
        return(paste(covariates_display, collapse = " + "))
      }
    }, error = function(e) {
      return("Error reading tags")
    })
  }

  # Helper function to determine selection status and comment
  get_selection_info <- function(row, step_models, threshold_ofv, threshold_rse, phase) {
    delta_ofv <- row$delta_ofv
    rse_max <- row$rse_max

    # Check if this is base model
    if (row$action == "base_model") {
      return(list(selected = "-", comment = "Base model"))
    }

    # Check completion status
    if (row$status != "completed") {
      return(list(selected = "NO", comment = paste("Model", row$status)))
    }

    # For backward elimination
    if (grepl("backward|remove", phase, ignore.case = TRUE)) {
      if (is.na(delta_ofv)) {
        return(list(selected = "NO", comment = "Delta OFV not available"))
      }
      # Backward: negative delta_ofv means worsening when removing
      # We keep the covariate if removal causes large negative delta_ofv
      if (abs(delta_ofv) > threshold_ofv) {
        return(list(selected = "KEPT", comment = sprintf("Removal would worsen OFV by %.2f", abs(delta_ofv))))
      } else {
        return(list(selected = "REMOVED", comment = sprintf("Removal acceptable (ΔOFV=%.2f < %.2f)", abs(delta_ofv), threshold_ofv)))
      }
    }

    # For forward selection - evaluate based on both OFV and RSE
    if (is.na(delta_ofv)) {
      return(list(selected = "NO", comment = "Delta OFV not available"))
    }

    # Check OFV improvement
    ofv_good <- delta_ofv > threshold_ofv

    # Check RSE
    rse_good <- is.na(rse_max) || rse_max < threshold_rse

    # Generate appropriate comment
    if (!ofv_good && !rse_good) {
      comment <- sprintf("OFV (%.2f ≤ %.2f) and high RSE (%.1f%% > %d%%)",
                         delta_ofv, threshold_ofv, rse_max, threshold_rse)
      return(list(selected = "NO", comment = comment))
    } else if (!ofv_good) {
      comment <- sprintf("Insufficient OFV improvement (%.2f ≤ %.2f)",
                         delta_ofv, threshold_ofv)
      return(list(selected = "NO", comment = comment))
    } else if (!rse_good) {
      comment <- sprintf("RSE too high (%.1f%% > %d%%)",
                         rse_max, threshold_rse)
      return(list(selected = "NO", comment = comment))
    } else {
      # Both criteria met - check if this is the best in the step
      step_best <- step_models[step_models$delta_ofv > threshold_ofv &
                                 (is.na(step_models$rse_max) | step_models$rse_max < threshold_rse), ]

      if (nrow(step_best) > 0) {
        best_ofv <- max(step_best$delta_ofv, na.rm = TRUE)
        if (abs(delta_ofv - best_ofv) < 0.01) {  # Account for rounding
          return(list(selected = "BEST",
                      comment = sprintf("Best model (ΔOFV=%.2f, RSE=%.1f%%)", delta_ofv, rse_max)))
        } else {
          return(list(selected = "YES",
                      comment = sprintf("Meets criteria (ΔOFV=%.2f, RSE=%.1f%%)", delta_ofv, rse_max)))
        }
      }
    }

    return(list(selected = "NO", comment = "Unknown"))
  }

  # Process each unique step
  unique_steps <- unique(db$step_number)
  unique_steps <- unique_steps[!is.na(unique_steps)]

  for (step in sort(unique_steps)) {
    step_models <- db[db$step_number == step & !is.na(db$step_number), ]

    if (nrow(step_models) == 0) next

    # Determine phase and threshold
    phase <- unique(step_models$phase)[1]
    is_backward <- grepl("backward", phase, ignore.case = TRUE)
    threshold_ofv <- if (is_backward) backward_threshold else forward_threshold

    # Process each model in the step
    for (i in 1:nrow(step_models)) {
      row <- step_models[i, ]

      # Get selection info
      selection_info <- get_selection_info(row, step_models, threshold_ofv, rse_threshold, phase)

      # Determine phase/step display
      if (row$action == "base_model") {
        phase_step <- "Base"
      } else if (is_backward) {
        phase_step <- sprintf("Backward_Step%d", step)
      } else {
        phase_step <- sprintf("Forward_Step%d", step)
      }

      # Get covariates display
      covariates_str <- get_model_covariates_display(row$model_name)

      # Determine status display
      status_display <- ifelse(row$status == "completed", "Successful",
                               ifelse(row$status == "failed", "Failed", "In Progress"))

      # Get description
      description <- row$step_description %||%
        ifelse(row$action == "add_covariate",
               paste("Add", row$covariate_tested),
               ifelse(row$action == "remove_covariate",
                      paste("Remove", row$covariate_tested),
                      row$action))

      # Add to results
      results <- rbind(results, data.frame(
        Phase_Step = phase_step,
        Model = row$model_name,
        Parent_Model = row$parent_model %||% "-",
        Description = description,
        Status = status_display,
        Covariates_in_Model = covariates_str,
        OFV = row$ofv %||% NA,
        Delta_OFV = row$delta_ofv %||% NA,
        RSE_Max = row$rse_max %||% NA,
        Selected = selection_info$selected,
        Comment = selection_info$comment,
        stringsAsFactors = FALSE
      ))
    }
  }

  # Handle base model separately (don't process in the step loop)
  # Base models should not go through the step loop above
  base_models <- db[db$action == "base_model" | is.na(db$step_number), ]
  if (nrow(base_models) > 0) {
    for (i in 1:nrow(base_models)) {
      row <- base_models[i, ]

      # Skip if already added
      if (row$model_name %in% results$Model) next

      # Get covariates display
      covariates_str <- get_model_covariates_display(row$model_name)

      results <- rbind(results, data.frame(
        Phase_Step = "Base",
        Model = row$model_name,
        Parent_Model = "-",
        Description = "Base Model",
        Status = "Successful",
        Covariates_in_Model = covariates_str,
        OFV = row$ofv %||% NA,
        Delta_OFV = NA,
        RSE_Max = row$rse_max %||% NA,
        Selected = "-",
        Comment = "Base model",
        stringsAsFactors = FALSE
      ))
    }
  }

  # Sort by Phase_Step first, then by model number
  if (nrow(results) > 0) {
    # Extract model numbers for sorting
    model_numbers <- as.numeric(gsub("^run", "", results$Model))

    # Create sort order for Phase_Step: Base < Forward < Backward
    phase_order <- ifelse(results$Phase_Step == "Base", 0,
                          ifelse(grepl("^Forward", results$Phase_Step), 1, 2))

    # Extract step numbers for proper ordering within Forward and Backward
    step_numbers <- as.numeric(gsub(".*Step", "", results$Phase_Step))
    step_numbers[is.na(step_numbers)] <- 0  # For Base

    # Sort by: phase_order, then step_numbers, then model_numbers
    results <- results[order(phase_order, step_numbers, model_numbers), ]
  }
  if (nrow(results) > 0) {
    results$OFV <- round(results$OFV, 2)
    results$Delta_OFV <- round(results$Delta_OFV, 2)
  }
  return(results)
}



#' Print Enhanced SCM Results Table
#'
#' @title Display SCM results table with RSE and evaluation comments
#' @param search_state List containing search state
#' @param show_rse Logical. Whether to show RSE column (default: TRUE)
#' @param truncate_covariates Integer. Max characters for covariate column (default: 35)
#' @export
print_scm_results_table <- function(search_state, show_rse = TRUE, truncate_covariates = 35) {

  results <- create_scm_results_table(search_state)

  if (nrow(results) == 0) {
    cat("No results to display\n")
    return(invisible(NULL))
  }

  cat("\n================== SCM RESULTS TABLE ==================\n\n")

  # Prepare formatted output
  for (i in seq_len(nrow(results))) {
    if (i == 1) {
      # Print header based on whether RSE is shown
      if (show_rse) {
        cat(sprintf("%-15s %-9s %-12s %-30s %-10s %-35s %8s %10s %6s %8s %s\n",
                    "Phase_Step", "Model", "Parent_Model", "Description", "Status",
                    "Covariates_in_Model", "OFV", "Delta_OFV", "RSE%", "Selected", "Comment"))
        cat(paste(rep("-", 180), collapse = ""), "\n")
      } else {
        cat(sprintf("%-15s %-9s %-12s %-30s %-10s %-35s %8s %10s %8s %s\n",
                    "Phase_Step", "Model", "Parent_Model", "Description", "Status",
                    "Covariates_in_Model", "OFV", "Delta_OFV", "Selected", "Comment"))
        cat(paste(rep("-", 165), collapse = ""), "\n")
      }
    }

    # Truncate covariates if needed
    cov_display <- results$Covariates_in_Model[i]
    if (nchar(cov_display) > truncate_covariates) {
      cov_display <- paste0(substr(cov_display, 1, truncate_covariates - 3), "...")
    }

    # Format RSE
    rse_display <- if (!is.na(results$RSE_Max[i])) {
      sprintf("%.1f", results$RSE_Max[i])
    } else {
      "-"
    }

    # Format OFV
    ofv_display <- if (!is.na(results$OFV[i])) {
      sprintf("%.2f", results$OFV[i])
    } else {
      "-"
    }

    # Format Delta OFV
    delta_ofv_display <- if (!is.na(results$Delta_OFV[i])) {
      sprintf("%.2f", results$Delta_OFV[i])
    } else {
      "-"
    }

    # Truncate comment if needed
    comment_display <- results$Comment[i]
    if (nchar(comment_display) > 50) {
      comment_display <- paste0(substr(comment_display, 1, 47), "...")
    }

    # Print row
    if (show_rse) {
      cat(sprintf("%-15s %-9s %-12s %-30s %-10s %-35s %8.2f %10s %6s %8s %s\n",
                  results$Phase_Step[i],
                  results$Model[i],
                  results$Parent_Model[i],
                  substr(results$Description[i], 1, 30),
                  results$Status[i],
                  cov_display,
                  if (!is.na(results$OFV[i])) results$OFV[i] else NA,
                  delta_ofv_display,
                  rse_display,
                  results$Selected[i],
                  comment_display))
    } else {
      cat(sprintf("%-15s %-9s %-12s %-30s %-10s %-35s %8.2f %10s %8s %s\n",
                  results$Phase_Step[i],
                  results$Model[i],
                  results$Parent_Model[i],
                  substr(results$Description[i], 1, 30),
                  results$Status[i],
                  cov_display,
                  if (!is.na(results$OFV[i])) results$OFV[i] else NA,
                  delta_ofv_display,
                  results$Selected[i],
                  comment_display))
    }
  }

  # Summary
  cat("\n================== SUMMARY ==================\n")

  # Find final selected model (last BEST from forward selection)
  forward_best <- results[results$Selected == "BEST" & grepl("^Forward", results$Phase_Step), ]
  backward_removed <- results[results$Selected == "REMOVED" & grepl("^Backward", results$Phase_Step), ]

  if (nrow(forward_best) > 0) {
    # Get the last BEST model from forward selection
    final_forward <- forward_best[nrow(forward_best), ]

    # Check if backward elimination happened and covariates were removed
    if (nrow(backward_removed) > 0) {
      cat("Forward selection completed, backward elimination removed some covariates.\n")
      # The final model is the one after backward removal
      final_model <- backward_removed[nrow(backward_removed), ]
    } else if (nrow(results[grepl("^Backward", results$Phase_Step), ]) > 0) {
      # Backward elimination happened but covariates were kept
      cat("Forward selection completed, backward elimination tested but covariates were kept.\n")
      final_model <- final_forward
    } else {
      final_model <- final_forward
    }

    base_model <- results[results$Phase_Step == "Base", ]

    cat(sprintf("\nFinal Model: %s\n", final_model$Model))
    cat(sprintf("Final Covariates: %s\n", final_model$Covariates_in_Model))
    cat(sprintf("Final OFV: %.2f\n", final_model$OFV))

    if (nrow(base_model) > 0 && !is.na(base_model$OFV[1]) && !is.na(final_model$OFV)) {
      total_improvement <- final_model$OFV - base_model$OFV[1]
      cat(sprintf("Total OFV Improvement from Base: %.2f\n", total_improvement))
    }

    # Count models by selection status
    cat(sprintf("\nModel Statistics:\n"))
    cat(sprintf("  Total models tested: %d\n", nrow(results) - sum(results$Phase_Step == "Base")))
    cat(sprintf("  Models meeting criteria (YES/BEST): %d\n",
                sum(results$Selected %in% c("YES", "BEST"), na.rm = TRUE)))
    cat(sprintf("  Models failed: %d\n", sum(results$Status == "Failed")))
    cat(sprintf("  Models with high RSE: %d\n", sum(grepl("RSE", results$Comment, ignore.case = TRUE) &
                                                      grepl("high|>", results$Comment))))
    cat(sprintf("  Models with insufficient OFV: %d\n",
                sum(grepl("Insufficient OFV|OFV \\(.*≤", results$Comment))))

    # Step-by-step summary
    cat("\nStep-by-Step Summary:\n")
    unique_steps <- unique(results$Phase_Step)
    unique_steps <- unique_steps[unique_steps != "Base"]

    for (step in unique_steps) {
      step_data <- results[results$Phase_Step == step, ]

      # For backward elimination, check for REMOVED (successful removal)
      if (grepl("^Backward", step)) {
        removed_in_step <- step_data[step_data$Selected == "REMOVED", ]
        kept_in_step <- step_data[step_data$Selected == "KEPT", ]

        if (nrow(removed_in_step) > 0) {
          cat(sprintf("  %s: Covariate removed - %s (ΔOFV=%.2f, RSE=%.1f%%)\n",
                      step, removed_in_step$Model[1],
                      abs(removed_in_step$Delta_OFV[1]), removed_in_step$RSE_Max[1]))
        } else if (nrow(kept_in_step) > 0) {
          cat(sprintf("  %s: Covariate kept - removal would worsen model\n", step))
        } else {
          cat(sprintf("  %s: %d tested, none removed\n", step, nrow(step_data)))
        }
      } else {
        # For forward selection, check for BEST
        best_in_step <- step_data[step_data$Selected == "BEST", ]

        if (nrow(best_in_step) > 0) {
          cat(sprintf("  %s: %s selected (ΔOFV=%.2f, RSE=%.1f%%)\n",
                      step, best_in_step$Model[1],
                      best_in_step$Delta_OFV[1], best_in_step$RSE_Max[1]))
        } else {
          meets_criteria <- sum(step_data$Selected %in% c("YES", "BEST"))
          cat(sprintf("  %s: No model selected (%d tested, %d met criteria)\n",
                      step, nrow(step_data), meets_criteria))
        }
      }
    }

  } else {
    cat("No significant improvements found during forward selection.\n")
    base_model <- results[results$Phase_Step == "Base", ]
    if (nrow(base_model) > 0) {
      cat(sprintf("Base model retained: %s (OFV=%.2f)\n",
                  base_model$Model[1], base_model$OFV[1]))
    }
  }

  return(invisible(results))
}

