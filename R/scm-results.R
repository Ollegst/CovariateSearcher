# =============================================================================
# SCM RESULTS
# File: R/scm-results.R
# Part of CovariateSearcher Package
# Statistical model evaluation and selection
# =============================================================================


#' Create SCM Results Table with Full Model Covariates
#'
#' @title Generate comprehensive SCM results table with all covariates in each model
#' @description Creates a detailed table showing phase/step, model name, all covariates
#'   currently in the model, OFV, delta OFV, and selection status
#' @param search_state List containing search state with database and tags
#' @return Data frame with comprehensive SCM results
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
    Selected = character(),
    stringsAsFactors = FALSE
  )

  # Helper function to get covariates for a model from YAML tags
  get_model_covariates_display <- function(model_name) {
    yaml_file <- file.path(search_state$models_folder, paste0(model_name, ".yaml"))
    yaml_data <- yaml::read_yaml(yaml_file)

    if (length(yaml_data$tags) == 0) {
      return(character())
    }

    # Convert tags to display format
    covariates_display <- character()

    for (tag in yaml_data$tags) {
      # Find matching covariate in search definition
      matching_rows <- search_state$covariate_search[
        grepl(paste0("_", tag, "$"), search_state$covariate_search$cov_to_test),
      ]

      if (nrow(matching_rows) > 0) {
        # Format as COVARIATE_on_PARAMETER
        cov <- matching_rows$COVARIATE[1]
        param <- matching_rows$PARAMETER[1]
        cov_display <- paste0(cov, "_on_", param)
        covariates_display <- c(covariates_display, cov_display)
      }
    }

    return(unique(covariates_display))
  }

  # Process each model
  for (i in 1:nrow(db)) {
    row <- db[i, ]

    # Get Phase_Step
    phase <- as.character(row$phase)
    step <- as.numeric(row$step_number)

    # Determine phase display name
    if (phase %in% c("base", "base_model")) {
      phase_display <- "Base"
    } else if (phase %in% c("forward_selection", "forward", "univariate", "Development")) {
      phase_display <- "Forward"
    } else if (phase %in% c("backward_elimination", "backward", "removal")) {
      phase_display <- "Backward"
    } else if (phase == "retry") {
      phase_display <- "Retry"
    } else {
      phase_display <- "Unknown"
    }

    # Create Phase_Step label
    if (phase_display == "Base") {
      phase_step <- "Base"
    } else {
      phase_step <- paste0(phase_display, "_Step", step)
    }

    # Model name
    model_name <- as.character(row$model_name)

    # Parent model
    parent_model <- ifelse(is.na(row$parent_model), "-", as.character(row$parent_model))

    # Description
    description <- as.character(row$step_description)
    if (is.na(row$step_description) || row$step_description == "") {
      description <- as.character(row$covariate_tested)
    }

    # Status
    status <- as.character(row$status)
    status_display <- ifelse(status %in% c("completed", "successful", "complete"),
                             "Successful", "Failed")

    # Get covariates in model
    model_covariates <- get_model_covariates_display(model_name)
    covariates_str <- ifelse(length(model_covariates) == 0, "Base",
                             paste(model_covariates, collapse = " + "))

    # OFV and Delta OFV
    ofv <- round(as.numeric(row$ofv), 2)
    delta_ofv <- round(as.numeric(row$delta_ofv), 2)

    # Determine SELECTED status
    selected <- "NO"

    # For base model
    if (phase_display == "Base") {
      selected <- "-"
    }
    # For forward selection phases
    else if (phase_display == "Forward") {
      # Check if meets threshold
      if (!is.na(delta_ofv) && delta_ofv > forward_threshold) {
        selected <- "YES"

        # Find all models in this step that meet threshold
        step_models <- db[db$step_number == step, ]
        qualified_models <- step_models[!is.na(step_models$delta_ofv) &
                                          step_models$delta_ofv > forward_threshold, ]

        if (nrow(qualified_models) > 0) {
          best_ofv <- min(qualified_models$ofv, na.rm = TRUE)
          if (abs(ofv - best_ofv) < 0.01) {
            selected <- "BEST"
          }
        }
      }
    }
    # For backward elimination phases
    else if (phase_display == "Backward") {
      if (!is.na(delta_ofv)) {
        # Accept if improvement or within threshold
        if (delta_ofv < 0 || delta_ofv < backward_threshold) {
          selected <- "YES"

          # Find all accepted removals in this step
          step_models <- db[db$step_number == step, ]
          accepted_models <- step_models[!is.na(step_models$delta_ofv) &
                                           (step_models$delta_ofv < 0 |
                                              step_models$delta_ofv < backward_threshold), ]

          if (nrow(accepted_models) > 0) {
            best_ofv <- min(accepted_models$ofv, na.rm = TRUE)
            if (abs(ofv - best_ofv) < 0.01) {
              selected <- "BEST"
            }
          }
        }
      }
    }

    # Add row to results
    results <- rbind(results, data.frame(
      Phase_Step = phase_step,
      Model = model_name,
      Parent_Model = parent_model,
      Description = description,
      Status = status_display,
      Covariates_in_Model = covariates_str,
      OFV = ofv,
      Delta_OFV = delta_ofv,
      Selected = selected,
      stringsAsFactors = FALSE
    ))
  }

  # Sort by model number
  model_numbers <- as.numeric(gsub("^run", "", results$Model))
  results <- results[order(model_numbers), ]

  return(results)
}

#' Print SCM Results Table
#'
#' @title Display SCM results table with formatting
#' @param search_state List containing search state
#' @export
print_scm_results_table <- function(search_state) {

  results <- create_scm_results_table(search_state)

  if (nrow(results) == 0) {
    cat("No results to display\n")
    return(invisible(NULL))
  }

  cat("\n================== SCM RESULTS TABLE ==================\n\n")

  # Display the table - force it to show
  for (i in seq_len(nrow(results))) {
    if (i == 1) {
      # Print header
      cat(sprintf("%-15s %-6s %-12s %-40s %-10s %-40s %8s %10s %8s\n",
                  "Phase_Step", "Model", "Parent_Model", "Description", "Status",
                  "Covariates_in_Model", "OFV", "Delta_OFV", "Selected"))
      cat(paste(rep("-", 160), collapse = ""), "\n")
    }
    cat(sprintf("%-15s %-6s %-12s %-40s %-10s %-40s %8.2f %10.2f %8s\n",
                results$Phase_Step[i],
                results$Model[i],
                results$Parent_Model[i],
                substr(results$Description[i], 1, 40),
                results$Status[i],
                substr(results$Covariates_in_Model[i], 1, 40),
                results$OFV[i],
                results$Delta_OFV[i],
                results$Selected[i]))
  }

  # Summary
  cat("\n================== SUMMARY ==================\n")

  # Find final model (last BEST)
  best_models <- results[results$Selected == "BEST", ]

  if (nrow(best_models) > 0) {
    final_model <- best_models[nrow(best_models), ]
    cat(sprintf("Final Model: %s\n", final_model$Model))
    cat(sprintf("Final Covariates: %s\n", final_model$Covariates_in_Model))
    cat(sprintf("Final OFV: %.2f\n", final_model$OFV))

    # Calculate total OFV improvement from base
    base_model <- results[results$Phase_Step == "Base", ]
    if (nrow(base_model) > 0) {
      total_improvement <- final_model$OFV - base_model$OFV[1]
      cat(sprintf("Total OFV Improvement: %.2f\n", total_improvement))
    }
  }

  # Show selection summary
  cat("\n================== SELECTION SUMMARY ==================\n")

  # Forward selection summary
  forward_steps <- unique(results$Phase_Step[grepl("^Forward", results$Phase_Step)])
  for (step in forward_steps) {
    step_data <- results[results$Phase_Step == step, ]
    selected_data <- step_data[step_data$Selected %in% c("YES", "BEST"), ]

    if (nrow(selected_data) > 0) {
      best_model <- selected_data$Model[selected_data$Selected == "BEST"][1]
      cat(sprintf("%s: %d models tested, %d met criteria, best: %s\n",
                  step, nrow(step_data), nrow(selected_data), best_model))
    } else {
      cat(sprintf("%s: %d models tested, none met criteria\n",
                  step, nrow(step_data)))
    }
  }

  # Backward elimination summary
  backward_steps <- unique(results$Phase_Step[grepl("^Backward", results$Phase_Step)])
  for (step in backward_steps) {
    step_data <- results[results$Phase_Step == step, ]
    accepted_data <- step_data[step_data$Selected %in% c("YES", "BEST"), ]

    if (nrow(accepted_data) > 0) {
      best_model <- accepted_data$Model[accepted_data$Selected == "BEST"][1]
      cat(sprintf("%s: %d removals tested, %d accepted, best: %s\n",
                  step, nrow(step_data), nrow(accepted_data), best_model))
    } else {
      cat(sprintf("%s: %d removals tested, none accepted\n",
                  step, nrow(step_data)))
    }
  }

  return(invisible(results))
}
