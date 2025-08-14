# =============================================================================
# SCM EVALUATION
# File: R/scm-evaluation.R
# Part of CovariateSearcher Package
# Statistical model evaluation and selection
# =============================================================================



#' Select Best Model from Statistical Evaluation
#'
#' @title Evaluate models and select the best one based on statistical criteria
#' @description Evaluates completed models using delta OFV and RSE thresholds
#'   to identify the best performing model.
#' @param search_state List containing covariate search state and configuration
#' @param model_names Character vector. Model names to evaluate
#' @param ofv_threshold Numeric. OFV improvement threshold (uses config if NULL)
#' @param rse_threshold Numeric. Maximum RSE threshold (uses config if NULL)
#' @return List with best model, evaluation details, and updated search_state
#' @export
select_best_model <- function(search_state, model_names, ofv_threshold = NULL, rse_threshold = NULL) {
  # Use config defaults if not specified
  if (is.null(ofv_threshold)) {
    ofv_threshold <- search_state$search_config$forward_ofv_threshold
  }
  if (is.null(rse_threshold)) {
    rse_threshold <- search_state$search_config$max_rse_threshold
  }

  cat(sprintf("\n📊 EVALUATING MODELS (ΔOFV > %.2f, RSE < %d%%)\n",
              ofv_threshold, rse_threshold))

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

      if (length(parent_ofv) > 0 && !is.na(parent_ofv) && !is.na(model_data$ofv[i])) {
        delta_ofv <- parent_ofv - model_data$ofv[i]  # Positive = improvement
        model_data$delta_ofv[i] <- delta_ofv

        # Update in main database
        db_idx <- which(search_state$search_database$model_name == model_data$model_name[i])
        search_state$search_database$delta_ofv[db_idx] <- delta_ofv
      }
    }
  }

  # Evaluate each model
  evaluation_results <- model_data %>%
    dplyr::mutate(
      delta_ofv_significant = !is.na(delta_ofv) & delta_ofv > ofv_threshold,
      rse_acceptable = is.na(rse_max) | rse_max < rse_threshold,
      overall_significant = delta_ofv_significant & rse_acceptable,
      evaluation_notes = dplyr::case_when(
        is.na(delta_ofv) ~ "Delta OFV not available",
        !delta_ofv_significant & !rse_acceptable ~ "Poor OFV and high RSE",
        !delta_ofv_significant ~ "Insufficient OFV improvement",
        !rse_acceptable ~ "RSE too high",
        overall_significant ~ "Meets all criteria",
        TRUE ~ "Unknown issue"
      )
    ) %>%
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
  for (i in 1:nrow(evaluation_results)) {
    row <- evaluation_results[i, ]
    status_icon <- if (row$overall_significant) "✅" else "❌"
    best_icon <- if (length(row$model_name) > 0 &&
                     !is.na(row$model_name) &&
                     row$model_name == best_model) " 🏆" else ""

    cat(sprintf("  %s %s: ΔOFV=%.2f, %s%s\n",
                status_icon, row$model_name,
                ifelse(is.na(row$delta_ofv), 0, row$delta_ofv),
                row$evaluation_notes, best_icon))
  }

  cat(sprintf("\n🎯 Summary: %d significant models found\n", length(significant_models)))
  if (!is.null(best_model)) {
    best_delta <- evaluation_results$delta_ofv[evaluation_results$model_name == best_model]
    cat(sprintf("🏆 Best model selected: %s (ΔOFV = %.2f)\n", best_model, best_delta))
  } else {
    cat("❌ No significant improvement found - keeping current base model\n")
  }

  return(list(
    search_state = search_state,
    best_model = best_model,
    significant_models = significant_models,
    evaluation_results = evaluation_results,
    criteria_used = list(
      ofv_threshold = ofv_threshold,
      rse_threshold = rse_threshold
    ),
    status = if (!is.null(best_model)) "best_model_found" else "no_improvement"
  ))
}

