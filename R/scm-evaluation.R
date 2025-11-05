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
  if (is.null(rse_threshold)) {
    rse_threshold <- search_state$search_config$max_rse_threshold %||% 50
  }

  # Calculate display threshold for df=1 (most common case)
  ofv_threshold_display <- pvalue_to_threshold(p_value, df = 1)

  cat(sprintf("\n📊 EVALUATING MODELS (Forward OFV threshold: %.2f for df=1, RSE < %d%%)\n",
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

      if (length(parent_ofv) > 0 && !is.na(parent_ofv) && !is.na(model_data$ofv[i])) {
        delta_ofv <- parent_ofv - model_data$ofv[i]  # Positive = improvement
        model_data$delta_ofv[i] <- delta_ofv

        # Update in main database
        db_idx <- which(search_state$search_database$model_name == model_data$model_name[i])
        search_state$search_database$delta_ofv[db_idx] <- delta_ofv
      }
    }
  }

  # CRITICAL: Calculate threshold per model based on covariate df
  model_data$ofv_threshold <- NA_real_
  model_data$covariate_df <- NA_integer_

  for (i in 1:nrow(model_data)) {
    # Extract covariate name from tag
    cov_tag <- model_data$covariate_tested[i]

    if (!is.na(cov_tag) && nchar(cov_tag) > 0) {
      cov_name <- extract_covariate_name_from_tag(cov_tag)

      if (!is.na(cov_name)) {
        # Calculate df for this covariate
        df <- calculate_covariate_df(cov_name, search_state$covariate_search)
        model_data$covariate_df[i] <- df

        # Calculate threshold for this specific covariate
        model_data$ofv_threshold[i] <- pvalue_to_threshold(p_value, df)
      }
    }

    # Default to df=1 if couldn't determine
    if (is.na(model_data$ofv_threshold[i])) {
      model_data$covariate_df[i] <- 1L
      model_data$ofv_threshold[i] <- pvalue_to_threshold(p_value, df = 1)
    }
  }

  # Evaluate each model using its specific threshold
  evaluation_results <- model_data %>%
    dplyr::mutate(
      delta_ofv_significant = !is.na(delta_ofv) & delta_ofv > ofv_threshold,
      rse_acceptable = is.na(rse_max) | rse_max < rse_threshold,
      overall_significant = delta_ofv_significant & rse_acceptable,
      evaluation_notes = dplyr::case_when(
        is.na(delta_ofv) ~ "Delta OFV not available",
        !delta_ofv_significant & !rse_acceptable ~ sprintf("Poor OFV (need >%.2f) and high RSE", ofv_threshold),
        !delta_ofv_significant ~ sprintf("Insufficient OFV (%.2f, need >%.2f for df=%d)", delta_ofv, ofv_threshold, covariate_df),
        !rse_acceptable ~ sprintf("RSE too high (%.1f%%, limit %d%%)", rse_max, rse_threshold),
        overall_significant ~ sprintf("Meets all criteria (OFV=%.2f>%.2f, RSE=%.1f%%)", delta_ofv, ofv_threshold, rse_max),
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
    best_icon <- if (length(row$model_name) > 0 && !is.na(row$model_name) &&
                     !is.null(best_model) && !is.na(best_model)) {
      if (row$model_name == best_model) " 🏆" else ""
    } else {
      ""
    }

    # Show threshold used for this model
    cat(sprintf("  %s %s: OFV=%.2f (threshold=%.2f for df=%d), RSE=%.1f%% - %s%s\n",
                status_icon,
                row$model_name,
                ifelse(is.na(row$delta_ofv), 0, row$delta_ofv),
                row$ofv_threshold,
                row$covariate_df,
                ifelse(is.na(row$rse_max), 0, row$rse_max),
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

