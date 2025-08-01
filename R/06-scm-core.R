# =============================================================================
# SCM CORE FUNCTIONS - STEPWISE COVARIATE MODELING
# File: R/06-scm-core.R
# Part of CovariateSearcher Package
# =============================================================================

#' Get Remaining Covariates for Testing
#'
#' @title Get list of covariate tags that haven't been tested from base model
#' @description Identifies which covariates from the search definition haven't
#'   been added to the specified base model yet.
#' @param search_state List containing covariate search state and configuration
#' @param base_model_id Character. Model to check current covariates against
#' @return Character vector of covariate tag names that can still be tested
#' @export
get_remaining_covariates <- function(search_state, base_model_id) {
  # Get current covariates in the base model
  current_covs <- get_model_covariates(search_state, base_model_id)

  # Get all available covariate tags
  all_cov_tags <- names(search_state$tags)[grepl("^cov_", names(search_state$tags))]

  # Return tags not currently in the model
  remaining_tags <- setdiff(all_cov_tags, current_covs)

  return(remaining_tags)
}

#' Get Dropped Covariates from Previous Steps
#'
#' @title Get covariates that were tested but not selected in previous steps
#' @description Identifies covariates that were tested in earlier SCM steps
#'   but not included in the current best model.
#' @param search_state List containing covariate search state and configuration
#' @param current_model_id Character. Current best model
#' @param tested_covariates Character vector. All covariate tags tested so far
#' @return Character vector of covariate tags that were dropped
#' @export
get_dropped_covariates <- function(search_state, current_model_id, tested_covariates) {
  current_covs <- get_model_covariates(search_state, current_model_id)
  dropped_covs <- setdiff(tested_covariates, current_covs)

  return(dropped_covs)
}

#' Run Univariate Step
#'
#' @title Run univariate analysis: test each covariate individually from base model
#' @description Creates individual covariate models for testing in parallel.
#'   Each covariate is added to the base model separately.
#' @param search_state List containing covariate search state and configuration
#' @param base_model_id Character. Base model to test from
#' @param covariates_to_test Character vector. Covariate tags to test
#' @param step_name Character. Description for this step
#' @return List with created model information and updated search_state
#' @export
run_univariate_step <- function(search_state, base_model_id, covariates_to_test, step_name) {
  if (length(covariates_to_test) == 0) {
    cat("❌ No covariates to test\n")
    return(list(
      search_state = search_state,
      step_name = step_name,
      base_model = base_model_id,
      models_created = character(0),
      status = "no_covariates"
    ))
  }

  cat(sprintf("\n🔬 %s\n", step_name))
  cat(sprintf("Base model: %s\n", base_model_id))
  cat(sprintf("Testing %d covariates: %s\n",
              length(covariates_to_test),
              paste(sapply(covariates_to_test, function(x) search_state$tags[[x]]), collapse = ", ")))

  # Create models for each covariate
  created_models <- list()
  step_start_time <- Sys.time()
  step_number <- max(search_state$search_database$step_number, na.rm = TRUE) + 1

  cat("🔧 Creating test models...\n")

  for (i in seq_along(covariates_to_test)) {
    cov_tag <- covariates_to_test[i]
    cov_name <- search_state$tags[[cov_tag]]

    cat(sprintf("  [%d/%d] Testing %s (%s)... ", i, length(covariates_to_test), cov_tag, cov_name))

    tryCatch({
      # Use existing add_covariate_to_model function
      result <- add_covariate_to_model(search_state, base_model_id, cov_tag)

      if (!is.null(result$model_name)) {
        model_name <- result$model_name
        created_models[[cov_tag]] <- model_name

        # Update search_state with the new model
        search_state <- result$search_state

        # Add step-specific information to database
        db_idx <- which(search_state$search_database$model_name == model_name)
        if (length(db_idx) > 0) {
          search_state$search_database$step_description[db_idx] <- step_name
          search_state$search_database$phase[db_idx] <- "forward_selection"
          search_state$search_database$step_number[db_idx] <- step_number
          search_state$search_database$covariate_tested[db_idx] <- cov_name
          search_state$search_database$action[db_idx] <- "add_single_covariate"
        }

        cat("✓\n")
      } else {
        cat("✗ Failed\n")
      }

    }, error = function(e) {
      cat(sprintf("✗ Error: %s\n", e$message))
    })
  }

  creation_time <- as.numeric(difftime(Sys.time(), step_start_time, units = "mins"))
  cat(sprintf("✅ Created %d test models in %.1f minutes\n",
              length(created_models), creation_time))

  return(list(
    search_state = search_state,
    step_name = step_name,
    base_model = base_model_id,
    models_created = unlist(created_models),
    covariate_tags = names(created_models),
    status = "models_created",
    creation_time = creation_time
  ))
}

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
    best_icon <- if (row$model_name == best_model) " 🏆" else ""

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
