#' @title Model Functions
#' @description Model management and operations
#' @name model-functions
NULL

#' Add Covariate to Model
#'
#' Creates a new model by adding a single covariate to an existing base model.
#' This is the core function for stepwise covariate modeling.
#'
#' @param search_state List. Current search state from initialize_covariate_search()
#' @param base_model_id Character. Base model identifier (e.g., "run1")
#' @param covariate_tag Character. Covariate tag to add (e.g., "cov_cl_wt")
#' @return Updated search state with new model added to database
#' @export
add_covariate_to_model <- function(search_state, base_model_id, covariate_tag) {

  cat(sprintf("🔧 Adding covariate %s to model %s\n", covariate_tag, base_model_id))

  # Get covariate information
  if (!covariate_tag %in% names(search_state$tags)) {
    stop("Covariate tag not found: ", covariate_tag)
  }

  covariate_name <- search_state$tags[[covariate_tag]]
  cat(sprintf("  Covariate: %s\n", covariate_name))

  # Generate new model name
  search_state$model_counter <- search_state$model_counter + 1
  new_model_name <- sprintf("run%d", search_state$model_counter)

  cat(sprintf("  New model: %s\n", new_model_name))

  # Find covariate definition in search database
  matching_cov <- search_state$covariate_search[
    grepl(paste0("_", covariate_name, "$"), search_state$covariate_search$cov_to_test), ]

  if (nrow(matching_cov) == 0) {
    warning("No matching covariate definition found for: ", covariate_name)
    cov_definition <- "Unknown"
  } else {
    cov_definition <- matching_cov$cov_to_test[1]
  }

  # Create new model entry for database
  new_model_entry <- tibble::tibble(
    model_name = new_model_name,
    step_description = sprintf("Add %s", covariate_name),
    phase = "forward_selection",
    step_number = 1L,
    parent_model = base_model_id,
    covariate_tested = covariate_name,
    action = "add_single_covariate",
    ofv = NA_real_,
    delta_ofv = NA_real_,
    rse_max = NA_real_,
    status = "created",
    tags = list(c(covariate_name)),
    submission_time = as.POSIXct(NA),
    completion_time = as.POSIXct(NA),
    retry_attempt = 0L,
    original_model = NA_character_,
    estimation_issue = NA_character_,
    excluded_from_step = FALSE
  )

  # Add to search database
  search_state$search_database <- dplyr::bind_rows(
    search_state$search_database, 
    new_model_entry
  )

  cat(sprintf("✅ Model %s added to database\n", new_model_name))

  return(search_state)
}

#' Get Model Status
#'
#' @param search_state List. Current search state
#' @param model_name Character. Model name to check
#' @return Character. Model status
#' @export
get_model_status <- function(search_state, model_name) {

  model_row <- search_state$search_database[
    search_state$search_database$model_name == model_name, ]

  if (nrow(model_row) == 0) {
    return("not_found")
  }

  return(model_row$status[1])
}

#' Get Model OFV
#'
#' @param search_state List. Current search state
#' @param model_name Character. Model name
#' @return Numeric. Model OFV or NA
#' @export
get_model_ofv <- function(search_state, model_name) {

  model_row <- search_state$search_database[
    search_state$search_database$model_name == model_name, ]

  if (nrow(model_row) == 0) {
    return(NA_real_)
  }

  return(model_row$ofv[1])
}

#' Get Model Covariates
#'
#' @param search_state List. Current search state
#' @param model_name Character. Model name
#' @return Character vector. Covariates in the model
#' @export
get_model_covariates <- function(search_state, model_name) {

  model_row <- search_state$search_database[
    search_state$search_database$model_name == model_name, ]

  if (nrow(model_row) == 0) {
    return(character(0))
  }

  # For now, return the single covariate tested
  # In full implementation, this would track cumulative covariates
  covariate <- model_row$covariate_tested[1]
  if (is.na(covariate)) {
    return(character(0))
  }

  return(covariate)
}
