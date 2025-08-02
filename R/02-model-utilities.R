#' Extract Model Results
#'
#' @title Extract comprehensive model results from output files
#' @description Extracts OFV, parameters, and other results from NONMEM output
#' @param search_state List containing search state
#' @param model_name Character. Model name
#' @return List with extracted results
#' @export
extract_model_results <- function(search_state, model_name) {

  status <- get_model_status(search_state, model_name)
  ofv <- get_model_ofv(search_state, model_name)

  # Basic result structure
  results <- list(
    model_name = model_name,
    status = status,
    ofv = ofv,
    parameters = NULL,
    rse_values = NULL,
    extraction_time = Sys.time()
  )

  # TODO: Add parameter and RSE extraction when .ext parsing is implemented
  # For now, return basic results

  return(results)
}

#' Add Covariate to Model with Detailed Logging
#'
#' @title Wrapper for add_covariate_to_model with comprehensive logging
#' @description Calls add_covariate_to_model with detailed timestamped logging
#'   and saves complete log files for debugging and audit purposes.
#' @param search_state List containing search state
#' @param base_model_id Character. Base model identifier (e.g., "run1")
#' @param covariate_tag Character. Covariate tag to add (e.g., "cov_cl_wt")
#' @return List with result and log information
#' @export
add_covariate_with_detailed_logging <- function(search_state, base_model_id, covariate_tag) {

  # Initialize logging
  log_entries <- character(0)
  start_time <- Sys.time()

  # Logging helper function
  log_msg <- function(message) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    entry <- paste0(timestamp, " - ", message)
    log_entries <<- c(log_entries, entry)
    cat(entry, "\n")
  }

  # Start logging
  log_msg("=== Starting Covariate Addition Process ===")
  log_msg(paste("Parent model:", base_model_id))
  log_msg(paste("Covariate tag:", covariate_tag))

  # Get and validate tag value
  if (covariate_tag %in% names(search_state$tags)) {
    tag_value <- search_state$tags[[covariate_tag]]
    log_msg(paste("Tag value found:", tag_value))
  } else {
    log_msg(paste("ERROR: Covariate tag not found:", covariate_tag))
    error_log_filename <- paste0("ERROR_", covariate_tag, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_log.txt")
    writeLines(log_entries, error_log_filename)
    return(list(
      search_state = search_state,
      status = "error",
      error_message = "Covariate tag not found",
      log_file = error_log_filename,
      log_entries = log_entries
    ))
  }

  # Find matching covariate in search definition
  matching_cov <- search_state$covariate_search[
    grepl(paste0("_", tag_value, "$"), search_state$covariate_search$cov_to_test), ]

  if (nrow(matching_cov) > 0) {
    log_msg(paste("Matching covariate found:", matching_cov$COVARIATE[1]))
    combined_param <- paste0(matching_cov$COVARIATE[1], "_", matching_cov$PARAMETER[1])
    log_msg(paste("Combined parameter name:", combined_param))
  } else {
    log_msg(paste("ERROR: No matching covariate definition found for:", tag_value))
    error_log_filename <- paste0("ERROR_", covariate_tag, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_log.txt")
    writeLines(log_entries, error_log_filename)
    return(list(
      search_state = search_state,
      status = "error",
      error_message = "No matching covariate definition found",
      log_file = error_log_filename,
      log_entries = log_entries
    ))
  }

  # Predict new model name
  new_model_number <- search_state$model_counter + 1
  predicted_model <- paste0("run", new_model_number)
  log_msg(paste("New model name:", predicted_model))

  # Call the actual function with comprehensive error capture
  tryCatch({
    log_msg("Step 1: Creating BBR model...")

    # Call the existing add_covariate_to_model function
    result_state <- add_covariate_to_model(search_state, base_model_id, covariate_tag)

    log_msg("BBR model created successfully")
    log_msg("Step 2: Adding covariate to model file...")
    log_msg("Covariate successfully added to model file")

    # Calculate processing time
    process_time <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)
    log_msg(paste("=== Process completed successfully in", process_time, "seconds ==="))

    # Save success log file
    log_filename <- paste0(predicted_model, "_add_", tag_value, "_log.txt")
    log_msg(paste("Saving log to:", log_filename))

    writeLines(log_entries, log_filename)

    return(list(
      search_state = result_state,
      status = "success",
      model_name = predicted_model,
      covariate_added = tag_value,
      log_file = log_filename,
      log_entries = log_entries,
      processing_time_seconds = process_time
    ))

  }, error = function(e) {
    log_msg(paste("ERROR occurred:", e$message))
    log_msg("=== Process failed ===")

    # Save error log with detailed error information
    error_log_filename <- paste0("ERROR_", covariate_tag, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_log.txt")
    log_msg(paste("Saving error log to:", error_log_filename))

    writeLines(log_entries, error_log_filename)

    return(list(
      search_state = search_state,
      status = "error",
      error_message = e$message,
      log_file = error_log_filename,
      log_entries = log_entries,
      attempted_model = predicted_model,
      attempted_covariate = tag_value
    ))
  })
}
