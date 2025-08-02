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

#' Read Model File
#'
#' @title Read NONMEM control file with proper path handling
#' @description Reads model control file (.ctl or .mod) and stores file path as attribute
#' @param search_state List containing search state
#' @param run_name Character. Model name
#' @param extensions Character vector. File extensions to try (default: c(".ctl", ".mod"))
#' @return Character vector of model file lines with file_path attribute
#' @export
read_model_file <- function(search_state, run_name, extensions = c(".ctl", ".mod")) {
  base_path <- file.path(search_state$models_folder, run_name, run_name)
  for (ext in extensions) {
    file_path <- paste0(base_path, ext)
    if (file.exists(file_path)) {
      lines <- readLines(file_path, warn = FALSE)
      attr(lines, "file_path") <- file_path
      return(lines)
    }
  }
  stop("No file found for ", run_name, " with extensions: ", paste(extensions, collapse = ", "))
}

#' Write Model File
#'
#' @title Write modified NONMEM control file back to disk
#' @description Writes model file lines back to original location using stored file_path attribute
#' @param search_state List containing search state (unused but kept for consistency)
#' @param lines Character vector. Model file lines with file_path attribute
#' @return Updated search_state (unchanged)
#' @export
write_model_file <- function(search_state, lines) {
  file_path <- attr(lines, "file_path")
  if (is.null(file_path)) {
    stop("No file path found. Make sure the lines were read using read_model_file()")
  }
  writeLines(lines, file_path)
  cat("File saved to:", basename(file_path), "\n")
  return(search_state)
}

#' Add Covariate to Model File
#'
#' @title Core functionality to add covariate to NONMEM model file
#' @description Modifies NONMEM control file to add covariate relationship
#' @param search_state List containing search state
#' @param ref_model Character. Model name to modify
#' @param cov_on_param Character. Combined covariate-parameter name (e.g., "WT_CL")
#' @param id_var Character. ID variable name (default: "ID")
#' @param data_file Data.frame. Dataset for time-varying checks
#' @param covariate_search Data.frame. Covariate search configuration
#' @return Updated search_state (unchanged - file modification is side effect)
#' @export
model_add_cov <- function(search_state, ref_model, cov_on_param, id_var = "ID",
                          data_file, covariate_search) {

  modelcode <- read_model_file(search_state, ref_model)
  original_file_path <- attr(modelcode, "file_path")

  cov_on_param <- paste0("beta_", cov_on_param)

  cova <- covariate_search$COVARIATE[covariate_search$cov_to_test == cov_on_param]
  param <- covariate_search$PARAMETER[covariate_search$cov_to_test == cov_on_param]
  ref <- covariate_search$REFERENCE[covariate_search$cov_to_test == cov_on_param]

  # Get max THETA number
  thetas <- modelcode[grepl('THETA\\(..?\\)', modelcode)] %>%
    gsub(pattern = '.*THETA\\(', replacement = '') %>%
    gsub(pattern = '\\).*', replacement = '') %>%
    as.double()

  newtheta <- max(thetas) + 1
  temp_cov <- dplyr::filter(covariate_search, cov_to_test == cov_on_param)

  # Determine covariate type and formula
  cov_status <- temp_cov$STATUS[temp_cov$COVARIATE == cova]
  cov_formula <- temp_cov$FORMULA[temp_cov$COVARIATE == cova]

  FLAG <- dplyr::case_when(
    length(cov_status) == 0 | length(cov_formula) == 0 ~ "ERROR: Missing covariate info",
    cov_status == "cat" & cov_formula == "linear" ~ "1",
    cov_status == "cat" & cov_formula == "power" ~ "2",
    cov_status == "con" & cov_formula == "linear" ~ "3",
    cov_status == "con" & cov_formula == "power" ~ "2",
    cov_status == "con" & cov_formula == "power1" ~ "5",
    cov_status == "con" & cov_formula == "power0.75" ~ "6",
    .default = "Please check covariate status and formula"
  )

  initialValuethetacov <- dplyr::case_when(
    FLAG == "5" ~ "1 FIX",
    FLAG == "6" ~ "0.75 FIX",
    .default = "0.1"
  )

  # Generate formula based on FLAG
  if(FLAG == "2") formule <- paste0(' * (',cova,'/',ref,')**THETA(', newtheta ,')')
  if(FLAG == "3") formule <- paste0(' * (1 + (',cova,'-',ref, ') * THETA(',newtheta ,'))')
  if(FLAG == "5") formule <- paste0(' * (',cova,'/',ref,')** THETA(', newtheta ,')')
  if(FLAG == "6") formule <- paste0(' * (',cova,'/',ref,')** THETA(', newtheta ,')')

  # Handle categorical covariates (simplified for core module)
  if(FLAG == "1") {
    formule <- paste0(' * (1 + THETA(', newtheta ,') * ',cova ,')')
  }

  # Check if time-dependent
  max_levels <- max(tapply(data_file[[cova]], data_file[[id_var]],
                           function(x) length(unique(x))), na.rm = TRUE)
  time_varying <- max_levels > 1

  if(time_varying == FALSE){
    linetu <- grep(paste0('^\\s*TV_', param), modelcode)
  } else {
    linetu <- grep(paste0('^\\s*', param), modelcode)
  }

  # Add covariate to parameter line
  if(grepl(";", modelcode[linetu])){
    modelcode[linetu] <- sub("(.*?)(\\s*;)", paste0("\\1", formule, "\\2"), modelcode[linetu])
  } else {
    modelcode[linetu] <- paste0(modelcode[linetu], formule)
  }

  # Add THETA line
  newthetaline <- paste0(initialValuethetacov, ' ; ', cov_on_param, ' ;  ; RATIO')
  lineomeg <- grep('\\$OMEGA', modelcode)[1]

  modelcode <- c(
    modelcode[1:(lineomeg - 1)],
    newthetaline,
    modelcode[lineomeg:length(modelcode)]
  )

  # Write back
  attr(modelcode, "file_path") <- original_file_path
  search_state <- write_model_file(search_state, modelcode)

  return(search_state)
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
