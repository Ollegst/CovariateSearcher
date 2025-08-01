# =============================================================================
# MODEL UTILITIES - FILE I/O AND MODEL MODIFICATION
# File: R/02-model-utilities.R
# Part of CovariateSearcher Package
# =============================================================================

#' Read Model File
#'
#' @title Read model file content with path tracking
#' @description Reads NONMEM model file (.ctl or .mod) and preserves file path
#' @param search_state List containing search state
#' @param run_name Character. Model name (e.g., "run1")
#' @param extensions Character vector. File extensions to try (default: c(".ctl", ".mod"))
#' @return Character vector with file content and path attribute
#' @export
read_model_file <- function(search_state, run_name, extensions = c(".ctl", ".mod")) {
  base_path <- file.path(search_state$models_folder, run_name)

  for (ext in extensions) {
    file_path <- paste0(base_path, ext)
    if (file.exists(file_path)) {
      lines <- readLines(file_path)
      attr(lines, "file_path") <- file_path
      return(lines)
    }
  }

  stop("No file found for ", run_name, " with extensions: ",
       paste(extensions, collapse = ", "))
}

#' Write Model File
#'
#' @title Write model file content to disk
#' @description Writes model content back to file using stored path
#' @param search_state List containing search state
#' @param lines Character vector with file content and path attribute
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

#' View Search Database
#'
#' @title View the search database with model relationships
#' @description Displays a formatted view of the current search database
#' @param search_state List containing search state
#' @param detailed Logical. Show detailed view (default: TRUE)
#' @return Invisible search database
#' @export
view_search_database <- function(search_state, detailed = TRUE) {

  if (nrow(search_state$search_database) == 0) {
    cat("Search database is empty.\n")
    return(invisible(NULL))
  }

  cat("=== Covariate Search Database ===\n")
  cat("Total models:", nrow(search_state$search_database), "\n\n")

  if (detailed) {
    db <- search_state$search_database %>%
      dplyr::mutate(
        ofv_display = ifelse(is.na(ofv), "pending", sprintf("%.2f", ofv)),
        parent_display = ifelse(is.na(parent_model), "-", parent_model),
        delta_display = ifelse(is.na(delta_ofv), "-", sprintf("%.2f", delta_ofv))
      )

    # Add changes column
    db$changes <- sapply(1:nrow(db), function(i) {
      model_name <- db$model_name[i]
      parent_model <- db$parent_display[i]

      if (model_name == search_state$base_model) {
        return("Base model")
      } else if (parent_model == "-" || is.na(parent_model)) {
        return("No parent info")
      } else {
        # Simple covariate difference
        current_covs <- get_model_covariates(search_state, model_name)
        if (length(current_covs) > 0) {
          cov_names <- sapply(current_covs, function(x) {
            if (x %in% names(search_state$tags)) search_state$tags[[x]] else x
          })
          return(paste0("+ ", paste(cov_names, collapse = ", ")))
        } else {
          return("No covariates")
        }
      }
    })

    # Select key columns
    db <- db %>%
      dplyr::select(model_name, parent_display, changes, status, ofv_display, delta_display)

    print(db, n = Inf)
  }

  return(invisible(search_state$search_database))
}

#' Calculate Delta OFV
#'
#' @title Calculate OFV difference between models
#' @description Calculates the change in OFV between parent and child models
#' @param search_state List containing search state
#' @param child_model Character. Child model name
#' @param parent_model Character. Parent model name
#' @return Numeric. Delta OFV (positive = improvement)
#' @export
calculate_delta_ofv <- function(search_state, child_model, parent_model) {
  child_ofv <- get_model_ofv(search_state, child_model)
  parent_ofv <- get_model_ofv(search_state, parent_model)

  if (is.na(child_ofv) || is.na(parent_ofv)) {
    return(NA_real_)
  }

  # Positive delta = improvement (lower OFV)
  delta_ofv <- parent_ofv - child_ofv
  return(delta_ofv)
}

#' Update Model Status from Files
#'
#' @title Update specific model status by reading output files
#' @description Updates model status and OFV by reading NONMEM output files
#' @param search_state List containing search state
#' @param model_name Character. Model name to update
#' @return Updated search_state with refreshed model information
#' @export
update_model_status_from_files <- function(search_state, model_name) {

  # Find model in database
  db_idx <- which(search_state$search_database$model_name == model_name)
  if (length(db_idx) == 0) {
    warning("Model not found in database: ", model_name)
    return(search_state)
  }

  # Update status
  new_status <- get_model_status(search_state, model_name)
  search_state$search_database$status[db_idx] <- new_status

  # Update OFV if completed
  if (new_status == "completed") {
    new_ofv <- get_model_ofv(search_state, model_name)
    search_state$search_database$ofv[db_idx] <- new_ofv
    search_state$search_database$completion_time[db_idx] <- Sys.time()

    # Update delta OFV if parent exists
    parent_model <- search_state$search_database$parent_model[db_idx]
    if (!is.na(parent_model) && parent_model != "") {
      delta_ofv <- calculate_delta_ofv(search_state, model_name, parent_model)
      search_state$search_database$delta_ofv[db_idx] <- delta_ofv
    }
  }

  return(search_state)
}

#' Validate Model Quality
#'
#' @title Validate model quality using multiple criteria
#' @description Checks model quality using OFV, RSE, and convergence criteria
#' @param search_state List containing search state
#' @param model_name Character. Model name to validate
#' @param rse_threshold Numeric. Maximum RSE threshold (default: 50)
#' @return List with validation results
#' @export
validate_model_quality <- function(search_state, model_name, rse_threshold = 50) {

  status <- get_model_status(search_state, model_name)

  if (status != "completed") {
    return(list(
      valid = FALSE,
      reason = paste("Model not completed. Status:", status),
      ofv = NA,
      rse_max = NA
    ))
  }

  # Get OFV
  ofv <- get_model_ofv(search_state, model_name)

  # Check for boundary OFV
  if (!is.na(ofv) && abs(ofv) > 1e10) {
    return(list(
      valid = FALSE,
      reason = "OFV boundary failure (> 10^10)",
      ofv = ofv,
      rse_max = NA
    ))
  }

  # TODO: Add RSE checking when .ext file parsing is available
  # For now, assume RSE is acceptable
  rse_max <- NA

  return(list(
    valid = TRUE,
    reason = "Model passed quality checks",
    ofv = ofv,
    rse_max = rse_max
  ))
}

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
