# =============================================================================
# FILE IO
# File: R/file-io.R
# Part of CovariateSearcher Package
# NONMEM file I/O operations
# =============================================================================


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
  base_path <- file.path(search_state$models_folder, run_name)
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



#' Read NONMEM EXT File
#'
#' Reads NONMEM .ext file to extract parameter estimates and OFV
#'
#' @param model_path Character. Path to model directory or ext file
#' @return List with OFV, parameters, and metadata
#' @export
read_nonmem_ext <- function(model_path) {

  # Find the .ext file
  if (file.exists(model_path) && grepl("\\.ext$", model_path)) {
    ext_file <- model_path
  } else {
    # Look for .ext file in model directory
    model_name <- basename(model_path)
    possible_ext_files <- c(
      file.path(model_path, paste0(model_name, ".ext")),
      file.path(dirname(model_path), paste0(model_name, ".ext")),
      paste0(model_path, ".ext")
    )

    ext_file <- NULL
    for (path in possible_ext_files) {
      if (file.exists(path)) {
        ext_file <- path
        break
      }
    }
  }

  if (is.null(ext_file) || !file.exists(ext_file)) {
    return(list(
      found = FALSE,
      error = "EXT file not found",
      ofv = NA_real_,
      parameters = NULL
    ))
  }

  # Read and parse .ext file
  tryCatch({
    ext_lines <- readLines(ext_file, warn = FALSE)

    # Find the final estimates (last non-comment line)
    data_lines <- ext_lines[!grepl("^\\s*;|^\\s*TABLE|^\\s*$", ext_lines)]

    if (length(data_lines) == 0) {
      return(list(found = FALSE, error = "No data lines in EXT file"))
    }

    # Get the last line (final estimates)
    final_line <- data_lines[length(data_lines)]
    values <- as.numeric(strsplit(trimws(final_line), "\\s+")[[1]])

    # First column is usually iteration, second is OFV
    ofv <- if (length(values) >= 2) values[2] else NA_real_

    # Extract parameter estimates (remaining columns)
    parameters <- if (length(values) > 2) values[3:length(values)] else numeric(0)

    return(list(
      found = TRUE,
      file = ext_file,
      ofv = ofv,
      parameters = parameters,
      n_parameters = length(parameters)
    ))

  }, error = function(e) {
    return(list(
      found = FALSE,
      error = paste("Error reading EXT file:", e$message),
      ofv = NA_real_
    ))
  })
}



#' Read NONMEM LST File Status
#'
#' Reads NONMEM .lst file to determine model status and issues
#'
#' @param model_path Character. Path to model directory or lst file
#' @return List with status information
#' @export
read_nonmem_lst <- function(model_path) {

  # Find the .lst file
  if (file.exists(model_path) && grepl("\\.lst$", model_path)) {
    lst_file <- model_path
  } else {
    model_name <- basename(model_path)
    possible_lst_files <- c(
      file.path(model_path, paste0(model_name, ".lst")),
      file.path(dirname(model_path), paste0(model_name, ".lst")),
      paste0(model_path, ".lst")
    )

    lst_file <- NULL
    for (path in possible_lst_files) {
      if (file.exists(path)) {
        lst_file <- path
        break
      }
    }
  }

  if (is.null(lst_file) || !file.exists(lst_file)) {
    return(list(
      found = FALSE,
      status = "not_run",
      error = "LST file not found"
    ))
  }

  # Read and analyze .lst file
  tryCatch({
    lst_content <- readLines(lst_file, warn = FALSE)

    # Check for successful completion
    if (any(grepl("MINIMIZATION SUCCESSFUL", lst_content))) {
      status <- "completed"
      termination <- "successful"
    } else if (any(grepl("MINIMIZATION TERMINATED", lst_content))) {
      status <- "terminated"
      termination <- "terminated"
    } else if (any(grepl("PARAMETER ESTIMATE IS NEAR ITS BOUNDARY", lst_content))) {
      status <- "boundary"
      termination <- "boundary_issue"
    } else if (any(grepl("ROUNDING ERRORS", lst_content))) {
      status <- "rounding_errors"
      termination <- "rounding_errors"
    } else {
      status <- "unknown"
      termination <- "unknown"
    }

    # Check for common issues
    issues <- character(0)
    if (any(grepl("COVARIANCE STEP ABORTED", lst_content))) {
      issues <- c(issues, "covariance_failed")
    }
    if (any(grepl("R MATRIX ALGORITHMICALLY SINGULAR", lst_content))) {
      issues <- c(issues, "singular_matrix")
    }
    if (any(grepl("STANDARD ERROR OF ESTIMATE", lst_content))) {
      # Has standard errors
    } else {
      issues <- c(issues, "no_standard_errors")
    }

    return(list(
      found = TRUE,
      file = lst_file,
      status = status,
      termination = termination,
      issues = issues,
      has_issues = length(issues) > 0
    ))

  }, error = function(e) {
    return(list(
      found = FALSE,
      status = "error",
      error = paste("Error reading LST file:", e$message)
    ))
  })
}



#' Get Model Status from Files
#'
#' Determines overall model status from NONMEM output files
#'
#' @param model_path Character. Path to model directory
#' @return Character. Overall model status
#' @export
get_model_status_from_files <- function(model_path) {

  lst_info <- read_nonmem_lst(model_path)
  ext_info <- read_nonmem_ext(model_path)

  if (!lst_info$found && !ext_info$found) {
    return("not_run")
  }

  if (!lst_info$found) {
    return("incomplete")
  }

  # Determine status based on LST file analysis
  if (lst_info$status == "completed" && !lst_info$has_issues) {
    return("completed")
  } else if (lst_info$status == "completed" && lst_info$has_issues) {
    return("completed_with_issues")
  } else if (lst_info$status %in% c("boundary", "rounding_errors")) {
    return("failed")
  } else {
    return("unknown")
  }
}



#' Get Model OFV from Files
#'
#' @title Extract OFV from model output files
#' @description Extracts OFV value from NONMEM output files
#' @param search_state List containing search state
#' @param model_name Character. Model name
#' @return Numeric. OFV value or NA
#' @export
get_model_ofv_from_files <- function(search_state, model_name) {

  model_path <- file.path(search_state$models_folder, model_name)
  status <- get_model_status_from_files(model_path)
  if (status != "completed") {
    return(NA)
  }

  tryCatch({
    # Try .lst file parsing first
    lst_file <- file.path(search_state$models_folder, model_name, paste0(model_name, ".lst"))
    if (file.exists(lst_file)) {
      lst_content <- readLines(lst_file, warn = FALSE)

      ofv_lines <- grep("OBJECTIVE FUNCTION VALUE", lst_content, value = TRUE)
      if (length(ofv_lines) > 0) {
        final_ofv_line <- utils::tail(ofv_lines, 1)
        ofv_match <- regmatches(final_ofv_line,
                                regexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", final_ofv_line))
        if (length(ofv_match) > 0) {
          return(as.numeric(ofv_match[1]))
        }
      }
    }

    return(NA)
  }, error = function(e) {
    return(NA)
  })
}



#' Get Model Covariates from Files
#'
#' @title Extract covariates from model using BBR tags
#' @description Gets covariate information from BBR model tags
#' @param search_state List containing search state
#' @param model_name Character. Model name
#' @return Character vector. Covariate names
#' @export
get_model_covariates_from_files <- function(search_state, model_name) {

  tryCatch({
    model_path <- file.path(search_state$models_folder, model_name)
    mod <- bbr::read_model(model_path)
    mod_tags <- mod$tags
    cov_tags <- names(search_state$tags)[grepl("^cov_", names(search_state$tags))]
    cov_tag_values <- unlist(search_state$tags[cov_tags])
    present_cov_values <- intersect(cov_tag_values, mod_tags)
    present_cov_names <- names(search_state$tags)[search_state$tags %in% present_cov_values]
    return(present_cov_names)
  }, error = function(e) {
    return(character(0))
  })
}

