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
  # Get status and OFV from actual NONMEM files, not database
  model_path <- file.path(search_state$models_folder, model_name)
  status <- get_model_status_from_files(model_path)  # ✅ From files
  ofv <- get_model_ofv_from_files(search_state, model_name)  # ✅ From files

  # Extract timestamps from LST file
  timestamps <- extract_nonmem_timestamps(model_name, search_state$models_folder)

  results <- list(
    model_name = model_name,
    status = status,
    ofv = ofv,
    parameters = NULL,
    rse_values = NULL,
    extraction_time = timestamps$stop_time %||% Sys.time()  # Use actual completion time
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
#' @param search_state List containing search state (unchanged in functional version)
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



#' Read NONMEM LST File with Basic Error Detection (FIXED)
#'
#' @title Robust LST file reader with comprehensive error handling
#' @description Enhanced version with comprehensive input validation and error handling
#' @param model_path Character. Path to model directory or lst file
#' @return List with status and error information
#' @export
read_nonmem_lst <- function(model_path) {
  # FIXED: Comprehensive input validation
  if (is.null(model_path) || length(model_path) == 0 ||
      nchar(as.character(model_path)) == 0 || as.character(model_path) == "") {
    return(list(
      found = FALSE,
      status = "invalid_input",
      error_message = "Invalid model path",
      error_excerpt = "",
      has_issues = TRUE
    ))
  }

  # FIXED: Safer file path handling
  model_path_str <- as.character(model_path)

  # Find the .lst file
  if (file.exists(model_path_str) && grepl("\\.lst$", model_path_str)) {
    lst_file <- model_path_str
  } else {
    model_name <- basename(model_path_str)
    # FIXED: Comprehensive model_name validation
    if (is.null(model_name) || length(model_name) == 0 ||
        nchar(as.character(model_name)) == 0 || as.character(model_name) == "") {
      return(list(
        found = FALSE,
        status = "invalid_path",
        error_message = "Cannot determine model name from path",
        error_excerpt = "",
        has_issues = TRUE
      ))
    }

    model_name_str <- as.character(model_name)
    possible_lst_files <- c(
      file.path(model_path_str, paste0(model_name_str, ".lst")),
      file.path(dirname(model_path_str), paste0(model_name_str, ".lst")),
      paste0(model_path_str, ".lst")
    )

    lst_file <- NULL
    for (path in possible_lst_files) {
      if (!is.null(path) && file.exists(path)) {
        lst_file <- path
        break
      }
    }
  }

  if (is.null(lst_file) || !file.exists(lst_file)) {
    return(list(
      found = FALSE,
      status = "not_run",
      error_message = "LST file not found",
      error_excerpt = "",
      has_issues = FALSE  # FIXED: Not having LST file yet is not an "issue"
    ))
  }

  # Read and analyze .lst file with comprehensive error handling
  tryCatch({
    lst_content <- readLines(lst_file, warn = FALSE)

    # FIXED: Handle empty LST file
    if (is.null(lst_content) || length(lst_content) == 0) {
      return(list(
        found = TRUE,
        file = lst_file,
        status = "incomplete",
        error_message = "Empty LST file",
        error_excerpt = "",
        has_issues = TRUE
      ))
    }

    # Initialize result
    result <- list(
      found = TRUE,
      file = lst_file,
      status = "unknown",
      error_message = "",
      error_excerpt = "",
      has_issues = FALSE
    )

    # FIXED: Safer pattern matching with validation
    has_successful <- any(grepl("MINIMIZATION SUCCESSFUL|OPTIMIZATION WAS COMPLETED", lst_content, ignore.case = FALSE))
    has_terminated <- any(grepl("MINIMIZATION TERMINATED", lst_content, ignore.case = FALSE))
    has_obj_terminated <- any(grepl("PROGRAM TERMINATED BY OBJ", lst_content, ignore.case = FALSE))

    # Check for successful completion
    if (has_successful) {
      result$status <- "completed"
      result$has_issues <- FALSE

    } else if (has_terminated) {
      result$status <- "failed"
      result$has_issues <- TRUE

      # FIXED: Comprehensive error message extraction with validation
      tryCatch({
        term_lines <- lst_content[grepl("MINIMIZATION TERMINATED|DUE TO|ERROR=",
                                        lst_content, ignore.case = FALSE)]

        if (length(term_lines) > 0) {
          # Extract the most informative line
          error_lines <- term_lines[grepl("DUE TO|ERROR=", term_lines, ignore.case = FALSE)]

          if (length(error_lines) > 0) {
            error_line <- error_lines[1]
            # FIXED: Safer string processing with validation
            if (!is.null(error_line) && nchar(as.character(error_line)) > 0) {
              clean_error <- tryCatch({
                error_str <- as.character(error_line)
                # Remove leading spaces
                error_str <- gsub("^\\s+", "", error_str)
                # Normalize spaces
                error_str <- gsub("\\s+", " ", error_str)
                # Remove "DUE TO"
                error_str <- gsub("DUE TO ", "", error_str)
                # Clean up error format
                error_str <- gsub("\\(ERROR=", "(Error ", error_str)
                # Final trim
                trimws(error_str)
              }, error = function(e) {
                "MINIMIZATION TERMINATED"
              })

              # FIXED: Ensure we have a non-empty error message
              if (!is.null(clean_error) && nchar(as.character(clean_error)) > 0) {
                result$error_message <- as.character(clean_error)
              } else {
                result$error_message <- "MINIMIZATION TERMINATED"
              }
            } else {
              result$error_message <- "MINIMIZATION TERMINATED"
            }
          } else {
            result$error_message <- "MINIMIZATION TERMINATED"
          }
        } else {
          result$error_message <- "MINIMIZATION TERMINATED"
        }
      }, error = function(e) {
        result$error_message <- "MINIMIZATION TERMINATED (error parsing details)"
      })

    } else if (has_obj_terminated) {
      result$status <- "failed"
      result$has_issues <- TRUE
      result$error_message <- "PROGRAM TERMINATED BY OBJ"

    } else {
      # FIXED: Better detection of incomplete vs running models
      has_execution <- any(grepl("NONMEM EXECUTION", lst_content, ignore.case = FALSE))

      if (has_execution) {
        result$status <- "incomplete"
        result$has_issues <- FALSE  # Running is not an "issue"
        result$error_message <- "Model still running"
      } else {
        result$status <- "incomplete"
        result$has_issues <- TRUE
        result$error_message <- "Model run incomplete"
      }
    }

    return(result)

  }, error = function(e) {
    return(list(
      found = FALSE,
      status = "read_error",
      error_message = paste("Error reading LST file:", as.character(e$message)),
      error_excerpt = "",
      has_issues = TRUE
    ))
  })
}


#' Get Model Status from Files with Enhanced Error Detection
#'
#' @title Determine overall model status with detailed error reporting
#' @description Enhanced version that provides detailed failure information
#' @param model_path Character. Path to model directory
#' @return Character. Overall model status with enhanced error detection
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

  # Return the status from enhanced LST analysis
  return(lst_info$status)
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

      ofv_lines <- grep("OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT", lst_content, value = TRUE)
      if (length(ofv_lines) > 0) {
        # Just take the first (and usually only) match
        ofv_match <- regmatches(ofv_lines[1],
                                regexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", ofv_lines[1]))
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


#' Extract NONMEM Start and Stop Timestamps from LST File
#'
#' @title Parse LST file to extract actual NONMEM execution timestamps
#' @description Reads NONMEM .lst file to extract the start time (first line)
#'   and stop time (line after "Stop Time:") when available.
#' @param model_name Character. Model name (e.g., "run1")
#' @param models_folder Character. Path to models folder (default: "models")
#' @return List with start_time and stop_time (POSIXct or NA)
#' @export
extract_nonmem_timestamps <- function(model_name, models_folder = "models") {

  lst_file_path <- file.path(models_folder, model_name, paste0(model_name, ".lst"))

  if (!file.exists(lst_file_path)) {
    return(list(
      start_time = NA,
      stop_time = NA,
      error = "LST file not found"
    ))
  }

  tryCatch({
    lst_lines <- readLines(lst_file_path, warn = FALSE)

    if (length(lst_lines) == 0) {
      return(list(
        start_time = NA,
        stop_time = NA,
        error = "Empty LST file"
      ))
    }

    # Extract start time (first line)
    start_time <- NA
    if (length(lst_lines) >= 1) {
      first_line <- trimws(lst_lines[1])
      # Parse timestamp format: "Mon Aug 11 07:19:54 EDT 2025"
      start_time <- tryCatch({
        # Remove timezone abbreviation and parse without it
        cleaned_line <- gsub(" [A-Z]{3,4} ", " ", first_line)  # Remove EDT/EST etc
        as.POSIXct(cleaned_line, format = "%a %b %d %H:%M:%S %Y")
      }, error = function(e) {
        # Fallback: try different format
        tryCatch({
          as.POSIXct(first_line, format = "%a %b %d %H:%M:%S %Z %Y", tz = "")
        }, error = function(e2) NA)
      })
    }

    # Extract stop time (line after "Stop Time:")
    stop_time <- NA
    stop_time_idx <- grep("^Stop Time:", lst_lines)

    if (length(stop_time_idx) > 0) {
      # Get the line after "Stop Time:"
      stop_line_idx <- stop_time_idx[1] + 1
      if (stop_line_idx <= length(lst_lines)) {
        stop_line <- trimws(lst_lines[stop_line_idx])
        # Parse timestamp format: "Mon Aug 11 07:23:07 EDT 2025"
        stop_time <- tryCatch({
          # Remove timezone abbreviation and parse without it
          cleaned_line <- gsub(" [A-Z]{3,4} ", " ", stop_line)  # Remove EDT/EST etc
          as.POSIXct(cleaned_line, format = "%a %b %d %H:%M:%S %Y")
        }, error = function(e) {
          # Fallback: try different format
          tryCatch({
            as.POSIXct(stop_line, format = "%a %b %d %H:%M:%S %Z %Y", tz = "")
          }, error = function(e2) NA)
        })
      }
    }

    return(list(
      start_time = start_time,
      stop_time = stop_time,
      found_start = !is.na(start_time),
      found_stop = !is.na(stop_time)
    ))

  }, error = function(e) {
    return(list(
      start_time = NA,
      stop_time = NA,
      error = paste("Error reading LST file:", e$message)
    ))
  })
}

