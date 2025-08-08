# =============================================================================
# MONITORING FILES
# File: R/monitoring-files.R
# Part of CovariateSearcher Package
# File-based model monitoring
# =============================================================================



#' Read NONMEM .ext File and Detect Estimation Issues
#'
#' @title Parse .ext files and detect OFV > 10^10 and other estimation problems
#' @description Reads NONMEM .ext files to extract current OFV and detect
#'   estimation issues including boundary failures (OFV > 10^10), infinite values,
#'   and other numerical problems.
#' @param search_state List containing covariate search state and configuration
#' @param model_name Character. Model name (e.g., "run25")
#' @return List with status, current_ofv, iterations, and issue detection
#' @export
read_ext_file <- function(search_state, model_name) {
  ext_file <- file.path(search_state$models_folder, model_name, paste0(model_name, ".ext"))

  if (!file.exists(ext_file)) {
    return(list(
      status = "not_started",
      current_ofv = NA,
      iterations = 0,
      last_iteration_time = NA,
      has_estimation_issues = FALSE,
      issue_type = NA,
      estimation_method = NA
    ))
  }

  tryCatch({
    ext_lines <- readLines(ext_file, warn = FALSE)

    if (length(ext_lines) == 0) {
      return(list(
        status = "empty_file",
        current_ofv = NA,
        iterations = 0,
        last_iteration_time = file.info(ext_file)$mtime,
        has_estimation_issues = FALSE,
        issue_type = NA,
        estimation_method = NA
      ))
    }

    # Find all TABLE headers to identify multiple estimation methods
    table_lines <- which(grepl("^TABLE", ext_lines))

    if (length(table_lines) == 0) {
      table_lines <- 1
    }

    # Process each table and find the final (last) table
    final_table_data <- NULL
    final_method <- "Unknown"

    for (i in seq_along(table_lines)) {
      table_start <- table_lines[i]
      table_end <- if (i < length(table_lines)) table_lines[i + 1] - 1 else length(ext_lines)

      # Extract method name from table header
      if (table_start <= length(ext_lines)) {
        method_line <- ext_lines[table_start]
        if (grepl("First Order", method_line)) {
          final_method <- "FOCE"
        } else if (grepl("Importance", method_line)) {
          final_method <- "IMP"
        } else if (grepl("Stochastic", method_line)) {
          final_method <- "SAEM"
        } else if (grepl("Laplacian", method_line)) {
          final_method <- "LAPLACE"
        } else {
          final_method <- paste("Method", i)
        }
      }

      # Find data lines in this table
      table_section <- ext_lines[table_start:table_end]
      data_lines <- table_section[!grepl("^#|^TABLE|^\\s*ITERATION\\s+", table_section) &
                                    nchar(trimws(table_section)) > 0]

      if (length(data_lines) > 0) {
        final_table_data <- data_lines
      }
    }

    if (is.null(final_table_data) || length(final_table_data) == 0) {
      return(list(
        status = "no_data",
        current_ofv = NA,
        iterations = 0,
        last_iteration_time = file.info(ext_file)$mtime,
        has_estimation_issues = FALSE,
        issue_type = NA,
        estimation_method = final_method
      ))
    }

    # Parse the last (most recent) data line from final table
    last_line <- tail(final_table_data, 1)
    values <- as.numeric(strsplit(trimws(last_line), "\\s+")[[1]])

    # Handle potential parsing errors
    if (length(values) < 2) {
      return(list(
        status = "parse_error",
        current_ofv = NA,
        iterations = 0,
        last_iteration_time = file.info(ext_file)$mtime,
        has_estimation_issues = TRUE,
        issue_type = "parse_error",
        estimation_method = final_method
      ))
    }

    # Extract iteration and OFV
    current_iteration <- values[1]
    current_ofv <- values[length(values)]  # Last column is typically OBJ

    # Detect estimation issues
    has_issues <- FALSE
    issue_type <- NA

    if (!is.na(current_ofv)) {
      if (is.infinite(current_ofv)) {
        has_issues <- TRUE
        issue_type <- "infinite_ofv"
      } else if (is.nan(current_ofv)) {
        has_issues <- TRUE
        issue_type <- "nan_ofv"
      } else if (abs(current_ofv) > 1e10) {  # KEY: OFV > 10^10 detection
        has_issues <- TRUE
        issue_type <- "high_ofv"
      }
    } else {
      has_issues <- TRUE
      issue_type <- "missing_ofv"
    }

    # Check for other problematic values in the iteration
    if (!has_issues) {
      problematic_values <- values[is.infinite(values) | is.nan(values) | abs(values) > 1e10]
      if (length(problematic_values) > 0) {
        has_issues <- TRUE
        issue_type <- "problematic_parameters"
      }
    }

    # Determine status based on iteration number
    model_status <- if (current_iteration < 0) "completed" else "progressing"

    return(list(
      status = model_status,
      current_ofv = current_ofv,
      iterations = abs(current_iteration),
      last_iteration_time = file.info(ext_file)$mtime,
      has_estimation_issues = has_issues,
      issue_type = issue_type,
      estimation_method = final_method
    ))

  }, error = function(e) {
    return(list(
      status = "read_error",
      current_ofv = NA,
      iterations = 0,
      last_iteration_time = file.info(ext_file)$mtime,
      has_estimation_issues = TRUE,
      issue_type = paste("read_error:", e$message),
      estimation_method = "Unknown"
    ))
  })
}



#' Read NONMEM EXT File with Issue Detection (BBR Enhanced)
#'
#' @title Enhanced .ext file reader that detects estimation problems
#' @description Enhanced version from 06-recovery.R with BBR integration
#' @param model_name Character. Model name (e.g., "run25")
#' @param models_folder Character. Models folder path
#' @return List with detailed status and issue detection
#' @export
read_ext_file_with_issues <- function(model_name, models_folder = "models") {

  ext_file <- file.path(models_folder, model_name, paste0(model_name, ".ext"))

  if (!file.exists(ext_file)) {
    return(list(
      status = "not_started",
      current_ofv = NA,
      iterations = 0,
      last_iteration_time = NA,
      has_estimation_issues = FALSE,
      issue_type = NA,
      estimation_method = NA
    ))
  }

  tryCatch({
    ext_lines <- readLines(ext_file, warn = FALSE)

    if (length(ext_lines) == 0) {
      return(list(
        status = "empty_file",
        current_ofv = NA,
        iterations = 0,
        last_iteration_time = file.info(ext_file)$mtime,
        has_estimation_issues = FALSE,
        issue_type = NA,
        estimation_method = NA
      ))
    }

    # Find TABLE headers
    table_lines <- which(grepl("^TABLE", ext_lines))
    if (length(table_lines) == 0) table_lines <- 1

    # Process final table
    final_table_data <- NULL
    final_method <- "Unknown"

    for (i in seq_along(table_lines)) {
      table_start <- table_lines[i]
      table_end <- if (i < length(table_lines)) table_lines[i + 1] - 1 else length(ext_lines)

      # Extract method name
      if (table_start <= length(ext_lines)) {
        method_line <- ext_lines[table_start]
        if (grepl("First Order", method_line)) {
          method_name <- "FOCE"
        } else if (grepl("Importance", method_line)) {
          method_name <- "IMP"
        } else if (grepl("Stochastic", method_line)) {
          method_name <- "SAEM"
        } else if (grepl("Laplacian", method_line)) {
          method_name <- "LAPLACE"
        } else {
          method_name <- paste("Method", i)
        }
      } else {
        method_name <- "Unknown"
      }

      # Find data lines
      table_section <- ext_lines[table_start:table_end]
      data_lines <- table_section[!grepl("^#|^TABLE|^\\s*ITERATION\\s+", table_section) &
                                    nchar(trimws(table_section)) > 0]

      if (length(data_lines) > 0) {
        final_table_data <- data_lines
        final_method <- method_name
      }
    }

    if (is.null(final_table_data) || length(final_table_data) == 0) {
      return(list(
        status = "no_data",
        current_ofv = NA,
        iterations = 0,
        last_iteration_time = file.info(ext_file)$mtime,
        has_estimation_issues = FALSE,
        issue_type = NA,
        estimation_method = final_method
      ))
    }

    # Parse last data line
    last_line <- tail(final_table_data, 1)
    values <- as.numeric(strsplit(trimws(last_line), "\\s+")[[1]])

    if (length(values) < 2) {
      return(list(
        status = "parse_error",
        current_ofv = NA,
        iterations = 0,
        last_iteration_time = file.info(ext_file)$mtime,
        has_estimation_issues = TRUE,
        issue_type = "parse_error",
        estimation_method = final_method
      ))
    }

    # Extract iteration and OFV
    current_iteration <- values[1]
    current_ofv <- values[length(values)]

    # Detect estimation issues
    has_issues <- FALSE
    issue_type <- NA

    if (!is.na(current_ofv)) {
      if (is.infinite(current_ofv)) {
        has_issues <- TRUE
        issue_type <- "infinite_ofv"
      } else if (is.nan(current_ofv)) {
        has_issues <- TRUE
        issue_type <- "nan_ofv"
      } else if (abs(current_ofv) > 1e10) {
        has_issues <- TRUE
        issue_type <- "high_ofv"
      }
    } else {
      has_issues <- TRUE
      issue_type <- "missing_ofv"
    }

    # Check for problematic values
    if (!has_issues) {
      problematic_values <- values[is.infinite(values) | is.nan(values) | abs(values) > 1e10]
      if (length(problematic_values) > 0) {
        has_issues <- TRUE
        issue_type <- "problematic_parameters"
      }
    }

    # Determine status
    model_status <- if (current_iteration < 0) "completed" else "progressing"

    return(list(
      status = model_status,
      current_ofv = current_ofv,
      iterations = abs(current_iteration),
      last_iteration_time = file.info(ext_file)$mtime,
      has_estimation_issues = has_issues,
      issue_type = issue_type,
      estimation_method = final_method
    ))

  }, error = function(e) {
    return(list(
      status = "read_error",
      current_ofv = NA,
      iterations = 0,
      last_iteration_time = file.info(ext_file)$mtime,
      has_estimation_issues = TRUE,
      issue_type = paste("read_error:", e$message),
      estimation_method = "Unknown"
    ))
  })
}

