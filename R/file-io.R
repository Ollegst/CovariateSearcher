# =============================================================================
# FILE IO
# File: R/file-io.R
# Part of CovariateSearcher Package
# NONMEM file I/O operations
# =============================================================================

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
      lines <- .read_control_stream(file_path)
      attr(lines, "file_path") <- file_path
      return(lines)
    }
  }
  stop("No file found for ", run_name, " with extensions: ", paste(extensions, collapse = ", "))
}


#' Find Model File Path
#'
#' @title Find the actual file path for a NONMEM model
#' @description Given a base path (without extension), finds the actual model file
#'   by trying .ctl and .mod extensions in order.
#' @param base_path Character. Base path without extension (e.g., "models/run001")
#' @param extensions Character vector. File extensions to try (default: c(".ctl", ".mod"))
#' @return Character string with the full path to the found file, or NULL if not found
#' @export
find_model_file <- function(base_path, extensions = c(".ctl", ".mod")) {
  for (ext in extensions) {
    file_path <- paste0(base_path, ext)
    if (file.exists(file_path)) {
      return(file_path)
    }
  }
  return(NULL)
}


# Lines that are not valid UTF-8 re-read as Windows-1252, the encoding a Windows
# editor saving "ANSI" writes. readLines() does not fail on such bytes: under a
# UTF-8 locale the line comes back invalid, and grepl() silently returns FALSE
# on it - a comment-only line carrying a cp1252 "µ" would not match "^\\s*;"
# and would be counted as a THETA. CP1252 rather than latin1 because latin1
# turns cp1252's quotes, dashes and euro sign (0x80-0x9F) into control
# characters. Valid UTF-8 lines are returned untouched; a byte cp1252 leaves
# undefined becomes "?".
#' @keywords internal
#' @noRd
.repair_encoding <- function(lines) {
  bad <- !validUTF8(lines)
  if (any(bad)) {
    lines[bad] <- iconv(lines[bad], from = "CP1252", to = "UTF-8", sub = "?")
  }
  lines
}


# Line indices where a `$<tag>` record starts. NM-TRAN accepts leading blanks
# and any case, so both are allowed; the name must not continue with a letter,
# so "THETA" matches $THETA and $THETA(0,1) but not the $PRIOR records $THETAP /
# $THETAPV. `tag` is a regex fragment, e.g. "(?:PK|PRED)".
#' @keywords internal
#' @noRd
.record_starts <- function(lines, tag) {
  grep(paste0("^\\s*\\$", tag, "(?![A-Za-z])"), lines,
       ignore.case = TRUE, perl = TRUE)
}


# TRUE for every line that starts a record (a `$` as first non-blank character);
# a record runs until the next such line.
#' @keywords internal
#' @noRd
.is_record_line <- function(lines) {
  grepl("^\\s*\\$", lines)
}


# Number of THETA values in the value part of one $THETA line (the text before
# the first ";"). Each parenthesised group - "(0, 2.5)", "(0, 2.5, 100) FIX" -
# is one value; outside groups each bare number is one; FIX/FIXED is not a
# value, also when written against the number. "(0,2.5) (0,30)" is 2, "2.5 30"
# is 2, "0.75 FIX" and "0.75FIX" are 1.
#' @keywords internal
#' @noRd
.count_theta_values <- function(value_text) {
  group_pat <- "\\([^)]*\\)"
  n_groups <- sum(gregexpr(group_pat, value_text)[[1]] > 0)
  rest <- gsub(group_pat, " ", value_text)
  rest <- gsub("FIX(ED)?", " ", rest, ignore.case = TRUE)
  tokens <- strsplit(trimws(rest), "[[:space:],]+")[[1]]
  number <- "^[-+]?(\\d+\\.?\\d*|\\.\\d+)([eEdD][-+]?\\d+)?$"
  n_groups + sum(grepl(number, tokens))
}


# Is this one THETA value an initial estimate of 0 that is not FIXED? NONMEM
# cannot start estimating a parameter from 0, so such a value is only valid
# fixed. `value_text` is one value: "0", "(0, 0, 1)", "(0)", "0 FIX".
#' @keywords internal
#' @noRd
.theta_init_is_unfixed_zero <- function(value_text) {
  value_text <- trimws(value_text)
  if (grepl("\\bFIX(ED)?\\b|[0-9.]FIX", value_text, ignore.case = TRUE)) return(FALSE)
  init <- if (grepl("^\\(", value_text)) {
    parts <- trimws(strsplit(sub("^\\(([^)]*)\\).*$", "\\1", value_text), ",")[[1]])
    if (length(parts) == 1L) parts[1] else parts[2]
  } else {
    regmatches(value_text, regexpr("^[-+]?(\\d+\\.?\\d*|\\.\\d+)([eEdD][-+]?\\d+)?", value_text))
  }
  if (length(init) == 0L) return(FALSE)
  isTRUE(suppressWarnings(as.numeric(sub("[dD]", "e", init))) == 0)
}


# The annotation of a $THETA / $OMEGA / $SIGMA line, "value ; name ; unit ;
# transform": fields split on ";" with empty ones kept, so a blank unit
# ("; CL ; ; RATIO") does not shift the transform into its place, and anything
# after the 4th field (a trailing comment) ignored. The transform is
# upper-cased: NM-TRAN and validation accept "log" as well as "LOG".
#' @keywords internal
#' @noRd
.parse_annotation <- function(line) {
  fields <- trimws(strsplit(paste0(line, ";"), ";", fixed = TRUE)[[1]])
  fields <- c(fields, "", "", "", "")
  list(value = fields[1], name = fields[2], unit = fields[3],
       trans = toupper(fields[4]))
}


# Every THETA in the control stream, one row per value, in NONMEM's numbering:
# all $THETA records in file order ($THETAP / $THETAPV excluded), a value on
# the record line itself included, blank and comment-only lines skipped, and
# several values on one line counted separately. This is the one place THETA
# numbers are derived from $THETA records; the writer, the remover and the
# simulation readers all use it.
#
# Columns: index (THETA number), line (line in `lines`), on_record_line,
# n_on_line (values sharing that line), and the annotation fields of the line
# ("value ; name ; unit ; transform"): name, unit, trans (upper-cased). A line
# holding several values gives its annotation to the first of them only.
#' @keywords internal
#' @noRd
.parse_theta_records <- function(lines) {
  empty <- data.frame(index = integer(), line = integer(), on_record_line = logical(),
                      n_on_line = integer(), name = character(), unit = character(),
                      trans = character(), stringsAsFactors = FALSE)
  starts <- .record_starts(lines, "THETA")
  record_lines <- which(.is_record_line(lines))
  rows <- list()
  for (s in starts) {
    e <- record_lines[record_lines > s][1]
    e <- if (is.na(e)) length(lines) else e - 1L
    for (i in s:e) {
      text <- lines[i]
      if (i == s) text <- sub("^\\s*\\$THETA\\s*", "", text, ignore.case = TRUE)
      if (!grepl("\\S", text) || grepl("^\\s*;", text)) next
      ann <- .parse_annotation(text)
      n <- .count_theta_values(ann$value)
      if (n == 0L) next
      rows[[length(rows) + 1L]] <- data.frame(
        line = i, on_record_line = i == s, n_on_line = n,
        name = c(ann$name, rep("", n - 1L)),
        unit = c(ann$unit, rep("", n - 1L)),
        trans = c(ann$trans, rep("", n - 1L)),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) return(empty)
  out <- do.call(rbind, rows)
  out$index <- seq_len(nrow(out))
  out[, names(empty)]
}


# Read a NONMEM control stream (.ctl/.mod), repairing non-UTF-8 lines. warn =
# FALSE because a file saved without a final newline is common and harmless,
# and model_report() drops a model on any warning.
#' @keywords internal
#' @noRd
.read_control_stream <- function(path) {
  .repair_encoding(readLines(path, warn = FALSE))
}


# Read a NONMEM listing as text. Listings are not guaranteed to be valid UTF-8 -
# a run that wrote PRDERR output can carry stray bytes. readLines() returns such
# lines without error, so they are repaired afterwards (.repair_encoding): the
# status markers are all ASCII and stay greppable whatever else the run
# emitted, and the diagnostic text callers quote (error_excerpt, the #TERM
# block) stays readable. A path that cannot be opened signals, which is what
# marks a run as failed.
#' @keywords internal
#' @noRd
.read_listing_lines <- function(path) {
  con <- file(path, open = "r")
  on.exit(close(con), add = TRUE)
  .repair_encoding(readLines(con, warn = FALSE, skipNul = TRUE))
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
  # A file stored as Windows-1252 is read with its lines repaired to UTF-8
  # (.read_control_stream); it is written back in Windows-1252 so saving a model
  # does not change the encoding of the user's file.
  if (file.exists(file_path) &&
      !all(validUTF8(readLines(file_path, warn = FALSE)))) {
    lines <- iconv(lines, from = "UTF-8", to = "CP1252", sub = "?")
  }
  writeLines(lines, file_path, useBytes = TRUE)
  cat("File saved to:", basename(file_path), "\n")
  return(search_state)
}



#' Read NONMEM EXT File
#'
#' @title Extract OFV and parameters from NONMEM .ext file
#' @description Reads NONMEM .ext file to extract parameter estimates and OFV.
#'   Handles multiple estimation methods by using the last -1000000000 line.
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

    # Remove TABLE headers and empty lines
    data_lines <- ext_lines[!grepl("^TABLE|^\\s*$", ext_lines)]

    if (length(data_lines) == 0) {
      return(list(
        found = FALSE,
        error = "No data lines in EXT file",
        ofv = NA_real_,
        parameters = NULL
      ))
    }

    # Find lines with -1000000000 (final estimates)
    # These lines contain the final parameter estimates and OFV
    final_lines_idx <- grep("^\\s*-1000000000", data_lines)

    if (length(final_lines_idx) > 0) {
      final_line <- data_lines[tail(final_lines_idx, 1)]

      # Parse the line into numeric values
      values <- as.numeric(strsplit(trimws(final_line), "\\s+")[[1]])

      # The last value is typically OFV
      ofv <- tail(values, 1)

      # Extract parameter estimates (excluding iteration number and OFV)
      # First value is iteration (-1000000000), last is OFV
      # Parameters are in between
      if (length(values) > 2) {
        parameters <- values[2:(length(values)-1)]
      } else {
        parameters <- numeric(0)
      }

    } else {
      # Fallback: No -1000000000 line found, try using last data line
      # This might happen with incomplete or non-standard files
      warning("No -1000000000 line found in EXT file, using last data line")

      last_line <- tail(data_lines, 1)
      values <- as.numeric(strsplit(trimws(last_line), "\\s+")[[1]])

      # Assume last value is OFV
      ofv <- tail(values, 1)

      # Extract parameters
      if (length(values) > 2) {
        parameters <- values[2:(length(values)-1)]
      } else {
        parameters <- numeric(0)
      }
    }

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
      ofv = NA_real_,
      parameters = NULL
    ))
  })
}



#' Read NONMEM LST File with Basic Error Detection
#'
#' @title Robust LST file reader with comprehensive error handling
#' @description Reads a model's .lst file and classifies the run status
#' @param model_path Character. Path to model directory or lst file
#' @return List with status and error information
#' @export
read_nonmem_lst <- function(model_path) {
  # Validate the model path
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

  model_path_str <- as.character(model_path)

  # Find the .lst file
  if (file.exists(model_path_str) && grepl("\\.lst$", model_path_str)) {
    lst_file <- model_path_str
  } else {
    model_name <- basename(model_path_str)
    # Validate the model name derived from the path
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
      has_issues = FALSE  # no LST file yet is not an "issue"
    ))
  }

  # Read and analyze .lst file with comprehensive error handling
  tryCatch({
    lst_content <- .read_listing_lines(lst_file)

    # Empty LST file
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
      has_issues = FALSE,
      nmtran_error = FALSE,
      has_stop_time = FALSE,
      lines = lst_content   # returned so callers need not read the listing again
    )

    # Generalized status markers for better portability across NONMEM versions.
    success_patterns <- c(
      "MINIMIZATION SUCCESSFUL",
      "OPTIMIZATION( WAS)? COMPLETED",
      "ESTIMATION STEP WAS COMPLETED"
    )
    hard_failure_patterns <- c(
      "MINIMIZATION TERMINATED",
      "PROGRAM TERMINATED BY OBJ",
      "TERMINATED DUE TO",
      "FATAL ERROR"
    )
    severe_warning_patterns <- c(
      "WARNING.*SEVERE",
      "NUMERICAL DIFFICULT",
      "SINGULAR"
    )
    stop_marker_patterns <- c("Stop Time:")
    # Keep runtime error patterns specific to avoid false positives from generic words.
    runtime_error_patterns <- c(
      "ERROR=",
      "ERROR IN",
      "PROGRAM TERMINATED",
      "ABORTING"
    )
    soft_end_patterns <- c("PROBLEM ENDED")
    # NM-TRAN rejected the control stream: NONMEM never runs, so the run has
    # failed whether or not nmfe went on to write "Stop Time:".
    nmtran_error_patterns <- c("AN ERROR WAS FOUND IN THE CONTROL STATEMENTS")

    pattern_hit <- function(patterns) {
      idx <- which(grepl(paste(patterns, collapse = "|"), lst_content, ignore.case = TRUE))
      list(found = length(idx) > 0, idx = idx)
    }

    success_hit <- pattern_hit(success_patterns)
    hard_failure_hit <- pattern_hit(hard_failure_patterns)
    severe_warning_hit <- pattern_hit(severe_warning_patterns)
    stop_hit <- pattern_hit(stop_marker_patterns)
    runtime_error_hit <- pattern_hit(runtime_error_patterns)
    soft_end_hit <- pattern_hit(soft_end_patterns)

    nmtran_error_hit <- pattern_hit(nmtran_error_patterns)

    has_successful <- success_hit$found
    has_hard_failure <- hard_failure_hit$found
    has_stop_time <- stop_hit$found
    result$has_stop_time <- has_stop_time

    # A run has finished only when nmfe has written "Stop Time:". Before that a
    # success or termination line may belong to an earlier $EST step, or the
    # run may still be in $COV, so neither decides the status yet. The one
    # exception is an NM-TRAN error, after which NONMEM cannot run at all.
    if (nmtran_error_hit$found) {
      result$status <- "failed"
      result$nmtran_error <- TRUE
      result$has_issues <- TRUE
      result$error_message <- trimws(lst_content[nmtran_error_hit$idx[1]])
      result$error_excerpt <- result$error_message

    } else if (!has_stop_time) {
      has_execution <- any(grepl("NONMEM EXECUTION", lst_content, ignore.case = FALSE))
      result$status <- "incomplete"
      result$has_issues <- !has_execution
      result$error_message <- if (has_execution) "Model still running" else "Model run incomplete"
      # Keep the last termination or error line seen so far as a diagnostic: it
      # does not decide the status, but it explains a run that never finished.
      marker_idx <- c(hard_failure_hit$idx, runtime_error_hit$idx)
      if (length(marker_idx) > 0) {
        result$error_excerpt <- trimws(lst_content[max(marker_idx)])
        result$error_message <- sprintf("%s (last marker: %s)", result$error_message,
                                        result$error_excerpt)
      }

    # Finished: explicit hard failures take precedence.
    } else if (has_hard_failure) {
      result$status <- "failed"
      result$has_issues <- TRUE
      result$error_message <- trimws(lst_content[hard_failure_hit$idx[1]])
      result$error_excerpt <- result$error_message

    } else if (has_successful) {
      result$status <- "completed"
      result$has_issues <- FALSE

    } else {
      # Finished with no hard failure and no success line.
      if (runtime_error_hit$found) {
        result$status <- "failed"
        result$has_issues <- TRUE
        result$error_message <- trimws(lst_content[runtime_error_hit$idx[1]])
        result$error_excerpt <- result$error_message
      } else {
        result$status <- "completed"
        if (severe_warning_hit$found || soft_end_hit$found) {
          result$has_issues <- TRUE
          result$error_message <- "Run completed with warning markers"
          marker_idx <- c(severe_warning_hit$idx, soft_end_hit$idx)
          if (length(marker_idx) > 0) {
            first_idx <- min(marker_idx)
            context_start <- max(1, first_idx - 2)
            context_end <- min(length(lst_content), first_idx + 2)
            result$error_excerpt <- paste(lst_content[context_start:context_end], collapse = "\n")
          }
        }
      }
    }


    return(result)

  }, error = function(e) {
    # Report the file that was resolved. Dropping it made a listing that exists
    # but cannot be read indistinguishable from one NONMEM has not written yet,
    # and callers that ask only for $file read that as "still running".
    return(list(
      found = TRUE,
      file = lst_file,
      status = "read_error",
      error_message = paste("Error reading LST file:", as.character(e$message)),
      error_excerpt = "",
      has_issues = TRUE
    ))
  })
}


#' Get Model Status from Files
#'
#' @title Determine overall model status with detailed error reporting
#' @description Classifies a model's run status from its output files, with
#'   detailed failure information
#' @param model_path Character. Path to model directory
#' @param require_cov_step Logical. Whether a successful covariance step (the
#'   presence of a \code{.cov} file) is required for success (default: TRUE).
#' @return Character. Overall model status
#' @export
get_model_status_from_files <- function(model_path, require_cov_step = TRUE) {

  # read_nonmem_lst() accepts the listing itself, but read_nonmem_ext() would
  # then look for "<model>.lst.ext" and find nothing, making a finished run read
  # as failed. Drop the extension so both readers search the same candidates,
  # which keeps the run-directory and flat layouts working alike.
  if (grepl("\\.lst$", model_path)) {
    model_path <- sub("\\.lst$", "", model_path)
  }

  lst_info <- read_nonmem_lst(model_path)
  ext_info <- read_nonmem_ext(model_path)

  if (!lst_info$found && !ext_info$found) {
    return("not_run")
  }

  # An OFV means nothing until the run has finished: read_nonmem_ext() falls back
  # to the last iteration line when there is no final estimate, so a model still
  # estimating reports a usable-looking value. Establish completion first, which
  # is also the order update_model_status_from_files() works in.
  if (!isTRUE(lst_info$found) || identical(lst_info$status, "incomplete")) {
    return("incomplete")
  }

  # A listing that exists but cannot be read is a failure - not a run in flight,
  # and not a success. Nothing in it can be trusted to describe the estimation,
  # so it must not fall through to the OFV test below and be called completed.
  if (identical(lst_info$status, "read_error")) {
    return("failed")
  }

  # NM-TRAN rejected the control stream, so NONMEM did not run: any .ext or
  # .cov in the folder is from an earlier run and must not make this a success.
  if (isTRUE(lst_info$nmtran_error)) {
    return("failed")
  }

  # Success is a usable OFV plus a covariance matrix - the same test
  # update_model_status_from_files() applies, so both readers agree. How
  # minimisation ended is deliberately not part of it: a run that stopped on
  # rounding errors but produced both is a success.
  ofv_ok <- isTRUE(ext_info$found) &&
    !is.null(ext_info$ofv) &&
    length(ext_info$ofv) > 0 &&
    !is.na(ext_info$ofv[1]) &&
    is.finite(ext_info$ofv[1]) &&
    abs(ext_info$ofv[1]) <= 1e10

  # Parameters parked at NONMEM's boundary marker are not a usable estimate.
  if (ofv_ok && !is.null(ext_info$parameters) &&
      any(abs(ext_info$parameters) >= 8.9999e5, na.rm = TRUE)) {
    ofv_ok <- FALSE
  }

  if (ofv_ok) {
    if (isTRUE(require_cov_step)) {
      # Anchor both the directory and the run's own name to the listing that was
      # actually found. model_path may be the run directory or the .lst itself
      # (read_nonmem_lst accepts either), and basename() of the latter would send
      # this looking for "<model>.lst.cov".
      cov_file <- file.path(
        dirname(lst_info$file),
        paste0(sub("\\.lst$", "", basename(lst_info$file)), ".cov")
      )
      if (!file.exists(cov_file)) {
        return("failed")
      }
    }
    return("completed")
  }

  # Without a usable OFV there is no result for the search to act on, however the
  # listing describes the end of estimation - a missing or unreadable .ext is the
  # case update_model_status_from_files() also reports as failed. Returning the
  # listing's own "completed" here would hand back a success that never met either
  # half of the test above.
  if (identical(lst_info$status, "completed")) {
    return("failed")
  }

  # Anything else the listing reported stands. "read_error" cannot reach here -
  # it returned above.
  return(lst_info$status)
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
    cov_tags <- names(search_state$tags)[grepl("^beta_", names(search_state$tags))]
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
#' @param lst_lines Character vector or NULL. The listing's lines when the caller
#'   has already read them; NULL (default) reads the listing here.
#' @return List with start_time and stop_time (POSIXct or NA)
#' @export
extract_nonmem_timestamps <- function(model_name, models_folder = "models", lst_lines = NULL) {

  if (is.null(lst_lines)) {
    # read_nonmem_lst() is the one resolver for where a model's output can be:
    # inside the run directory, or beside the control stream. It has read the
    # listing already, so its lines are reused.
    lst_probe <- read_nonmem_lst(file.path(models_folder, model_name))
    if (is.null(lst_probe$file) || !file.exists(lst_probe$file)) {
      return(list(
        start_time = NA,
        stop_time = NA,
        error = "LST file not found"
      ))
    }
    if (identical(lst_probe$status, "read_error")) {
      return(list(
        start_time = NA,
        stop_time = NA,
        error = lst_probe$error_message %||% "Cannot read LST file"
      ))
    }
    lst_lines <- lst_probe$lines
  }

  tryCatch({
    if (is.null(lst_lines)) lst_lines <- character(0)

    if (length(lst_lines) == 0) {
      return(list(
        start_time = NA,
        stop_time = NA,
        error = "Empty LST file"
      ))
    }

    # The %a and %b below are read in this session's LC_TIME, not the locale
    # NONMEM wrote in. Under a non-English LC_TIME none of the layouts match and
    # both timestamps come back NA with nothing signalled. Parse under C, where
    # the English abbreviations NONMEM writes always resolve, and restore after.
    old_lc_time <- Sys.getlocale("LC_TIME")
    on.exit(suppressWarnings(Sys.setlocale("LC_TIME", old_lc_time)), add = TRUE)
    suppressWarnings(Sys.setlocale("LC_TIME", "C"))

    # NONMEM stamps its start and stop lines in the host's locale, so the layout
    # varies between machines: "Wed Jul 29 14:30:08 EDT 2026" (month first,
    # 24-hour, zone before the year) and "Wed 29 Jul 2026 02:30:08 PM EDT" (day
    # first, 12-hour, trailing zone) are both produced. Try the known layouts and
    # take the first that yields a time. as.POSIXct() returns NA on a format
    # mismatch rather than signalling, so each candidate has to be tested.
    parse_nonmem_time <- function(line) {
      if (length(line) != 1L || is.na(line) || !nzchar(line)) return(NA)

      # The zone is a name ("EDT") or a numeric offset ("+03", "-0500").
      zone <- "([A-Z]{3,4}|[+-][0-9]{2,4})"
      candidates <- c(
        trimws(gsub(paste0(" ", zone, " "), " ", line)),            # zone mid-string
        trimws(sub(paste0("\\s+", zone, "\\s*$"), "", line))         # zone at the end
      )
      formats <- c(
        "%a %b %d %H:%M:%S %Y",     # Wed Jul 29 14:30:08 2026
        "%a %d %b %Y %I:%M:%S %p",  # Wed 29 Jul 2026 02:30:08 PM
        "%a %d %b %Y %H:%M:%S",     # Wed 29 Jul 2026 14:30:08
        "%a %b %d %I:%M:%S %p %Y"   # Wed Jul 29 02:30:08 PM 2026
      )

      for (candidate in unique(candidates)) {
        for (fmt in formats) {
          parsed <- suppressWarnings(as.POSIXct(candidate, format = fmt))
          if (!is.na(parsed)) return(parsed)
        }
      }
      NA
    }

    # Start time is the first line of the listing
    start_time <- parse_nonmem_time(trimws(lst_lines[1]))

    # Stop time is the line after the "Stop Time:" marker. Matched unanchored, the
    # same way update_model_status_from_files() tests for it, so a leading space
    # cannot make one function call the run finished and the other not.
    stop_time <- NA
    stop_time_idx <- grep("Stop Time:", lst_lines, fixed = TRUE)

    if (length(stop_time_idx) > 0) {
      stop_line_idx <- stop_time_idx[1] + 1
      if (stop_line_idx <= length(lst_lines)) {
        stop_time <- parse_nonmem_time(trimws(lst_lines[stop_line_idx]))
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

