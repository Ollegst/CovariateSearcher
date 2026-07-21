# =============================================================================
# UTILITIES
# File: R/utilities.R
# Part of CovariateSearcher Package
# Utility functions and helpers
# =============================================================================



#' Clean Directory - Remove NONMEM Working Files
#'
#' @title Remove all files starting with "WK_" from all model subdirectories
#' @param models_folder Character. Path to models directory (default: "models")
#' @return Number of files deleted
#' @export
clean_dir <- function(models_folder = "models") {

  # Get all model subdirectories (run1, run2, etc.)
  model_dirs <- list.dirs(models_folder, recursive = FALSE)

  if (length(model_dirs) == 0) {
    cat("No model directories found\n")
    return(0)
  }

  total_deleted <- 0

  # Process each model directory
  for (model_dir in model_dirs) {
    wk_files <- list.files(
      path = model_dir,
      pattern = "^WK_",
      full.names = TRUE
    )

    if (length(wk_files) > 0) {
      deleted <- sum(file.remove(wk_files))
      total_deleted <- total_deleted + deleted
    }
  }

  if (total_deleted == 0) {
    cat("No WK_ files found\n")
  } else {
    cat(sprintf("Deleted %d WK_ files from %d model directories\n", total_deleted, length(model_dirs)))
  }

  return(total_deleted)
}

#' Extract Covariate Name from Tag
#'
#' @title Parse covariate name from tag string
#' @description Extracts the covariate name from a tag in the format "beta_COVARIATE_PARAMETER"
#'   (e.g., "beta_AGE_CL" → "AGE", "beta_WT_V" → "WT")
#' @param tag Character. Tag string in format "beta_COVARIATE_PARAMETER"
#' @return Character. Covariate name, or NA if parsing fails
#' @examples
#' extract_covariate_name_from_tag("beta_AGE_CL")  # Returns "AGE"
#' extract_covariate_name_from_tag("beta_WT_V")    # Returns "WT"
#' extract_covariate_name_from_tag("beta_SEX_CL")  # Returns "SEX"
#' @export
extract_covariate_name_from_tag <- function(tag) {

  if (is.na(tag) || !is.character(tag) || length(tag) != 1) {
    return(NA_character_)
  }

  # Fixed-slot tag: beta_<COV>_<PARAM>[_<theta names> | _<level>]. COVARIATE is
  # always parts[2] (COV/PARAM are single tokens, enforced at validation), so this
  # works for the base tag (beta_COV_PARAM), a categorical per-level tag
  # (beta_COV_PARAM_LEVEL) and a multi-theta tag (beta_COV_PARAM_THETANAME).
  parts <- strsplit(tag, "_")[[1]]

  if (length(parts) < 3) {
    warning(sprintf("Tag format unexpected: %s (expected beta_COVARIATE_PARAMETER...)", tag))
    return(NA_character_)
  }

  return(parts[2])
}


#' Standardised checkpoint filename: \code{NN_phase_event.rds}
#'
#' @description Zero-padded step number first so the \code{scm_rds/} folder sorts
#'   chronologically and the highest \code{NN} is the latest checkpoint.
#'   \code{phase} is "forward"/"backward"/"redemption"/"final"; \code{event} is
#'   "created" (models made, pre-submit), "done" (step evaluated), "complete"
#'   (phase/run end), "running" (mid-step snapshot) or "error".
#' @param step Step number (coerced to a zero-padded integer; NA/non-finite → 00).
#' @param phase,event Character labels (see description).
#' @return A filename string like \code{"02_forward_done.rds"}.
#' @keywords internal
.scm_checkpoint_name <- function(step, phase, event) {
  n <- suppressWarnings(as.integer(step))
  if (length(n) != 1L || is.na(n)) n <- 0L
  sprintf("%02d_%s_%s.rds", n, phase, event)
}


#' Accept an input as either an in-memory object or a path to load
#'
#' @description Lets a function argument be given as the object directly OR as a
#'   path to a file that holds it, consistently across the package. If \code{x}
#'   is a single character string it is treated as a path and loaded by
#'   extension (\code{.csv} via \code{readr::read_csv}, \code{.rds} via
#'   \code{readRDS}); anything else (a data.frame, list, ...) is returned
#'   unchanged. Model files are intentionally NOT handled here - models are
#'   referenced by name against \code{models_folder}.
#' @param x The object, or a length-1 character path to a \code{.csv}/\code{.rds}.
#' @param what Character label for this input, used in error messages.
#' @return The in-memory object (loaded if a path was given).
#' @keywords internal
#' @noRd
.load_if_path <- function(x, what = "input") {
  if (!is.character(x) || length(x) != 1L) return(x)   # already an object
  if (!file.exists(x)) {
    stop(sprintf("`%s` is a path but the file does not exist: %s", what, x),
         call. = FALSE)
  }
  ext <- tolower(tools::file_ext(x))
  switch(ext,
    csv = readr::read_csv(x, show_col_types = FALSE),
    rds = readRDS(x),
    stop(sprintf("`%s`: cannot load a '.%s' file (supported: .csv, .rds): %s",
                 what, ext, x), call. = FALSE)
  )
}


#' Convert P-Value to Chi-Square ΔOFV Threshold
#'
#' @title Calculate ΔOFV threshold from p-value for likelihood ratio test
#' @description Converts a p-value to the corresponding chi-square ΔOFV threshold
#'   for model comparison in stepwise covariate modeling. ΔOFV follows a chi-square
#'   distribution with degrees of freedom equal to the difference in parameters.
#'
#'   For covariate modeling:
#'   - Continuous covariates: df = 1 (one parameter added)
#'   - Categorical covariates: df = number of levels - 1
#'
#' @param p_value Numeric. Significance level (e.g., 0.05, 0.01)
#' @param df Integer. Degrees of freedom for chi-square test (default: 1).
#'   \code{df = 0} (a fully-FIX covariate that adds no estimated parameter) returns
#'   a threshold of 0, so the covariate is selected by direct OFV comparison
#'   (kept if the model improves, i.e. any ΔOFV > 0).
#' @return Numeric. Chi-square critical value (ΔOFV threshold); 0 when df = 0.
#' @examples
#' # Standard forward selection (p = 0.05, df = 1)
#' pvalue_to_threshold(0.05, df = 1)  # Returns 3.84
#'
#' # Standard backward elimination (p = 0.01, df = 1)
#' pvalue_to_threshold(0.01, df = 1)  # Returns 6.63
#'
#' # Categorical covariate with 3 levels (df = 2)
#' pvalue_to_threshold(0.05, df = 2)  # Returns 5.99
#' @export
pvalue_to_threshold <- function(p_value, df = 1) {

  # Input validation
  if (!is.numeric(p_value) || length(p_value) != 1) {
    stop("p_value must be a single numeric value")
  }
  if (p_value <= 0 || p_value >= 1) {
    stop("p_value must be between 0 and 1")
  }
  if (!is.numeric(df) || length(df) != 1 || df < 0) {
    stop("df must be a non-negative integer")
  }

  # df == 0: a fully-FIX covariate adds no estimated parameter, so it is judged by
  # direct OFV comparison (kept if the model improves) -> threshold 0, i.e. any
  # ΔOFV > 0 counts. Consistent with qchisq(1 - p, df) -> 0 as df -> 0.
  if (df == 0) {
    return(0)
  }

  # Convert p-value to chi-square critical value
  threshold <- qchisq(1 - p_value, df = df)

  return(threshold)
}


#' Calculate Degrees of Freedom for Covariate
#'
#' @title Determine degrees of freedom for a covariate's likelihood-ratio test
#' @description Degrees of freedom = the number of ESTIMATED (non-FIX) parameters
#'   the covariate adds:
#'   \itemize{
#'     \item per-level categorical (cat.linear): number of levels - 1;
#'     \item single-factor forms (continuous power/linear/exponential, cat.power,
#'           and user expressions): the number of non-FIX thetas the formula
#'           declares (1 for the built-ins; N for an N-parameter expression such as
#'           \code{EMAX*cov/(EC50+cov)}), minus any marked \code{FIX} in \code{INIT}.
#'   }
#'   A fully-FIX covariate has df 0 (it adds no estimated parameter): it is then
#'   selected by DIRECT OFV comparison — \code{pvalue_to_threshold(df = 0)} returns
#'   a threshold of 0, so it is kept whenever the model improves (any ΔOFV > 0).
#' @param covariate_name Character. Name of the covariate
#' @param covariate_search Data frame. Covariate search configuration
#' @return Integer. Degrees of freedom (0 for a fully-FIX covariate).
#' @export
calculate_covariate_df <- function(covariate_name, covariate_search) {

  # Find covariate in search table
  cov_row <- covariate_search[covariate_search$COVARIATE == covariate_name, ]

  if (nrow(cov_row) == 0) {
    warning(sprintf("Covariate %s not found in covariate_search, defaulting to df=1",
                    covariate_name))
    return(1L)
  }

  # Get first matching row (in case of multiple parameters)
  cov_row <- cov_row[1, ]

  status  <- tolower(as.character(cov_row$STATUS))
  formula <- as.character(cov_row$FORMULA)
  def     <- tryCatch(get_covariate_formula(status, formula), error = function(e) NULL)
  if (is.null(def)) {
    warning(sprintf("Unknown formula for covariate %s, defaulting to df=1", covariate_name))
    return(1L)
  }

  table_init <- if ("INIT" %in% names(cov_row)) as.character(cov_row$INIT) else NA_character_

  # Per-level categorical (cat.linear): one theta per non-reference level.
  if (isTRUE(def$categorical)) {
    levels_str <- cov_row$LEVELS
    if (is.na(levels_str) || trimws(levels_str) == "") {
      warning(sprintf("Categorical covariate %s has no levels specified, defaulting to df=1",
                      covariate_name))
      return(1L)
    }
    n_levels <- length(strsplit(levels_str, ";")[[1]])
    init_fix <- !is.na(table_init) && trimws(table_init) != "" &&
                grepl("FIX", table_init, ignore.case = TRUE)
    df <- if (init_fix) 0L else (n_levels - 1L)
    return(as.integer(df))
  }

  # Single-factor form: count the non-FIX thetas the formula declares. Built-in
  # power/linear/exponential/cat.power declare no theta_names -> they are single
  # theta; an expression declares one name per estimated parameter.
  theta_names <- def$theta_names
  if (is.null(theta_names) || length(theta_names) == 0L) {
    n_thetas  <- 1L
    init_vals <- if (!is.na(table_init) && trimws(table_init) != "") table_init else (def$init %||% "0.1")
  } else {
    n_thetas  <- length(theta_names)
    init_vals <- if (n_thetas == 1L) {
      if (!is.na(table_init) && trimws(table_init) != "") table_init else "0.1"
    } else {
      parse_named_init(table_init, theta_names)
    }
  }
  n_fix <- sum(grepl("FIX", init_vals, ignore.case = TRUE))
  df    <- n_thetas - n_fix
  return(as.integer(df))
}


#' Decode Categorical Columns Using a Variable Specification
#'
#' @description
#' Replaces the numeric codes of one or more categorical columns with their
#' decoded labels, using each column's `values` -> `decode` mapping from a
#' variable specification. Every decoded column becomes a `factor` whose levels
#' are the `decode` labels in spec order, so downstream plots and tables show
#' readable categories instead of raw codes.
#'
#' The specification may be a loaded \pkg{yspec} object (from
#' [yspec::ys_load()]) **or** a plain list read from a spec YAML with
#' [yaml::read_yaml()]. In both cases the fields are read with `$`
#' (`spec[[col]]$values` / `$decode`): on a yspec column that resolves the yspec
#' accessor, and on a raw list it reads the list element - so the same call
#' works for either input. (Note: routing the same fields through `as.list()`
#' first does **not** work on a yspec column - it bypasses the accessor and the
#' `values`/`decode` pairing shifts.)
#'
#' @param data A data frame containing the columns to decode.
#' @param yaml_file The variable specification: a loaded yspec object, or a raw
#'   list from [yaml::read_yaml()]. Each entry keyed by column name is expected
#'   to carry `values` (the numeric codes) and `decode` (the matching labels).
#' @param column_names Character vector of column names in `data` to decode. A
#'   name whose spec entry lacks `values`/`decode`, or that is not a column of
#'   `data`, is left unchanged.
#'
#' @return `data`, with each requested column replaced by a `factor` of its
#'   `decode` labels (levels in spec order). Any code not present in `values`
#'   becomes `NA`.
#'
#' @examples
#' \dontrun{
#' spec  <- yspec::ys_load(here::here("data", "spec", "lookup.yml"))
#' flags <- yspec::pull_meta(spec, "flags")
#' dat   <- decode_dataset(dat, spec, c(flags$catcov))
#' }
#' @export
decode_dataset <- function(data, yaml_file, column_names) {
  # Read the spec/lookup entries by name.
  meta <- yaml_file

  # Decode each requested column that has a values -> decode mapping.
  for (col_name in column_names) {
    # `$values` / `$decode` are read directly (not via as.list()): this resolves
    # the yspec accessor for a ys_load object and the list field for a raw yaml
    # list alike, keeping the two vectors aligned.
    if (!is.null(meta[[col_name]]$decode) && !is.null(meta[[col_name]]$values)) {
      # Only touch columns actually present in the data.
      if (col_name %in% names(data)) {
        mapping_values <- meta[[col_name]]$values   # original numeric codes
        mapping_decode <- meta[[col_name]]$decode   # corresponding decoded labels

        # match() maps each data value to its position in `values`; index
        # `decode` to get the label. Level order follows `decode` so the factor
        # keeps the spec's category order (codes not in `values` -> NA).
        data[[col_name]] <- factor(
          mapping_decode[match(data[[col_name]], mapping_values)],
          levels = mapping_decode
        )
      }
    }
  }

  return(data)
}

