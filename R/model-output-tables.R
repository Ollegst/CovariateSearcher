# =============================================================================
# MODEL OUTPUT TABLES
# File: R/model-output-tables.R
# Part of CovariateSearcher Package
# Functions for creating formatted model parameter tables
# =============================================================================

#' Apply PPS Theme to Flextable
#'
#' @description Internal function to apply consistent formatting to flextables
#' @param x A flextable object
#' @return A formatted flextable object with PPS theme applied
#' @keywords internal
#' @export
theme_pps_table <- function(x) {
  # Set global defaults
  flextable::set_flextable_defaults(big.mark = "")

  # Define borders
  big_border <- officer::fp_border(color = "black", width = 1)
  std_border <- officer::fp_border(color = "gray", width = 0.5)

  # Typography settings
  font_name_table <- "Times New Roman"
  font_size_table <- 10

  if (!inherits(x, "flextable")) {
    stop("theme_pps_table supports only flextable objects.")
  }

  # Apply borders
  x <- flextable::border_remove(x)
  x <- flextable::border_inner(x, part = "all", border = std_border)
  x <- flextable::border_outer(x, part = "all", border = std_border)
  x <- flextable::hline_bottom(x, part = "header", border = big_border)
  x <- flextable::hline_bottom(x, part = "body", border = big_border)

  # Apply text formatting
  x <- flextable::bold(x = x, bold = TRUE, part = "header")
  x <- flextable::fix_border_issues(x, part = "all")

  # Apply font sizes
  x <- flextable::fontsize(x = x, size = font_size_table, part = "header")
  x <- flextable::fontsize(x = x, size = font_size_table, part = "body")
  x <- flextable::fontsize(x = x, size = font_size_table - 1, part = "footer")

  # Apply font and alignment
  x <- flextable::font(x = x, fontname = font_name_table, part = "all")
  x <- flextable::align(x = x, i = 1, j = c(-1), align = "center", part = "header")
  x <- flextable::set_table_properties(x = x, layout = "autofit", width = 1)

  return(x)
}


#' Extract Parameters from NONMEM Control File
#'
#' @description Internal function to extract parameter definitions from control file blocks
#' @param lines Character vector of control file lines
#' @param block_tag Character string identifying the block (e.g., "THETA", "OMEGA")
#' @param remove_prefix Logical. Remove numeric prefix from parameter names (default: FALSE)
#' @return A tibble with parameter names and transformations
#' @keywords internal
extract_params <- function(lines, block_tag, remove_prefix = FALSE) {

  # Find all indices where the block starts
  block_starts <- which(stringr::str_detect(
    lines,
    stringr::regex(paste0("^\\$", block_tag), ignore_case = TRUE)
  ))

  # Process each block header found
  params <- purrr::map_dfr(block_starts, function(start_idx) {

    # Extract the remainder of the header line (after the $TAG token)
    header_remainder <- stringr::str_remove(
      lines[start_idx],
      stringr::regex(paste0("^\\$", block_tag, "\\S*\\s*"), ignore_case = TRUE)
    )

    # Get the lines after the header
    sub_lines <- if (start_idx < length(lines)) {
      lines[(start_idx + 1):length(lines)]
    } else {
      character(0)
    }

    # The block ends when the next line starting with "$" is encountered
    end_idx <- dplyr::if_else(
      any(stringr::str_detect(sub_lines, "^\\$")),
      min(which(stringr::str_detect(sub_lines, "^\\$"))) - 1,
      length(sub_lines)
    )

    block_lines <- if (length(sub_lines) > 0 && end_idx > 0) {
      sub_lines[1:end_idx]
    } else {
      character(0)
    }

    # Prepend the header remainder if it contains content
    if (stringr::str_detect(header_remainder, "\\S")) {
      block_lines <- c(header_remainder, block_lines)
    }

    # Remove blank lines
    block_lines <- block_lines %>%
      purrr::keep(~ stringr::str_detect(.x, "\\S"))

    # If nothing left, return empty tibble
    if (length(block_lines) == 0) {
      return(tibble::tibble(param = character(0), trans = character(0)))
    }

    # Create a tibble and split each line by semicolon
    tibble::tibble(line = block_lines) %>%
      tidyr::separate(
        line,
        into = c("field1", "param", "field3", "transformation"),
        sep = ";",
        fill = "right",
        extra = "drop"
      ) %>%
      dplyr::mutate(
        param = stringr::str_trim(param),
        trans = stringr::str_trim(transformation)
      ) %>%
      dplyr::mutate(
        param = if (remove_prefix) stringr::str_remove(param, "^[0-9]+_") else param
      ) %>%
      dplyr::filter(param != "") %>%
      dplyr::select(param, trans)
  })

  return(params)
}


#' Extract Model Parameters from Control File
#'
#' @description Internal function to extract all parameter names from a NONMEM control file
#' @param model_name Character string. Name of the model (without .ctl extension)
#' @param models_folder Character string. Path to models folder (default: "models")
#' @return A list containing THETAS, OMEGAS, and SIGMA parameters with transformations
#' @keywords internal
extract_model_params <- function(model_name, models_folder = "models") {
  # Construct file path (supports both .ctl and .mod)
  ctl_path <- find_model_file(file.path(models_folder, model_name))

  if (is.null(ctl_path)) {
    stop(sprintf("Control file not found for: %s (.ctl or .mod)", model_name))
  }

  # Read the file as a vector of lines
  lines <- readLines(ctl_path)

  # Extract theta parameter names (removing numeric prefixes)
  theta_params <- extract_params(lines, "THETA", remove_prefix = TRUE)

  # Extract omega parameter names (keeping the names as-is)
  omega_params <- extract_params(lines, "OMEGA", remove_prefix = FALSE)

  # Extract sigma parameter names
  sigma_params <- extract_params(lines, "SIGMA", remove_prefix = FALSE)

  # Return a named list with the results
  list(
    THETAS = theta_params,
    OMEGAS = omega_params,
    SIGMA = sigma_params
  )
}


#' Calculate Condition Number from NONMEM .cor/.cov File
#'
#' @description Internal function to compute condition number from NONMEM
#' correlation/covariance matrix output. Prefers .cor, falls back to .cov.
#' @param model_number Character string. Model name/number
#' @param models_folder Character string. Path to models folder
#' @param tolerance Numeric tolerance for near-zero/non-positive eigenvalues
#' @return Numeric condition number, Inf for near-singular matrices, or NA if
#' computation is not possible
#' @keywords internal
calculate_condition_number<- function(model_number,
                                      models_folder = "models",
                                      tolerance = 1e-10) {
  model_path <- file.path(models_folder, model_number)
  matrix_file <- NA_character_
  if (file.exists(file.path(model_path, paste0(model_number, ".cor")))) {
    matrix_file <- file.path(model_path, paste0(model_number, ".cor"))
  } else if (file.exists(file.path(model_path, paste0(model_number, ".cov")))) {
    matrix_file <- file.path(model_path, paste0(model_number, ".cov"))
  }

  if (is.na(matrix_file)) {
    return(NA_real_)
  }

  lines <- readLines(matrix_file, warn = FALSE)

  # --- keep only the last TABLE block ---
  table_starts <- which(stringr::str_detect(lines, "^TABLE NO"))
  if (length(table_starts) > 0) {
    last_start <- table_starts[length(table_starts)]
    lines <- lines[(last_start + 1):length(lines)]
  }

  # --- drop the column-header row (NAME THETA1 THETA2 ...) ---
  lines <- lines[!stringr::str_detect(lines, "^\\s*NAME\\s")]

  # --- keep only rows that start with a parameter label ---
  data_rows_mask <- stringr::str_detect(
    lines,
    "^\\s*(THETA|OMEGA|SIGMA)"
  )
  data_lines <- lines[data_rows_mask]

  if (length(data_lines) == 0) {
    return(NA_real_)
  }

  # --- strip the first token (the row label) before number extraction ---
  data_lines <- stringr::str_replace(
    data_lines,
    "^\\s*\\S+\\s+",
    ""
  )

  numeric_tokens <- unlist(
    stringr::str_extract_all(
      data_lines,
      "[-+]?(?:\\d*\\.?\\d+)(?:[Ee][-+]?\\d+)?"
    )
  )
  values <- suppressWarnings(as.numeric(numeric_tokens))
  values <- values[is.finite(values)]

  if (length(values) == 0) {
    return(NA_real_)
  }

  # NONMEM matrix files can be either full square matrices or lower-triangular.
  n_vals <- length(values)
  square_n <- sqrt(n_vals)
  matrix_obj <- NULL

  if (abs(square_n - round(square_n)) < .Machine$double.eps^0.5) {
    n <- as.integer(round(square_n))
    matrix_obj <- matrix(values, nrow = n, byrow = TRUE)
  } else {
    tri_n <- (sqrt(8 * n_vals + 1) - 1) / 2
    if (abs(tri_n - round(tri_n)) < .Machine$double.eps^0.5) {
      n <- as.integer(round(tri_n))
      matrix_obj <- matrix(0, nrow = n, ncol = n)
      matrix_obj[lower.tri(matrix_obj, diag = TRUE)] <- values
      matrix_obj <- matrix_obj + t(matrix_obj) - diag(diag(matrix_obj))
    }
  }

  if (is.null(matrix_obj) || nrow(matrix_obj) == 0) {
    return(NA_real_)
  }

  is_zeroish_row <- function(x) {
    all(!is.finite(x) | abs(x) <= tolerance)
  }

  keep_idx <- which(!apply(matrix_obj, 1, is_zeroish_row))
  if (length(keep_idx) == 0) {
    return(NA_real_)
  }

  matrix_obj <- matrix_obj[keep_idx, keep_idx, drop = FALSE]
  matrix_obj <- (matrix_obj + t(matrix_obj)) / 2

  eig_values <- tryCatch(
    eigen(matrix_obj, symmetric = TRUE, only.values = TRUE)$values,
    error = function(e) NULL
  )

  if (is.null(eig_values)) {
    return(NA_real_)
  }

  eig_values <- eig_values[is.finite(eig_values)]
  if (length(eig_values) == 0) {
    return(NA_real_)
  }

  eig_nonneg <- eig_values[eig_values >= -tolerance]
  if (length(eig_nonneg) == 0) {
    return(NA_real_)
  }

  max_ev <- max(eig_nonneg)
  min_ev <- min(eig_nonneg)

  if (max_ev <= tolerance) {
    return(NA_real_)
  }

  if (min_ev <= tolerance) {
    return(Inf)
  }

  max_ev / min_ev
}


#' Decode a categorical covariate level to its human-readable label
#'
#' @description Internal helper. Given a covariate name and a numeric level, returns
#'   the decoded category name from a `lookup.yaml`-shaped list (each covariate entry
#'   carrying `values` and `decode`). Returns `NA_character_` if no usable decode is
#'   available, so callers can fall back to the generic "level N" label.
#' @param cov_name Character string. Covariate name (e.g. "SEXN").
#' @param level Character/numeric. The level value as parsed from the THETA name.
#' @param lookup List or NULL. Lookup spec keyed by covariate name.
#' @return Decoded category name, or `NA_character_` when not decodable.
#' @keywords internal
decode_cov_level <- function(cov_name, level, lookup) {
  if (is.null(lookup) || is.null(lookup[[cov_name]])) return(NA_character_)
  entry <- lookup[[cov_name]]
  if (is.null(entry$values) || is.null(entry$decode)) return(NA_character_)
  idx <- match(as.character(level), as.character(unlist(entry$values)))
  if (is.na(idx) || idx > length(entry$decode)) return(NA_character_)
  as.character(entry$decode[[idx]])
}


#' Name a covariate the way its spec does
#'
#' @description Internal helper. A covariate tag carries the dataset's column
#'   name (`ASIAN`), which is the join key everywhere but rarely how the
#'   covariate should read in a table (`Race2`). Takes `short` from the lookup
#'   entry, then its `label`, then `short` from `spec_pk`, and keeps the column
#'   name when none of them spells one out - the same order
#'   `create_covariate_table()` resolves its own covariate labels in.
#' @param cov_name Character string. Covariate column name, as parsed from the
#'   `beta_` tag.
#' @param lookup List or NULL. Lookup spec keyed by covariate name.
#' @param spec_pk List or NULL. Parameter/dataset spec keyed by column name.
#' @return Character string. Never `NA` - it falls back to `cov_name`.
#' @keywords internal
#' @noRd
.cov_display_name <- function(cov_name, lookup = NULL, spec_pk = NULL) {
  usable <- function(v) {
    !is.null(v) && length(v) >= 1 && !is.na(v[1]) && nzchar(as.character(v)[1])
  }
  field <- function(entry, name) {
    v <- tryCatch(entry[[name]], error = function(e) NULL)
    if (is.null(v)) v <- tryCatch(`$`(entry, name), error = function(e) NULL)
    v
  }

  entry <- if (!is.null(lookup)) lookup[[cov_name]] else NULL
  if (!is.null(entry)) {
    for (nm in c("short", "label")) {
      v <- field(entry, nm)
      if (usable(v)) return(as.character(v)[1])
    }
  }

  spec_entry <- if (!is.null(spec_pk)) {
    tryCatch(spec_pk[[cov_name]], error = function(e) NULL)
  } else {
    NULL
  }
  if (!is.null(spec_entry)) {
    v <- field(spec_entry, "short")
    if (usable(v)) return(as.character(v)[1])
  }

  cov_name
}


#' Put an estimation-scale value onto the reported scale
#'
#' @description Internal helper. The reported value of a parameter is not always
#'   what NONMEM estimated: a `;LOG` theta is exponentiated, and an OMEGA diagonal
#'   is shown as a CV percentage. Applying the same mapping to an estimate and to
#'   the bounds of its confidence interval keeps the two in the same units.
#'   All three transformations are monotone increasing, so a lower bound stays a
#'   lower bound.
#' @param value Numeric vector on NONMEM's estimation scale.
#' @param random_effect_sd Numeric vector from `bbr::param_estimates()`; `NA`
#'   marks a THETA.
#' @param trans Character vector of control-stream transformations ("LOG", "RATIO").
#' @param diag Logical vector. `TRUE` for diagonal OMEGA/SIGMA elements.
#' @return Numeric vector on the reported scale.
#' @keywords internal
#' @noRd
.to_report_scale <- function(value, random_effect_sd, trans, diag) {
  dplyr::case_when(
    is.na(random_effect_sd) & trans == "RATIO" ~ value,
    is.na(random_effect_sd) & trans == "LOG" ~ exp(value),
    diag == TRUE ~ 100 * sqrt(ifelse(exp(value) - 1 >= 0, exp(value) - 1, NA)),
    diag == FALSE ~ value
  )
}


#' Format a confidence interval as `[low, high]`
#'
#' @param low,high Numeric vectors of interval bounds.
#' @param digits Significant digits to keep (default 3).
#' @return Character vector; `NA_character_` where either bound is missing.
#' @keywords internal
#' @noRd
.format_ci <- function(low, high, digits = 3) {
  as_str <- function(x) {
    vapply(x, function(v) {
      if (is.na(v)) NA_character_
      else format(signif(v, digits), trim = TRUE, scientific = FALSE)
    }, character(1))
  }
  ifelse(is.na(low) | is.na(high), NA_character_,
         paste0("[", as_str(low), ", ", as_str(high), "]"))
}


#' Re-punctuate a bootstrap confidence interval as `[low, high]`
#'
#' @description Bootstrap summaries carry the interval as one pre-formatted
#'   string, conventionally hyphen-separated (`"[65.2-81.1]"`), which is
#'   ambiguous once a bound is negative (`"[-0.23--0.15]"`). The number pattern
#'   cannot itself match a separating hyphen, so the split is unambiguous.
#'   A string that does not parse is passed through untouched rather than dropped.
#' @param x Character vector of intervals, hyphen- or comma-separated, with or
#'   without enclosing brackets.
#' @return Character vector of `"[low, high]"` strings.
#' @keywords internal
#' @noRd
.reformat_boot_ci <- function(x) {
  x <- trimws(as.character(x))
  num <- "-?[0-9]*\\.?[0-9]+(?:[eE][+-]?[0-9]+)?"
  pattern <- paste0("^\\[?\\s*(", num, ")\\s*[,-]\\s*(", num, ")\\s*\\]?$")
  matched <- regmatches(x, regexec(pattern, x))

  vapply(seq_along(x), function(i) {
    parts <- matched[[i]]
    if (length(parts) != 3) return(x[i])
    paste0("[", parts[2], ", ", parts[3], "]")
  }, character(1))
}


#' Attach bootstrap median and confidence interval to a parameter table
#'
#' @description Matches `boot_results` to the report rows on NONMEM's own
#'   parameter names (`THETA1`, `OMEGA(1,1)`, ...), which `get_param2()` carries
#'   in `nonmem_name`. Matching by name rather than by position survives the rows
#'   the report drops (fixed `SIGMA(1,1)`, zero off-diagonals) and a bootstrap
#'   table ordered `THETA10` before `THETA2`.
#' @param param_est Data frame from `get_param2(include_ci = TRUE)`.
#' @param boot_results Data frame of bootstrap summaries. Column names are
#'   matched case-insensitively: the parameter name from
#'   `parameter_names`/`parameter`/`param`, the central value from
#'   `median`/`estimate`/`value`, and the interval from the first column whose
#'   name contains "CI".
#' @return `param_est` with `boot_median` and `boot_ci` columns added.
#' @keywords internal
#' @noRd
.join_bootstrap_results <- function(param_est, boot_results) {
  boot <- as.data.frame(boot_results, stringsAsFactors = FALSE)
  nm <- names(boot)

  pick <- function(candidates) {
    hit <- which(tolower(nm) %in% tolower(candidates))
    if (length(hit) > 0) nm[hit[1]] else NA_character_
  }
  name_col <- pick(c("parameter_names", "parameter_name", "parameter", "param"))
  med_col <- pick(c("median", "estimate", "value"))
  ci_hits <- grep("ci", nm, ignore.case = TRUE, value = TRUE)
  ci_col <- if (length(ci_hits) > 0) ci_hits[1] else NA_character_

  missing_cols <- c(
    if (is.na(name_col)) "parameter name (parameter_names/parameter/param)",
    if (is.na(med_col)) "central value (median/estimate/value)",
    if (is.na(ci_col)) "confidence interval (a column whose name contains 'CI')"
  )
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "`boot_results` is missing the %s column(s). Columns found: %s",
      paste(missing_cols, collapse = "; "), paste(nm, collapse = ", ")
    ), call. = FALSE)
  }

  keys <- trimws(as.character(boot[[name_col]]))
  if (anyDuplicated(keys) > 0) {
    stop(sprintf(
      "`boot_results` holds more than one row for: %s",
      paste(unique(keys[duplicated(keys)]), collapse = ", ")
    ), call. = FALSE)
  }

  boot_tbl <- data.frame(
    nonmem_name = keys,
    boot_median = round(suppressWarnings(as.numeric(boot[[med_col]])), 2),
    boot_ci = .reformat_boot_ci(boot[[ci_col]]),
    stringsAsFactors = FALSE
  )

  unmatched <- setdiff(keys, stats::na.omit(param_est$nonmem_name))
  if (length(unmatched) > 0) {
    cat(sprintf(
      "  Bootstrap parameters with no row in the report (not shown): %s\n",
      paste(unmatched, collapse = ", ")
    ))
  }

  dplyr::left_join(param_est, boot_tbl, by = "nonmem_name")
}


#' Get Model Parameters and Statistics
#'
#' @description Internal function to extract and format model parameters with estimates and statistics
#' @param model_number Character string. Model name/number
#' @param count_model Integer. Number of models being compared
#' @param shrinkage Character string. Type of shrinkage to report ("etasd", "etavr", "ebvsd", "ebvvr")
#' @param models_folder Character string. Path to models folder
#' @param spec_pk List. Optional parameter specifications with labels and units
#' @param lookup List. Optional covariate lookup spec (each entry carrying `values`
#'   and `decode`) used to decode categorical covariate levels in labels
#' @param include_ci Logical. Additionally return `nonmem_name` (NONMEM's own
#'   parameter name, kept as a join key for bootstrap results) and `CI`, the
#'   asymptotic interval `estimate ± 1.96 * SE` mapped onto the reported scale.
#'   `FALSE` (default) leaves the returned columns exactly as they were.
#' @return A formatted data frame with model parameters and statistics
#' @keywords internal
get_param2 <- function(model_number,
                       count_model,
                       shrinkage = "etasd",
                       models_folder = "models",
                       spec_pk = NULL,
                       lookup = NULL,
                       include_ci = FALSE) {

  # NULL/NA reads as "not asked for", so the flag can be threaded through from a
  # caller's own optional argument without a guard at every call site.
  include_ci <- isTRUE(include_ci)

  # Read model using bbr
  model_path <- file.path(models_folder, model_number)
  model_obj <- bbr::read_model(model_path)
  model_stats <- bbr::model_summary(model_obj)

  # Extract shrinkage values based on type
  shrinkage_value <- dplyr::case_when(
    shrinkage == "etasd" ~ c(
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$eta_sd,
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$eps_sd
    ),
    shrinkage == "etavr" ~ c(
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$eta_vr,
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$eps_vr
    ),
    shrinkage == "ebvsd" ~ c(
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$ebv_sd,
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$eps_sd
    ),
    shrinkage == "ebvvr" ~ c(
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$ebv_vr,
      tail(model_stats$shrinkage_details, 1)[[1]][[1]]$eps_vr
    )
  )

  # Extract OFV
  OFV <- data.table::data.table(
    parameter_names = "OFV",
    Parameter = tail(model_stats$ofv, 1)[[1]]$ofv_no_constant
  )

  # Extract condition number from NONMEM matrix files (.cor preferred, .cov fallback)
  COND_NUM <- data.table::data.table(
    parameter_names = "Conditional number",
    Parameter = calculate_condition_number(
      model_number = model_number,
      models_folder = models_folder,
      tolerance = 1e-10
    )
  )

  # Extract parameter definitions from control file
  params_list <- extract_model_params(model_number, models_folder)
  omega_param_names <- params_list$OMEGAS$param

  # Get parameter estimates
  param_est <- model_stats %>%
    bbr::param_estimates() %>%
    dplyr::mutate(dplyr::across(where(is.numeric))) %>%
    dplyr::filter(random_effect_sd != 0 | is.na(random_effect_sd) | diag == TRUE) %>%
    dplyr::mutate(
      parameter_names2  = parameter_names,
      parameter_names = rbind(
        params_list$THETAS,
        params_list$OMEGAS,
        params_list$SIGMA
      )$param,
      trans = rbind(
        params_list$THETAS,
        params_list$OMEGAS,
        params_list$SIGMA
      )$trans
    ) %>%
    dplyr::mutate(
      estimate = dplyr::case_when(
        diag == FALSE ~ random_effect_sd,   # Use correlation for off-diagonal
        TRUE ~ estimate                      # Keep original for others
      ),
      stderr = dplyr::case_when(
        diag == FALSE ~ random_effect_sdse,  # Use correlation SE for off-diagonal
        TRUE ~ stderr                         # Keep original for others
      )
    ) %>%
    dplyr::filter(!(parameter_names2 == "SIGMA(1,1)" & fixed == TRUE)) %>%
    dplyr::mutate(
      Parameter = .to_report_scale(estimate, random_effect_sd, trans, diag),
      RSE = dplyr::case_when(
        is.na(random_effect_sd) & trans == "RATIO" ~ abs((stderr/estimate) * 100),
        is.na(random_effect_sd) & trans == "LOG" ~ abs(stderr) * 100,
        diag == TRUE ~  100 * stderr * exp(estimate) / (2 * (exp(estimate) - 1)),
        diag == FALSE ~ 100 * stderr / estimate
      ),
      # Built on the estimation scale, then mapped over like the estimate, so the
      # interval carries the units the Estimate column shows. A parameter with no
      # standard error (held at a FIX value, or an unsuccessful covariance step)
      # leaves it NA.
      CI = .format_ci(
        .to_report_scale(estimate - 1.96 * stderr, random_effect_sd, trans, diag),
        .to_report_scale(estimate + 1.96 * stderr, random_effect_sd, trans, diag)
      )
    ) %>%
    dplyr::rename(nonmem_name = "parameter_names2")

  # Extract shrinkage for diagonal OMEGA elements only
  shrinkage_df <- dplyr::filter(
    param_est,
    diag == TRUE,
    parameter_names %in% omega_param_names
  ) %>%
    dplyr::select(parameter_names, shrinkage) %>%
    dplyr::rename(SHRINKAGE = shrinkage) %>%
    dplyr::mutate(SHRINKAGE = shrinkage_value[dplyr::row_number()])

  # Combine all parameters
  param_est <- param_est %>%
    # all_of, not any_of: the four are built in the mutate above and their
    # absence is a bug, not something to carry on past. The optional two exist
    # either way and are selected only when asked for.
    dplyr::select(
      dplyr::all_of(c("parameter_names", "Parameter", "RSE", "fixed")),
      dplyr::all_of(if (include_ci) c("nonmem_name", "CI") else character(0))
    ) %>%
    dplyr::left_join(shrinkage_df, by = "parameter_names") %>%
    dplyr::bind_rows(OFV, COND_NUM) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      RSE = ifelse(fixed == TRUE, NA, RSE),
      is_omega = parameter_names %in% omega_param_names
    )

  # Add labels and comments if spec_pk is provided
  if (!is.null(spec_pk)) {
    non_beta_params <- param_est$parameter_names[!grepl("^beta_", param_est$parameter_names) &
                                                   !param_est$parameter_names %in% c("OFV", "Conditional number")]
    missing_from_spec <- non_beta_params[!non_beta_params %in% names(spec_pk)]

    if (length(missing_from_spec) > 0) {
      cat(sprintf(
        "Parameters not found in spec_pk: %s",
        paste(missing_from_spec, collapse = ", ")
      ))
    }
    param_est <- param_est %>%
      dplyr::mutate(
        comment = dplyr::case_when(
          parameter_names %in% c("OFV", "Conditional number") ~ NA_character_,
          parameter_names %in% names(spec_pk) ~ sapply(parameter_names, function(p) {
            val <- spec_pk[[p]]$comment
            if (!is.null(val)) val else NA_character_
          }),
          TRUE ~ "Parameter-Covariate relationships"
        ),
        label = dplyr::case_when(
          grepl("^beta_", parameter_names) ~ {
            # Fixed-slot tag: beta_<COV>_<PARAM>[_<theta name(s)> | _<level>].
            # COV = parts[2], PARAM = parts[3] (single tokens); a trailing NUMERIC
            # token is a categorical level, otherwise trailing token(s) are the
            # multi-theta parameter name(s), e.g. beta_WT_CL_EMAX.
            parts <- strsplit(parameter_names, "_")[[1]]

            if (length(parts) < 3) {
              NA_character_
            } else {
              cov_name    <- parts[2]
              param_name  <- parts[3]
              extra       <- if (length(parts) >= 4) parts[4:length(parts)] else character(0)
              last_part   <- if (length(extra) > 0) extra[length(extra)] else NA_character_
              is_level    <- !is.na(last_part) && !is.na(suppressWarnings(as.numeric(last_part)))
              param_label <- if (param_name %in% names(spec_pk)) spec_pk[[param_name]]$label else param_name
              # The tag's own COV token stays the lookup key; only what is
              # printed becomes the name the spec gives it.
              cov_display <- .cov_display_name(cov_name, lookup, spec_pk)

              if (is_level) {
                level   <- last_part
                decoded <- decode_cov_level(cov_name, level, lookup)
                if (!is.na(decoded)) {
                  # Covariate first, level in parentheses - the order the short
                  # name reads in, so the two columns agree.
                  paste0("Effect of ", cov_display, " (", decoded, ") on ", param_label)
                } else {
                  paste0("Effect of ", cov_display, " level ", level, " on ", param_label)
                }
              } else if (length(extra) > 0) {
                # multi-theta: the theta name(s) attach to the covariate side (e.g.
                # WT_EMAX), matching how categorical levels attach to the covariate.
                paste0("Effect of ", paste(c(cov_display, extra), collapse = "_"), " on ", param_label)
              } else {
                paste0("Effect of ", cov_display, " on ", param_label)
              }
            }
          },
          parameter_names %in% names(spec_pk) ~ {
            label_value <- spec_pk[[parameter_names]]$label
            if (!is.null(label_value)) label_value else NA_character_
          },
          TRUE ~ NA_character_
        ),
        parameter_names = dplyr::case_when(
          grepl("^beta_", parameter_names) ~ {
            # Same fixed-slot parsing for the short name.
            parts <- strsplit(parameter_names, "_")[[1]]

            if (length(parts) < 3) {
              parameter_names
            } else {
              cov_name    <- parts[2]
              param_name  <- parts[3]
              extra       <- if (length(parts) >= 4) parts[4:length(parts)] else character(0)
              last_part   <- if (length(extra) > 0) extra[length(extra)] else NA_character_
              is_level    <- !is.na(last_part) && !is.na(suppressWarnings(as.numeric(last_part)))
              param_short <- if (param_name %in% names(spec_pk)) spec_pk[[param_name]]$short else param_name
              cov_display <- .cov_display_name(cov_name, lookup, spec_pk)

              if (is_level) {
                level   <- last_part
                decoded <- decode_cov_level(cov_name, level, lookup)
                # "SEX: Female~Vc/F" - the covariate, then the level it refers
                # to, punctuated as create_covariate_table() punctuates its own
                # scenario labels.
                if (!is.na(decoded)) {
                  paste0(cov_display, ": ", decoded, "~", param_short)
                } else {
                  paste0(cov_display, ": level ", level, "~", param_short)
                }
              } else if (length(extra) > 0) {
                # multi-theta: theta name(s) attach to the covariate side (WT_EMAX);
                # parameter stays after '~'.
                paste0(paste(c(cov_display, extra), collapse = "_"), "~", param_short)
              } else {
                paste0(cov_display, "~", param_short)
              }
            }
          },
          parameter_names %in% names(spec_pk) ~ sapply(parameter_names, function(p) {
            if (!is.null(spec_pk[[p]]$unit)) {
              paste0(spec_pk[[p]]$short, " (", spec_pk[[p]]$unit, ")")
            } else if (!is.null(spec_pk[[p]]$short)) {
              spec_pk[[p]]$short
            } else {
              p
            }
          }, USE.NAMES = FALSE),
          TRUE ~ parameter_names
        )
      ) %>%
      dplyr::relocate(label, .after = parameter_names)
  } else {
    # Default labels if no spec provided
    param_est <- param_est %>%
      dplyr::mutate(
        comment = dplyr::case_when(
          grepl("^beta_", parameter_names) ~ "Parameter-Covariate relationships",
          grepl("^THETA", parameter_names) ~ "Typical parameters",
          grepl("^OMEGA", parameter_names) ~ "Inter-individual variability",
          grepl("^SIGMA", parameter_names) ~ "Residual variability",
          parameter_names == "OFV" ~ NA_character_,
          TRUE ~ NA_character_
        ),
        label = parameter_names
      )
  }

  # Final formatting
  param_est <- param_est %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      comment = factor(comment, levels = c(
        "Typical parameters",
        "Inter-individual variability",
        "Correlation of random effects",
        "Parameter-Covariate relationships",
        "Residual variability",
        NA
      ))
    ) %>%
    dplyr::arrange(comment) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))

  # Format output based on number of models
  if (count_model > 1) {
    # Multiple model comparison format
    param_est <- param_est %>%
      dplyr::mutate(
        model = dplyr::case_when(
          fixed == TRUE ~ paste(Parameter, "FIX"),
          parameter_names %in% c("OFV", "Conditional number") ~ as.character(Parameter),
          !is.na(Parameter) & !is.na(RSE) & is_omega & !is.na(SHRINKAGE) ~
            paste0(Parameter, " (", RSE, "%)", " [", SHRINKAGE, "%]"),
          !is.na(Parameter) & !is.na(RSE) & is_omega & is.na(SHRINKAGE) ~
            paste0(Parameter, " (", RSE, "%)", " [NA%]"),
          !is.na(Parameter) & !is.na(RSE) & !is_omega ~
            paste0(Parameter, " (", RSE, "%)"),
          !is.na(Parameter) & is.na(RSE) & is_omega & !is.na(SHRINKAGE) ~
            paste0(Parameter, " (NA%)", " [", SHRINKAGE, "%]"),
          !is.na(Parameter) & is.na(RSE) & is_omega & is.na(SHRINKAGE) ~
            paste0(Parameter, " (NA%) [NA%]"),
          !is.na(Parameter) & is.na(RSE) & !is_omega ~
            paste0(Parameter, " (NA%)"),

          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::select("parameter_names", "label", "model", "comment") %>%
      dplyr::rename(!!as.character(model_number) := model)
  } else {
    # Single model format
    param_est <- param_est %>%
      dplyr::mutate(
        Parameter = dplyr::case_when(
          fixed == TRUE ~ paste(Parameter, "FIX"),
          fixed == FALSE ~ as.character(Parameter),
          TRUE ~ as.character(Parameter)
        )
      ) %>%
      dplyr::select(-fixed)

    # is_omega tells a missing shrinkage apart from one that was never expected,
    # which only the merged bootstrap column needs.
    if (!include_ci) {
      param_est <- dplyr::select(param_est, -is_omega)
    }
  }

  return(param_est)
}


#' Generate Parameter Table Report for Multiple Models
#'
#' @param model_names Character vector of model names
#' @param shrinkage Type of shrinkage to report ("etasd", "etavr", "ebvsd", "ebvvr")
#' @param models_folder Path to models folder
#' @param spec_pk Optional yspec object for parameter formatting
#' @param lookup Optional covariate lookup spec used to decode categorical covariate
#'   levels in the table labels. A `lookup.yaml`-shaped list (e.g.
#'   `yaml::read_yaml("data/spec/lookup.yaml")`) where each covariate entry carries
#'   `values` and `decode`. When `NULL` (default) or when a covariate/level has no
#'   usable decode, the generic "level N" label is used.
#' @param bootstrap Logical. Report a single model beside a non-parametric
#'   bootstrap. The final-model columns collapse to `Estimate` and
#'   `RSE (%) [Shrinkage (%)]`, an asymptotic `95% CI` column
#'   (`estimate ± 1.96 * SE`) is added, and the bootstrap `Median` and
#'   `95% CI-bootstrap` follow under their own spanning header. Requires exactly
#'   one model name and a `boot_results`. Default `FALSE` leaves the table as it
#'   was.
#'
#'   The interval is built where the standard error lives and then mapped onto
#'   the reported scale, so it carries the units of the Estimate beside it. Two
#'   cases leave it empty: a parameter with no standard error (held at a FIX
#'   value, or an unsuccessful covariance step), and an OMEGA whose lower bound
#'   reaches a negative variance, which has no CV% - roughly, once the printed
#'   RSE passes the high twenties for a small OMEGA. The bound is left out
#'   rather than floored at zero, which would state a limit the estimation did
#'   not support.
#' @param boot_results Bootstrap summaries as a data frame, or a path to a
#'   `.csv`/`.rds` holding one. Rows are matched to the report on NONMEM's own
#'   parameter names (`THETA1`, `OMEGA(1,1)`, ...), so order does not matter and
#'   extra rows are reported and skipped. Needs a parameter-name column
#'   (`parameter_names`/`parameter`/`param`), a central-value column
#'   (`median`/`estimate`/`value`) and one column whose name contains "CI",
#'   holding the interval as `"[65.2-81.1]"` or `"[65.2, 81.1]"`. Values are used
#'   as given: they must already be on the scale the report shows (CV% for OMEGA
#'   diagonals, back-transformed THETAs).
#' @return flextable object with formatted parameter table
#' @export
model_report <- function(model_names,
                         shrinkage = "etasd",
                         models_folder = "models",
                         spec_pk = NULL,
                         lookup = NULL,
                         bootstrap = FALSE,
                         boot_results = NULL) {

  if (length(model_names) == 0) {
    stop("At least one model name must be provided")
  }

  bootstrap <- isTRUE(bootstrap)

  if (bootstrap) {
    if (length(model_names) != 1) {
      stop(sprintf(
        paste("bootstrap = TRUE reports one model against its bootstrap;",
              "%d model names were given."), length(model_names)
      ), call. = FALSE)
    }
    if (is.null(boot_results)) {
      stop("bootstrap = TRUE requires `boot_results`.", call. = FALSE)
    }
    boot_results <- .load_if_path(boot_results, "boot_results")
  } else if (!is.null(boot_results)) {
    warning("`boot_results` is ignored while bootstrap = FALSE.", call. = FALSE)
  }

  count_model <- length(model_names)

  cat(sprintf("\n Generating report for %d model(s)...\n", count_model))

  # Try to generate parameter table for each model with error handling
  param_results <- list()
  failed_models <- character(0)
  successful_models <- character(0)

  for (model_name in model_names) {
    cat(sprintf("  Processing %s... ", model_name))

    result <- tryCatch({
      param_table <- get_param2(
        model_number = model_name,
        count_model = count_model,
        shrinkage = shrinkage,
        models_folder = models_folder,
        spec_pk = spec_pk,
        lookup = lookup,
        include_ci = bootstrap
      )
      cat("✓\n")
      list(success = TRUE, data = param_table, model = model_name)
    }, error = function(e) {
      cat("✗\n")
      cat(sprintf("    ⚠️  Error: %s\n", conditionMessage(e)))
      list(success = FALSE, error = conditionMessage(e), model = model_name)
    }, warning = function(w) {
      cat("⚠️\n")
      cat(sprintf("    Warning: %s\n", conditionMessage(w)))
      list(success = FALSE, error = paste("Warning:", conditionMessage(w)), model = model_name)
    })

    if (result$success) {
      param_results[[model_name]] <- result$data
      successful_models <- c(successful_models, model_name)
    } else {
      failed_models <- c(failed_models, model_name)
    }
  }

  # Check if we have any successful models
  if (length(successful_models) == 0) {
    stop("\nAll models failed to process. Cannot generate report.")
  }

  # Print summary
  cat("\n")
  if (length(failed_models) > 0) {
    cat("⚠️  WARNING: The following models failed and were excluded:\n")
    for (failed_model in failed_models) {
      cat(sprintf("  • %s\n", failed_model))
    }
    cat("\n")
  }

  cat(sprintf("✅ Successfully processed %d/%d model(s)\n\n",
              length(successful_models), count_model))

  # Update count to reflect successful models only
  count_model <- length(successful_models)

  # Combine successful model tables
  param_est <- param_results %>%
    purrr::reduce(dplyr::full_join, by = c("parameter_names", "label", "comment")) %>%
    dplyr::arrange(comment)

  if (bootstrap) {
    param_est <- .join_bootstrap_results(param_est, boot_results)
  }

  # Create grouped table with section headers
  grouped_table <- param_est %>%
    dplyr::group_by(comment) %>%
    dplyr::summarise(
      parameter_names = "",
      label = "",
      comment_info = dplyr::first(comment),
      .groups = "drop"
    ) %>%
    dplyr::mutate(comment_info = as.character(comment)) %>%
    dplyr::bind_rows(param_est, .id = "group") %>%
    dplyr::arrange(comment, group) %>%
    dplyr::mutate(
      parameter_names = ifelse(!is.na(comment_info), comment_info, parameter_names),
      label = ifelse(!is.na(comment_info), "", label),
      comment = ifelse(!is.na(comment_info), "", comment)
    )

  # Rename columns based on single vs multiple models
  if (count_model == 1 && bootstrap) {
    # Estimate keeps its own column; RSE and shrinkage merge into one the way the
    # multi-model columns merge them. Section header rows carry no numbers, and
    # neither do OFV, the conditional number, or a parameter held at a FIX value.
    for (needed in c("RSE", "SHRINKAGE", "CI", "is_omega")) {
      if (!needed %in% names(grouped_table)) grouped_table[[needed]] <- NA
    }

    grouped_table <- grouped_table %>%
      dplyr::mutate(
        # An OMEGA keeps the bracket even when its shrinkage could not be read,
        # as the multi-model column does: without it the row is indistinguishable
        # from a THETA that never had one.
        rse_shrinkage = dplyr::case_when(
          is.na(RSE) & is.na(SHRINKAGE) ~ "",
          is.na(RSE) ~ paste0("NA [", SHRINKAGE, "]"),
          !is.na(SHRINKAGE) ~ paste0(RSE, " [", SHRINKAGE, "]"),
          !is.na(is_omega) & is_omega ~ paste0(RSE, " [NA]"),
          TRUE ~ as.character(RSE)
        )
      ) %>%
      dplyr::select(
        "parameter_names", "label",
        Estimate = "Parameter",
        `RSE (%) [Shrinkage (%)]` = "rse_shrinkage",
        `95% CI` = "CI",
        Median = "boot_median",
        `95% CI-bootstrap` = "boot_ci"
      )

  } else if (count_model == 1) {
    # For single model, check which columns actually exist before renaming
    col_names <- names(grouped_table)

    # Build rename list dynamically based on existing columns
    rename_list <- list()
    if ("Parameter" %in% col_names) rename_list[["Estimate"]] <- "Parameter"
    if ("RSE" %in% col_names) rename_list[["RSE (%)"]] <- "RSE"
    if ("SHRINKAGE" %in% col_names) rename_list[["Shrinkage (%)"]] <- "SHRINKAGE"

    # Only rename if we have columns to rename
    if (length(rename_list) > 0) {
      grouped_table <- grouped_table %>%
        dplyr::rename(!!!rename_list)
    }

    # Remove helper columns
    grouped_table <- grouped_table %>%
      dplyr::select(-dplyr::any_of(c("comment_info", "group", "comment")))

  } else {
    # Multiple models
    keep_cols <- c("group", "comment", "parameter_names", "label", "comment_info")
    new_names <- names(grouped_table)
    new_names[!new_names %in% keep_cols] <- paste0(
      "Model ",
      new_names[!new_names %in% keep_cols],
      " Estimate (RSE%) [Shrinkage%]"
    )
    colnames(grouped_table) <- new_names
    grouped_table <- grouped_table %>%
      dplyr::select(-dplyr::any_of(c("comment_info", "group", "comment")))
  }

  # Create and format flextable
  flextable_object <- flextable::flextable(grouped_table)

  # Spanning header separating the final model's columns from the bootstrap's
  if (bootstrap) {
    flextable_object <- flextable::add_header_row(
      flextable_object,
      values = c("", "Final model", "Non-parametric bootstrap"),
      colwidths = c(2, 3, 2)
    )
  }

  flextable_object <- flextable_object %>%
    flextable::align(j = c(-1), align = "center", part = "all") %>%
    theme_pps_table() %>%
    flextable::bold(
      i = which(grouped_table$label == ""),
      bold = TRUE,
      part = "body"
    ) %>%
    flextable::merge_v(j = 1) %>%
    flextable::autofit()

  # Add footer note if models were skipped
  if (length(failed_models) > 0) {
    footer_text <- sprintf(
      "Note: %d model(s) excluded due to errors: %s",
      length(failed_models),
      paste(failed_models, collapse = ", ")
    )
    flextable_object <- flextable::add_footer_lines(
      flextable_object,
      values = footer_text
    )
  }

  return(flextable_object)
}
