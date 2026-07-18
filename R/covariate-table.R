# =============================================================================
# COVARIATE SCENARIO TABLE
# File: R/covariate-table.R
# Part of CovariateSearcher Package
# =============================================================================

#' Create a Covariate Table (Null Patient + One-at-a-Time Variations)
#'
#' @description
#' Builds a covariate table for covariate-effect / forest-plot style analyses.
#' The first row is the "null patient": every covariate held at its reference
#' value. Each subsequent row varies exactly one covariate while all others stay
#' at reference. Continuous covariates are varied across the requested data
#' percentiles; categorical covariates are varied across their non-reference
#' levels.
#'
#' Covariates are taken from the model (its `beta_<COV>_<PARAM>` THETA names).
#' Reference values, covariate type, and categorical levels come from the
#' covariate-search object. Percentiles for continuous covariates are computed
#' from the supplied dataset, using one row per subject.
#'
#' @param model_name Character. Model name without extension, e.g. "run28".
#' @param covariate_search Data frame. Covariate-search table with columns
#'   `COVARIATE`, `STATUS` ("con"/"cat"), `REFERENCE`, and `LEVELS`
#'   (semicolon-separated levels for categoricals, e.g. "0;1;2"). If a
#'   `cov_to_test` column is absent it is derived from `COVARIATE`/`PARAMETER`.
#' @param data Data frame. The analysis dataset used to compute continuous
#'   covariate percentiles.
#' @param percentiles Numeric vector of probabilities in \[0, 1] used to vary
#'   continuous covariates. Default `c(0.05, 0.95)`.
#' @param models_folder Character. Folder containing the model. Default "models".
#' @param id_col Character. Subject ID column in `data`, used to de-duplicate to
#'   one row per subject before computing percentiles. Default "ID".
#' @param lookup Optional named list (lookup.yaml-style) keyed by covariate, used
#'   only to render the `Scenario` description. Each entry may hold `short` or
#'   `label` (a human name for the covariate) and `values`/`decode` (to translate
#'   a categorical level to its label). When absent, the covariate code and raw
#'   level are used.
#' @param wrap_width Integer. If a `Scenario` string is longer than this many
#'   characters, a line break is inserted so the "(...)" detail (continuous) or
#'   the category level (categorical) moves onto a second line, which helps long
#'   labels fit on plots. Set to `Inf` or `NULL` to disable. Default 30.
#'
#' @return A `data.frame` whose first column `Scenario` describes each row
#'   ("Typical subject" for the null patient, "Low/High <covariate> (Nth
#'   percentile)" for continuous, "<covariate>: <level>" for categorical),
#'   followed by one column per covariate. Row 1 is the null patient (all
#'   reference values); each further row is a single-covariate variation.
#'
#' @examples
#' \dontrun{
#' tbl <- create_covariate_table(
#'   model_name       = "run28",
#'   covariate_search = search_state$covariate_search,
#'   data             = search_state$data_file,
#'   percentiles      = c(0.05, 0.95)
#' )
#' }
#' @export
create_covariate_table <- function(model_name,
                                    covariate_search,
                                    data,
                                    percentiles = c(0.05, 0.95),
                                    models_folder = "models",
                                    id_col = "ID",
                                    lookup = NULL,
                                    wrap_width = 30) {

  # ---- Validate inputs ------------------------------------------------------
  required_cols <- c("COVARIATE", "STATUS", "REFERENCE", "LEVELS")
  missing_cols <- setdiff(required_cols, names(covariate_search))
  if (length(missing_cols) > 0) {
    stop("`covariate_search` is missing required column(s): ",
         paste(missing_cols, collapse = ", "))
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame containing the analysis dataset.")
  }
  if (is.null(percentiles)) {
    percentiles <- c(0.05, 0.95)
  }
  if (length(percentiles) == 0 || !is.numeric(percentiles) ||
      any(percentiles < 0 | percentiles > 1)) {
    stop("`percentiles` must be a non-empty numeric vector of ",
         "probabilities in [0, 1].")
  }
  if (!id_col %in% names(data)) {
    stop("`id_col` ('", id_col, "') not found in `data`.")
  }

  # ---- Identify covariates in the model -------------------------------------
  # Read the model's beta_<COV>_<PARAM> THETA names straight from the control
  # stream, so this function is self-contained (no package internals needed).
  base_path  <- file.path(models_folder, model_name)
  candidates <- c(paste0(base_path, ".ctl"), paste0(base_path, ".mod"),
                  file.path(base_path, paste0(model_name, ".ctl")),
                  file.path(base_path, paste0(model_name, ".mod")))
  ctl_path <- candidates[file.exists(candidates)]
  if (length(ctl_path) == 0) {
    stop("Model control file not found for '", model_name, "' under '",
         models_folder, "' (looked for .ctl / .mod).")
  }
  ctl_path <- ctl_path[1]

  # THETA annotations look like:  <init> ; <name> ; <unit> ; <trans>
  # The covariate-effect THETAs are named beta_<COV>_<PARAM>[_<LEVEL>].
  ctl_lines   <- readLines(ctl_path, warn = FALSE)
  theta_names <- vapply(strsplit(ctl_lines, ";"), function(parts) {
    if (length(parts) >= 2) trimws(parts[2]) else NA_character_
  }, character(1))
  beta_names <- unique(theta_names[!is.na(theta_names) &
                                     grepl("^beta_", theta_names)])
  if (length(beta_names) == 0) {
    stop("No covariate (beta_*) THETAs found in model '", model_name, "'.")
  }

  # Derive the join key if the caller did not supply one.
  cov_search <- covariate_search
  if (!"cov_to_test" %in% names(cov_search)) {
    if (!"PARAMETER" %in% names(cov_search)) {
      stop("`covariate_search` needs a 'cov_to_test' or a 'PARAMETER' column.")
    }
    cov_search$cov_to_test <- paste0("beta_", cov_search$COVARIATE, "_",
                                     cov_search$PARAMETER)
  }

  # A covariate is in the model when one of its beta THETAs is present. Match the
  # exact tag or a level-suffixed tag (e.g. beta_SEXN_CL_2 for multi-level cats).
  in_model <- vapply(cov_search$cov_to_test, function(tag) {
    any(beta_names == tag | startsWith(beta_names, paste0(tag, "_")))
  }, logical(1))

  # One metadata row per covariate (reference/type/levels are covariate-level
  # properties, identical across the parameters a covariate acts on).
  meta <- cov_search[in_model, required_cols, drop = FALSE]
  meta <- meta[!duplicated(meta$COVARIATE), , drop = FALSE]

  if (nrow(meta) == 0) {
    stop("None of the model's covariates were found in `covariate_search`.")
  }

  covariates <- meta$COVARIATE
  reference  <- stats::setNames(as.numeric(meta$REFERENCE), covariates)

  # ---- Scenario-description helpers (use `lookup` when supplied) -------------
  wrap_w <- if (is.null(wrap_width) || !is.finite(wrap_width)) Inf else wrap_width

  # Join a label and its detail with a space, or a line break when the combined
  # text would exceed `wrap_w` (moves the "(...)" detail or the category level
  # onto its own line, without splitting words).
  join_wrap <- function(head, tail) {
    if (nchar(head) + 1L + nchar(tail) > wrap_w) {
      paste0(head, "\n", tail)
    } else {
      paste0(head, " ", tail)
    }
  }

  cov_label <- function(cov) {
    entry <- lookup[[cov]]
    if (!is.null(entry)) {
      if (!is.null(entry$short)) return(as.character(entry$short))
      if (!is.null(entry$label)) return(as.character(entry$label))
    }
    cov
  }
  cov_unit <- function(cov) {
    entry <- lookup[[cov]]
    if (!is.null(entry) && !is.null(entry$unit)) as.character(entry$unit) else ""
  }
  decode_level <- function(cov, lvl) {
    entry <- lookup[[cov]]
    if (!is.null(entry) && !is.null(entry$values) && !is.null(entry$decode)) {
      idx <- match(as.character(lvl), as.character(unlist(entry$values)))
      if (!is.na(idx) && idx <= length(entry$decode)) {
        return(as.character(entry$decode[[idx]]))
      }
    }
    as.character(lvl)
  }
  ordinal <- function(n) {
    n   <- round(n)
    suf <- if ((n %% 100) %in% 11:13) {
      "th"
    } else {
      c("th", "st", "nd", "rd", rep("th", 6))[(n %% 10) + 1]
    }
    paste0(n, suf)
  }
  cont_scenario <- function(label, p, value, unit) {
    direction <- if (p < 0.5) "Low" else if (p > 0.5) "High" else "Median"
    value_txt <- format(value, trim = TRUE)
    if (nzchar(unit)) value_txt <- paste0(value_txt, " ", unit)
    head <- paste0(direction, " ", label)
    tail <- paste0("(", ordinal(p * 100), " percentile: ", value_txt, ")")
    join_wrap(head, tail)
  }

  # ---- Null patient (reference) row -----------------------------------------
  ref_row   <- as.data.frame(as.list(reference),
                             stringsAsFactors = FALSE,
                             check.names = FALSE)
  rows      <- list(ref_row)
  scenarios <- "Typical subject"

  # ---- One row per subject for percentile computation -----------------------
  data_by_subject <- data[!duplicated(data[[id_col]]), , drop = FALSE]

  # ---- One-at-a-time variation rows -----------------------------------------
  for (i in seq_len(nrow(meta))) {
    cov    <- meta$COVARIATE[i]
    status <- tolower(as.character(meta$STATUS[i]))
    label  <- cov_label(cov)

    if (status == "con") {
      if (!cov %in% names(data_by_subject)) {
        warning("Continuous covariate '", cov, "' not found in `data`; skipped.")
        next
      }
      qvals <- unname(stats::quantile(
        data_by_subject[[cov]],
        probs = percentiles, na.rm = TRUE, names = FALSE
      ))
      unit <- cov_unit(cov)
      for (k in seq_along(percentiles)) {
        new_row        <- ref_row
        new_row[[cov]] <- qvals[k]
        rows[[length(rows) + 1]] <- new_row
        scenarios <- c(scenarios,
                       cont_scenario(label, percentiles[k], qvals[k], unit))
      }
    } else if (status == "cat") {
      levels_all <- suppressWarnings(
        as.numeric(strsplit(as.character(meta$LEVELS[i]), ";")[[1]])
      )
      levels_all <- levels_all[!is.na(levels_all)]
      var_levels <- setdiff(levels_all, reference[[cov]])
      for (lvl in var_levels) {
        new_row        <- ref_row
        new_row[[cov]] <- lvl
        rows[[length(rows) + 1]] <- new_row
        scenarios <- c(scenarios,
                       join_wrap(paste0(label, ":"), decode_level(cov, lvl)))
      }
    } else {
      warning("Unknown STATUS '", status, "' for covariate '", cov,
              "'; skipped.")
      next
    }
  }

  # ---- Assemble -------------------------------------------------------------
  result <- do.call(rbind, rows)
  result <- cbind(Scenario = scenarios, result, stringsAsFactors = FALSE)
  rownames(result) <- NULL
  result
}
