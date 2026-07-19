# =============================================================================
# APPLY COVARIATE MODEL TO INDIVIDUAL THETAS
# File: R/apply-covariate-model.R
# Part of CovariateSearcher Package
# =============================================================================

#' Apply a Model's Covariate Relationships to Individual Thetas
#'
#' @description
#' Reproduces, in R, the covariate transformations written in a NONMEM model and
#' applies them to a set of individual parameter values for one covariate
#' scenario. Each structural parameter theta (e.g. `THETA2` = CL) is multiplied
#' by the covariate factors defined in the model, using the covariate values in
#' `covariates` and the covariate coefficients already present in
#' `individual_thetas` (the beta thetas). Only the structural thetas are returned.
#'
#' This replaces hand-written blocks such as
#' `mutate(THETA2 = THETA2 * (DOSE/75)^THETA7 * (BWT/70)^THETA8 * ...)` by
#' reconstructing the same factors from the model itself:
#' \itemize{
#'   \item the beta coefficient's theta index comes from the `$THETA` order,
#'   \item the structural parameter's theta index comes from its `$PK`
#'         typical-value line (`TV_<param>` or `TV<param> = THETA(n)`),
#'   \item continuous formulas/reference values come from `covariate_search`,
#'   \item categorical level -> theta mappings come from the `$PK` `IF/ELSEIF`
#'         block (so multi-level categoricals are handled by the covariate value).
#' }
#'
#' @param model_name Character. Model name without extension, e.g. "run28".
#' @param covariate_search Data frame with columns `COVARIATE`, `PARAMETER`,
#'   `STATUS` ("con"/"cat"), `FORMULA` ("power"/"linear"/"exponential"/...),
#'   and `REFERENCE`. A `cov_to_test` column is derived from
#'   `COVARIATE`/`PARAMETER` when absent.
#' @param individual_thetas Data frame whose columns are named `THETA1`,
#'   `THETA2`, ... (population + ETA values already combined). Beta columns hold
#'   the covariate coefficients.
#' @param covariates A single covariate scenario: a named vector or one-row data
#'   frame whose names are covariate columns (e.g. one row of
#'   \code{\link{create_covariate_table}} output). Applied to every row of
#'   `individual_thetas`. Extra names (such as `Scenario`) are ignored.
#' @param models_folder Character. Folder containing the model. Default "models".
#'
#' @return A `data.frame` with an `ID` column and only the structural (non-beta)
#'   theta columns, each already multiplied by the model's covariate factors for
#'   the supplied scenario. One row per row of `individual_thetas`.
#' @export
apply_covariate_model <- function(model_name,
                                  covariate_search,
                                  individual_thetas,
                                  covariates,
                                  models_folder = "models") {

  # ---- Normalise the single covariate scenario to a named vector ------------
  if (is.data.frame(covariates)) {
    if (nrow(covariates) != 1) {
      stop("`covariates` must be a single scenario (one row).")
    }
    covariates <- unlist(covariates[1, , drop = TRUE])
  }
  if (is.null(names(covariates))) {
    stop("`covariates` must be named (one value per covariate column).")
  }

  # ---- Read the model control stream ----------------------------------------
  base_path  <- file.path(models_folder, model_name)
  candidates <- c(paste0(base_path, ".ctl"), paste0(base_path, ".mod"),
                  file.path(base_path, paste0(model_name, ".ctl")),
                  file.path(base_path, paste0(model_name, ".mod")))
  ctl_path <- candidates[file.exists(candidates)]
  if (length(ctl_path) == 0) {
    stop("Model control file not found for '", model_name, "' under '",
         models_folder, "' (looked for .ctl / .mod).")
  }
  lines <- readLines(ctl_path[1], warn = FALSE)

  # Return the lines of a $BLOCK (header + body, up to the next $ record).
  get_block <- function(tag) {
    starts <- grep(paste0("^\\s*\\$", tag), lines, ignore.case = TRUE)
    if (length(starts) == 0) return(character(0))
    out <- character(0)
    for (s in starts) {
      e <- s + 1
      while (e <= length(lines) && !grepl("^\\s*\\$", lines[e])) e <- e + 1
      out <- c(out, lines[s:(e - 1)])
    }
    out
  }
  first_theta_index <- function(txt) {
    m <- regmatches(txt, regexpr("THETA\\(\\s*\\d+\\s*\\)", txt))
    if (length(m) == 0) return(NA_integer_)
    as.integer(gsub("\\D", "", m[1]))
  }

  # ---- $THETA order: name of THETA(n) is the nth record's 2nd `;` field ------
  theta_block <- get_block("THETA")
  theta_names <- character(0)
  for (ln in theta_block) {
    if (grepl("^\\s*\\$", ln)) next                 # header line
    parts <- strsplit(ln, ";")[[1]]
    if (!grepl("[0-9]", parts[1])) next             # not a numeric THETA record
    theta_names <- c(theta_names,
                     if (length(parts) >= 2) trimws(parts[2]) else "")
  }
  if (length(theta_names) == 0) {
    stop("Could not parse any $THETA records from model '", model_name, "'.")
  }
  theta_index    <- stats::setNames(seq_along(theta_names), theta_names)
  structural_idx <- which(!grepl("^beta_", theta_names))

  # ---- $PK: structural parameter -> theta index, and categorical IF/ELSEIF --
  pk <- get_block("PK")
  if (length(pk) == 0) pk <- get_block("PRED")     # fall back to $PRED models

  param_theta_index <- function(param) {
    # Prefer the typical-value line (TV_<param> or TV<param>): it holds the clean
    # structural THETA. The `<param> = ...` line often carries covariate betas as
    # its first THETA, so only fall back to it when there is no TV line.
    ln <- pk[grepl(paste0("^\\s*TV_?", param, "\\b\\s*="), pk)]
    if (length(ln) == 0) ln <- pk[grepl(paste0("^\\s*", param, "\\b\\s*="), pk)]
    if (length(ln) == 0) return(NA_integer_)
    first_theta_index(ln[1])
  }

  # For a categorical relationship, read its `IF(COV.EQ.x)THEN <tag> = 1 [+
  # THETA(k)]` block and return a named integer vector (level -> theta index) for
  # non-reference levels. Scoped to the `tag` (beta_<COV>_<PARAM>) assignment
  # target so the same covariate on two parameters stays separate. The reference
  # level (assigned 1) is simply absent from the result.
  cat_level_theta <- function(tag, cov) {
    cond_re   <- paste0("(?i)(?:IF|ELSEIF)\\(\\s*", cov,
                        "\\s*\\.EQ\\.\\s*([^)]+?)\\s*\\)\\s*THEN")
    assign_re <- paste0("^\\s*", tag, "\\s*=")
    out           <- integer(0)
    current_level <- NA_character_
    for (i in seq_along(pk)) {
      g <- regmatches(pk[i], regexec(cond_re, pk[i], perl = TRUE))[[1]]
      if (length(g) > 0) { current_level <- trimws(g[2]); next }
      if (!is.na(current_level) && grepl(assign_re, pk[i])) {
        tk <- first_theta_index(pk[i])
        if (!is.na(tk)) out[current_level] <- tk   # `= 1` (reference) -> skipped
      }
    }
    out
  }

  # ---- Derive the join key on the covariate-search table --------------------
  cov_search <- covariate_search
  if (!"cov_to_test" %in% names(cov_search)) {
    cov_search$cov_to_test <- paste0("beta_", cov_search$COVARIATE, "_",
                                     cov_search$PARAMETER)
  }
  beta_names <- theta_names[grepl("^beta_", theta_names)]

  result <- individual_thetas

  # ---- Apply each covariate relationship that is present in the model -------
  for (r in seq_len(nrow(cov_search))) {
    tag  <- cov_search$cov_to_test[r]
    in_model <- any(beta_names == tag |
                      startsWith(beta_names, paste0(tag, "_")))
    if (!in_model) next

    cov    <- cov_search$COVARIATE[r]
    param  <- cov_search$PARAMETER[r]
    status <- tolower(as.character(cov_search$STATUS[r]))
    formula <- tolower(as.character(cov_search$FORMULA[r]))
    ref    <- suppressWarnings(as.numeric(cov_search$REFERENCE[r]))
    cov_def <- get_covariate_formula(status, formula)

    if (!cov %in% names(covariates)) {
      warning("Covariate '", cov, "' not in `covariates`; skipped.")
      next
    }
    m <- param_theta_index(param)
    struct_col <- paste0("THETA", m)
    if (is.na(m) || !struct_col %in% names(result)) {
      warning("Structural theta for parameter '", param,
              "' not found in model/individual_thetas; skipped.")
      next
    }
    cov_val <- as.numeric(covariates[[cov]])

    # Per-level categorical (cat.linear) reads the level->theta mapping from the
    # $PK IF/ELSEIF block. Every other form (including cat.power) is a single
    # factor reconstructed from the SAME registry entry model_add_cov wrote from,
    # so the write and read sides cannot drift.
    if (!is.null(cov_def) && isTRUE(cov_def$categorical)) {
      # Multi-level safe: pick the theta for the covariate's level (ref -> 1).
      lvl_theta <- cat_level_theta(tag, cov)
      k <- lvl_theta[as.character(cov_val)]
      if (is.na(k)) next                            # reference or level absent
      beta_col <- paste0("THETA", k)
      if (!beta_col %in% names(result)) {
        warning("Beta theta '", beta_col, "' for '", cov, "' missing; skipped.")
        next
      }
      factor <- 1 + result[[beta_col]]
    } else {
      # Single factor. A single-parameter form (built-in or 1-parameter expression)
      # owns the base tag beta_<COV>_<PARAM>; a multi-parameter expression owns one
      # tag per parameter, beta_<COV>_<PARAM>_<name>. Gather the beta(s) in
      # parameter order and hand to the registry's r_eval (scalar/vector for one,
      # list-of-N for many).
      if (is.null(cov_def)) {
        warning("Unknown FORMULA '", formula, "' for '", cov, "'; skipped.")
        next
      }
      tns  <- cov_def$theta_names
      need <- if (length(tns) > 1L) paste0(tag, "_", tns) else tag
      kk   <- theta_index[need]
      bcol <- paste0("THETA", kk)
      if (any(is.na(kk)) || !all(bcol %in% names(result))) {
        warning("Beta theta(s) for '", tag, "' missing; skipped.")
        next
      }
      th <- if (length(tns) > 1L) lapply(bcol, function(b) result[[b]]) else result[[bcol]]
      factor <- cov_def$r_eval(cov_val, ref, th)
    }

    result[[struct_col]] <- result[[struct_col]] * factor
  }

  # ---- Keep the structural thetas only, add an ID ---------------------------
  struct_cols <- paste0("THETA", structural_idx)
  struct_cols <- struct_cols[struct_cols %in% names(result)]
  out <- result[, struct_cols, drop = FALSE]
  out <- cbind(ID = seq_len(nrow(out)), out)
  rownames(out) <- NULL
  out
}
