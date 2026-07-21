# =============================================================================
# EXPOSURE FOREST PLOT PIPELINE
# File: R/forest-plots.R
# Part of CovariateSearcher Package
#
# The sampling -> scenario-table -> simulate -> plot pipeline for exposure-
# metric forest plots. Parameter construction (apply_covariate_model,
# build_scenario_parameters) and the standalone covariate boxplots live in
# their own files, since their output feeds this pipeline and the boxplots.
# =============================================================================


# ---- 1. Theta uncertainty sampling --------------------------------------------

#' Sample THETA Parameter Vectors from Estimation Uncertainty
#'
#' @description
#' Draws `Nsamples` vectors of the model's THETA parameters (structural
#' parameters + covariate betas) from the NONMEM estimation uncertainty, and
#' returns them **on the estimation scale exactly as NONMEM reports them** (a
#' log-parameterised THETA stays on the log scale). [apply_covariate_model()]
#' then evaluates the model's own `$PK` equations, whose `EXP(...)` performs the
#' log -> natural transformation once, at the right place.
#'
#' Sampling is done on the estimation scale that the covariance matrix describes:
#' \itemize{
#'   \item If a `.cov` (or `.cor`) file is available, draws come from the full
#'         multivariate-normal uncertainty distribution, preserving parameter
#'         correlations (via \pkg{mvtnorm}).
#'   \item If neither is available or usable, falls back to sampling each THETA
#'         independently from its `.ext` standard error, with a warning.
#' }
#' No scale transformation is applied here: the draws are returned raw so the
#' back-transform lives in exactly one place (the `EXP(...)` in the model's `$PK`
#' equation, evaluated by [apply_covariate_model()]). This avoids the double
#' `exp()` that a tag-based back-transform here would cause. A correct log-normal
#' (always-positive) draw for a log-estimated structural parameter therefore
#' arises when its `$PK` line is evaluated downstream.
#'
#' @param model Character. Model name without extension, e.g. "run28".
#' @param models_folder Character. Folder containing the model. Default "models".
#'   Both flat (`models/run28.ext`) and per-model-subfolder
#'   (`models/run28/run28.ext`) layouts are supported.
#' @param Nsamples Integer. Number of uncertainty draws. Default 1e5.
#' @param seed Integer. Random seed for reproducibility. Default 1234.
#'
#' @return A `data.frame` with `Nsamples` rows and columns `ID` (1:Nsamples) and
#'   `THETA1`, `THETA2`, ..., `THETAn` on the **estimation scale** (raw `.ext`
#'   scale; a log-parameterised THETA stays on the log scale), with THETA order
#'   matching the model's `$THETA` block. The attribute `"sampling_method"` is
#'   set to `"mvn"` or `"independent"` to record which path was used.
#'
#' @examples
#' \dontrun{
#' samples <- sample_individual_thetas("run28", models_folder = "models",
#'                                     Nsamples = 1000)
#' attr(samples, "sampling_method")
#' # Feed straight into the covariate model for one scenario:
#' # apply_covariate_model("run28", covariate_search, samples, scenario_row)
#' }
#' @export
sample_individual_thetas <- function(model,
                                     models_folder = "models",
                                     Nsamples = 1e5,
                                     seed = 1234) {

  if (!is.character(model) || length(model) != 1) {
    stop("`model` must be a single character string (model name).")
  }
  Nsamples <- as.integer(Nsamples)
  if (is.na(Nsamples) || Nsamples < 1) {
    stop("`Nsamples` must be a positive integer.")
  }

  # ---- Locate the .ext output -----------------------------------------------
  ext_file <- .find_model_component(model, models_folder, "ext")
  if (is.na(ext_file)) {
    stop("`.ext` file not found for '", model, "' under '", models_folder, "'.")
  }

  # ---- Point estimates and standard errors from the .ext --------------------
  ext <- read_ext_iterations(ext_file)
  if (nrow(ext) == 0) {
    stop("Could not read parameter estimates from: ", ext_file)
  }

  # FINAL estimates (ITERATION -1000000000) and their standard errors
  # (ITERATION -1000000001), which read_ext_iterations() labels TYPE "FINAL"/"SE".
  # Take the last of each: the final estimation step and its $COVARIANCE output,
  # which keeps the estimates and SEs mutually consistent.
  # `%in%` (not `==`): TYPE is NA for unrecognised ITERATION markers, and
  # `NA == "FINAL"` would inject phantom all-NA rows into the selection.
  final_rows <- ext[ext$TYPE %in% "FINAL", , drop = FALSE]
  se_rows    <- ext[ext$TYPE %in% "SE",    , drop = FALSE]
  if (nrow(final_rows) == 0) {
    stop("No FINAL parameter row (ITERATION -1000000000) found in ", ext_file)
  }
  final_row <- final_rows[nrow(final_rows), , drop = FALSE]
  se_row    <- if (nrow(se_rows) > 0) se_rows[nrow(se_rows), , drop = FALSE] else se_rows

  theta_cols <- grep("^THETA[0-9]+$", names(ext), value = TRUE)
  if (length(theta_cols) == 0) {
    stop("No THETA columns found in ", ext_file)
  }
  n_theta <- length(theta_cols)

  theta_est <- stats::setNames(as.numeric(final_row[1, theta_cols]), theta_cols)

  if (nrow(se_row) > 0) {
    theta_se <- stats::setNames(as.numeric(se_row[1, theta_cols]), theta_cols)
  } else {
    theta_se <- stats::setNames(rep(NA_real_, n_theta), theta_cols)
  }
  # NONMEM flags fixed / non-estimated parameters with SE 0 or ~1e10.
  is_fixed <- is.na(theta_se) | theta_se <= 0 | theta_se >= 1e9

  # ---- Covariance matrix (THETA block only), or NULL if unavailable ---------
  Sigma <- .read_theta_cov(model, models_folder, theta_cols)

  # ---- Draw the samples on the estimation scale -----------------------------
  set.seed(seed)
  if (!is.null(Sigma)) {
    method  <- "mvn"
    Sigma   <- .make_psd(Sigma)
    samples <- tryCatch(
      mvtnorm::rmvnorm(Nsamples, mean = theta_est, sigma = Sigma,
                       method = "svd"),
      error = function(e) NULL
    )
    if (is.null(samples)) {
      warning("Multivariate sampling failed for '", model,
              "'; falling back to independent sampling from .ext SEs.")
      Sigma <- NULL
    }
  }
  if (is.null(Sigma)) {
    method <- "independent"
    if (all(is_fixed)) {
      warning("No usable .cov/.cor and no standard errors for '", model,
              "'; all THETAs returned at their point estimates (no uncertainty).")
    } else {
      warning("No usable .cov/.cor for '", model,
              "'; sampling THETAs independently from .ext standard errors ",
              "(parameter correlations ignored).")
    }
    sd_use <- theta_se
    sd_use[is_fixed] <- 0
    samples <- vapply(
      seq_len(n_theta),
      function(j) stats::rnorm(Nsamples, mean = theta_est[j], sd = sd_use[j]),
      numeric(Nsamples)
    )
    if (Nsamples == 1L) samples <- matrix(samples, nrow = 1L)
  }
  colnames(samples) <- theta_cols

  # ---- No scale transformation here -----------------------------------------
  # Draws are returned on the estimation scale (raw .ext scale). The single
  # log -> natural back-transform lives downstream in apply_covariate_model(),
  # which evaluates the model's own EXP(...) $PK line. Doing it here as well
  # (per the $THETA ;LOG tag) would double-exponentiate log parameters.

  # ---- Assemble the result --------------------------------------------------
  out <- data.frame(ID = seq_len(Nsamples), samples,
                    check.names = FALSE, stringsAsFactors = FALSE)
  attr(out, "sampling_method") <- method
  out
}


# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

#' Locate a model component file (flat or bbr per-model subfolder)
#'
#' @description Thin wrapper over [find_model_file()] that tolerates both the
#'   flat layout (`models/run.ext`) and the bbr output-subfolder layout
#'   (`models/run/run.ext`). `exts` are extension stems without the dot.
#' @keywords internal
#' @noRd
.find_model_component <- function(model, models_folder, exts) {
  dotted <- paste0(".", exts)
  hit <- find_model_file(file.path(models_folder, model), dotted)          # flat
  if (is.null(hit)) {
    hit <- find_model_file(file.path(models_folder, model, model), dotted) # subfolder
  }
  if (is.null(hit)) NA_character_ else hit
}


#' Read the THETA covariance matrix from a NONMEM .cov/.cor file
#'
#' @description Returns an `n_theta x n_theta` covariance matrix ordered to match
#'   `theta_names` (absolute THETA order), or `NULL` when no usable matrix file
#'   exists. Prefers `.cov` (already a covariance); converts `.cor`
#'   (correlations with SEs on the diagonal) to covariance. Fixed / missing
#'   parameters get zero variance so they are held at their estimate.
#' @keywords internal
#' @noRd
.read_theta_cov <- function(model, models_folder, theta_names) {
  cov_path <- .find_model_component(model, models_folder, "cov")
  cor_path <- .find_model_component(model, models_folder, "cor")

  is_corr <- FALSE
  mfile <- NA_character_
  if (!is.na(cov_path)) {
    mfile <- cov_path
  } else if (!is.na(cor_path)) {
    mfile <- cor_path
    is_corr <- TRUE
  }
  if (is.na(mfile)) return(NULL)

  mat <- .read_nonmem_matrix(mfile)
  if (is.null(mat)) return(NULL)
  if (is_corr) {
    # NONMEM .cor carries SEs on the diagonal and correlations off-diagonal:
    # Cov_ij = r_ij * SE_i * SE_j (off-diagonal), Var_i = SE_i^2.
    se <- diag(mat)
    mat <- mat * outer(se, se)
    diag(mat) <- se^2
  }

  # Build a full matrix in THETA order; zeros for anything absent (fixed params).
  full <- matrix(0, nrow = length(theta_names), ncol = length(theta_names),
                 dimnames = list(theta_names, theta_names))
  common <- intersect(theta_names, rownames(mat))
  if (length(common) == 0) return(NULL)
  full[common, common] <- mat[common, common]
  full
}


#' Parse a NONMEM square, labelled matrix (.cov/.cor) keeping row/col names
#' @keywords internal
#' @noRd
.read_nonmem_matrix <- function(mfile) {
  lines <- readLines(mfile, warn = FALSE)

  # Keep only the last TABLE block (e.g. when $COV printed more than one).
  tbl <- grep("^\\s*TABLE NO", lines)
  if (length(tbl) > 0) {
    lines <- lines[(tbl[length(tbl)] + 1):length(lines)]
  }

  header_idx <- grep("^\\s*NAME\\b", lines)
  if (length(header_idx) == 0) return(NULL)
  col_names <- strsplit(trimws(lines[header_idx[1]]), "\\s+")[[1]][-1]
  if (length(col_names) == 0) return(NULL)

  data_lines <- lines[grepl("^\\s*(THETA|OMEGA|SIGMA)", lines)]
  if (length(data_lines) == 0) return(NULL)

  row_names <- sub("^\\s*(\\S+).*$", "\\1", data_lines)
  body      <- sub("^\\s*\\S+\\s+", "", data_lines)
  rows <- lapply(strsplit(trimws(body), "\\s+"),
                 function(x) suppressWarnings(as.numeric(x)))
  rows <- lapply(rows, function(r) { length(r) <- length(col_names); r })

  mat <- do.call(rbind, rows)
  rownames(mat) <- row_names
  colnames(mat) <- col_names

  # Restrict to the labelled square block present on both axes.
  common <- intersect(rownames(mat), colnames(mat))
  if (length(common) == 0) return(NULL)
  mat[common, common, drop = FALSE]
}


#' Project a symmetric matrix to the nearest positive-semidefinite matrix
#'
#' @description Symmetrises and, if any eigenvalue is meaningfully negative,
#'   clamps negatives to zero and reconstructs. Keeps sampling well-defined for
#'   near-singular NONMEM covariance matrices without a \pkg{Matrix} dependency.
#' @keywords internal
#' @noRd
.make_psd <- function(s, tol = 1e-8) {
  s <- (s + t(s)) / 2
  ev <- eigen(s, symmetric = TRUE)
  vals <- ev$values
  if (length(vals) && min(vals) < -tol * max(abs(vals), 1)) {
    vals[vals < 0] <- 0
    s <- ev$vectors %*% diag(vals, length(vals)) %*% t(ev$vectors)
    s <- (s + t(s)) / 2
    dimnames(s) <- NULL
  }
  s
}


# ---- 2. Covariate scenario table ----------------------------------------------

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
#'   `label` (a human name for the covariate), `unit` (continuous unit) and
#'   `values`/`decode` (to translate a categorical level to its label). When
#'   absent, the covariate code and raw level are used.
#' @param spec Optional yspec object, or a path to a spec YAML (loaded with
#'   [yspec::ys_load()]) - an alternative to `lookup`: the `short`/`unit`/
#'   `values`/`decode` are read from the spec's columns. Any explicit `lookup`
#'   entries take precedence over the spec-derived ones.
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
                                    spec = NULL,
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

  # A yspec `spec` is an alternative to `lookup`: derive short/unit/values/decode
  # from it, then let any explicit `lookup` entries override per covariate.
  if (!is.null(spec)) {
    spec_lu <- .spec_to_lookup(spec)
    lookup  <- utils::modifyList(spec_lu, if (is.null(lookup)) list() else lookup)
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
    value_txt <- format(round(value, 2), trim = TRUE)   # 2 dp in the label
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

  # Typical-subject description (every covariate at its REFERENCE), labelled with
  # the same short/unit/decoded-level machinery, one covariate per line (fits
  # many covariates), attached as an attribute so callers/plots can show it (see
  # plot_exposure_forest's `typical_subject`). e.g.
  #   "Typical subject:\nWeight: 70 kg\nSex: Male".
  ts_parts <- vapply(seq_len(nrow(meta)), function(i) {
    cov <- meta$COVARIATE[i]; lbl <- cov_label(cov); refv <- reference[[cov]]
    if (tolower(as.character(meta$STATUS[i])) == "cat") {
      paste0(lbl, ": ", decode_level(cov, refv))
    } else {
      u <- cov_unit(cov); v <- format(round(refv, 2), trim = TRUE)
      paste0(lbl, ": ", if (nzchar(u)) paste0(v, " ", u) else v)
    }
  }, character(1))
  typical_subject <- paste0("Typical subject:\n", paste(ts_parts, collapse = "\n"))

  # ---- Assemble -------------------------------------------------------------
  result <- do.call(rbind, rows)
  result <- cbind(Scenario = scenarios, result, stringsAsFactors = FALSE)
  rownames(result) <- NULL
  attr(result, "typical_subject") <- typical_subject
  result
}

# Convert a yspec `spec` (object or path) to the `lookup`-list shape that
# create_covariate_table() consumes: per covariate a list of short/unit/values/
# decode, taken from the spec's own per-column fields (a yspec column mirrors its
# YAML: short/label/unit/values/decode). Field access is defensive - a column it
# cannot read is simply omitted, so labels fall back to raw codes (never errors).
.spec_to_lookup <- function(spec) {
  if (is.character(spec) && length(spec) == 1L) {
    spec <- yspec::ys_load(spec)
  }
  nms <- tryCatch(names(spec), error = function(e) character(0))
  out <- list()
  for (nm in nms) {
    cl <- tryCatch(as.list(spec[[nm]]), error = function(e) NULL)
    if (is.null(cl)) next
    short <- if (!is.null(cl[["short"]])) cl[["short"]] else cl[["label"]]
    entry <- list(short  = short,
                  unit   = cl[["unit"]],
                  values = cl[["values"]],
                  decode = cl[["decode"]])
    entry <- entry[!vapply(entry, is.null, logical(1))]
    if (length(entry) > 0) out[[nm]] <- entry
  }
  out
}

# Resolve `metric_info`/`param_info` to a named list(label=, unit=) per quantity.
# Accepts that list directly, OR a spec: a yspec object, a path to a spec YAML,
# or a spec-shaped list (entries carrying `short`) - in which case each entry's
# `short` (label) and `unit` are taken from the spec.
.resolve_quantity_info <- function(x) {
  if (is.null(x)) return(NULL)
  is_spec <- (is.character(x) && length(x) == 1L) || inherits(x, "yspec") ||
    (is.list(x) && length(x) > 0 &&
       any(vapply(x, function(e) is.list(e) && !is.null(e[["short"]]), logical(1))))
  if (!is_spec) return(x)                              # already a label/unit list
  lu  <- .spec_to_lookup(x)
  out <- list()
  for (nm in names(lu)) {
    e <- lu[[nm]]
    out[[nm]] <- list(label = if (!is.null(e$short)) e$short else nm, unit = e$unit)
  }
  out
}

# Stop if any tabulated quantity has no label in `q_info` (user must supply one).
.require_quantity_labels <- function(q_info, quantities, arg = "metric_info") {
  miss <- quantities[!quantities %in% names(q_info)]
  if (length(miss) > 0) {
    stop("No label/unit in `", arg, "` for: ", paste(miss, collapse = ", "),
         ". Add an entry (label + unit) for each of these to the spec / list.",
         call. = FALSE)
  }
  invisible(TRUE)
}

# Build ONE scenario-summary table (absolute or relative) as a formatted
# data.frame: rows = scenarios, leading covariate columns (from `scenario_tbl`,
# decoded via `lookup`, units in the header), then one column per quantity.
#   absolute cell (\n-separated): median / [lo, hi] / geomean / (geoCV%)
#   relative cell: <ratio> [lo, hi]   (median/lo/hi divided by the reference median)
# `long` has a Scenario column + numeric quantity columns (one row per sample);
# `q_info[[q]]` = list(label=, unit=) for the quantity headers.
.scenario_summary_table <- function(long, quantities, q_info, scenario_tbl,
                                    reference, percentiles = c(0.05, 0.95),
                                    lookup = NULL, relative = FALSE, digits = 2) {
  plo <- min(percentiles); phi <- max(percentiles)
  r2  <- function(x) ifelse(is.na(x), "NA", format(round(x, digits), trim = TRUE))
  scn <- as.character(long$Scenario)

  # scenario order: the scenario table's order (definition order), else `long`
  scen <- if (!is.null(scenario_tbl) && "Scenario" %in% names(scenario_tbl)) {
    as.character(scenario_tbl$Scenario)
  } else if (is.factor(long$Scenario)) {
    levels(long$Scenario)
  } else {
    unique(scn)
  }

  ref_med <- vapply(quantities, function(q)
    stats::median(long[[q]][scn == reference], na.rm = TRUE), numeric(1))

  fmt_cell <- function(x, q) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_character_)
    med <- stats::median(x)
    qs  <- stats::quantile(x, c(plo, phi), names = FALSE)
    if (relative) {
      d <- ref_med[[q]]
      paste0(r2(med / d), " [", r2(qs[1] / d), ", ", r2(qs[2] / d), "]")
    } else {
      pos <- x[x > 0]
      gm  <- if (length(pos)) exp(mean(log(pos))) else NA_real_
      gcv <- if (length(pos) > 1) sqrt(exp(stats::var(log(pos))) - 1) * 100 else NA_real_
      paste0(r2(med), "\n[", r2(qs[1]), ", ", r2(qs[2]), "]\n",
             r2(gm), "\n(", r2(gcv), "%)")
    }
  }

  out <- data.frame(Scenario = scen, stringsAsFactors = FALSE, check.names = FALSE)

  # leading covariate columns (decoded, unit headers) from the scenario table
  if (!is.null(scenario_tbl)) {
    idx <- match(scen, as.character(scenario_tbl$Scenario))
    for (cv in setdiff(names(scenario_tbl), "Scenario")) {
      e <- lookup[[cv]]
      short <- if (!is.null(e$short)) e$short else if (!is.null(e$label)) e$label else cv
      hdr   <- if (!is.null(e$unit) && nzchar(e$unit)) paste0(short, " [", e$unit, "]") else short
      raw   <- scenario_tbl[[cv]][idx]
      out[[hdr]] <- if (!is.null(e$values) && !is.null(e$decode)) {
        m <- match(as.character(raw), as.character(unlist(e$values)))
        ifelse(is.na(m), as.character(raw), as.character(unlist(e$decode))[m])
      } else {
        r2(as.numeric(raw))
      }
    }
  }

  # quantity columns
  for (q in quantities) {
    info <- q_info[[q]]
    lab  <- if (!is.null(info$label)) info$label else q
    hdr  <- if (!is.null(info$unit) && nzchar(info$unit)) paste0(lab, " [", info$unit, "]") else lab
    out[[hdr]] <- vapply(scen, function(s) fmt_cell(long[[q]][scn == s], q), character(1))
  }
  out
}


# ---- 3. Simulate concentration-time profiles ----------------------------------

#' Simulate Concentration-Time Profiles for Each Covariate Scenario
#'
#' @description
#' Runs each scenario's parameter draws through an mrgsolve model under a
#' user-defined dosing regimen, and stacks the resulting concentration-time
#' profiles tagged by scenario. Consumes the named list from
#' [build_scenario_parameters()]: each element is one scenario's per-sample
#' parameters, and every sample is simulated as an individual subject (via
#' mrgsolve `idata`) under the common dose (via `events`).
#'
#' The model must expose the parameters it needs as `$PARAM` (e.g.
#' `THETA1..THETAn`); columns of the parameter tables matching those names
#' override them per subject. Covariate effects are already baked into the
#' parameters upstream by [apply_covariate_model()], so the mrgsolve model must
#' be structural only (no covariate terms).
#'
#' @param param_sets Named list of data frames from [build_scenario_parameters()]
#'   (each: `ID` + structural `THETA` columns; the list names label the scenarios
#'   and are carried into a `Scenario` column of the output). Given as either the
#'   list itself OR a character path to an `.rds` file to load.
#' @param mod A loaded mrgsolve model object (from [mrgsolve::mread()]), compiled
#'   by the caller before calling this function.
#' @param dose An mrgsolve event object ([mrgsolve::ev()]) describing the dosing
#'   regimen, applied to every subject, e.g.
#'   `mrgsolve::ev(amt = 300, cmt = 1, ii = 12, addl = 5, ss = 1)`.
#' @param start,end,delta Numeric. Observation time grid for [mrgsolve::mrgsim()].
#'   Defaults `0`, `24`, `0.1`. For AUC over one steady-state interval set `end`
#'   to one dosing interval (`ii`).
#' @param verbose Logical. When `TRUE` (default) print a `[i/n] Simulating
#'   scenario: <name>` progress line as each scenario is simulated. Set `FALSE`
#'   to silence.
#'
#' @return A `data.frame` of stacked profiles - one row per
#'   (scenario, sample, time) - with a `Scenario` column (an ordered factor, in
#'   scenario definition order), `ID` (the sample), `time`, and the model's
#'   captured columns (e.g. `CP`, `IPRED`, `DV`), ready for metric derivation,
#'   e.g. `group_by(Scenario, ID) %>% summarise(AUC = ..., Cmax = ..., Cmin = ...)`.
#'
#' @section Memory:
#' Holds every time point for every sample in every scenario
#' (~`n_scenarios * Nsamples * n_times` rows). Fine for modest `Nsamples`; at
#' `1e5`, simulate and reduce to metrics one scenario at a time.
#'
#' @seealso [build_scenario_parameters()]
#' @examples
#' \dontrun{
#' mod  <- mrgsolve::mread("models/sim_model.cpp")   # loaded once, by the caller
#' dose <- mrgsolve::ev(amt = 300, cmt = 1, ii = 12, addl = 5, ss = 1)
#' profiles <- simulate_scenario_profiles(param_sets, mod, dose, end = 12)
#' }
#' @export
simulate_scenario_profiles <- function(param_sets,
                                        mod,
                                        dose,
                                        start = 0,
                                        end = 24,
                                        delta = 0.1,
                                        verbose = TRUE) {

  # param_sets may be given as an object OR a character path to an .rds to load.
  param_sets <- .load_if_path(param_sets, "param_sets")

  if (!is.list(param_sets) || is.null(names(param_sets))) {
    stop("`param_sets` must be a named list ",
         "(output of build_scenario_parameters()).")
  }
  if (!inherits(dose, "ev")) {
    stop("`dose` must be an mrgsolve event object in `ev` format ",
         "(created with mrgsolve::ev()).")
  }

  # Simulate each scenario's samples as a population, tag with the scenario name.
  # `output = "df"` makes mrgsim() return a plain data.frame directly: mrgsim()
  # otherwise returns an S4 `mrgsims` object, and a bare as.data.frame() on it
  # fails here because mrgsolve is only Suggested (used via ::), so inside this
  # package's namespace as.data.frame resolves to base's S3 generic, which cannot
  # dispatch to mrgsolve's S4 coercion method.
  n_scen <- length(param_sets)
  profiles <- purrr::imap_dfr(param_sets, function(params, scenario) {
    if (verbose) {
      message(sprintf("[%d/%d] Simulating scenario: %s",
                      match(scenario, names(param_sets)), n_scen, scenario))
    }
    out <- mrgsolve::mrgsim(
      mod,
      idata  = params,
      events = dose,
      start  = start,
      end    = end,
      delta  = delta,
      output = "df"
    )
    out$Scenario <- scenario
    out
  })

  # Keep the scenario definition order (typical subject first) as factor levels,
  # so it survives group_by()/summarise() and RDS round-trips - the forest plot
  # then orders the axis correctly with no scenario table or `scenario_order`.
  profiles$Scenario <- factor(profiles$Scenario, levels = names(param_sets))
  profiles
}


# ---- 3b. Stack scenario parameters (parameter-forest input) -------------------

# Map structural THETA positions to their $PK parameter names, e.g.
# c(THETA1 = "CL", THETA2 = "V2"). Mirrors apply_covariate_model()'s THETA->param
# logic (the line where THETA(p) is the leading theta -> LHS -> strip TV_). Used
# only for axis/title labels, so a position it cannot resolve is simply omitted
# (the caller keeps the THETAn name). Returns a named character vector.
.structural_param_names <- function(model_name, models_folder = "models") {
  base_path  <- file.path(models_folder, model_name)
  candidates <- c(paste0(base_path, ".ctl"), paste0(base_path, ".mod"),
                  file.path(base_path, paste0(model_name, ".ctl")),
                  file.path(base_path, paste0(model_name, ".mod")))
  ctl_path <- candidates[file.exists(candidates)]
  if (length(ctl_path) == 0) return(character(0))
  lines <- readLines(ctl_path[1], warn = FALSE)

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

  theta_names <- character(0)
  for (ln in get_block("THETA")) {
    if (grepl("^\\s*\\$", ln)) next
    parts <- strsplit(ln, ";")[[1]]
    if (!grepl("[0-9]", parts[1])) next
    theta_names <- c(theta_names, if (length(parts) >= 2) trimws(parts[2]) else "")
  }
  if (length(theta_names) == 0) return(character(0))
  structural_idx <- which(!grepl("^beta_", theta_names))

  pk <- get_block("PK")
  if (length(pk) == 0) pk <- get_block("PRED")
  if (length(pk) == 0) return(character(0))

  map <- character(0)
  for (p in structural_idx) {
    for (raw in pk) {
      ln <- trimws(sub(";.*$", "", raw))
      if (!grepl("=", ln)) next
      rhs <- sub("^[^=]*=", "", ln)
      if (grepl(paste0("THETA\\(\\s*", p, "\\s*\\)"), ln) &&
          identical(first_theta_index(rhs), as.integer(p))) {
        param <- sub("^TV_?", "", trimws(sub("=.*$", "", ln)))   # TV_CL/TVCL -> CL
        if (nzchar(param)) map[paste0("THETA", p)] <- param
        break
      }
    }
  }
  map
}

#' Stack Scenario Parameter Tables into One Long, Scenario-Tagged Data Frame
#'
#' @description
#' Turns the named list from [build_scenario_parameters()] into a single long
#' data frame with a `Scenario` column (an ordered factor in scenario-definition
#' order), ready to feed straight to [plot_exposure_forest()] for a **parameter
#' forest** - the covariate effect on a structural parameter (CL, V, ...), with
#' no simulation or exposure metrics. It is the pre-simulation counterpart of the
#' stacking [simulate_scenario_profiles()] does internally.
#'
#' When `model` is supplied, the `THETA1..THETAn` columns are renamed to the
#' model's parameter names (e.g. `THETA1 -> CL`) by reading its `$PK`, so the
#' forest axis/title read the real names. Positions that cannot be resolved keep
#' their `THETAn` name.
#'
#' @param param_sets Named list from [build_scenario_parameters()] (each element:
#'   `ID` + structural `THETA` columns), given as either the list itself OR a
#'   character path to an `.rds` file to load.
#' @param model Optional model name (as for [build_scenario_parameters()]); when
#'   supplied, THETA columns are relabelled to their `$PK` parameter names.
#' @param models_folder Character. Folder containing `model`. Default "models".
#'
#' @return A long `data.frame`: `ID`, the parameter columns (named `THETA1..` or
#'   the mapped parameter names), and an ordered-factor `Scenario` column.
#' @seealso [build_scenario_parameters()], [plot_exposure_forest()]
#' @examples
#' \dontrun{
#' params <- build_scenario_parameters("run19", covariate_search, thetas, data)
#' pf <- stack_scenario_parameters(params, model = "run19")
#' plot_exposure_forest(pf, metric = "CL", ss = FALSE, width = 6, height = 6)
#' }
#' @export
stack_scenario_parameters <- function(param_sets, model = NULL,
                                      models_folder = "models") {
  param_sets <- .load_if_path(param_sets, "param_sets")
  if (!is.list(param_sets) || is.null(names(param_sets))) {
    stop("`param_sets` must be a named list ",
         "(output of build_scenario_parameters()).")
  }

  long <- purrr::imap_dfr(param_sets, function(df, scenario) {
    df$Scenario <- scenario
    df
  })
  long$Scenario <- factor(long$Scenario, levels = names(param_sets))

  if (!is.null(model)) {
    map <- .structural_param_names(model, models_folder)
    hit <- names(map)[names(map) %in% names(long)]
    if (length(hit) > 0) names(long)[match(hit, names(long))] <- unname(map[hit])
  }
  long
}


# ---- 4. Exposure forest plot --------------------------------------------------

# Column names used inside dplyr / ggplot2 data-masking.
utils::globalVariables(c("Scenario", "VALUE", "NAME", "COLOR", "MIN", "MAX",
                         "band", "xmin", "xmax", "ymin", "ymax"))

#' Forest Plot of a Covariate Effect on an Exposure Metric
#'
#' @description
#' Builds a forest plot of one exposure metric's covariate effects: each
#' scenario's metric distribution is normalised to the typical subject's median
#' and shown as a box (2.5 / 25 / 50 / 75 / 97.5 percentiles), with a reference
#' line at 1 and a shaded clinical-relevance band. Consumes an
#' already-calculated metrics table (`ID`, `AUC`, `Cmax`, `Cmin`, `Scenario`) -
#' e.g. a `group_by(Scenario, ID) %>% summarise(...)` result, saved to RDS or in
#' memory.
#'
#' @param data A `Scenario` column and the metric column (`AUC`, `Cmax`, or
#'   `Cmin`), one row per sample per scenario. Given as either a `data.frame` OR
#'   a character path to a `.csv`/`.rds` file to load.
#' @param metric Character. The column of `data` to forest (its distribution is
#'   normalised to the reference scenario's median). Defaults to the exposure
#'   metrics `"AUC"`/`"Cmax"`/`"Cmin"`, but any numeric column works - e.g. a
#'   structural parameter column (`"CL"`, `"V2"`, ...) from
#'   [stack_scenario_parameters()] for a parameter forest.
#' @param ss Logical. Steady state? Adds an "ss" suffix to the metric in the
#'   title (e.g. "AUC" vs "AUCss"). Default `TRUE`.
#' @param ClinicalRelevanceLow,ClinicalRelevanceHigh Numeric. Bounds of the
#'   shaded (inner) clinical-relevance band (ratio scale). Defaults `0.8`,
#'   `1.25`.
#' @param outer_range Numeric length-2 vector, or `NULL`. Bounds of a second,
#'   wider shaded reference band (ratio scale) drawn behind the inner one.
#'   Default `c(0.5, 2)`; `NULL` draws only the inner clinical-relevance band.
#' @param reference Character. Scenario used as the reference (denominator of the
#'   ratio and highlighted colour). Default "Typical subject".
#' @param scenario Optional. The scenario table from [build_scenario_parameters()]
#'   (a `data.frame`, or a path to a saved `.rds`/`.csv`): its `Scenario` column
#'   sets the axis order and its `"typical_subject"` attribute supplies the
#'   subtitle - handy for exposure forests, whose metrics table (built with
#'   `group_by()`/`summarise()`) has lost both. May also be a plain character
#'   vector giving just the axis order. When `NULL` (default) the factor levels
#'   of `data$Scenario` are used if it is a factor (as
#'   [simulate_scenario_profiles()] sets them), else first-appearance order.
#' @param fontsize Numeric. Base font size. Default 9.
#' @param title Character or NULL. Plot title; when NULL it is built from
#'   `metric` and `ss` ("Covariate effects on <metric>[ss]").
#' @param typical_subject Controls the reference-subject subtitle under the
#'   title. `TRUE` (default) uses the ready-made string that
#'   [create_covariate_table()] / [build_scenario_parameters()] attach as
#'   `attr(data, "typical_subject")` (nothing shown if that attribute is
#'   absent); `FALSE`/`NULL` shows no subtitle; a character string is shown
#'   verbatim (custom text, e.g. "Typical subject: 70 kg male, ECOG 0").
#' @param filename Character or NULL. Base path for saved output; any extension
#'   is stripped and one file per `output_format` is written as
#'   `<stem>.<format>`. Missing parent folders are created. `NULL` (default)
#'   does not save.
#' @param output_format Character. Which format(s) to save when `filename` is
#'   given: one or both of `"emf"` and `"png"`. Default both. `.emf` is written
#'   with `devEMF::emf`, `.png` with the [ggplot2::ggsave()] default device.
#' @param metric_info Metric labels/units for the summary-table headers, as
#'   EITHER a named list `list(label=, unit=)` per metric (e.g.
#'   `AUC = list(label="AUCss", unit="ng*h/mL")`) OR a spec supplying them - a
#'   yspec object, a path to a spec YAML, or a spec-shaped list (each entry's
#'   `short` becomes the label, plus its `unit`). When supplied (with `filename`
#'   and a `scenario` data.frame), two summary tables covering **every** metric
#'   column of `data` are written next to the plot as
#'   `<model>-exposure-table-absolute.rds` and `…-relative.rds`. **Every metric
#'   must have a label** or the call stops (add it to the spec/list). Absolute
#'   cell = `median / [lo,hi] / geomean / (geoCV%)`; relative = `ratio [lo,hi]`
#'   vs the `reference` median. Covariate columns come from `scenario` (decoded
#'   via `lookup`/`spec`); interval at `percentiles`; 2 dp. No `metric_info` ->
#'   no table.
#' @param model Optional character used only to name the summary-table files
#'   (`<model>-exposure-table-*.rds`).
#' @param percentiles Numeric length-2. Interval for the summary tables (same for
#'   absolute and relative). Default `c(0.05, 0.95)`.
#' @param lookup,spec Covariate lookup list and/or yspec object (as in
#'   [create_covariate_table()]) used to decode categoricals and label the
#'   covariate columns of the summary tables.
#' @param width,height Numeric. Output size in inches. Mandatory (no default);
#'   required whenever `filename` is given.
#'
#' @return The ggplot object (invisibly saved to `filename` when supplied).
#'
#' @seealso [theme_forest()]
#' @examples
#' \dontrun{
#' raw <- readRDS("data/derived/exposure_metrics_run19.rds")
#' for (m in c("AUC", "Cmax", "Cmin")) {
#'   plot_exposure_forest(
#'     raw, metric = m, ss = TRUE,
#'     filename = paste0("results/run19/", m, "ss_forestplot"),
#'     width = 6, height = 6
#'   )
#' }
#' }
#' @import ggplot2
#' @export
plot_exposure_forest <- function(data,
                                 metric = c("AUC", "Cmax", "Cmin"),
                                 ss = TRUE,
                                 ClinicalRelevanceLow = 0.8,
                                 ClinicalRelevanceHigh = 1.25,
                                 outer_range = c(0.5, 2),
                                 reference = "Typical subject",
                                 scenario = NULL,
                                 fontsize = 9,
                                 title = NULL,
                                 typical_subject = TRUE,
                                 filename = NULL,
                                 output_format = c("emf", "png"),
                                 metric_info = NULL,
                                 model = NULL,
                                 percentiles = c(0.05, 0.95),
                                 lookup = NULL,
                                 spec = NULL,
                                 width,
                                 height) {

  # data may be given as an object OR a character path to load (.csv/.rds).
  data <- .load_if_path(data, "data")

  # `scenario` sets the axis order and, for exposure forests, carries the
  # typical-subject string. It may be the scenario table from
  # build_scenario_parameters (a data.frame, or a path to a saved .rds/.csv) --
  # its `Scenario` column gives the order and its "typical_subject" attribute the
  # subtitle -- OR a plain character vector of scenario names (order only). An
  # exposure metrics table built with group_by()/summarise() loses both, which
  # is why the scenario table is passed here.
  if (is.character(scenario) && length(scenario) == 1L && file.exists(scenario)) {
    scenario <- .load_if_path(scenario, "scenario")
  }
  scenario_order <- NULL
  if (is.data.frame(scenario)) {
    if ("Scenario" %in% names(scenario)) scenario_order <- scenario$Scenario
  } else if (!is.null(scenario)) {
    scenario_order <- as.character(scenario)          # plain order vector
  }

  # typical_subject: TRUE -> the "typical_subject" attribute on `data`, or on the
  # `scenario` table when `data` lacks it (the exposure case); FALSE/NULL -> no
  # subtitle; a character string -> shown verbatim (custom).
  if (isTRUE(typical_subject)) {
    typical_subject <- attr(data, "typical_subject")
    if (is.null(typical_subject) && is.data.frame(scenario)) {
      typical_subject <- attr(scenario, "typical_subject")
    }
  } else if (isFALSE(typical_subject)) {
    typical_subject <- NULL
  }

  # `metric` is any numeric column of `data` to forest (its distribution
  # normalised to the reference median). AUC/Cmax/Cmin are the defaults, but a
  # parameter column (e.g. from stack_scenario_parameters()) works just as well.
  metric <- as.character(metric)[1]
  output_format <- match.arg(output_format, c("emf", "png"), several.ok = TRUE)
  if (!all(c("Scenario", metric) %in% names(data))) {
    stop("`data` must contain a 'Scenario' column and a '", metric, "' column.")
  }
  if (!reference %in% data$Scenario) {
    stop("Reference scenario '", reference, "' not found in `data$Scenario`.")
  }

  # Reference median (typical subject) -> normalise the metric to a ratio.
  ref_val <- stats::median(data[[metric]][data$Scenario == reference],
                           na.rm = TRUE)
  if (!is.finite(ref_val) || ref_val == 0) {
    stop("Reference median for '", metric, "' at scenario '", reference,
         "' is missing or zero.")
  }

  # Y-axis scenario order: explicit `scenario_order` override, else the factor
  # levels of data$Scenario (set by simulate_scenario_profiles()), else the order
  # scenarios first appear in the data.
  scen_levels <- if (!is.null(scenario_order)) {
    as.character(scenario_order)
  } else if (is.factor(data$Scenario)) {
    levels(data$Scenario)
  } else {
    unique(as.character(data$Scenario))
  }

  d <- data
  d$VALUE <- d[[metric]] / ref_val
  d <- dplyr::group_by(d, Scenario)
  d <- dplyr::mutate(
    d,
    MIN   = as.numeric(stats::quantile(VALUE, 0.025, na.rm = TRUE)),
    MAX   = as.numeric(stats::quantile(VALUE, 0.975, na.rm = TRUE)),
    NAME  = Scenario,
    COLOR = dplyr::case_when(
      Scenario == reference ~ "Typical value",
      .default = "Parameter\nfor defined\ncategory"
    )
  )
  d <- dplyr::ungroup(d)

  ttl <- if (is.null(title)) {
    paste0("Covariate effects on ", metric, if (isTRUE(ss)) "ss" else "")
  } else {
    title
  }
  boxlabel <- paste0(
    "Change in ", metric, " relative to reference individual\n",
    "Box plots: 2.5% / 25% / 50% / 75% / 97.5%"
  )

  # Shaded reference band(s) (behind the boxes), mapped to `fill` so they get
  # legend swatches next to the boxplot colours. The inner band is the clinical-
  # relevance range; `outer_range` (when not NULL) adds a wider band drawn
  # underneath it (outer band listed first so it renders below the inner one).
  inner_lab <- paste0("Reference range\n", ClinicalRelevanceLow, "-",
                      ClinicalRelevanceHigh)
  if (!is.null(outer_range)) {
    if (length(outer_range) != 2L || !is.numeric(outer_range)) {
      stop("`outer_range` must be NULL or a numeric length-2 vector, e.g. c(0.5, 2).")
    }
    outer_lab <- paste0("Reference range\n", outer_range[1], "-", outer_range[2])
    bands <- data.frame(
      xmin = -Inf, xmax = Inf,
      ymin = c(outer_range[1], ClinicalRelevanceLow),
      ymax = c(outer_range[2], ClinicalRelevanceHigh),
      band = factor(c(outer_lab, inner_lab), levels = c(inner_lab, outer_lab))
    )
    fill_values <- stats::setNames(
      c("#185AA9", "#7AC36A", "darkgrey", "lightgrey"),
      c("Typical value", "Parameter\nfor defined\ncategory", inner_lab, outer_lab)
    )
  } else {
    bands <- data.frame(
      xmin = -Inf, xmax = Inf,
      ymin = ClinicalRelevanceLow, ymax = ClinicalRelevanceHigh,
      band = factor(inner_lab, levels = inner_lab)
    )
    fill_values <- stats::setNames(
      c("#185AA9", "#7AC36A", "lightgrey"),
      c("Typical value", "Parameter\nfor defined\ncategory", inner_lab)
    )
  }

  p <- ggplot() +
    theme_forest(base_size = fontsize) +
    geom_rect(data = bands,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                  fill = band),
              alpha = 0.4) +
    stat_summary(
      data = d, fun.data = boxquantile, geom = "boxplot",
      aes(x = NAME, y = VALUE, fill = COLOR),
      width = 0.2, alpha = 0.5, na.rm = TRUE
    ) +
    scale_fill_manual(NULL, values = fill_values,
                      breaks = names(fill_values)) +
    scale_x_discrete(limits = rev(scen_levels)) +
    geom_hline(yintercept = 1, linetype = 2) +
    theme(
      legend.position   = "bottom",
      legend.background = element_rect(linetype = "solid", color = "black")
    ) +
    ylab(boxlabel) +
    xlab(NULL) +
    coord_flip(ylim = c(min(d$MIN, na.rm = TRUE) - 0.2,
                        max(d$MAX, na.rm = TRUE) + 0.2)) +
    ggtitle(ttl, subtitle = if (!is.null(typical_subject) &&
                                nzchar(typical_subject)) typical_subject else NULL) +
    theme(
      plot.subtitle = element_text(size = fontsize - 2),   # a bit smaller than axes
      axis.text     = element_text(size = fontsize)
    )

  if (!is.null(filename)) {
    stem <- tools::file_path_sans_ext(filename)
    out_dir <- dirname(stem)                     # create the folder if missing
    if (nzchar(out_dir) && !dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }
    for (fmt in output_format) {
      out_file <- paste0(stem, ".", fmt)
      if (fmt == "emf") {
        ggsave(out_file, plot = p, device = devEMF::emf,
               width = width, height = height)
      } else {
        ggsave(out_file, plot = p, width = width, height = height)
      }
    }
  }

  # Scenario summary tables (absolute + relative), saved next to the plot when
  # `metric_info` (headers) and the scenario table are supplied. Built from all
  # metrics in `metric_info`, so the full multi-metric table is written even
  # though one metric is plotted (re-written harmlessly on each per-metric call).
  if (!is.null(metric_info) && !is.null(filename) && is.data.frame(scenario)) {
    lu <- lookup
    if (!is.null(spec)) {
      lu <- utils::modifyList(.spec_to_lookup(spec),
                              if (is.null(lookup)) list() else lookup)
    }
    qi <- .resolve_quantity_info(metric_info)             # list or spec -> label/unit
    metrics_tbl <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], "ID")
    .require_quantity_labels(qi, metrics_tbl, "metric_info")
    tdir <- dirname(stem)
    if (nzchar(tdir) && !dir.exists(tdir)) {
      dir.create(tdir, recursive = TRUE, showWarnings = FALSE)
    }
    base <- if (!is.null(model)) paste0(model, "-exposure-table") else "exposure-table"
    saveRDS(.scenario_summary_table(data, metrics_tbl, qi, scenario,
              reference, percentiles, lu, relative = FALSE),
            file.path(tdir, paste0(base, "-absolute.rds")))
    saveRDS(.scenario_summary_table(data, metrics_tbl, qi, scenario,
              reference, percentiles, lu, relative = TRUE),
            file.path(tdir, paste0(base, "-relative.rds")))
  }

  p
}


#' Save a Covariate Parameter Forest for Every Structural Parameter
#'
#' @description
#' Convenience driver that builds and saves a covariate **parameter forest** for
#' each structural parameter of a model, plus one combined PDF of them all. It
#' stacks the scenario parameter tables ([stack_scenario_parameters()], which
#' relabels the `THETA` columns to their `$PK` names) and forests each parameter
#' with [plot_exposure_forest()] - no simulation or exposure metrics.
#'
#' Files are written under `output_folder/<model>/` (created if missing): one
#' `<model>-forest-plot-<param>.emf`/`.png` per parameter, plus
#' `<model>-forest-plots.pdf` containing every parameter forest (one per page).
#'
#' @param param_sets Named list from [build_scenario_parameters()], given as
#'   either the list itself OR a character path to an `.rds` file to load.
#' @param model Character. Model name - used to relabel THETA columns via its
#'   `$PK`, to name the output subfolder, and in the file names.
#' @param parameters Optional character vector selecting which parameters to
#'   plot; `NULL` (default) plots every structural parameter column.
#' @param output_folder Base output directory; the model name is appended, so
#'   files land in `output_folder/<model>/`. Default `"results/figure"`.
#' @param output_format Character vector, subset of `c("emf","png")`; which
#'   per-parameter image format(s) to save. Default both. `.emf` via
#'   [devEMF::emf()], `.png` via the [ggplot2::ggsave()] default device.
#' @param combined_pdf Logical; if `TRUE` (default) also write a single
#'   multi-page PDF of every parameter forest to the same folder.
#' @param width,height Numeric. Figure size in inches. Default 6 x 6.
#' @param outer_range Passed to [plot_exposure_forest()]. `NULL` (default) draws
#'   only the clinical-relevance band on parameter forests (the wider 0.5-2 band
#'   is dropped); pass e.g. `c(0.5, 2)` to add it back.
#' @param typical_subject Reference-subject subtitle. `TRUE` (default) uses the
#'   string `build_scenario_parameters()` attached to `param_sets`; `FALSE`/`NULL`
#'   suppresses it; a character string is shown verbatim (custom). Passed on to
#'   [plot_exposure_forest()].
#' @param param_info Parameter labels/units for the summary-table headers - a
#'   named list `list(label=, unit=)` per parameter (e.g.
#'   `CL = list(label="CL", unit="L/h")`) OR a spec (yspec object / path /
#'   spec-shaped list, using each entry's `short` + `unit`). When supplied (with
#'   a `scenario` data.frame), two summary tables are written to the output
#'   folder: `<model>-parameter-table-absolute.rds` and `…-relative.rds`, with
#'   one column per **plotted** structural parameter (mirrors the forest).
#'   **Every plotted parameter must have a label** or the call stops (restrict
#'   with `parameters=` or add it). Same cell content as the exposure tables -
#'   see [plot_exposure_forest()]'s `metric_info`. No `param_info` -> no table.
#' @param scenario The scenario table from [build_scenario_parameters()], used
#'   for the summary tables' covariate columns.
#' @param reference Character. Reference scenario for the relative table's ratio
#'   denominator. Default "Typical subject".
#' @param percentiles Numeric length-2. Interval for the summary tables. Default
#'   `c(0.05, 0.95)`.
#' @param lookup,spec Covariate lookup list and/or yspec object used to decode
#'   categoricals and label the covariate columns of the summary tables.
#' @param models_folder Character. Folder containing `model`. Default "models".
#' @param verbose Logical; print progress. Default `TRUE`.
#' @param ... Further arguments passed to [plot_exposure_forest()] (e.g.
#'   `reference`, `ClinicalRelevanceLow`, `ClinicalRelevanceHigh`, `fontsize`).
#'
#' @return Invisibly, a named list of the ggplot objects (one per parameter).
#' @seealso [stack_scenario_parameters()], [plot_exposure_forest()]
#' @examples
#' \dontrun{
#' params <- build_scenario_parameters("run19", covariate_search, thetas, data)
#' plot_parameter_forests(params, model = "run19")
#' # -> results/figure/run19/run19-forest-plot-CL.emf/.png, ...,
#' #    results/figure/run19/run19-forest-plots.pdf
#' }
#' @export
plot_parameter_forests <- function(param_sets,
                                    model,
                                    parameters = NULL,
                                    output_folder = "results/figure",
                                    output_format = c("emf", "png"),
                                    combined_pdf = TRUE,
                                    width = 6,
                                    height = 6,
                                    outer_range = NULL,
                                    typical_subject = TRUE,
                                    param_info = NULL,
                                    scenario = NULL,
                                    reference = "Typical subject",
                                    percentiles = c(0.05, 0.95),
                                    lookup = NULL,
                                    spec = NULL,
                                    models_folder = "models",
                                    verbose = TRUE,
                                    ...) {
  output_format <- match.arg(output_format, c("emf", "png"), several.ok = TRUE)

  param_sets <- .load_if_path(param_sets, "param_sets")
  # typical_subject: TRUE (default) -> the string build_scenario_parameters
  # attached to param_sets; FALSE/NULL -> no subtitle; a string -> custom. The
  # stacked frame does not carry the attribute, so resolve to a string here and
  # pass that on.
  if (isTRUE(typical_subject)) {
    typical_subject <- attr(param_sets, "typical_subject")
  } else if (isFALSE(typical_subject)) {
    typical_subject <- NULL
  }

  stacked <- stack_scenario_parameters(param_sets, model = model,
                                       models_folder = models_folder)
  param_cols <- setdiff(names(stacked), c("ID", "Scenario"))
  if (!is.null(parameters)) {
    miss <- setdiff(parameters, param_cols)
    if (length(miss) > 0) {
      stop("Parameter(s) not found in the scenario tables: ",
           paste(miss, collapse = ", "),
           ". Available: ", paste(param_cols, collapse = ", "))
    }
    param_cols <- as.character(parameters)
  }
  if (length(param_cols) == 0) stop("No parameter columns found to plot.")

  # Files go to output_folder/<model>/ (create it, incl. a missing figure/).
  out_dir <- file.path(output_folder, model)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  if (verbose) {
    cat(sprintf("Parameter forests for %s: %d parameter(s) -> %s\n",
                model, length(param_cols), out_dir))
  }

  plots <- list()
  for (i in seq_along(param_cols)) {
    prm  <- param_cols[i]
    stem <- file.path(out_dir, paste0(model, "-forest-plot-", prm))
    plots[[prm]] <- plot_exposure_forest(
      data            = stacked,
      metric          = prm,
      ss              = FALSE,
      filename        = stem,
      output_format   = output_format,
      width           = width,
      height          = height,
      outer_range     = outer_range,
      typical_subject = typical_subject,
      ...
    )
    if (verbose) cat(sprintf("  [%d/%d] %s\n", i, length(param_cols), prm))
  }

  # One combined multi-page PDF of every parameter forest, same folder.
  if (isTRUE(combined_pdf)) {
    pdf_path <- file.path(out_dir, paste0(model, "-forest-plots.pdf"))
    if (verbose) cat(sprintf("  combined PDF -> %s\n", pdf_path))
    grDevices::cairo_pdf(pdf_path, width = width, height = height,
                         onefile = TRUE)
    for (prm in param_cols) print(plots[[prm]])
    grDevices::dev.off()
  }

  # Scenario summary tables (absolute + relative) for the parameters, saved in
  # the same folder, when `param_info` (headers) and the scenario table are given.
  if (!is.null(param_info) && is.data.frame(scenario)) {
    lu <- lookup
    if (!is.null(spec)) {
      lu <- utils::modifyList(.spec_to_lookup(spec),
                              if (is.null(lookup)) list() else lookup)
    }
    qi <- .resolve_quantity_info(param_info)              # list or spec -> label/unit
    # Table mirrors the forest: one column per plotted structural parameter.
    .require_quantity_labels(qi, param_cols, "param_info")
    base <- paste0(model, "-parameter-table")
    saveRDS(.scenario_summary_table(stacked, param_cols, qi, scenario,
              reference, percentiles, lu, relative = FALSE),
            file.path(out_dir, paste0(base, "-absolute.rds")))
    saveRDS(.scenario_summary_table(stacked, param_cols, qi, scenario,
              reference, percentiles, lu, relative = TRUE),
            file.path(out_dir, paste0(base, "-relative.rds")))
    if (verbose) cat(sprintf("  tables -> %s-{absolute,relative}.rds\n", base))
  }

  invisible(plots)
}


# -----------------------------------------------------------------------------
# Support functions
# -----------------------------------------------------------------------------

#' Forest-plot ggplot2 Theme
#'
#' @description A clean black-on-white `theme_bw()` variant used for the covariate
#'   / exposure forest plots (inward ticks, bordered panel, black text).
#' @param base_size Base font size. Default 12.
#' @param base_family Base font family. Default "".
#' @return A ggplot2 theme object to add to a plot.
#' @import ggplot2
#' @export
theme_forest <- function(base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      line             = element_line(colour = "black"),
      rect             = element_rect(fill = "white", colour = NA),
      text             = element_text(colour = "black"),
      axis.text        = element_text(size = rel(1), colour = "black"),
      axis.text.x      = element_text(margin = grid::unit(c(4, 4, 0, 4), "mm")),
      axis.text.y      = element_text(margin = grid::unit(c(4, 4, 4, 0), "mm")),
      axis.ticks       = element_line(colour = "black"),
      axis.ticks.length = grid::unit(-2, "mm"),
      legend.key       = element_rect(colour = NA),
      panel.border     = element_rect(colour = "black"),
      strip.background = element_rect(fill = "white", colour = NA),
      strip.text       = element_text(size = rel(1))
    )
}


#' Box summary at the 2.5 / 25 / 50 / 75 / 97.5 percentiles
#'
#' @description Summary function for [ggplot2::stat_summary()] with
#'   `geom = "boxplot"`: returns the box statistics named as ggplot expects
#'   (`ymin`, `lower`, `middle`, `upper`, `ymax`).
#' @param y Numeric vector.
#' @return A named numeric vector of length 5.
#' @keywords internal
#' @noRd
boxquantile <- function(y) {
  structure(
    stats::quantile(y, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE),
    names = c("ymin", "lower", "middle", "upper", "ymax")
  )
}
