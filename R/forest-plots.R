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
#'   (each: `ID` + structural `THETA` columns). The list names label the
#'   scenarios and are carried into a `Scenario` column of the output.
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
#' @param data Data frame with a `Scenario` column and the metric column
#'   (`AUC`, `Cmax`, or `Cmin`), one row per sample per scenario.
#' @param metric Character. Which metric to plot: "AUC", "Cmax", or "Cmin".
#' @param ss Logical. Steady state? Adds an "ss" suffix to the metric in the
#'   title (e.g. "AUC" vs "AUCss"). Default `TRUE`.
#' @param ClinicalRelevanceLow,ClinicalRelevanceHigh Numeric. Bounds of the
#'   shaded clinical-relevance band (ratio scale). Defaults `0.8`, `1.25`.
#' @param reference Character. Scenario used as the reference (denominator of the
#'   ratio and highlighted colour). Default "Typical subject".
#' @param scenario_order Optional character vector giving the scenario order on
#'   the axis, overriding the default. When `NULL` (default), the factor levels
#'   of `data$Scenario` are used if it is a factor - which
#'   [simulate_scenario_profiles()] sets to the scenario definition order, so the
#'   correct order flows through automatically - otherwise the order scenarios
#'   first appear in `data`.
#' @param fontsize Numeric. Base font size. Default 9.
#' @param title Character or NULL. Plot title; when NULL it is built from
#'   `metric` and `ss` ("Covariate effects on <metric>[ss]").
#' @param filename Character or NULL. Base path for saved output; any extension
#'   is stripped and one file per `output_format` is written as
#'   `<stem>.<format>`. `NULL` (default) does not save.
#' @param output_format Character. Which format(s) to save when `filename` is
#'   given: one or both of `"emf"` and `"png"`. Default both. `.emf` is written
#'   with `devEMF::emf`, `.png` with the [ggplot2::ggsave()] default device.
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
                                 reference = "Typical subject",
                                 scenario_order = NULL,
                                 fontsize = 9,
                                 title = NULL,
                                 filename = NULL,
                                 output_format = c("emf", "png"),
                                 width,
                                 height) {

  metric <- match.arg(metric)
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

  # Shaded reference bands (behind the boxes), mapped to `fill` so they get
  # legend swatches next to the boxplot colours. Outer band listed first so it is
  # drawn underneath the (narrower) inner band.
  inner_lab <- paste0("Reference range\n", ClinicalRelevanceLow, "-",
                      ClinicalRelevanceHigh)
  outer_lab <- "Reference range\n0.5-2"
  bands <- data.frame(
    xmin = -Inf, xmax = Inf,
    ymin = c(0.5, ClinicalRelevanceLow),
    ymax = c(2,   ClinicalRelevanceHigh),
    band = factor(c(outer_lab, inner_lab), levels = c(inner_lab, outer_lab))
  )
  fill_values <- stats::setNames(
    c("#185AA9", "#7AC36A", "darkgrey", "lightgrey"),
    c("Typical value", "Parameter\nfor defined\ncategory", inner_lab, outer_lab)
  )

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
    ggtitle(ttl) +
    theme(
      plot.subtitle = element_text(size = fontsize),
      axis.text     = element_text(size = fontsize)
    )

  if (!is.null(filename)) {
    stem <- tools::file_path_sans_ext(filename)
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

  p
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
