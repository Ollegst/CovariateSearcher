# =============================================================================
# SAMPLE THETA UNCERTAINTY
# File: R/sample-theta-uncertainty.R
# Part of CovariateSearcher Package
# STEP 1 of the exposure-metrics forest-plot workflow.
# =============================================================================

#' Sample THETA Parameter Vectors from Estimation Uncertainty
#'
#' @description
#' Draws `Nsamples` vectors of the model's THETA parameters (structural
#' parameters + covariate betas) from the NONMEM estimation uncertainty, and
#' returns them **in absolute (natural) scale** so they can be fed directly into
#' [apply_covariate_model()] and, later, a simulation engine.
#'
#' Sampling is done on the estimation scale that the covariance matrix describes:
#' \itemize{
#'   \item If a `.cov` (or `.cor`) file is available, draws come from the full
#'         multivariate-normal uncertainty distribution, preserving parameter
#'         correlations (via \pkg{mvtnorm}).
#'   \item If neither is available or usable, falls back to sampling each THETA
#'         independently from its `.ext` standard error, with a warning.
#' }
#' After sampling, each THETA is mapped to absolute scale using its `$THETA`
#' transformation field, read with the package's own `extract_model_params()`
#' (the same reader used for the model output tables): a `LOG`-tagged parameter
#' is exponentiated (`exp()`), while `RATIO`/untagged parameters are left
#' unchanged. This yields a log-normal (always-positive) draw for log-estimated
#' structural parameters. Covariate betas are written as `; RATIO`, so they pass
#' through untouched.
#'
#' @param model Character. Model name without extension, e.g. "run28".
#' @param models_folder Character. Folder containing the model. Default "models".
#'   Both flat (`models/run28.ext`) and per-model-subfolder
#'   (`models/run28/run28.ext`) layouts are supported.
#' @param Nsamples Integer. Number of uncertainty draws. Default 1e5.
#' @param seed Integer. Random seed for reproducibility. Default 1234.
#'
#' @return A `data.frame` with `Nsamples` rows and columns `ID` (1:Nsamples) and
#'   `THETA1`, `THETA2`, ..., `THETAn` in absolute scale, with THETA order
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

  # ---- Back-transform each THETA to absolute scale --------------------------
  # Reuse the package's own $THETA reader (parameter name + transformation) -
  # the same one that builds the model output tables. `trans` is per-THETA in
  # $THETA order, so it aligns positionally with the .ext THETA columns exactly
  # as `get_model_parameters_and_statistics()` relies on.
  trans <- extract_model_params(model, models_folder)$THETAS$trans
  if (length(trans) != n_theta) {
    warning("Parsed ", length(trans), " $THETA transformation tag(s) but the ",
            ".ext reports ", n_theta, " THETA(s); treating all as natural scale ",
            "(no log back-transform).")
    trans <- rep(NA_character_, n_theta)
  }
  for (j in which(trans == "LOG")) {
    samples[, j] <- exp(samples[, j])
  }

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
