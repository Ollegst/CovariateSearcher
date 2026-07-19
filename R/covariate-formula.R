# =============================================================================
# COVARIATE FORMULA REGISTRY
# File: R/covariate-formula.R
# Part of CovariateSearcher Package
#
# Single source of truth for covariate-effect math. Each (STATUS, FORMULA)
# entry bundles everything BOTH sides of the pipeline need, so the NONMEM write
# side and the R reconstruction side can never drift apart:
#   - nonmem      : renders the NONMEM factor string that model_add_cov appends
#                   to the structural parameter (write side)
#   - r_eval      : reconstructs the same factor numerically, used by
#                   apply_covariate_model in the forest-plot pipeline (read side)
#   - init        : initial THETA value model_add_cov writes for the beta
#   - categorical : TRUE only for forms written as a $PK IF/ELSEIF per-level
#                   block (cat.linear). Those keep their bespoke per-level logic
#                   in model_add_cov / apply_covariate_model; the registry only
#                   flags them so both sides agree on which branch to take.
#
# The built-ins below mirror the exact legacy output byte-for-byte. Users can
# add continuous forms (e.g. logit) at load time with register_covariate_formula().
#
# NOTE: this file is additive infrastructure. Until model_add_cov and
# apply_covariate_model are wired to get_covariate_formula(), nothing here is
# exercised and behaviour is unchanged.
# =============================================================================

# Load-time registry: key "status.formula" -> entry list.
.covariate_formula_registry <- new.env(parent = emptyenv())

#' Register a Covariate Formula
#'
#' @title Register a covariate-effect formula usable by both the NONMEM writer
#'   and the R reconstruction
#' @description Adds (or overrides) a covariate-effect form keyed on
#'   \code{(STATUS, FORMULA)}. Both \code{model_add_cov} (NONMEM write side) and
#'   \code{apply_covariate_model} (R read side) look the form up here, so a form
#'   registered once is applied consistently on both sides.
#' @param status Character. Covariate status, \code{"con"} or \code{"cat"}.
#' @param formula Character. Formula name, e.g. \code{"power"}, \code{"linear"},
#'   \code{"exponential"}, or a user name such as \code{"logit"}.
#' @param nonmem Function \code{(cova, ref, n)} returning the NONMEM factor
#'   string appended to the structural parameter (e.g.
#'   \code{" * (WT/70)**THETA(5)"}). \code{cova} is the covariate column name,
#'   \code{ref} the reference value, \code{n} the THETA number.
#' @param r_eval Function \code{(cov_val, ref, th)} returning the numeric factor
#'   for a covariate value \code{cov_val}, reference \code{ref} and estimated
#'   beta \code{th}. Must reproduce the math in \code{nonmem}.
#' @param init Character. Initial THETA value written for the beta
#'   (default \code{"0.1"}; may include \code{"FIX"}).
#' @param categorical Logical. \code{TRUE} only for multi-level categorical
#'   forms written as a \code{$PK} \code{IF/ELSEIF} block (\code{nonmem}/
#'   \code{r_eval} are then unused). Default \code{FALSE}.
#' @return Invisibly, the registry key.
#' @export
register_covariate_formula <- function(status, formula, nonmem = NULL,
                                        r_eval = NULL, init = "0.1",
                                        categorical = FALSE) {
  status  <- tolower(as.character(status))
  formula <- tolower(as.character(formula))
  key <- paste(status, formula, sep = ".")
  assign(key, list(
    status      = status,
    formula     = formula,
    nonmem      = nonmem,
    r_eval      = r_eval,
    init        = init,
    categorical = categorical
  ), envir = .covariate_formula_registry)
  invisible(key)
}

# Look up a covariate formula entry. Returns NULL if the (status, formula)
# combination is not registered (callers decide how to handle that).
get_covariate_formula <- function(status, formula) {
  key <- paste(tolower(as.character(status)), tolower(as.character(formula)), sep = ".")
  if (!exists(key, envir = .covariate_formula_registry, inherits = FALSE)) {
    return(NULL)
  }
  get(key, envir = .covariate_formula_registry, inherits = FALSE)
}

#' List Registered Covariate Formulas
#'
#' @title List the registered covariate-effect forms
#' @description Returns the \code{"status.formula"} keys currently registered.
#' @return Character vector of registry keys, sorted.
#' @export
list_covariate_formulas <- function() {
  sort(ls(.covariate_formula_registry))
}

# ---- Built-in continuous forms (byte-exact mirror of legacy output) -----------

# power:   * (COV/ref)**THETA(n)      init "0.1"
.cov_pow_nonmem <- function(cova, ref, n) paste0(' * (', cova, '/', ref, ')**THETA(', n, ')')
.cov_pow_reval  <- function(cov_val, ref, th) (cov_val / ref) ^ th
register_covariate_formula("con", "power",
                           nonmem = .cov_pow_nonmem, r_eval = .cov_pow_reval, init = "0.1")

# linear:   * (1 + (COV-ref) * THETA(n))      init "0.1"
register_covariate_formula(
  "con", "linear",
  nonmem = function(cova, ref, n) paste0(' * (1 + (', cova, '-', ref, ') * THETA(', n, '))'),
  r_eval = function(cov_val, ref, th) 1 + (cov_val - ref) * th,
  init   = "0.1"
)

# exponential:   * EXP(THETA(n) * (COV-ref))     init "0.1"
register_covariate_formula(
  "con", "exponential",
  nonmem = function(cova, ref, n) paste0(' * EXP(THETA(', n, ') * (', cova, '-', ref, '))'),
  r_eval = function(cov_val, ref, th) exp(th * (cov_val - ref)),
  init   = "0.1"
)

# cat.power: a covariate with discrete but NUMERIC levels (e.g. dose
# 35/70/125/150 mg) modelled as a power relationship on the actual values.
# Identical rendering to con.power and NOT categorical -- this is what keeps the
# read side (apply_covariate_model) from mistreating it as an IF/ELSEIF block.
register_covariate_formula("cat", "power",
                           nonmem = .cov_pow_nonmem, r_eval = .cov_pow_reval, init = "0.1")

# cat.linear: multi-level categorical written as a $PK IF/ELSEIF per-level block
# (one beta per non-reference level). Flagged categorical; the per-level logic
# stays in model_add_cov (writer) and apply_covariate_model (reader).
register_covariate_formula("cat", "linear", categorical = TRUE)
