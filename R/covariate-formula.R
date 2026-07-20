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
# Both sides of the pipeline consume this registry: the NONMEM writer
# (model_add_cov) via get_covariate_formula() / detect_param_transform() /
# nonmem_log() / parse_named_init(), and the R reconstruction (apply_covariate_model)
# via get_covariate_formula(). Register a form once here and it applies
# consistently on both sides.
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
#'   string appended MULTIPLICATIVELY to a normal-scale typical value (e.g.
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
#' @param nonmem_log Function \code{(cova, ref, n)} returning the ADDITIVE term
#'   appended to a LOG-scale typical value (e.g. \code{" + THETA(5)*LOG(WT/70)"}),
#'   used when a time-constant covariate lands on a log-parameterized parameter
#'   (\code{PARAM = EXP(TV + ETA)}). \code{NULL} (the default, and for user
#'   expressions) means no additive form is available, so the covariate is written
#'   multiplicatively regardless of scale.
#' @return Invisibly, the registry key.
#' @export
register_covariate_formula <- function(status, formula, nonmem = NULL,
                                        r_eval = NULL, init = "0.1",
                                        categorical = FALSE, nonmem_log = NULL) {
  status  <- tolower(as.character(status))
  formula <- tolower(as.character(formula))
  key <- paste(status, formula, sep = ".")
  assign(key, list(
    status      = status,
    formula     = formula,
    nonmem      = nonmem,
    r_eval      = r_eval,
    init        = init,
    categorical = categorical,
    nonmem_log  = nonmem_log
  ), envir = .covariate_formula_registry)
  invisible(key)
}

# Look up a covariate formula entry. Returns NULL if the (status, formula)
# combination is not registered (callers decide how to handle that).
get_covariate_formula <- function(status, formula) {
  key <- paste(tolower(as.character(status)), tolower(as.character(formula)), sep = ".")
  if (exists(key, envir = .covariate_formula_registry, inherits = FALSE)) {
    return(get(key, envir = .covariate_formula_registry, inherits = FALSE))
  }
  # Not a built-in name: treat FORMULA as a single-factor expression
  # (e.g. "IMAX*cov/(IC50+cov)"). Returns NULL if it is not a valid expression.
  parse_covariate_expression(formula)
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

# ---- Parameter transformation detection ---------------------------------------
# Classify how a structural parameter carries its between-subject random effect,
# which decides whether a population (time-constant) covariate is written
# multiplicatively or additively:
#   "normal" -> PARAM = TV * EXP(ETA)          (typical value on natural scale)
#   "log"    -> PARAM = EXP(TV + ETA)          (typical value on log scale)
#   "unknown"-> an ETA that is neither (exotic; callers reject it)
# Robust to models that already carry covariates: it inspects the EXP() that
# directly wraps the parameter's ETA and asks whether a typical-value term
# (anything besides ETA) sits inside that SAME EXP.

# Return the content inside the first EXP(...) whose body contains an ETA(...),
# paren-balanced; NA if the parameter's ETA is not wrapped in an EXP at all.
.exp_inner_containing_eta <- function(rhs) {
  starts <- gregexpr("EXP\\(", rhs, ignore.case = TRUE)[[1]]
  if (length(starts) == 0L || starts[1] == -1L) return(NA_character_)
  chars <- strsplit(rhs, "")[[1]]; n <- length(chars)
  for (s in starts) {
    open  <- s + 3L                          # position of '(' in "EXP("
    depth <- 0L; end <- NA_integer_
    for (i in open:n) {
      if (chars[i] == "(") depth <- depth + 1L
      else if (chars[i] == ")") { depth <- depth - 1L; if (depth == 0L) { end <- i; break } }
    }
    if (is.na(end)) next
    inner <- paste(chars[(open + 1L):(end - 1L)], collapse = "")
    if (grepl("\\bETA\\(", inner)) return(inner)   # \b so THETA( is not matched as ETA(
  }
  NA_character_
}

# Classify parameter `param`'s transformation from the model code lines.
detect_param_transform <- function(modelcode, param) {
  idx <- grep(paste0("^\\s*", param, "\\b\\s*=.*\\bETA\\("), modelcode)
  if (length(idx) == 0L) return("normal")    # no IIV on this parameter -> multiplicative default
  line  <- modelcode[idx[length(idx)]]
  rhs   <- trimws(sub(";.*$", "", sub("^[^=]*=", "", line)))
  inner <- .exp_inner_containing_eta(rhs)
  if (is.na(inner)) return("unknown")        # ETA present but not inside EXP() -> exotic
  # Is there a typical-value term (anything besides ETA) inside the same EXP?
  wo <- gsub("\\bETA\\(\\s*\\d+\\s*\\)", "", inner)
  wo <- gsub("[-+*/^() \t]", "", wo)
  if (nzchar(wo)) "log" else "normal"
}

# ---- Expression formulas ------------------------------------------------------
# A FORMULA that is not a built-in name is treated as a single-factor expression
# written in terms of reserved symbols `cov` (the covariate) and optionally `ref`
# (its REFERENCE); EVERY other symbol is an estimated parameter (a THETA).
# Example: "IMAX*cov/(IC50+cov)" -> thetas IMAX, IC50 (order of appearance).

# Operators + math functions an expression may contain. Anything else -- including
# `if`/`ifelse` -- is rejected, which keeps expressions strictly single-factor.
.COV_EXPR_ALLOWED <- c("(", "+", "-", "*", "/", "^",
                       "exp", "log", "log10", "sqrt")

#' Parse a covariate-effect expression into a formula def (or NULL if invalid).
#' @keywords internal
#' @noRd
parse_covariate_expression <- function(formula) {
  formula <- trimws(as.character(formula))
  if (length(formula) != 1L || is.na(formula) || formula == "") return(NULL)

  expr <- tryCatch(parse(text = formula)[[1]], error = function(e) NULL)
  if (is.null(expr)) return(NULL)

  syms   <- all.vars(expr)
  thetas <- setdiff(syms, c("cov", "ref"))
  if (length(thetas) == 0L) return(NULL)                  # no estimated parameter

  used <- setdiff(unique(all.names(expr)), syms)          # function/operator names
  if (length(setdiff(used, .COV_EXPR_ALLOWED)) > 0L) return(NULL)  # disallowed fn

  list(
    status      = NA_character_,
    formula     = formula,
    expr        = expr,
    theta_names = thetas,
    categorical = FALSE,
    init        = "0.1",
    nonmem = function(cova, ref, n) {
      paste0(" * (", .translate_expr_to_nonmem(expr, cova, ref, thetas, n), ")")
    },
    r_eval = function(cov_val, ref, th) {
      # `th` holds the estimated beta(s). For one parameter it is the beta itself
      # (a scalar or a per-sample vector); for several it is a list, one element
      # per parameter (in theta_names order), each a scalar or per-sample vector.
      env <- list(cov = cov_val, ref = ref)
      if (length(thetas) == 1L) {
        env[[thetas[1L]]] <- th
      } else {
        for (i in seq_along(thetas)) env[[thetas[i]]] <- th[[i]]
      }
      eval(expr, envir = env)
    }
  )
}

# Render a parsed expression to a NONMEM factor string: cov -> covariate variable,
# ref -> its numeric value, parameter symbols -> THETA(n), THETA(n+1), ...,
# math functions -> uppercase, and `^` -> `**`.
.translate_expr_to_nonmem <- function(expr, cova, ref, thetas, n) {
  walk <- function(node) {
    if (is.symbol(node)) {
      nm <- as.character(node)
      if (nm == "cov") return(as.symbol(cova))
      if (nm == "ref") return(ref)
      idx <- match(nm, thetas)
      if (!is.na(idx)) return(str2lang(paste0("THETA(", n + idx - 1L, ")")))
      return(node)
    }
    if (is.call(node)) {
      fn     <- as.character(node[[1]])
      fn_out <- if (fn %in% c("exp", "log", "log10", "sqrt")) toupper(fn) else fn
      return(as.call(c(as.symbol(fn_out), lapply(as.list(node)[-1], walk))))
    }
    node
  }
  s <- paste(deparse(walk(expr), width.cutoff = 500L), collapse = " ")
  gsub("\\^", "**", s)
}

# Parse a per-parameter INIT spec ("EMAX=0.1; EC50=(0,10,1000)") into a character
# vector of $THETA init strings aligned to `theta_names`; missing entries default
# to "0.1". Names must match the expression's parameter names.
#' @keywords internal
#' @noRd
parse_named_init <- function(init_str, theta_names) {
  out <- stats::setNames(rep("0.1", length(theta_names)), theta_names)
  if (length(init_str) != 1L || is.na(init_str) ||
      trimws(as.character(init_str)) == "") {
    return(unname(out))
  }
  for (part in strsplit(as.character(init_str), ";", fixed = TRUE)[[1]]) {
    part <- trimws(part)
    if (part == "" || !grepl("=", part, fixed = TRUE)) next
    nm  <- trimws(sub("=.*$", "", part))
    val <- trimws(sub("^[^=]*=", "", part))
    if (nm %in% theta_names && val != "") out[[nm]] <- val
  }
  unname(out)
}

# ---- Built-in continuous forms (byte-exact mirror of legacy output) -----------

# For a log-scale (population) parameter the same effect is written ADDITIVELY on
# the log typical value: multiplicative factor f -> additive term LOG(f), which
# for the built-ins simplifies to the closed forms below.
# power:   normal  * (COV/ref)**THETA(n)   |  log  + THETA(n)*LOG(COV/ref)
.cov_pow_nonmem <- function(cova, ref, n) paste0(' * (', cova, '/', ref, ')**THETA(', n, ')')
.cov_pow_logadd <- function(cova, ref, n) paste0(' + THETA(', n, ')*LOG(', cova, '/', ref, ')')
.cov_pow_reval  <- function(cov_val, ref, th) (cov_val / ref) ^ th
register_covariate_formula("con", "power",
                           nonmem = .cov_pow_nonmem, r_eval = .cov_pow_reval, init = "0.1",
                           nonmem_log = .cov_pow_logadd)

# linear:   normal  * (1 + (COV-ref) * THETA(n))   |  log  + LOG(1 + (COV-ref) * THETA(n))
register_covariate_formula(
  "con", "linear",
  nonmem     = function(cova, ref, n) paste0(' * (1 + (', cova, '-', ref, ') * THETA(', n, '))'),
  nonmem_log = function(cova, ref, n) paste0(' + LOG(1 + (', cova, '-', ref, ') * THETA(', n, '))'),
  r_eval = function(cov_val, ref, th) 1 + (cov_val - ref) * th,
  init   = "0.1"
)

# exponential:   normal  * EXP(THETA(n) * (COV-ref))   |  log  + THETA(n) * (COV-ref)
register_covariate_formula(
  "con", "exponential",
  nonmem     = function(cova, ref, n) paste0(' * EXP(THETA(', n, ') * (', cova, '-', ref, '))'),
  nonmem_log = function(cova, ref, n) paste0(' + THETA(', n, ') * (', cova, '-', ref, ')'),
  r_eval = function(cov_val, ref, th) exp(th * (cov_val - ref)),
  init   = "0.1"
)

# cat.power: a covariate with discrete but NUMERIC levels (e.g. dose
# 35/70/125/150 mg) modelled as a power relationship on the actual values.
# Identical rendering to con.power and NOT categorical -- this is what keeps the
# read side (apply_covariate_model) from mistreating it as an IF/ELSEIF block.
register_covariate_formula("cat", "power",
                           nonmem = .cov_pow_nonmem, r_eval = .cov_pow_reval, init = "0.1",
                           nonmem_log = .cov_pow_logadd)

# cat.linear: multi-level categorical written as a $PK IF/ELSEIF per-level block
# (one beta per non-reference level). Flagged categorical; the per-level logic
# stays in model_add_cov (writer) and apply_covariate_model (reader).
register_covariate_formula("cat", "linear", categorical = TRUE)
