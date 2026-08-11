# =============================================================================
# COVARIATE FORMULA REGISTRY
# File: R/covariate-formula.R
# Part of CovariateSearcher Package
#
# Source of truth for how model_add_cov writes covariate effects into NONMEM.
# Each (STATUS, FORMULA) entry bundles what the writer needs:
#   - nonmem      : the NONMEM factor string model_add_cov appends to a
#                   normal-scale typical value (multiplicative)
#   - nonmem_log  : the additive term for a log-scale typical value (see below)
#   - init        : initial THETA value model_add_cov writes for the beta
#   - categorical : TRUE only for forms written as a $PK IF/ELSEIF per-level
#                   block (cat.linear), which keep their bespoke per-level logic
#                   in model_add_cov; the registry just flags them.
#
# The built-ins below mirror the exact legacy output byte-for-byte. The registry
# is internal and closed: a FORMULA that is not one of these names is parsed as a
# user expression instead (see get_covariate_formula).
#
# Consumed by the WRITE side model_add_cov (via get_covariate_formula() /
# detect_param_transform() / nonmem_log() / parse_named_init()) and by
# calculate_covariate_df (theta count / INIT / categorical). The forest READ side
# (apply_covariate_model) evaluates the model's $PK equations directly and no
# longer uses this registry.
# =============================================================================

# Load-time registry: key "status.formula" -> entry list.
.covariate_formula_registry <- new.env(parent = emptyenv())

#' Register a Covariate Formula
#'
#' @title Declare a built-in covariate-effect form
#' @description Stores a covariate-effect form in the registry under
#'   \code{(STATUS, FORMULA)}. Called at load time to declare the built-ins
#'   below; the entries are then read by \code{model_add_cov} (to write the
#'   covariate into the control stream) and \code{calculate_covariate_df} (for
#'   the LRT degrees of freedom).
#' @param status Character. Covariate status, \code{"con"} or \code{"cat"}.
#' @param formula Character. Formula name: \code{"power"}, \code{"linear"},
#'   \code{"exponential"}.
#' @param nonmem Function \code{(cova, ref, n)} returning the NONMEM factor
#'   string appended MULTIPLICATIVELY to a normal-scale typical value (e.g.
#'   \code{" * (WT/70)**THETA(5)"}). \code{cova} is the covariate column name,
#'   \code{ref} the reference value, \code{n} the THETA number.
#' @param init Character. Initial THETA value written for the beta
#'   (default \code{"0.1"}; may include \code{"FIX"}).
#' @param categorical Logical. \code{TRUE} only for multi-level categorical
#'   forms written as a \code{$PK} \code{IF/ELSEIF} block (\code{nonmem} is then
#'   unused). Default \code{FALSE}.
#' @param nonmem_log Function \code{(cova, ref, n)} returning the ADDITIVE term
#'   appended to a LOG-scale typical value (e.g. \code{" + THETA(5)*LOG(WT/70)"}),
#'   used when a time-constant covariate lands on a log-parameterized parameter
#'   (\code{PARAM = EXP(TV + ETA)}). \code{NULL} (the default) means no additive
#'   form is available, so the covariate is written multiplicatively regardless
#'   of scale.
#' @return Invisibly, the registry key.
#' @keywords internal
#' @noRd
register_covariate_formula <- function(status, formula, nonmem = NULL,
                                        init = "0.1",
                                        categorical = FALSE, nonmem_log = NULL) {
  status  <- tolower(as.character(status))
  formula <- tolower(as.character(formula))
  key <- paste(status, formula, sep = ".")
  assign(key, list(
    status      = status,
    formula     = formula,
    nonmem      = nonmem,
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
#'   Used in the "unknown formula" error message.
#' @return Character vector of registry keys, sorted.
#' @keywords internal
#' @noRd
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
#
# MU-referencing of a genuine TV_PARAM line (MU_n = LOG(TV_PARAM); PARAM =
# EXP(MU_n+ETA)) looks identical to that structurally -- a typical-value term
# riding alongside ETA inside EXP() -- but is algebraically EXP(LOG(TV)+ETA) ==
# TV*EXP(ETA), the "normal" space, not "log". validate_param_transformations
# already forbids the one case that would make this ambiguous (TV_PARAM itself
# written as EXP(THETA(n))), so when the EXP()-riding symbol turns out to be a
# MU reference specifically to TV_PARAM, the parameter's own
# `$THETA ; NAME ; units ; RATIO|LOG` tag (required by validate_parameter_blocks
# when initialize_covariate_search runs with validate_parameters = TRUE) is the
# disambiguator instead of assuming "log". A MU reference to anything OTHER than
# TV_PARAM (e.g. MU_n = LOG(THETA(n)) with no separate TV_ line at all) is left
# alone: that is genuinely "log" and must still trip validate_param_transformations'
# no_tv check, since there is no TV_ line for model_add_cov to append to.

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

# Exact-name lookup of the "value ; NAME ; units ; RATIO|LOG" comment tag that
# validate_parameter_blocks requires on every $THETA/$OMEGA/$SIGMA line. Returns
# "RATIO", "LOG", or NA if `param` has no such line.
.theta_tag_for <- function(modelcode, param) {
  for (line in modelcode[grepl(";", modelcode, fixed = TRUE)]) {
    fields <- trimws(strsplit(sub("^[^;]*", "", line), ";")[[1]])
    fields <- fields[nzchar(fields)]
    if (length(fields) >= 2 && identical(fields[1], param)) {
      tag <- toupper(fields[length(fields)])
      if (tag %in% c("RATIO", "LOG")) return(tag)
    }
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
  if (!nzchar(wo)) return("normal")
  # MU-referencing of a genuine TV_<param> line specifically: `wo`'s own line
  # must be "wo = LOG(TV_<param>)", not LOG() of anything else (e.g. a raw
  # THETA(n), which would mean there is no separate typical-value line at all).
  # Also require a line-start "TV_<param> = ..." assignment -- the SAME
  # criterion model_add_cov uses to find where to write the covariate and
  # validate_param_transformations' no_tv check uses to validate it exists.
  # A conditionally-assigned TV (e.g. "IF(SEX.EQ.1) TV_CL = THETA(1)", never
  # starting the line with "TV_CL") would satisfy the text pattern above but
  # not this one, and reclassifying that as "normal" would make model_add_cov
  # silently fall back to writing the covariate onto the PARAM = EXP(MU+ETA)
  # line itself, destroying the MU-referenced form -- and skip the no_tv stop()
  # that exists precisely to catch a log parameter with nowhere safe to write.
  mu_line <- grep(
    paste0("^\\s*", wo, "\\b\\s*=\\s*LOG\\(\\s*TV_", param, "\\s*\\)"),
    modelcode, ignore.case = TRUE, value = TRUE
  )
  has_tv_line <- length(grep(paste0("^\\s*TV_", param, "\\b"), modelcode)) > 0L
  if (length(mu_line) > 0L && has_tv_line) {
    tag <- .theta_tag_for(modelcode, param)
    if (!is.na(tag)) return(if (tag == "RATIO") "normal" else "log")
  }
  "log"
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
    # On a log-scale typical value the expression is JOINED with '+' instead of
    # '*'. The expression itself is never transformed: writing it on the log
    # scale (e.g. log(EMAX*cov/(EC50+cov))) is the user's responsibility.
    nonmem_log = function(cova, ref, n) {
      paste0(" + (", .translate_expr_to_nonmem(expr, cova, ref, thetas, n), ")")
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
register_covariate_formula("con", "power",
                           nonmem = .cov_pow_nonmem, init = "0.1",
                           nonmem_log = .cov_pow_logadd)

# linear:   normal  * (1 + (COV-ref) * THETA(n))   |  log  + LOG(1 + (COV-ref) * THETA(n))
register_covariate_formula(
  "con", "linear",
  nonmem     = function(cova, ref, n) paste0(' * (1 + (', cova, '-', ref, ') * THETA(', n, '))'),
  nonmem_log = function(cova, ref, n) paste0(' + LOG(1 + (', cova, '-', ref, ') * THETA(', n, '))'),
  init   = "0.1"
)

# exponential:   normal  * EXP(THETA(n) * (COV-ref))   |  log  + THETA(n) * (COV-ref)
register_covariate_formula(
  "con", "exponential",
  nonmem     = function(cova, ref, n) paste0(' * EXP(THETA(', n, ') * (', cova, '-', ref, '))'),
  nonmem_log = function(cova, ref, n) paste0(' + THETA(', n, ') * (', cova, '-', ref, ')'),
  init   = "0.1"
)

# cat.power: a covariate with discrete but NUMERIC levels (e.g. dose
# 35/70/125/150 mg) modelled as a power relationship on the actual values.
# Identical rendering to con.power and NOT categorical -- so model_add_cov writes
# it as a plain multiplicative/additive factor, not an IF/ELSEIF block.
register_covariate_formula("cat", "power",
                           nonmem = .cov_pow_nonmem, init = "0.1",
                           nonmem_log = .cov_pow_logadd)

# cat.linear: multi-level categorical written as a $PK IF/ELSEIF per-level block
# (one beta per non-reference level). Flagged categorical; the per-level logic
# stays in model_add_cov (the writer).
register_covariate_formula("cat", "linear", categorical = TRUE)
