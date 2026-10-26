#!/usr/bin/env Rscript

# Old-vs-new comparison for the functions that read and edit control streams.
#
# Usage, from the package root:
#   Rscript .claude/scripts/compare-model-readers.R [git_ref]
#
# Loads extract_params, .theta_tag_for, validate_parameter_blocks,
# apply_covariate_model, .structural_param_names, remove_covariate_from_model
# and model_add_cov as they are at `git_ref` (default HEAD), and runs them
# beside the working-tree versions on a generated set of ordinary control
# streams: one $THETA record, no $PRIOR records, no comment-only lines.
# Value styles (plain, bounds, FIX, scientific, LOG, inline notes, blank
# lines), $PK styles (normal, log, MU-referenced, TVCL), covariates (none,
# power, categorical IF block, EMAX) and $OMEGA layouts are crossed.
#
# Any difference is listed and the script exits with status 1. A change that
# is meant to alter one of these functions on ordinary models will show up
# here, which is the point: such a change has to be justified, not discovered.

args <- commandArgs(trailingOnly = TRUE)
ref  <- if (length(args) >= 1) args[[1]] else "HEAD"

suppressMessages(devtools::load_all(".", quiet = TRUE))
ns <- asNamespace("CovariateSearcher")
old_env <- new.env(parent = ns)
load_old <- function(file, fns) {
  src <- system2("git", c("show", paste0(ref, ":", file)), stdout = TRUE)
  ex <- parse(text = src, keep.source = FALSE)
  for (e in ex) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% fns) eval(e, old_env)
  }
}
load_old("R/model-output-tables.R", "extract_params")
load_old("R/covariate-formula.R", ".theta_tag_for")
load_old("R/validation.R", "validate_parameter_blocks")
load_old("R/apply-covariate-model.R", "apply_covariate_model")
load_old("R/forest-plots.R", ".structural_param_names")
load_old("R/model-modification.R", "remove_covariate_from_model")
stopifnot(length(ls(old_env, all.names = TRUE)) == 6)

# ---- corpus -------------------------------------------------------------------
theta_styles <- list(
  plain   = c("2.5 ; CL ; L/h ; RATIO", "30 ; V ; L ; RATIO"),
  bounds  = c("(0, 2.5, 100) ; CL ; L/h ; RATIO", "(0,30) ; V ; L ; RATIO"),
  fix     = c("2.5 FIX ; CL ; L/h ; RATIO", "(0, 30) FIX ; V ; L ; RATIO"),
  sci     = c("2.5E+00 ; CL ; L/h ; RATIO", "3.0e1 ; V ; L ; RATIO"),
  logpar  = c("0.916 ; CL ; L/h ; LOG", "3.4 ; V ; L ; LOG"),
  inline  = c("2.5   ; CL ; L/h ; RATIO   ", "30 ; V ; L ; RATIO ; extra note"),
  blank   = c("", "2.5 ; CL ; L/h ; RATIO", "   ", "30 ; V ; L ; RATIO", "")
)
pk_styles <- list(
  normal = c("TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1))",
             "TV_V = THETA(2)", "V = TV_V * EXP(ETA(2))"),
  log    = c("TV_CL = THETA(1)", "CL = EXP(TV_CL + ETA(1))",
             "TV_V = THETA(2)", "V = EXP(TV_V + ETA(2))"),
  mu     = c("TV_CL = THETA(1)", "MU_1 = LOG(TV_CL)", "CL = EXP(MU_1 + ETA(1))",
             "TV_V = THETA(2)", "MU_2 = LOG(TV_V)", "V = EXP(MU_2 + ETA(2))"),
  tvcl   = c("TVCL = THETA(1)", "CL = TVCL * EXP(ETA(1))",
             "TVV = THETA(2)", "V = TVV * EXP(ETA(2))")
)
cov_styles <- list(
  none   = list(pk = NULL, theta = NULL, cov = NULL),
  power  = list(pk = function(p) sub("^(TV_?CL = THETA\\(1\\))", "\\1 * (WT/70)**THETA(4)", p),
                theta = "0.75 ; beta_WT_CL ; ; RATIO", cov = "WT", status = "con"),
  cat    = list(pk = function(p) c("IF(SEX.EQ.1)THEN", "beta_SEX_CL = 1",
                                   "ELSEIF(SEX.EQ.2)THEN", "beta_SEX_CL = 1 + THETA(4)",
                                   "ENDIF",
                                   sub("^(TV_?CL = THETA\\(1\\))", "\\1 * beta_SEX_CL", p)),
                theta = "0.1 ; beta_SEX_CL_2 ; ; RATIO", cov = "SEX", status = "cat"),
  emax   = list(pk = function(p) sub("^(TV_?CL = THETA\\(1\\))",
                                     "\\1 * (THETA(4) * AGE/(THETA(5) + AGE))", p),
                theta = c("0.5 ; beta_AGE_CL_EMAX ; ; RATIO", "10 ; beta_AGE_CL_EC50 ; ; RATIO"),
                cov = "AGE", status = "con")
)
omega_styles <- list(
  diag  = c("$OMEGA", "0.1 ; IIV_CL ; ; RATIO", "0.1 ; IIV_V ; ; RATIO"),
  block = c("$OMEGA BLOCK(2)", "0.1 ; IIV_CL ; ; RATIO", "0.01 ; COR_CL_V ; ; RATIO",
            "0.1 ; IIV_V ; ; RATIO"),
  sameline = c("$OMEGA 0.1 ; IIV_CL ; ; RATIO", "0.1 ; IIV_V ; ; RATIO")
)
tails <- list(sigma = c("$SIGMA", "1 FIX ; EPS ; ; RATIO", "$ESTIMATION METHOD=1 INTER"),
              omega_last = character(0))

build <- function(th, pk, cv, om, tl) {
  pk_lines <- if (is.null(cv$pk)) pk else cv$pk(pk)
  c("$PROBLEM diff check", "$INPUT ID TIME DV AMT WT SEX AGE", "$DATA d.csv IGNORE=@",
    "$PK", pk_lines, "$ERROR", "Y = F + F*THETA(3)*EPS(1)",
    "$THETA", th, "0.1 ; PROP ; ; RATIO", cv$theta, om, tl)
}

# ---- compare --------------------------------------------------------------------
same <- function(a, b) isTRUE(all.equal(a, b, check.attributes = FALSE))
quiet <- function(expr) suppressWarnings(suppressMessages({
  out <- NULL; invisible(capture.output(out <- tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e))))); out
}))

n <- 0; diffs <- character(0)
check <- function(label, a, b) {
  n <<- n + 1
  if (!same(a, b)) diffs <<- c(diffs, label)
}

for (tn in names(theta_styles)) for (pn in names(pk_styles)) for (cn in names(cov_styles))
for (on in names(omega_styles)) for (ln in names(tails)) {
  id <- paste(tn, pn, cn, on, ln, sep = "/")
  cv <- cov_styles[[cn]]
  lines <- build(theta_styles[[tn]], pk_styles[[pn]], cv, omega_styles[[on]], tails[[ln]])
  dir <- tempfile("m"); dir.create(dir)
  writeLines(lines, file.path(dir, "run1.ctl"))

  for (tag in c("THETA", "OMEGA", "SIGMA"))
    check(paste(id, "extract_params", tag),
          quiet(old_env$extract_params(lines, tag)), quiet(extract_params(lines, tag)))
  for (p in c("CL", "V", "PROP", "IIV_CL", "EPS"))
    check(paste(id, ".theta_tag_for", p),
          old_env$.theta_tag_for(lines, p), .theta_tag_for(lines, p))
  check(paste(id, "validate_parameter_blocks"),
        quiet(old_env$validate_parameter_blocks(file.path(dir, "run1.ctl"))),
        quiet(validate_parameter_blocks(file.path(dir, "run1.ctl"))))
  check(paste(id, ".structural_param_names"),
        old_env$.structural_param_names("run1", dir), .structural_param_names("run1", dir))
  check(paste(id, "model_add_cov $OMEGA insertion line"),
        grep('\\$OMEGA', lines)[1],
        grep('^\\s*\\$OMEGA(?![A-Za-z])', lines, perl = TRUE)[1])

  nth <- 3 + length(cv$theta)
  thetas <- as.data.frame(as.list(setNames(seq(1.5, by = 0.5, length.out = nth),
                                           paste0("THETA", seq_len(nth)))))
  cs <- data.frame(COVARIATE = c("WT", "SEX", "AGE"), REFERENCE = c(70, 1, 50))
  for (covs in list(c(WT = 70, SEX = 1, AGE = 50), c(WT = 140, SEX = 2, AGE = 30)))
    check(paste(id, "apply_covariate_model", paste(covs, collapse = ",")),
          quiet(old_env$apply_covariate_model("run1", cs, thetas, covs, models_folder = dir)),
          quiet(apply_covariate_model("run1", cs, thetas, covs, models_folder = dir)))

  if (!is.null(cv$cov)) {
    tag_val <- paste0(cv$cov, "_CL")
    st <- list(models_folder = NULL, model_counter = 1L,
               tags = list(cov_tag = tag_val),
               covariate_search = data.frame(COVARIATE = cv$cov, PARAMETER = "CL",
                                             STATUS = cv$status, FORMULA = "x",
                                             cov_to_test = paste0("beta_", tag_val)),
               search_database = data.frame(model_name = "run1", tags = I(list(tag_val))))
    run_remove <- function(fn) {
      d <- tempfile("r"); dir.create(d)
      writeLines(lines, file.path(d, "run1.ctl"))
      st$models_folder <- d
      res <- quiet(fn(st, "run1", "cov_tag", save_as_new_model = FALSE, step_number = 2L))
      list(status = if (is.list(res)) res$status else res,
           ctl = readLines(file.path(d, "run1.ctl")))
    }
    check(paste(id, "remove_covariate_from_model"),
          run_remove(old_env$remove_covariate_from_model),
          run_remove(remove_covariate_from_model))
  }
}

cat(sprintf("models: %d | comparisons: %d | differences: %d\n",
            length(theta_styles) * length(pk_styles) * length(cov_styles) *
              length(omega_styles) * length(tails), n, length(diffs)))
if (length(diffs)) {
  # Every difference, grouped by the function and argument compared (the part
  # after the model id), so a change can be accounted for in full; then the
  # first 40 in detail.
  kinds <- table(sub("^\\S+ ", "", diffs))
  cat("  differences by comparison:\n")
  cat(sprintf("    %5d  %s", as.integer(kinds), names(kinds)), sep = "\n")
  cat(paste0("  DIFF ", head(unique(diffs), 40)), sep = "\n")
}

# ---- model_add_cov: old vs new on every ordinary base model ---------------------
load_old("R/model-modification.R", "model_add_cov")
add_cs <- data.frame(PARAMETER = "CL", COVARIATE = "WT", STATUS = "con", FORMULA = "power",
                     REFERENCE = 70, LEVELS = NA, cov_to_test = "beta_WT_CL",
                     stringsAsFactors = FALSE)
add_dat <- data.frame(ID = rep(1:4, each = 2), WT = rep(c(60, 70, 80, 90), each = 2))
run_add <- function(fn, lines) {
  d <- tempfile("a"); dir.create(d)
  writeLines(lines, file.path(d, "run1.ctl"))
  res <- quiet(fn(list(models_folder = d, search_config = list()), "run1", "WT_CL",
                  data_file = add_dat, covariate_search = add_cs))
  list(err = if (is.character(res)) res else NULL, ctl = readLines(file.path(d, "run1.ctl")))
}
na <- 0; add_diffs <- character(0)
for (tn in names(theta_styles)) for (pn in names(pk_styles))
for (on in names(omega_styles)) for (ln in names(tails)) {
  lines <- build(theta_styles[[tn]], pk_styles[[pn]], cov_styles$none, omega_styles[[on]], tails[[ln]])
  na <- na + 1
  if (!same(run_add(old_env$model_add_cov, lines), run_add(model_add_cov, lines)))
    add_diffs <- c(add_diffs, paste(tn, pn, on, ln, sep = "/"))
}
cat(sprintf("model_add_cov: %d base models compared, %d differences\n", na, length(add_diffs)))
if (length(add_diffs)) cat(paste0("  DIFF ", add_diffs), sep = "\n")

if (length(diffs) || length(add_diffs)) {
  cat("\nRESULT: DIFFERENCES against", ref, "\n")
  quit(status = 1)
}
cat("\nRESULT: identical to", ref, "\n")
