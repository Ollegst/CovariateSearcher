# =============================================================================
# APPLY COVARIATE MODEL TO INDIVIDUAL THETAS
# File: R/apply-covariate-model.R
# Part of CovariateSearcher Package
# =============================================================================

#' Apply a Model's Covariate Relationships to Individual Thetas
#'
#' @description
#' Reproduces a NONMEM model's typical-subject structural parameters in R for
#' one or many covariate scenarios by **evaluating the model's own `$PK`
#' equations** with the sampled THETA draws and `ETA = 0`. Covariate effects
#' (already written into the equations) and any log/normal parameterization
#' are handled automatically, because the actual equation is run:
#' `CL = EXP(TV_CL + 0)` yields CL on the natural scale directly. There is no
#' separate covariate-factor step and no transform detection, so the R result
#' cannot drift from the NONMEM model.
#'
#' Categorical covariates (written as `$PK` `IF/ELSEIF` blocks) are handled the
#' same way -- the block is evaluated for the scenario's covariate value. When
#' `covariates` supplies one value per row instead (real per-subject
#' combinations), each row is routed through the `IF/ELSEIF/ELSE` branch its
#' own covariate values select, independently of every other row.
#'
#' @param model_name Character. Model name without extension, e.g. "run28".
#' @param covariate_search Covariate-search table with columns `COVARIATE`,
#'   `PARAMETER`, `STATUS`, `FORMULA`, `REFERENCE` (`REFERENCE` supplies the
#'   value for any covariate an equation references that is not in
#'   `covariates`), given as either a `data.frame` OR a character path to a
#'   `.csv`/`.rds` file to load.
#' @param individual_thetas Data frame whose columns are `THETA1`, `THETA2`, ...
#'   the sampled THETA draws on the **estimation scale** (raw, as returned by
#'   [sample_individual_thetas()]; a log-parameterised THETA stays on the log
#'   scale -- the equation's `EXP` back-transforms it). One row per draw; when
#'   `eta = TRUE`, row *i*'s THETA draw is paired with ETA draw *i*.
#' @param covariates A covariate scenario: a named vector or one-row data frame
#'   of covariate values (e.g. one row of [create_covariate_table()]) is
#'   applied to every draw. A data frame with the same number of rows as
#'   `individual_thetas` instead pairs row *i*'s covariate values with row
#'   *i*'s THETA (and ETA, when `eta = TRUE`) draw -- e.g. real per-subject
#'   covariate combinations pulled straight from a dataset. In the per-row
#'   form, a column is only treated as a covariate if its name is listed in
#'   `covariate_search$COVARIATE`; anything else (such as `Scenario`) is
#'   ignored outright, whatever it contains. Every row of a REGISTERED
#'   covariate needs a real value: an `NA` for even one row **stops** rather
#'   than falling back to `covariate_search`'s `REFERENCE` -- real per-subject
#'   data is expected to be complete, so a missing value there is treated as
#'   an error to fix upstream, not something to patch over silently.
#' @param models_folder Character. Folder containing the model. Default "models".
#' @param eta Logical. If `TRUE`, draw one ETA vector per row of
#'   `individual_thetas` from the model's final `$OMEGA` matrix (read from its
#'   `.ext` file, the same way [sample_individual_thetas()] reads THETA
#'   uncertainty) and evaluate `$PK` with those draws instead of `ETA = 0`, so
#'   the result carries between-subject variability on top of the sampled
#'   THETA uncertainty and covariate effect. `OMEGA` elements never estimated
#'   (e.g. off-diagonal under a diagonal `$OMEGA`) are treated as 0
#'   (uncorrelated). If `$PK` references an `ETA(k)` beyond the OMEGA matrix's
#'   size, **stops** rather than silently falling back (checked once, up
#'   front, before `$PK` is interpreted). Default `FALSE` reproduces the
#'   typical-subject value for the scenario.
#' @param seed Integer. Random seed for the ETA draw when `eta = TRUE`.
#'   Ignored otherwise. Default 1234.
#'
#' @return A `data.frame` with an `ID` column and the structural (non-`beta_`)
#'   THETA columns, each holding that parameter's value for the scenario on the
#'   **natural scale**, obtained by evaluating the model's own `$PK` equations
#'   with `ETA = 0` (or, when `eta = TRUE`, a sampled ETA draw). One row per
#'   row of `individual_thetas`.
#' @export
apply_covariate_model <- function(model_name,
                                  covariate_search,
                                  individual_thetas,
                                  covariates,
                                  models_folder = "models",
                                  eta = FALSE,
                                  seed = 1234) {

  # covariate_search may be given as an object or a character path (.csv/.rds).
  covariate_search <- .load_if_path(covariate_search, "covariate_search")

  N <- nrow(individual_thetas)

  # ---- Normalise the covariate scenario(s) -----------------------------------
  # One row (or a named vector) broadcasts to every draw. An N-row data frame
  # instead pairs row i's values with row i's THETA/ETA draw, like
  # individual_thetas/eta_draws already do.
  if (is.data.frame(covariates)) {
    if (nrow(covariates) == 1) {
      covariates <- unlist(covariates[1, , drop = TRUE])
    } else if (nrow(covariates) != N) {
      stop("`covariates` must be a single scenario (one row) or have the ",
           "same number of rows as `individual_thetas` (", N, "); got ",
           nrow(covariates), " row(s).")
    }
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
    # (?![A-Za-z]): $THETA but not the $PRIOR records $THETAP / $THETAPV.
    starts <- grep(paste0("^\\s*\\$", tag, "(?![A-Za-z])"), lines,
                   ignore.case = TRUE, perl = TRUE)
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
    # A value on the record line itself ("$THETA 0.5 ; CL") is a THETA too.
    if (grepl("^\\s*\\$", ln)) ln <- sub("^\\s*\\$[A-Za-z]+\\s*", "", ln)
    parts <- strsplit(ln, ";")[[1]]
    if (!grepl("[0-9]", parts[1])) next
    theta_names <- c(theta_names,
                     if (length(parts) >= 2) trimws(parts[2]) else "")
  }
  if (length(theta_names) == 0) {
    stop("Could not parse any $THETA records from model '", model_name, "'.")
  }
  structural_idx <- which(!grepl("^beta_", theta_names))

  # ---- $PK block -------------------------------------------------------------
  pk <- get_block("PK")
  if (length(pk) == 0) pk <- get_block("PRED")
  if (length(pk) == 0) {
    stop("No $PK or $PRED block found in model '", model_name, "'.")
  }

  # ---- ETA draws (optional) --------------------------------------------------
  # One MVN(0, OMEGA) draw per row of individual_thetas, paired by row index
  # with that row's THETA draw. OMEGA comes from the model's own FINAL .ext
  # estimates, same file/row sample_individual_thetas() reads THETA from.
  eta_draws <- NULL
  if (isTRUE(eta)) {
    omega_mat <- .read_final_omega_matrix(model_name, models_folder)
    omega_mat <- .make_psd(omega_mat)
    set.seed(seed)
    eta_draws <- tryCatch(
      mvtnorm::rmvnorm(N, mean = rep(0, nrow(omega_mat)), sigma = omega_mat,
                       method = "svd"),
      error = function(err) {
        stop("Failed to draw ETAs from '", model_name, "'s OMEGA matrix: ",
             conditionMessage(err))
      }
    )

    # Fail fast if $PK references an ETA beyond what OMEGA provides. Every
    # eval_r() call inside the $PK interpreter below is wrapped in its own
    # tryCatch (do_assign(), the IF-condition checks), so a stop() raised from
    # inside e$ETA() would otherwise be swallowed into a silent raw-THETA
    # fallback -- checking here, before interpretation starts, surfaces it.
    pk_code <- sub(";.*$", "", pk)                       # strip comments
    # \\b guard so THETA( doesn't match as ETA( (see .exp_inner_containing_eta).
    eta_refs <- regmatches(pk_code,
                           gregexpr("\\bETA\\(\\s*\\d+\\s*\\)", pk_code, ignore.case = TRUE))
    eta_idx <- as.integer(gsub("\\D", "", unlist(eta_refs)))
    if (length(eta_idx) > 0 && max(eta_idx) > ncol(eta_draws)) {
      stop("Model '", model_name, "' $PK references ETA(", max(eta_idx),
           ") but its OMEGA matrix only has ", ncol(eta_draws), " element(s).")
    }
  }

  # ---- Covariate values: covariate_search REFERENCE, overridden by scenario --
  # A scalar covariate that is NA is dropped, falling through to REFERENCE --
  # a deliberate "leave this one at reference" scenario value. A per-row
  # (length-N) covariate column is held to a stricter rule, but only for a
  # REGISTERED covariate (a name listed in covariate_search$COVARIATE): every
  # row needs a real value, so ANY NA there -- one row or every row -- is
  # refused outright rather than silently patched from REFERENCE. A column
  # whose name is not a registered covariate (e.g. a `Scenario` label riding
  # along in the table) is not held to that rule at all and is simply
  # ignored, whatever it happens to contain.
  ref_vals <- list()
  known_covs <- character(0)
  if (!is.null(covariate_search$COVARIATE) && "REFERENCE" %in% names(covariate_search)) {
    known_covs <- as.character(covariate_search$COVARIATE)
    for (i in seq_len(nrow(covariate_search))) {
      cv <- known_covs[i]
      rv <- suppressWarnings(as.numeric(covariate_search$REFERENCE[i]))
      if (nzchar(cv) && !is.na(rv)) ref_vals[[cv]] <- rv
    }
  }
  cov_vals <- ref_vals
  for (nm in names(covariates)) {
    # as.character() first: as.numeric() on a bare factor column returns its
    # internal level codes, not the values it displays (e.g. factor("M","F")
    # -> 2, 1 by alphabetical level order) -- silently wrong, not NA, so the
    # NA check below would never catch it. Going through the character labels
    # is safe for an already-numeric or -character column too.
    v <- suppressWarnings(as.numeric(as.character(covariates[[nm]])))
    if (length(v) > 1L && length(v) != N) {
      stop("Covariate '", nm, "' in `covariates` has length ", length(v),
           ", expected 1 or ", N, ".")
    }
    if (length(v) == 1L) {
      if (!is.na(v)) cov_vals[[nm]] <- v
      next
    }
    if (!(nm %in% known_covs)) next   # not a registered covariate - ignore entirely
    if (anyNA(v)) {
      stop("Covariate '", nm, "' in `covariates` has NA for row(s) ",
           paste(which(is.na(v)), collapse = ", "),
           " -- every row needs a value when `covariates` is given per row",
           " (no REFERENCE fallback for a missing per-row covariate).")
    }
    cov_vals[[nm]] <- v
  }

  # ---- Evaluation environment ------------------------------------------------
  # THETA(k) -> the sampled draws for THETA k (length-N vector); ETA(k) -> 0
  # (typical subject), or a sampled length-N draw when `eta = TRUE`; NONMEM
  # intrinsics -> their R equivalents; covariates -> their scenario/reference
  # values. $PK assignments accumulate in this env.
  e <- new.env(parent = baseenv())
  e$THETA <- function(k) {
    col <- paste0("THETA", as.integer(k))
    if (!col %in% names(individual_thetas)) {
      stop("Model references ", col, " but `individual_thetas` has no such column.")
    }
    individual_thetas[[col]]
  }
  e$ETA <- if (is.null(eta_draws)) {
    function(k) 0
  } else {
    function(k) {
      k <- as.integer(k)
      if (k > ncol(eta_draws)) {
        stop("Model references ETA(", k, ") but '", model_name,
             "'s OMEGA matrix only has ", ncol(eta_draws), " element(s).")
      }
      eta_draws[, k]
    }
  }
  e$EXP <- exp; e$LOG <- log; e$LOG10 <- log10; e$SQRT <- sqrt; e$ABS <- abs
  for (nm in names(cov_vals)) assign(nm, cov_vals[[nm]], envir = e)

  # Translate a NONMEM expression fragment to R (operators only; intrinsics such
  # as EXP/LOG are provided as functions in `e`, so they need no rewrite).
  nm_to_r <- function(txt) {
    txt <- gsub("\\*\\*", "^", txt)                       # ** -> ^
    txt <- gsub("\\.EQ\\.",  "==", txt, ignore.case = TRUE)
    txt <- gsub("\\.NE\\.",  "!=", txt, ignore.case = TRUE)
    txt <- gsub("\\.GE\\.",  ">=", txt, ignore.case = TRUE)
    txt <- gsub("\\.LE\\.",  "<=", txt, ignore.case = TRUE)
    txt <- gsub("\\.GT\\.",  ">",  txt, ignore.case = TRUE)
    txt <- gsub("\\.LT\\.",  "<",  txt, ignore.case = TRUE)
    txt <- gsub("\\.AND\\.", "&",  txt, ignore.case = TRUE)
    txt <- gsub("\\.OR\\.",  "|",  txt, ignore.case = TRUE)
    txt <- gsub("\\.NOT\\.", "!",  txt, ignore.case = TRUE)
    txt
  }
  eval_r <- function(txt) eval(parse(text = nm_to_r(txt)), envir = e)

  # ---- Interpret the $PK block, honouring IF / ELSEIF / ELSE / ENDIF ---------
  # Each stack entry is a length-N logical MASK (one flag per row). A
  # condition is evaluated for every row -- THETA(k)/ETA(k) are already
  # length-N, and a per-row `covariates` column may vary too -- so different
  # rows can be in different IF/ELSEIF/ELSE branches at once. do_assign()
  # writes only the rows its mask selects, leaving other rows' already-
  # assigned values untouched, so a variable set across several branches ends
  # up with each row's own branch's value. A branch guarded only by scalar
  # inputs ends up with a uniformly all-TRUE or all-FALSE mask, which selects
  # (or skips) the whole vector at once, the same as an unconditional line.
  skipped       <- character(0)
  active_stack  <- list()     # length-N logical mask per nesting level
  matched_stack <- list()     # length-N: has this row matched a branch yet?
  is_active <- function() {
    if (length(active_stack) == 0) return(rep(TRUE, N))
    Reduce(`&`, active_stack)
  }
  # Coerce a condition's result to a length-N logical mask: a scalar result
  # (every covariate involved is scalar) broadcasts, and NA (an unevaluable or
  # not-yet-assigned operand) reads as "not matched" rather than erroring.
  as_mask <- function(x) {
    x <- as.logical(x)
    if (length(x) == 1L) x <- rep(x, N)
    x[is.na(x)] <- FALSE
    x
  }

  do_assign <- function(line, mask = NULL) {
    if (!grepl("=", line)) return(invisible())
    lhs <- trimws(sub("=.*$", "", line))
    rhs <- trimws(sub("^[^=]*=", "", line))
    if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", lhs)) return(invisible())
    if (is.null(mask)) mask <- is_active()
    if (!any(mask)) return(invisible())    # nothing in this branch, don't touch lhs at all
    val <- tryCatch(eval_r(rhs), error = function(err) { skipped <<- c(skipped, line); NULL })
    if (is.null(val)) return(invisible())
    if (length(val) == 1L) val <- rep(val, N)
    if (exists(lhs, envir = e, inherits = FALSE)) {
      full <- get(lhs, envir = e)
      if (length(full) == 1L) full <- rep(full, N)
    } else {
      full <- rep(NA_real_, N)
    }
    full[mask] <- val[mask]
    assign(lhs, full, envir = e)
    invisible()
  }

  for (raw in pk) {
    ln <- trimws(sub(";.*$", "", raw))                    # strip comment
    if (ln == "" || grepl("^\\$", ln)) next                # blank / header
    up <- toupper(ln)

    if (grepl("^ENDIF\\b", up)) {
      if (length(active_stack)) {
        active_stack  <- active_stack[-length(active_stack)]
        matched_stack <- matched_stack[-length(matched_stack)]
      }
      next
    }
    if (grepl("^ELSEIF\\b", up) || grepl("^ELSE\\s*IF\\b", up)) {
      cond <- sub("(?i)^ELSE\\s*IF\\s*\\(", "", ln, perl = TRUE)
      cond <- sub("(?i)\\)\\s*THEN\\s*$", "", cond, perl = TRUE)
      if (length(active_stack)) {
        i <- length(active_stack)
        outer <- if (i > 1) Reduce(`&`, active_stack[seq_len(i - 1)]) else rep(TRUE, N)
        already <- matched_stack[[i]]
        c_ok <- as_mask(tryCatch(eval_r(cond), error = function(err) FALSE))
        active_stack[[i]]  <- outer & !already & c_ok
        matched_stack[[i]] <- already | c_ok
      }
      next
    }
    if (grepl("^ELSE\\b", up)) {
      if (length(active_stack)) {
        i <- length(active_stack)
        outer <- if (i > 1) Reduce(`&`, active_stack[seq_len(i - 1)]) else rep(TRUE, N)
        already <- matched_stack[[i]]
        active_stack[[i]]  <- outer & !already
        matched_stack[[i]] <- rep(TRUE, N)
      }
      next
    }
    # block IF ( ... ) THEN
    if (grepl("(?i)^IF\\s*\\(.*\\)\\s*THEN\\s*$", ln, perl = TRUE)) {
      cond <- sub("(?i)^IF\\s*\\(", "", ln, perl = TRUE)
      cond <- sub("(?i)\\)\\s*THEN\\s*$", "", cond, perl = TRUE)
      outer <- is_active()
      c_ok  <- outer & as_mask(tryCatch(eval_r(cond), error = function(err) FALSE))
      active_stack  <- c(active_stack, list(c_ok))
      matched_stack <- c(matched_stack, list(c_ok))
      next
    }
    # single-line IF( ... ) LHS = RHS
    if (grepl("(?i)^IF\\s*\\(", ln, perl = TRUE)) {
      m <- regmatches(ln, regexec("(?i)^IF\\s*\\((.*?)\\)\\s*(.*)$", ln, perl = TRUE))[[1]]
      if (length(m) == 3) {
        outer <- is_active()
        c_ok  <- outer & as_mask(tryCatch(eval_r(m[2]), error = function(err) FALSE))
        do_assign(m[3], mask = c_ok)
      }
      next
    }
    # plain assignment
    do_assign(ln)
  }

  # ---- Map each structural THETA position to its parameter's value -----------
  # THETA(p) is the "leading" theta of exactly one $PK definition line; its LHS
  # (TV_<param> or <param>) names the parameter, whose evaluated value we return
  # in column THETA<p>. Positions with no such line (e.g. a base-model covariate
  # exponent) fall back to the raw sampled draw, matching the prior behaviour.
  out <- data.frame(ID = seq_len(N))
  missing_params <- character(0)
  for (p in structural_idx) {
    col <- paste0("THETA", p)
    def_line <- NA_character_
    for (raw in pk) {
      ln <- trimws(sub(";.*$", "", raw))
      if (!grepl("=", ln)) next
      rhs <- sub("^[^=]*=", "", ln)
      if (grepl(paste0("THETA\\(\\s*", p, "\\s*\\)"), ln) &&
          identical(first_theta_index(rhs), as.integer(p))) {
        def_line <- ln; break
      }
    }
    val <- NULL
    if (!is.na(def_line)) {
      lhs   <- trimws(sub("=.*$", "", def_line))
      param <- sub("^TV_?", "", lhs)                      # TV_CL / TVCL -> CL
      if (exists(param, envir = e, inherits = FALSE)) {
        val <- get(param, envir = e)
      } else {
        missing_params <- c(missing_params, col)
      }
    }
    if (is.null(val)) val <- individual_thetas[[col]]     # raw fallback
    if (length(val) == 1L) val <- rep(val, N)
    out[[col]] <- val
  }

  if (length(missing_params) > 0) {
    warning("apply_covariate_model(): could not evaluate the $PK parameter for ",
            paste(missing_params, collapse = ", "),
            "; returned the raw sampled value(s) instead.",
            if (length(skipped)) paste0(" Skipped $PK line(s): ",
                                        paste(utils::head(skipped, 5), collapse = " | ")) else "")
  }

  # A parameter can exist (so it isn't in missing_params) and still be NA for
  # some rows only: a $PK branch that assigns it for the first time with no
  # ELSE/default leaves the rows that took no branch at NA, even though every
  # row's covariate input was itself perfectly valid -- e.g. a real SEXN = 5
  # with no matching IF/ELSEIF for it. That is otherwise silent, most likely
  # in a per-row (real per-subject) `covariates` call, where different rows
  # can take different -- or no -- branches of the same $PK block.
  na_cols <- setdiff(names(out), c("ID", missing_params))
  na_cols <- na_cols[vapply(na_cols, function(cn) anyNA(out[[cn]]), logical(1))]
  if (length(na_cols) > 0) {
    warning("apply_covariate_model(): ", paste(na_cols, collapse = ", "),
            " contain NA for some row(s) -- likely a $PK branch with no ",
            "matching ELSE/default for that row's covariate combination.")
  }

  out
}


#' Read the final OMEGA point-estimate matrix from a NONMEM .ext file
#'
#' @description Builds the full symmetric BSV matrix implied by the model's
#'   FINAL `OMEGA(i,j)` estimates, from the columns `.ext`'s `read.table()`
#'   sanitises to `OMEGA(i,j)` / `OMEGA.i.j.` (same spelling
#'   [sample_individual_thetas()]'s THETA reading relies on). Matrix positions
#'   with no such column (an element never estimated, e.g. off-diagonal under
#'   a diagonal `$OMEGA`) are 0.
#' @keywords internal
#' @noRd
.read_final_omega_matrix <- function(model, models_folder) {
  ext_file <- .find_model_component(model, models_folder, "ext")
  if (is.na(ext_file)) {
    stop("`.ext` file not found for '", model, "' under '", models_folder, "'.")
  }
  ext <- read_ext_iterations(ext_file)
  if (nrow(ext) == 0) {
    stop("Could not read parameter estimates from: ", ext_file)
  }
  final_rows <- ext[ext$TYPE %in% "FINAL", , drop = FALSE]
  if (nrow(final_rows) == 0) {
    stop("No FINAL parameter row (ITERATION -1000000000) found in ", ext_file)
  }
  final_row <- final_rows[nrow(final_rows), , drop = FALSE]

  block_pat  <- "^OMEGA[.(](\\d+)[,.](\\d+)[.)]$"
  omega_cols <- grep(block_pat, names(final_row), value = TRUE)
  if (length(omega_cols) == 0) {
    stop("No OMEGA columns found in ", ext_file)
  }
  i_idx <- as.integer(sub(block_pat, "\\1", omega_cols))
  j_idx <- as.integer(sub(block_pat, "\\2", omega_cols))
  vals  <- as.numeric(final_row[1, omega_cols])

  n_eta <- max(i_idx, j_idx)
  mat <- matrix(0, n_eta, n_eta)
  mat[cbind(i_idx, j_idx)] <- vals
  mat[cbind(j_idx, i_idx)] <- vals
  mat
}
