# =============================================================================
# APPLY COVARIATE MODEL TO INDIVIDUAL THETAS
# File: R/apply-covariate-model.R
# Part of CovariateSearcher Package
# =============================================================================

#' Apply a Model's Covariate Relationships to Individual Thetas
#'
#' @description
#' Reproduces a NONMEM model's typical-subject structural parameters in R for one
#' covariate scenario by **evaluating the model's own `$PK` equations** with the
#' sampled THETA draws and `ETA = 0`. Covariate effects (already written into the
#' equations) and any log/normal parameterization are handled automatically,
#' because the actual equation is run: `CL = EXP(TV_CL + 0)` yields CL on the
#' natural scale directly. There is no separate covariate-factor step and no
#' transform detection, so the R result cannot drift from the NONMEM model.
#'
#' Categorical covariates (written as `$PK` `IF/ELSEIF` blocks) are handled the
#' same way -- the block is evaluated for the scenario's covariate value.
#'
#' @param model_name Character. Model name without extension, e.g. "run28".
#' @param covariate_search Data frame with columns `COVARIATE`, `PARAMETER`,
#'   `STATUS`, `FORMULA`, `REFERENCE`. `REFERENCE` supplies the value for any
#'   covariate an equation references that is not in `covariates`.
#' @param individual_thetas Data frame whose columns are `THETA1`, `THETA2`, ...
#'   the sampled THETA draws on the **estimation scale** (raw, as returned by
#'   [sample_individual_thetas()]; a log-parameterised THETA stays on the log
#'   scale -- the equation's `EXP` back-transforms it). One row per draw.
#' @param covariates A single covariate scenario: a named vector or one-row data
#'   frame of covariate values (e.g. one row of [create_covariate_table()]).
#'   Applied to every draw. Extra names (such as `Scenario`) are ignored.
#' @param models_folder Character. Folder containing the model. Default "models".
#'
#' @return A `data.frame` with an `ID` column and the structural (non-`beta_`)
#'   THETA columns, each holding that parameter's value for the scenario on the
#'   **natural scale**, obtained by evaluating the model's own `$PK` equations
#'   with `ETA = 0`. One row per row of `individual_thetas`.
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
    if (grepl("^\\s*\\$", ln)) next
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

  # ---- Covariate values: covariate_search REFERENCE, overridden by scenario --
  cov_vals <- list()
  if (!is.null(covariate_search$COVARIATE) && "REFERENCE" %in% names(covariate_search)) {
    for (i in seq_len(nrow(covariate_search))) {
      cv <- as.character(covariate_search$COVARIATE[i])
      rv <- suppressWarnings(as.numeric(covariate_search$REFERENCE[i]))
      if (nzchar(cv) && !is.na(rv)) cov_vals[[cv]] <- rv
    }
  }
  for (nm in names(covariates)) {
    v <- suppressWarnings(as.numeric(covariates[[nm]]))
    if (!is.na(v)) cov_vals[[nm]] <- v
  }

  N <- nrow(individual_thetas)

  # ---- Evaluation environment ------------------------------------------------
  # THETA(k) -> the sampled draws for THETA k (length-N vector); ETA(k) -> 0
  # (typical subject); NONMEM intrinsics -> their R equivalents; covariates ->
  # their scenario/reference values. $PK assignments accumulate in this env.
  e <- new.env(parent = baseenv())
  e$THETA <- function(k) {
    col <- paste0("THETA", as.integer(k))
    if (!col %in% names(individual_thetas)) {
      stop("Model references ", col, " but `individual_thetas` has no such column.")
    }
    individual_thetas[[col]]
  }
  e$ETA   <- function(k) 0
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
  # Covariates are scalar for one scenario, so each IF condition is a single
  # TRUE/FALSE selecting a branch; THETA draws make the assigned values vectors.
  skipped       <- character(0)
  active_stack  <- logical(0)     # is the current (innermost) branch active?
  matched_stack <- logical(0)    # has any branch of this IF matched yet?
  is_active <- function() length(active_stack) == 0 || all(active_stack)

  do_assign <- function(line) {
    if (!grepl("=", line)) return(invisible())
    lhs <- trimws(sub("=.*$", "", line))
    rhs <- trimws(sub("^[^=]*=", "", line))
    if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", lhs)) return(invisible())
    val <- tryCatch(eval_r(rhs), error = function(err) { skipped <<- c(skipped, line); NULL })
    if (!is.null(val)) assign(lhs, val, envir = e)
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
        if (isTRUE(matched_stack[i])) {
          active_stack[i] <- FALSE
        } else {
          c_ok <- isTRUE(tryCatch(eval_r(cond), error = function(err) FALSE))
          active_stack[i] <- c_ok; matched_stack[i] <- c_ok
        }
      }
      next
    }
    if (grepl("^ELSE\\b", up)) {
      if (length(active_stack)) {
        i <- length(active_stack)
        active_stack[i]  <- !isTRUE(matched_stack[i])
        matched_stack[i] <- TRUE
      }
      next
    }
    # block IF ( ... ) THEN
    if (grepl("(?i)^IF\\s*\\(.*\\)\\s*THEN\\s*$", ln, perl = TRUE)) {
      cond <- sub("(?i)^IF\\s*\\(", "", ln, perl = TRUE)
      cond <- sub("(?i)\\)\\s*THEN\\s*$", "", cond, perl = TRUE)
      c_ok <- is_active() && isTRUE(tryCatch(eval_r(cond), error = function(err) FALSE))
      active_stack  <- c(active_stack, c_ok)
      matched_stack <- c(matched_stack, c_ok)
      next
    }
    # single-line IF( ... ) LHS = RHS
    if (grepl("(?i)^IF\\s*\\(", ln, perl = TRUE)) {
      m <- regmatches(ln, regexec("(?i)^IF\\s*\\((.*?)\\)\\s*(.*)$", ln, perl = TRUE))[[1]]
      if (length(m) == 3 && is_active()) {
        if (isTRUE(tryCatch(eval_r(m[2]), error = function(err) FALSE))) do_assign(m[3])
      }
      next
    }
    # plain assignment
    if (is_active()) do_assign(ln)
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

  out
}
