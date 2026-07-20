# =============================================================================
# RECONSTRUCT STEP FROM DISK
# File: R/resume-reconstruct.R
# Part of CovariateSearcher Package
# Re-register an SCM step's models from disk when they are missing from the
# saved database (e.g. the run died mid-step, before any checkpoint captured
# the step, and the failed model was rerun manually).
# =============================================================================


#' Re-register an SCM step's models from disk
#'
#' @title Reconstruct one SCM step's database rows from the model files on disk
#' @description Fallback for resuming a search whose interrupted step is present
#'   on disk but absent from every saved checkpoint (the run crashed before the
#'   step was checkpointed). It scans the models folder for model directories not
#'   yet in the database, keeps the ones that differ from \code{base_model} by
#'   exactly one covariate in the requested \code{direction}, inserts a database
#'   row for each (with the same \code{phase}/\code{action}/\code{covariate_tested}
#'   a live run would write), advances \code{model_counter} past everything on
#'   disk, and reads each model's results from its files (filling \code{ofv},
#'   \code{status} and \code{delta_ofv}). After this you can call
#'   \code{\link{continue_search}} on the returned state.
#'
#'   This is deliberately \strong{user-driven}: a technical/cluster failure can
#'   only be detected and fixed (rerun) by you, so you supply the \code{step_number},
#'   \code{base_model} and \code{direction} rather than have the function guess.
#'   The covariate each model tests is derived from its own \code{.yaml} tags
#'   (\code{setdiff} against the base), never from a display or checkpoint.
#'
#' @param search_state List or NULL. The search state to augment (takes
#'   precedence) — typically loaded from the newest checkpoint that predates the
#'   crashed step. If NULL, \code{checkpoint} is loaded instead.
#' @param step_number Integer. The step number to assign to the reconstructed
#'   models (the interrupted step; e.g. the forward step after the last saved one).
#' @param base_model Character. The model the step was built from — the forward
#'   base for an add-step, or the model being reduced for a removal-step. Must be
#'   in the database and have an OFV (needed to compute \code{delta_ofv}).
#' @param direction Character. "forward" (each model \emph{adds} one covariate to
#'   \code{base_model}) or "backward" (each model \emph{removes} one).
#' @param checkpoint Character or NULL. Full path to a checkpoint \code{.rds} to
#'   load when \code{search_state} is NULL — you point it at the correct file
#'   (e.g. \code{models/scm_rds/02_forward_created.rds}).
#' @param models_folder Character or NULL. Override the models folder; defaults to
#'   the loaded state's \code{models_folder}.
#' @return The updated \code{search_state}, with the step's rows registered and
#'   their results read from files. Models that do not read back as
#'   \code{"completed"} are reported so you can rerun them before resuming.
#' @seealso \code{\link{continue_search}}
#' @export
reconstruct_step_from_disk <- function(search_state = NULL,
                                       step_number,
                                       base_model,
                                       direction = c("forward", "backward"),
                                       checkpoint = NULL,
                                       models_folder = NULL) {

  direction <- match.arg(direction)

  if (missing(step_number) || missing(base_model)) {
    stop("reconstruct_step_from_disk(): `step_number` and `base_model` are required.")
  }

  # ---- obtain the search state (in-memory takes precedence; else the rds) ----
  if (is.null(search_state)) {
    if (is.null(checkpoint)) {
      stop("reconstruct_step_from_disk(): provide either `search_state` or a `checkpoint` .rds path")
    }
    if (!file.exists(checkpoint)) {
      stop(sprintf("reconstruct_step_from_disk(): checkpoint file not found: %s", checkpoint))
    }
    search_state <- load_search_state(checkpoint)
  }

  if (is.null(search_state$search_database) || nrow(search_state$search_database) == 0) {
    stop("reconstruct_step_from_disk(): search_state has no database to add to")
  }
  db <- search_state$search_database
  mf <- models_folder %||% search_state$models_folder
  if (is.null(mf) || !dir.exists(mf)) {
    stop("reconstruct_step_from_disk(): models_folder not found: ", mf)
  }

  # --- base model must be known, with an OFV (delta_ofv is relative to it) ---
  base_idx <- which(db$model_name == base_model)
  if (length(base_idx) == 0) {
    stop(sprintf("reconstruct_step_from_disk(): base_model '%s' is not in the database.", base_model))
  }
  if (is.na(db$ofv[base_idx[1]])) {
    warning(sprintf(paste0("Base model '%s' has no OFV in the database; delta_ofv cannot be ",
                           "computed until it does (re-read it from files first)."), base_model))
  }

  base_yaml <- file.path(mf, paste0(base_model, ".yaml"))
  if (!file.exists(base_yaml)) {
    stop(sprintf("reconstruct_step_from_disk(): base model yaml not found: %s", base_yaml))
  }
  base_tags <- unlist(yaml::read_yaml(base_yaml)$tags)

  # --- candidate model directories on disk, not yet in the database ----------
  # Models follow the package's run<N> convention; retry models are run<N>001
  # (create_retry_model) and are NOT independent step members — exclude them the
  # same way update_model_counter() does, so they can't be mis-registered as a
  # second model for the same covariate.
  run_dirs   <- list.dirs(mf, recursive = FALSE, full.names = FALSE)
  run_dirs   <- run_dirs[grepl("^run[0-9]+$", run_dirs) & !grepl("^run[0-9]+001$", run_dirs)]
  candidates <- run_dirs[!(run_dirs %in% db$model_name)]
  candidates <- candidates[order(as.integer(sub("^run", "", candidates)))]

  if (length(candidates) == 0) {
    cat("reconstruct_step_from_disk(): no un-registered run directories found — nothing to do.\n")
    return(search_state)
  }

  # --- build a schema-safe row per member model ------------------------------
  base_row   <- db[base_idx[1], , drop = FALSE]
  new_rows   <- list()
  registered <- character(0)
  skipped    <- character(0)   # on-disk models that don't match this step/direction

  for (m in candidates) {
    yml <- file.path(mf, paste0(m, ".yaml"))
    if (!file.exists(yml)) { skipped <- c(skipped, m); next }
    tags_m <- unlist(yaml::read_yaml(yml)$tags)

    added   <- setdiff(tags_m, base_tags)   # covariate(s) this model adds vs base
    removed <- setdiff(base_tags, tags_m)   # covariate(s) this model drops vs base

    # Membership: exactly one covariate different, in the requested direction.
    # (A covariate that maps to several beta tags won't match length==1 and is
    # surfaced via `skipped` rather than silently dropped.)
    is_member <- if (direction == "forward") {
      length(added) == 1L && length(removed) == 0L
    } else {
      length(removed) == 1L && length(added) == 0L
    }
    if (!is_member) { skipped <- c(skipped, m); next }

    if (direction == "forward") {
      cov_bare <- added
      action <- "add_covariate";    phase <- "forward_selection";    verb <- "Add"
    } else {
      cov_bare <- removed
      action <- "remove_covariate"; phase <- "backward_elimination"; verb <- "Remove"
    }

    # covariate_tested is the beta_-prefixed tag (the form the package stores and
    # that extract_covariate_name_from_tag() parses); guard double-prefixing.
    cov_tag   <- if (grepl("^beta_", cov_bare)) cov_bare else paste0("beta_", cov_bare)
    cov_label <- if (!is.null(search_state$tags) && cov_tag %in% names(search_state$tags)) {
      search_state$tags[[cov_tag]]
    } else cov_bare

    r <- base_row
    r$model_name       <- m
    r$step_description  <- paste(verb, cov_label)
    r$phase            <- phase
    r$step_number      <- as.integer(step_number)
    r$parent_model     <- base_model
    r$covariate_tested <- cov_tag
    r$action           <- action
    r$ofv <- NA_real_; r$delta_ofv <- NA_real_; r$rse_max <- NA_real_
    r$status           <- "created"
    r$submission_time  <- as.POSIXct(NA)
    r$completion_time  <- as.POSIXct(NA)
    r$tags             <- I(list(tags_m))
    new_rows[[m]]      <- r
    registered         <- c(registered, m)
  }

  if (length(skipped) > 0) {
    cat(sprintf("ℹ️  Skipped %d on-disk model(s) not matching a single-covariate %s of '%s': %s\n",
                length(skipped), direction, base_model, paste(skipped, collapse = ", ")))
  }

  if (length(new_rows) == 0) {
    cat(sprintf(paste0("reconstruct_step_from_disk(): none of %d candidate model(s) matched a ",
                       "single-covariate %s of base '%s' — nothing registered.\n"),
                length(candidates), direction, base_model))
    return(search_state)
  }

  search_state$search_database <- dplyr::bind_rows(db, dplyr::bind_rows(new_rows))

  # --- reset the model counter from the augmented database -------------------
  # Reuse the package's own routine, which correctly ignores run<N>001 retries
  # (a naive max over run numbers would let a retry inflate the counter).
  search_state <- update_model_counter(search_state)

  # --- read each model's results from its files (fills ofv/status/delta_ofv) --
  for (m in registered) {
    search_state <- update_model_status_from_files(search_state, m, force = TRUE)
  }

  # --- summary ---------------------------------------------------------------
  reg <- search_state$search_database[
    search_state$search_database$model_name %in% registered,
    c("model_name", "covariate_tested", "status", "ofv", "delta_ofv")]
  cat(sprintf("\n✅ reconstruct_step_from_disk(): registered %d model(s) into step %d (%s from %s)\n",
              length(registered), as.integer(step_number), direction, base_model))
  print(reg, row.names = FALSE)
  pending <- reg$model_name[reg$status != "completed"]
  if (length(pending) > 0) {
    cat(sprintf("⚠️  %d model(s) do not read back as 'completed' — rerun/fix before continue_search(): %s\n",
                length(pending), paste(pending, collapse = ", ")))
  }

  search_state
}
