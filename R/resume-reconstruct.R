# =============================================================================
# RECONSTRUCT STEP FROM DISK
# File: R/resume-reconstruct.R
# Part of CovariateSearcher Package
# Re-register an SCM step's models from disk when they are missing from the
# saved database (e.g. the run died mid-step before a checkpoint captured it).
# base_model / direction / step_number are auto-detected from the files.
# =============================================================================


#' On-disk SCM models not yet in the search database
#'
#' @description Model directories (\code{run<N>}) present in the models folder
#'   but absent from \code{search_state$search_database}. Retry models
#'   (\code{run<N>001}) are excluded, as in \code{update_model_counter()}.
#' @keywords internal
.scm_unregistered_models <- function(search_state, models_folder = NULL) {
  mf <- models_folder %||% search_state$models_folder
  if (is.null(mf) || !dir.exists(mf)) return(character(0))
  d <- list.dirs(mf, recursive = FALSE, full.names = FALSE)
  d <- d[grepl("^run[0-9]+$", d) & !grepl("^run[0-9]+001$", d)]
  d <- d[!(d %in% search_state$search_database$model_name)]
  d[order(suppressWarnings(as.integer(sub("^run", "", d))))]
}


#' Recorded parent (bbr \code{based_on}) of a model, read from its \code{.yaml}
#' @keywords internal
.scm_model_parent <- function(models_folder, model) {
  yml <- file.path(models_folder, paste0(model, ".yaml"))
  if (!file.exists(yml)) return(NA_character_)
  bo <- yaml::read_yaml(yml)$based_on
  if (is.null(bo) || length(bo) == 0) return(NA_character_)
  basename(as.character(bo)[1])
}


#' Re-register an SCM step's models from disk (auto-detected)
#'
#' @title Reconstruct one SCM step's database rows from the model files on disk
#' @description Fallback for resuming a search whose interrupted step is on disk
#'   but missing from every saved checkpoint. It finds the model directories not
#'   yet in the database and, from the files alone, works out:
#'   \itemize{
#'     \item \strong{base_model} — the step's parent, from each model's recorded
#'       \code{based_on} (all candidates must share one parent);
#'     \item \strong{direction} — "forward" if the models \emph{add} a covariate
#'       versus the base, "backward" if they \emph{remove} one (all must agree);
#'     \item \strong{step_number} — \code{max(step) + 1}, the number the live
#'       search would assign next.
#'   }
#'   It inserts a database row per model (same \code{phase}/\code{action}/
#'   \code{covariate_tested} a live run writes), resets \code{model_counter}, and
#'   reads each model's results from its files (filling \code{ofv}/\code{status}/
#'   \code{delta_ofv}). \code{\link{continue_search}} calls this automatically
#'   when it detects such a gap, so you rarely call it directly.
#'
#'   Each detection can be overridden with the matching argument. If the
#'   candidates disagree (more than one parent, or a mix of add and remove) it
#'   stops and asks you to be explicit rather than guess. A cluster/technical
#'   failure still can't be detected from files, so you must rerun any dropped
#'   model before resuming.
#'
#' @param search_state List or NULL. State to augment (takes precedence); if
#'   NULL, \code{checkpoint} is loaded.
#' @param step_number Integer or NULL. Override the auto-detected step number
#'   (\code{max(step) + 1}).
#' @param base_model Character or NULL. Override the auto-detected base (the
#'   candidates' common \code{based_on} parent). Must be in the database.
#' @param direction "forward"/"backward" or NULL. Override the auto-detected
#'   direction.
#' @param checkpoint Character or NULL. Path to a checkpoint \code{.rds} to load
#'   when \code{search_state} is NULL (e.g.
#'   \code{models/scm_rds/03_forward_done.rds}).
#' @param models_folder Character or NULL. Override the models folder; defaults
#'   to the loaded state's \code{models_folder}.
#' @return The updated \code{search_state}, with the step's rows registered and
#'   their results read from files. Models that do not read back as
#'   \code{"completed"} are reported so you can rerun them before resuming.
#' @seealso \code{\link{continue_search}}
#' @export
reconstruct_step_from_disk <- function(search_state = NULL,
                                       step_number = NULL,
                                       base_model = NULL,
                                       direction = NULL,
                                       checkpoint = NULL,
                                       models_folder = NULL) {

  # ---- obtain the search state (in-memory takes precedence; else the rds) ----
  if (is.null(search_state)) {
    if (is.null(checkpoint)) {
      stop("reconstruct_step_from_disk(): provide either `search_state` or a `checkpoint` .rds path",
           call. = FALSE)
    }
    if (!file.exists(checkpoint)) {
      stop(sprintf("reconstruct_step_from_disk(): checkpoint file not found: %s", checkpoint),
           call. = FALSE)
    }
    search_state <- load_search_state(checkpoint)
  }
  if (is.null(search_state$search_database) || nrow(search_state$search_database) == 0) {
    stop("reconstruct_step_from_disk(): search_state has no database to add to", call. = FALSE)
  }
  db <- search_state$search_database
  mf <- models_folder %||% search_state$models_folder
  if (is.null(mf) || !dir.exists(mf)) {
    stop("reconstruct_step_from_disk(): models_folder not found: ", mf, call. = FALSE)
  }

  # ---- candidate models on disk, not yet in the database --------------------
  candidates <- .scm_unregistered_models(search_state, mf)
  if (length(candidates) == 0) {
    cat("reconstruct_step_from_disk(): no un-registered run directories — nothing to do.\n")
    return(search_state)
  }

  # ---- auto-detect base_model from the candidates' recorded parents ---------
  if (is.null(base_model)) {
    parents <- vapply(candidates, function(m) .scm_model_parent(mf, m), character(1))
    parents <- unique(parents[!is.na(parents)])
    if (length(parents) == 0) {
      stop("reconstruct_step_from_disk(): could not read a parent (based_on) from the ",
           "candidate models; pass `base_model` explicitly.", call. = FALSE)
    }
    if (length(parents) > 1) {
      stop(sprintf(paste0("reconstruct_step_from_disk(): candidates point to more than one parent ",
                          "(%s) — looks like more than one unsaved step. Reconstruct one step at a ",
                          "time by passing `base_model` (and `step_number`/`direction`)."),
                   paste(parents, collapse = ", ")), call. = FALSE)
    }
    base_model <- parents
    cat(sprintf("• base_model auto-detected (based_on): %s\n", base_model))
  }

  base_idx <- which(db$model_name == base_model)
  if (length(base_idx) == 0) {
    stop(sprintf(paste0("reconstruct_step_from_disk(): base_model '%s' is not in the database ",
                        "(load the newest checkpoint, or pass the right base_model)."), base_model),
         call. = FALSE)
  }
  if (is.na(db$ofv[base_idx[1]])) {
    warning(sprintf("Base model '%s' has no OFV in the database; delta_ofv can't be computed until it does.",
                    base_model))
  }
  base_yaml <- file.path(mf, paste0(base_model, ".yaml"))
  if (!file.exists(base_yaml)) {
    stop(sprintf("reconstruct_step_from_disk(): base model yaml not found: %s", base_yaml), call. = FALSE)
  }
  base_tags <- unlist(yaml::read_yaml(base_yaml)$tags)

  # ---- classify each candidate: adds one cov (forward) / removes one (backward)
  members <- list()          # model -> list(cov, dir, tags)
  skipped <- character(0)
  for (m in candidates) {
    tags_m  <- unlist(yaml::read_yaml(file.path(mf, paste0(m, ".yaml")))$tags)
    added   <- setdiff(tags_m, base_tags)
    removed <- setdiff(base_tags, tags_m)
    if (length(added) == 1L && length(removed) == 0L) {
      members[[m]] <- list(cov = added,   dir = "forward",  tags = tags_m)
    } else if (length(removed) == 1L && length(added) == 0L) {
      members[[m]] <- list(cov = removed, dir = "backward", tags = tags_m)
    } else {
      skipped <- c(skipped, m)   # not a single-covariate change of the base
    }
  }
  if (length(members) == 0) {
    if (length(skipped)) {
      cat(sprintf("ℹ️  Skipped %d model(s) not a single-covariate change of '%s': %s\n",
                  length(skipped), base_model, paste(skipped, collapse = ", ")))
    }
    cat("reconstruct_step_from_disk(): nothing to register.\n")
    return(search_state)
  }

  # ---- resolve direction (auto from the candidates, or honour the override) --
  dirs <- vapply(members, function(x) x$dir, character(1))
  if (is.null(direction)) {
    ud <- unique(dirs)
    if (length(ud) > 1) {
      stop("reconstruct_step_from_disk(): candidates mix add and remove (forward & backward) ",
           "relative to the base — pass `direction` explicitly.", call. = FALSE)
    }
    direction <- ud
    cat(sprintf("• direction auto-detected: %s\n", direction))
  } else {
    direction <- match.arg(direction, c("forward", "backward"))
    off <- names(members)[dirs != direction]
    if (length(off)) {
      skipped <- c(skipped, off)
      members <- members[dirs == direction]
    }
    if (length(members) == 0) {
      cat(sprintf("reconstruct_step_from_disk(): no candidate matches direction '%s'.\n", direction))
      return(search_state)
    }
  }

  # ---- resolve step_number (auto = the number the live loop would assign) ----
  if (is.null(step_number)) {
    mx <- suppressWarnings(max(db$step_number, na.rm = TRUE))
    step_number <- if (is.finite(mx)) mx + 1L else 1L
    cat(sprintf("• step_number auto-detected: %d\n", step_number))
  }
  step_number <- as.integer(step_number)

  if (direction == "forward") {
    action <- "add_covariate";    phase <- "forward_selection";    verb <- "Add"
  } else {
    action <- "remove_covariate"; phase <- "backward_elimination"; verb <- "Remove"
  }

  # ---- build a schema-safe row per member model -----------------------------
  base_row   <- db[base_idx[1], , drop = FALSE]
  new_rows   <- list()
  registered <- character(0)
  for (m in names(members)) {
    info      <- members[[m]]
    # covariate_tested is the beta_-prefixed tag (the form the package stores and
    # that extract_covariate_name_from_tag() parses); guard double-prefixing.
    cov_tag   <- if (grepl("^beta_", info$cov)) info$cov else paste0("beta_", info$cov)
    cov_label <- if (!is.null(search_state$tags) && cov_tag %in% names(search_state$tags)) {
      search_state$tags[[cov_tag]]
    } else info$cov

    r <- base_row
    r$model_name       <- m
    r$step_description  <- paste(verb, cov_label)
    r$phase            <- phase
    r$step_number      <- step_number
    r$parent_model     <- base_model
    r$covariate_tested <- cov_tag
    r$action           <- action
    r$ofv <- NA_real_; r$delta_ofv <- NA_real_; r$rse_max <- NA_real_
    r$status           <- "created"
    r$submission_time  <- as.POSIXct(NA)
    r$completion_time  <- as.POSIXct(NA)
    r$tags             <- I(list(info$tags))
    new_rows[[m]]      <- r
    registered         <- c(registered, m)
  }
  search_state$search_database <- dplyr::bind_rows(db, dplyr::bind_rows(new_rows))

  # ---- counter (reuse the package routine; ignores run<N>001 retries) -------
  search_state <- update_model_counter(search_state)

  # ---- read each model's results from its files (fills ofv/status/delta_ofv) -
  for (m in registered) {
    search_state <- update_model_status_from_files(search_state, m, force = TRUE)
  }

  # ---- report ---------------------------------------------------------------
  if (length(skipped) > 0) {
    cat(sprintf("ℹ️  Skipped %d model(s) not part of this step: %s\n",
                length(skipped), paste(unique(skipped), collapse = ", ")))
  }
  reg <- search_state$search_database[
    search_state$search_database$model_name %in% registered,
    c("model_name", "covariate_tested", "parent_model", "status", "ofv", "delta_ofv")]
  cat(sprintf("\n✅ Registered %d model(s) into step %d (%s from %s)\n",
              length(registered), step_number, direction, base_model))
  print(reg, row.names = FALSE)
  pending <- reg$model_name[reg$status != "completed"]
  if (length(pending) > 0) {
    cat(sprintf("⚠️  Not 'completed' (rerun before resuming): %s\n",
                paste(pending, collapse = ", ")))
  }

  search_state
}
