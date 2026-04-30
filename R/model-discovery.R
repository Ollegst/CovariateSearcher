# =============================================================================
# MODEL DISCOVERY
# File: R/model-discovery.R
# Part of CovariateSearcher Package
# Existing model discovery and analysis
# =============================================================================



#' Discover Existing Models
#'
#' @title Discover existing models and set up relationships
#' @description Scans the models folder and catalogs existing models
#' @param search_state List containing search state
#' @return Updated search_state with discovered models
#' @export
discover_existing_models <- function(search_state) {

  cat("Discovering existing models...\n")

  search_state <- initialize_search_database_core(search_state)

  model_files <- list.files(
    search_state$models_folder,
    pattern = "^run\\d+\\.(ctl|mod)$",
    full.names = FALSE
  )

  model_names <- gsub("\\.(ctl|mod)$", "", model_files)
  model_names <- unique(model_names)
  model_names <- model_names[order(as.numeric(gsub("run", "", model_names)))]

  if (length(model_names) == 0) {
    cat("No existing models found.\n")
    search_state$discovered_models <- character(0)
    return(search_state)
  }

  cat(
    "Found ", length(model_names), " existing models: ",
    paste(model_names, collapse = ", "), "\n",
    sep = ""
  )

  search_state$discovered_models <- model_names
  discovered_rows <- vector("list", length(model_names))

  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_path <- file.path(search_state$models_folder, model_name)

    parent_model <- NA_character_
    step_desc <- "Manual/External"
    phase <- "manual"
    action <- "manual_modification"
    covariate <- ""
    notes <- ""

    mod <- tryCatch(
      bbr::read_model(model_path),
      error = function(e) NULL
    )

    if (!is.null(mod)) {

      if (!is.null(mod$based_on) && length(mod$based_on) > 0) {
        parent_model <- mod$based_on[1]
      }

      notes <- tryCatch(
        if (length(mod$notes) > 0) mod$notes[1] else "",
        error = function(e) ""
      )

      if (notes != "" && grepl("^Step \\d+", notes)) {
        if (grepl("Retry", notes)) {
          action <- "retry"
          covariate <- gsub("^Step \\d+ Retry \\+ ", "", notes)
          step_desc <- sprintf("Retry %s", covariate)
          phase <- "retry"
        } else {
          action <- "add_single_covariate"
          covariate <- gsub("^Step \\d+ \\+ ", "", notes)
          step_desc <- sprintf("Add %s", covariate)
          phase <- "individual_testing"
        }
      } else if (notes != "" && grepl("^[+-]", notes)) {
        action <- if (startsWith(notes, "+")) "add_single_covariate" else "remove_single_covariate"
        covariate <- gsub("^[+-]\\s*", "", notes)
        step_desc <- paste(if (startsWith(notes, "+")) "Add" else "Remove", covariate)
        phase <- "individual_testing"
      } else if (notes != "") {
        step_desc <- notes
        if (is.na(parent_model)) {
          phase <- "manual"
          action <- "manual_modification"
        } else {
          phase <- "manual"
          action <- "manual_modification"
        }
      }
    }

    status <- get_model_status_from_files(model_path)

    ofv <- if (status == "completed") {
      tryCatch({
        ext_data <- read_nonmem_ext(model_path)
        ext_data$ofv
      }, error = function(e) NA_real_)
    } else {
      NA_real_
    }

    covariates <- get_model_covariates_from_files(search_state, model_name)

    model_tags <- tryCatch({
      if (!is.null(mod) && !is.null(mod$tags)) mod$tags else character(0)
    }, error = function(e) {
      character(0)
    })

    discovered_rows[[i]] <- data.frame(
      model_name = model_name,
      step_description = step_desc,
      phase = phase,
      step_number = NA_integer_,
      parent_model = parent_model,
      covariate_tested = if (nzchar(covariate)) covariate else paste(covariates, collapse = ";"),
      action = action,
      ofv = ofv,
      delta_ofv = NA_real_,
      rse_max = NA_real_,
      status = status,
      tags = I(list(model_tags)),
      submission_time = as.POSIXct(NA),
      completion_time = if (status == "completed") Sys.time() else as.POSIXct(NA),
      retry_attempt = ifelse(grepl("\\d{3}$", model_name), 1L, NA_integer_),
      original_model = NA_character_,
      estimation_issue = NA_character_,
      excluded_from_step = FALSE,
      stringsAsFactors = FALSE
    )
  }

  db <- dplyr::bind_rows(discovered_rows)

  if (!(search_state$base_model %in% db$model_name)) {
    stop(
      "Base model '", search_state$base_model,
      "' was not found among discovered models."
    )
  }

  get_step_number <- function(model_name, db, visited = character(0)) {
    if (model_name %in% visited) {
      stop("Circular parent relationship detected for model: ", model_name)
    }

    idx <- which(db$model_name == model_name)
    if (length(idx) == 0) {
      return(NA_integer_)
    }

    parent <- db$parent_model[idx[1]]

    if (is.na(parent) || parent == "" || !(parent %in% db$model_name)) {
      return(0L)
    }

    parent_step <- get_step_number(parent, db, c(visited, model_name))
    if (is.na(parent_step)) {
      return(NA_integer_)
    }

    parent_step + 1L
  }

  is_descendant_of <- function(model_name, ancestor_name, db, visited = character(0)) {
    if (model_name %in% visited) {
      stop("Circular parent relationship detected for model: ", model_name)
    }

    idx <- which(db$model_name == model_name)
    if (length(idx) == 0) {
      return(FALSE)
    }

    parent <- db$parent_model[idx[1]]

    if (is.na(parent) || parent == "") {
      return(FALSE)
    }

    if (identical(parent, ancestor_name)) {
      return(TRUE)
    }

    if (!(parent %in% db$model_name)) {
      return(FALSE)
    }

    is_descendant_of(parent, ancestor_name, db, c(visited, model_name))
  }

  historical_step <- vapply(
    db$model_name,
    get_step_number,
    integer(1),
    db = db
  )

  base_idx <- which(db$model_name == search_state$base_model)
  base_hist_step <- historical_step[base_idx]

  # Default all discovered models to historical/manual and non-search step
  db$step_number <- -1L
  db$phase <- ifelse(db$phase == "retry", db$phase, "manual")
  db$action <- ifelse(db$action == "retry", db$action, "manual_modification")
  db$step_description <- ifelse(
    db$model_name == search_state$base_model,
    db$step_description,
    "Historical predecessor"
  )

  # Active search base
  db$step_number[base_idx] <- 0L
  db$phase[base_idx] <- "base"
  db$action[base_idx] <- "base_model"
  db$step_description[base_idx] <- "Base Model"

  # Descendants of active base
  descendant_idx <- which(vapply(
    db$model_name,
    is_descendant_of,
    logical(1),
    ancestor_name = search_state$base_model,
    db = db
  ))

  if (length(descendant_idx) > 0) {
    db$step_number[descendant_idx] <- historical_step[descendant_idx] - base_hist_step
  }

  search_state$search_database <- db

  cat("Added ", nrow(db), " models to search database.\n", sep = "")
  return(search_state)
}
