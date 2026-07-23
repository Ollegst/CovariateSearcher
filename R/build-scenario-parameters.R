# =============================================================================
# SCENARIO PARAMETER SETS
# File: R/build-scenario-parameters.R
# Part of CovariateSearcher Package
# Covariate-scenario step of the exposure-metrics forest-plot workflow.
# =============================================================================

#' Build Parameter Sets for the Typical Subject and Each Covariate Scenario
#'
#' @description
#' Takes an already-sampled set of THETA vectors (from
#' [sample_individual_thetas()]) and, for each covariate scenario, **updates the
#' structural parameters** by applying the model's covariate relationships with
#' [apply_covariate_model()]. Scenarios come from [create_covariate_table()]:
#' row 1 is the typical subject (all covariates at reference), and each further
#' row varies one covariate across the requested percentiles (continuous) or
#' levels (categorical).
#'
#' The sampling of parameter uncertainty is already done upstream - this step
#' only re-expresses those same draws under each covariate condition. Because the
#' typical subject sits at reference covariate values, every covariate factor is
#' 1 there, so its parameter set is just the sampled structural thetas unchanged.
#' Each sample keeps the same `ID` across all scenarios, so a later step can pair
#' a scenario with the typical subject sample-by-sample.
#'
#' The scenario table (from [create_covariate_table()]) is also saved to
#' `scenario_table_path` as an RDS, so a later plot can restore the scenario
#' order from it.
#'
#' @param model Character. Model name without extension, e.g. "run28".
#' @param covariate_search Covariate-search table (consumed by
#'   [create_covariate_table()] and [apply_covariate_model()]), given as either a
#'   `data.frame` OR a character path to a `.csv`/`.rds` file to load.
#' @param thetas Sampled THETA vectors from [sample_individual_thetas()] (columns
#'   `THETA1`..`THETAn`, absolute scale; covariate-beta columns must be present,
#'   as they drive the factors), given as either a `data.frame` OR a character
#'   path to a `.csv`/`.rds` file to load.
#' @param data Analysis dataset (used by [create_covariate_table()] to compute
#'   continuous-covariate percentiles), given as either a `data.frame` OR a
#'   character path to a `.csv`/`.rds` file to load. It is used **as-is**: the
#'   model's `$DATA` `IGNORE`/`ACCEPT` filter is not applied, so pre-filter it to
#'   the modelling population before calling (the function warns to this effect).
#'   No decoding is needed - continuous covariates use numeric percentiles and
#'   categorical scenarios come from `covariate_search$LEVELS` + `lookup`.
#' @param percentiles Numeric vector of probabilities in \[0, 1] for continuous
#'   covariates, passed straight to [create_covariate_table()]. User-selectable.
#'   Default `c(0.05, 0.95)`.
#' @param models_folder Character. Folder containing the model. Default "models".
#' @param lookup Optional `lookup.yaml`-style named list keyed by covariate
#'   (the same object you read with `read_yaml()`), forwarded to
#'   [create_covariate_table()] so scenario labels show covariate **units** and
#'   **decoded categorical levels**. When `NULL` (default), raw covariate codes
#'   and levels are shown.
#' @param spec Optional yspec object, or a path to a spec YAML, forwarded to
#'   [create_covariate_table()] - a convenient alternative to `lookup`: the
#'   short/unit/values/decode are taken from the spec so scenario labels show
#'   units and decoded levels. Any explicit `lookup` entries override the spec.
#' @param id_col Character. Subject-ID column used to reduce `data` to one row
#'   per subject before computing percentiles. Forwarded to
#'   [create_covariate_table()]. Default "ID".
#' @param wrap_width Integer. Scenario-label wrap width, forwarded to
#'   [create_covariate_table()]. Default 30; `Inf` or `NULL` disables wrapping.
#' @param scenario_table_path Character or NULL. Where to save the scenario table
#'   (the [create_covariate_table()] result, in definition order) as an RDS for
#'   later reference - e.g. to restore the scenario order in
#'   [plot_exposure_forest()] via `scenario_order`. May be a full file path
#'   (".../foo.rds") OR a **directory** (a trailing slash, an existing folder, or
#'   any path without an `.rds` extension) - for a directory the default file
#'   name `scenario_table_<model>.rds` is written inside it. Defaults to
#'   `paste0("scenario_table_", model, ".rds")`; missing parent folders are
#'   created (nested). Pass `NULL` to skip saving.
#'
#' @return A named list of data frames, one per scenario (named by the scenario
#'   description). Each holds `ID` (sample index) plus the structural THETA
#'   columns with that scenario's covariate factors applied. The first element is
#'   the typical subject.
#'
#' @seealso [sample_individual_thetas()], [create_covariate_table()],
#'   [apply_covariate_model()]
#' @examples
#' \dontrun{
#' thetas <- sample_individual_thetas("run28", Nsamples = 1000)
#' param_sets <- build_scenario_parameters(
#'   model            = "run28",
#'   covariate_search = search_state$covariate_search,
#'   thetas           = thetas,
#'   data             = search_state$data_file,
#'   percentiles      = c(0.05, 0.95)
#' )
#' names(param_sets)       # "Typical subject", "High WT (95th ...)", ...
#' head(param_sets[[1]])   # typical-subject parameter draws
#' }
#' @export
build_scenario_parameters <- function(model,
                                      covariate_search,
                                      thetas,
                                      data,
                                      percentiles = c(0.05, 0.95),
                                      models_folder = "models",
                                      lookup = NULL,
                                      spec = NULL,
                                      id_col = "ID",
                                      wrap_width = 30,
                                      scenario_table_path =
                                        paste0("scenario_table_", model, ".rds")) {

  # Each data input may be given as an object OR a character path to load
  # (.csv/.rds) -- see .load_if_path(). `model` stays a name (models_folder).
  covariate_search <- .load_if_path(covariate_search, "covariate_search")
  thetas <- .load_if_path(thetas, "thetas")
  data <- .load_if_path(data, "data")

  if (!is.data.frame(thetas)) {
    stop("`thetas` must be a data.frame of sampled THETA vectors ",
         "(output of sample_individual_thetas()).")
  }

  # `data` drives the covariate distributions but is used AS-IS: the model's
  # $DATA IGNORE/ACCEPT filter is NOT applied here. Remind the caller to pass a
  # dataset already reduced to the modelling population. (No decoding needed:
  # continuous covariates use numeric percentiles and categorical scenarios come
  # from covariate_search$LEVELS + the `lookup` labels, not the data column.)
  warning(
    "build_scenario_parameters(): `data` is used as-is - it is NOT filtered to ",
    "match the model's $DATA (IGNORE/ACCEPT). Pre-filter `data` to the ",
    "modelling population (same row/subject exclusions as the NONMEM run) ",
    "before calling, so the covariate percentiles match the estimated data.",
    call. = FALSE
  )

  # Scenario table: row 1 = typical subject, rows 2+ = one-at-a-time variations.
  # `percentiles` flows straight through to create_covariate_table().
  scenarios <- create_covariate_table(
    model_name       = model,
    covariate_search = covariate_search,
    data             = data,
    percentiles      = percentiles,
    models_folder    = models_folder,
    id_col           = id_col,
    lookup           = lookup,
    spec             = spec,
    wrap_width       = wrap_width
  )

  # Persist the scenario table (labels + covariate values, in definition order)
  # so downstream forest plots can restore the scenario order. `NULL` skips it.
  if (!is.null(scenario_table_path)) {
    # Accept either a full file path (".../foo.rds") OR a directory - a trailing
    # slash, an existing directory, or a path with no .rds extension. For a
    # directory, write the default file name inside it, so e.g.
    #   scenario_table_path = "results/forestplots/rds/run16/"
    # saves results/forestplots/rds/run16/scenario_table_run16.rds instead of
    # trying to saveRDS() onto the folder itself.
    is_dir <- grepl("[/\\\\]$", scenario_table_path) ||
      dir.exists(scenario_table_path) ||
      !grepl("\\.[Rr][Dd][Ss]$", scenario_table_path)
    if (is_dir) {
      scenario_table_path <- file.path(
        sub("[/\\\\]+$", "", scenario_table_path),
        paste0("scenario_table_", model, ".rds")
      )
    }
    dir_out <- dirname(scenario_table_path)
    if (dir_out != "." && !dir.exists(dir_out)) {
      dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
    }
    saveRDS(scenarios, scenario_table_path)
  }

  cov_cols <- setdiff(names(scenarios), "Scenario")

  # For each scenario, update the structural parameters by applying the model's
  # covariate factors to the already-sampled thetas.
  param_sets <- lapply(seq_len(nrow(scenarios)), function(s) {
    apply_covariate_model(
      model_name        = model,
      covariate_search  = covariate_search,
      individual_thetas = thetas,
      covariates        = scenarios[s, cov_cols, drop = FALSE],
      models_folder     = models_folder
    )
  })

  names(param_sets) <- scenarios$Scenario
  # Carry the scenario table, its typical-subject description, and the resolved
  # covariate lookup through on `param_sets`, so plot_parameter_forests can show
  # the subtitle AND write the summary table (covariate columns) without the
  # caller re-supplying `scenario`/`lookup`.
  attr(param_sets, "typical_subject") <- attr(scenarios, "typical_subject")
  attr(param_sets, "scenario_table")  <- scenarios
  cov_lu <- lookup
  if (!is.null(spec)) {
    cov_lu <- utils::modifyList(.spec_to_lookup(spec),
                                if (is.null(lookup)) list() else lookup)
  }
  attr(param_sets, "covariate_lookup") <- cov_lu
  param_sets
}
