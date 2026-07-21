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
#'   character path to a `.csv`/`.rds` file to load.
#' @param percentiles Numeric vector of probabilities in \[0, 1] for continuous
#'   covariates, passed straight to [create_covariate_table()]. User-selectable.
#'   Default `c(0.05, 0.95)`.
#' @param models_folder Character. Folder containing the model. Default "models".
#' @param lookup Optional `lookup.yaml`-style named list keyed by covariate
#'   (the same object you read with `read_yaml()`), forwarded to
#'   [create_covariate_table()] so scenario labels show covariate **units** and
#'   **decoded categorical levels**. When `NULL` (default), raw covariate codes
#'   and levels are shown.
#' @param id_col Character. Subject-ID column used to reduce `data` to one row
#'   per subject before computing percentiles. Forwarded to
#'   [create_covariate_table()]. Default "ID".
#' @param wrap_width Integer. Scenario-label wrap width, forwarded to
#'   [create_covariate_table()]. Default 30; `Inf` or `NULL` disables wrapping.
#' @param scenario_table_path Character or NULL. Where to save the scenario table
#'   (the [create_covariate_table()] result, in definition order) as an RDS for
#'   later reference - e.g. to restore the scenario order in
#'   [plot_exposure_forest()] via `scenario_order`. Defaults to
#'   `paste0("scenario_table_", model, ".rds")`; missing parent folders are
#'   created. Pass `NULL` to skip saving.
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
    wrap_width       = wrap_width
  )

  # Persist the scenario table (labels + covariate values, in definition order)
  # so downstream forest plots can restore the scenario order. `NULL` skips it.
  if (!is.null(scenario_table_path)) {
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
  param_sets
}
