# Build Parameter Sets for the Typical Subject and Each Covariate Scenario

Takes an already-sampled set of THETA vectors (from
[`sample_individual_thetas()`](https://ollegst.github.io/CovariateSearcher/reference/sample_individual_thetas.md))
and, for each covariate scenario, **updates the structural parameters**
by applying the model's covariate relationships with
[`apply_covariate_model()`](https://ollegst.github.io/CovariateSearcher/reference/apply_covariate_model.md).
Scenarios come from
[`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md):
row 1 is the typical subject (all covariates at reference), and each
further row varies one covariate across the requested percentiles
(continuous) or levels (categorical).

The sampling of parameter uncertainty is already done upstream - this
step only re-expresses those same draws under each covariate condition.
Because the typical subject sits at reference covariate values, every
covariate factor is 1 there, so its parameter set is just the sampled
structural thetas unchanged. Each sample keeps the same `ID` across all
scenarios, so a later step can pair a scenario with the typical subject
sample-by-sample.

The scenario table (from
[`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md))
is also saved to `scenario_table_path` as an RDS, so a later plot can
restore the scenario order from it.

## Usage

``` r
build_scenario_parameters(
  model,
  covariate_search,
  thetas,
  data,
  percentiles = c(0.05, 0.95),
  models_folder = "models",
  lookup = NULL,
  spec = NULL,
  id_col = "ID",
  wrap_width = 30,
  scenario_table_path = paste0("scenario_table_", model, ".rds")
)
```

## Arguments

- model:

  Character. Model name without extension, e.g. "run28".

- covariate_search:

  Covariate-search table (consumed by
  [`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md)
  and
  [`apply_covariate_model()`](https://ollegst.github.io/CovariateSearcher/reference/apply_covariate_model.md)),
  given as either a `data.frame` OR a character path to a `.csv`/`.rds`
  file to load.

- thetas:

  Sampled THETA vectors from
  [`sample_individual_thetas()`](https://ollegst.github.io/CovariateSearcher/reference/sample_individual_thetas.md)
  (columns `THETA1`..`THETAn`, absolute scale; covariate-beta columns
  must be present, as they drive the factors), given as either a
  `data.frame` OR a character path to a `.csv`/`.rds` file to load.

- data:

  Analysis dataset (used by
  [`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md)
  to compute continuous-covariate percentiles), given as either a
  `data.frame` OR a character path to a `.csv`/`.rds` file to load. It
  is used **as-is**: the model's `$DATA` `IGNORE`/`ACCEPT` filter is not
  applied, so pre-filter it to the modelling population before calling
  (the function warns to this effect). No decoding is needed -
  continuous covariates use numeric percentiles and categorical
  scenarios come from `covariate_search$LEVELS` + `lookup`.

- percentiles:

  Numeric vector of probabilities in \[0, 1\] for continuous covariates,
  passed straight to
  [`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md).
  User-selectable. Default `c(0.05, 0.95)`.

- models_folder:

  Character. Folder containing the model. Default "models".

- lookup:

  Optional `lookup.yaml`-style named list keyed by covariate (the same
  object you read with `read_yaml()`), forwarded to
  [`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md)
  so scenario labels show covariate **units** and **decoded categorical
  levels**. When `NULL` (default), raw covariate codes and levels are
  shown.

- spec:

  Optional yspec object, or a path to a spec YAML, forwarded to
  [`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md) -
  a convenient alternative to `lookup`: the short/unit/values/decode are
  taken from the spec so scenario labels show units and decoded levels.
  Any explicit `lookup` entries override the spec.

- id_col:

  Character. Subject-ID column used to reduce `data` to one row per
  subject before computing percentiles. Forwarded to
  [`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md).
  Default "ID".

- wrap_width:

  Integer. Scenario-label wrap width, forwarded to
  [`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md).
  Default 30; `Inf` or `NULL` disables wrapping.

- scenario_table_path:

  Character or NULL. Where to save the scenario table (the
  [`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md)
  result, in definition order) as an RDS for later reference - e.g. to
  restore the scenario order in
  [`plot_exposure_forest()`](https://ollegst.github.io/CovariateSearcher/reference/plot_exposure_forest.md)
  via `scenario_order`. May be a full file path (".../foo.rds") OR a
  **directory** (a trailing slash, an existing folder, or any path
  without an `.rds` extension) - for a directory the default file name
  `scenario_table_<model>.rds` is written inside it. Defaults to
  `paste0("scenario_table_", model, ".rds")`; missing parent folders are
  created (nested). Pass `NULL` to skip saving.

## Value

A named list of data frames, one per scenario (named by the scenario
description). Each holds `ID` (sample index) plus the structural THETA
columns with that scenario's covariate factors applied. The first
element is the typical subject.

## See also

[`sample_individual_thetas()`](https://ollegst.github.io/CovariateSearcher/reference/sample_individual_thetas.md),
[`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md),
[`apply_covariate_model()`](https://ollegst.github.io/CovariateSearcher/reference/apply_covariate_model.md)

## Examples

``` r
if (FALSE) { # \dontrun{
thetas <- sample_individual_thetas("run28", Nsamples = 1000)
param_sets <- build_scenario_parameters(
  model            = "run28",
  covariate_search = search_state$covariate_search,
  thetas           = thetas,
  data             = search_state$data_file,
  percentiles      = c(0.05, 0.95)
)
names(param_sets)       # "Typical subject", "High WT (95th ...)", ...
head(param_sets[[1]])   # typical-subject parameter draws
} # }
```
