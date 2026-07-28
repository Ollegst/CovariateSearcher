# Create a Covariate Table (Null Patient + One-at-a-Time Variations)

Builds a covariate table for covariate-effect / forest-plot style
analyses. The first row is the "null patient": every covariate held at
its reference value. Each subsequent row varies exactly one covariate
while all others stay at reference. Continuous covariates are varied
across the requested data percentiles; categorical covariates are varied
across their non-reference levels.

Covariates are taken from the model (its `beta_<COV>_<PARAM>` THETA
names). Reference values, covariate type, and categorical levels come
from the covariate-search object. Percentiles for continuous covariates
are computed from the supplied dataset, using one row per subject.

## Usage

``` r
create_covariate_table(
  model_name,
  covariate_search,
  data,
  percentiles = c(0.05, 0.95),
  models_folder = "models",
  id_col = "ID",
  lookup = NULL,
  spec = NULL,
  wrap_width = 30
)
```

## Arguments

- model_name:

  Character. Model name without extension, e.g. "run28".

- covariate_search:

  Data frame. Covariate-search table with columns `COVARIATE`, `STATUS`
  ("con"/"cat"), `REFERENCE`, and `LEVELS` (semicolon-separated levels
  for categoricals, e.g. "0;1;2"). If a `cov_to_test` column is absent
  it is derived from `COVARIATE`/`PARAMETER`.

- data:

  Data frame. The analysis dataset used to compute continuous covariate
  percentiles.

- percentiles:

  Numeric vector of probabilities in \[0, 1\] used to vary continuous
  covariates. Default `c(0.05, 0.95)`.

- models_folder:

  Character. Folder containing the model. Default "models".

- id_col:

  Character. Subject ID column in `data`, used to de-duplicate to one
  row per subject before computing percentiles. Default "ID".

- lookup:

  Optional named list (lookup.yaml-style) keyed by covariate, used only
  to render the `Scenario` description. Each entry may hold `short` or
  `label` (a human name for the covariate), `unit` (continuous unit) and
  `values`/`decode` (to translate a categorical level to its label).
  When absent, the covariate code and raw level are used.

- spec:

  Optional yspec object, or a path to a spec YAML (loaded with
  [`yspec::ys_load()`](https://rdrr.io/pkg/yspec/man/ys_load.html)) - an
  alternative to `lookup`: the `short`/`unit`/ `values`/`decode` are
  read from the spec's columns. Any explicit `lookup` entries take
  precedence over the spec-derived ones.

- wrap_width:

  Integer. If a `Scenario` string is longer than this many characters, a
  line break is inserted so the "(...)" detail (continuous) or the
  category level (categorical) moves onto a second line, which helps
  long labels fit on plots. Set to `Inf` or `NULL` to disable. Default
  30.

## Value

A `data.frame` whose first column `Scenario` describes each row
("Typical subject" for the null patient, "Low/High (Nth percentile)" for
continuous, ": " for categorical), followed by one column per covariate.
Row 1 is the null patient (all reference values); each further row is a
single-covariate variation.

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- create_covariate_table(
  model_name       = "run28",
  covariate_search = search_state$covariate_search,
  data             = search_state$data_file,
  percentiles      = c(0.05, 0.95)
)
} # }
```
