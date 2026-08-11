# Build a covariate parameter-search reference table

Combines a user-specified Parameter/Covariate/Category/Formula mapping
with subject-level longitudinal data to automatically compute reference
values (median for continuous covariates, most prevalent level for
categorical covariates) from baseline (Time == 0) data, detect
categorical LEVELS from the FULL dataset ordered to match `yaml_data`
(so a level that only appears away from baseline, e.g. via a
time-varying dose under a power formula, is still captured), cross-check
those levels against the YAML covariate specification, and determine
whether each covariate is time-dependent using the FULL dataset (a
time-dependent covariate emits a
[`warning()`](https://rdrr.io/r/base/warning.html) noting that its
REFERENCE still reflects baseline only). The result is a long table
ready for
[`validate_covariate_search_table`](https://ollegst.github.io/CovariateSearcher/reference/validate_covariate_search_table.md)
/
[`initialize_covariate_search`](https://ollegst.github.io/CovariateSearcher/reference/initialize_covariate_search.md).

## Usage

``` r
build_covariate_reference_table(
  data,
  id,
  time,
  Parameter,
  Covariate,
  Category,
  Formula,
  yaml_data,
  INIT = NULL
)
```

## Arguments

- data:

  A data.frame containing subject-level longitudinal data, including an
  ID column, a time column, and all covariates referenced in
  `Covariate`.

- id:

  Name (string) of the subject identifier column in `data`.

- time:

  Name (string) of the time column in `data`.

- Parameter:

  Character vector naming the PK/PD parameter for each row (e.g. "CL",
  "CL", "V"), aligned by position with the other spec vectors.

- Covariate:

  Character vector naming the covariate for each row.

- Category:

  Character vector, one of "con" or "cat" per row.

- Formula:

  Character vector. Per row, either a built-in shortcut – continuous:
  `"linear"`, `"power"`, `"exponential"`; categorical: `"linear"`
  (per-level) or `"power"` (numeric levels) – or a single-factor
  expression written in the reserved symbols `cov` (the covariate) and
  `ref` (its REFERENCE) plus one symbol per estimated THETA, e.g.
  `"EMAX*cov/(EC50+cov)"`. (`"power1"`/`"power0.75"` were removed: use
  `"power"` with a fixed `INIT` such as `"1 FIX"` / `"0.75 FIX"`.)

- yaml_data:

  A named list (parsed YAML) where each categorical covariate has a
  `values` element listing its valid numeric codes.

- INIT:

  Optional character vector, aligned by position with the spec vectors,
  giving the initial `$THETA` value per row (e.g. `"0.1"`,
  `"(0, 0.5, 2)"`, `"1 FIX"`, or a named spec for multi-parameter
  expressions such as `"EMAX=0.5; EC50=10"`). `NULL` (default) omits the
  column, so every beta uses the formula default (`0.1`).

## Value

A data.frame with columns PARAMETER, COVARIATE, STATUS, FORMULA, LEVELS,
REFERENCE, TIME_DEPENDENT (and INIT when supplied) – one row per
(Parameter, Covariate) pair, matching the input spec order.

## See also

[`validate_covariate_search_table`](https://ollegst.github.io/CovariateSearcher/reference/validate_covariate_search_table.md),
[`initialize_covariate_search`](https://ollegst.github.io/CovariateSearcher/reference/initialize_covariate_search.md)
