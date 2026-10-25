# Apply a Model's Covariate Relationships to Individual Thetas

Reproduces a NONMEM model's typical-subject structural parameters in R
for one or many covariate scenarios by **evaluating the model's own
`$PK` equations** with the sampled THETA draws and `ETA = 0`. Covariate
effects (already written into the equations) and any log/normal
parameterization are handled automatically, because the actual equation
is run: `CL = EXP(TV_CL + 0)` yields CL on the natural scale directly.
There is no separate covariate-factor step and no transform detection,
so the R result cannot drift from the NONMEM model.

Categorical covariates (written as `$PK` `IF/ELSEIF` blocks) are handled
the same way – the block is evaluated for the scenario's covariate
value. When `covariates` supplies one value per row instead (real
per-subject combinations), each row is routed through the
`IF/ELSEIF/ELSE` branch its own covariate values select, independently
of every other row.

## Usage

``` r
apply_covariate_model(
  model_name,
  covariate_search,
  individual_thetas,
  covariates,
  models_folder = "models",
  eta = FALSE,
  seed = 1234
)
```

## Arguments

- model_name:

  Character. Model name without extension, e.g. "run28".

- covariate_search:

  Covariate-search table with columns `COVARIATE`, `PARAMETER`,
  `STATUS`, `FORMULA`, `REFERENCE` (`REFERENCE` supplies the value for
  any covariate an equation references that is not in `covariates`),
  given as either a `data.frame` OR a character path to a `.csv`/`.rds`
  file to load.

- individual_thetas:

  Data frame whose columns are `THETA1`, `THETA2`, ... the sampled THETA
  draws on the **estimation scale** (raw, as returned by
  [`sample_individual_thetas()`](https://ollegst.github.io/CovariateSearcher/reference/sample_individual_thetas.md);
  a log-parameterised THETA stays on the log scale – the equation's
  `EXP` back-transforms it). One row per draw; when `eta = TRUE`, row
  *i*'s THETA draw is paired with ETA draw *i*.

- covariates:

  A covariate scenario: a named vector or one-row data frame of
  covariate values (e.g. one row of
  [`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md))
  is applied to every draw. A data frame with the same number of rows as
  `individual_thetas` instead pairs row *i*'s covariate values with row
  *i*'s THETA (and ETA, when `eta = TRUE`) draw – e.g. real per-subject
  covariate combinations pulled straight from a dataset. In the per-row
  form, a column is only treated as a covariate if its name is listed in
  `covariate_search$COVARIATE`; anything else (such as `Scenario`) is
  ignored outright, whatever it contains. Every row of a REGISTERED
  covariate needs a real value: an `NA` for even one row **stops**
  rather than falling back to `covariate_search`'s `REFERENCE` – real
  per-subject data is expected to be complete, so a missing value there
  is treated as an error to fix upstream, not something to patch over
  silently.

- models_folder:

  Character. Folder containing the model. Default "models".

- eta:

  Logical. If `TRUE`, draw one ETA vector per row of `individual_thetas`
  from the model's final `$OMEGA` matrix (read from its `.ext` file, the
  same way
  [`sample_individual_thetas()`](https://ollegst.github.io/CovariateSearcher/reference/sample_individual_thetas.md)
  reads THETA uncertainty) and evaluate `$PK` with those draws instead
  of `ETA = 0`, so the result carries between-subject variability on top
  of the sampled THETA uncertainty and covariate effect. `OMEGA`
  elements never estimated (e.g. off-diagonal under a diagonal `$OMEGA`)
  are treated as 0 (uncorrelated). If `$PK` references an `ETA(k)`
  beyond the OMEGA matrix's size, **stops** rather than silently falling
  back (checked once, up front, before `$PK` is interpreted). Default
  `FALSE` reproduces the typical-subject value for the scenario.

- seed:

  Integer. Random seed for the ETA draw when `eta = TRUE`. Ignored
  otherwise. Default 1234.

## Value

A `data.frame` with an `ID` column and the structural (non-`beta_`)
THETA columns, each holding that parameter's value for the scenario on
the **natural scale**, obtained by evaluating the model's own `$PK`
equations with `ETA = 0` (or, when `eta = TRUE`, a sampled ETA draw).
One row per row of `individual_thetas`.
