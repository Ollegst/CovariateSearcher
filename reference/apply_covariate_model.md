# Apply a Model's Covariate Relationships to Individual Thetas

Reproduces a NONMEM model's typical-subject structural parameters in R
for one covariate scenario by **evaluating the model's own `$PK`
equations** with the sampled THETA draws and `ETA = 0`. Covariate
effects (already written into the equations) and any log/normal
parameterization are handled automatically, because the actual equation
is run: `CL = EXP(TV_CL + 0)` yields CL on the natural scale directly.
There is no separate covariate-factor step and no transform detection,
so the R result cannot drift from the NONMEM model.

Categorical covariates (written as `$PK` `IF/ELSEIF` blocks) are handled
the same way – the block is evaluated for the scenario's covariate
value.

## Usage

``` r
apply_covariate_model(
  model_name,
  covariate_search,
  individual_thetas,
  covariates,
  models_folder = "models"
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
  `EXP` back-transforms it). One row per draw.

- covariates:

  A single covariate scenario: a named vector or one-row data frame of
  covariate values (e.g. one row of
  [`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md)).
  Applied to every draw. Extra names (such as `Scenario`) are ignored.

- models_folder:

  Character. Folder containing the model. Default "models".

## Value

A `data.frame` with an `ID` column and the structural (non-`beta_`)
THETA columns, each holding that parameter's value for the scenario on
the **natural scale**, obtained by evaluating the model's own `$PK`
equations with `ETA = 0`. One row per row of `individual_thetas`.
