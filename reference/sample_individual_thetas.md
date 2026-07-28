# Sample THETA Parameter Vectors from Estimation Uncertainty

Draws `Nsamples` vectors of the model's THETA parameters (structural
parameters + covariate betas) from the NONMEM estimation uncertainty,
and returns them **on the estimation scale exactly as NONMEM reports
them** (a log-parameterised THETA stays on the log scale).
[`apply_covariate_model()`](https://ollegst.github.io/CovariateSearcher/reference/apply_covariate_model.md)
then evaluates the model's own `$PK` equations, whose `EXP(...)`
performs the log -\> natural transformation once, at the right place.

Sampling is done on the estimation scale that the covariance matrix
describes:

- If a `.cov` (or `.cor`) file is available, draws come from the full
  multivariate-normal uncertainty distribution, preserving parameter
  correlations (via mvtnorm).

- If neither is available or usable, falls back to sampling each THETA
  independently from its `.ext` standard error, with a warning.

No scale transformation is applied here: the draws are returned raw so
the back-transform lives in exactly one place (the `EXP(...)` in the
model's `$PK` equation, evaluated by
[`apply_covariate_model()`](https://ollegst.github.io/CovariateSearcher/reference/apply_covariate_model.md)).
This avoids the double [`exp()`](https://rdrr.io/r/base/Log.html) that a
tag-based back-transform here would cause. A correct log-normal
(always-positive) draw for a log-estimated structural parameter
therefore arises when its `$PK` line is evaluated downstream.

## Usage

``` r
sample_individual_thetas(
  model,
  models_folder = "models",
  Nsamples = 1e+05,
  seed = 1234
)
```

## Arguments

- model:

  Character. Model name without extension, e.g. "run28".

- models_folder:

  Character. Folder containing the model. Default "models". Both flat
  (`models/run28.ext`) and per-model-subfolder
  (`models/run28/run28.ext`) layouts are supported.

- Nsamples:

  Integer. Number of uncertainty draws. Default 1e5.

- seed:

  Integer. Random seed for reproducibility. Default 1234.

## Value

A `data.frame` with `Nsamples` rows and columns `ID` (1:Nsamples) and
`THETA1`, `THETA2`, ..., `THETAn` on the **estimation scale** (raw
`.ext` scale; a log-parameterised THETA stays on the log scale), with
THETA order matching the model's `$THETA` block. The attribute
`"sampling_method"` is set to `"mvn"` or `"independent"` to record which
path was used.

## Examples

``` r
if (FALSE) { # \dontrun{
samples <- sample_individual_thetas("run28", models_folder = "models",
                                    Nsamples = 1000)
attr(samples, "sampling_method")
# Feed straight into the covariate model for one scenario:
# apply_covariate_model("run28", covariate_search, samples, scenario_row)
} # }
```
