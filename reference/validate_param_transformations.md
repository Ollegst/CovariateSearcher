# Validate parameter transformations for population covariates

For every parameter that will receive a POPULATION (time-constant)
covariate, checks that the base model writes it in a recognised form –
normal-scale `PARAM = TV * EXP(ETA)` or log-scale
`PARAM = EXP(TV + ETA)` – so `model_add_cov` knows whether to render the
covariate multiplicatively (`*`) or additively (`+`). Fails fast at
initialization on any parameter whose parameterization cannot be
classified. Also requires a log-parameterized population target to have
a separate `TV_<param>` line (the additive covariate is written on it,
inside the `EXP`); an inline `PARAM = EXP(THETA + ETA)` is rejected with
a refactor message, because the additive term would otherwise land
outside the `EXP`. Time-varying covariates are placed on the individual
parameter line (always multiplicative) and so do not require either
check.

## Usage

``` r
validate_param_transformations(
  covariate_search,
  data_file,
  id_col,
  model_name,
  models_folder = "models"
)
```

## Arguments

- covariate_search:

  data.frame with COVARIATE, PARAMETER, TIME_DEPENDENT.

- data_file:

  data.frame with the analysis dataset (for the time-varying check).

- id_col:

  Character. Subject identifier column name.

- model_name:

  Character. Base model name (without extension).

- models_folder:

  Character. Folder containing the model (default "models").

## Value

Invisibly `TRUE` if all population parameters are classifiable.
