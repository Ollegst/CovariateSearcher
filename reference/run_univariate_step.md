# Run Univariate Step

Run Univariate Step

## Usage

``` r
run_univariate_step(
  search_state,
  base_model_id,
  covariates_to_test = NULL,
  step_name,
  include_excluded = TRUE
)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- base_model_id:

  Character. Base model to test from

- covariates_to_test:

  Character vector. Covariate tags to test (optional)

- step_name:

  Character. Description for this step

- include_excluded:

  Logical. Whether to include excluded covariates (default: TRUE for
  SCM)

## Value

List with created model information and updated search_state
