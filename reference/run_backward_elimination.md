# Execute backward elimination from a forward selection result

Iteratively removes covariates that have minimal impact on model fit.
Removes the covariate with smallest ΔOFV increase if below threshold.

## Usage

``` r
run_backward_elimination(
  search_state,
  starting_model,
  backward_p_value = NULL,
  auto_submit = TRUE,
  auto_retry = TRUE,
  rse_threshold = NULL
)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- starting_model:

  Character. Model to start backward elimination from

- backward_p_value:

  Numeric. p-value threshold for removal (default: 0.001)

- auto_submit:

  Logical. Whether to automatically submit models (default: TRUE)

- auto_retry:

  Logical. Whether to enable automatic retry (default: TRUE)

- rse_threshold:

  Numeric. Maximum RSE threshold as percentage. If NULL, uses
  search_state\$search_config\$max_rse_threshold (default: 50)

## Value

List with backward elimination results and updated search_state

## Details

Run Backward Elimination
