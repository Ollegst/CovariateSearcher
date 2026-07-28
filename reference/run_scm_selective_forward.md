# Execute proper stepwise forward selection with cumulative model building

Runs true forward selection where each step builds from the best model
of the previous step, testing only remaining (untested) covariates.
Implements standard pharmacometrics SCM methodology.

## Usage

``` r
run_scm_selective_forward(
  search_state,
  base_model_id = NULL,
  forward_p_value = NULL,
  rse_threshold = NULL,
  auto_submit = TRUE,
  auto_retry = TRUE,
  resume = FALSE
)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- base_model_id:

  Character. Starting base model (default: "run1")

- forward_p_value:

  Numeric. P-value for forward selection (uses config if NULL)

- rse_threshold:

  Numeric. Maximum RSE threshold (uses config if NULL)

- auto_submit:

  Logical. Whether to automatically submit models (default: TRUE)

- auto_retry:

  Logical. Whether to enable automatic retry (default: TRUE)

- resume:

  Logical. If TRUE, treat this call as a continuation (used by
  [`continue_search`](https://ollegst.github.io/CovariateSearcher/reference/continue_search.md)):
  the first loop pass skips the "test all" step and goes straight to
  selective narrowing, reconstructing the previous step from the
  database via
  [`get_step_models`](https://ollegst.github.io/CovariateSearcher/reference/get_step_models.md).
  Fresh runs use FALSE (default) and begin by testing all available
  covariates.

## Value

List with updated search_state and forward selection results
