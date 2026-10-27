# Execute complete stepwise covariate modeling algorithm

Main orchestration function that runs the complete SCM workflow:

1.  Initial univariate analysis on base model

2.  Iterative forward selection steps Covariates excluded after a failed
    retry are offered again at every step when
    `search_config$retest_failed` is TRUE (the default). All models
    within each step run in parallel.

## Usage

``` r
run_stepwise_covariate_modeling(
  search_state,
  base_model_id = NULL,
  auto_submit = TRUE,
  forward_p_value = NULL,
  rse_threshold = NULL
)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- base_model_id:

  Character. Starting base model

- auto_submit:

  Logical. Whether to automatically submit models (default: TRUE)

- forward_p_value:

  Numeric. P-value for forward selection (uses config if NULL)

- rse_threshold:

  Numeric. Maximum RSE threshold (uses config if NULL)

## Value

List with complete SCM results and updated search_state

## Details

Run Complete Stepwise Covariate Modeling
