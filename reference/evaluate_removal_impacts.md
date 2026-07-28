# Evaluate the impact of removing each covariate

Calculates ΔOFV for each removal and identifies the covariate with
smallest impact that meets the threshold for removal

## Usage

``` r
evaluate_removal_impacts(
  search_state,
  base_model,
  removal_models,
  completed_models,
  backward_p_value,
  rse_threshold = NULL
)
```

## Arguments

- search_state:

  List containing search state

- base_model:

  Character. Current base model name

- removal_models:

  List. Named list of removal test models

- completed_models:

  Character vector. Successfully completed models

- backward_p_value:

  Numeric. P-value for backward elimination

- rse_threshold:

  Numeric. Maximum RSE threshold as percentage. If NULL, uses
  search_state\$search_config\$max_rse_threshold (default: 50)

## Value

List with evaluation results and covariate to remove

## Details

Evaluate Removal Impacts
