# Extract models that showed significant improvement in a specific step

Returns model names from a step that have ΔOFV above threshold.
Threshold is calculated per model based on covariate degrees of freedom.

## Usage

``` r
get_significant_models_from_step(
  search_state,
  step_number,
  p_value,
  rse_threshold = NULL,
  phase = c("forward", "backward")
)
```

## Arguments

- search_state:

  List containing search state

- step_number:

  Integer. Step number to check

- p_value:

  Numeric. P-value for significance testing (e.g., 0.05 for forward,
  0.01 for backward)

- rse_threshold:

  Numeric. RSE threshold for significance

- phase:

  Character. Which acceptance rule to apply, `"forward"` (default) or
  `"backward"`. The two are not interchangeable: they store `delta_ofv`
  with opposite signs and compare it in opposite directions.

## Value

Character vector of significant model names

## Details

Get Significant Models from Step
