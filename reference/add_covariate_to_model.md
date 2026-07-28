# Add Covariate to Model

Add Covariate to Model

## Usage

``` r
add_covariate_to_model(
  search_state,
  base_model_id,
  covariate_tag,
  step_number = NULL,
  lookup_file = NULL,
  phase = "forward_selection"
)
```

## Arguments

- search_state:

  List. Current search state from initialize_covariate_search()

- base_model_id:

  Character. Base model identifier (e.g., "run1")

- covariate_tag:

  Character. Covariate tag to add (e.g., "beta_cl_wt")

- step_number:

  Integer. Step number this model belongs to. Required - the automated
  search passes its current step; a manual add must say which step the
  model belongs to.

- lookup_file:

  Character or NULL. Optional path to lookup YAML for categorical
  labels. If NULL, uses search_state configuration/default.

- phase:

  Character. Phase recorded in the search database. Defaults to
  `"forward_selection"`, which is what the automated forward search
  creates. Use `"individual_testing"` for a deliberate one-off add
  outside the search (e.g. trying an extra covariate on the final
  model), so the row is not mistaken for a step of the search itself.

## Value

List with updated search_state and new model information
