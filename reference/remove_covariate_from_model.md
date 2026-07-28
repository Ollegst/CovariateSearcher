# Remove covariate using tag name with functional state update

Removes a covariate from a model using tag-based interface. Returns
updated search_state.

## Usage

``` r
remove_covariate_from_model(
  search_state,
  model_name,
  covariate_tag,
  save_as_new_model = TRUE,
  step_number = NULL
)
```

## Arguments

- search_state:

  List containing search state

- model_name:

  Character. Model name to modify

- covariate_tag:

  Character. Covariate tag to remove (e.g., "cov_cl_race")

- save_as_new_model:

  Logical. Whether to create new model (default: TRUE)

- step_number:

  Integer or NULL. Optional step number for the new model's database
  row. NULL (default) auto-calculates as the parent model's step + 1, so
  covariates removed from the same parent share a step (one round).

## Value

List with updated search_state and operation details

## Details

Remove Covariate from Model (Clean Interface)
