# Core functionality to add covariate to NONMEM model file with enhanced logging

Modifies NONMEM control file to add covariate relationship with detailed
logging

## Usage

``` r
model_add_cov(
  search_state,
  ref_model,
  cov_on_param,
  id_var = "ID",
  data_file,
  covariate_search,
  capture_log = FALSE,
  lookup_file = NULL
)
```

## Arguments

- search_state:

  List containing search state

- ref_model:

  Character. Model name to modify

- cov_on_param:

  Character. Combined covariate-parameter name (e.g., "WT_CL")

- id_var:

  Character. ID variable name (default: "ID")

- data_file:

  Data.frame. Dataset for time-varying checks

- covariate_search:

  Data.frame. Covariate search configuration

- capture_log:

  Function. Logging function (optional)

- lookup_file:

  Character or NULL. Optional path to lookup YAML for categorical
  labels. If NULL, uses search_state\$search_config\$lookup_file then
  defaults to data/spec/lookup.yaml.

## Value

Updated search_state
