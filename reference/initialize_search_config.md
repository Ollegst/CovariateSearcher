# Initialize search configuration parameters

Sets up default configuration for SCM workflow

## Usage

``` r
initialize_search_config(
  search_state,
  lookup_file = NULL,
  require_cov_step = TRUE
)
```

## Arguments

- search_state:

  List containing search state

- lookup_file:

  Character or NULL. Optional path to lookup YAML for categorical
  covariate labels.

- require_cov_step:

  Logical. Whether a successful covariance step (a `.cov` file) is
  required for a model to count as completed (default: TRUE). Stored in
  the config so model discovery, which runs straight after this, applies
  the user's setting rather than a hard-coded one.

## Value

Updated search_state with initialized configuration

## Details

Initialize Search Configuration
