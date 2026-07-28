# Get list of covariates excluded from current step with details

Returns covariates that have been excluded due to estimation issues

## Usage

``` r
get_excluded_covariates(
  search_state,
  return_details = FALSE,
  phase_filter = NULL
)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- return_details:

  Logical. Whether to return detailed exclusion info (default: FALSE)

- phase_filter:

  Character vector of phases to include (e.g., "forward"). If NULL,
  include exclusions from all phases.

## Value

Character vector of excluded covariate names, or data.frame if
return_details=TRUE

## Details

Get Excluded Covariates
