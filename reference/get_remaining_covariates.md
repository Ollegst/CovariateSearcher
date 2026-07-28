# Get list of covariate tags that haven't been tested from base model

Identifies which covariates from the search definition haven't been
added to the specified base model yet, with exclusion filtering.

## Usage

``` r
get_remaining_covariates(search_state, base_model_id, include_excluded = TRUE)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- base_model_id:

  Character. Model to check current covariates against

- include_excluded:

  Logical. Whether to include excluded covariates (default: TRUE)

## Value

Character vector of covariate tag names that can still be tested

## Details

Get Remaining Covariates for Testing
