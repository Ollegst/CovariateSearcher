# Validate covariate search table

Performs reusable checks on the covariate search table and returns the
validated table. Creates `cov_to_test` if it does not exist.

## Usage

``` r
validate_covariate_search_table(covariate_search, data_file)
```

## Arguments

- covariate_search:

  data.frame with covariate search specification

- data_file:

  data.frame with analysis dataset

## Value

validated covariate_search data.frame
