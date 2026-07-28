# Create retry model with modified initial estimates

Creates a retry model (e.g., run2001 from run2) with the problematic
covariate's THETA started from a different initial estimate (see
[`adjust_theta_for_covariate`](https://ollegst.github.io/CovariateSearcher/reference/adjust_theta_for_covariate.md)).
When every THETA for that covariate is FIXED there is nothing to
perturb, so no retry is created and the result carries
`status = "skipped"`.

## Usage

``` r
create_retry_model(
  search_state,
  original_model_name,
  issue_type = "estimation_error"
)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- original_model_name:

  Character. Name of problematic model

- issue_type:

  Character. Type of estimation issue detected

## Value

List with retry model information and updated search_state; `status` is
one of `"created"`, `"skipped"` or `"failed"`

## Details

Create Retry Model with Adjusted THETA Values
