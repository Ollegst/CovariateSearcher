# Process detected estimation issues with smart retry/exclusion logic

Orchestrates the recovery process for models with estimation issues.
Creates retries for original models or excludes covariates for failed
retries.

## Usage

``` r
process_estimation_issues(search_state, models_with_issues)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- models_with_issues:

  List. Models with issues from detect_estimation_problems

## Value

List with recovery actions taken and updated search_state

## Details

Process Estimation Issues
