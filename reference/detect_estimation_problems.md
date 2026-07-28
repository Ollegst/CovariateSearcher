# Monitor multiple models for estimation problems

Checks a list of models for estimation issues by reading their .ext
files and detecting boundary failures, convergence problems, etc.

## Usage

``` r
detect_estimation_problems(
  search_state,
  model_names,
  check_interval_minutes = 30
)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- model_names:

  Character vector. Model names to monitor

- check_interval_minutes:

  Numeric. Check interval (default 30)

## Value

List with updated search_state and models with issues detected

## Details

Detect Estimation Problems in Multiple Models
