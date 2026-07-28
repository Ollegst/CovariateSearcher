# Calculate Condition Number from NONMEM .cor/.cov File

Internal function to compute condition number from NONMEM
correlation/covariance matrix output. Prefers .cor, falls back to .cov.

## Usage

``` r
calculate_condition_number(
  model_number,
  models_folder = "models",
  tolerance = 1e-10
)
```

## Arguments

- model_number:

  Character string. Model name/number

- models_folder:

  Character string. Path to models folder

- tolerance:

  Numeric tolerance for near-zero/non-positive eigenvalues

## Value

Numeric condition number, Inf for near-singular matrices, or NA if
computation is not possible
