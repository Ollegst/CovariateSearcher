# Determine overall model status with detailed error reporting

Classifies a model's run status from its output files, with detailed
failure information

## Usage

``` r
get_model_status_from_files(model_path, require_cov_step = TRUE)
```

## Arguments

- model_path:

  Character. Path to model directory

- require_cov_step:

  Logical. Whether a successful covariance step (the presence of a
  `.cov` file) is required for success (default: TRUE).

## Value

Character. Overall model status

## Details

Get Model Status from Files
