# Check base model parameter formatting during SCM initialization

Called during initialize_covariate_search() to ensure base model has
properly formatted THETA, OMEGA, and SIGMA blocks for SCM operations.
This prevents issues during covariate addition/removal.

## Usage

``` r
validate_base_model_parameters(
  base_model_path,
  models_folder = NULL,
  strict = TRUE,
  check_omega_structure = TRUE,
  check_comments = TRUE
)
```

## Arguments

- base_model_path:

  Character. Path to base model directory or .ctl file

- models_folder:

  Character. Directory containing models (default: NULL)

- strict:

  Logical. If TRUE, stops on validation failure (default: TRUE)

- check_omega_structure:

  Logical. Validate OMEGA BLOCK structure (default: TRUE)

- check_comments:

  Logical. Validate comment structure (default: TRUE)

## Value

List with validation results. Stops execution if strict=TRUE and
validation fails.

## Details

Validate Base Model Parameter Structure (For Initialization)
