# Ensure covariate search parameters exist in model code

Verifies that each PARAMETER from covariate search can be found in the
model control stream before covariate addition is attempted.

## Usage

``` r
validate_covariate_parameter_mapping(
  covariate_search,
  model_name,
  models_folder = "models",
  covariate_tags = NULL,
  strict = TRUE,
  verbose = TRUE
)
```

## Arguments

- covariate_search:

  Data frame. Covariate search table.

- model_name:

  Character. Model name (e.g., "run1").

- models_folder:

  Character. Models directory (default: "models").

- covariate_tags:

  Character vector or NULL. Optional subset of `cov_to_test` entries to
  validate.

- strict:

  Logical. If TRUE, stop on validation failure.

- verbose:

  Logical. If TRUE, print progress messages.

## Value

Data frame with validation results for each tested mapping.

## Details

Validate Covariate-Parameter Mapping Against Model Code
