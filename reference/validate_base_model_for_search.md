# Validate base model readiness for covariate search

Checks that the selected base model:

- exists in the models folder

- finished successfully

- has a readable OFV result

## Usage

``` r
validate_base_model_for_search(base_model_path, models_folder = "models")
```

## Arguments

- base_model_path:

  Character. Base model name, for example `"run6"`. Should be provided
  without file extension.

- models_folder:

  Character. Path to the folder containing model files.

## Value

Logical `TRUE` if the base model is valid for search initialization.

## Details

This function should be used inside
[`initialize_covariate_search()`](https://ollegst.github.io/CovariateSearcher/reference/initialize_covariate_search.md)
before starting the actual covariate search workflow.
