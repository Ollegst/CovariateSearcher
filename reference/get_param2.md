# Get Model Parameters and Statistics

Internal function to extract and format model parameters with estimates
and statistics

## Usage

``` r
get_param2(
  model_number,
  count_model,
  shrinkage = "etasd",
  models_folder = "models",
  spec_pk = NULL,
  lookup = NULL
)
```

## Arguments

- model_number:

  Character string. Model name/number

- count_model:

  Integer. Number of models being compared

- shrinkage:

  Character string. Type of shrinkage to report ("etasd", "etavr",
  "ebvsd", "ebvvr")

- models_folder:

  Character string. Path to models folder

- spec_pk:

  List. Optional parameter specifications with labels and units

- lookup:

  List. Optional covariate lookup spec (each entry carrying `values` and
  `decode`) used to decode categorical covariate levels in labels

## Value

A formatted data frame with model parameters and statistics
