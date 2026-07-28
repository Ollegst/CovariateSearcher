# Generate Parameter Table Report for Multiple Models

Generate Parameter Table Report for Multiple Models

## Usage

``` r
model_report(
  model_names,
  shrinkage = "etasd",
  models_folder = "models",
  spec_pk = NULL,
  lookup = NULL
)
```

## Arguments

- model_names:

  Character vector of model names

- shrinkage:

  Type of shrinkage to report ("etasd", "etavr", "ebvsd", "ebvvr")

- models_folder:

  Path to models folder

- spec_pk:

  Optional yspec object for parameter formatting

- lookup:

  Optional covariate lookup spec used to decode categorical covariate
  levels in the table labels. A `lookup.yaml`-shaped list (e.g.
  `yaml::read_yaml("data/spec/lookup.yaml")`) where each covariate entry
  carries `values` and `decode`. When `NULL` (default) or when a
  covariate/level has no usable decode, the generic "level N" label is
  used.

## Value

flextable object with formatted parameter table
