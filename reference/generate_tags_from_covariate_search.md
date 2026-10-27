# Generate or Update tags.yaml File from Covariate Search Table

Creates or updates the tags.yaml file with covariate definitions based
on the covariate search table. Preserves existing content and only
updates the covariates section.

## Usage

``` r
generate_tags_from_covariate_search(
  covariate_search,
  tags_yaml_path = "data/spec/tags.yaml",
  verbose = TRUE
)
```

## Arguments

- covariate_search:

  A data frame, or the path to a .csv or .rds file holding it

- tags_yaml_path:

  Path to tags.yaml file (default: "data/spec/tags.yaml")

- verbose:

  Logical. Print progress messages (default: TRUE)

## Value

Logical. TRUE if successful, FALSE otherwise
