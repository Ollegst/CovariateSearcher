# Generate Tags YAML with Search State Integration

Wrapper function that uses search_state if available

## Usage

``` r
update_tags_yaml(
  search_state = NULL,
  covariate_search = NULL,
  tags_yaml_path = "data/spec/tags.yaml",
  verbose = TRUE
)
```

## Arguments

- search_state:

  Optional. Search state containing covariate_search data or path

- covariate_search:

  Data frame or path to covariate search CSV (used if search_state not
  provided)

- tags_yaml_path:

  Path to tags.yaml file (default: "data/spec/tags.yaml")

- verbose:

  Logical. Print progress messages

## Value

Logical. TRUE if successful
