# Load covariate tags from YAML configuration

Loads the tags.yaml file containing covariate mappings

## Usage

``` r
load_tags(search_state, tags_file = file.path("data", "spec", "tags.yaml"))
```

## Arguments

- search_state:

  List containing search state

- tags_file:

  Path to the tags file (default: "data/spec/tags.yaml")

## Value

Updated search_state with tags loaded

## Details

Load Tags from YAML File
