# Convert covariate name back to tag

Finds the tag in search_state\$tags that corresponds to the given
covariate name (e.g., "WT_CL" → "cov_cl_wt")

## Usage

``` r
get_covariate_tag_from_name(search_state, covariate_name)
```

## Arguments

- search_state:

  List containing search state

- covariate_name:

  Character. Covariate name (e.g., "WT_CL")

## Value

Character. Covariate tag or NULL if not found

## Details

Get Covariate Tag from Name
