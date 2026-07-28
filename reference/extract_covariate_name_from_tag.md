# Parse covariate name from tag string

Extracts the covariate name from a tag in the format
"beta_COVARIATE_PARAMETER" (e.g., "beta_AGE_CL" → "AGE", "beta_WT_V" →
"WT")

## Usage

``` r
extract_covariate_name_from_tag(tag)
```

## Arguments

- tag:

  Character. Tag string in format "beta_COVARIATE_PARAMETER"

## Value

Character. Covariate name, or NA if parsing fails

## Details

Extract Covariate Name from Tag

## Examples

``` r
extract_covariate_name_from_tag("beta_AGE_CL")  # Returns "AGE"
#> [1] "AGE"
extract_covariate_name_from_tag("beta_WT_V")    # Returns "WT"
#> [1] "WT"
extract_covariate_name_from_tag("beta_SEX_CL")  # Returns "SEX"
#> [1] "SEX"
```
