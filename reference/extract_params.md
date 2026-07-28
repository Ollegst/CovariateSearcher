# Extract Parameters from NONMEM Control File

Internal function to extract parameter definitions from control file
blocks

## Usage

``` r
extract_params(lines, block_tag, remove_prefix = FALSE)
```

## Arguments

- lines:

  Character vector of control file lines

- block_tag:

  Character string identifying the block (e.g., "THETA", "OMEGA")

- remove_prefix:

  Logical. Remove numeric prefix from parameter names (default: FALSE)

## Value

A tibble with parameter names and transformations
