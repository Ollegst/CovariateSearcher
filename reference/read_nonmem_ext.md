# Extract OFV and parameters from NONMEM .ext file

Reads NONMEM .ext file to extract parameter estimates and OFV. Handles
multiple estimation methods by using the last -1000000000 line.

## Usage

``` r
read_nonmem_ext(model_path)
```

## Arguments

- model_path:

  Character. Path to model directory or ext file

## Value

List with OFV, parameters, and metadata

## Details

Read NONMEM EXT File
