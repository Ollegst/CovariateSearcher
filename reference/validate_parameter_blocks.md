# Check THETA, OMEGA, and SIGMA block formatting for SCM compatibility

Validates that parameter blocks follow the required format:

- OMEGA BLOCK: One value per line

- All blocks: Proper comment structure (value ; name ; units ;
  transform)

Required format for all parameter lines: number ; PARAM_NAME ;
[units](https://rdrr.io/r/base/units.html) ; RATIO\|LOG

Examples: \$THETA 0.5 ; CL ; L/h ; LOG 10 ; V ; L ; LOG

\$OMEGA BLOCK(3) 0.1 ; IIV_CL ; ; RATIO 0.1 ; IIV_CL_V2 ; ; RATIO 0.1 ;
IIV_V2 ; ; RATIO

\$SIGMA 0.1 ; RUV_PROP ; ; RATIO

## Usage

``` r
validate_parameter_blocks(
  model_file,
  check_omega_structure = TRUE,
  check_comments = TRUE,
  allow_empty_units = TRUE
)
```

## Arguments

- model_file:

  Character. Path to .ctl or .mod file

- check_omega_structure:

  Logical. Check OMEGA BLOCK has one value per line (default: TRUE)

- check_comments:

  Logical. Require proper comment structure (default: TRUE)

- allow_empty_units:

  Logical. Allow empty units field (default: TRUE)

## Value

List with validation results

## Details

Validate NONMEM Parameter Block Formatting
