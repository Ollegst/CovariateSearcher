# Read NONMEM Extended Output File

Reads and formats a NONMEM extended output (.ext) file containing
iteration history, objective function values, and parameter estimates.
Handles multiple estimation steps and different iteration types (BURN,
ITER, FINAL).

## Usage

``` r
read_ext_iterations(ext_file)
```

## Arguments

- ext_file:

  Character. Path to the .ext file

## Value

Data frame with columns:

- ITERATION - Iteration number

- Parameter columns - THETA, OMEGA, SIGMA values

- OBJ - Objective function value

- EST.NO - Estimation step number

- EST.NAME - Estimation step name/description

- TYPE - Iteration type (ITER, BURN, FINAL, SE, EIGEN, CONDNUM)

- EVALUATION - Logical, TRUE if evaluation step

Returns empty data frame if file doesn't exist or is incomplete

## Examples

``` r
if (FALSE) { # \dontrun{
# Read ext file for run123
ext_data <- read_ext_iterations("models/run123/run123.ext")

# Check final estimates
final_estimates <- ext_data[ext_data$TYPE == "FINAL", ]
} # }
```
