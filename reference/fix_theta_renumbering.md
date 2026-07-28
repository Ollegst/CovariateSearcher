# Renumber THETA parameters after removing some

Adjusts THETA numbering throughout model file after removing THETAs

## Usage

``` r
fix_theta_renumbering(modelcode, theta_numbers_to_remove, log_function)
```

## Arguments

- modelcode:

  Character vector. Model file lines

- theta_numbers_to_remove:

  Numeric vector. THETA numbers that were removed

- log_function:

  Function. Logging function

## Value

Character vector. Updated model code with renumbered THETAs

## Details

Fix THETA Renumbering
