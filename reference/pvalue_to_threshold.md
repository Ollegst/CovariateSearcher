# Calculate ΔOFV threshold from p-value for likelihood ratio test

Converts a p-value to the corresponding chi-square ΔOFV threshold for
model comparison in stepwise covariate modeling. ΔOFV follows a
chi-square distribution with degrees of freedom equal to the difference
in parameters.

For covariate modeling:

- Continuous covariates: df = 1 (one parameter added)

- Categorical covariates: df = number of levels - 1

## Usage

``` r
pvalue_to_threshold(p_value, df = 1)
```

## Arguments

- p_value:

  Numeric. Significance level (e.g., 0.05, 0.01)

- df:

  Integer. Degrees of freedom for chi-square test (default: 1). `df = 0`
  (a fully-FIX covariate that adds no estimated parameter) returns a
  threshold of 0, so the covariate is selected by direct OFV comparison
  (kept if the model improves, i.e. any ΔOFV \> 0).

## Value

Numeric. Chi-square critical value (ΔOFV threshold); 0 when df = 0.

## Details

Convert P-Value to Chi-Square ΔOFV Threshold

## Examples

``` r
# Standard forward selection (p = 0.05, df = 1)
pvalue_to_threshold(0.05, df = 1)  # Returns 3.84
#> [1] 3.841459

# Standard backward elimination (p = 0.01, df = 1)
pvalue_to_threshold(0.01, df = 1)  # Returns 6.63
#> [1] 6.634897

# Categorical covariate with 3 levels (df = 2)
pvalue_to_threshold(0.05, df = 2)  # Returns 5.99
#> [1] 5.991465
```
