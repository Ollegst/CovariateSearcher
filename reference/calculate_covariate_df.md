# Determine degrees of freedom for a covariate's likelihood-ratio test

Degrees of freedom = the number of ESTIMATED (non-FIX) parameters the
covariate adds:

- per-level categorical (cat.linear): number of levels - 1;

- single-factor forms (continuous power/linear/exponential, cat.power,
  and user expressions): the number of non-FIX thetas the formula
  declares (1 for the built-ins; N for an N-parameter expression such as
  `EMAX*cov/(EC50+cov)`), minus any marked `FIX` in `INIT`.

A fully-FIX covariate has df 0 (it adds no estimated parameter): it is
then selected by DIRECT OFV comparison — `pvalue_to_threshold(df = 0)`
returns a threshold of 0, so it is kept whenever the model improves (any
ΔOFV \> 0).

## Usage

``` r
calculate_covariate_df(covariate_name, covariate_search)
```

## Arguments

- covariate_name:

  Character. Name of the covariate

- covariate_search:

  Data frame. Covariate search configuration

## Value

Integer. Degrees of freedom (0 for a fully-FIX covariate).

## Details

Calculate Degrees of Freedom for Covariate
