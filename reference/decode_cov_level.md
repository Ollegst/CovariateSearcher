# Decode a categorical covariate level to its human-readable label

Internal helper. Given a covariate name and a numeric level, returns the
decoded category name from a `lookup.yaml`-shaped list (each covariate
entry carrying `values` and `decode`). Returns `NA_character_` if no
usable decode is available, so callers can fall back to the generic
"level N" label.

## Usage

``` r
decode_cov_level(cov_name, level, lookup)
```

## Arguments

- cov_name:

  Character string. Covariate name (e.g. "SEXN").

- level:

  Character/numeric. The level value as parsed from the THETA name.

- lookup:

  List or NULL. Lookup spec keyed by covariate name.

## Value

Decoded category name, or `NA_character_` when not decodable.
