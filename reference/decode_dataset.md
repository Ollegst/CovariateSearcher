# Decode Categorical Columns Using a Variable Specification

Replaces the numeric codes of one or more categorical columns with their
decoded labels, using each column's `values` -\> `decode` mapping from a
variable specification. Every decoded column becomes a `factor` whose
levels are the `decode` labels in spec order, so downstream plots and
tables show readable categories instead of raw codes.

The specification may be a loaded yspec object (from
[`yspec::ys_load()`](https://rdrr.io/pkg/yspec/man/ys_load.html)) **or**
a plain list read from a spec YAML with
[`yaml::read_yaml()`](https://yaml.r-lib.org/reference/read_yaml.html).
In both cases the fields are read with `$` (`spec[[col]]$values` /
`$decode`): on a yspec column that resolves the yspec accessor, and on a
raw list it reads the list element - so the same call works for either
input. (Note: routing the same fields through
[`as.list()`](https://rdrr.io/r/base/list.html) first does **not** work
on a yspec column - it bypasses the accessor and the `values`/`decode`
pairing shifts.)

## Usage

``` r
decode_dataset(data, yaml_file, column_names)
```

## Arguments

- data:

  A data frame containing the columns to decode.

- yaml_file:

  The variable specification: a loaded yspec object, or a raw list from
  [`yaml::read_yaml()`](https://yaml.r-lib.org/reference/read_yaml.html).
  Each entry keyed by column name is expected to carry `values` (the
  numeric codes) and `decode` (the matching labels).

- column_names:

  Character vector of column names in `data` to decode. A name whose
  spec entry lacks `values`/`decode`, or that is not a column of `data`,
  is left unchanged.

## Value

`data`, with each requested column replaced by a `factor` of its
`decode` labels (levels in spec order). Any code not present in `values`
becomes `NA`.

## Examples

``` r
if (FALSE) { # \dontrun{
spec  <- yspec::ys_load(here::here("data", "spec", "lookup.yml"))
flags <- yspec::pull_meta(spec, "flags")
dat   <- decode_dataset(dat, spec, c(flags$catcov))
} # }
```
