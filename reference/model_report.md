# Generate Parameter Table Report for Multiple Models

Generate Parameter Table Report for Multiple Models

## Usage

``` r
model_report(
  model_names,
  shrinkage = "etasd",
  models_folder = "models",
  spec_pk = NULL,
  lookup = NULL,
  bootstrap = FALSE,
  boot_results = NULL
)
```

## Arguments

- model_names:

  Character vector of model names

- shrinkage:

  Type of shrinkage to report ("etasd", "etavr", "ebvsd", "ebvvr")

- models_folder:

  Path to models folder

- spec_pk:

  Optional yspec object for parameter formatting

- lookup:

  Optional covariate lookup spec used to decode categorical covariate
  levels in the table labels. A `lookup.yaml`-shaped list (e.g.
  `yaml::read_yaml("data/spec/lookup.yaml")`) where each covariate entry
  carries `values` and `decode`. When `NULL` (default) or when a
  covariate/level has no usable decode, the generic "level N" label is
  used.

- bootstrap:

  Logical. Report a single model beside a non-parametric bootstrap. The
  final-model columns collapse to `Estimate` and
  `RSE (%) [Shrinkage (%)]`, an asymptotic `95% CI` column
  (`estimate ± 1.96 * SE`) is added, and the bootstrap `Median` and
  `95% CI-bootstrap` follow under their own spanning header. Requires
  exactly one model name and a `boot_results`. Default `FALSE` leaves
  the table as it was.

  The interval is built where the standard error lives and then mapped
  onto the reported scale, so it carries the units of the Estimate
  beside it. Two cases leave it empty: a parameter with no standard
  error (held at a FIX value, or an unsuccessful covariance step), and
  an OMEGA whose lower bound reaches a negative variance, which has no
  CV% - roughly, once the printed RSE passes the high twenties for a
  small OMEGA. The bound is left out rather than floored at zero, which
  would state a limit the estimation did not support.

- boot_results:

  Bootstrap summaries as a data frame, or a path to a `.csv`/`.rds`
  holding one. Rows are matched to the report on NONMEM's own parameter
  names (`THETA1`, `OMEGA(1,1)`, ...), so order does not matter and
  extra rows are reported and skipped. Needs a parameter-name column
  (`parameter_names`/`parameter`/`param`), a central-value column
  (`median`/`estimate`/`value`) and one column whose name contains "CI",
  holding the interval as `"[65.2-81.1]"` or `"[65.2, 81.1]"`. Values
  are used as given: they must already be on the scale the report shows
  (CV% for OMEGA diagonals, back-transformed THETAs).

## Value

flextable object with formatted parameter table
