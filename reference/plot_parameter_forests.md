# Save a Covariate Parameter Forest for Every Structural Parameter

Convenience driver that builds and saves a covariate **parameter
forest** for each structural parameter a covariate acts on, plus one
combined PDF of them all. It stacks the scenario parameter tables
([`stack_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/stack_scenario_parameters.md),
which relabels the `THETA` columns to their `$PK` names) and forests
each parameter with
[`plot_exposure_forest()`](https://ollegst.github.io/CovariateSearcher/reference/plot_exposure_forest.md) -
no simulation or exposure metrics.

Parameters with **no covariate effect** (values identical across all
scenarios - e.g. a parameter no covariate acts on, or a non-structural
theta such as a residual-error term) are **skipped**, since their forest
would be flat. Only the remaining parameters need a
`param_info`/`pk.yaml` label.

Each parameter's forest shows only the scenarios that **move that
parameter**

- the reference ("Typical subject") plus any scenario whose value
  differs from it - so `CL`'s forest shows only the covariates that act
  on `CL`, not every covariate. The summary table (when `param_info` is
  given) is likewise **one table per parameter** (its relevant scenarios
  and the covariate columns that vary), saved as a named list.

Plots are written under `output_plot_folder/<model>/` (created if
missing): one `<model>-forest-plot-<param>.emf`/`.png` per parameter,
plus `<model>-forest-plots.pdf` containing every parameter forest (one
per page). The summary-table RDS go to `output_rds_folder/<model>/` when
supplied, else alongside the plots.

## Usage

``` r
plot_parameter_forests(
  param_sets,
  model,
  parameters = NULL,
  output_plot_folder = "results/figure",
  output_rds_folder = NULL,
  output_format = c("emf", "png"),
  combined_pdf = TRUE,
  width = 6,
  height = 6,
  outer_range = NULL,
  typical_subject = TRUE,
  param_info = NULL,
  scenario = NULL,
  reference = "Typical subject",
  percentiles = c(0.05, 0.95),
  lookup = NULL,
  spec = NULL,
  models_folder = "models",
  verbose = TRUE,
  ...
)
```

## Arguments

- param_sets:

  Named list from
  [`build_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/build_scenario_parameters.md),
  given as either the list itself OR a character path to an `.rds` file
  to load.

- model:

  Character. Model name - used to relabel THETA columns via its `$PK`,
  to name the output subfolder, and in the file names.

- parameters:

  Optional character vector selecting which parameters to plot; `NULL`
  (default) considers every structural parameter column (those with no
  covariate effect are then skipped automatically).

- output_plot_folder:

  Base output directory for the plots (per-parameter `.emf`/`.png` and
  the combined PDF); the model name is appended, so files land in
  `output_plot_folder/<model>/`. If `model` is already a folder in
  `output_plot_folder` (you passed a fully qualified path such as
  `"results/models/run19/forestplots/plots/"`), it is **not** appended
  again, so you don't get a redundant `<model>/<model>/`. Default
  `"results/figure"`.

- output_rds_folder:

  Base output directory for the summary-table RDS
  (`<model>-parameter-table-{absolute,relative}.rds`), with the same
  `<model>`-append / no-double behaviour as `output_plot_folder`. `NULL`
  (default) writes the tables alongside the plots; pass e.g.
  `"results/models/run19/forestplots/rds/"` to keep them in a separate
  folder.

- output_format:

  Character vector, subset of `c("emf","png")`; which per-parameter
  image format(s) to save. Default both. `.emf` via
  [`devEMF::emf()`](https://rdrr.io/pkg/devEMF/man/emf.html), `.png` via
  the
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  default device.

- combined_pdf:

  Logical; if `TRUE` (default) also write a single multi-page PDF of
  every parameter forest to the same folder.

- width, height:

  Numeric. Figure size in inches. Default 6 x 6.

- outer_range:

  Passed to
  [`plot_exposure_forest()`](https://ollegst.github.io/CovariateSearcher/reference/plot_exposure_forest.md).
  `NULL` (default) draws only the clinical-relevance band on parameter
  forests (the wider 0.5-2 band is dropped); pass e.g. `c(0.5, 2)` to
  add it back.

- typical_subject:

  Reference-subject subtitle. `TRUE` (default) uses the string
  [`build_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/build_scenario_parameters.md)
  attached to `param_sets`; `FALSE`/`NULL` suppresses it; a character
  string is shown verbatim (custom). Passed on to
  [`plot_exposure_forest()`](https://ollegst.github.io/CovariateSearcher/reference/plot_exposure_forest.md).

- param_info:

  Parameter labels/units for the summary-table headers - a named list
  `list(label=, unit=)` per parameter (e.g.
  `CL = list(label="CL", unit="L/h")`) OR a spec (yspec object / path /
  spec-shaped list, using each entry's `short` + `unit`). When supplied
  (with a `scenario`), two RDS are written to the output folder,
  `<model>-parameter-table-absolute.rds` and `…-relative.rds`, each a
  **named list with one table per parameter** (that parameter's relevant
  scenarios and the covariate columns that vary). **Every plotted
  parameter must have a label** or the call stops (restrict with
  `parameters=` or add it). Cell content as in the exposure tables - see
  [`plot_exposure_forest()`](https://ollegst.github.io/CovariateSearcher/reference/plot_exposure_forest.md)'s
  `metric_info`. No `param_info` -\> no table.

- scenario:

  The scenario table for the summary tables' covariate columns. Defaults
  to the table
  [`build_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/build_scenario_parameters.md)
  attaches to `param_sets`, so you normally don't pass it (only needed
  if you built `param_sets` with an older version, or want a different
  table).

- reference:

  Character. Reference scenario for the relative table's ratio
  denominator. Default "Typical subject".

- percentiles:

  Numeric length-2. Interval for the summary tables. Default
  `c(0.05, 0.95)`.

- lookup, spec:

  Covariate lookup list and/or yspec object used to decode categoricals
  and label the covariate columns of the summary tables.

- models_folder:

  Character. Folder containing `model`. Default "models".

- verbose:

  Logical; print progress. Default `TRUE`.

- ...:

  Further arguments passed to
  [`plot_exposure_forest()`](https://ollegst.github.io/CovariateSearcher/reference/plot_exposure_forest.md)
  (e.g. `x_lim`, `reference`, `ClinicalRelevanceLow`,
  `ClinicalRelevanceHigh`, `fontsize`).

## Value

Invisibly, a named list of the ggplot objects (one per parameter).

## See also

[`stack_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/stack_scenario_parameters.md),
[`plot_exposure_forest()`](https://ollegst.github.io/CovariateSearcher/reference/plot_exposure_forest.md)

## Examples

``` r
if (FALSE) { # \dontrun{
params <- build_scenario_parameters("run19", covariate_search, thetas, data)
plot_parameter_forests(params, model = "run19")
# -> results/figure/run19/run19-forest-plot-CL.emf/.png, ...,
#    results/figure/run19/run19-forest-plots.pdf
} # }
```
