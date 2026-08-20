# Forest Plot of a Covariate Effect on an Exposure Metric

Builds a forest plot of one exposure metric's covariate effects: each
scenario's metric distribution is normalised to the typical subject's
median and shown as a box (2.5 / 25 / 50 / 75 / 97.5 percentiles), with
a reference line at 1 and a shaded clinical-relevance band. Consumes an
already-calculated metrics table (`ID`, `AUC`, `Cmax`, `Cmin`,
`Scenario`) - e.g. a `group_by(Scenario, ID) %>% summarise(...)` result,
saved to RDS or in memory.

## Usage

``` r
plot_exposure_forest(
  data,
  metric = c("AUC", "Cmax", "Cmin"),
  ss = TRUE,
  ClinicalRelevanceLow = 0.8,
  ClinicalRelevanceHigh = 1.25,
  outer_range = c(0.5, 2),
  reference = "Typical subject",
  scenario = NULL,
  fontsize = 9,
  title = NULL,
  x_lim = NULL,
  x_breaks = NULL,
  typical_subject = TRUE,
  filename = NULL,
  output_format = c("emf", "png"),
  metric_info = NULL,
  model = NULL,
  percentiles = c(0.05, 0.95),
  lookup = NULL,
  spec = NULL,
  width,
  height
)
```

## Arguments

- data:

  A `Scenario` column and the metric column (`AUC`, `Cmax`, or `Cmin`),
  one row per sample per scenario. Given as either a `data.frame` OR a
  character path to a `.csv`/`.rds` file to load.

- metric:

  Character. The column of `data` to forest (its distribution is
  normalised to the reference scenario's median). Defaults to the
  exposure metrics `"AUC"`/`"Cmax"`/`"Cmin"`, but any numeric column
  works - e.g. a structural parameter column (`"CL"`, `"V2"`, ...) from
  [`stack_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/stack_scenario_parameters.md)
  for a parameter forest.

- ss:

  Logical. Steady state? Adds an "ss" suffix to the metric in the title
  (e.g. "AUC" vs "AUCss") **and** to the saved file stem, so a
  steady-state and a single-dose plot of the same metric do not
  overwrite each other (`AUC_forest` -\> `AUC_forest_ss.emf`). The
  suffix is not added when the stem already ends in "ss". Default
  `TRUE`.

- ClinicalRelevanceLow, ClinicalRelevanceHigh:

  Numeric. Bounds of the shaded (inner) clinical-relevance band (ratio
  scale). Defaults `0.8`, `1.25`.

- outer_range:

  Numeric length-2 vector, or `NULL`. Bounds of a second, wider shaded
  reference band (ratio scale) drawn behind the inner one. Default
  `c(0.5, 2)`; `NULL` draws only the inner clinical-relevance band.

- reference:

  Character. Scenario used as the reference (denominator of the ratio
  and highlighted colour). Default "Typical subject".

- scenario:

  Optional. The scenario table from
  [`build_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/build_scenario_parameters.md)
  (a `data.frame`, or a path to a saved `.rds`/`.csv`): its `Scenario`
  column sets the axis order and its `"typical_subject"` attribute
  supplies the subtitle - handy for exposure forests, whose metrics
  table (built with `group_by()`/`summarise()`) has lost both. May also
  be a plain character vector giving just the axis order. When `NULL`
  (default) the factor levels of `data$Scenario` are used if it is a
  factor (as
  [`simulate_scenario_profiles()`](https://ollegst.github.io/CovariateSearcher/reference/simulate_scenario_profiles.md)
  sets them), else first-appearance order.

- fontsize:

  Numeric. Base font size. Default 9.

- title:

  Character or NULL. Plot title; when NULL it is built from `metric` and
  `ss` ("Covariate effects on ss").

- x_lim:

  Numeric length-2 vector, or `NULL`. Fixed limits for the horizontal
  (ratio) axis - the change relative to the reference - e.g. `c(0, 3)`.
  `NULL` (default) auto-scales to the data (the box whiskers plus a
  small margin). The plot is drawn with `coord_flip()`, so this sets the
  visual x-axis the reader sees.

- x_breaks:

  Numeric vector, or `NULL`. Tick positions on that same ratio axis,
  e.g. `c(0.5, 0.8, 1, 1.15, 1.25)`. `NULL` (default) lets ggplot2
  choose, which lands on round numbers that need not include 1 or the
  relevance bounds. Breaks outside `x_lim` are simply not drawn.

- typical_subject:

  Controls the reference-subject subtitle under the title. `TRUE`
  (default) uses the ready-made string that
  [`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md)
  /
  [`build_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/build_scenario_parameters.md)
  attach as `attr(data, "typical_subject")` (nothing shown if that
  attribute is absent); `FALSE`/`NULL` shows no subtitle; a character
  string is shown verbatim (custom text, e.g. "Typical subject: 70 kg
  male, ECOG 0").

- filename:

  Character or NULL. Base path for saved output; any extension is
  stripped and one file per `output_format` is written as
  `<stem>.<format>`, or `<stem>_ss.<format>` when `ss = TRUE` (see
  `ss`). Missing parent folders are created. `NULL` (default) does not
  save.

- output_format:

  Character. Which format(s) to save when `filename` is given: one or
  both of `"emf"` and `"png"`. Default both. `.emf` is written with
  [`devEMF::emf`](https://rdrr.io/pkg/devEMF/man/emf.html), `.png` with
  the
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  default device.

- metric_info:

  Metric labels/units for the summary-table headers, as EITHER a named
  list `list(label=, unit=)` per metric (e.g.
  `AUC = list(label="AUCss", unit="ng*h/mL")`) OR a spec supplying
  them - a yspec object, a path to a spec YAML, or a spec-shaped list
  (each entry's `short` becomes the label, plus its `unit`). When
  supplied (with `filename` and a `scenario` data.frame), two summary
  tables covering **every** metric column of `data` are written next to
  the plot as `<model>-exposure-table-absolute.rds` and
  `…-relative.rds`. **Every metric must have a label** or the call stops
  (add it to the spec/list). Absolute cell =
  `median / [lo,hi] / geomean / (geoCV%)`; relative = `ratio [lo,hi]` vs
  the `reference` median. Covariate columns come from `scenario`
  (decoded via `lookup`/`spec`); interval at `percentiles`; 2 dp. No
  `metric_info` -\> no table.

- model:

  Optional character used only to name the summary-table files
  (`<model>-exposure-table-*.rds`).

- percentiles:

  Numeric length-2. Interval for the summary tables (same for absolute
  and relative). Default `c(0.05, 0.95)`.

- lookup, spec:

  Covariate lookup list and/or yspec object (as in
  [`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md))
  used to decode categoricals and label the covariate columns of the
  summary tables.

- width, height:

  Numeric. Output size in inches. Mandatory (no default); required
  whenever `filename` is given.

## Value

The ggplot object (invisibly saved to `filename` when supplied).

## See also

[`theme_forest()`](https://ollegst.github.io/CovariateSearcher/reference/theme_forest.md)

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- readRDS("data/derived/exposure_metrics_run19.rds")
for (m in c("AUC", "Cmax", "Cmin")) {
  plot_exposure_forest(
    raw, metric = m, ss = TRUE,
    filename = paste0("results/run19/", m, "ss_forestplot"),
    width = 6, height = 6
  )
}
} # }
```
