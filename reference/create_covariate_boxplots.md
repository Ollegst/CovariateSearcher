# Create Covariate Boxplots for AUC / Cmax / Cmin

Builds boxplots of an exposure metric across covariate groups.
Continuous covariates are binned into quartile groups; categorical
covariates use their (decoded) factor levels. Each box is annotated with
its median value at the median level, and (when `total_panel = TRUE`) an
extra right-most `Total` panel pools all data as a reference. Optionally
splits into side-by-side panels by a stratification column, and can also
emit a single multi-page PDF of every plot.

Covariate axis labels come straight from the yspec `spec` via
[`yspec::ys_get_short_unit()`](https://rdrr.io/pkg/yspec/man/ys_get_short_unit.html)
("Short (unit)"). Categorical covariates are expected to be decoded
already (via
[`decode_dataset()`](https://ollegst.github.io/CovariateSearcher/reference/decode_dataset.md));
if a categorical covariate still holds raw numeric codes the function
stops and tells you how to decode.

## Usage

``` r
create_covariate_boxplots(
  data,
  spec,
  con = NULL,
  cat = NULL,
  type = c("AUC", "Cmax", "Cmin"),
  drug,
  param_info,
  stratification = NULL,
  total_panel = TRUE,
  output_folder = "results/figure/simulations",
  width = NULL,
  height = NULL,
  base_size = 10,
  verbose = TRUE,
  combined_pdf = TRUE,
  prefix = NULL,
  output_format = c("emf", "png"),
  show_median = TRUE,
  show_n = TRUE,
  percent_change = FALSE,
  label_size = NULL,
  ss = TRUE
)
```

## Arguments

- data:

  One row per subject/replicate, containing columns for AUC/Cmax/Cmin
  (as named in `param_info`) and all baseline covariates listed in
  `con`/`cat`; categorical covariates should already be decoded (see
  [`decode_dataset()`](https://ollegst.github.io/CovariateSearcher/reference/decode_dataset.md)).
  Given as either a `data.frame` OR a character path to a `.csv`/`.rds`
  file to load.

- spec:

  A loaded yspec object (from
  [`yspec::ys_load()`](https://rdrr.io/pkg/yspec/man/ys_load.html)), or
  a path to the spec YAML file (loaded with
  [`yspec::ys_load()`](https://rdrr.io/pkg/yspec/man/ys_load.html));
  used for the "Short (unit)" covariate axis labels.

- con:

  character vector of continuous covariate column names.

- cat:

  character vector of categorical covariate column names.

- type:

  character vector of the metric column(s) of `data` to plot (default
  `c("AUC","Cmax","Cmin")`); any numeric column works, and each name
  must have an entry in `param_info`.

- drug:

  character, full drug name used in titles AND filenames (e.g.
  "Camizestrant").

- param_info:

  Metric labels/units, keyed by the `type` names, as EITHER a named list
  `list(label=, unit=)` per metric (e.g.
  `list(AUC = list(label="AUCss", unit="mg*hr/L"))`) OR a spec supplying
  them

  - a yspec object, a path to a spec YAML, or a spec-shaped list (each
    entry's `short` becomes the label, plus its `unit`). Every `type`
    must have an entry or the call stops.

- stratification:

  optional column name (string) in `data` to split plots into
  side-by-side panels (e.g. "COMB"). NULL = single panel.

- total_panel:

  logical; if TRUE (default) add a right-most `Total` panel pooling all
  subjects as a reference. FALSE drops it.

- output_folder:

  base output directory. Default "results/figure/simulations".
  AUC/Cmax/Cmin subfolders are created inside it as needed.

- width, height:

  optional numeric; if both supplied, overrides the automatic sizing
  logic based on number of stratification levels.

- base_size:

  base font size passed to theme_bw().

- verbose:

  logical; if TRUE (default) prints progress messages ("i/n AUC:
  processing WT ... done -\> path/to/file.emf") as each parameter type /
  covariate combination is processed.

- combined_pdf:

  logical; if TRUE (default), also saves a single multi-page PDF
  containing every plot generated in this call (one page per parameter
  type x covariate combination, in the order processed), directly under
  `output_folder`. Filename is "---...\_ss-.pdf", e.g.
  "Camizestrant-AUC-Cmax-Cmin_ss-1405.pdf", where HHMM is the current
  time.

- prefix:

  optional string prepended to every output filename (both the per-plot
  image files and the combined PDF), so different simulation runs do not
  overwrite each other in the same folder. `NULL`/`""` (default) adds
  nothing. E.g. `prefix = "run19"` gives "run19---\_ss.emf".

- output_format:

  character vector, subset of c("emf","png"); which image format(s) to
  save each per-plot figure as. Default both. `.emf` is written with
  [`devEMF::emf()`](https://rdrr.io/pkg/devEMF/man/emf.html), `.png`
  with the
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  default device. (The combined multi-page file is always a PDF.)

- show_median:

  logical; if TRUE (default) each box carries its median value in a
  small white label sitting on the median line.

- show_n:

  logical; if TRUE (default) the number of subjects in each box is
  printed underneath it as "n = ".

- percent_change:

  logical; if TRUE the median label also shows the change from the
  **first level of the same panel**, e.g. `1,234 (-10%)`. The first
  level is the reference and carries no suffix, and a panel holding a
  single box (the pooled `Total` panel) gets none either. Requires
  `show_median = TRUE` to be visible. Default FALSE.

- label_size:

  numeric or NULL. Text size of the median label, in the usual ggplot
  `size` units. `NULL` (default) derives it from `base_size`
  (`base_size/.pt * 0.65`); give a number to shrink it where the boxes
  are narrow or the panel count is high, e.g. `label_size = 1.8`.

- ss:

  logical; steady state? If TRUE (default) an "ss" suffix is added to
  the metric label on the y axis (" AUCss, ") and "\_ss" to every saved
  file name, so steady-state and single-dose runs of the same drug do
  not overwrite each other. Nothing is appended where the label or file
  stem already ends in "ss", which is common when the labels come from a
  spec holding `AUCss`. Matches the `ss` argument of
  [`plot_exposure_forest()`](https://ollegst.github.io/CovariateSearcher/reference/plot_exposure_forest.md).

## Value

A nested list:
results\[[type](https://rdrr.io/r/base/typeof.html)\]\[covariate\] = the
faceted ggplot object (also saved to disk as .emf/.png per
`output_format`). Returned invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
spec <- yspec::ys_load(here::here("data", "spec", "lookup.yml"))
flags <- yspec::pull_meta(spec, "flags")
dat  <- decode_dataset(dat, spec, c(flags$catcov))

param_info <- list(
  AUC  = list(label = "AUCss",   unit = "mg*hr/L"),
  Cmax = list(label = "Cmax_ss", unit = "ng/mL"),
  Cmin = list(label = "Cmin_ss", unit = "ng/mL")
)
results <- create_covariate_boxplots(
  data           = dat,
  spec           = spec,
  con            = c("WT", "AGE", "CRCLI"),
  cat            = c("RACEN", "SEXN", "ECOGBL"),
  type           = c("AUC", "Cmax"),
  drug           = "Camizestrant",
  param_info     = param_info,
  stratification = "COMB"
)
} # }
```
