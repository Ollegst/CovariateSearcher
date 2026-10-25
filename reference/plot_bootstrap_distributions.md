# Plot Bootstrap Parameter Distributions

One histogram per parameter of a non-parametric bootstrap, each panel
carrying the final model's estimate in red and the bootstrap's 2.5th,
50th and 97.5th percentiles in blue. Reading the two together shows
whether the bootstrap agrees with the fit: a red line sitting away from
the blue median, or hard against a percentile, is the thing this plot is
for.

Panels are titled and ordered exactly as
[`model_report()`](https://ollegst.github.io/CovariateSearcher/reference/model_report.md)
names its rows, so a panel and its table row can be read against one
another.

## Usage

``` r
plot_bootstrap_distributions(
  boot_run,
  model_name,
  models_folder = "models",
  spec_pk = NULL,
  lookup = NULL,
  parameters = NULL,
  bins = 30,
  ncol = NULL,
  footnote = NULL,
  wrap_width = 22
)
```

## Arguments

- boot_run:

  A `bbi_nmboot_model` object, a table of bootstrap draws (long with
  `parameter_names`/`estimate`, or wide with one THETA/OMEGA/SIGMA
  column each), or a path to a `.csv`/`.rds` holding one.

- model_name:

  Character. The model the bootstrap was built from, whose estimates
  supply the red line and whose control stream supplies the labels.

- models_folder:

  Character. Path to the models folder (default "models").

- spec_pk:

  Optional yspec object, as passed to
  [`model_report()`](https://ollegst.github.io/CovariateSearcher/reference/model_report.md).
  Supplies the panel titles with their units.

- lookup:

  Optional covariate lookup spec, as passed to
  [`model_report()`](https://ollegst.github.io/CovariateSearcher/reference/model_report.md),
  used to decode categorical covariate levels in the panel titles.

- parameters:

  Optional character vector selecting which parameters get a panel.
  Entries are matched against both the NONMEM name (`THETA1`) and the
  panel title (`CL/F (L/h)`); an entry matching neither exactly is tried
  as a regular expression against both. `NULL` (default) plots every
  parameter that moved across the draws.

- bins:

  Integer. Histogram bins per panel (default 30).

- ncol:

  Integer. Panel columns; `NULL` (default) lets ggplot2 choose.

- footnote:

  Character or `NULL`. Free text placed under the figure, left-aligned
  against the plot edge. Several elements go on their own lines. `NULL`
  (default) adds nothing. Rendered the same way
  [`create_covariate_boxplots()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_boxplots.md)
  renders its own, so the two sit together in one report.

- wrap_width:

  Integer. A panel title longer than this is broken across two lines at
  a space, never mid-word; the first line takes as many words as fit
  within it. Default 22, sized for a facet strip rather than the table's
  wider column - raise it to pull another word onto the first line,
  lower it to push one down. `NULL` or `Inf` leaves titles on one line.

## Value

A ggplot2 object.

## Details

Draws are plotted on the scale the report shows, not the scale NONMEM
estimated on: a `;LOG` THETA is exponentiated and an OMEGA/SIGMA
diagonal becomes a CV percentage, through the same mapping
[`model_report()`](https://ollegst.github.io/CovariateSearcher/reference/model_report.md)
applies to the estimate. The `;LOG` annotations are read with the count
check
[`plot_nonmem_iterations()`](https://ollegst.github.io/CovariateSearcher/reference/plot_nonmem_iterations.md)
uses, so a record whose lines are not all annotated contributes no
rescaling and its panels stay as NONMEM wrote them.

Two kinds of parameter are left out by default. One that did not move
across the draws has no distribution to show - naming it in `parameters`
puts it back. An OMEGA off-diagonal is plotted as the covariance NONMEM
estimated, where the report's own row shows a correlation; the two
therefore do not match, and it is only drawn when `parameters` asks for
it.

## Examples

``` r
if (FALSE) { # \dontrun{
boot_run <- bbr::read_model(file.path("models", "run100-boot"))

p <- plot_bootstrap_distributions(boot_run, "run100", spec_pk = spec_pk,
                                  lookup = lookup)
print(p)

# Covariate effects only, from an already-tabulated set of draws
plot_bootstrap_distributions(bbr::bootstrap_estimates(boot_run,
                                                      format_long = TRUE),
                             "run100", spec_pk = spec_pk,
                             parameters = "~")
} # }
```
