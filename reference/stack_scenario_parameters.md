# Stack Scenario Parameter Tables into One Long, Scenario-Tagged Data Frame

Turns the named list from
[`build_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/build_scenario_parameters.md)
into a single long data frame with a `Scenario` column (an ordered
factor in scenario-definition order), ready to feed straight to
[`plot_exposure_forest()`](https://ollegst.github.io/CovariateSearcher/reference/plot_exposure_forest.md)
for a **parameter forest** - the covariate effect on a structural
parameter (CL, V, ...), with no simulation or exposure metrics. It is
the pre-simulation counterpart of the stacking
[`simulate_scenario_profiles()`](https://ollegst.github.io/CovariateSearcher/reference/simulate_scenario_profiles.md)
does internally.

When `model` is supplied, the `THETA1..THETAn` columns are renamed to
the model's parameter names (e.g. `THETA1 -> CL`) by reading its `$PK`,
so the forest axis/title read the real names. Positions that cannot be
resolved keep their `THETAn` name.

## Usage

``` r
stack_scenario_parameters(param_sets, model = NULL, models_folder = "models")
```

## Arguments

- param_sets:

  Named list from
  [`build_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/build_scenario_parameters.md)
  (each element: `ID` + structural `THETA` columns), given as either the
  list itself OR a character path to an `.rds` file to load.

- model:

  Optional model name (as for
  [`build_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/build_scenario_parameters.md));
  when supplied, THETA columns are relabelled to their `$PK` parameter
  names.

- models_folder:

  Character. Folder containing `model`. Default "models".

## Value

A long `data.frame`: `ID`, the parameter columns (named `THETA1..` or
the mapped parameter names), and an ordered-factor `Scenario` column.

## See also

[`build_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/build_scenario_parameters.md),
[`plot_exposure_forest()`](https://ollegst.github.io/CovariateSearcher/reference/plot_exposure_forest.md)

## Examples

``` r
if (FALSE) { # \dontrun{
params <- build_scenario_parameters("run19", covariate_search, thetas, data)
pf <- stack_scenario_parameters(params, model = "run19")
plot_exposure_forest(pf, metric = "CL", ss = FALSE, width = 6, height = 6)
} # }
```
