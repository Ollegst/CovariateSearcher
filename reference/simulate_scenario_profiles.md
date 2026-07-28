# Simulate Concentration-Time Profiles for Each Covariate Scenario

Runs each scenario's parameter draws through an mrgsolve model under a
user-defined dosing regimen, and stacks the resulting concentration-time
profiles tagged by scenario. Consumes the named list from
[`build_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/build_scenario_parameters.md):
each element is one scenario's per-sample parameters, and every sample
is simulated as an individual subject (via mrgsolve `idata`) under the
common dose (via `events`).

The model must expose the parameters it needs as `$PARAM` (e.g.
`THETA1..THETAn`); columns of the parameter tables matching those names
override them per subject. Covariate effects are already baked into the
parameters upstream by
[`apply_covariate_model()`](https://ollegst.github.io/CovariateSearcher/reference/apply_covariate_model.md),
so the mrgsolve model must be structural only (no covariate terms).

## Usage

``` r
simulate_scenario_profiles(
  param_sets,
  mod,
  dose,
  start = 0,
  end = 24,
  delta = 0.1,
  verbose = TRUE
)
```

## Arguments

- param_sets:

  Named list of data frames from
  [`build_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/build_scenario_parameters.md)
  (each: `ID` + structural `THETA` columns; the list names label the
  scenarios and are carried into a `Scenario` column of the output).
  Given as either the list itself OR a character path to an `.rds` file
  to load.

- mod:

  A loaded mrgsolve model object (from
  [`mrgsolve::mread()`](https://mrgsolve.org/docs/reference/mread.html)),
  compiled by the caller before calling this function.

- dose:

  An mrgsolve event object
  ([`mrgsolve::ev()`](https://mrgsolve.org/docs/reference/ev.html))
  describing the dosing regimen, applied to every subject, e.g.
  `mrgsolve::ev(amt = 300, cmt = 1, ii = 12, addl = 5, ss = 1)`.

- start, end, delta:

  Numeric. Observation time grid for
  [`mrgsolve::mrgsim()`](https://mrgsolve.org/docs/reference/mrgsim.html).
  Defaults `0`, `24`, `0.1`. For AUC over one steady-state interval set
  `end` to one dosing interval (`ii`).

- verbose:

  Logical. When `TRUE` (default) print a
  `[i/n] Simulating scenario: <name>` progress line as each scenario is
  simulated. Set `FALSE` to silence.

## Value

A `data.frame` of stacked profiles - one row per (scenario, sample,
time) - with a `Scenario` column (an ordered factor, in scenario
definition order), `ID` (the sample), `time`, and the model's captured
columns (e.g. `CP`, `IPRED`, `DV`), ready for metric derivation, e.g.
`group_by(Scenario, ID) %>% summarise(AUC = ..., Cmax = ..., Cmin = ...)`.

## Memory

Holds every time point for every sample in every scenario
(~`n_scenarios * Nsamples * n_times` rows). Fine for modest `Nsamples`;
at `1e5`, simulate and reduce to metrics one scenario at a time.

## See also

[`build_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/build_scenario_parameters.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mod  <- mrgsolve::mread("models/sim_model.cpp")   # loaded once, by the caller
dose <- mrgsolve::ev(amt = 300, cmt = 1, ii = 12, addl = 5, ss = 1)
profiles <- simulate_scenario_profiles(param_sets, mod, dose, end = 12)
} # }
```
