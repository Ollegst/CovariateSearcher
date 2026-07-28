# Reconstruct one SCM step from the database

Supportive lookup used by both selective forward selection and
[`continue_search`](https://ollegst.github.io/CovariateSearcher/reference/continue_search.md).
Given a step number, it returns the models tested in that step, the base
(parent) model they were built from, which completed, which were
significant, and the step winner — all read from the search database, so
a step can be reconstructed with no in-memory run state. This is what
makes selective forward resume-safe: the "test only covariates from the
previous step's significant models" narrowing is recovered from the
database rather than a local variable.

## Usage

``` r
get_step_models(
  search_state,
  step_number,
  p_value = NULL,
  rse_threshold = NULL
)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- step_number:

  Integer. Step number to reconstruct

- p_value:

  Numeric. P-value for significance (uses forward config if NULL)

- rse_threshold:

  Numeric. RSE threshold (uses config if NULL)

## Value

List with: `exists` (logical), `step_number`, `base_model` (common
parent), `models` (all tested), `completed_models`,
`significant_models`, and `best_model` (highest-ΔOFV significant model,
or NULL).

## Details

Identify the Models Tested in an SCM Step (with their base model)
