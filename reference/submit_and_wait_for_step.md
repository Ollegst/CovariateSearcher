# Submit models and wait for all to complete with status tracking

Submits a step's models, monitors them to completion, and creates retry
models for any that hit estimation issues.

Only models this search created are submitted. Any name in `model_names`
that the search did not create - anything already on disk when the
search was initialized - is reported and dropped, and if that leaves
nothing to submit the call returns `status = "no_models"`. Monitoring
and retry decisions cover this step's own models only, so a model from
earlier work cannot be started by a covariate step; the printed status
summary still reports on every model in the database. A model found to
have produced no output at all is submitted once more rather than sent
down the retry path, since nothing about its initial estimates is what
stopped it from running.

A model that removes a covariate is never retried. Backward elimination
takes a parameter out, so there are no initial estimates left to
perturb, and a removal that will not estimate already means the
covariate stays - the reading
[`evaluate_removal_impacts()`](https://ollegst.github.io/CovariateSearcher/reference/evaluate_removal_impacts.md)
takes from the model not completing. Such a model keeps its `"failed"`
status and is offered for removal again at the next backward step.
Retries of covariate *additions* are unaffected.

## Usage

``` r
submit_and_wait_for_step(
  search_state,
  model_names,
  step_name,
  max_wait_minutes = NULL,
  threads = NULL,
  auto_submit = TRUE,
  auto_retry = TRUE
)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- model_names:

  Character vector. Model names to submit and monitor

- step_name:

  Character. Description of current step

- max_wait_minutes:

  Optional timeout - NULL means no limit

- threads:

  Numeric. Number of threads per model (uses config if NULL)

- auto_submit:

  Logical. Whether to automatically submit models (default: TRUE)

- auto_retry:

  Logical. Whether to enable automatic retry for failed models (default:
  TRUE)

## Value

List with completion results and updated search_state

## Details

Submit Models and Wait for Completion with Auto-Updates
