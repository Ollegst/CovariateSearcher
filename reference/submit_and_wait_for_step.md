# Submit models and wait for all to complete with status tracking

Submits a step's models, monitors them to completion, and creates retry
models for any that hit estimation issues

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
