# Execute full automated stepwise covariate modeling workflow from scratch

Runs complete end-to-end SCM testing starting from base model through
forward selection and final model validation. Includes automatic retry,
error recovery, progress monitoring, and comprehensive reporting.
Designed for hands-off execution with intelligent decision making.

## Usage

``` r
run_automated_scm_testing(
  search_state,
  base_model_id = NULL,
  scm_type = c("standard", "selective"),
  starting_phase = c("forward", "backward"),
  full_scm = TRUE,
  forward_p_value = NULL,
  backward_p_value = NULL,
  rse_threshold = NULL,
  require_cov_step = TRUE,
  auto_submit = TRUE,
  auto_retry = TRUE,
  save_checkpoints = TRUE,
  final_testing = TRUE
)
```

## Arguments

- search_state:

  List containing covariate search state and configuration

- base_model_id:

  Character. Starting base model name

- scm_type:

  Character. Type of SCM algorithm to use:

  - "standard" - Traditional SCM testing all covariates each step

  - "selective" - Selective SCM testing only significant model
    covariates

- starting_phase:

  Character. Which phase to start with:

  - "forward" - Start with forward selection (SCM)

  - "backward" - Start with backward elimination (SCM+)

- full_scm:

  Logical. Whether to run complete SCM workflow. If TRUE: Always runs
  Forward selection → Backward elimination (regardless of
  starting_phase) If FALSE: Runs only the specified starting_phase Note:
  True SCM should always include both forward and backward phases
  (default: TRUE)

- forward_p_value:

  Numeric. P-value for forward selection significance testing. If NULL,
  uses search_state\$search_config\$forward_p_value (default: 0.05)

- backward_p_value:

  Numeric. P-value for backward elimination significance testing. If
  NULL, uses search_state\$search_config\$backward_p_value (default:
  0.001, more stringent)

- rse_threshold:

  Numeric. Maximum RSE threshold as percentage. If NULL, uses
  search_state\$search_config\$max_rse_threshold (default: 50)

- require_cov_step:

  Logical. Whether to require a successful covariance step (presence of
  .cov file) for a model to be considered completed (default: TRUE)

- auto_submit:

  Logical. Whether to automatically submit models to cluster (default:
  TRUE)

- auto_retry:

  Logical. Whether to enable automatic retry for failed models (default:
  TRUE)

- save_checkpoints:

  Logical. Whether to save state after each major step (default: TRUE)

- final_testing:

  Logical. Whether to test excluded covariates on final model (default:
  TRUE)

## Value

List containing comprehensive SCM results

## Details

Run Complete Automated SCM Testing from Base Model
