# Resume the backward-elimination phase from the last full step

Re-evaluates the removal models already created in the last backward
step (reconstructed from the database via `get_step_models`), picks the
winner with `evaluate_removal_impacts`, then continues normal backward
elimination from that winner. No models are re-created for the last
step, so there is no duplication.

## Usage

``` r
.scm_continue_backward(
  search_state,
  last_step,
  backward_p_value,
  rse_threshold,
  auto_submit,
  auto_retry
)
```
