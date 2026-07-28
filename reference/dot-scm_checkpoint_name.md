# Standardised checkpoint filename: `NN_phase_event.rds`

Zero-padded step number first so the `scm_rds/` folder sorts
chronologically and the highest `NN` is the latest checkpoint. `phase`
is "forward"/"backward"/"redemption"/"final"; `event` is "created"
(models made, pre-submit), "done" (step evaluated), "complete"
(phase/run end), "running" (mid-step snapshot) or "error".

## Usage

``` r
.scm_checkpoint_name(step, phase, event)
```

## Arguments

- step:

  Step number (coerced to a zero-padded integer; NA/non-finite → 00).

- phase, event:

  Character labels (see description).

## Value

A filename string like `"02_forward_done.rds"`.
