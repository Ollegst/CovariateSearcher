# Quick Start Guide

## CovariateSearcher Quick Start

A minimal end-to-end walkthrough: prepare three inputs, run a stepwise
covariate search, read the results, resume if it stops.

For the full reference see the [online
documentation](https://ollegst.github.io/CovariateSearcher/).

------------------------------------------------------------------------

### What you need

#### 1. A base model

A NONMEM control stream whose parameters are named and whose typical
values are written on their own `TV_` lines.

    $THETA
    ; Simple names in $THETA - no TV_ prefix here
    0.5 ; CL ; L/h ; RATIO
    10  ; V  ; L   ; RATIO

    $PK
    ; TV_ prefix HERE, in $PK
    TV_CL = THETA(1)
    TV_V  = THETA(2)

    CL = TV_CL * EXP(ETA(1))     ; normal scale
    V  = EXP(TV_V + ETA(2))      ; log scale - both are fine

    $OMEGA BLOCK(2)
    0.1 ; IIV_CL   ; ; LOG
    0.1 ; IIV_CL_V ; ; RATIO
    0.1 ; IIV_V    ; ; LOG

    $SIGMA
    0.1 ; RUV_PROP ; ; LOG

`$THETA` line format is `value ; NAME ; units ; RATIO|LOG`, and
`$OMEGA BLOCK` needs **one value per line**:

    # Wrong                          # Right
    $OMEGA BLOCK(2)                  $OMEGA BLOCK(2)
    0.1     ; IIV_CL                 0.1 ; IIV_CL   ; ; LOG
    0.1 0.1 ; IIV_V                  0.1 ; IIV_CL_V ; ; RATIO
                                     0.1 ; IIV_V    ; ; LOG

**The `TV_` line is not cosmetic.** A time-constant covariate goes on
the typical value, and on a log-scale parameter it has to be *added
inside* the `EXP()`. The package checks this at initialization and
stops, with a message telling you how to rewrite, if a parameter you are
testing covariates on is written as:

| Rejected | Why |
|----|----|
| `V = EXP(THETA(2) + ETA(2))` | no separate `TV_V` line, so the covariate term would land outside the `EXP` |
| `TV_V = EXP(THETA(2))` with `V = TV_V*EXP(ETA(2))` | log scale disguised as normal - the covariate would be placed multiplicatively on a log value |
| `V = EXP(TV_V + EXP(ETA(2))` | unbalanced parentheses - the transform cannot be detected |

Only parameters that actually receive a covariate are checked.

#### 2. An analysis dataset

A standard NONMEM dataset (CSV) containing every covariate you want to
test.

#### 3. A covariate search table

Rather than writing this by hand, build it from your data - references,
levels and time-dependence are then computed for you:

``` r

library(CovariateSearcher)

spec <- yspec::ys_load("data/spec/lookup.yml")   # or a plain parsed YAML list
data <- read.csv("data/derived/analysis.csv")

cov_tbl <- build_covariate_reference_table(
  data      = data,
  id        = "ID",
  time      = "TIME",
  Parameter = c("CL",  "CL",     "V"),
  Covariate = c("WT",  "SEX",    "WT"),
  Category  = c("con", "cat",    "con"),
  Formula   = c("power", "linear", "linear"),
  yaml_data = spec
)

write.csv(cov_tbl, "data/derived/covariate_search.csv", row.names = FALSE)
```

The result is one row per parameter-covariate pair:

| PARAMETER | COVARIATE | STATUS | FORMULA | LEVELS | REFERENCE | TIME_DEPENDENT |
|-----------|-----------|--------|---------|--------|-----------|----------------|
| CL        | WT        | con    | power   | NA     | 71.7      | No             |
| CL        | SEX       | cat    | linear  | 0;1    | 1         | No             |
| V         | WT        | con    | linear  | NA     | 71.7      | No             |

| Column | Meaning |
|----|----|
| `PARAMETER` | model parameter the covariate acts on (`CL`, `V`, `KA`, …) |
| `COVARIATE` | column name in the dataset |
| `STATUS` | **exactly `con` or `cat`** - continuous or categorical |
| `FORMULA` | continuous: `linear`, `power`, `exponential`; categorical: `linear` (one theta per non-reference level) or `power` (covariates whose levels are numeric, e.g. dose); or a custom expression |
| `LEVELS` | `;`-separated **observed level values** for categorical covariates (`0;1`), `NA` for continuous |
| `REFERENCE` | value the effect is normalised to: median (continuous) or most frequent level (categorical) |
| `TIME_DEPENDENT` | `Yes`/`No`, computed from the data; decides whether the effect is placed at individual or population level |
| `INIT` | *optional* initial `$THETA` value - see below |

`cov_to_test` (`beta_WT_CL`, …) is generated for you; you never write
it.

> **`STATUS` must be `con` or `cat`.** Writing `continuous` does **not**
> raise an error - the value is not recognised, so `FORMULA` is then
> treated as a custom expression and you silently get a wrong model. Use
> [`build_covariate_reference_table()`](https://ollegst.github.io/CovariateSearcher/reference/build_covariate_reference_table.md)
> and this cannot happen: it rejects anything else up front.

Custom formulas are written as an expression in the reserved names `cov`
(the covariate) and `ref` (its REFERENCE); every other symbol becomes an
estimated THETA:

``` r

Formula = "EMAX*cov/(EC50+cov)"     # two thetas: EMAX, EC50
```

The optional `INIT` column sets the initial estimate of the covariate
THETA - a plain value, a bounded triple, `FIX`, or one entry per theta
for a multi-parameter expression:

``` r

INIT = c("0.1", "(0, 0.5, 2)", "0.75 FIX", "EMAX=0.5; EC50=10")
```

The [Covariate
Formulas](https://ollegst.github.io/CovariateSearcher/articles/articles/covariate-formulas.md)
article covers all of this in full: how each form is written into the
control stream, categorical per-level effects, placement on log- and
normal-scale parameters, degrees of freedom, and registering your own
formula.

------------------------------------------------------------------------

### Run a search

#### Initialize

``` r

ss <- initialize_covariate_search(
  base_model_path       = "run1",
  data_file_path        = "data/derived/analysis.csv",
  covariate_search_path = "data/derived/covariate_search.csv",
  models_folder         = "models"
)
```

#### Forward then backward (standard SCM)

``` r

results <- run_automated_scm_testing(
  search_state   = ss,
  base_model_id  = "run1",
  scm_type       = "standard",   # test every remaining covariate each step
  starting_phase = "forward",
  full_scm       = TRUE          # forward -> backward
)
```

#### One phase only

`full_scm = FALSE` runs just the phase you name:

``` r

# Forward selection only
results <- run_automated_scm_testing(ss, "run1",
                                     starting_phase = "forward",
                                     full_scm = FALSE)

# Backward elimination only - start from a model that already has covariates
results <- run_automated_scm_testing(ss, "run20",
                                     starting_phase = "backward",
                                     full_scm = FALSE)
```

With `full_scm = TRUE` and `starting_phase = "backward"` you get the
full SCM+ sequence: backward, then forward, then backward again.

#### Faster: selective SCM

`scm_type = "selective"` carries only the covariates that were
significant in step 1 into later steps, instead of retesting all of
them:

``` r

results <- run_automated_scm_testing(ss, "run1", scm_type = "selective",
                                     starting_phase = "forward", full_scm = TRUE)
```

#### Thresholds

``` r

results <- run_automated_scm_testing(
  search_state     = ss,
  base_model_id    = "run1",
  forward_p_value  = 0.01,   # stricter entry
  backward_p_value = 0.001,  # stricter retention
  rse_threshold    = 30,     # max RSE (%)
  require_cov_step = TRUE    # require a successful $COVARIANCE step
)
```

------------------------------------------------------------------------

### What counts as an improvement

A model is kept when **both** hold:

1.  **ΔOFV beyond the p-value threshold.** The threshold is a chi-square
    quantile for the covariate’s degrees of freedom, so it is *not* a
    fixed 3.84. df is the number of **estimated** (non-`FIX`) thetas the
    covariate adds:

    | covariate             | df  | forward threshold at p = 0.05 |
    |-----------------------|-----|-------------------------------|
    | continuous, one theta | 1   | 3.84                          |
    | categorical, 3 levels | 2   | 5.99                          |
    | `EMAX*cov/(EC50+cov)` | 2   | 5.99                          |
    | every theta `FIX`     | 0   | kept on any ΔOFV \> 0         |

2.  **Maximum RSE below `rse_threshold`** (default 50%). In backward
    elimination an `NA` RSE is tolerated, for workflows that run without
    `$COVARIANCE`.

------------------------------------------------------------------------

### Results

``` r

results$final_model        # e.g. "run85"
results$final_covariates   # covariates in the final model
results$excluded_covariates

view_comprehensive_table(results$search_state)   # every model, step by step
```

------------------------------------------------------------------------

### Save and resume

Checkpoints are written automatically to `<models_folder>/scm_rds/` as
`NN_phase_event.rds`, numbered by step so the highest `NN` is the most
recent. You can also save at any time:

``` r

save_search_state(ss, "my_state.rds")   # bare name -> <models_folder>/scm_rds/
```

To resume an interrupted search, point
[`continue_search()`](https://ollegst.github.io/CovariateSearcher/reference/continue_search.md)
at the latest checkpoint. It works out where the search stopped and
carries on:

``` r

results <- continue_search(
  checkpoint = "models/scm_rds/05_forward_done.rds",
  scm_type   = "selective",
  full_scm   = TRUE
)
```

------------------------------------------------------------------------

### Testing an extra covariate afterwards

To try a covariate that was never in the original table, add it to the
search state - then the search’s history, step numbers and ΔOFV are all
kept. This is usually a new session, so load the state back from the
latest checkpoint.

`base_model_id` is the model the search finished on. It is printed when
the search ends (`🎯 Final model: run42`) and by
`print_scm_results_table(ss)`; the state itself has no field for it, so
name it explicitly:

``` r

spec <- yspec::ys_load("data/spec/lookup.yml")   # from the setup above
data <- read.csv("data/derived/analysis.csv")

cov_smk <- build_covariate_reference_table(
  data = data, id = "ID", time = "TIME",
  Parameter = "V", Covariate = "SMK", Category = "cat", Formula = "linear",
  yaml_data = spec
)

ss <- load_search_state("models/scm_rds/06_final_complete.rds")
ss <- add_covariates_to_search(ss, cov_smk)

add <- add_covariate_to_model(
  ss,
  base_model_id = "run42",               # the model the search finished on
  covariate_tag = "beta_SMK_V",
  step_number   = max(ss$search_database$step_number, na.rm = TRUE) + 1,
  phase         = "individual_testing"   # not part of the search itself
)

ss <- add$search_state                   # the state lives inside the result
stopifnot(add$status == "success")

bbr::submit_model(bbr::read_model(file.path(ss$models_folder, add$model_name)),
                  .bbi_args = list(threads = 12))
```

------------------------------------------------------------------------

### Common issues

**Base model rejected at initialization.** Read the message: it names
the parameter and the exact rewrite (usually a missing `TV_` line - see
the table above).

**Good models rejected on RSE.** Raise the threshold, or drop the
covariance requirement:

``` r

results <- run_automated_scm_testing(ss, "run1",
                                     rse_threshold = 100,
                                     require_cov_step = FALSE)
```

**A model failed to estimate.** The search creates a retry (`run12` -\>
`run12001`) with a different initial estimate for the covariate THETA,
and submits it when `auto_retry = TRUE`.

------------------------------------------------------------------------

### Where next

``` r

?initialize_covariate_search
?run_automated_scm_testing
?build_covariate_reference_table
?continue_search
```

The [online documentation](https://ollegst.github.io/CovariateSearcher/)
covers covariate formulas in depth, the recovery system, forest plots
and simulation, and troubleshooting.
