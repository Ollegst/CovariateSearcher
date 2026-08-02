# Complete SCM Workflow

## Complete SCM Workflow

A full run: build the covariate table, optionally start from a model
that already carries covariates, strip it back, extend the table,
search, then test extras on the final model.

    build table -> [prepare base] -> initialize -> backward -> extend table -> forward+backward -> extras

Formula and `INIT` details are in [Covariate
Formulas](https://ollegst.github.io/CovariateSearcher/articles/covariate-formulas.md).
Interrupted runs are covered separately by
[`?continue_search`](https://ollegst.github.io/CovariateSearcher/reference/continue_search.md).

------------------------------------------------------------------------

### 1. Covariate table

``` r

library(CovariateSearcher)

data <- read.csv("data/derived/analysis.csv")
spec <- yspec::ys_load("data/spec/lookup.yml")

cov <- build_covariate_reference_table(
  data      = data,
  id        = "ID",
  time      = "TIME",
  Parameter = c("CL",           "V1"),
  Covariate = c("AGE",          "ASIAN"),
  Category  = c("con",          "cat"),
  Formula   = c("power",        "linear"),
  INIT      = c("(0, 0.5, 2)",  "0.1"),   ## optional - default 0.1 for every beta
  yaml_data = spec
)

## optional - REFERENCE is computed from the data; override it afterwards
cov$REFERENCE[cov$COVARIATE == "AGE"] <- "50"

write.csv(cov, "data/derived/covariate_search.csv", row.names = FALSE)
```

You supply `Parameter`, `Covariate`, `Category` and `Formula`.
Everything else is derived:

| column | derived from |
|----|----|
| `REFERENCE` | median of the baseline values (`con`) or the most frequent level (`cat`) |
| `LEVELS` | observed baseline levels, `;`-separated (`cat` only) |
| `TIME_DEPENDENT` | `Yes` if a subject has more than one value in the full dataset |
| `cov_to_test` | `beta_<COVARIATE>_<PARAMETER>` |

`INIT` is optional - without it every covariate beta starts at `0.1`.
Use it to bound (`"(0, 0.5, 2)"`) or fix (`"0.75 FIX"`) a beta; a
multi-theta formula takes one entry per theta, by name
(`"EMAX=0.5; EC50=10"`).

`REFERENCE` is not an argument - it is always computed from the data.
Override it on the returned table when you want a round number instead
of the observed median.

------------------------------------------------------------------------

### 2. Optional: base model that already has covariates

Skip this if your base model is structural only.

``` r

prep <- prepare_search_base_model(
  base_model_path       = "run1",
  covariate_tags        = c("beta_AGE_CL", "beta_ASIAN_V1"),
  new_model_number      = 2,
  data_file_path        = "data/derived/analysis.csv",
  covariate_search_path = "data/derived/covariate_search.csv",
  models_folder         = "models",
  idcol                 = "ID"
)

prep$model_name        # "run2"
prep$covariates_added
prep$log_file          # combined log of every covariate added
```

**It does not submit the model.** Submit it yourself and wait for it to
finish:

``` r

mod <- bbr::read_model(file.path("models", prep$model_name))
bbr::submit_model(mod, .bbi_args = list(threads = 12), .overwrite = TRUE)
```

------------------------------------------------------------------------

### 3. Initialize

``` r

ss <- initialize_covariate_search(
  base_model_path       = "run2",
  data_file_path        = "data/derived/analysis.csv",
  covariate_search_path = "data/derived/covariate_search.csv",
  models_folder         = "models",
  threads               = 12
)
```

| argument | default | note |
|----|----|----|
| `models_folder` | `"models"` | where models live and new ones are written |
| `timecol`, `idcol` | `"TIME"`, `"ID"` | dataset columns |
| `threads` | `60` | passed to bbi on submission |
| `require_base_run` | `TRUE` | base model must have a finished run with an OFV. `FALSE` only to build/inspect control streams - a search cannot run without a base OFV |
| `lookup_file` | `NULL` | decodes categorical levels in THETA names |
| `starting_model_number` | `NULL` | sets the model counter: `10` means the next model created is `run11`. Default continues from the models already discovered in `models_folder` |

The base model is validated here: parameters that will receive a
population covariate must be classifiable as normal (`P = TV*EXP(ETA)`)
or log (`P = EXP(TV + ETA)`), and a log-scale one needs its own `TV_`
line.

------------------------------------------------------------------------

### 4. Backward elimination

``` r

bw <- run_automated_scm_testing(
  search_state     = ss,
  base_model_id    = "run2",
  starting_phase   = "backward",
  full_scm         = FALSE,        # this phase only
  backward_p_value = 0.01,
  rse_threshold    = 50
)

bw$final_model
bw$final_covariates
```

`run_backward_elimination(ss, starting_model = "run2")` is the
lower-level call if you want the phase without checkpointing.

------------------------------------------------------------------------

### 5. Extend the covariate table

Build the additional covariate/parameter pairs the same way, then add
them to the search state you just got back:

``` r

cov_extra <- build_covariate_reference_table(
  data      = data,
  id        = "ID",
  time      = "TIME",
  Parameter = c("CL",     "CL",    "V1"),
  Covariate = c("SMK",    "EGFR",  "SEX"),
  Category  = c("cat",    "con",   "cat"),
  Formula   = c("linear", "power", "linear"),
  yaml_data = spec
)

write.csv(cov_extra, "data/derived/covariate_search_extra.csv", row.names = FALSE)

ss <- add_covariates_to_search(bw$search_state,
                               "data/derived/covariate_search_extra.csv")
```

[`add_covariates_to_search()`](https://ollegst.github.io/CovariateSearcher/reference/add_covariates_to_search.md)
takes the table either as an object or as a path to a `.csv`, so writing
it out first keeps a record of exactly what was added.

This appends the rows, revalidates the whole table, regenerates
`tags.yaml` and refreshes `ss$tags`. Use `bw$search_state`, not a fresh
[`initialize_covariate_search()`](https://ollegst.github.io/CovariateSearcher/reference/initialize_covariate_search.md) -
re-initializing rebuilds the database from the model files and loses
every step number and ΔOFV.

A covariate/parameter pair already in the table is rejected as a
duplicate.

------------------------------------------------------------------------

### 6. Forward, then backward

``` r

res <- run_automated_scm_testing(
  search_state     = ss,
  base_model_id    = bw$final_model,  # model the search starts from
  scm_type         = "selective",     # "standard" or "selective"
  starting_phase   = "forward",       # "forward" or "backward"
  full_scm         = TRUE,            # TRUE = both phases, FALSE = starting_phase only
  forward_p_value  = 0.05,            # entry criterion
  backward_p_value = 0.01,            # retention criterion
  rse_threshold    = 50,              # max RSE (%)
  require_cov_step = TRUE,            # require a successful $COVARIANCE step
  auto_submit      = TRUE,            # FALSE = create the models, submit them yourself
  auto_retry       = TRUE,            # retry a failed model from a different init
  save_checkpoints = TRUE             # write scm_rds/NN_phase_event.rds each step
)

res$final_model
res$final_covariates
res$excluded_covariates
view_comprehensive_table(res$search_state)
```

| argument | default | effect |
|----|----|----|
| `search_state` | — | required |
| `base_model_id` | `NULL` | model to start from; `NULL` uses `search_state$base_model` |
| `scm_type` | `"standard"` | `"standard"` tests every remaining covariate each step; `"selective"` carries forward only those significant in step 1 |
| `starting_phase` | `"forward"` | which phase runs first |
| `full_scm` | `TRUE` | `TRUE` runs both phases (`backward` start gives backward → forward → backward); `FALSE` runs only `starting_phase` |
| `forward_p_value` | `NULL` | entry p-value; `NULL` uses the search config (0.05) |
| `backward_p_value` | `NULL` | retention p-value; `NULL` uses the search config (0.01) |
| `rse_threshold` | `NULL` | maximum RSE %; `NULL` uses the search config (50) |
| `require_cov_step` | `TRUE` | a model counts as completed only with a successful `$COVARIANCE` step |
| `auto_submit` | `TRUE` | `FALSE` creates the models without submitting them |
| `auto_retry` | `TRUE` | on an estimation failure, create `run<N>001` from a different initial estimate and submit it |
| `save_checkpoints` | `TRUE` | write `<models_folder>/scm_rds/NN_phase_event.rds` after each step |

The p-value and RSE arguments fall back to `search_state$search_config`,
so set them once at initialization or per call here.

------------------------------------------------------------------------

### 7. Test extra covariates on the final model

This is usually a new session, so load the state back first. Checkpoints
are written to `<models_folder>/scm_rds/` as `NN_phase_event.rds`: `NN`
is the step, `phase` is `forward`, `backward`, `redemption` or `final`,
and `event` is `created`, `done`, `complete`, `running` or `error`. The
step number leads and is zero-padded, so the folder sorts
chronologically and the last name is the most recent:

``` r

list.files("models/scm_rds")
#> "01_forward_created.rds"   "01_forward_done.rds"
#> "02_forward_created.rds"   "02_forward_done.rds"
#> "05_forward_complete.rds"  "06_backward_created.rds"
#> "06_backward_done.rds"     "06_backward_complete.rds"
#> "06_final_complete.rds"

ss <- load_search_state("models/scm_rds/06_final_complete.rds")
ss <- update_all_model_statuses(ss)

data <- read.csv("data/derived/analysis.csv")    # section 1; needed below
spec <- yspec::ys_load("data/spec/lookup.yml")
```

[`load_search_state()`](https://ollegst.github.io/CovariateSearcher/reference/load_search_state.md)
needs the full path;
[`save_search_state()`](https://ollegst.github.io/CovariateSearcher/reference/save_search_state.md)
accepts a bare file name and writes it to `<models_folder>/scm_rds/`.

A checkpoint holds the search state and nothing else, so the list
[`run_automated_scm_testing()`](https://ollegst.github.io/CovariateSearcher/reference/run_automated_scm_testing.md)
returned is gone - `final_model` with it. Take the name from the
search’s own output:

    🎯 Final model: run42

or reprint it from the database with `print_scm_results_table(ss)`.
There is no `ss$final_model`: the state carries the database, not the
search’s verdict.

Then the same two calls, pointed at that model:

``` r

cov_smk <- build_covariate_reference_table(
  data = data, id = "ID", time = "TIME",
  Parameter = "V1", Covariate = "SMK", Category = "cat", Formula = "linear",
  yaml_data = spec
)

write.csv(cov_smk, "data/derived/covariate_search_smk.csv", row.names = FALSE)

ss <- add_covariates_to_search(ss, "data/derived/covariate_search_smk.csv")

add <- add_covariate_to_model(
  ss,
  base_model_id = "run42",          # the model the search finished on
  covariate_tag = "beta_SMK_V1",
  step_number   = max(ss$search_database$step_number, na.rm = TRUE) + 1,
  phase         = "individual_testing"
)

ss <- add$search_state
stopifnot(add$status == "success")

bbr::submit_model(bbr::read_model(file.path(ss$models_folder, add$model_name)),
                  .bbi_args = list(threads = 12), .overwrite = TRUE)
```

[`add_covariate_to_model()`](https://ollegst.github.io/CovariateSearcher/reference/add_covariate_to_model.md)
returns the call’s result, not a state - the state is one element inside
it, at `add$search_state`, which is why the assignment is a separate
line. On a failure it returns `status = "error"` and leaves the state
untouched, so without the check the submit line fails on a `NULL` model
name rather than on the real reason.

#### Step numbers

Every model in the database carries a `step_number`. The search assigns
one per step, so all models tested against the same parent share it. A
manual add takes whatever you pass - `max(...) + 1` above puts the extra
tests in a step of their own, after the search.

``` r

table(ss$search_database$step_number, ss$search_database$phase)

## models belonging to one step
subset(ss$search_database, step_number == 7,
       c("model_name", "covariate_tested", "status", "ofv", "delta_ofv"))
```

#### Results

Statuses and OFVs come from the NONMEM output files, so refresh them
once the runs finish:

``` r

ss <- update_all_model_statuses(ss)

view_comprehensive_table(ss)                 # printed table
tab <- create_comprehensive_table(ss)        # same thing as a data frame
```

The table has one row per model: `model_name`, parent, `step` (`Base`,
`Step 3`, `Step 3 (Retry)`), `changes` (the covariate added or removed),
`status`, OFV, ΔOFV and the parameter count.

For the ΔOFV-versus-threshold view of a search:

``` r

print_scm_results_table(ss)
```

`phase = "individual_testing"` marks the row as a one-off test rather
than a step of the search.

------------------------------------------------------------------------

### See also

- [Quick
  Start](https://ollegst.github.io/CovariateSearcher/quick-start.md)
- [Covariate
  Formulas](https://ollegst.github.io/CovariateSearcher/articles/covariate-formulas.md) -
  FORMULA vocabulary, `INIT`, degrees of freedom
- [`?continue_search`](https://ollegst.github.io/CovariateSearcher/reference/continue_search.md) -
  resuming an interrupted search
