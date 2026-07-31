# Recovery and Resuming a Search

## Recovery and Resuming a Search

Two things are described here: what the search does on its own when a
model fails, including exactly how the initial estimate is changed, and
how to pick a search back up from a checkpoint once model states on disk
have moved on.

### How Recovery Works

#### Detection Phase

A model’s status is read from its output files, never from the cluster.
Three separate checks contribute.

**The `.ext` file** is parsed for a usable objective function value. The
issue recorded on the model is one of:

| `estimation_issue` | Meaning |
|----|----|
| `infinite_ofv` | OFV is infinite |
| `nan_ofv` | OFV is `NaN` |
| `high_ofv` | `abs(OFV) > 1e10` |
| `missing_ofv` | no OFV present |
| `problematic_parameters` | some parameter is infinite, `NaN`, or `abs(value) > 1e10` |

**The `.lst` file** is scanned for status markers, matched
case-insensitively so they survive differences between NONMEM versions:

- completed: `MINIMIZATION SUCCESSFUL`, `OPTIMIZATION COMPLETED`,
  `ESTIMATION STEP WAS COMPLETED`
- hard failure: `MINIMIZATION TERMINATED`, `PROGRAM TERMINATED BY OBJ`,
  `TERMINATED DUE TO`, `FATAL ERROR`
- severe warning: `WARNING…SEVERE`, `NUMERICAL DIFFICULT`, `SINGULAR`
- runtime error: `ERROR=`, `ERROR IN`, `PROGRAM TERMINATED`, `ABORTING`

**The covariance step** is required by default. With
`require_cov_step = TRUE`, a model that otherwise completed but has no
`.cov` file is recorded as failed with
`Covariance step failed (no .cov file)`, since NONMEM writes that file
only when the covariance step succeeds. Set it to `FALSE` to accept such
models.

A model with no output directory at all is a separate case, recorded as
`NONMEM never started - no output directory`: the run never reached
NONMEM.

**A listing that exists but cannot be read** is recorded as failed, not
as a run still in flight. Listings are read tolerantly — a run that
wrote `PRDERR` output can carry bytes that are not valid text, and those
are decoded rather than refused — so this is rare. When it does happen
the model is given a terminal status on purpose: the monitoring loop
waits for every model to finish, and a file that will never become
readable would otherwise hold it open indefinitely.

#### Automatic Retry

When a failure is detected, the search marks the model `failed` and
creates a retry named after it (`run11` becomes `run11001`) with the
covariate’s THETA moved to a different initial estimate, then submits
it. The console stays blocked throughout; `auto_retry = FALSE` turns the
whole thing off, leaving failures recorded but untouched. Which move the
THETA makes is set out under [Moving the THETA initial
estimate](#moving-the-theta-initial-estimate).

Retry does not apply to backward elimination: a removal takes a
parameter out, so there is nothing to perturb. A model that never
started is resubmitted once instead.

### What a retry looks like

Console output while it happens, abridged — none of this is typed:

``` text
❌ NEW failures: run11
🔒 Locked run11 status as 'failed'
🔧 IMMEDIATE RETRY: 1 models just failed - creating retries
  run11: estimation_error

🔍 Processing run11 (issue: estimation_error)
  This is original model - creating retry with adjusted THETA
🔧 Creating retry model for run11 (issue: estimation_error)
  Original: run11 → Retry: run11001
  ✅ Retry model 'run11001' created
✅ Created 1 retry models: run11001
🚀 Submitting retry model run11001... ✓

[Step 3] ✅ Model run11001 (beta_AGE_CL) completed: OFV 1239.70 → 1234.50 (ΔOFV: 5.20)
```

The original keeps its `failed` status as the record of what happened;
the retry carries the result. If the retry fails too, the covariate is
excluded from later forward steps rather than retried again, and
[`get_excluded_covariates()`](https://ollegst.github.io/CovariateSearcher/reference/get_excluded_covariates.md)
will list it.

### A retry can win its step

A retry competes in its step like any other model, and if it comes out
best the search continues from it. Step evaluation selects on step
number, `completed` status and a non-missing ΔOFV — it does not filter
on `phase`, so a `retry` row is a candidate, and
[`create_retry_model()`](https://ollegst.github.io/CovariateSearcher/reference/create_retry_model.md)
copies the original’s `step_number` so it lands in the right step.

It is scored as a full replacement for the model it retries, not as
something bolted on afterwards. `covariate_tested` and `parent_model`
are copied too, so its ΔOFV is measured against the step’s base model
rather than against the failed run, and its significance threshold uses
the degrees of freedom of the right covariate. The only things that
differ from the model it replaces are its `phase`/`action` (`retry`) and
the initial estimate it started from.

So a chain like `run9 → run11 (failed) → run11001 (best of step 3)`
leaves `run11001` as the base that step 4 adds covariates to.

### Moving the THETA initial estimate

A covariate’s `$THETA` entry comes verbatim from the `INIT` column of
the covariate table, so it can be a bare value, a bounded triple or a
fixed value, and the move has to respect which. A sign flip is only
valid when nothing constrains the parameter.

| `INIT` as written | Form | Retry uses | Example |
|----|----|----|----|
| `0.1` | unbounded | sign flip | `0.1` → `-0.1` |
| `(0, 0.1, 3)` | bounded | midpoint of the wider side of the range | `(0, 0.1, 3)` → `(0, 1.55, 3)` |
| `(0, 0.1)` | one-sided | sign flip when it stays inside the bound, otherwise the midpoint of the bounded side | `(0, 0.1)` → `(0, 0.05)` |
| `0.75 FIX` | fixed | never touched | `0.75 FIX` → `0.75 FIX` |

The bounded rule always lands strictly inside the bounds, so a retry
cannot be born invalid — which a sign flip would be for a parameter with
a lower bound of 0.

`FIX` is read from the spec only, the text before the first `;`, so the
word `FIX` appearing in a comment cannot trigger it. When *every* THETA
belonging to the covariate is fixed there is nothing to perturb, so no
retry model is created and
[`create_retry_model()`](https://ollegst.github.io/CovariateSearcher/reference/create_retry_model.md)
returns `status = "skipped"` rather than a failure.

### Resuming a Search

A run writes a checkpoint to `<models_folder>/scm_rds/` at each step
boundary, named `NN_phase_event.rds` where `NN` is the step number, so
the highest `NN` is the most recent state. `*_created.rds` is written
once a step’s models exist but before they are submitted; `*_done.rds`
after the step was evaluated.

Resuming is one call, pointed at the newest checkpoint:

``` r

res <- continue_search(
  checkpoint = "models/scm_rds/03_forward_created.rds",
  scm_type   = "selective",   # "standard" or "selective"
  full_scm   = TRUE           # FALSE if the original run was forward-only
)

res$status             # "completed" or "incomplete_step"
res$final_model
res$final_covariates
```

You supply only `scm_type` and `full_scm`. The database does not record
which forward method was used, so that one cannot be inferred;
everything else is worked out from the state, including which phase was
interrupted, the current best model, and the thresholds (which default
to the state’s `search_config`).

Before continuing,
[`continue_search()`](https://ollegst.github.io/CovariateSearcher/reference/continue_search.md)
does three things worth knowing about.

**It re-reads the last step from disk.** Every model at the highest step
number is refreshed with `force = TRUE`, so output that appeared after
the checkpoint was written is picked up even when the checkpoint
recorded that model as `completed` or `failed`. This is what makes a
manual rerun visible to the search.

**It registers models the checkpoint never saw.** If a run died between
creating a step’s models and saving it, those models are on disk but
absent from the database.
[`reconstruct_step_from_disk()`](https://ollegst.github.io/CovariateSearcher/reference/reconstruct_step_from_disk.md)
is called automatically to add them, detecting base model, direction and
step number from the files.

**It refuses to continue from a half-finished step.** A model counts as
finished only when it has reached a final answer — `completed`, `failed`
or `estimation_error`. Anything else, whatever it is called, means the
search does not yet know how the run ended, so the call returns
`status = "incomplete_step"` and names what is pending rather than
evaluating a step on partial results.

The test is deliberately written that way round. Asking “has it
finished?” cannot miss an unfinished state, whereas listing the
unfinished ones can and did: the same state is recorded as `in_progress`
by one reader and `incomplete` by another, and a guard that listed only
the first let a step be declared complete while a model was still
estimating.

Only models the search created itself can block. One it did not create
is never re-read, so its status can never change, and waiting on it
would never end.

#### Scenario: checkpoint from the start of a step, failures rerun by hand

The common case when a cluster drops jobs: you have
`NN_phase_created.rds` from before submission, you resubmitted the
failures yourself, and their output is now on disk. The call above is
all that is needed, because the refresh picks those reruns up. If it
reports `incomplete_step` instead, one of the step’s models still has no
readable output; finish that model and call again.

#### Refreshing statuses without continuing

To inspect the state first, or to refresh a step other than the last
one, do it explicitly.
[`update_model_status_from_files()`](https://ollegst.github.io/CovariateSearcher/reference/update_model_status_from_files.md)
**returns** a modified state and changes nothing in place, so its return
value has to be assigned:

``` r

ss <- load_search_state("models/scm_rds/03_forward_created.rds")

# Models rerun by hand. force = TRUE re-reads a model even when the database
# already calls it completed or failed.
for (m in c("run24", "run27")) {
  ss <- update_model_status_from_files(ss, m, force = TRUE)
}

# Everything else not already in a terminal state
ss <- update_all_model_statuses(ss)

save_search_state(ss, "03_forward_refreshed.rds")   # bare name -> scm_rds/

# Resume from the state in memory instead of a file
res <- continue_search(search_state = ss, scm_type = "selective", full_scm = TRUE)
```

The automatic refresh inside
[`continue_search()`](https://ollegst.github.io/CovariateSearcher/reference/continue_search.md)
covers the **highest** step only. A model you reran from an earlier step
keeps its stale status unless you refresh it explicitly as above,
because
[`update_all_model_statuses()`](https://ollegst.github.io/CovariateSearcher/reference/update_all_model_statuses.md)
skips anything already in a terminal state (`completed`, `failed`,
`estimation_error`).

#### Why not `load_existing_search()`

[`load_existing_search()`](https://ollegst.github.io/CovariateSearcher/reference/load_existing_search.md)
rebuilds a state by discovering models on disk. That is what you want
when picking a project back up after its checkpoints are gone, but it is
not a resume: the checkpoint’s database is discarded, and step numbers,
phases and ΔOFV are re-derived from files. It also records every model
it finds as one this search did not create, so their results are read
but they are never resubmitted. When a checkpoint exists, prefer
[`continue_search()`](https://ollegst.github.io/CovariateSearcher/reference/continue_search.md).

### FAQ

#### Do I need to delete existing models before resuming?

No. Numbering continues from the highest existing `run<N>`, so nothing
is overwritten and no duplicates are created.

#### Will models that already ran be run again?

No. Completed models are read from the output they already have, and the
search creates models only for covariates it still has to test. Beyond
that, the search submits only models it created itself: anything already
sitting in `models/` when the search was initialised is never submitted,
whatever its run number.

#### A covariate keeps failing. How do I take it out?

Exclusions live in the database, on the `excluded_from_step` flag, not
in `search_config`. Inspect them with the exported helpers:

``` r

get_excluded_covariates(search_state)                        # covariate names
get_excluded_covariates(search_state, return_details = TRUE) # with the models
view_exclusion_status(search_state)
```

A covariate is excluded automatically when its retry also fails. To keep
one out of a search from the beginning, drop its row from the covariate
search table rather than editing the state.

#### Can I restart one phase from a specific model?

Start a separate search from that model, in its own models folder, so
that the two runs cannot interleave numbering:

``` r

search_state <- initialize_covariate_search(
  base_model_path       = "run20",               # start from run20 instead
  data_file_path        = "data/derived/data.csv",
  covariate_search_path = "data/derived/covariates.csv",
  models_folder         = "models_phase4_retry"  # separate folder
)
```
