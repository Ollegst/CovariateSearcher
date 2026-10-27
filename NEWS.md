# CovariateSearcher 0.5.0

This release comes from a full review of the package. Searches are more
reliable, validation is stricter, and some outputs change. Read "Changes you may
notice" before rerunning an existing project.

## Changes you may notice

* Categorical covariate THETAs are named by the numeric level
  (`beta_SEX_CL_2`), whether or not a lookup file is given. Reports decode the
  level ("Effect of SEX (Female) on CL"). Models named by the decoded label in
  older versions (`beta_SEX_CL_FEMALE`) are still read. The `lookup_file`
  argument of `model_add_cov()` and `add_covariate_to_model()` no longer changes
  the model.
* Validation at initialisation stops on:
  * a THETA initial value of 0 that is not FIXED, in the base model or in `INIT`;
  * `END IF` in `$PK` (NM-TRAN only accepts `ENDIF`);
  * a log-normal parameter written `P = EXP(... + ETA)` whose typical value does
    not come through `TV_<param>` (`TV_P = THETA(n)`, `MU_k = LOG(TV_P)`);
  * a bounded THETA written across two lines;
  * categorical LEVELS that differ from the levels in the data, a categorical
    covariate with fewer than two levels, or with no REFERENCE;
  * a continuous formula that uses the reference but has no REFERENCE;
  * a STATUS other than `con`/`cat`, a FORMULA that is neither built-in nor an
    expression in `cov`, or a non-numeric REFERENCE;
  * `require_cov_step` that is not TRUE/FALSE, or a non-numeric `rse_threshold`.
* `build_covariate_reference_table()` stops when a covariate has no value at
  TIME == 0.
* A missing record (`NA` or `.`) is not a covariate value: it no longer makes a
  covariate time-varying and no longer becomes a categorical level. The dataset
  is read with `.` as missing.
* A run counts as finished only when its listing has `Stop Time:`. An NM-TRAN
  error counts as failed at once, even if result files from an earlier run are
  present.
* Parameter tables show estimates with 3 significant figures (OFV 2 decimals,
  condition number 1). The condition number is NONMEM's (from the correlation
  matrix), labelled "Condition number". `model_report()` states the CV% formula
  in a footer.
* The search database's `phase` column holds `base`, `forward_selection`,
  `redemption`, `backward_elimination` or `manual`; `action` holds what was done
  (`add_covariate`, `remove_covariate`, `retry`, ...). Older databases are still
  read.

## New

* `retest_failed` in `run_automated_scm_testing()` and `continue_search()`
  (default `TRUE`): whether covariates excluded after a failed model and a
  failed retry are tested again.
* `id_col` in `create_covariate_boxplots()`: `n =` counts distinct subjects.
* `load_tags()` takes the path of the tags file.

## Search fixes

* Selective forward: redemption starts from the best model with every covariate
  not yet tested on it, is numbered after the last step created, and continues
  for as long as each step finds something significant. It no longer re-runs
  the last step's models or stops after one round.
* Excluding a covariate after a failed retry takes effect.
* The final backward runs after a forward search that changed nothing, when the
  starting model already has covariates and no initial backward ran.
* A covariate fixed in INIT is never removed in backward elimination, and
  degrees of freedom are read from the right row when a covariate is on
  several parameters.
* An error in the initial backward phase stops the search instead of continuing
  on a stale model counter. New model numbers never reuse a `run<N>` on disk.
* Removing a covariate stops if its THETA is still used elsewhere in `$PK`.
* `$THETA` blocks spread over several records, values on the record line,
  several values on one line, comment lines and `$THETAP`/`$THETAPV` priors are
  numbered correctly everywhere.
* A covariate is placed on the right scale when an IOV term or a scaled ETA
  shares the `EXP()`, and after a time-varying covariate was added to a
  log-scale parameter.

## Resume (`continue_search()`)

* A model the search created but never started is submitted
  (`auto_submit = TRUE`) or reported in `incomplete_step`; it is no longer
  counted as failed.
* Retry models on disk are registered, and a failed model without a retry gets
  one (`auto_retry = TRUE`).
* A selective search always re-enters selective forward, so redemption starts
  or continues after a resume.
* Checkpoints are written when a step's models are created and when the step is
  done; nothing is saved while waiting.

## Reports and plots

* The results summary names the removal the search made and takes the final
  model from the last step that changed it.
* Bootstrap intervals: the CI column is a column named with "CI" as a word, or a
  lower/upper pair; round-bracket intervals parse.
* Without `spec_pk`, parameter tables are still grouped into sections.
* Forest plots: fractional percentiles are labelled as written (2.5th); a
  single-line `IF` with nested parentheses is evaluated, and an `IF` condition
  that cannot be evaluated triggers a warning. Exposure tables include only the
  metrics that have a label.
* Boxplots: tied quantiles are labelled by interval, and the jittered points are
  reproducible.
* Control streams saved in Windows-1252 are read correctly and keep their
  encoding when the package writes them back.
