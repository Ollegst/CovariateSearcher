# Resume a stepwise covariate search from the last completed step

Resumes an SCM run that stopped part-way (e.g. a model run failed and
was rerun manually). It re-reads the last step's model outputs from
disk, confirms that step is complete, determines the current best model,
and continues the search using the same machinery as a fresh run:

- **Forward** — re-enters the forward method from the current best
  model. Selective forward reconstructs the previous step's significant
  covariates from the database (via
  [`get_step_models`](https://ollegst.github.io/CovariateSearcher/reference/get_step_models.md)),
  so the selective narrowing is preserved. When `full_scm`, backward
  elimination follows.

- **Backward** — re-evaluates the last step's removal models and
  continues elimination from the winner.

If the interrupted step's models are on disk but missing from the loaded
checkpoint (the run died before that step was saved), they are
registered automatically first via
[`reconstruct_step_from_disk`](https://ollegst.github.io/CovariateSearcher/reference/reconstruct_step_from_disk.md)
— base model, direction and step number are detected from the files, so
you still only supply `scm_type`/`full_scm`. The forward method
(`scm_type`) and `full_scm` are passed as arguments — you choose them
when you launch a run, and the database does not record which forward
method was used. Thresholds default to the state's `search_config`. The
interrupted phase (forward vs backward) and the current best model are
detected from the database.

## Usage

``` r
continue_search(
  search_state = NULL,
  checkpoint = NULL,
  scm_type = NULL,
  full_scm = TRUE,
  forward_p_value = NULL,
  backward_p_value = NULL,
  rse_threshold = NULL,
  auto_submit = TRUE,
  auto_retry = TRUE
)
```

## Arguments

- search_state:

  List. In-memory search state to resume (takes precedence). Typically
  the state you just refreshed with
  [`update_model_status_from_files`](https://ollegst.github.io/CovariateSearcher/reference/update_model_status_from_files.md).

- checkpoint:

  Character or NULL. Full path to a per-step checkpoint `.rds` to load
  when `search_state` is NULL (per-step files live in
  `<models_folder>/scm_rds/`, e.g. `models/scm_rds/03_forward_done.rds`,
  `models/scm_rds/05_backward_done.rds`,
  `models/scm_rds/02_forward_created.rds`).

- scm_type:

  Character or NULL. "standard" or "selective". Required for a forward
  resume (the database does not record which forward method was used);
  ignored for a backward resume.

- full_scm:

  Logical. Whether to continue into backward elimination after forward
  selection (default TRUE). Set FALSE if the original run was
  forward-only.

- forward_p_value, backward_p_value, rse_threshold:

  Numeric or NULL. Thresholds; default to the state's `search_config`.

- auto_submit:

  Logical. Whether to submit newly created models (default TRUE).

- auto_retry:

  Logical. Whether to auto-retry failed models (default TRUE).

## Value

Invisibly, a list with `search_state`, `status` ("completed" or
"incomplete_step"), `resumed_phase`, `final_model` and
`final_covariates`.

## Details

Continue an Interrupted SCM Search
