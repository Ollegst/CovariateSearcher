# Reconstruct one SCM step's database rows from the model files on disk

Fallback for resuming a search whose interrupted step is on disk but
missing from every saved checkpoint. It finds the model directories not
yet in the database and, from the files alone, works out:

- **base_model** — the step's parent, from each model's recorded
  `based_on` (all candidates must share one parent);

- **direction** — "forward" if the models *add* a covariate versus the
  base, "backward" if they *remove* one (all must agree);

- **step_number** — `max(step) + 1`, the number the live search would
  assign next.

It inserts a database row per model (same `phase`/`action`/
`covariate_tested` a live run writes), resets `model_counter`, and reads
each model's results from its files (filling `ofv`/`status`/
`delta_ofv`).
[`continue_search`](https://ollegst.github.io/CovariateSearcher/reference/continue_search.md)
calls this automatically when it detects such a gap, so you rarely call
it directly.

Each detection can be overridden with the matching argument. If the
candidates disagree (more than one parent, or a mix of add and remove)
it stops and asks you to be explicit rather than guess. A
cluster/technical failure still can't be detected from files, so you
must rerun any dropped model before resuming.

## Usage

``` r
reconstruct_step_from_disk(
  search_state = NULL,
  step_number = NULL,
  base_model = NULL,
  direction = NULL,
  checkpoint = NULL,
  models_folder = NULL
)
```

## Arguments

- search_state:

  List or NULL. State to augment (takes precedence); if NULL,
  `checkpoint` is loaded.

- step_number:

  Integer or NULL. Override the auto-detected step number
  (`max(step) + 1`).

- base_model:

  Character or NULL. Override the auto-detected base (the candidates'
  common `based_on` parent). Must be in the database.

- direction:

  "forward"/"backward" or NULL. Override the auto-detected direction.

- checkpoint:

  Character or NULL. Path to a checkpoint `.rds` to load when
  `search_state` is NULL (e.g. `models/scm_rds/03_forward_done.rds`).

- models_folder:

  Character or NULL. Override the models folder; defaults to the loaded
  state's `models_folder`.

## Value

The updated `search_state`, with the step's rows registered and their
results read from files. Models that do not read back as `"completed"`
are reported so you can rerun them before resuming.

## Details

Re-register an SCM step's models from disk (auto-detected)

## See also

[`continue_search`](https://ollegst.github.io/CovariateSearcher/reference/continue_search.md)
