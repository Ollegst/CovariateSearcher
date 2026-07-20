# CovariateSearcher — Function Reference

**What this is:** an agent-facing map of *every* function in `R/`, so future sessions don't have to re-read the whole codebase. Generated from a full read of the pre-refactor code (package **v0.1.30**, 30 files in `R/`, ~120 functions). Keep it in sync as the modular reorg lands.

**Legend:** `Exp` ✓ = exported (`@export`), · = internal/documented-internal. *Side effects* flag file writes, `bbr::` calls, `<<-`, `stop()`, hardcoded paths. *Nested* = a closure defined inside another function (not top-level).

**Housekeeping:** this file is a dev/agent aid, not package content — add `^FUNCTIONS\.md$` to `.Rbuildignore` so it doesn't ship or trip `R CMD check`.

---

## Target-module map (current file → planned home)

| Current file(s) | Planned module |
|---|---|
| `CovariateSearchr.R`, `imports.R`, `utilities.R` | `utils.R` (+ package scaffolding stays) |
| `file-io.R`, `monitoring-files.R`, `plot_nonmem_iterations.R` (ext reader), `model-output-tables.R` (matrix/`$THETA` parsers) | `nonmem-io.R` |
| `initialization.R`, `database-management.R`, `model-discovery.R`, `validation.R` | `search-state.R` |
| `model-modification.R` | `model-modification.R` |
| `full_scm_search.R`, `scm-algorithm.R`, `scm-execution.R`, `scm-evaluation.R` | `scm-engine.R` |
| `scm-selective-forward.R`, `scm-backward.R` | `scm-method-*.R` (+ engine bits) |
| `recovery-detection.R`, `recovery-actions.R` | `recovery.R` |
| `reporting.R`, `scm-results.R`, `model-output-tables.R` (tables/plots) | `reporting.R` |
| (new) covariate formula catalogue | `covariate-formula.R` |
| `sample-theta-uncertainty.R`, `covariate-table.R`, `simulate-scenario-profiles.R`, `plot-exposure-forest.R` | `forest-plots.R` (**done**); `apply-covariate-model.R` + `build-scenario-parameters.R` + `covariate-boxplots.R` kept separate |

---

## Cross-cutting hazards & consolidation candidates (read before any merge)

**Duplicate implementations (merge targets — but preserve contracts):**
- **`.ext` readers ×3:** `read_nonmem_ext` (file-io.R → `found/ofv/parameters`, FINAL `-1e9` line, flat+subdir), `read_ext_file` (monitoring-files.R → `status/current_ofv/iterations/has_estimation_issues/…`, last/current line, subdir-only), `read_ext_iterations` (plot_nonmem_iterations.R → tidy data.frame, consumed positionally by the sim sampler). **Different OFV semantics + file locations — do not conflate.**
- **`.cov`/`.cor` matrix parsers ×2:** `calculate_condition_number` (model-output-tables.R, reconstructs lower-triangular) vs `.read_nonmem_matrix` (forest-plots.R, assumes labelled square).
- **`$THETA` name/trans parsers ×3:** `extract_params`/`extract_model_params` (model-output-tables.R) vs inline in `apply_covariate_model` vs inline in `create_covariate_table`.
- **Categorical-level decode ×2+:** `decode_cov_level` (model-output-tables.R, the only unit-tested fn) vs nested `decode_level` (forest-plots.R) vs the inline decode in `model_add_cov`.
- **Covariate formula algebra ×2 (highest risk):** `model_add_cov` *writes* NONMEM factors; `apply_covariate_model` *reconstructs the same math in R*. Must stay in sync. Also, `model_add_cov`'s formula block is **duplicated verbatim within itself** (~L404-408 and ~L490-494).
- **name→tag idiom** `names(tags)[tags==name]` repeated ~6× in scm-selective-forward.R + full_scm_search.R; overlaps `get_covariate_tag_from_name`.
- **"Get covariates" family (names lie about source):** `get_model_covariates_from_db` (reads **bbr files**, returns tag **values**), `get_model_covariates_from_files` (reads bbr files, returns tag **names**), `get_covariates_from_models` (DB, many models). **Do not collapse blindly.**

**Schema declared in 5 places (must all stay in sync / all get `stage`):** canonical `initialize_search_database_core` (18 cols) · self-heal `required_cols` in `validation.R` (13-col subset) · self-heal in `scm-selective-forward.R` · `model-discovery.R` rebuilds the 18-col row by hand · row-builders `add_covariate_to_model`/`remove_covariate_from_model` (`bind_rows`) + `create_retry_model` (**order-sensitive `rbind` on a blanked `db[1,]` template**).

**`action` / `phase` vocabulary (display registry must cover all):**
- `action` written: `base_model`, `add_covariate`, `remove_covariate`, `add_single_covariate`, `remove_single_covariate`, `retry`, `manual_modification`.
- `phase` written: `base`, `forward_selection`, `covariate_removal`, `backward_elimination`, `retry`, `manual`, `individual_testing`.
- Display generators (`generate_step_display`/`_changes_display`/`_step_description`) only recognize `{base_model, add_covariate, remove_covariate, retry}` → **everything else renders "Unknown: X"**.

**Confirmed bugs (fix during the relevant phase):**
- ~~`run_univariate_step` error handler `<-`~~ **FIXED 2026-07-18**: error handler used `<-` (handler-local) → thrown-error failures dropped from tracking → now `<<-` (scm-execution.R:153-154), matching the file's existing `<<-` callbacks.
- ~~`remove_covariate_from_model` action label~~ **FIXED 2026-07-18** (A1): wrote `action="remove_single_covariate"` → "Unknown" in tables (backward path was only patched by `scm-backward.R:155`); now writes `"remove_covariate"` at root so manual removal displays correctly.
- ~~Retry misclassification~~ **FIXED 2026-07-18**: `grepl("\\d{3}$")` flagged `run100`+ as retries → now `grepl("^run\\d+001$")` (was 6 sites; the 2 yaml-analysis helpers later removed in the dead-code sweep — remaining: `process_estimation_issues`, `update_model_counter`, `generate_recovery_report`, `discover_models`). Residual false positives only for normal models ending in `001` (run1001…).
- Excluded-covariate "final testing" block in `full_scm_search.R` is **dead** (filters `phase=="forward"` which is never written; also gated by `run_forward && !run_final_backward`, false for `full_scm`). → retire (owner dropped `excluded_only`).
- ~~`cat.power` read-side bug~~ **FIXED 2026-07-19 (registry, Step ③)**: `apply_covariate_model` previously gated the categorical `IF`-parse branch on `status=="cat"` alone, so `cat.power` (e.g. dose 35/70/125/150) wrongly entered it (no `IF(COV.EQ.x)` block → mis-reconstructed). Now BOTH `model_add_cov` (write) and `apply_covariate_model` (read) dispatch on the registry's `categorical` flag (`get_covariate_formula(status, formula)$categorical`): `cat.linear` → per-level `IF/ELSEIF`; everything else incl. `cat.power` → single factor from the same registry entry (`nonmem` write / `r_eval` read), so write & read can't drift.
- ~~`generate_phase`~~ **removed 2026-07-18** (dead, unexported).

**Undeclared deps (block a clean install / R CMD check):** `mvtnorm` (sample-theta), `mrgsolve` (simulate-scenario), `devEMF` (forest + boxplots); base-priority `tools`/`grid`/`grDevices` used un-declared. **5 sim `@export`s not in NAMESPACE** (`sample_individual_thetas`, `build_scenario_parameters`, `simulate_scenario_profiles`, `plot_exposure_forest`+`theme_forest`, `create_covariate_boxplots`).

**Removed on `feature/package-rebuild`:** `analyze_model_covariates_yaml`, `analyze_model_from_logic`, `get_model_parent_yaml`, `get_dropped_covariates`, `generate_phase`, `get_model_covariates`, `read_model_yaml` (dead-code sweep 2026-07-18, last two are cascade orphans); `resume_selective_forward` (2026-07-18); `data-operations.R` + its 3 exported functions.

**`<<-` scope hazards (break if extracted into functions):** `submit_and_wait_for_step` error callbacks (scm-execution.R L298, L578; success path uses `<-`) · error handlers in `full_scm_search.R` · log accumulators in model-modification/recovery.

**Hardcoded relative paths:** `data/spec/tags.yaml`, `data/spec/lookup.yaml`, `models/scm_rds/`, `scenario_table_<model>.rds` (cwd), `results/figure/simulations/`, `scm_report.txt`.

---

## Package scaffolding

### R/CovariateSearchr.R
_Package-level scaffolding: `_PACKAGE` doc, the central `@importFrom` manifest, and the master `globalVariables` block. No functions._

**Notes:** No function definitions. Holds the central `utils::globalVariables(...)` block (~60 names: DB columns + dplyr-chain vars + per-function NSE vars for `extract_params`/`get_param2`/`model_report`/`submit_and_wait_for_step`) and the central `@importFrom` manifest on `"_PACKAGE"` (dplyr, tibble, bbr, yaml, readr, purrr, stringr, stats, utils, data.table, flextable, officer, tidyr, rlang). **This block is the single most important thing to keep intact when moving NSE-heavy functions.**

### R/imports.R
_Package import shell: the `%||%` null-coalescing operator and one stray import tag._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `` %||%(x, y) `` | ✓ | Null-coalesce: `x` unless NULL, else `y` | `x`, `y` | `x` or `y` | — | used throughout |

**Notes:** Only real code is `%||%`; carries a lone `#' @importFrom stats qchisq`. `%||%` is **re-defined locally** inside `generate_scm_report` (redundant shadow).

---

## Initialization & config

### R/initialization.R
_Main entry point: builds `search_state`, loads CSV/YAML inputs, validates, auto-generates `tags.yaml`, orchestrates DB/config/model-discovery setup._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `initialize_covariate_search(base_model_path, data_file_path, covariate_search_path, models_folder="models", timecol, idcol, threads=60, validate_parameters=TRUE, lookup_file=NULL, starting_model_number=NULL)` | ✓ | Full init: load data, verify ID col + `$DATA` filename match, gen tags, validate base model, build DB/config, discover models, set counter | two CSV paths, base model name, `starting_model_number` | full `search_state` list (13 slots) | `cat`; `readr::read_csv`; `readLines`; `dir.create` `models/scm_rds/`; many `stop()`; hardcoded `data/spec/tags.yaml` | calls `generate_tags_from_covariate_search`, `validate_base_model_for_search`, `load_tags`, `validate_setup`, `initialize_search_database_core`, `initialize_search_config`, `discover_existing_models`, `update_model_counter`, `validate_base_model_parameters`; called-by `load_existing_search` |
| `load_tags(search_state)` | ✓ | Read `tags.yaml` into `$tags` | `search_state` | updated `search_state` | `yaml::read_yaml`; `stop()`; hardcoded `data/spec/tags.yaml` | called-by `initialize_covariate_search` |
| `validate_covariate_search_table(covariate_search, data_file)` | ✓ | Reusable checks: required cols, non-empty COV/PARAM, dup `cov_to_test`, covariates in data, categorical LEVELS/REFERENCE numeric & consistent | two data.frames | validated `covariate_search` (adds `cov_to_test`) | `cat`; many `stop()` | called-by `validate_setup` |
| `validate_setup(search_state)` | ✓ | High-level setup validation wrapper | `search_state` | updated `search_state` | `cat`; `stop()` | calls `validate_covariate_search_table`, `validate_covariate_parameter_mapping`; called-by `initialize_covariate_search` |
| `validate_base_model_for_search(base_model_path, models_folder="models")` | ✓ | Confirm base model exists, finished, has OFV | model name, folder | `TRUE` or `stop()` | `stop()` | calls `get_model_status_from_files`, `read_nonmem_lst`, `read_nonmem_ext` |
| `initialize_search_config(search_state, lookup_file=NULL)` | ✓ | Set default SCM config (p-values, RSE, threads, lookup, phase) | `search_state`, lookup | updated `search_state` (`$search_config`) | `cat`; hardcoded `data/spec/lookup.yaml` | called-by `initialize_covariate_search` |
| `generate_tags_from_covariate_search(covariate_search, tags_yaml_path="data/spec/tags.yaml", verbose=TRUE)` | ✓ | Build/replace `## Covariates` section of `tags.yaml`; validates continuous FORMULA vocab | df or CSV path | logical | `writeLines`/`readLines`/`dir.create`; `stop()`; default `data/spec/tags.yaml` | nested `generate_tag_entry`; called-by `initialize_covariate_search`, `update_tags_yaml` |
| `update_tags_yaml(search_state=NULL, covariate_search=NULL, tags_yaml_path="data/spec/tags.yaml", verbose=TRUE)` | ✓ | Thin wrapper choosing df/path source | either arg | logical | `stop()` | calls `generate_tags_from_covariate_search` |

**Notes:** `cov_to_test = beta_<COV>_<PARAM>` computed in two places here + in `validate_covariate_search_table` (dup). FORMULA vocab inconsistent: validation allows `c(linear,power,power1,power0.75,exponential)` but the per-entry comment builder recognizes `c(linear,power,exponential,logistic)`. No schema/`action`/`phase` writes here.

---

## Database & state

### R/database-management.R
_Search-database schema/self-heal, RDS save/load, model-lookup accessors, model counter, comprehensive display-table builders._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `load_existing_search(base_model_path, data_file_path, covariate_search_path, models_folder="models", timecol, idcol, threads=60, lookup_file=NULL)` | ✓ | Discovery-mode wrapper: re-init from existing models | CSV/model paths | `search_state` or `stop()` | `cat`; `list.files`; `stop()` | calls `initialize_covariate_search` |
| `save_search_state(search_state, filename)` | ✓ | Save RDS; bare name → `models/scm_rds/` | state, filename | `invisible(search_state)` | `saveRDS`; `dir.create`; hardcoded `scm_rds/` | — |
| `load_search_state(filename)` | ✓ | Load RDS (no migration) | filename | `search_state` | `readRDS`; `stop()` | — |
| `initialize_search_database_core(search_state)` | ✓ | Create empty 18-col search DB (**canonical schema**) | `search_state` | updated `search_state` | `cat` | called-by `initialize_covariate_search`, `discover_existing_models` |
| `get_model_status(search_state, model_name)` | ✓ | DB status lookup | state, name | char status or `"not_found"` | — | — |
| `get_model_ofv_from_database(search_state, model_name)` | ✓ | Cached OFV lookup | state, name | numeric or `NA_real_` | — | — |
| `get_model_covariates_from_db(search_state, model_name)` | ✓ | Read **bbr tags**, intersect w/ `beta_*` tag **values** | state, name | char or `character(0)` | `bbr::read_model` | called widely (forward/backward/selective) |
| `update_model_counter(search_state)` | ✓ | Set `model_counter` to last non-retry run number | `search_state` | updated `search_state` | `cat` | called-by `initialize_covariate_search` |
| `create_comprehensive_table(search_state, use_separate_columns=TRUE)` | ✓ | Build display df (parent/type/step/changes/status/ofv/delta/param) | `search_state` | `data.frame` | `stop()` | calls `generate_step_display`/`_changes_display`/`_step_description`; called-by `view_comprehensive_table` |
| `generate_step_display(step_number, action)` | · | "Base"/"Step N"/"Step N (Retry)" | vectors | char vector | — | called-by `create_comprehensive_table` |
| `view_comprehensive_table(search_state, use_separate_columns=TRUE)` | ✓ | Print comprehensive table | `search_state` | `invisible(df)` | `cat`; `print` | calls `create_comprehensive_table` |
| `generate_changes_display(action, covariate_tested)` | · | "Add/Remove/Retry X" | vectors | char vector | — | called-by `create_comprehensive_table` |
| `generate_step_description(step_number, action, covariate_tested)` | · | "Step N: Add X" | vectors | char vector | — | called-by `create_comprehensive_table` |

**Notes:** **Canonical schema (18 cols):** `model_name, step_description, phase, step_number, parent_model, covariate_tested, action, ofv, delta_ofv, rse_max, status, tags (I(list())), submission_time, completion_time (POSIXct), retry_attempt, original_model, estimation_issue, excluded_from_step`. Display generators branch on `action ∈ {base_model, add_covariate, remove_covariate, retry}` → discovered/manual/`*_single_*` rows fall through to "Unknown". No `action`/`phase` **written** here (display only).

### R/model-discovery.R
_Scans the models folder, parses bbr `based_on`/notes into DB rows, computes relative step numbers via parent-chain recursion._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `discover_existing_models(search_state)` | ✓ | Catalog `run\d+.(ctl|mod)`, read bbr per run, build `search_database`, assign relative step numbers | `search_state` | updated `search_state` (+ `$discovered_models`) | `cat`; `list.files`; `bbr::read_model`; `stop()` (missing base / circular parent) | calls `initialize_search_database_core`, `get_model_status_from_files`, `read_nonmem_ext`, `get_model_covariates_from_files`, `dplyr::bind_rows`; nested `get_step_number`, `is_descendant_of`; called-by `initialize_covariate_search` |

**Notes:** **4th row-builder** — hand-builds the 18-col row (must stay in sync w/ canonical schema). Writes `action ∈ {manual_modification(default), retry, add_single_covariate, remove_single_covariate}`, `phase ∈ {manual(default), retry, individual_testing}` — but then **overwrites** most: `phase <- ifelse(retry, phase, "manual")`, `action <- ifelse(retry, action, "manual_modification")`, so only `retry` survives; base row forced to `action="base_model"/phase="base"/step_number=0L`. Net: the note-parsing into `*_single_covariate` is discarded for non-retry models.

### R/validation.R
_Reads NONMEM output to update DB status/OFV/RSE/ΔOFV; THETA/OMEGA/SIGMA block-format validators; covariate→parameter mapping check._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `update_model_status_from_files(search_state, model_name, force=FALSE)` | ✓ | Read `.lst`/`.ext`/`.cov`, set status/ofv/rse_max/issue, compute signed ΔOFV vs parent | state, name, force | updated `search_state` | `cat`; `readLines`; **self-heals missing DB cols** | calls `extract_nonmem_timestamps`, `read_nonmem_ext`, `get_param2`; called-by `force_update_models`, `update_all_model_statuses` |
| `force_update_models(search_state, model_names)` | ✓ | Force-reread one/many | state, names | updated `search_state` | `cat` | calls `update_model_status_from_files` |
| `update_all_model_statuses(search_state, show_progress=TRUE)` | ✓ | Update all non-terminal models; report newly-significant via per-df thresholds | `search_state` | updated `search_state` | `cat` | calls `update_model_status_from_files`, `extract_covariate_name_from_tag`, `calculate_covariate_df`, `pvalue_to_threshold`; called-by `submit_and_wait_for_step`, `select_best_model` |
| `get_model_max_rse(search_state, model_name)` | ✓ | Max finite RSE via `get_param2` | state, name | numeric or `NA_real_` | — | calls `get_param2` |
| `validate_parameter_blocks(model_file, check_omega_structure=TRUE, check_comments=TRUE, allow_empty_units=TRUE)` | ✓ | Check THETA/OMEGA/SIGMA line formatting | .ctl path | `list(valid, issues, warnings, model_file, blocks_checked)` | `readLines` | called-by `validate_base_model_parameters` |
| `print_parameter_validation(validation_result, verbose=TRUE)` | ✓ | Pretty-print validation + guidance | validation list | `invisible(...)` | `cat` | called-by `validate_base_model_parameters` |
| `validate_base_model_parameters(base_model_path, models_folder=NULL, strict=TRUE, check_omega_structure=TRUE, check_comments=TRUE)` | ✓ | Resolve base model, run+print block validation | model path/folder | validation `list` or `stop()` | `cat`; `stop`/`warning`; `list.files` | calls `validate_parameter_blocks`, `print_parameter_validation`; called-by `initialize_covariate_search` |
| `validate_covariate_parameter_mapping(covariate_search, model_name, models_folder="models", covariate_tags=NULL, strict=TRUE, verbose=TRUE)` | · | Verify each PARAMETER appears in model code | search table, model | `data.frame(cov_to_test, COVARIATE, PARAMETER, parameter_found)` | `cat`; `readLines`; `stop`/`warning` | calls `find_model_file`; called-by `validate_setup` |

**Notes:** **Self-heal schema #2** — `update_model_status_from_files` declares a 13-col `required_cols` subset, adds missing via `case_when` defaults (`NA_real_` for numerics, `FALSE` for `excluded_from_step`, else `NA_character_`). **Status values:** `completed/failed/in_progress/unknown/stopped/read_error`. **ΔOFV sign:** backward (`action` matches "remove") uses `child - parent`; forward uses `parent - child` (positive=improvement both). Completion needs valid OFV AND (if `require_cov_step`) a `.cov`; boundary params (`abs >= 8.99990e5`) → failed.

---

## File I/O & model modification

### R/file-io.R
_NONMEM file I/O: locate/read/write control files; parse `.ext`/`.lst` for status, OFV, covariates, timestamps._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `read_model_file(search_state, run_name, extensions=c(".ctl",".mod"))` | ✓ | Read control file, stash path | `models_folder`+`run_name` | char lines w/ `file_path` attr | `stop()` if none | called-by `model_add_cov`, `remove_covariate_from_model` |
| `find_model_file(base_path, extensions=c(".ctl",".mod"))` | ✓ | Resolve actual model file path | `base_path` (no ext) | full path or `NULL` | — | called-by `create_model_info_log`, `adjust_theta_for_covariate`, validation, sim |
| `write_model_file(search_state, lines)` | ✓ | Write lines back to `file_path` attr | `lines` w/ attr | `search_state` | `writeLines`; `stop()` if attr missing | called-by `model_add_cov`, `remove_covariate_from_model` |
| `read_nonmem_ext(model_path)` | ✓ | OFV+params from `.ext` (last `-1e9` line) | dir or `.ext` | `list(found, file, ofv, parameters, n_parameters)` / `list(found=FALSE, error, ofv=NA, parameters=NULL)` | `warning()` | called-by `get_model_status_from_files`, validation, discovery, scm-algorithm, init |
| `read_nonmem_lst(model_path)` | ✓ | Classify `.lst` run status via regex | dir or `.lst` | `list(found, file, status, error_message, error_excerpt, has_issues)` | — | called-by `get_model_status_from_files`, init |
| `get_model_status_from_files(model_path)` | ✓ | Combine LST+EXT into one status | model dir | char status (`completed_with_issues` when EXT OFV ok but LST failed) | — | calls `read_nonmem_lst`, `read_nonmem_ext` |
| `get_model_covariates_from_files(search_state, model_name)` | ✓ | Covariate **tag names** present via bbr tags | `$tags` (`beta_*`) | char vec (names) or `character(0)` | `bbr::read_model` | called-by `discover_existing_models` |
| `extract_nonmem_timestamps(model_name, models_folder="models")` | ✓ | Parse start/stop from `.lst` | name, folder | `list(start_time, stop_time, found_start, found_stop)` | — | called-by `submit_and_wait_for_step`, validation |

**Notes:** `read_nonmem_ext` (`$parameters`/`$n_parameters` computed but read by **no** caller — dead fields). `read_model_file`'s `file_path` attr is a hard contract for `write_model_file`.

### R/model-modification.R
_Add/remove covariates by text-editing NONMEM control streams (THETA insert/renumber, FLAG-driven formulae) and appending `search_database` rows._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `add_covariate_to_model(search_state, base_model_id, covariate_tag, step_number=NULL, lookup_file=NULL)` | ✓ | Copy parent, add one covariate, append DB row | tag, base model | `list(status, model_name, covariate_added, step_number, search_state, technical_log, log_file)` / error list | `bbr::read_model/copy_model_from/add_tags/replace_all_notes`; `writeLines` logs; `log_filename <<-`; `stop()`; rolls back `model_counter` | calls `validate_covariate_parameter_mapping`, `model_add_cov`; called-by `run_univariate_step` |
| `create_model_info_log(search_state, model_name, parent_model, covariate_name, cov_info)` | ✓ | Write human `_info.txt` | `cov_info` row | `NULL` | `writeLines`, `cat` | calls `find_model_file` |
| `model_add_cov(search_state, ref_model, cov_on_param, id_var="ID", data_file, covariate_search, capture_log=FALSE, lookup_file=NULL)` | ✓ | **Core control-stream editor:** insert THETA + covariate formula | `cov_on_param`, `data_file` | `list(search_state, log_entries)` or `search_state` | `write_model_file`; `stop()`; hardcoded `data/spec/lookup.yaml`; `captured_log <<-` | calls `read_model_file`, `write_model_file`; called-by `add_covariate_to_model`, `prepare_search_base_model` |
| `fix_theta_renumbering(modelcode, theta_numbers_to_remove, log_function)` | ✓ | Renumber `THETA(n)` after removals (temp placeholders) | modelcode, removed nums | char modelcode | — | called-by `remove_covariate_from_model` |
| `prepare_search_base_model(base_model_path, covariate_tags, new_model_number, data_file_path, covariate_search_path, models_folder="models", idcol="ID", overwrite=TRUE, lookup_file=NULL)` | ✓ | Build one child w/ many covariates + combined log (no DB row) | tags, paths | `list(status, model_name, model_path, parent_model, covariate_tags, covariates_added, log_file)` | `readr::read_csv`; `bbr::*`; `writeLines`; `stop()`; hardcoded lookup | calls `validate_covariate_parameter_mapping`, `model_add_cov` |
| `remove_covariate_from_model(search_state, model_name, covariate_tag, save_as_new_model=TRUE)` | ✓ | Strip covariate formula+THETA, renumber, append DB row | tag, model | `list(status, model_name, covariate_removed, search_state)` | `bbr::copy_model_from/read_model/remove_tags/add_notes`; `write_model_file`; `writeLines`; `log_messages <<-`; `stop()` | calls `read_model_file`, `fix_theta_renumbering`, `write_model_file`; called-by `run_backward_elimination` |

**Notes — row-builders & FLAG logic (the reconciliation core):**
- **`add_covariate_to_model`** (`bind_rows`): `action="add_covariate"`, `phase="forward_selection"`, `step_number` provided or `max+1`, `parent_model=base_model_id`, `covariate_tested=cov_to_test` (`beta_*`), `status="created"`, `tags=I(list(actual .yaml tags))`, rest NA/0L/FALSE. Increments `model_counter`→`run<N>`; rolls back on error.
- **`remove_covariate_from_model`** (`bind_rows`, only if `save_as_new_model`): `action="remove_single_covariate"` (← display bug root), `phase="covariate_removal"`, `parent_model=model_name`, `tags=I(list(character(0)))` (**discards** computed `remaining_tags`).
- **`model_add_cov` FLAG** (`case_when`): `cat+linear→"1"`, `cat+power→"2"`, `con+linear→"3"`, `con+power→"2"` (**collision**), `con+power1→"5"`, `con+power0.75→"6"`, `con+exponential→"4"`. `init`: F5→`"1 FIX"`, F6→`"0.75 FIX"`, else `"0.1"`. Formulae: F2/5/6 `*(cova/ref)**THETA(n)`, F3 `*(1+(cova-ref)*THETA(n))`, F4 `*EXP(THETA(n)*(cova-ref))` — **block duplicated verbatim ~L404 & ~L490**. FLAG "1" builds `IF/ELSEIF/ENDIF` (levels by frequency, ref excluded), THETA lines named `beta_<COV>_<PARAM>_<LABEL>` (decoded label, **not** numeric level). Rewrites `$TABLE FILE=` to match run number. `remove_*`'s removal regexes must mirror these emitted formulae.

### R/monitoring-files.R
_Single-model `.ext` parser: live OFV/iteration status + estimation-issue detection._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `read_ext_file(search_state, model_name)` | ✓ | Parse last data line of final `.ext` table; detect issues | `models_folder`+`model_name` | `list(status, current_ofv, iterations, last_iteration_time, has_estimation_issues, issue_type, estimation_method)` | — | called-by `detect_estimation_problems` |

**Notes:** Expects `.ext` at `models_folder/<m>/<m>.ext` (subdir only), unlike `read_nonmem_ext` (flat+subdir). OFV = **last column of last line** (monitoring "current"); issue threshold `abs(ofv) > 1e10`; `issue_type ∈ {infinite/nan/high/missing_ofv, problematic_parameters, parse_error, read_error:*}`.

---

## Recovery

### R/recovery-detection.R
_Scan models for estimation problems (via `.ext`) and produce an aggregate recovery report._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `detect_estimation_problems(search_state, model_names, check_interval_minutes=30)` | ✓ | Check each model's `.ext`; flag DB rows | `model_names` | `list(search_state, models_with_issues)` | mutates `estimation_issue`/`status="estimation_error"`; `cat` | calls `read_ext_file` |
| `generate_recovery_report(search_state)` | ✓ | Summarize status distribution + retry stats | `search_database` | `list(timestamp, status_distribution, retry_statistics, excluded_covariates, recovery_success_rate)` | — | calls `get_excluded_covariates` |

**Notes:** `check_interval_minutes` unused (dead param). Retry detection `grepl("\\d{3}$")` misclassifies `run100` etc.

### R/recovery-actions.R
_Retry/exclusion recovery: build `run<N>001` retries with sign-flipped THETAs, route retries vs exclusions, mark failed covariates._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `create_retry_model(search_state, original_model_name, issue_type="estimation_error")` | ✓ | Copy failed model → `run<num>001`, flip THETA sign, append DB row | original name | success/fail `list(...)` | `bbr::copy_model_from/read_model/replace_all_notes`; sets `based_on`; `writeLines`; `log_entries <<-`; `stop()` | calls `adjust_theta_for_covariate`; called-by `process_estimation_issues` |
| `adjust_theta_for_covariate(search_state, model_name, covariate_tag)` | ✓ | Negate leading THETA init for a covariate's lines | tag, model | `list(success, theta_lines_modified, covariate, matched_terms)` | `writeLines` model file; `cat` | calls `find_model_file`; called-by `create_retry_model` |
| `process_estimation_issues(search_state, models_with_issues)` | ✓ | Route each issue: retry originals / exclude failed retries | `models_with_issues` | `list(search_state, retry_models_created, excluded_covariates, recovery_actions)` | `cat` | calls `create_retry_model`, `handle_failed_retry`; called-by `submit_and_wait_for_step` |
| `handle_failed_retry(search_state, retry_model_name, exclusion_reason="retry_failed")` | ✓ | Mark retry+original excluded from step | retry name | `list(search_state, status, excluded_covariate, ...)` | mutates several DB cols; `cat` | called-by `process_estimation_issues` |

**Notes:** **3rd row-builder** — `create_retry_model` uses **template `db[1,]` + blank + `rbind`** (order-sensitive, not `bind_rows`): `action="retry"`, `phase="retry"`, `retry_attempt=1L`, `original_model=original`, `estimation_issue=issue_type`. Sets bbr `based_on` to the original **parent** (skip failed). "THETA adjustment" = sign-flip of the leading numeric on matching lines (preserves precision/`FIX`).

---

## SCM engine

### R/full_scm_search.R
_Top-level orchestrator: (optional initial backward →) forward → final backward → excluded-covariate final testing, with checkpoints + reporting._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `run_automated_scm_testing(search_state, base_model_id, scm_type, starting_phase, full_scm, forward_p_value, backward_p_value, rse_threshold, require_cov_step, auto_submit, auto_retry, save_checkpoints, checkpoint_prefix, final_testing)` | ✓ | Drive the whole SCM/SCM+ pipeline; dispatch standard vs selective forward + backward | `scm_type ∈ {standard,selective}`, `starting_phase ∈ {forward,backward}`, `full_scm`, p-values, `rse_threshold` | big `list(search_state, status, ..., final_model, forward_results, initial_backward_results, final_backward_results, final_covariates, excluded_covariates, ..., final_summary)` | `save_search_state()`→`<prefix>_*.rds` (cwd); `stop()`; `<<-` in 3 error handlers; heavy `cat` | calls `pvalue_to_threshold`, `run_backward_elimination`, `run_stepwise_covariate_modeling`, `run_scm_selective_forward`, `run_univariate_step`, `submit_and_wait_for_step`, `select_best_model`, `get_model_covariates_from_db`, `get_excluded_covariates`, `save_search_state`; **top-level (no in-repo callers)** |

**Notes:** Phase flags derived from `full_scm`+`starting_phase`. Reconciles selective vs standard via `final_best_model %||% final_model`. **Return shape is load-bearing** (`create_scm_results_table` unwraps `$search_state`, reads `forward_results`/`final_backward_results`/`excluded_covariates`/`final_summary`). Excluded-covariate final-testing block is **dead** (only fires `run_forward && !run_final_backward`, false for `full_scm`; also filters `phase=="forward"` never written). Stale "backward not implemented" comments; mojibake banner at L318.

### R/scm-algorithm.R
_Standard forward SCM + covariate-set helpers + exclusion-status printer._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `get_remaining_covariates(search_state, base_model_id, include_excluded=TRUE)` | ✓ | `^beta_` tags not yet in model, optionally minus excluded | state, base, flag | `character()` tags | `cat` | calls `get_model_covariates_from_db`, `get_excluded_covariates`; called-by forward drivers |
| `get_excluded_covariates(search_state, return_details=FALSE, phase_filter=NULL)` | ✓ | Covariates flagged `excluded_from_step==TRUE` | flags | `character()` or details `data.frame` | — | called-by `get_remaining_covariates`, `run_univariate_step`, `view_exclusion_status`, orchestrator |
| `run_stepwise_covariate_modeling(search_state, base_model_id=NULL, auto_submit=TRUE, forward_p_value=NULL, rse_threshold=NULL)` | ✓ | **Standard forward SCM:** univariate step 1 then iterative forward until no improvement. DB-driven each step (`get_remaining_covariates(current_base)`), so resume-safe by passing the current best as `base_model_id`. | base, p-value, rse | `list(search_state, status, base_model, final_model, steps_completed, step_results, total_time_minutes, final_covariates)` | reads `.ext`/`.yaml`; `save_search_state()`→`scm_forward_step_<N>.rds` after each step; `cat` | calls `get_remaining_covariates`, `run_univariate_step`, `submit_and_wait_for_step`, `select_best_model`, `get_model_covariates_from_db`, `read_nonmem_ext`, `pvalue_to_threshold`, `save_search_state`; called-by orchestrator + `continue_search` |
| `view_exclusion_status(search_state)` | ✓ | Pretty-print exclusion table | `search_state` | `invisible(NULL)` | `cat` | calls `get_excluded_covariates` |

**Notes:** Roxygen advertises "final testing of dropped covariates" but body only does forward steps (that phase is in the orchestrator). Step numbers derived from `max(step_number)+1`.

### R/scm-execution.R
_One univariate SCM step (create candidates) + submit-and-monitor loop with auto-retry._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `run_univariate_step(search_state, base_model_id, covariates_to_test=NULL, step_name, include_excluded=TRUE)` | ✓ | One child model per covariate tag at a shared step number | tags, step_name | `list(search_state, ..., models_created, successful/failed_covariates, status)` | `stop()` on invalid tags / step inconsistency; `cat` | calls `get_remaining_covariates`, `get_excluded_covariates`, `add_covariate_to_model`; called-by forward drivers + orchestrator |
| `submit_and_wait_for_step(search_state, model_names, step_name, max_wait_minutes=NULL, threads=NULL, auto_submit=TRUE, auto_retry=TRUE)` | ✓ | Submit via bbr, poll 60s, auto-retry failures until done | `model_names` | `list(search_state, completed_models, failed_models, still_running, ..., retry_models_created, status)` | **`bbr::read_model/submit_model`**; `save_search_state()`→`monitoring_update_<N>.rds` every 5th; `Sys.sleep(60)`; **`<<-` in error handlers**; `cat` | calls `bbr::*`, `find_model_file`, `update_all_model_statuses`, `extract_nonmem_timestamps`, `process_estimation_issues`, `save_search_state`; called-by all drivers |

**Notes:** **BUG** — `run_univariate_step`'s tryCatch error handler records failures with `<-` in the handler frame → thrown-error failures dropped from tracking. `submit_and_wait_for_step` = the long-running monitor (`while(TRUE)` until `models_still_running==0`/timeout); tracks completed/failed/already-retried to fire `<failed>001` retries once. **`<<-` at L298/L578 mutate the enclosing `search_state` from inside `tryCatch` error callbacks — must return state if extracted.**

### R/scm-evaluation.R
_Statistical best-model selection by per-covariate ΔOFV threshold + RSE._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `select_best_model(search_state, model_names, p_value=NULL, rse_threshold=NULL)` | ✓ | Pick highest-ΔOFV model clearing its df-specific χ² threshold + RSE | `model_names`, p-value, rse | `list(search_state, best_model, significant_models, evaluation_results, criteria_used, status)` | mutates `delta_ofv` for rows missing it; `cat` | calls `update_all_model_statuses`, `pvalue_to_threshold`, `extract_covariate_name_from_tag`, `calculate_covariate_df`; called-by forward drivers + orchestrator |

**Notes:** Threshold per-model from covariate df (default df=1 when unresolved). `NA` RSE treated as acceptable. Best = `which.max(delta_ofv)` among significant rows. Re-reads outputs via `update_all_model_statuses` first.

### R/scm-selective-forward.R
_Selective forward (propagate only covariates from significant models) + redemption phase + checkpoint-resume._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `get_significant_models_from_step(search_state, step_number, p_value, rse_threshold=NULL)` | ✓ | Models in a step clearing per-covariate threshold + RSE | step, p-value | `character()` model names | `warning()` on missing cols | calls `extract_covariate_name_from_tag`, `calculate_covariate_df`, `pvalue_to_threshold`; called-by selective + `get_step_models` |
| `get_step_models(search_state, step_number, p_value=NULL, rse_threshold=NULL)` | ✓ | **Supportive lookup:** reconstruct a step from the DB — base (common parent), tested models, completed, significant, winner. Excludes the `phase=="base"` row. | step | `list(exists, step_number, base_model, models, completed_models, significant_models, best_model)` | none | calls `get_significant_models_from_step`; called-by `run_scm_selective_forward` (continuation branch) + `continue_search` (fwd & bwd) |
| `get_covariates_from_models(search_state, model_names)` | ✓ | Unique valid `^beta_` tags tested in given models | `model_names` | `character()` tags | verbose `cat` | called-by selective + resume |
| `run_scm_selective_forward(search_state, base_model_id=NULL, forward_p_value=NULL, rse_threshold=NULL, auto_submit=TRUE, auto_retry=TRUE, resume=FALSE)` | ✓ | **Selective forward:** first step = all covariates; later steps = only covariates from prior significant models; then redemption. `resume=TRUE` (from `continue_search`) makes the FIRST pass a continuation (selective from `get_step_models(last_step)`) instead of "test all". | base, p-value, rse, resume | `list(search_state, status, final_best_model, step_results, total_time_minutes)` | `save_search_state()`→`scm_selective_step_<N>.rds`/`scm_redemption_<N>.rds`/`scm_selective_complete.rds` (cwd); `cat` | calls `get_remaining_covariates`, `get_step_models`, `get_covariates_from_models`, `get_model_covariates_from_db`, `run_univariate_step`, `submit_and_wait_for_step`, `select_best_model`, `pvalue_to_threshold`, `save_search_state`; called-by orchestrator + `continue_search` |
**Notes:** `run_scm_selective_forward` = the biggest fn (main loop + Scenario A/B redemption). Fresh vs. continuation is an explicit `resume` flag (init `is_continuation <- isTRUE(resume)`, then TRUE after the first pass) — **not** inferred from the DB, because the base model can sit at any `step_number` (it's tagged `phase=="base"`, not fixed at step 0). The continuation branch reconstructs the previous step from the DB via `get_step_models`, so no in-memory `step_results` is needed to resume. `resume_selective_forward` was **removed** earlier (forward-only, broken); `continue_search` (below) replaces it for both phases.

### R/continue-search.R
_Resume an interrupted SCM run (forward or backward) from the last full step._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `continue_search(search_state=NULL, checkpoint=NULL, scm_type=NULL, full_scm=TRUE, forward_p_value=NULL, backward_p_value=NULL, rse_threshold=NULL, auto_submit=TRUE, auto_retry=TRUE)` | ✓ | Resume from last full step: re-read last-step files (`force=TRUE`), require step complete, detect phase from DB, find current best, re-enter the right method. Fwd→(bwd if `full_scm`). | in-memory state **or** checkpoint path; `scm_type` (req. for fwd resume); `full_scm` | invisibly `list(search_state, status, resumed_phase, final_model, final_covariates)`; `status="incomplete_step"` if last step still running | `cat`; `load_search_state`; delegates → method saves | calls `update_model_status_from_files`, `update_all_model_statuses`, `get_step_models`, `evaluate_removal_impacts`, `run_scm_selective_forward(resume=TRUE)`/`run_stepwise_covariate_modeling`/`run_backward_elimination`, `get_model_covariates_from_db` |
| `.scm_continue_backward(search_state, last_step, backward_p_value, rse_threshold, auto_submit, auto_retry)` | — | Re-evaluate the last backward step's removal models (rebuilt via `get_step_models`) → winner → continue `run_backward_elimination` from winner (no re-creation of last step) | last_step | `list(search_state, final_model)` | `cat` | calls `get_step_models`, `evaluate_removal_impacts`, `run_backward_elimination` |
| `.scm_phase_is_backward(phase)` | — | TRUE if phase matches `backward`/`removal` | phase | logical | none | — |
**Notes:** No new state field — `scm_type`/`full_scm` are **args** (DB doesn't record the forward method; thresholds come from `search_config`). Method/phase re-entry reuses the existing methods: **standard fwd** and **backward** are already DB-driven (pass the current best as the start); **selective fwd** gets `resume=TRUE`. Forward is skipped (jump to backward) when the last step yielded no significant model. Requires the last step to be fully terminal (no `created`/`in_progress`/`submitted`) — else returns `incomplete_step`. Winner + significance for the last forward step come from `get_step_models` (same evaluator as the live selective loop; RSE-aware) — the old `.scm_current_best_forward` heuristic was removed.

### R/resume-reconstruct.R
_Re-register an interrupted step's models from disk when they're absent from every saved checkpoint (human-gated resume fallback for the gap before the first per-step checkpoint)._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `reconstruct_step_from_disk(search_state=NULL, step_number, base_model, direction=c("forward","backward"), checkpoint=NULL, models_folder=NULL)` | ✓ | Scan models folder for `run<N>` dirs not in the DB (excl. `run<N>001` retries), keep those differing from `base_model` by exactly one covariate in `direction`, insert schema-safe rows (`phase`/`action`/beta-tag as a live run would), reset counter, read results from files. User supplies step/base/direction (Metworx failures are user-detected, not inferable from files). | in-memory state **or** `checkpoint` path; `step_number`; `base_model`; `direction` | updated `search_state`; prints registered rows + any non-`completed`/skipped models | `cat`; reads `.yaml` + output files | calls `load_search_state`, `yaml::read_yaml`, `update_model_status_from_files`, `update_model_counter`; output feeds `continue_search` |
**Notes:** The **normal** path is the new save-at-creation checkpoints (`*_step_N_created.rds`) written before each step submits, so a mid-step crash already leaves the step in a checkpoint; this function is the fallback when even that is missing. Deliberately user-driven (`direction`, `base_model` not inferred) — a Metworx-dropped run leaves files unchanged, so failure isn't detectable from disk; the user reruns it, then resumes.

### R/scm-backward.R
_Backward elimination + removal-impact evaluator + covariate name/tag/FIX helpers._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `run_backward_elimination(search_state, starting_model, backward_p_value=NULL, auto_submit=TRUE, auto_retry=TRUE, rse_threshold=NULL)` | ✓ | Iteratively remove least-impactful covariate (smallest ΔOFV increase below threshold) until none qualify | starting model, p-value | `list(search_state, status, starting_model, final_model, removed_covariates, ..., starting_ofv, final_ofv, final_covariates)` | `save_search_state()`→`backward_step_<N>.rds`/`backward_elimination_complete.rds` (cwd); `stop()`; `cat` | calls `get_model_covariates_from_db`, `get_fixed_covariates`, `get_covariate_tag_from_name`, `remove_covariate_from_model`, `submit_and_wait_for_step`, `evaluate_removal_impacts`, `pvalue_to_threshold`, `save_search_state`; called-by orchestrator |
| `evaluate_removal_impacts(search_state, base_model, removal_models, completed_models, backward_p_value, rse_threshold=NULL)` | ✓ | ΔOFV per removal candidate; pick smallest meeting criteria | removal_models (cov→model) | `list(search_state, removal_impacts, covariate_to_remove, new_base_model, delta_ofv, ofv_threshold, covariate_df, removable_count)` | writes `delta_ofv` back; `stop()`; `cat` | calls `extract_covariate_name_from_tag`, `calculate_covariate_df`, `pvalue_to_threshold`; called-by `run_backward_elimination` |
| `get_fixed_covariates(search_state, covariate_names)` | ✓ | Covariates flagged FIX (via `covariate_search$FIX_STATUS`) | names | `character()` fixed | — | called-by `run_backward_elimination` |
| `get_covariate_tag_from_name(search_state, covariate_name)` | ✓ | Reverse-map name→tag w/ pattern fallback | name | tag or `NULL` | — | called-by `run_backward_elimination` |

**Notes:** Removal candidates built via `remove_covariate_from_model(save_as_new_model=TRUE)` (wrapped `suppressMessages`), then rows stamped `phase="backward_elimination"`, **`action="remove_covariate"`** (the patch masking the `remove_single_covariate` root bug). `get_fixed_covariates` only checks `FIX_STATUS`; THETA-FIX file parse is a TODO (unimplemented). `evaluate_removal_impacts` seeds an 8-col frame but rbinds 10-col rows (works only because seed is empty).

---

## Utilities

### R/utilities.R
_YAML/model-name helpers + SCM statistics (p-value→ΔOFV thresholds, covariate df, tag parsing, working-file cleanup)._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `clean_dir(models_folder="models")` | ✓ | Delete all `WK_*` NONMEM working files | folder | `integer(1)` count | **file deletion** (`file.remove`); `cat` | — |
| `extract_covariate_name_from_tag(tag)` | ✓ | Covariate name from `beta_COV_PARAM` | tag | `character(1)` or `NA` | `warning` | called by evaluation/selection/validation |
| `pvalue_to_threshold(p_value, df=1)` | ✓ | p-value → χ² ΔOFV threshold via `qchisq` | p, df | `numeric(1)` | `stop()` | called widely |
| `calculate_covariate_df(covariate_name, covariate_search)` | ✓ | df=1 continuous, n_levels−1 categorical (`LEVELS` split on `;`) | name, table | `integer(1)` | `warning` | called widely |

**Notes:** `analyze_*` contain duplicate `run1`→BASE / `\d{3}$`→RETRY regex; both are exported-but-unused. `tags_list` arg unused. No `globalVariables`/`@importFrom` here.

---

## Reporting & tables

### R/reporting.R
_Renders the search database into a formatted plain-text report file._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `generate_scm_report(search_state, output_file="scm_report.txt", print_console=TRUE)` | ✓ | Step-by-step forward/backward/redemption text report | `search_state`, output path | `invisible(NULL)` | **file write** (`writeLines`); `cat`; `stop()` if DB empty | calls `pvalue_to_threshold`; redefines local `%||%` |

**Notes:** Local `%||%` shadow. Classifies via `action`/`phase` (grepl "remove"/"backward"/"redemption|final_test|retry"). Hardcoded `scm_report.txt` (cwd). Backward branch prints `base_ofv` defined only in an earlier block — stale/undefined if base row absent.

### R/scm-results.R
_Builds + pretty-prints the enhanced SCM results table with per-model RSE/OFV selection verdicts._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `create_scm_results_table(search_state)` | ✓ | Per-model results w/ df-aware forward/backward verdicts (`BEST`/`YES`/`NO`/`KEPT`/`REMOVED`) + comments | `search_state` or wrapper list | `data.frame(Phase_Step, Model, Parent_Model, Description, Status, Covariates_in_Model, OFV, Delta_OFV, RSE_Max, Selected, Comment)` | `cat`; reads per-model `.yaml` | calls `pvalue_to_threshold`, `extract_covariate_name_from_tag`, `calculate_covariate_df`, `yaml::read_yaml`; called-by `print_scm_results_table` |
| `print_scm_results_table(search_state, show_rse=TRUE, truncate_covariates=35)` | ✓ | Console-format table + summary | df or state | `invisible(results)` | `cat` | calls `create_scm_results_table` |

**Notes:** Nested helpers `get_model_covariates_display` (YAML tags→`COV_on_PARAM`) and `get_selection_info` (verdict engine — **df-threshold logic duplicated across forward/backward branches**). `Phase_Step` derived from **`phase`+`step_number`, not `action`** (so it's not a pure display-registry lookup). Row-appends via `rbind` in a loop (quadratic). Unwraps `$search_state` at L13-14. Sorts by `gsub("^run","",Model)`.

### R/model-output-tables.R
_NONMEM control/`.cov`/`.cor` parsers + the flextable parameter-report pipeline (`get_param2` → `model_report`)._

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `theme_pps_table(x)` | ✓ | Apply PPS/Times-New-Roman theme to a flextable | flextable | themed flextable | `stop()` if not flextable; `flextable::`/`officer::` | called-by `model_report` |
| `extract_params(lines, block_tag, remove_prefix=FALSE)` | · | Parse a `$THETA`/`$OMEGA`/`$SIGMA` block → name+transform (split each line on `;`) | ctl lines, tag | `tibble(param, trans)` | — | calls `purrr::map_dfr`/`tidyr::separate`; called-by `extract_model_params` |
| `extract_model_params(model_name, models_folder="models")` | · | All THETA/OMEGA/SIGMA params for a model | name, folder | `list(THETAS, OMEGAS, SIGMA)` of tibbles | `stop()` if no ctl | calls `find_model_file`, `extract_params`, `readLines`; called-by `get_param2`, `sample_individual_thetas` |
| `calculate_condition_number(model_number, models_folder="models", tolerance=1e-10)` | · | Condition number (max/min eigenvalue) from `.cor` (fallback `.cov`) | model, folder | `numeric(1)`/`Inf`/`NA` | reads `.cor`/`.cov` | calls `stringr::`, `eigen`; called-by `get_param2` |
| `decode_cov_level(cov_name, level, lookup)` | · | Categorical level→decoded label via lookup | cov, level, lookup | `character(1)` or `NA` | — | called-by `get_param2`; **unit-tested** |
| `get_param2(model_number, count_model, shrinkage="etasd", models_folder="models", spec_pk=NULL, lookup=NULL)` | · | One model's formatted parameter/estimate/RSE/shrinkage table (+OFV, cond number, beta_-tag labels) | model, count, shrinkage | `data.frame` (single- or multi-model shape) | `bbr::read_model/model_summary/param_estimates`; `cat` | calls `calculate_condition_number`, `extract_model_params`, `decode_cov_level`; called-by `model_report`, validation (`get_model_max_rse`, `update_model_status_from_files`) |
| `model_report(model_names, shrinkage="etasd", models_folder="models", spec_pk=NULL, lookup=NULL)` | ✓ | Per-model `get_param2`, join, group, render flextable | names | `flextable` | `cat`; `stop()` if all fail | calls `get_param2`, `theme_pps_table`, `flextable::`/`purrr::reduce` |

**Notes:** `extract_*`, `calculate_condition_number`, `decode_cov_level`, `get_param2` are documented-internal (no `@export`). `theme_pps_table` has both `@keywords internal` **and** `@export` → exported. `get_param2` beta_-tag label parse **duplicated** (label col + short col). `shrinkage` `case_when` selects a length-2 vector (fragile recycling).

### R/plot_nonmem_iterations.R
_Reads NONMEM `.ext` iteration history and plots parameter/OBJ trajectories. (Header comment mislabels the file `ext-file-utilities.R`.)_

| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `read_ext_iterations(ext_file)` | ✓ | Parse `.ext` → tidy iteration table (split TABLE blocks, standardize OBJ, classify `TYPE` ITER/BURN/FINAL/SE/EIGEN/CONDNUM, flag EVALUATION) | `.ext` path | `data.frame(ITERATION, params…, OBJ, EST.NO, EST.NAME, TYPE, EVALUATION)` or empty | `stop()` if multiple OBJ cols | calls `utils::read.table`/`scan`; called-by `plot_nonmem_iterations`, `sample_individual_thetas` |
| `plot_nonmem_iterations(model_name, models_dir="models", transform=TRUE, skip_iterations=0, obj_var="OBJ", max_iterations=100)` | ✓ | Faceted ggplot of OBJ + non-fixed parameter trajectories | name, dir | `ggplot` | `stop()` on bad input | calls `read_ext_iterations`, `tidyr::pivot_longer`, `ggplot2::` |

**Notes:** Own `utils::globalVariables(c("ITERATION","TYPE","value","variable"))`. `transform`/`obj_var` accepted but unused. Uses `@import ggplot2` (full). `read_ext_iterations`' data.frame is consumed **positionally** by `sample_individual_thetas` (col 2 → OBJ), so its column layout/`TYPE` labels are a hard contract.

---

## Exposure-forest simulation pipeline (7 files)

_Data flow: `sample_individual_thetas` → `create_covariate_table` → `apply_covariate_model` (via `build_scenario_parameters`) → `simulate_scenario_profiles` → `plot_exposure_forest`. `create_covariate_boxplots` is a separate branch._

### R/forest-plots.R — theta uncertainty sampling
| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `sample_individual_thetas(model, models_folder="models", Nsamples=1e5, seed=1234)` | ✓(not in NAMESPACE) | Draw `Nsamples` THETA vectors from estimation uncertainty → absolute scale | model, N, seed | `data.frame(ID, THETA1..n)` + attr `sampling_method` | **`mvtnorm::rmvnorm`** (undeclared); `set.seed`; `stop`/`warning` | calls `.find_model_component`, `read_ext_iterations`, `.read_theta_cov`, `.make_psd`, `extract_model_params` |
| `.find_model_component(model, models_folder, exts)` | · | Locate model file (flat/subdir) | exts | path or `NA` | — | calls `find_model_file` |
| `.read_theta_cov(model, models_folder, theta_names)` | · | `n×n` THETA covariance from `.cov`(pref)/`.cor`, zeros for fixed | theta_names | matrix or `NULL` | — | calls `.find_model_component`, `.read_nonmem_matrix` |
| `.read_nonmem_matrix(mfile)` | · | Parse labelled square `.cov`/`.cor` (last TABLE) | path | matrix or `NULL` | `readLines` | called-by `.read_theta_cov` |
| `.make_psd(s, tol=1e-8)` | · | Symmetrise + nearest-PSD projection | matrix | PSD matrix | — | called-by `sample_individual_thetas` |

**Notes:** Dup targets: `.read_nonmem_matrix` vs `calculate_condition_number`; uses `extract_model_params` for `$THETA` trans. `@export` present, **not in NAMESPACE**.

### R/forest-plots.R — covariate scenario table
| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `create_covariate_table(model_name, covariate_search, data, percentiles=c(0.05,0.95), models_folder="models", id_col="ID", lookup=NULL, wrap_width=30)` | ✓(in NAMESPACE) | Null-patient + single-covariate-variation scenario table | search table, data, percentiles | `data.frame`: col1 `Scenario` + one col per covariate; row1 all-reference | `readLines`; `stats::quantile`; `warning`/`stop` | nested helpers; called-by `build_scenario_parameters` |
| `join_wrap/cov_label/cov_unit/decode_level/ordinal/cont_scenario` | · nested | Label/format helpers | — | char scalars | — | nested |

**Notes:** Independently re-parses `$THETA` to find `beta_<COV>_<PARAM>` names + rebuilds `cov_to_test`; nested `decode_level` duplicates `decode_cov_level`. No undeclared deps. `man/create_covariate_table.Rd` exists.

### R/apply-covariate-model.R
| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `apply_covariate_model(model_name, covariate_search, individual_thetas, covariates, models_folder="models")` | ✓(in NAMESPACE) | **Reconstruct the model's covariate factors in R** and apply to sampled THETAs for one scenario | search table (+FORMULA), thetas, one scenario | `data.frame(ID, structural THETA cols)` | `readLines`; `warning`/`stop` | nested helpers; called-by `build_scenario_parameters` |
| `get_block/first_theta_index/param_theta_index/cat_level_theta` | · nested | `$THETA`/`$PK` parsing + categorical level→theta from `IF/ELSEIF` | — | block lines / int / named int | — | nested |

**Notes:** **Primary reconciliation target** — re-derives everything `model_add_cov` writes: `$THETA` order/names, `$PK` `TV_<param>` structural index, `IF(COV.EQ.x)` level→theta, and hardcodes continuous factors (`power/power1/power0.75`→`(cov/ref)^th`, `linear`→`1+(cov-ref)*th`, `exponential`→`exp(th*(cov-ref))`); categorical factor `1 + beta`. **Level→theta relies on the `$PK` numeric-level parse** (the `$THETA` name has the decoded label, not the level). `man/apply_covariate_model.Rd` exists.

### R/build-scenario-parameters.R
| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `build_scenario_parameters(model, covariate_search, thetas, data, percentiles=c(0.05,0.95), models_folder="models", lookup=NULL, id_col="ID", wrap_width=30, scenario_table_path=paste0("scenario_table_",model,".rds"))` | ✓(not in NAMESPACE) | For each scenario, apply covariate factors to sampled THETAs; persist table | thetas, search table, data | named list of `data.frame`s (one per scenario; elem1 = typical subject) | **`saveRDS`** to `scenario_table_<model>.rds` (cwd); `dir.create`; `stop` | calls `create_covariate_table`, `apply_covariate_model` |

**Notes:** Pure orchestration. `@export` present, **not in NAMESPACE**. Hardcoded default RDS output path (cwd).

### R/forest-plots.R — simulate scenario profiles
| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `simulate_scenario_profiles(param_sets, mod, dose, start=0, end=24, delta=0.1)` | ✓(not in NAMESPACE) | Simulate every sample of every scenario under a common dose; stack + tag by scenario | param_sets, mrgsolve `mod`, `ev` dose | `data.frame` (Scenario ordered factor, ID, time, captured cols) | **`mrgsolve::mrgsim`** (undeclared); `purrr::imap_dfr`; `stop` | upstream `build_scenario_parameters`; downstream `plot_exposure_forest` |

**Notes:** `mrgsolve` undeclared. `inherits(dose,"ev")` couples to mrgsolve. `@export`, **not in NAMESPACE**. Caller builds `mod`/`dose`.

### R/forest-plots.R — exposure forest plot
| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `plot_exposure_forest(data, metric=c("AUC","Cmax","Cmin"), ss=TRUE, ClinicalRelevanceLow=0.8, ClinicalRelevanceHigh=1.25, reference="Typical subject", scenario_order=NULL, fontsize=9, title=NULL, filename=NULL, output_format=c("emf","png"), width, height)` | ✓(not in NAMESPACE) | Forest plot of one metric normalised to typical-subject median (box 2.5/25/50/75/97.5) | data, metric, bands, filename | `ggplot` (invisibly; saved if `filename`) | **file write** `ggsave` — **`devEMF::emf`**(undeclared)/png; `tools::file_path_sans_ext`; `stats::`; `dplyr::` | calls `theme_forest`, `boxquantile`; upstream `simulate_scenario_profiles` |
| `theme_forest(base_size=12, base_family="")` | ✓(not in NAMESPACE) | `theme_bw()` variant | sizes | ggplot2 theme | `grid::unit` | called-by `plot_exposure_forest` |
| `boxquantile(y)` | · | Box stats at 2.5/25/50/75/97.5 | y | named numeric length-5 | `stats::quantile` | `stat_summary` |

**Notes:** `devEMF` undeclared; `grid`/`tools`/`grDevices` un-declared base-priority. `@export` on `plot_exposure_forest`+`theme_forest`, **neither in NAMESPACE**. Hardcoded palette + band strings.

### R/covariate-boxplots.R
| Function | Exp | Purpose | Key inputs | Returns | Side effects | Calls / Called-by |
|---|---|---|---|---|---|---|
| `create_covariate_boxplots(data, spec, con=NULL, cat=NULL, type=c("AUC","Cmax","Cmin"), drug, param_info, stratification=NULL, output_folder="results/figure/simulations", width=NULL, height=NULL, base_size=10, verbose=TRUE, combined_pdf=TRUE)` | ✓(not in NAMESPACE) | Boxplots per metric×covariate (continuous→quartile bins, categorical→decoded), + REF box, per-cov `.emf` + combined PDF | data, `spec` (yspec), con/cat, type | nested list `results[[type]][[cov]]` = ggplot (invisible) | **file write** `.emf` via `devEMF`, `grDevices::cairo_pdf`; `dir.create`; `yspec::ys_load/ys_get_short_unit`; `grid::unit`; `stop` | standalone (not in the forest data flow) |

**Notes:** `devEMF` undeclared; `grid`/`grDevices` un-declared; `yspec` declared. `@export`, **not in NAMESPACE**. Hardcoded `results/figure/simulations`. Defers categorical decode to `yspec::decode_dataset` (fails fast on raw numeric codes) — conceptually overlaps `create_covariate_table`'s decode but doesn't re-implement it.
