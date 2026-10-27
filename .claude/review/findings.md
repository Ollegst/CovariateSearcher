# Package review — findings register

Read-only review of `R/` for logic errors, inconsistencies and optimisation,
area by area, with every existing behaviour kept. This file is the record:
findings are added here as they are reported, and nothing in `R/` changes
until the owner has accepted a finding.

**Baseline:** `main` at `db146f1` (v0.4.7) — 13 test files, 162 tests,
0 failures, 18 warnings (all from `test-model-status.R`). Old-vs-new
comparison: `.claude/scripts/compare-model-readers.R`.

## Summary (review completed 2026-10-25)

All 8 areas and the cross-area pass reviewed by Fable; every finding re-checked independently
(Opus; Sonnet for the cross-area pass). **88 findings recorded, 1 rejected.**

| Severity | Count | Meaning |
|---|---|---|
| result | 32 | can change which covariates are accepted, the model written, or published numbers |
| failure | 19 | crash, hang, lost/overwritten files, or a covariate silently not tested |
| cosmetic | 37 | messages, labels, dead code, small inefficiencies |

**Highest priority** — confirmed by running code, reachable through normal use:
- **SIM-12** forest-plot pipeline crashes for any model with exactly one covariate effect (since 41beeb2).
- **SCM-01** selective forward's redemption phase starts from the wrong step (re-runs models, stops after one round).
- **UTL-01** degrees of freedom looked up by covariate name only — WT fixed on CL makes an estimated WT on V pass at threshold 0.
- **MOD-01** a new covariate can be bound to an existing residual-error THETA.
- **MOD-02** parameters with IOV are treated as log scale; the covariate is added instead of multiplied.
- **REP-04 / REP-10** a lowercase `; log` tag or a negative correlation lets a high-RSE parameter pass the RSE test.
- **XAR-03** an indented ` $THETA` makes every model fail the RSE test.
- **STA-01** a mistyped categorical FORMULA is written without the covariate; it is never really tested.
- **REC-01** resuming after a crash before submission silently skips that step's covariates.
- **SIM-01** MU-referenced models without a `TV_` line feed log-scale values to mrgsolve.
- **REP-01 / REP-03** the condition number and small estimates in the published parameter table are wrong.

**Recommended order:** fix the groups, not single findings (see *Fix groups* at the end) — G1 (one
`$THETA` parser), G2 (one record matcher), G3 (one annotation reader) and G6 (pass the tag for df)
together close most of the result-level findings. Then SIM-12 and SCM-01 on their own; then the rest
by severity. Each fix: a failing test first, the old-vs-new comparison, `r-pkg-qc`, `diff-auditor`.

## Rules

- **Review only.** A review pass never edits `R/`, tests or docs.
- **Before flagging,** read `.claude/FUNCTIONS.md`: its cross-cutting hazards,
  deliberate exceptions ("do not fix it back"), known limitations and
  *Rejected designs*. A deliberate decision recorded there is not a finding.
- **Verified** is either `confirmed` (shown by running code, with the input
  and output quoted) or `plausible` (reasoned from reading only). A second,
  independent check is needed before `plausible` becomes `confirmed`.
- **Every fix** needs a test that fails before it, the old-vs-new comparison
  showing nothing else changed, then `r-pkg-qc` and `diff-auditor`.
- **Rejected findings stay here** with the reason, so they are not re-proposed.
  A fixed bug is also recorded under *Confirmed bugs* in `.claude/FUNCTIONS.md`.

## Areas

| # | Prefix | Area | Files | Status |
|---|---|---|---|---|
| 1 | SCM | SCM search engine | full_scm_search, scm-algorithm, scm-execution, scm-evaluation, scm-selective-forward, scm-backward | reviewed (Fable) + verified (Opus) — 10 findings |
| 2 | MOD | Model editing | model-modification, covariate-formula | reviewed (Fable) + verified (Opus) — 12 findings |
| 3 | STA | Search setup and state | initialization, validation, database-management, model-discovery | reviewed (Fable) + verified (Opus) — 10 findings |
| 4 | NIO | NONMEM file reading and monitoring | file-io, monitoring-files (+ `.ext` reader in plot_nonmem_iterations) | reviewed (Fable) + verified (Opus) — 7 findings |
| 5 | REC | Recovery and resume | recovery-detection, recovery-actions, continue-search, resume-reconstruct | reviewed (Fable) + verified (Opus) — 7 findings |
| 6 | REP | Reporting and diagnostics | reporting, scm-results, model-output-tables, plot_nonmem_iterations, plot-bootstrap-distributions | reviewed (Fable) + verified (Opus) — 14 findings |
| 7 | SIM | Simulation and exposure plots | forest-plots, apply-covariate-model, build-scenario-parameters, covariate-boxplots | reviewed (Fable) + verified (Opus) — 12 findings |
| 8 | UTL | Shared utilities | utilities, CovariateSearchr, imports | reviewed (Fable) + verified (Opus) — 7 findings |
| X | XAR | Cross-area pass | shared rules implemented in more than one area | reviewed (Fable) + verified (Sonnet) — 9 findings, 1 rejected |

## Entry format

```
### SCM-01 — <one-line claim>
- **Location:** R/<file>.R:<line>[, …]
- **Type:** bug | inconsistency | optimisation | simplification
- **Severity:** result (changes which model/covariates the search reports) |
  failure (crash, hang, lost work) | cosmetic
- **Verified:** confirmed | plausible
- **Evidence:** the code path, and the failing input/output where one exists
- **Proposed fix:** …
- **Must keep:** the behaviour a fix has to preserve
- **Status:** open | accepted | rejected — <reason> | fixed in <commit>
```

## Decisions needed from the owner

| ID | Question |
|---|---|
| SCM-01 | Accept the fix? And on the "step k created models but none completed" exit, should redemption retry step k's failed covariates (today's Scenario A) or exclude them (Scenario B)? **Decided 2026-10-27:** redemption is a new selective search from the best model. Step 1 tests every covariate not in the best model and without a COMPLETED model on that base (failed ones are tested again, as redemption already uses `include_excluded = TRUE`); step 2+ tests only covariates significant in the previous step, on the new winner; continue while a step finds something significant. Implement by the parent-model rule rather than Scenario A/B step arithmetic. |
| SCM-02 | Is a base that already carries covariates, run with `starting_phase = "forward"`, supported? If yes, final backward must not be skipped. |
| SCM-06 + REC-04 | Should a covariate whose retry failed be excluded until redemption? Today exclusion never takes effect (REC-04), so SCM-06's fix alone would change nothing. **Decided 2026-10-27:** standard SCM keeps testing failed/excluded covariates at every step (`include_excluded = TRUE`, current behaviour — record in FUNCTIONS.md). Selective main steps keep `FALSE`. Redemption and the forward phase of SCM+ are new searches: their first step includes previously excluded covariates. Fix REC-04 (names vs names) so `FALSE` works. **Added 2026-10-27:** a user option `retest_failed` (default TRUE, stored in `search_config`) controls whether failed/excluded covariates are tested again — every step in standard SCM, redemption step 1, and the first forward step of SCM+. `final_testing` stays accepted with no effect. |
| NIO-02 | Should "finished" mean `Stop Time:` is present (what monitoring uses)? That rejects base models from tools that do not write `Stop Time:` (only `nmfe` does). Alternative: align the other way. |

## Findings

### Area 1 — SCM search engine
Reviewed by Fable (scripts in `D:\R\tmp\review-scm\`), every finding re-checked by an
independent Opus pass (`D:\R\tmp\verify-scm\`). Verdicts below are the checker's.

#### SCM-01 — Selective forward's redemption phase starts from the wrong step
- **Location:** R/scm-selective-forward.R:577-581, :603, :610, :712/:731, :757, :907-912
- **Type:** bug · **Severity:** result · **Verified:** confirmed (reviewer and checker each ran it)
- **Evidence:** `current_step` is incremented (:603) only after a step produced significant
  models. Forward normally ends with "step k created models, none significant" (:577-581),
  breaking with `current_step == k` while the database already holds step k, so
  `last_main_step <- current_step - 1` (:610) is k-1. Consequences: Scenario A fires instead
  of B, redemption rebuilds covariate sets already tested at step k, it then evaluates main
  step k (:909, nothing significant by construction) instead of the redemption step it just
  ran, prints "No significant improvements" and stops after one round. When step 1 finds
  nothing, the whole univariate screen is rebuilt. Simulated search (oracle OFVs, real
  selection/evaluation/redemption code; only model creation, submission and file I/O mocked —
  checker confirmed the mock assigns step numbers exactly like `run_univariate_step`):
  final model `{A,D}` (OFV 970) with duplicates; patched: three redemption rounds, `{A,C,D}`
  (OFV 964), no duplicates.
- **Affected exits:** :506, :537, :577. Correct already: :343, :404, :474 (ran out of covariates).
- **Proposed fix:** set `last_created_step <- last_step` after `step_results <- list()`
  (:267), `last_created_step <- current_step` after a successful `run_univariate_step`
  (:512), and use it at :610. Optionally assert the redemption step number matches.
- **Caveat found by the checker:** on the :537 exit (step k created, nothing completed) the
  fix switches to Scenario B, which excludes all of step k's covariates from redemption
  (:722), so their failed models are never retried; today's Scenario A retries them. Either
  decide that is right, or set `last_created_step` only once step k has completed models.
- **Must keep:** Scenario A on the :474 exit; Scenario B's exclusion of the last step's
  covariates; the overall pick (:962); the four phase-filtered pools and their grep test.
- **Status:** fixed in d6cdce4 (2026-10-26)

#### SCM-02 — Final backward is skipped after an unchanged forward even with no initial backward
- **Location:** R/full_scm_search.R:316-321
- **Type:** bug or undocumented design · **Severity:** result · **Verified:** plausible (checker agrees)
- **Evidence:** the skip tests only `model_before_forward == current_model`; its message
  ("identical to initial backward") is false for `starting_phase = "forward"`. With a base that
  already carries covariates and a forward that adds nothing, those covariates are never tested
  for removal. Nothing else forces backward; base validation does not reject such a base.
  Mitigation: README.Rmd:221-230 sends "previously developed model" users to
  `starting_phase = "backward"`.
- **Proposed fix:** add `&& run_initial_backward` to the skip condition.
- **Must keep:** the skip for SCM+ (backward → forward → backward) when forward changed nothing.
- **Status:** fixed in 4c591ad (2026-10-26)

#### SCM-03 — Resuming a selective search can skip the redemption phase
- **Location:** R/continue-search.R:308-345 (caller side); R/model-modification.R:29; R/scm-selective-forward.R:612-950
- **Type:** inconsistency · **Severity:** result (resume only) · **Verified:** confirmed by reading
- **Evidence:** `continue_search` jumps to backward when the last step has no significant
  models — exactly where a live selective run starts redemption. FUNCTIONS.md records the
  jump but not its effect on redemption. Redemption rows carry `phase = "forward_selection"`
  like main rows, so a resume after a redemption step re-enters the main loop
  (`include_excluded = FALSE`, base = that step's winner) and later runs redemption again.
  The gap widens once SCM-01 is fixed.
- **Proposed fix:** for `scm_type == "selective"`, always re-enter
  `run_scm_selective_forward(resume = TRUE)` and let it decide. Telling redemption rows apart
  needs a new `phase` value — a vocabulary change touching the display registry; flag, not do.
- **Must keep:** standard-SCM resume (converged → backward); the backward resume path.
- **Status:** fixed in d791a20 (2026-10-26)

#### SCM-04 — After an initial-backward error the search continues on a stale model counter
- **Location:** R/full_scm_search.R:176-197; R/model-modification.R:90-95
- **Type:** bug · **Severity:** failure (overwritten files) · **Verified:** confirmed for initial backward
- **Evidence:** the error handler returns the pre-phase `search_state` and continues into
  forward; `model_counter` is only re-synced at initialization and in
  `reconstruct_step_from_disk`. Forward then reuses `run<N>` names and
  `add_covariate_to_model` uses `bbr::copy_model_from(.overwrite = TRUE)`, overwriting removal
  models already on disk. Final backward (:333-368) is *not* affected the same way: nothing is
  created after it; its rows survive in its own checkpoints and `continue_search`'s auto-heal
  registers them.
- **Proposed fix:** treat an initial-backward error like a forward error — return
  `status = "failed"`, `error_phase = "initial_backward"` — instead of continuing.
- **Must keep:** the `<<-` error capture; the forward-failure return shape read by `create_scm_results_table`.
- **Status:** fixed in 043bec2 (2026-10-26)

#### SCM-05 — Fallback-base block in selective forward is unreachable from normal use
- **Location:** R/scm-selective-forward.R:354-396 vs :402-409
- **Type:** simplification · **Severity:** cosmetic · **Verified:** plausible, with correction
- **Evidence:** `best_model` is NULL only when the previous step has no significant models, and
  :404 then breaks. Correction by the checker: a *direct* call of the exported function with
  `resume = TRUE` on such a step does use the block's pick as the redemption base and step
  winner. Not reachable from a fresh run, `continue_search` or `run_automated_scm_testing`.
- **Proposed fix:** replace with `current_best_model <- prev_step$base_model` (keeps the direct
  resume case) and update the grep test at tests/testthat/test-model-criteria.R:365-389.
- **Must keep:** "no significant models ⇒ stop the main loop"; the other three pools and the test.
- **Status:** fixed in 2bcf8c9 (2026-10-26)

#### SCM-06 — Standard and selective forward treat excluded covariates differently
- **Location:** R/scm-algorithm.R:204, :342, :355 vs R/scm-selective-forward.R:298, :500, :861
- **Type:** inconsistency · **Severity:** cosmetic (extra runs) · **Verified:** confirmed as fact; looks intended
- **Evidence:** standard SCM re-offers an excluded covariate every step (and may retry it each
  time); selective defers it to the explicitly "including excluded" redemption step. Not
  recorded in FUNCTIONS.md.
- **Proposed fix:** if intended, record it in FUNCTIONS.md; otherwise pass `include_excluded = FALSE` in standard's later steps.
- **Status:** fixed in d6cdce4 (2026-10-26)

#### SCM-07 — The returned `excluded_covariates` is always empty
- **Location:** R/full_scm_search.R:390-408, :564, :584, :603 (vs the unfiltered :280)
- **Type:** bug (reporting) · **Severity:** cosmetic · **Verified:** confirmed; dead block already recorded
- **Evidence:** the filter `phase == "forward"` matches nothing (exclusion rows are
  `forward_selection`/`retry`). FUNCTIONS.md:55 already records this as why the final-testing
  block is dead. New: it also empties the summary and the returned field that
  `create_scm_results_table` reads, while the forward-failure return (:280) is unfiltered.
- **Proposed fix:** filter on `"forward_selection"`; retire the dead block (:415-519) as recorded.
- **Must keep:** the return-list field names.
- **Status:** fixed in 2bcf8c9 (2026-10-26)

#### SCM-08 — Backward's banner states the unreadable-RSE rule differently from the evaluator
- **Location:** R/scm-backward.R:40-45 vs R/scm-evaluation.R:117-122
- **Type:** inconsistency · **Severity:** cosmetic · **Verified:** confirmed
- **Evidence:** `require_cov_step = NA` passes validation (`is.logical(NA)`); the banner
  (`isTRUE`) says "accepted", the evaluator (`identical(…, FALSE)`, recorded as deliberate) rejects.
- **Proposed fix:** banner uses `!identical(require_cov_step, FALSE)`.
- **Status:** fixed in 043bec2 (2026-10-26)

#### SCM-09 — A covariate fixed through `INIT` still gets a removal model every backward step
- **Location:** R/scm-backward.R:504-532 vs R/utilities.R:552-586
- **Type:** inconsistency · **Severity:** cosmetic (wasted runs) · **Verified:** confirmed; already recorded as a TODO
- **Evidence:** `get_fixed_covariates` reads `FIX_STATUS`, which nothing writes; `INIT = "… FIX"`
  gives df 0. The verdict is unaffected (threshold 0), only runs are wasted.
- **Proposed fix:** also treat `calculate_covariate_df(...) == 0` as fixed.
- **Status:** fixed in f0dd2bc (2026-10-26)

#### SCM-10 — `get_covariate_tag_from_name`'s constructed fallback is dead in practice
- **Location:** R/scm-backward.R:554-567
- **Type:** simplification · **Severity:** cosmetic · **Verified:** confirmed
- **Evidence:** tags keep their original case (initialization.R:166/1189); the lower-case
  fallback only fires on a case mismatch, and names with more than two `_` parts are skipped.
- **Proposed fix:** drop the fallback, or make the comparison case-insensitive.
- **Status:** fixed in 2bcf8c9 (2026-10-26)

### Area 4 — NONMEM file reading and monitoring
Reviewed by Fable (`D:\R\tmp\review-NIO\`), re-checked by Opus (`D:\R\tmp\verify-NIO\`);
nothing under test was mocked. Session used: R 4.4.2, UTF-8 locale (the default for
R >= 4.2 on Windows and on Linux/Metworx).

#### NIO-01 — Control streams are read without encoding repair; a cp1252 byte in a comment breaks comment detection
- **Location:** R/file-io.R:22 (`read_model_file`), R/model-modification.R:1267; R/validation.R:803; other bare
  control-stream `readLines()` sites: initialization.R:133,858; validation.R:1244; recovery-actions.R:391;
  model-output-tables.R:160; apply-covariate-model.R:110; forest-plots.R:222,390,789
- **Type:** bug · **Severity:** failure on the default path; result off it · **Verified:** confirmed (both)
- **Evidence:** in a UTF-8 session a cp1252 byte (µ = 0xB5, saved by a legacy "ANSI" editor) gives an
  invalid string; `grepl("^\s*;")` returns FALSE on it, so a comment-only line in `$THETA` is counted as
  a THETA. Driven through `remove_covariate_from_model` with two covariates, "remove WT on CL" returned
  success but stripped the AGE effect from V and bound `beta_AGE_V`'s value to the WT factor.
  **Default path (checker):** `initialize_covariate_search(validate_parameters = TRUE)` runs
  `validate_parameter_blocks`, which also reads with bare `readLines` and rejects the same comment line —
  init stops with "Base model validation FAILED", blaming comment format rather than encoding. Silent
  corruption therefore needs `validate_parameters = FALSE`, a direct call on an unvalidated model, or a
  model edited by hand after init.
- **Proposed fix:** after `readLines`, `bad <- !validUTF8(lines); lines[bad] <- iconv(lines[bad], "CP1252", "UTF-8")`
  — **CP1252, not latin1** (latin1 turns smart quotes, dashes and € into control characters). Apply it in
  one shared reader used by `read_model_file` **and** validation.R:803 (and the other sites listed),
  otherwise init still rejects the base model.
- **Note:** write-back then stores those comment lines as UTF-8 (harmless to NONMEM; worth knowing if
  other tools expect cp1252).
- **Must keep:** valid UTF-8 untouched; ASCII unchanged; `file_path` attribute; `.ctl` before `.mod`.
- **Status:** fixed in a08a8cb (2026-10-26)

#### NIO-02 — The two run-success readers disagree on when a run has finished (= STA-02)
- **Location:** R/file-io.R:449 vs R/validation.R:141-155
- **Type:** inconsistency · **Severity:** result only in a rare case; otherwise failure · **Verified:** confirmed (both)
- **Evidence:** listing with "MINIMIZATION SUCCESSFUL", no `Stop Time:`, no `.cov` (a run in `$COV` or
  between `$EST` steps): `get_model_status_from_files` → `failed` (`require_cov_step = TRUE`) /
  `completed` (FALSE); `update_model_status_from_files` → `in_progress` for both. FUNCTIONS.md:40 says
  they are identical. Also, by reading: a bbi failed launch is `failed` to one and `not_run` to the other.
  **Impact (checker):** with the default `require_cov_step = TRUE` a running base reads `failed` and init
  stops loudly. A wrong number needs `require_cov_step = FALSE` plus a multi-`$EST` base started between
  estimation steps: it is seeded `completed` with the first method's OFV, never re-read, and every ΔOFV
  is wrong.
- **Proposed fix:** `read_nonmem_lst` returns `has_stop_time`; `get_model_status_from_files` returns
  `"incomplete"` without it. **Risk (checker):** `Stop Time:` is written by `nmfe`; a base model run some
  other way would then read `incomplete` forever and be rejected at init (accepted today). No existing
  test covers a listing without `Stop Time:`. Decision needed (see top).
- **Must keep:** the rejected design (termination verdict never disqualifies; rounding-error run with OFV
  + `.cov` is `completed`); the `incomplete`/`in_progress` spellings; `read_error` → `failed` first.
- **Status:** fixed in 304a56a (2026-10-26)

#### NIO-03 — `.read_listing_lines`' latin1 fallback can never fire
- **Location:** R/file-io.R:51-71; FUNCTIONS.md:185 describes it
- **Type:** bug · **Severity:** cosmetic · **Verified:** confirmed (both)
- **Evidence:** `readLines` never errors on invalid bytes, so the `tryCatch` fallback is dead; one
  `read_nonmem_lst()` on a listing with two 0xB5 lines raised 24 warnings, the timestamp reader 26;
  classification stays correct (NONMEM's marker lines are ASCII). The existing non-UTF-8 test passes only
  because `expect_silent` wraps the read, not the classification.
- **Proposed fix:** read once, repair invalid lines with CP1252 as in NIO-01.
- **Must keep:** `skipNul = TRUE`; an unopenable path still errors (the `read_error` test).
- **Status:** fixed in a08a8cb (2026-10-26)

#### NIO-04 — `read_ext_iterations` crashes on a TABLE with a header but no rows yet
- **Location:** R/plot_nonmem_iterations.R:66-82
- **Type:** bug · **Severity:** failure (user-called plot only) · **Verified:** confirmed (both)
- **Evidence:** "replacement has 1 row, data has 0" while an `$EST` has just started — the moment an
  iteration plot is most wanted. The guard at :66 only catches a bare TABLE line.
- **Proposed fix:** `if (nrow(df) == 0L)` skip the section.
- **Must keep:** column layout and TYPE labels used by `sample_individual_thetas` and `.read_final_omega_matrix`.
- **Status:** fixed in bb86fa6 (2026-10-26)

#### NIO-05 — Each monitoring poll reads every active model's listing about six times
- **Location:** R/file-io.R:548,559; R/validation.R:92,138,166; R/scm-execution.R:408
- **Type:** optimisation · **Severity:** cosmetic · **Verified:** confirmed (trace count; checker by reading)
- **Proposed fix:** return the lines / `has_stop_time` from `read_nonmem_lst` and reuse them; let
  `extract_nonmem_timestamps` take pre-read lines; drop the second timestamp pass. Overlaps STA-10.
- **Status:** fixed in 6df29b2 (2026-10-26)

#### NIO-06 — `read_ext_file` reports a finished run's OFV as 0 and SAEM burn-in as completed
- **Location:** R/monitoring-files.R:104-122, 154
- **Type:** bug · **Severity:** cosmetic (sole caller `detect_estimation_problems` is not used by the search) · **Verified:** confirmed (both)
- **Proposed fix:** OFV from the last row with ITERATION >= 0 or == -1000000000; completed iff a
  -1000000000 row exists in the last table.
- **Status:** open — low priority

#### NIO-07 — Timestamps with a numeric time zone (`+03`) never parse
- **Location:** R/file-io.R:586-589
- **Type:** bug · **Severity:** cosmetic (`submission_time`/`completion_time` only) · **Verified:** confirmed (both)
- **Proposed fix:** also strip `\s+[+-]\d{2,4}`.
- **Status:** fixed in 6df29b2 (2026-10-26)

### Area 3 — Search setup and state
Reviewed by Fable (`D:\R\tmp\review-STA\`), re-checked by Opus (`D:\R\tmp\verify-STA\`).

#### STA-01 — A FORMULA that is not a real covariate form passes validation and is written with no covariate in it
- **Location:** R/covariate-formula.R:78-86, 213-244; R/initialization.R:700-711, 1164-1183; R/model-modification.R:370-374, 453-462
- **Type:** bug · **Severity:** result · **Verified:** confirmed end to end (checker drove the real functions)
- **Evidence:** an unregistered name falls through to `parse_covariate_expression`, which accepts any
  symbol without requiring `cov`. From a hand-written CSV, `initialize_covariate_search` passed
  `cat/SEX/"exponential"` and `con/WT/"BETA*2"`, and `model_add_cov` wrote `TV_CL = THETA(1) * (THETA(3))`
  and `TV_V = THETA(2) * (THETA(3) * 2)`. `add_covariates_to_search` with the typo "exponental" also passed.
  Only `build_covariate_reference_table` rejects these. Effect (checker's correction): the extra THETA is
  confounded with the parameter's own, so ΔOFV ≈ 0 and it is never accepted — the intended covariate is
  simply never tested and is reported as not significant.
- **Proposed fix:** in `validate_covariate_search_table` (and `generate_tags_from_covariate_search`)
  require a built-in for that STATUS or an expression using `cov` — the rule `build_covariate_reference_table`
  already applies. Lower-case STATUS first (it is compared case-sensitively there).
- **Must keep:** user expressions in `cov`/`ref`/theta names; `cat.linear`/`cat.power`; case-insensitive matching.
- **Status:** fixed in f16b579 (2026-10-26); audit follow-up in f670f39 (2026-10-26)

#### STA-02 — Duplicate of NIO-02 (run-success readers disagree on "finished"); confirmed again independently.

#### STA-03 — `starting_model_number` below the highest existing model reuses run numbers, overwrites files and silently drops a model
- **Location:** R/initialization.R:244-255; R/database-management.R:299-331; R/model-modification.R:90-95; R/utilities.R (`.is_search_model`)
- **Type:** bug · **Severity:** failure and result · **Verified:** confirmed (real bbr models)
- **Evidence:** existing run1, run7, run12; `starting_model_number = 5` passes (only checked against
  the base number). Two adds made run6 and run7: the user's run7 was overwritten
  (`copy_model_from(.overwrite = TRUE)`), the database got two run7 rows, and
  `update_model_status_from_files(run7)` errors on a length-2 status. **Found by the checker:**
  `.is_search_model` matches the first run7 row (the discovered one, `created_by_search = FALSE`), so the
  new run7 is dropped before submission and never status-checked — the step silently loses that covariate.
- **Proposed fix:** after `update_model_counter`, `stop()` if `starting_model_number < model_counter`.
- **Must keep:** jumping numbering upward; the retry exclusion in the counter.
- **Status:** fixed in c13da8f (2026-10-26)

#### STA-04 — The `$DATA` name check stops on the documented `.rds` dataset input
- **Location:** R/initialization.R:16-17, 129-156
- **Type:** bug · **Severity:** failure · **Verified:** confirmed by reading (deterministic)
- **Evidence:** compares full basenames, so `x.rds` never equals `$DATA x.csv`.
- **Proposed fix:** compare names without extensions (this also accepts `data.dat` vs `data.csv` — state it), or skip for non-CSV input.
- **Status:** fixed in 619ea53 (2026-10-26)

#### STA-05 — Time-varying detection counts NA as a level in `model_add_cov` only (root shared with MOD-05)
- **Location:** R/initialization.R:860-868, 497-501; R/model-modification.R:416-417
- **Type:** inconsistency · **Severity:** cosmetic for built-in formulas; result for log-scale user expressions · **Verified:** confirmed
- **Evidence:** WT = 60, NA, 60 for one subject: `model_add_cov` places the effect on the individual
  line (`CL = TV_CL * EXP(ETA(1)) * (WT/75)**THETA(3)`) while the table says time-independent. For the
  built-ins this fits the same model (checker); for a user expression on a log-scale parameter it is a
  wrong model (written on the log scale by contract, but multiplied in).
- **Proposed fix:** one shared `.covariate_is_time_varying(data, cov, id)` that ignores NA, used by all three sites.
- **Status:** fixed in 2bcf8c9 (2026-10-26)

#### STA-06 — No `TIME == 0` rows: continuous REFERENCE becomes NA silently; categorical crashes with an opaque error
- **Location:** R/initialization.R:429-467, 511-512, 669-691
- **Type:** bug · **Severity:** failure · **Verified:** confirmed
- **Evidence:** first record at TIME 0.5 → `REFERENCE = NA` for a continuous covariate (validation
  passes it; the model would get `(WT/NA)`); categorical → "values must be length 1, but FUN(X[[1]]) result is length 0".
- **Proposed fix:** stop (or warn and fall back to each subject's first record) when the baseline is
  empty. Require a non-NA REFERENCE only for built-in formulas or expressions that use `ref` — not for
  expressions like `EMAX*cov/(EC50+cov)` (checker).
- **Must keep:** baseline-only REFERENCE for time-dependent covariates.
- **Status:** fixed in 6df29b2 (2026-10-26)

#### STA-07 — `create_retry_model` writes status "error", which nothing treats as terminal
- **Location:** R/recovery-actions.R:72, 203; R/utilities.R:197-199
- **Type:** inconsistency · **Severity:** cosmetic · **Verified:** confirmed as inconsistency
- **Evidence:** in the real flow the original is already "failed" and the next re-read restores
  "failed" (self-heals); the recorded `estimation_issue` is lost. The reviewer's demo used a row whose
  files read completed, which the retry path never produces.
- **Proposed fix (checker's correction):** do **not** write "estimation_error" or make "error" terminal —
  that breaks the failed counts (scm-results.R:225 shows "In Progress"; reporting.R:115/295 and
  database-management.R:432 stop counting it). Leave the status as "failed" and set only `estimation_issue`.
- **Status:** fixed in 01e3094 (2026-10-26)

#### STA-08 — Self-heal fills missing columns with the wrong types
- **Location:** R/validation.R:34-55
- **Type:** inconsistency · **Severity:** cosmetic · **Verified:** confirmed; partly already recorded
- **Evidence:** healed `completion_time` becomes character, `step_number` double (FUNCTIONS.md claims
  each column gets a default of its own type). The missing action/phase/created_by_search columns are
  already recorded (FUNCTIONS.md:44).
- **Proposed fix:** heal from the canonical empty schema.
- **Status:** fixed in 6df29b2 (2026-10-26)

#### STA-09 — `add_covariates_to_search(tags_yaml_path = …)` writes one file and re-reads another
- **Location:** R/initialization.R:287-296, 1487-1500
- **Type:** bug · **Severity:** cosmetic (fails loudly) · **Verified:** confirmed by reading
- **Proposed fix:** `load_tags` takes the path; pass `tags_yaml_path` through. (The hardcoded path is listed in FUNCTIONS.md:81.)
- **Status:** fixed in 6df29b2 (2026-10-26)

#### STA-10 — `read_all_model_files` re-reads the listing and carries dead branches
- **Location:** R/validation.R:92-170
- **Type:** simplification · **Severity:** cosmetic · **Verified:** confirmed by reading
- **Evidence:** second listing read; its `read_error` branch only fires on a race; an inner
  `if (!ext_results$found)` is unreachable. Fold together with NIO-05.
- **Status:** fixed in 6df29b2 (2026-10-26)

### Area 2 — Model editing
Reviewed by Fable (`D:\R\tmp\review-MOD\`), re-checked by Opus (`D:\R\tmp\verify-MOD\`); the real
writer, remover and `bbr` were driven, nothing under test mocked. Three proposed fixes were corrected
by the checker — the corrected versions are the ones below.

#### MOD-01 — `model_add_cov` can bind a new covariate to an existing THETA
- **Location:** R/model-modification.R:342-353
- **Type:** bug · **Severity:** result · **Verified:** confirmed (both)
- **Evidence:** the next THETA number comes from the **last** `THETA(n)` on each line (greedy `.*`).
  With `W = SQRT(THETA(5)**2 + THETA(4)**2*IPRED**2)` (additive SD numbered highest, written first) it
  wrote `TV_CL = THETA(1) * (WT/70)**THETA(5)` — the covariate exponent is the residual SD — plus an
  unreferenced sixth THETA. Silent; the model runs. Also: a commented-out `;TV_Q = THETA(6)` gives
  THETA(7) with 6 entries (NM-TRAN error); an unreferenced trailing entry collides silently. Models the
  search builds itself are safe (it appends in order); hand-built base models are exposed.
- **Proposed fix (corrected):** `newtheta <- max(per-value $THETA entry count, max THETA(n) on comment-stripped lines) + 1`.
  Counting `$THETA` lines alone is wrong: `$THETA (0,2.5) (0,30) (0,1)` is 3 THETAs on one line (see MOD-12).
- **Must keep:** insertion at the end of the last `$THETA` record; `$THETAP` skipped; "no THETA ⇒ 1".
- **Status:** fixed in fd22066 (2026-10-26)

#### MOD-02 — Parameters with IOV (or an ETA scaled by a THETA) are treated as log scale, so the covariate is added to a natural-scale TV
- **Location:** R/covariate-formula.R:171-196; R/initialization.R:905-948
- **Type:** bug · **Severity:** result · **Verified:** confirmed (both)
- **Evidence:** `TV_CL = THETA(1) / CL = TV_CL * EXP(ETA(1) + IOVCL)` → classified "log", validation
  passes, writer produced `TV_CL = THETA(1) + THETA(4)*LOG(WT/70)`. Same for `EXP(ETA(1)*THETA(9))`.
  IOV in this form is a common popPK layout.
- **Proposed fix (corrected):** if `TV_<param>` or `THETA(` sits **outside** the EXP ⇒ "normal". Inside
  the EXP, "log" when the residue **contains** `TV_<param>` or a MU symbol; "unknown" only when it
  contains neither. (The reviewer's exact-match rule would wrongly reject log-scale IOV
  `EXP(TV_CL + ETA(1) + IOVCL)`, MU+IOV and `EXP(TV_CL + ETA(1)*THETA(9))`.) All shapes in FUNCTIONS.md
  keep their classification. Separate edge case, not fixed by either: `EXP(LOG(TV_CL)+ETA)` reads "log".
- **Must keep:** "normal" without ETA; "unknown" when ETA is not inside EXP; the MU tag override.
- **Status:** fixed in 6c8365b (2026-10-26); audit follow-up in f670f39 (2026-10-26)

#### MOD-03 — `add_covariate_to_model` loses its error log and reports "'con' is not a connection"
- **Location:** R/model-modification.R:155-161 (also 80, 228); consumer R/scm-execution.R:147
- **Type:** bug · **Severity:** failure (diagnostics lost) · **Verified:** confirmed (both)
- **Evidence:** `log_filename <<-` inside the tryCatch expression skips the local binding and creates
  a global; `writeLines(…, NULL)` then throws. The stored failure reason is the misleading message, no
  error log is written, `log_filename` appears in the user's global environment. The half-created
  `run<N>.ctl/.yaml` stays on disk without a database row (as known limitation 2).
- **Proposed fix:** plain `log_filename <- file.path(...)`.
- **Status:** fixed in 12c3cf9 (2026-10-26)

#### MOD-04 — Removal renumbers while a removed THETA is still referenced elsewhere
- **Location:** R/model-modification.R:1298-1328, 1387-1389
- **Type:** bug · **Severity:** result (hand-edited models only) · **Verified:** confirmed (both)
- **Evidence:** if a user reuses a covariate THETA in another equation with it first on the line
  (`Q = (WT/70)**THETA(5) * THETA(3)`), removing the covariate reports success and renumbers Q's
  exponent onto the proportional-error THETA. The search itself never shares a THETA. For categorical
  covariates the existing guard can never fire.
- **Proposed fix (corrected placement):** after Steps 6/7 and before `fix_theta_renumbering`, stop if
  any removed THETA is still referenced on a comment-stripped line. Not right after Step 5 — additive
  categorical IF lines still hold the THETA there.
- **Must keep:** the three recorded known limitations of the guard.
- **Status:** fixed in 2bcf8c9 (2026-10-26)

#### MOD-05 — NA in a categorical column writes `ELSEIF(SEX.EQ.NA)` with an extra THETA (root shared with STA-05)
- **Location:** R/model-modification.R:416-417, 508-513
- **Type:** bug · **Severity:** failure (NM-TRAN error) · **Verified:** confirmed (both)
- **Proposed fix:** drop NA before counting levels; one shared time-varying helper (see STA-05).
- **Status:** fixed in 2bcf8c9 (2026-10-26)

#### MOD-06 — `$PRED` models get no categorical `IF` block
- **Location:** R/model-modification.R:548; removal Step 6 (~1332)
- **Type:** bug · **Severity:** failure (silent at write time) · **Verified:** confirmed (both)
- **Evidence:** the block is appended after the `$PK` record, which a `$PRED` model does not have; the
  model gets `TV_CL = THETA(1) * beta_SEX_CL` with `beta_SEX_CL` never defined. README:147 treats
  `$PRED` as supported.
- **Proposed fix:** match `$PK` or `$PRED` (with optional leading blanks), stop if neither; same in removal Step 6.
- **Status:** fixed in 54e55ae (2026-10-26)

#### MOD-07 — Categorical covariate: unused THETA when no non-reference level is observed; df from declared vs observed levels
- **Location:** R/model-modification.R:513-517, 617-642; R/utilities.R:554-567; R/initialization.R:658
- **Type:** bug / inconsistency · **Severity:** result · **Verified:** confirmed (both)
- **Evidence:** data with SEX = 0 only and LEVELS "0;1" writes an unreferenced `beta_SEX_CL` THETA at
  df 1. Hand-written LEVELS "1;2;3" with only 1, 2 observed writes one THETA but tests at df 2 (too
  strict). `build_covariate_reference_table` sets LEVELS from the data, so this needs hand-written LEVELS.
- **Proposed fix:** stop when no non-reference level is observed; require declared LEVELS == observed,
  or count df from observed levels.
- **Status:** fixed in 2bcf8c9 (2026-10-26); audit follow-up in f670f39 (2026-10-26)

#### MOD-08 — A character REFERENCE is written as a quoted string in user expressions (`WT/"75"`)
- **Location:** R/covariate-formula.R:254; R/initialization.R:512, 1461-1465
- **Type:** bug · **Severity:** result (that covariate's models fail, so it is never selected) · **Verified:** confirmed end to end
- **Evidence:** `build_covariate_reference_table` returns REFERENCE as character; passed in memory to
  `initialize_covariate_search`, the writer produced `TV_CL = THETA(1) * (1 + THETA(2) * LOG(WT/"75"))`.
  Reading the table from CSV makes it numeric (the vignette path works). `add_covariates_to_search`
  also coerces the column to character.
- **Proposed fix:** `as.numeric(ref)`; stop on NA only when the expression uses `ref`.
- **Status:** fixed in f16b579 (2026-10-26)

#### MOD-09 — STATUS case handled inconsistently ("CAT" leaves a dead IF block after removal)
- **Location:** R/model-modification.R:1331; R/initialization.R:615, 376
- **Type:** inconsistency · **Severity:** cosmetic · **Verified:** confirmed (both)
- **Proposed fix:** normalise STATUS in validation; removal dispatches on the registry's `categorical` flag.
- **Status:** fixed in f16b579 (2026-10-26)

#### MOD-10 — The remover fails on an indented ` $THETA` record that the writer accepts
- **Location:** R/model-modification.R:1228, 1232, 1332
- **Type:** inconsistency · **Severity:** failure (loud) · **Verified:** confirmed (both)
- **Proposed fix:** use the leading-blank-tolerant record patterns in the remover, as the writer does.
- **Status:** fixed in 54e55ae (2026-10-26)

#### MOD-11 — Dead code in model-modification.R
- **Location:** R/model-modification.R:503, 461-463, 228; FUNCTIONS.md's "formula block duplicated verbatim" note is stale
- **Type:** simplification · **Severity:** cosmetic · **Verified:** confirmed (both)
- **Status:** open — low priority

#### MOD-12 — Removal counts one THETA per line, so lines holding several values are undercounted (found by the checker)
- **Location:** R/model-modification.R, `remove_covariate_from_model` Step 4 (THETA counting)
- **Type:** bug · **Severity:** result (old-style multi-value `$THETA` lines only) · **Verified:** plausible (checker, by reading and counting)
- **Evidence:** `$THETA (0,2.5) (0,30) (0,1)` / `0.1 0.5` is 5 THETAs; Step 4 counts 2, so a covariate
  after them is numbered wrongly. Predates the 2026-10-25 multi-record fix (which kept the
  one-value-per-line rule). The writer currently gets this layout right.
- **Proposed fix:** count values per line (each `(…)` group or bare number, ignoring `FIX`), with one
  helper shared with MOD-01.
- **Status:** fixed in fd22066 (2026-10-26)

### Area 5 — Recovery and resume
Reviewed by Fable (`D:\R\tmp\review-REC\`), re-checked by Opus (`D:\R\tmp\verify-REC\`).

#### REC-01 — Resuming after a crash before submission marks the step's models failed and skips them
- **Location:** R/continue-search.R:256-285, 308-342; R/validation.R:99-103
- **Type:** bug · **Severity:** result · **Verified:** confirmed (checker re-ran it without the reviewer's mocks)
- **Evidence:** `continue_search` force re-reads every last-step row; a model with no run directory
  reads "failed — NONMEM never started". The pending guard does not block on "failed", so the step
  counts as finished with nothing significant and the search goes straight to backward elimination;
  forward is never re-entered. The covariates of that step are never tested. The recovery article
  (recovery-system.Rmd, "checkpoint from the start of a step") tells users to resume from
  `NN_forward_created.rds` and promises `incomplete_step` when a model has no output — false here.
  **Triggers (checker's correction):** a crash between the `_created` checkpoint and submission, or
  in the middle of the submit loop. Not "all submissions failed": the live loop also stops there.
- **Proposed fix:** snapshot statuses before the re-read; a search-created row (`.is_search_model`)
  with a non-terminal pre-read status and no run directory is "not started" — report it in
  `incomplete_step`, or submit it when `auto_submit`. A non-search row must not be treated this way
  (it would block forever).
- **Must keep:** a model whose run directory exists and failed stays failed; a submitted job with a
  run directory but no output already reads `in_progress` and blocks.
- **Status:** fixed in d791a20 (2026-10-26)

#### REC-02 — One stray `run<N>` folder without a `.yaml` makes every resume crash
- **Location:** R/resume-reconstruct.R:20-23, 155
- **Type:** bug · **Severity:** failure · **Verified:** confirmed (both)
- **Evidence:** an empty `run9/` → "cannot open the connection" from `reconstruct_step_from_disk` and
  `continue_search` (unguarded `yaml::read_yaml`; unregistered folders are found by name only).
- **Proposed fix:** only consider folders with a `.yaml` (or `.ctl`/`.mod`) beside them, or catch the
  read and list the folder as skipped.
- **Status:** fixed in 1997c00 (2026-10-26)

#### REC-03 — Resume never recovers or creates retries
- **Location:** R/resume-reconstruct.R:21; R/continue-search.R:256-343
- **Type:** bug · **Severity:** result · **Verified:** plausible (both mechanisms confirmed in code; impact not run end to end)
- **Evidence:** (a) `run<N>001` folders are ignored by the heal, and the monitoring checkpoint is
  only written every 5th cycle, so a retry can exist on disk but in no checkpoint — its result is
  never read and the original counts as failed. (b) On resume a last-step model that failed for an
  estimation reason is never sent through `process_estimation_issues`; the live loop would retry it.
- **Proposed fix:** register `run<N>001` folders whose `run<N>` is in the database (never one whose
  original is missing); when `auto_retry`, retry eligible failed last-step rows before evaluating.
- **Must keep:** retries out of the model counter; removal models never retried.
- **Status:** fixed in d791a20 (2026-10-26)

#### REC-04 — Excluding a covariate after a failed retry never takes effect (tag names compared with tag values)
- **Location:** R/scm-algorithm.R:45-61, 142-144; R/scm-execution.R:85; producer R/recovery-actions.R:709-720
- **Type:** bug · **Severity:** cosmetic today · **Verified:** confirmed with real code (both)
- **Evidence:** exclusion rows carry the tag **name** (`beta_WT_CL`); `get_remaining_covariates`
  tests tag **values** (`WT_CL`) against them, so nothing is excluded. Built with the real
  `generate_tags_from_covariate_search`, `add_covariate_to_model`, `create_retry_model` and
  `handle_failed_retry`: `get_excluded_covariates()` → `beta_WT_CL`;
  `get_remaining_covariates(include_excluded = FALSE)` still returns `beta_WT_CL`. No test covers either function.
- **Why it changes nothing today (checker):** selective forward only tests covariates from the
  previous step's significant models, and a covariate whose model and retry both failed can never be
  significant — it is deferred to redemption by that narrowing, not by the exclusion filter.
- **Link to SCM-06:** SCM-06's proposed fix ("pass `include_excluded = FALSE` in standard SCM") would
  do nothing unless REC-04 is fixed first. Once fixed, an exclusion holds in every later step until
  redemption (no step filter) — a behaviour change for standard SCM if SCM-06's fix is also applied.
- **Proposed fix:** compare names with names (`setdiff(names, get_excluded_covariates())`) in both places.
- **Status:** fixed in d6cdce4 (2026-10-26)

#### REC-05 — A retry of a THETA with `INF` bounds (or init 0, or huge bounds) is not perturbed sensibly
- **Location:** R/recovery-actions.R:326-351
- **Type:** bug · **Severity:** cosmetic (wasted identical rerun, then exclusion) · **Verified:** confirmed (both)
- **Evidence:** `(-INF, 0.1, INF)`, `(0, 0.1, INF)` and init `0` give no perturbation, so an identical
  copy is submitted as the "retry". **Found by the checker:** huge finite bounds give absurd inits —
  `(0, 0.1, 1E6)` → `(0, 500000, 1E6)`. Only reachable through a user INIT column.
- **Proposed fix:** treat infinite (and very large) bounds as absent so the one-sided/unbounded rules
  apply; move a 0 init to a small value inside the bounds.
- **Must keep:** the four documented rules and test-theta-init-perturbation.R.
- **Status:** fixed in 6df29b2 (2026-10-26)

#### REC-06 — `detect_estimation_problems` and `generate_recovery_report` use states the search never writes
- **Location:** R/recovery-detection.R:44-48, 92; R/monitoring-files.R:144-154
- **Type:** inconsistency · **Severity:** cosmetic (no live caller) · **Verified:** plausible
- **Evidence:** `detect_estimation_problems` is never called and would stamp a terminal status from a
  possibly mid-run `.ext`; `generate_recovery_report` counts status "retry_failed", which is only
  ever written to `estimation_issue`.
- **Proposed fix:** retire it or gate the write on a completed `.ext`; count retries by `action == "retry"`.
- **Status:** fixed in 01e3094 (2026-10-26)

#### REC-07 — After auto-heal the model counter ignores skipped folders on disk
- **Location:** R/resume-reconstruct.R:249; R/database-management.R:299-331; R/model-modification.R:94
- **Type:** bug · **Severity:** failure (overwritten files) · **Verified:** plausible
- **Evidence:** the counter comes from the database only; a skipped `run<N>` folder numbered higher
  can be overwritten by `copy_model_from(.overwrite = TRUE)`. Same family as STA-03 / SCM-04.
- **Proposed fix:** raise the counter to the highest non-retry `run<N>` on disk, or warn.
- **Status:** fixed in 043bec2 (2026-10-26)

### Area 6 — Reporting and diagnostics
Reviewed by Fable (`D:\R\tmp\review-REP\`), re-checked by Opus (`D:\R\tmp\verify-REP\`). The
checker confirmed the reviewer's `bbr` mocks match the shape of bbr's real captured summary.

#### REP-04 — A lowercase `; log` tag lets a high-RSE parameter pass the search's RSE limit
- **Location:** R/model-output-tables.R:402-403, 628-629; R/plot_nonmem_iterations.R:185; vs R/validation.R:982
- **Type:** bug · **Severity:** result (affects the search, not just the report) · **Verified:** confirmed (both)
- **Evidence:** validation and the writer upper-case the transform; `.to_report_scale` and the RSE
  formula compare against exact "LOG"/"RATIO", so "log" falls through. **Found by the checker:** a THETA
  tagged "log" with RSE 150% gives `get_model_max_rse` = 11.6 (tagged "LOG" → 150), so it silently
  passes the RSE acceptance test. In the report the row is blank.
- **Proposed fix:** `toupper(trimws(...))` once in `extract_params`.
- **Status:** fixed in bef931c (2026-10-26)

#### REP-10 — A negative off-diagonal RSE escapes the RSE limit
- **Location:** R/model-output-tables.R:631 vs 628
- **Type:** bug · **Severity:** result (models with `$OMEGA BLOCK`) · **Verified:** confirmed by reading (checker)
- **Evidence:** off-diagonal RSE is `100*SE/correlation` without `abs()`, so a negative correlation gives a
  negative RSE that never becomes the maximum the RSE test reads.
- **Proposed fix:** `abs(100 * stderr / estimate)`.
- **Status:** fixed in a1e2139 (2026-10-26)

#### REP-01 — The "Conditional number" in the parameter table is not NONMEM's condition number
- **Location:** R/model-output-tables.R:195-199, 248-312; labels at 584/674/686/827
- **Type:** bug · **Severity:** result (published value; selection not affected) · **Verified:** confirmed (both)
- **Evidence:** NONMEM writes standard errors on the `.cor` diagonal; the code never resets it to 1.
  bbr example complex/1001: package 10,013 vs NONMEM's printed 4,353 (unit-diagonal eigenvalues match
  NONMEM's one for one); basic/1: 122.6 vs 605.5. **Also (checker):** with SEs on the diagonal the matrix
  is indefinite, and the negative eigenvalues are silently dropped. The `.cov` fallback is scale-dependent.
- **Proposed fix:** `diag <- 1` after dropping FIX rows; `cov2cor` for the `.cov` fallback. NONMEM's own
  value (`PRINT=E`) is often absent, so it cannot be the only route. Fix the label ("condition number").
- **Must keep:** last-TABLE selection; FIX rows removed; `Inf` for a singular matrix; `NA` when unusable.
- **Status:** fixed in 6cfd760 (2026-10-26)

#### REP-03 — Estimates are rounded to 2 decimals, so small THETAs print as 0
- **Location:** R/model-output-tables.R:818; also `.join_bootstrap_results` (boot median)
- **Type:** bug · **Severity:** result (published values) · **Verified:** confirmed (both)
- **Evidence:** THETA 0.0045 prints "0" next to CI [0.00274, 0.00626]; -0.082 prints -0.08.
- **Proposed fix:** 3 significant figures for estimates and the boot median; format OFV and the
  condition number separately; leave RSE rounding alone (it feeds the RSE test).
- **Status:** fixed in 6cfd760 (2026-10-26)

#### REP-02 — The printed results summary names the wrong final model when several removals are acceptable
- **Location:** R/scm-results.R:142-146, 414, 424, 469-471
- **Type:** inconsistency · **Severity:** result (console summary only) · **Verified:** confirmed (both)
- **Evidence:** the engine removes only the cheapest acceptable removal; the table labels all of them
  REMOVED and the summary takes the last one (run6 instead of run5). The step line has the same
  model-number-order bug, and the error carries into later steps.
- **Proposed fix:** pick the smallest ΔOFV in the last backward step **that has an acceptable removal**,
  or relabel (needs a new label for "acceptable, not chosen").
- **Status:** fixed in 6cfd760 (2026-10-26)

#### REP-06 — `model_report` drops a model on any warning, such as a missing final newline
- **Location:** R/model-output-tables.R:160, 964-968
- **Type:** bug · **Severity:** failure (announced) · **Verified:** confirmed (both)
- **Evidence:** a `.ctl` without a trailing newline is excluded; with `bootstrap = TRUE` a single warning
  ends in "all models failed".
- **Proposed fix:** `warn = FALSE` in the shared control-stream reader (with NIO-01); log warnings and keep the model.
- **Status:** fixed in a08a8cb (2026-10-26)

#### REP-07 — The bootstrap interval column is chosen by the substring "ci"
- **Location:** R/model-output-tables.R:481-482
- **Type:** bug · **Severity:** result in an edge case (wrong interval printed silently) · **Verified:** confirmed (both)
- **Evidence:** a `precision` column is taken as the interval ("0.1"). The contract documents "first column containing CI".
- **Proposed fix:** prefer an anchored match; build from a lower/upper pair; error when nothing parses.
- **Must keep:** "95% CI-bootstrap" (tested); case-insensitive matching.
- **Status:** fixed in 6cfd760 (2026-10-26)

#### REP-14 — FIX parameters get an absurd asymptotic CI in the bootstrap report (found by the checker)
- **Location:** R/model-output-tables.R (`get_param2(include_ci = TRUE)`, comment at 632-635)
- **Type:** bug · **Severity:** cosmetic (bootstrap report only) · **Verified:** confirmed on a real bbr summary
- **Evidence:** real bbr reports `stderr = 1e10` for FIX elements (the reviewer's mock used NA), so a FIX
  RATIO THETA prints CI "[-19600000000, 19600000000]" and a FIX LOG THETA "[0, Inf]". Contradicts the code
  comment and FUNCTIONS.md ("FIX … leaves it NA").
- **Proposed fix:** treat `stderr >= 1e10` (or the FIX flag) as no SE.
- **Status:** fixed in 1b5d7f2 (2026-10-26)

#### REP-05 — Every OMEGA/SIGMA diagonal is shown as lognormal CV%, and the table does not say so
- **Location:** R/model-output-tables.R:404, 630
- **Type:** documentation request · **Severity:** cosmetic · **Verified:** DELIBERATE (recorded in FUNCTIONS.md:35 and the `.to_report_scale` row)
- **Evidence:** the formula `100*sqrt(exp(v)-1)` for every diagonal is a recorded choice; for an additive
  SIGMA the "CV%" is not meaningful, and nothing in the output states the formula.
- **Proposed fix:** a footer stating the formula. Honouring an SD/ADD tag would also need a validator change (only RATIO/LOG allowed).
- **Status:** fixed in 6cfd760 (2026-10-26)

#### REP-08 — Without `spec_pk` the parameter table has no section headers
- **Location:** R/model-output-tables.R:603-607, 792-800 · **Type:** bug · **Severity:** cosmetic · **Verified:** confirmed (both)
- **Proposed fix:** derive the section from the NONMEM name; drop the empty header.
- **Status:** fixed in 6cfd760 (2026-10-26)

#### REP-09 — A step that contains a retry is headed "REDEMPTION/FINAL TESTING"
- **Location:** R/reporting.R:75-79 · **Type:** bug · **Severity:** cosmetic · **Verified:** confirmed (both)
- **Note:** after dropping "retry" from the pattern the redemption label is dead (redemption rows are
  written as forward_selection — SCM-03).
- **Status:** fixed in 6cfd760 (2026-10-26)

#### REP-11 — Shrinkage selection evaluates every field
- **Location:** R/model-output-tables.R:557-574 · **Type:** bug · **Severity:** cosmetic · **Verified:** plausible; already noted as fragile in FUNCTIONS.md
- **Evidence:** bbr's real example has all six fields, so it does not bite there.
- **Status:** fixed in 6cfd760 (2026-10-26)

#### REP-12 — `$OMEGA BLOCK(1) SAME` breaks `get_param2` for IOV models
- **Location:** R/model-output-tables.R:64-141, 600-613 · **Type:** bug · **Severity:** failure (outside the SCM flow) · **Verified:** plausible (parser confirmed)
- **Status:** rejected — owner 2026-10-27: models must name every parameter for proper table output, so a BLOCK(1) SAME line without its own name is not a supported input

#### REP-13 — Dead code in the reporting files
- **Location:** R/scm-results.R:22-23, 86, 202, 209; R/model-output-tables.R:599; eps shrinkage · **Type:** simplification · **Severity:** cosmetic · **Verified:** confirmed
- **Note:** the local `%||%` in reporting.R is already recorded in FUNCTIONS.md.
- **Status:** fixed in 6cfd760 (2026-10-26)

### Area 8 — Shared utilities
Reviewed by Fable (`D:\R\tmp\review-UTL\`), re-checked by Opus (`D:\R\tmp\verify-UTL\`).

#### UTL-01 — Degrees of freedom are looked up by covariate name only, so one covariate on two parameters gets one df
- **Location:** R/utilities.R:533-542; callers R/scm-evaluation.R:90, R/validation.R:648
- **Type:** bug · **Severity:** result · **Verified:** confirmed through the real evaluator (both)
- **Evidence:** both callers pass only "WT" and the first matching row wins. Table WT/CL `INIT = "0.75 FIX"`,
  WT/V `INIT = "0.1"` (passes validation): both get df 0 and threshold 0, so `select_best_model` accepted
  the estimated `beta_WT_V` on ΔOFV 1 ("threshold=0.00 for df=0 … Best model selected: run3"); in backward
  its removal is only accepted at ΔOFV < 0. Rows swapped: both get df 1, so the FIX `beta_WT_CL` is tested
  at 3.84. Same for a categorical with different FORMULA per parameter (RACE power on CL, linear on V →
  df 1 for both; V should be 2). WT fixed on CL and estimated on V is a common pattern.
- **Proposed fix:** optional `tag` argument (last, the function is exported): select by
  `cov_to_test == tag` (as `model_add_cov` does), then COVARIATE + PARAMETER (needed for per-level tags
  like `beta_RACE_CL_2`), then COVARIATE alone (test fixtures; discovery's non-tag values). Pass the tag
  from both callers; SCM-09's fix must pass it too.
- **Status:** fixed in f0dd2bc (2026-10-26)

#### UTL-03 — The dataset is read without treating "." as missing
- **Location:** R/utilities.R:124; data call sites R/initialization.R:80, R/model-modification.R:872
- **Type:** bug · **Severity:** failure for categorical covariates; same as STA-05 for continuous ones · **Verified:** confirmed (both)
- **Evidence:** `readr::read_csv` defaults treat only "" and "NA" as missing; "." (NONMEM's usual
  marker, common for intermittent lab covariates such as eGFR) turns the column into text. A
  subject-constant covariate is then classed time-varying (placement as STA-05); a categorical stops at
  the level check. STA-05's "ignore NA" fix does not catch "." strings.
- **Proposed fix:** `na = c("", "NA", ".")` at the dataset call sites only (not for every table `.load_if_path` reads).
- **Status:** fixed in 2bcf8c9 (2026-10-26)

#### UTL-06 — A non-numeric `rse_threshold` is silently replaced by 50% (missing validation at the entry points)
- **Location:** R/full_scm_search.R:99, 118; R/continue-search.R:227; R/utilities.R:449-453
- **Type:** bug · **Severity:** result (off-contract input) · **Verified:** confirmed (checker); the helper's fallback is DELIBERATE (FUNCTIONS.md:405)
- **Evidence:** only `run_automated_scm_testing` checks `rse_threshold`, and it compares strings for a
  string: "30" stops with a misleading range message; "10", "1", "100" pass and silently become 50, which
  is then written into `search_config`. `continue_search`, `run_scm_selective_forward`,
  `run_backward_elimination`, `evaluate_removal_impacts` and `select_best_model` don't check at all.
- **Proposed fix:** an `is.numeric(rse_threshold) && length 1` check at each exported entry point; leave the helper as designed.
- **Status:** fixed in 043bec2 (2026-10-26)

#### UTL-02 — `.is_removal_model` resolves a retry's direction one level deep and from the retry's own phase
- **Location:** R/utilities.R:313-327
- **Type:** hardening · **Severity:** cosmetic (checker: unreachable from package-written state) · **Verified:** confirmed as code behaviour
- **Evidence:** wrong sign only for a database without an `action` column (no package version ever wrote
  one; `create_retry_model` would fail first) or a retry of a retry (only via a direct
  `create_retry_model` call). Removal retries are already suppressed.
- **Proposed fix:** resolve retries in a bounded loop; fall back to the original's phase.
- **Status:** fixed in 6df29b2 (2026-10-26)

#### UTL-04 — `globalVariables` is out of sync (R CMD check NOTEs)
- **Location:** R/CovariateSearchr.R:46-76; R/covariate-boxplots.R:394-424; R/forest-plots.R:1095-1196
- **Type:** inconsistency · **Severity:** cosmetic · **Verified:** confirmed (checker, with a corrected script)
- **Evidence:** 13 undeclared NSE names (band, COLOR, facet_grp, Freq, NAME, Scenario, VALUE, xlev, xmax,
  xmin, y, ymax, ymin); 3 declared names used nowhere (runtime_minutes, get_model_ofv, best_delta_ofv).
- **Status:** open — low priority

#### UTL-05 — Exported helpers fail cryptically on off-contract input
- **Location:** R/utilities.R:65, 493 · **Type:** bug · **Severity:** cosmetic · **Verified:** confirmed (both)
- **Evidence:** a length-2 tag errors; a factor tag returns NA; NA p or df → "missing value where
  TRUE/FALSE needed". Package callers catch these and fall back to df 1.
- **Status:** fixed in 6df29b2 (2026-10-26)

#### UTL-07 — `generate_tags_from_covariate_search` has its own CSV-only loader
- **Location:** R/initialization.R:1137-1153 · **Type:** simplification · **Severity:** cosmetic · **Verified:** confirmed (checker)
- **Evidence:** an `.rds` table errors with "Missing required columns". Switching to `.load_if_path`
  would reject extensions other than .csv/.rds, which the current loader accepts as CSV — mention it.
- **Status:** fixed in 6df29b2 (2026-10-26)

### Area 7 — Simulation and exposure plots
Reviewed by Fable (`D:\R\tmp\review-SIM\`), re-checked by Opus (`D:\R\tmp\verify-SIM\`); no mocks.
mrgsolve could not compile on this machine, so `simulate_scenario_profiles` was checked by reading only.

#### SIM-12 — `build_scenario_parameters` crashes when the model has exactly one covariate effect (found by the checker)
- **Location:** R/apply-covariate-model.R:89 (`unlist(covariates[1, , drop = TRUE])`); reached from R/build-scenario-parameters.R
- **Type:** bug · **Severity:** failure · **Verified:** confirmed (WT-only model)
- **Evidence:** `create_covariate_table` keeps only covariates with a `beta_` THETA, so a one-covariate
  model gives a one-column scenario table; `unlist(df[1, , drop = TRUE])` on one column returns an
  unnamed number, and `apply_covariate_model` stops with "`covariates` must be named". Introduced in
  commit 41beeb2 (per-row covariates); the tests always use two covariates or a named vector. A final
  model with a single covariate is common, so the forest-plot pipeline fails for it.
- **Proposed fix:** `unlist(covariates[1, , drop = FALSE])` (or `as.list` first); add a one-covariate test.
- **Status:** fixed in 24eade5 (2026-10-26)

#### SIM-01 — MU-referenced `$PK` without a `TV_` line returns log-scale values as the parameters, silently
- **Location:** R/apply-covariate-model.R:394-419; R/forest-plots.R:826-838 (`.structural_param_names`)
- **Type:** bug · **Severity:** result · **Verified:** confirmed end to end (both)
- **Evidence:** `MU_1 = THETA(1) + …; CL = EXP(MU_1 + ETA(1))` → CL returned as -0.693 instead of 0.5,
  labelled `MU_1`, no warning; per scenario -0.69/-1.04/-0.31 instead of 0.5/0.35/0.73. The documented
  form (`TV_CL = …; MU_1 = LOG(TV_CL); CL = EXP(MU_1+ETA)`, covariate-formulas.Rmd) works. Reachable from
  search-built models: parameters without a covariate (V, KA) are never validated, so a TV-less MU form
  passes and reaches mrgsolve on the log scale. Also `TVV = THETA(2); V2 = EXP(TVV+ETA(2))` falls back to
  the raw log draw (with a warning) while the label says "V".
- **Proposed fix (corrected by the checker):** after finding the defining line, follow it to the first
  later assignment that uses that variable — triggered when the name does not exist **or matches
  `^MU_[0-9]+$`** (the reviewer's trigger never fires for MU). Skip self-references, stop after the first
  hop to an existing name (do not recurse into `K = CL/V`), ignore non-identifier left sides. In
  `.structural_param_names` use a syntactic test (no evaluation environment there). Checked on 10 shapes:
  TV_CL, TVCL, IF-reassigned TV, categorical, multi-theta and plain forms stay byte-identical.
- **Status:** fixed in ba07a7a (2026-10-26)

#### SIM-03 — A single-line `IF` with nested parentheses is silently skipped
- **Location:** R/apply-covariate-model.R:374-382
- **Type:** bug · **Severity:** result (hand-written models) · **Verified:** confirmed (both)
- **Evidence:** `IF (SEXN.EQ.2.AND.(WT.GT.50)) TVCL = TVCL + 0.5` (SEXN 2, WT 70) → CL 0.5, expected
  0.824, no warning; also `IF (LOG(WT).GT.4) TVCL = …`. The block form works.
- **Proposed fix:** find the matching parenthesis by depth for the single-line form, and give failing
  conditions their own warning (adding them to `skipped` would show nothing). Note: an IF on a covariate
  missing from the search table (silently FALSE today) would then warn too — a stated behaviour change.
- **Status:** fixed in ba07a7a (2026-10-26)

#### SIM-04 — `plot_parameter_forests` ignores `reference` in the plot while the tables use it
- **Location:** R/forest-plots.R:1361, 1477-1489; roxygen ~1333
- **Type:** inconsistency · **Severity:** result (only when `reference` is not "Typical subject") · **Verified:** confirmed (both)
- **Evidence:** with `reference = "SEXN: 2"` the plot is normalised to Typical subject (1.00 / 1.65) while
  the relative table uses SEXN: 2.
- **Proposed fix:** pass `reference = reference`; correct the roxygen. Side effects: a mistyped
  reference will now stop; the "Typical value" colour moves to the chosen reference.
- **Status:** fixed in f53e7ef (2026-10-26)

#### SIM-08 — A mistyped scenario file path empties the forest plot without a clear error
- **Location:** R/forest-plots.R:1040-1048 · **Type:** bug · **Severity:** failure · **Verified:** confirmed (checker)
- **Proposed fix:** stop when a length-1 `.rds`/`.csv` path does not exist (plain order vectors stay valid).
- **Status:** fixed in 0c0a509 (2026-10-26)

#### SIM-02 — `END IF` (two words) is not recognised by the `$PK` interpreter
- **Location:** R/apply-covariate-model.R:333; related R/model-modification.R ~1360
- **Type:** bug · **Severity:** result, **if** NM-TRAN accepts `END IF` · **Verified:** plausible (interpreter behaviour confirmed; NONMEM syntax unsettled — the local manuals show only `ENDIF`)
- **Evidence:** with `END IF`, scenarios not taking the branch get log-scale CL and V. The package only
  writes `ENDIF`, so only hand-written blocks are affected. The removal-side pattern is a different issue
  (only package-written blocks), but if hit it would delete the rest of `$PK`.
- **Proposed fix:** `^END\s*IF\b` (harmless either way; matches the existing `ELSE IF` acceptance).
- **Status:** fixed in ba07a7a (2026-10-26)

#### SIM-06 — A categorical covariate without LEVELS silently gets no forest scenario
- **Location:** R/forest-plots.R:515-527 · **Type:** bug · **Severity:** result (unlikely) · **Verified:** confirmed
- **Evidence:** validation normally stops on missing LEVELS, so only hand-built tables or a "CAT" STATUS (MOD-09) reach it.
- **Proposed fix:** warn, or fall back to the observed levels.
- **Status:** fixed in ba07a7a (2026-10-26)

#### SIM-09 — Lower-case `$pk` falls back to raw draws with no warning
- **Location:** R/apply-covariate-model.R:241-247, 401, 409 · **Type:** bug · **Severity:** result (rare input) · **Verified:** confirmed (both)
- **Status:** fixed by the shared case-insensitive record matcher (.record_starts, G2, 54e55ae); lower-case code inside $PK is not supported — owner 2026-10-27: models are written in upper case

#### SIM-07 — Exposure tables require a label for every numeric column
- **Location:** R/forest-plots.R:1230-1231 · **Type:** usability · **Severity:** cosmetic · **Verified:** confirmed; close to the recorded design ("every tabulated quantity must be labelled")
- **Proposed fix:** tabulate only labelled columns, if the owner agrees.
- **Status:** fixed in ba07a7a (2026-10-26)

#### SIM-05 — Percentile labels round 2.5 to "2nd" and 97.5 to "98th"
- **Location:** R/forest-plots.R:464-478 · **Type:** bug · **Severity:** cosmetic · **Verified:** confirmed (both)
- **Status:** fixed in ba07a7a (2026-10-26)

#### SIM-10 — Boxplot `n =` counts rows, not subjects; tied quartile bins keep "Qk" labels
- **Location:** R/covariate-boxplots.R:303-349 · **Type:** inconsistency · **Severity:** cosmetic · **Verified:** confirmed (both)
- **Status:** fixed in ba07a7a (2026-10-26)

#### SIM-11 — Boxplots draw every raw point several times; jitter is unseeded
- **Location:** R/covariate-boxplots.R:330-334, 385, 449-498 · **Type:** optimisation · **Severity:** cosmetic · **Verified:** plausible
- **Evidence:** mostly the documented Total panel plus one render per format; file size at 1e5 subjects; emf/png/pdf differ slightly.
- **Status:** fixed in ba07a7a (2026-10-26)


## Fix groups — findings that share a root cause
From the cross-area pass. Findings in one group should be fixed together, usually by one shared
helper that every site calls, so the copies cannot drift apart again. XAR-nn findings are listed once
their check is complete.

| Group | Shared helper | Findings |
|---|---|---|
| G1 | One `$THETA` record parser: one row per value (index, init, name, unit, transform, line, on-record-line flag) | MOD-01, MOD-12, XAR-04, SIM-01 (position lookup), REP-12 (OMEGA `SAME`); also `.listing_column_meta`, `adjust_theta_for_covariate` |
| G2 | One record-header matcher: `^\s*\$TAG(?![A-Za-z])`, case-insensitive; record end `^\s*\$` | MOD-06, MOD-10, SIM-09, XAR-03 |
| G3 | One `;`-annotation reader: 4th field, upper-cased, same name matching everywhere | REP-04, XAR-01; MOD-02 touches the same `detect_param_transform` |
| G4 | One direction predicate (`.scm_backward_rows` / `.is_removal_model`) everywhere | XAR-02, UTL-02, REP-09, REP-13, REP-02; SCM-03 needs a redemption `phase` value in the same registry |
| G5 | One `require_cov_step` accessor plus validation at initialization | SCM-08, XAR-05; NIO-02 / STA-02 share the two run-success readers |
| G6 | One tag / name / row resolver | REC-04, SCM-07, SCM-10, UTL-01 (pass the tag, not the covariate name), XAR-07, XAR-10 |
| G7 | One `.covariate_is_time_varying(data, cov, id)` that ignores NA and "." | STA-05, MOD-05, UTL-03; XAR-09 (fix the article) |
| G8 | Normalise the covariate table once in `validate_covariate_search_table` (lower-case STATUS, numeric REFERENCE, FORMULA must be built-in or use `cov`) | STA-01, MOD-08, MOD-09, XAR-07 |
| G9 | Status vocabulary: `.is_terminal_status` plus the display maps (database-management.R:431, scm-results.R:224, reporting.R:114, validation.R:584) | STA-07, REC-06, REC-01 |
| G10 | Model counter from disk and database | STA-03, SCM-04, REC-07 |
| G11 | Categorical level naming / decoding | MOD-07, SIM-06, XAR-08 |
| G12 | `END IF` in both readers (interpreter and remover) | SIM-02 |
| G13 | One encoding-repairing control-stream reader | NIO-01, NIO-03, REP-06 (`warn = FALSE`) |

Rules the cross-area pass compared and found consistent: the `.ext` FINAL-row readers, `.cov` file
location in both run-success readers, the OFV validity limits, the inline terminal-status sets in
scm-execution.R, the retry-name pattern, the name→tag lookups, the `action` display registry, and the
covariate formula written by `model_add_cov` vs rebuilt by `apply_covariate_model` for every built-in form.


### Cross-area pass
Reviewed by Fable (`D:\R\tmp\review-XAR\`), re-checked by Sonnet (`D:\R\tmp\verify-XAR\`).

#### XAR-01 — A `RATIO` tag followed by a comment makes `model_add_cov` add a log-scale term to a natural-scale TV (MU models)
- **Location:** R/covariate-formula.R:148-160 (`.theta_tag_for`, last `;` field) vs R/model-output-tables.R:122-131 and R/validation.R:938-989 (4th field); consumer R/model-modification.R:429-455
- **Type:** inconsistency · **Severity:** result · **Verified:** confirmed end to end (both)
- **Evidence:** MU model `TV_CL = THETA(1); MU_1 = LOG(TV_CL); CL = EXP(MU_1+ETA(1))`. With
  `2.5 ; CL ; L/h ; RATIO` the writer produces `TV_CL = THETA(1) * (WT/70)**THETA(3)`; with
  `2.5 ; CL ; L/h ; RATIO ; typical clearance` it produces `TV_CL = THETA(1) + THETA(3)*LOG(WT/70)` —
  validation passes both. Same for a name written `1_CL`.
- **Proposed fix:** one shared annotation reader that **keeps empty fields** (so `; CL ; ; RATIO` still
  works) and takes field 4, upper-cased; used by `.theta_tag_for`, `extract_params`,
  `validate_parameter_blocks`, `.listing_column_meta`. Keep the exact-name match and the comment-line skip.
- **Status:** fixed in bef931c (2026-10-26)

#### XAR-03 — An indented ` $THETA` makes every model fail the RSE test
- **Location:** R/model-output-tables.R:71, 80, 92 (`^\$`) vs R/validation.R:826, R/model-modification.R:656 (`^\s*\$`)
- **Type:** inconsistency · **Severity:** result · **Verified:** confirmed (both); bbr's own parser accepts indented records
- **Evidence:** validation passes an indented file, but `extract_params` finds 0 rows; `get_param2`
  errors, `rse_max` is stored as NA, and with the default `require_cov_step` the evaluator rejects every
  candidate ("RSE unavailable"). Every model the search builds inherits the base's indentation. (The
  remover half is MOD-10.)
- **Proposed fix:** one record-start matcher `^\s*\$TAG(?![A-Za-z])`, and the same for the block-end
  test in `extract_params` (otherwise an indented `$OMEGA` after a flush `$THETA` is swallowed).
- **Status:** fixed in 54e55ae (2026-10-26)

#### XAR-04 — Validation accepts several bounded values on one `$THETA` line, which the readers cannot handle
- **Location:** R/validation.R:991-1005 vs R/apply-covariate-model.R:133-146, R/forest-plots.R:810-819, R/model-output-tables.R:111-137
- **Type:** bug · **Severity:** result (hand-written base models) · **Verified:** confirmed (both)
- **Evidence:** `(0,2.5) (0,30) ; CL …`, `$THETA (0,2.5) (0,30)`, `$THETA 2.5 30` and `(0,2.5) 30` all
  pass; only `2.5 30` is caught. `apply_covariate_model` then returns only THETA1 — V is dropped from the
  simulations. The writer is unaffected (it numbers from referenced THETAs). Related to MOD-12 (remover).
- **Proposed fix:** count each `(…)` group as one value, check the record line too, reject more than one
  value per line — but keep allowing a single value on the record line (`$THETA 0.5 ; CL`).
- **Status:** fixed in fd22066 (2026-10-26)

#### XAR-02 — The results table scores a manual removal as a forward step
- **Location:** R/scm-results.R:103, 201 vs R/continue-search.R:11-13, R/reporting.R:50
- **Type:** inconsistency · **Severity:** result (results table) · **Verified:** confirmed (both)
- **Evidence:** a `remove_covariate_from_model` row keeps `phase = "covariate_removal"`, which the
  table's own patterns (`backward|remove`, `backward`) miss: "Forward_Step1 | run2 | 20 | BEST", while
  `generate_scm_report` calls it a rejected removal. Rows from the backward loop are relabelled and not
  affected. (The retry-of-removal case is unreachable — UTL-02.)
- **Proposed fix:** use `.scm_backward_rows()` at both sites.
- **Status:** fixed in 6cfd760 (2026-10-26)

#### XAR-05 — `require_cov_step = NA` is read four different ways
- **Location:** R/file-io.R:478; R/validation.R:239-240; R/scm-evaluation.R:117-118; R/scm-backward.R:40-45; R/initialization.R:55, 198, 222
- **Type:** inconsistency · **Severity:** failure (malformed input only) · **Verified:** confirmed (both)
- **Evidence:** no entry point rejects NA (`is.logical(NA)` is TRUE). Then: the base check says
  "completed", `update_model_status_from_files` errors (caught per model, so completed models stay
  pending), the evaluator rejects. `continue_search` and `reconstruct_step_from_disk` call it unwrapped and abort.
- **Proposed fix:** one accessor plus a `stop()` at initialization unless TRUE/FALSE; keep the evaluator's fail-closed rule.
- **Status:** fixed in 043bec2 (2026-10-26)

#### XAR-07 — A `cov_to_test` written without the `beta_` prefix passes validation, then nothing can find it
- **Location:** R/initialization.R:585-593, 1189-1194; R/model-modification.R:55-58, 333-337
- **Type:** inconsistency · **Severity:** failure (loud, off-contract table) · **Verified:** confirmed (both)
- **Evidence:** tags.yaml gets the prefix, the table does not; `add_covariate_to_model` stops at the first
  add ("No matching covariate definition found"). The docs say this column is generated, not written.
- **Proposed fix:** add the prefix in `validate_covariate_search_table`.
- **Status:** fixed in f16b579 (2026-10-26); audit follow-up in f670f39 (2026-10-26)

#### XAR-08 — Categorical THETAs named from a lookup decode (`beta_SEX_CL_MALE`) are labelled as multi-theta effects
- **Location:** R/model-modification.R:524-531 vs R/model-output-tables.R:706-722, ~750
- **Type:** inconsistency · **Severity:** cosmetic · **Verified:** confirmed by reading (both)
- **Evidence:** the report shows "Effect of SEX_MALE on CL" instead of "SEX (Male)".
- **Proposed fix:** always write the numeric level and decode when displaying, or let the reader match decode labels.
- **Status:** fixed in 6cfd760 (2026-10-26)

#### XAR-10 — `covariate_tested` holds a tag name in most rows but a tag value in discovered rows
- **Location:** R/model-discovery.R:78-89, 132; R/recovery-actions.R:173; R/model-modification.R:190, 1435
- **Type:** inconsistency · **Severity:** cosmetic · **Verified:** confirmed (both)
- **Evidence:** discovery stores `WT_CL`; `extract_covariate_name_from_tag("WT_CL")` returns NA, so such
  a row would get df 1. Whether discovered rows are ever evaluated was not settled.
- **Proposed fix:** discovery stores the tag name; one display helper for step descriptions.
- **Status:** open (group G6) — low priority

#### XAR-09 — The Quick Start article says TIME_DEPENDENT decides placement; it does not
- **Location:** vignettes/quick-start.Rmd:127
- **Type:** documentation · **Severity:** cosmetic · **Verified:** code behaviour DELIBERATE (FUNCTIONS.md:204: placement is recomputed from the data)
- **Proposed fix:** correct the article sentence.
- **Status:** fixed in 2bcf8c9 (2026-10-26)

#### XAR-06 — REJECTED: a commented-out `beta_` line is not counted by `create_covariate_table`
- **Reason:** the reviewer's test model left the real `beta_WT_CL` line active. With the actual line
  commented out, `create_covariate_table` stops with "No covariate (beta_*) THETAs found" — commented
  lines are not counted. Do not re-propose.

The cross-area pass also re-raised a string comparison of REFERENCE in `model_add_cov`
(model-modification.R:506, 513: `"1.0"` keeps the reference level as an extra THETA) — a duplicate of
MOD-08 / G8; normalising REFERENCE to numeric once covers it.
