# Fix plan for the 2026-10-25 package review

How to work through the findings in `.claude/review/findings.md` without losing any existing
behaviour. Findings are referred to by ID; the register holds the evidence, location, proposed fix
(already corrected by the checkers) and what each fix must keep. Read the register entry before
starting any item — this plan only sets the order, the grouping and the checks.

Size: **S** = one function, a few lines · **M** = one shared helper or several call sites · **L** = a
new shared helper adopted across files, or behaviour that needs an owner decision.

---

## How every fix is done

1. **Branch:** work on `fix/review-2026-10` (from `main`), one commit per item or group below.
2. **Read** the register entry and the `.claude/FUNCTIONS.md` rows for the functions touched (search
   it — do not read it whole).
3. **Failing test first.** Add a test to `tests/testthat/` that reproduces the finding and fails on the
   current code. Run it against the old code to prove it fails. Reviewers' scripts under
   `D:\R\tmp\review-<prefix>\` and checkers' under `D:\R\tmp\verify-<prefix>\` are a starting point.
4. **Fix**, following the register's corrected proposal and its *Must keep* list.
5. **Check nothing else changed:**
   - `Rscript .claude/scripts/compare-model-readers.R` — old vs new for the seven control-stream
     functions on 840 generated models. Extend its corpus when a group changes a function it covers.
   - Full test suite.
6. **Agents:** `r-pkg-qc`, then `diff-auditor` against this plan's item, then `functions-index` when a
   function's behaviour changed.
7. **Record:** set the register entry's status to `fixed in <commit>`; add the bug to *Confirmed
   bugs* in `.claude/FUNCTIONS.md`; update any article that describes the changed behaviour.

**Models:** implement on Opus; run the checks (`r-pkg-qc`, `diff-auditor`) on the models set in their
agent files.

---

## Phase 0 — Before any code change

| # | Task | Why |
|---|---|---|
| 0.1 | Owner answers the open decisions (table below) — **all answered 2026-10-27** | Phases 3–6 contain items that cannot start without them |
| 0.2 | Add characterisation tests for untested functions that fixes will touch: `get_remaining_covariates`, `get_excluded_covariates`, `print_scm_results_table`, `calculate_covariate_df` with one covariate on two parameters, `build_scenario_parameters` with one covariate | So a fix cannot silently change behaviour nobody tested |
| 0.3 | Extend `compare-model-readers.R` to also compare `create_covariate_table`, `detect_param_transform` and `get_param2`'s name/scale columns | These are touched by G1–G3 |

### Decisions needed

| Decision | Blocks | Options |
|---|---|---|
| SCM-01: step where every model failed | 3.2 | redemption retries those covariates (today) / excludes them |
| SCM-02: base that already carries covariates with `starting_phase = "forward"` | 3.9 | supported → never skip final backward / not supported → document |
| SCM-06 + REC-04: covariate whose retry failed | 3.3 | excluded until redemption in both methods / keep re-offering in standard SCM |
| NIO-02: when has a run finished | 4.6 | `Stop Time:` present (rejects runs from tools other than `nmfe`) / align the other reader instead |
| REP-05: OMEGA/SIGMA as lognormal CV% | 5.8 | add a footer stating the formula / also honour an SD/ADD tag (needs a validator change) |
| SIM-02: does NM-TRAN accept `END IF` | 6.3 | check NONMEM; the fix is harmless either way |
| SIM-07: exposure tables need a label for every numeric column | 6.6 | keep (documented design) / tabulate only labelled columns |

---

## Phase 1 — Quick, isolated, high value

Independent one-place fixes. Do these first: they remove real failures and give an early, clean run
of the process.

| # | Finding(s) | Change | Size |
|---|---|---|---|
| 1.1 | **SIM-12** | `unlist(covariates[1, , drop = FALSE])` in `apply_covariate_model`; test with a one-covariate model through `build_scenario_parameters` | S |
| 1.2 | **MOD-03** | `log_filename <- …` (not `<<-`) in `add_covariate_to_model` | S |
| 1.3 | **REP-10** | `abs()` on the off-diagonal RSE | S |
| 1.4 | **SIM-04** | pass `reference = reference` in `plot_parameter_forests`; fix its roxygen | S |
| 1.5 | **REC-02** | resume ignores `run<N>` folders without a `.yaml`/`.ctl`, lists them as skipped | S |
| 1.6 | **NIO-04** | `read_ext_iterations` skips a TABLE with no rows | S |
| 1.7 | **STA-04** | `$DATA` name check compares names without extensions (note: also accepts `.dat` vs `.csv`) | S |
| 1.8 | **SIM-08** | stop on a missing scenario `.rds`/`.csv` path | S |
| 1.9 | **REP-14** | treat `stderr >= 1e10` as no SE in the asymptotic CI | S |
| 1.10 | **STA-03** | stop when `starting_model_number` is below the highest existing model (first step of G10) | S |

---

## Phase 2 — Foundations: shared control-stream helpers

These replace several drifting copies with one helper each. Most result-level findings in areas 2, 6
and 7 are closed here. **Order matters:** each step uses the one before.

### 2.1 — G13: one control-stream reader (NIO-01, NIO-03, REP-06) · M
- New internal `.read_control_stream(path)`: `readLines(warn = FALSE)`, then repair invalid UTF-8 lines
  with `iconv(…, "CP1252", "UTF-8")` (**CP1252, not latin1**).
- Use it at every control-stream read: `read_model_file`, validation.R:803 and :1244, and the other
  sites listed under NIO-01. Make `.read_listing_lines` use the same repair (NIO-03) and update the
  FUNCTIONS.md note that describes the dead fallback.
- REP-06: `model_report` logs warnings instead of dropping the model.
- Tests: a `µ` comment in `$THETA` through validation, add and remove; a `.ctl` without a final newline.

### 2.2 — G2: one record matcher (MOD-06, MOD-10, XAR-03, SIM-09) · M
- New internal `.record_starts(lines, tag)` = `^\s*\$TAG(?![A-Za-z])`, case-insensitive, and
  `.record_end()` = next `^\s*\$` line.
- Adopt in: `model_add_cov` (incl. `$PK`/`$PRED` for the categorical block — MOD-06), the remover
  (Steps 4 and 6 — MOD-10), `extract_params` start **and** end tests (XAR-03), validation, the two
  `get_block` helpers.
- SIM-09 (lower-case `$pk`): upper-case code lines inside the `$PK` interpreter.
- Tests: indented records through validation → add → remove → `get_param2`; `$PRED` model with a
  categorical covariate; `$THETAP` still excluded.

### 2.3 — G1: one `$THETA` parser (MOD-01, MOD-12, XAR-04, SIM-01 position lookup, REP-12) · L
- New internal `.parse_theta_records(lines)`: one row per **value** (each `(…)` group or bare number;
  `FIX` ignored; a value on the record line counted; comment lines skipped), with index, init, name,
  unit, transform, line number, on-record-line flag.
- `model_add_cov`: next THETA = `max(number of values, highest THETA(n) on comment-stripped lines) + 1`
  (MOD-01; counting lines alone is wrong).
- Remover Step 4: number from the parser (MOD-12).
- `validate_parameter_blocks`: reject more than one value per line, but allow a single value on the
  record line (XAR-04).
- `apply_covariate_model`, `.structural_param_names`, `extract_params`, `.listing_column_meta`,
  `adjust_theta_for_covariate`: take names from the parser.
- REP-12: an `$OMEGA … SAME` record yields a row (same parser idea for OMEGA), or a clear error.
- Tests: residual-error THETA referenced only in `$ERROR` as the last THETA on a line; multi-value lines;
  `THETA(n)` in comments; the existing `test-theta-records.R` must still pass.

### 2.4 — G3: one `;`-annotation reader (REP-04, XAR-01; feeds MOD-02) · M
- New internal `.theta_annotation(line)`: split on `;` **keeping empty fields**, name = field 2,
  units = field 3, transform = `toupper(trimws(field 4))`.
- Use in `.theta_tag_for` (fixes XAR-01 — a trailing comment no longer hides the tag), `extract_params`
  (fixes REP-04 — lowercase `log`), `validate_parameter_blocks`, `.listing_column_meta`.
- Tests: `; log` gives the same table row and RSE as `; LOG`; a 5-field RATIO line on a MU model gives a
  multiplicative covariate.

---

## Phase 3 — Search correctness

| # | Finding(s) | Change | Size | Needs |
|---|---|---|---|---|
| 3.1 | **G6: UTL-01**, SCM-09 | `calculate_covariate_df(…, tag = NULL)`: select by `cov_to_test == tag`, then COVARIATE + PARAMETER, then COVARIATE; both callers pass the tag. SCM-09: treat df 0 as fixed in `get_fixed_covariates`, passing the tag | M | — |
| 3.2 | **SCM-01** | redemption as a new selective search from the best model: step 1 = covariates not in it and without a completed model on it (parent-model rule, replaces Scenario A/B); step 2+ = significant ones only; step numbers from the last created step | M | decided 2026-10-27 |
| 3.3 | **REC-04**, SCM-06 | add option `retest_failed` (default TRUE, in `search_config`) that gates retesting failed covariates in standard SCM steps, redemption step 1 and the SCM+ first forward step; compare tag names with names in `get_remaining_covariates` and `run_univariate_step`; standard SCM keeps `include_excluded = TRUE` (record it); first step of redemption and of SCM+ forward includes excluded covariates | S | decided 2026-10-27 |
| 3.4 | **MOD-02** | `detect_param_transform`: TV or THETA outside the EXP ⇒ normal; inside, log only if the residue contains TV/MU, else unknown | M | G3 |
| 3.5 | **G8: STA-01**, MOD-08, MOD-09, XAR-07 | normalise the covariate table in `validate_covariate_search_table`: lower-case STATUS, numeric REFERENCE, `beta_` prefix, FORMULA must be built-in or use `cov` | M | — |
| 3.6 | **G7: STA-05**, MOD-05, UTL-03, XAR-09 | one `.covariate_is_time_varying()` ignoring NA and "."; read the dataset with `na = c("", "NA", ".")` at the dataset call sites; fix the Quick Start sentence about TIME_DEPENDENT | M | — |
| 3.7 | **MOD-04** | after Steps 6/7, stop if a removed THETA is still referenced | S | G1 |
| 3.8 | **MOD-07** | stop when no non-reference level is observed; require declared LEVELS = observed (or df from observed) | S | G8 |
| 3.9 | **SCM-02** | skip final backward after an unchanged forward only if the initial backward ran or the model has no covariates (owner, 2026-10-27) | S | decided 2026-10-27 |
| 3.10 | SCM-05, SCM-07, SCM-10 | fallback block → `prev_step$base_model` (update the grep test); excluded-covariates filter on `forward_selection`, retire the dead block; drop the dead tag fallback | S | — |

---

## Phase 4 — Resume, state and robustness

| # | Finding(s) | Change | Size | Needs |
|---|---|---|---|---|
| 4.1 | **REC-01** | resume: a search-created row with no run directory and a non-terminal saved status is "not started" — report or submit it | M | — |
| 4.2 | **REC-03**, SCM-03 | resume registers `run<N>001` retries whose original is in the database and retries eligible failures; a selective resume always re-enters `run_scm_selective_forward` | M | 3.2 |
| 4.3 | **G10: SCM-04**, REC-07 | stop after an initial-backward error; raise the counter to the highest model on disk after auto-heal | S | 1.10 |
| 4.4 | **G5: XAR-05**, SCM-08 | one `require_cov_step` accessor; stop at initialization unless TRUE/FALSE; banner wording | S | — |
| 4.5 | **G9: STA-07**, REC-06 | retry path keeps status "failed" and sets only `estimation_issue`; display maps cover every status; retire or gate `detect_estimation_problems` | S | — |
| 4.6 | **NIO-02** (= STA-02) | align the two run-success readers per decision; add a test without `Stop Time:` | M | decided 2026-10-27: `Stop Time:` |
| 4.7 | UTL-06 | `is.numeric` check for `rse_threshold` at every exported entry point | S | — |
| 4.8 | REC-05, NIO-05, NIO-07, STA-06, STA-08, STA-09, STA-10, UTL-02, UTL-05, UTL-07 | smaller robustness fixes, one commit each or grouped by file | S each | — |

---

## Phase 5 — Reporting

| # | Finding(s) | Change | Size | Needs |
|---|---|---|---|---|
| 5.1 | **REP-01** | condition number from a unit-diagonal correlation matrix (`.cor` diag ← 1; `cov2cor` for `.cov`); label "Condition number" in all four places | S | — |
| 5.2 | **REP-03** | 3 significant figures for estimates and the bootstrap median; OFV and condition number formatted separately; RSE rounding unchanged | S | — |
| 5.3 | **G4: REP-02**, XAR-02, REP-09, REP-13 | results table uses `.scm_backward_rows()`; final model = smallest ΔOFV in the last backward step with an acceptable removal; drop dead thresholds; redemption heading | M | — |
| 5.4 | **REP-07** | anchored CI column match; build from a lower/upper pair; error if nothing parses | S | — |
| 5.5 | REP-08 | section headers from the NONMEM name without `spec_pk` | S | — |
| 5.6 | REP-11 | shrinkage `switch()` | S | — |
| 5.7 | XAR-08 | categorical THETA names keep the numeric level; decode at display time | S | G11 |
| 5.8 | REP-05 | per decision: footer stating the CV% formula | S | decided 2026-10-27 |

---

## Phase 6 — Simulation

| # | Finding(s) | Change | Size | Needs |
|---|---|---|---|---|
| 6.1 | **SIM-01** | validation: every MU-referenced parameter goes through a TV_<param> line; any variable inside EXP(... + ETA) is TV_<param> (inline THETA allowed) | M | decided 2026-10-27 |
| 6.2 | **SIM-03** | depth-counted single-line `IF`; warn on conditions that cannot be evaluated (state the behaviour change) | S | — |
| 6.3 | SIM-02 | validation rejects END IF (only ENDIF is NM-TRAN syntax) | S | decided 2026-10-27 |
| 6.4 | SIM-06 | forest-plot path stops on a categorical covariate with no LEVELS (validation already does) | S | decided 2026-10-27 |
| 6.5 | SIM-05, SIM-10, SIM-11 | percentile labels; subject counts and bin labels in boxplots; cap/seed the jitter layer | S each | — |
| 6.6 | SIM-07 | tabulate only labelled columns | S | decided 2026-10-27 |

---

## Phase 7 — Cleanup and documentation

- Dead code and stale notes: MOD-11, REP-13 leftovers, the stale "formula block duplicated" note in
  FUNCTIONS.md (MOD-11), the `.read_listing_lines` note (NIO-03).
- `globalVariables` (UTL-04) — run `R CMD check` afterwards.
- NIO-06, XAR-10.
- **Finish consolidating G1 / G3** (raised by the Phase 2 audit). Done in Phase 2: numbering
  (`model_add_cov`, remover Step 4, validation) and names in `apply_covariate_model` /
  `.structural_param_names` use `.parse_theta_records`; `.theta_tag_for` uses `.parse_annotation`.
  Still separate: `extract_params` (own `separate()`, one row per line — agrees with the parser
  only because validation now rejects multi-value lines), `.listing_column_meta`,
  `adjust_theta_for_covariate`, and `validate_parameter_blocks`' own field split. Also: a bound
  split across lines (`(0,` / `2.5)`) counts as no value in `.count_theta_values`.
- Articles: update any article whose behaviour changed (forest-plots for SIM-12/SIM-01, recovery-system
  for REC-01, model-reports-and-diagnostics for REP-01/03/05, quick-start for XAR-09).
- Final: full `R CMD check`, the comparison script, and a fresh `functions-index` audit.

---

## Progress

Mark items here as they land (`done <commit>`), and the register entry as `fixed in <commit>`.

| Phase | Status |
|---|---|
| 0 | not started |
| 1 | done — 10 items, commits 24eade5..c13da8f (2026-10-26) |
| 2 | done — 2.1 G13 (a08a8cb), 2.2 G2 (54e55ae; SIM-09 → 6.x), 2.3 G1 (fd22066; REP-12 → 5.x), 2.4 G3 (bef931c) |
| 3 | in progress — 3.1 UTL-01 + SCM-09 (f0dd2bc), 3.4 MOD-02 (6c8365b), 3.5 G8 (f16b579), 3.6 G7, 3.7 MOD-04, 3.8 MOD-07, 3.10 SCM-05/07/10 (2bcf8c9); Phase 3 audit (Sonnet) follow-ups f670f39; 3.2 SCM-01 + 3.3 REC-04/SCM-06 + retest_failed (d6cdce4); 3.9 SCM-02 (4c591ad). **Phase 3 complete.** |
| 4 | **complete** — 4.3 G10 SCM-04 + REC-07, 4.4 G5 XAR-05 + SCM-08, 4.7 UTL-06 (043bec2), 4.5 G9 STA-07 + REC-06 (01e3094), 4.1 REC-01 + 4.2 REC-03/SCM-03 + phase vocabulary (d791a20), 4.6 NIO-02 (304a56a), 4.8 batch (6df29b2) |
| 5 | **complete** — 5.1–5.8 (6cfd760) |
| 6 | **complete** — 6.1–6.6 (ba07a7a) |
| 7 | **complete** — c610151 |
