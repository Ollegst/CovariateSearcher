# CovariateSearcher

An R package for automated stepwise covariate modeling (SCM) against
NONMEM models via `bbr`.

## Before reporting work as done

Work is **not** reportable as complete until these have run:

1.  **`r-pkg-qc`** — roxygenise, testthat, and (when the change is
    substantial) `R CMD check`. Reports verbatim; fixes nothing.
2.  **`diff-auditor`** — audits the working diff against the approved
    plan for removed functionality, silent simplification, and scope
    creep.
3.  **`functions-index`** — only when you added, removed, renamed, or
    changed the behavior of a function in `R/`. Leaving `FUNCTIONS.md`
    stale makes the next session confidently wrong.

State each verdict in the summary. If a finding is left unaddressed, say
which and why — the decision to ship anyway is the user’s, not yours.

This applies to any turn that changed code. It does not apply to
answering questions, reading code, or planning.

The drift report is injected automatically at session start and after
compaction (see below), so at the start of any turn you already know
which `FUNCTIONS.md` rows are trustworthy. Consult it before writing a
new helper.

What each custom agent does and cannot do:
`OneDrive - AZCollaboration\claude-agents\README.md`.

## Running R

    & "C:\Users\kdgc151\AppData\Local\Programs\R\R-4.4.2\bin\Rscript.exe" -e "<expr>"

Do not redirect a native command’s stderr with `2>&1` in Windows
PowerShell — it wraps each line in an ErrorRecord and reports a false
failure.

## Generated files — never hand-edit

- `man/*.Rd` and `NAMESPACE` are produced by roxygen2. Edit the roxygen
  block in `R/`, then regenerate with
  `roxygen2::roxygenise(load_code = 'source')`. This package documents
  from source, not from a built namespace.
- `docs/` is built by CI and served from `gh-pages`; it is deliberately
  untracked.

A `PreToolUse` hook
([.claude/hooks/guard-generated-files.ps1](https://ollegst.github.io/CovariateSearcher/.claude/hooks/guard-generated-files.ps1))
blocks edits to the first group.

## FUNCTIONS.md

[FUNCTIONS.md](https://ollegst.github.io/CovariateSearcher/FUNCTIONS.md)
maps every function in `R/` — signature, purpose, side effects, and call
graph — plus hand-curated sections on cross-cutting hazards, duplicate
implementations, and known bugs.

**Read it before writing a new helper.** This package has ~130 functions
across 29 files, many internal; the most common avoidable mistake here
is reimplementing something that already exists. The hazard sections
exist because several functions have names that misdescribe what they
read.

To check whether it is current:

    & "C:\Users\kdgc151\AppData\Local\Programs\R\R-4.4.2\bin\Rscript.exe" ".claude\scripts\functions-index.R"

The `functions-index` agent applies the resulting drift report.
Everything above the `## Package scaffolding` heading is human knowledge
no parser can regenerate — do not rewrite it.

## Comments

Comments describe what the code does, never how it changed. No
`# FIXED:`, `# ENHANCED`, `(ENHANCED VERSION)`. The edit history lives
in git.

Note that `FIX` is real NONMEM syntax in control streams
(`$THETA (0, 1 FIX)`) and is unrelated.
