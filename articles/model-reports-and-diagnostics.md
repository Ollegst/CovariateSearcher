# Model Reports and Diagnostics

## Model Reports and Diagnostics

Functions for looking at a finished model.
[`model_report()`](https://ollegst.github.io/CovariateSearcher/reference/model_report.md)
builds the parameter table you publish.
[`plot_nonmem_iterations()`](https://ollegst.github.io/CovariateSearcher/reference/plot_nonmem_iterations.md)
shows how the estimation got there — the trajectory behind the numbers
in that table. When the model has been bootstrapped,
[`model_report()`](https://ollegst.github.io/CovariateSearcher/reference/model_report.md)
can report the bootstrap beside the fit, and
[`plot_bootstrap_distributions()`](https://ollegst.github.io/CovariateSearcher/reference/plot_bootstrap_distributions.md)
shows each parameter’s bootstrap distribution against the final
estimate.

They are usually read together: the table says a parameter is 121.85
with 4.8% RSE, the trajectory says whether it settled there or was still
moving when NONMEM stopped.

------------------------------------------------------------------------

### The parameter table

``` r

library(CovariateSearcher)

pk_res <- model_report(model_names = "run28")
pk_res
```

The return value is a `flextable`, so it prints in a document, an HTML
article or the Viewer pane, and can be exported without further work.

| argument | default | effect |
|----|----|----|
| `model_names` | — | one name, or several for a side-by-side comparison |
| `shrinkage` | `"etasd"` | which shrinkage NONMEM reports; see below |
| `models_folder` | `"models"` | where the models live |
| `spec_pk` | `NULL` | parameter metadata; an R object, **not** a path |
| `lookup` | `NULL` | covariate metadata, used to decode categorical levels |
| `bootstrap` | `FALSE` | report one model beside its bootstrap; see [Reporting a bootstrap](#reporting-a-bootstrap) |
| `boot_results` | `NULL` | the bootstrap summaries `bootstrap = TRUE` needs — a data frame or a `.csv`/`.rds` path |

#### Parameter names come from `spec_pk`

Parameter names never come from the `.ext` file.
[`model_report()`](https://ollegst.github.io/CovariateSearcher/reference/model_report.md)
takes them from the annotations in your control stream — the second
semicolon-separated field of each `$THETA`, `$OMEGA` and `$SIGMA` line —
so a run annotated `0.5 ; CL ; L/h ; LOG` reports a parameter called
`CL`, with or without a spec.

Without `spec_pk` that is all you get: the bare annotation name, no
unit, no description, and no section. Supply a spec and each parameter
gains a formatted name, a label and a unit:

``` r

spec_pk <- yaml::read_yaml("data/spec/pk-extend.yml")

pk_res <- model_report(
  model_names = "run28",
  spec_pk     = spec_pk
)
```

| parameter_names | label | Estimate | RSE (%) | Shrinkage (%) |
|----|----|----|----|----|
| **Typical parameters** |  |  |  |  |
| Ka (1/hr) | Absorption rate constant | 4.91 | 20.35 |  |
| CL/F (L/h) | Apparent clearance | 122 | 4.76 |  |
| Vc/F (L) | Apparent central volume | 479 | 6.29 |  |
| **Inter-individual variability** |  |  |  |  |
| KA CV% | IIV on absorption | 615 | 49.21 | 10.16 |
| CL/F CV% | IIV on clearance | 52.2 | 4.48 | 2.82 |
| **Parameter-Covariate relationships** |  |  |  |  |
| AGE~CL/F | Effect of AGE on clearance | -0.06 | 39.26 |  |
| **Residual variability** |  |  |  |  |
| prop error | Proportional error | 34.4 | 0.51 | 12.10 |

`spec_pk` must be an in-memory object — read the YAML yourself and pass
the result. A file path is not accepted. The [YAML Specification
Files](https://ollegst.github.io/CovariateSearcher/articles/yaml-specification-files.md)
article covers what belongs in that file.

Two rows are appended below the parameter sections: `OFV`, and
`Condition number` (NONMEM’s: the largest over the smallest eigenvalue
of the correlation matrix) computed from the run’s `.cor` (or `.cov`)
output.

Estimates are shown with 3 significant figures; the OFV has 2 decimals,
the condition number 1 decimal, and RSE and shrinkage 2 decimals. A
footer under the table reads “Variance estimates (OMEGA/SIGMA diagonals)
are shown as CV% = 100\*sqrt(exp(variance) - 1).”

#### Sections

Rows are grouped under headers in a fixed order:

1.  Typical parameters
2.  Inter-individual variability
3.  Correlation of random effects
4.  Parameter-Covariate relationships
5.  Residual variability

Which section a parameter lands in comes from its `comment:` field in
`spec_pk`. A parameter that is in the spec but has no `comment:`, or
whose `comment:` is not one of the five names above, is left unsectioned
and falls to the bottom of the table beside `OFV` — so keep them exact.

Covariate effects need no entry: any `beta_*` parameter goes to
“Parameter-Covariate relationships” automatically.

With no `spec_pk` at all the rows are grouped into the same sections
from NONMEM’s own parameter names (`THETA`, `OMEGA`, `SIGMA`; `beta_*`
to covariate relationships).

#### Shrinkage

| value     | reported                                |
|-----------|-----------------------------------------|
| `"etasd"` | ETA shrinkage on the SD scale (default) |
| `"etavr"` | ETA shrinkage on the variance scale     |
| `"ebvsd"` | EBV shrinkage on the SD scale           |
| `"ebvvr"` | EBV shrinkage on the variance scale     |

#### Notation in the cells

| you see | it means |
|----|----|
| `0.75 FIX` | the parameter was fixed in estimation; no RSE is reported for it |
| empty Shrinkage | not applicable — THETAs have no shrinkage |
| `NA` | the value could not be calculated, usually a failed `$COVARIANCE` step |

#### Decoding categorical levels

A categorical covariate estimates one THETA per non-reference level,
tagged `beta_SEXN_V_2`. By default the table names the level by its raw
value:

| parameter_names   | label                                             |
|-------------------|---------------------------------------------------|
| SEXN level 2~Vc/F | Effect of SEXN level 2 on Apparent central volume |

Pass the `lookup` spec — the same one used elsewhere in the package,
where each covariate carries `values` and `decode` — and the level
becomes its category:

``` r

lookup <- yaml::read_yaml("data/spec/lookup.yaml")

pk_res <- model_report(
  model_names = "run28",
  spec_pk     = spec_pk,
  lookup      = lookup
)
```

| parameter_names  | label                                              |
|------------------|----------------------------------------------------|
| SEXN Female~Vc/F | Effect of Female (SEXN) on Apparent central volume |

A covariate or level missing from `lookup` keeps the “level N” form, so
a partial lookup is safe.

#### Comparing models

Pass several names and each model becomes a column, combined as
`Estimate (RSE%) [Shrinkage%]`:

``` r

pk_res <- model_report(
  model_names = c("run10", "run28", "run45"),
  spec_pk     = spec_pk
)
```

The comparison is a full join on parameter name, so a parameter present
in one model and absent from another simply leaves a gap — which is what
makes a base model and its covariate models readable side by side.

#### Reporting a bootstrap

With `bootstrap = TRUE`, one model is reported next to the summaries of
its non-parametric bootstrap:

``` r

pk_res <- model_report(
  model_names  = "run28",
  spec_pk      = spec_pk,
  bootstrap    = TRUE,
  boot_results = "results/run28-boot/boot_summary.csv"   # or a data frame
)
```

The final-model columns change shape, and the bootstrap gets its own
spanning header:

| column                    | holds                                       |
|---------------------------|---------------------------------------------|
| `Estimate`                | the final estimate                          |
| `RSE (%) [Shrinkage (%)]` | as in the normal table                      |
| `95% CI`                  | asymptotic interval, `estimate ± 1.96 × SE` |
| `Median`                  | bootstrap median                            |
| `95% CI-bootstrap`        | bootstrap interval                          |

It takes **exactly one** model name and a `boot_results`; anything else
stops the call. A `boot_results` passed without `bootstrap = TRUE` is
ignored, with a warning.

The asymptotic interval is built on the scale the standard error belongs
to and then mapped onto the reported scale, so it is in the same units
as the `Estimate` beside it. It is left empty in two cases rather than
given a made-up bound:

- the parameter has no standard error (a `FIX` value, or the covariance
  step failed);
- an OMEGA whose lower bound would be a negative variance, which has no
  CV%. For a small OMEGA this starts once the printed RSE passes the
  high twenties.

`boot_results` is one row per parameter, matched to the report on
NONMEM’s own names, so row order does not matter and rows the report has
no use for are listed and skipped. It needs three columns:

| column | accepted names | example |
|----|----|----|
| parameter | `parameter_names`, `parameter` or `param` | `THETA1`, `OMEGA(1,1)` |
| central value | `median`, `estimate` or `value` | `73.4` |
| interval | one column whose name has “CI” as a word, or a lower/upper pair | `"[65.2-81.1]"`, `"[65.2, 81.1]"` or `"(65.2, 81.1)"` |

**The values are printed as given.** They must already be on the scale
the report shows: back-transformed THETAs, and CV% for OMEGA diagonals.
Summarising raw bootstrap draws without that step puts log-scale numbers
next to natural- scale estimates.

#### Models that fail to load

A model that cannot be processed is reported, excluded, and the table is
built from the rest:

     Generating report for 3 model(s)...
      Processing run1... ✗
        ⚠️  Error: File does not exist: 'models/run1.yaml'
      Processing run28... ✓
      Processing run45... ✓

    ⚠️  WARNING: The following models failed and were excluded:
      • run1

    ✅ Successfully processed 2/3 model(s)

The function only stops if **every** model fails.

#### Export

``` r

flextable::save_as_docx(pk_res, path = "output/parameters.docx")
flextable::save_as_pptx(pk_res, path = "output/parameters.pptx")
flextable::save_as_html(pk_res, path = "output/parameters.html")
flextable::save_as_image(pk_res, path = "output/parameters.png")
```

Being a `flextable`, it also takes further styling before export:

``` r

pk_res <- flextable::add_header_lines(
  pk_res,
  values = "Population PK Parameter Estimates"
)
```

------------------------------------------------------------------------

### Estimation trajectories

[`plot_nonmem_iterations()`](https://ollegst.github.io/CovariateSearcher/reference/plot_nonmem_iterations.md)
reads `<models_dir>/<model>/<model>.ext` and returns a faceted `ggplot`
— a leading panel for the objective function, then one per parameter,
plotted against iteration. With `transform = TRUE` it also reads the
control stream, `<models_dir>/<model>.ctl` or `.mod`, for the `$THETA`
annotations.

``` r

p <- plot_nonmem_iterations("run28")
p
```

Use it when a run converged to something implausible, terminated early,
or produced an RSE you do not believe. A parameter still drifting at the
last iteration, one oscillating between two values, or an objective
function that drops in one large step tell you more than the final
estimate alone.

| argument | default | effect |
|----|----|----|
| `model_name` | — | one model; the function is single-model |
| `models_dir` | `"models"` | where the models live |
| `transform` | `TRUE` | plot on the reported scale rather than the raw `.ext` scale |
| `skip_iterations` | `0` | drop this many iterations from the start of each step |
| `obj_var` | `"OBJ"` | which column occupies the leading panel |
| `max_iterations` | `100` | upper bound on the iteration **number** shown |

#### What `transform` does

`transform = TRUE` uses what your control stream already declares about
its own parameters. Nothing is inferred: if the model says a THETA is on
the log scale, it is plotted on the natural one; if it names a
parameter, the panel carries that name.

| column | shown as | panel |
|----|----|----|
| THETA annotated `;LOG` | `exp(θ)` — the scale you report | `CL` |
| THETA annotated `;RATIO`, or with no transform field | as estimated | `V1` |
| OMEGA / SIGMA | **always as NONMEM wrote them** | `IIV_CL` |

Variances are never converted. A CV% in the parameter table is a
rendering choice for a final estimate; a trajectory is for watching a
number settle, and that is easier against the value the estimator is
actually moving.

Both the names and the log flag come from the record’s own comment —
`0.5 ; CL ; L/h ; LOG` gives the name `CL` and the transform `LOG`.
Anything the model does not declare falls back to the `.ext` itself, so
an unannotated `$OMEGA` keeps panels called `OMEGA.1.1.` (not a typo:
`OMEGA(1,1)` is not a valid column name in R, so reading the table
converts it).

Annotations are matched to columns **by position**, which is only sound
when the counts agree — a line with no `;` comment is not parsed at all,
and would shift every name after it onto the wrong parameter. Each
record is therefore checked on its own and skipped whole on a mismatch,
with a warning naming it. A fully annotated `$THETA` still gets its
names when a `BLOCK()` `$OMEGA` cannot.

`transform = FALSE` plots the `.ext` exactly as written — raw values,
and raw column names.

#### Four things the plot does not show

These are deliberate, and all of them will otherwise look like bugs:

- **One estimation step.** With several `$EST` records the function
  plots the highest-numbered one whose name does not contain
  “Evaluation” — so a `MAXEVAL=0` evaluation appended after a real
  estimation is skipped rather than plotted as the result.
- **No constant parameters.** A parameter that never changes across
  iterations is dropped, so `FIX` thetas never appear. An empty panel
  would carry no information.
- **Burn-in sits left of zero.** SAEM burn-in iterations are renumbered
  negative so they lead into the estimation iterations continuously; the
  colour legend distinguishes `BURN` from `ITER`.
- **`max_iterations` is a cutoff, not a count.** It keeps iterations
  numbered below that value. `max_iterations = 100` on a run that
  converged at iteration 340 shows the first hundred, not the last
  hundred — raise it to see the end.

`skip_iterations` is the complement: it drops the leading iterations of
each step, which is how you get rid of an early objective-function
excursion that flattens the scale of every other panel.

#### Choosing the leading panel

`obj_var` names the column plotted first. The default suits every
estimation method, because
[`read_ext_iterations()`](https://ollegst.github.io/CovariateSearcher/reference/read_ext_iterations.md)
standardises whatever objective column NONMEM wrote — `SAEMOBJ`,
`MCMCOBJ` — to `OBJ`. Naming a column that is not in the file is an
error listing the columns that are.

Naming a *parameter* column promotes it to the leading panel in place of
the objective function, which is a way to put one parameter’s trajectory
first when that is what you are chasing. It is named and rescaled by
`transform` like any other panel, and it is not repeated further down.
Note that `obj_var` takes the `.ext` column name — `THETA1`, not `CL` —
since it selects the column before the annotations are read.

The returned object is an ordinary `ggplot`, so it takes the usual
additions:

``` r

p <- plot_nonmem_iterations("run28", skip_iterations = 5, max_iterations = 500)
p + ggplot2::labs(title = "run28 - final estimation step")
```

------------------------------------------------------------------------

### Bootstrap distributions

[`plot_bootstrap_distributions()`](https://ollegst.github.io/CovariateSearcher/reference/plot_bootstrap_distributions.md)
draws one histogram per parameter of a bootstrap run. Each panel marks
the final model’s estimate in **red** and the bootstrap’s 2.5th, 50th
and 97.5th percentiles in **blue**. A red line well away from the blue
median, or close to one of the outer percentiles, is what to look for.

``` r

boot_run <- bbr::read_model(file.path("models", "run28-boot"))

p <- plot_bootstrap_distributions(
  boot_run,
  model_name = "run28",       # the model the bootstrap was built from
  spec_pk    = spec_pk,       # panel titles with units, as in model_report()
  lookup     = lookup
)
p

## Covariate effects only
plot_bootstrap_distributions(boot_run, "run28", spec_pk = spec_pk,
                             parameters = "~")
```

`boot_run` can be the `bbr` bootstrap model, the table of draws from
[`bbr::bootstrap_estimates()`](https://metrumresearchgroup.github.io/bbr/reference/summarize_bootstrap.html)
(long, with `parameter_names`/`estimate`, or wide, with one column per
parameter), or a path to a `.csv`/`.rds` holding either. Draws that did
not converge are dropped.

| argument | default | effect |
|----|----|----|
| `model_name` | — | supplies the red line (its estimates) and the labels (its control stream) |
| `models_folder` | `"models"` | where that model lives |
| `spec_pk`, `lookup` | `NULL` | panel titles and decoded levels, as in [`model_report()`](https://ollegst.github.io/CovariateSearcher/reference/model_report.md) |
| `parameters` | `NULL` | which parameters get a panel. Matched against the NONMEM name (`THETA1`) and the panel title (`CL/F (L/h)`); an entry matching neither exactly is used as a regular expression |
| `bins` | `30` | histogram bins per panel |
| `ncol` | `NULL` | panel columns; `NULL` lets ggplot2 choose |
| `wrap_width` | `22` | a longer panel title is broken onto two lines at a space; `NULL`/`Inf` keeps one line |
| `footnote` | `NULL` | caption text under the figure, one line per element — rendered the same way as the boxplots’ caption |

Panels are titled and ordered exactly as
[`model_report()`](https://ollegst.github.io/CovariateSearcher/reference/model_report.md)
names its rows, so a panel can be read against its table row. Draws are
shown on the scale the report uses, not the one NONMEM estimated on: a
`;LOG` THETA is exponentiated and an OMEGA/SIGMA diagonal becomes a CV%,
through the same mapping the table applies. If a record’s lines are not
all annotated, its `;LOG` tags are not used and its panels stay as
NONMEM wrote them — the same rule
[`plot_nonmem_iterations()`](https://ollegst.github.io/CovariateSearcher/reference/plot_nonmem_iterations.md)
follows.

Two kinds of parameter are left out unless you name them in
`parameters`:

- one that did not move across the draws, which has no distribution to
  show;
- an OMEGA off-diagonal, which is plotted as the covariance NONMEM
  estimated while the table shows a correlation, so the two would not
  match.

The result is a ggplot, so save it with
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

------------------------------------------------------------------------

### Reading the two together

``` r

spec_pk <- yaml::read_yaml("data/spec/pk-extend.yml")

model_report(model_names = "run28", spec_pk = spec_pk)
plot_nonmem_iterations("run28")
```

Both read the same `$THETA`, `$OMEGA` and `$SIGMA` comments, so the
panels carry the names you already recognise from the table — `CL` next
to `CL/F (L/h)`, not `THETA1`. The structural parameters line up
directly: a clearance reported as 121.85 is the `CL` panel settling near
121.85, because both apply the same `;LOG` back-transform.

The variance rows are where they differ, deliberately. The table reports
IIV as CV%; the panel shows the OMEGA the estimator was moving. A CV% of
52.2 is an OMEGA near 0.25 — related, but do not expect the two numbers
to match on sight.

------------------------------------------------------------------------

### See also

- [YAML Specification
  Files](https://ollegst.github.io/CovariateSearcher/articles/yaml-specification-files.md)
  — what goes in `spec_pk` and `lookup`
- [Complete
  Workflow](https://ollegst.github.io/CovariateSearcher/articles/complete-workflow.md)
  — where these fit in a search
- [Troubleshooting](https://ollegst.github.io/CovariateSearcher/articles/troubleshooting.md)
  — when a run fails outright
