# Model Reports and Diagnostics

## Model Reports and Diagnostics

Two functions for looking at a finished model.
[`model_report()`](https://ollegst.github.io/CovariateSearcher/reference/model_report.md)
builds the parameter table you publish.
[`plot_nonmem_iterations()`](https://ollegst.github.io/CovariateSearcher/reference/plot_nonmem_iterations.md)
shows how the estimation got there — the trajectory behind the numbers
in that table.

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
| CL/F (L/h) | Apparent clearance | 121.85 | 4.76 |  |
| Vc/F (L) | Apparent central volume | 479.42 | 6.29 |  |
| **Inter-individual variability** |  |  |  |  |
| KA CV% | IIV on absorption | 615.49 | 49.21 | 10.16 |
| CL/F CV% | IIV on clearance | 52.22 | 4.48 | 2.82 |
| **Parameter-Covariate relationships** |  |  |  |  |
| AGE~CL/F | Effect of AGE on clearance | -0.06 | 39.26 |  |
| **Residual variability** |  |  |  |  |
| prop error | Proportional error | 34.39 | 0.51 | 12.10 |

`spec_pk` must be an in-memory object — read the YAML yourself and pass
the result. A file path is not accepted. The [YAML Specification
Files](https://ollegst.github.io/CovariateSearcher/articles/yaml-specification-files.md)
article covers what belongs in that file.

Two rows are appended below the parameter sections: `OFV`, and
`Conditional number` computed from the run’s `.cor` (or `.cov`) output.

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

With no `spec_pk` at all the function sections by name prefix instead —
`beta_*` to covariate relationships, and names beginning `THETA`,
`OMEGA` or `SIGMA` to their respective sections. Since the names are
your control-stream annotations, that fallback only bites for
annotations literally called `THETA1` and the like; annotate as `CL` and
the parameter is unsectioned.

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
