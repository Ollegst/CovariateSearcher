library(flextable)
library(officer)

#' Covariate missing / sentinel-value check (continuous & categorical)
#'
#' For each covariate, reports how many rows hold each "value of concern" both
#' at baseline (time == 0 & evid == 0) and over the whole dataset. Values
#' checked: NA and each user-supplied sentinel (e.g. -99) for all covariates,
#' plus 0 for continuous covariates. All columns are coerced to numeric first.
#' Checks are performed only on rows satisfying `keep_filter` (e.g. exclusion
#' flags removed).
#'
#' @param data A data.frame (NONMEM-style, with time and evid columns).
#' @param con,cat Character vectors of continuous / categorical covariate names
#'   ("col//label" format is accepted; labels are stripped).
#' @param missing_values Numeric vector of sentinel values to count (e.g. c(-99)).
#' @param keep_filter Optional string expression selecting rows to KEEP, applied
#'   before all checks (e.g. "EXFLG == 0"). NULL = use all rows.
#' @param time_col,evid_col Names of the time and event-ID columns.
#' @return Long data.frame: one row per covariate x value-checked, with columns
#'   column, type, value, n_baseline, n_wholedata. Missing columns report NA.
check_cov_na <- function(data, con = character(0), cat = character(0),
                         missing_values = numeric(0),
                         keep_filter = NULL,
                         time_col = "TIME", evid_col = "EVID") {

  # apply user-defined keep filter (e.g. "EXFLG == 0")
  if (!is.null(keep_filter)) {
    keep <- eval(parse(text = keep_filter), envir = data, enclos = parent.frame())
    if (!is.logical(keep))
      stop("keep_filter must evaluate to a logical vector; got ", class(keep)[1])
    keep[is.na(keep)] <- FALSE                       # NA in flag -> drop the row
    data <- data[keep, , drop = FALSE]
  }
  if (!nrow(data)) {
    warning("No rows remain after keep_filter; nothing to check.")
    return(invisible(NULL))
  }

  strip <- function(v) sub("//.*$", "", v)           # "WT//Weight" -> "WT"
  con <- strip(con); cat <- strip(cat)

  # baseline subset (guard NA in TIME/EVID so no phantom rows slip in)
  base <- data[data[[time_col]] == 0 & data[[evid_col]] == 1,]

  num <- function(x) suppressWarnings(as.numeric(as.character(x)))

  count_val <- function(x, val) {
    x <- num(x)
    if (is.na(val)) sum(is.na(x)) else sum(x == val, na.rm = TRUE)
  }

  build <- function(colname, type, values) {
    values  <- unique(values)
    present <- colname %in% names(data)
    do.call(rbind, lapply(values, function(v) {
      data.frame(
        column      = colname,
        type        = type,
        value       = if (is.na(v)) "NA" else as.character(v),
        n_baseline  = if (present) count_val(base[[colname]], v) else NA_integer_,
        n_wholedata = if (present) count_val(data[[colname]], v) else NA_integer_,
        stringsAsFactors = FALSE
      )
    }))
  }

  rows <- list()
  for (cn in con) rows[[length(rows) + 1]] <- build(cn, "con", c(NA, 0, missing_values))
  for (ct in cat) rows[[length(rows) + 1]] <- build(ct, "cat", c(NA, missing_values))

  if (!length(rows)) return(invisible(NULL))
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out
}

#' High-level disposition of subjects and observations by study
#'
#' One row per study plus an Overall row. EXCLFLG is the single arbiter: a row
#' is retained iff EXCLFLG == incl_value (compared as a trimmed string), and
#' excluded otherwise. No method (M1/M3) logic lives here - anything that should
#' drop a record is assumed already encoded in EXCLFLG upstream.
#'
#' Two different bases on purpose
#' ------------------------------
#'   Subjects  : ALL rows. A subject is "included" if they have >= 1 row with
#'               EXCLFLG == incl_value (a kept dosing row alone is enough), and
#'               "excluded" only if NO row of theirs is kept. This is the full
#'               enrolled disposition and will NOT match the NONMEM subject
#'               count, by design.
#'   Observations: EVID == 0 & MDV != 1 - the usable observations NONMEM fits.
#'               These figures DO line up with the run.
#'
#' Columns (counts shown as "N (x.x%)", % over that row's denominator)
#'   Number of Subjects                  : all distinct IDs (denominator)
#'   Number (%) of Excluded Subjects     : no kept row anywhere
#'   Number (%) of Included Subjects     : >= 1 kept row
#'   Number of Observations              : observations available (denominator)
#'   Number (%) of Excluded Observations : observations dropped by EXCLFLG
#'   Number (%) of Included Observations : observations retained  <- matches NONMEM
#'
#' Within each base, total = excluded + included.
#'
#' Overall row = plain COLUMN SUM of the per-study counts (percentages then
#' recomputed from the summed counts). Observation figures are true pooled
#' totals; subject figures are ADDITIVE - a subject in two studies is counted
#' once per study.
#'
#' @param dat        data.frame.
#' @param id,evid,mdv,exclflg,study  column-name overrides.
#' @param incl_value EXCLFLG value meaning "include" (default 0).
#'
#' @return data.frame, one row per study + Overall.
nonmem_disposition_overview <- function(dat,
                                        id         = "ID",
                                        evid       = "EVID",
                                        mdv        = "MDV",
                                        exclflg    = "EXCLFLG",
                                        study      = "STUDYID",
                                        incl_value = 0) {

  # validate
  needed <- c(id = id, evid = evid, mdv = mdv, exclflg = exclflg, study = study)
  miss   <- needed[!needed %in% names(dat)]
  if (length(miss))
    stop("Column(s) not found in 'dat': ",
         paste(sprintf("'%s' (arg '%s')", miss, names(miss)), collapse = ", "))

  # normalise
  d <- data.frame(
    id      = dat[[id]],
    evid    = suppressWarnings(as.numeric(dat[[evid]])),
    mdv     = suppressWarnings(as.numeric(dat[[mdv]])),
    exclflg = trimws(as.character(dat[[exclflg]])),
    study   = as.character(dat[[study]]),
    stringsAsFactors = FALSE
  )
  incl_chr <- trimws(as.character(incl_value))
  eq <- function(x, val) !is.na(x) & x == val

  d$kept <- eq(d$exclflg, incl_chr)         # retained per EXCLFLG, verbatim
  d$obs  <- eq(d$evid, 0) & !eq(d$mdv, 1)   # usable PK observation (NONMEM fits)

  # raw per-study counts
  counts <- function(s) {
    subj_tot <- length(unique(s$id))                 # ALL rows
    subj_inc <- length(unique(s$id[s$kept]))         # >= 1 kept row anywhere
    data.frame(
      study    = NA_character_,
      subj_tot = subj_tot,
      subj_exc = subj_tot - subj_inc,
      subj_inc = subj_inc,
      obs_tot  = sum(s$obs),
      obs_exc  = sum(s$obs & !s$kept),
      obs_inc  = sum(s$obs &  s$kept),
      stringsAsFactors = FALSE
    )
  }
  studies <- sort(unique(d$study))
  raw <- do.call(rbind, lapply(studies, function(st) {
    r <- counts(d[d$study == st, ]); r$study <- st; r
  }))

  # Overall = column sum of the study rows
  tot <- raw[1, , drop = FALSE]; tot$study <- "Overall"
  for (cn in setdiff(names(raw), "study")) tot[[cn]] <- sum(raw[[cn]])
  raw <- rbind(raw, tot)

  # format
  fmt <- function(n, denom) {
    pct <- ifelse(denom > 0, 100 * n / denom, 0)
    sprintf("%d (%.1f%%)", n, pct)
  }
  data.frame(
    Study                                 = raw$study,
    `Number of Subjects`                  = raw$subj_tot,
    `Number (%) of Excluded Subjects`     = fmt(raw$subj_exc, raw$subj_tot),
    `Number (%) of Included Subjects`     = fmt(raw$subj_inc, raw$subj_tot),
    `Number of Observations`              = raw$obs_tot,
    `Number (%) of Excluded Observations` = fmt(raw$obs_exc, raw$obs_tot),
    `Number (%) of Included Observations` = fmt(raw$obs_inc, raw$obs_tot),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

#' Excluded measurements by reason (rows) and study (columns)
#'
#' A reason x study matrix of excluded measurement counts. EXCLFLG is the
#' arbiter and its (trimmed) value IS the reason, used verbatim. No subjects, no
#' method logic - just where the excluded measurements went, by reason.
#'
#' Measurement base
#' ----------------
#'   measurement          : EVID == 0 & MDV != 1  (a usable observation; under
#'                          BLQ == 1 => MDV != 1 this equals "has a usable result")
#'   excluded measurement : measurement & EXCLFLG != incl_value
#' Reasons are drawn from excluded measurements only. Each cell shows
#' "N (x.x%)", % over that STUDY's total measurements - so the per-reason
#' percentages in a column sum to that study's overall excluded-measurement %
#' and the counts reconcile with nonmem_disposition_overview.
#'
#' Rows are ordered by total excluded count (desc). The Overall column is the
#' pooled total across studies (% over all measurements).
#'
#' @param dat        data.frame.
#' @param evid,mdv,exclflg,study  column-name overrides.
#' @param incl_value EXCLFLG value meaning "include" (default 0).
#'
#' @return data.frame: Reason | <study 1> | <study 2> | ... | Overall.
nonmem_exclusion_summary <- function(dat,
                                     evid       = "EVID",
                                     mdv        = "MDV",
                                     exclflg    = "EXCLFLG",
                                     study      = "STUDYID",
                                     incl_value = 0) {

  # validate
  needed <- c(evid = evid, mdv = mdv, exclflg = exclflg, study = study)
  miss   <- needed[!needed %in% names(dat)]
  if (length(miss))
    stop("Column(s) not found in 'dat': ",
         paste(sprintf("'%s' (arg '%s')", miss, names(miss)), collapse = ", "))

  # normalise
  d <- data.frame(
    evid    = suppressWarnings(as.numeric(dat[[evid]])),
    mdv     = suppressWarnings(as.numeric(dat[[mdv]])),
    exclflg = trimws(as.character(dat[[exclflg]])),
    study   = as.character(dat[[study]]),
    stringsAsFactors = FALSE
  )
  incl_chr <- trimws(as.character(incl_value))
  eq <- function(x, val) !is.na(x) & x == val

  d$meas      <- eq(d$evid, 0) & !eq(d$mdv, 1)        # usable measurement
  d$excl_meas <- d$meas & !eq(d$exclflg, incl_chr)    # excluded measurement

  fmt <- function(n, denom)
    sprintf("%d (%.1f%%)", n, if (denom > 0) 100 * n / denom else 0)

  studies   <- sort(unique(d$study))
  meas_tot  <- vapply(studies, function(st) sum(d$meas[d$study == st]), numeric(1))
  grand_tot <- sum(d$meas)

  # reasons present among excluded measurements, ordered by total frequency
  reasons <- names(sort(table(d$exclflg[d$excl_meas]), decreasing = TRUE))
  if (!length(reasons))
    return(data.frame(Reason = character(0), check.names = FALSE,
                      stringsAsFactors = FALSE))

  # reason x study matrix
  rows <- lapply(reasons, function(r) {
    cells <- vapply(studies,
                    function(st) sum(d$excl_meas & d$study == st & d$exclflg == r),
                    numeric(1))
    row <- data.frame(Reason = r, check.names = FALSE, stringsAsFactors = FALSE)
    for (st in studies) row[[st]] <- fmt(cells[[st]], meas_tot[[st]])
    row[["Overall"]] <- fmt(sum(cells), grand_tot)
    row
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' BLQ (below-limit-of-quantification) disposition by study
#'
#' One row per study plus an Overall row. Reports how many usable PK
#' observations are BLQ, and how many of those BLQ records are retained vs.
#' excluded per EXCLFLG. EXCLFLG is the single arbiter, same convention as
#' nonmem_disposition_overview(): a row is retained iff EXCLFLG == incl_value
#' (compared as a trimmed string).
#'
#' BLQ observation definition
#' --------------------------
#'   EVID == 0 & LDV == 1  -- a usable-PK-timepoint record flagged as BLQ.
#'   (MDV is NOT used to define BLQ here: whether a BLQ record is itself
#'   further marked MDV=1 depends on the M1/M3 handling method, which is
#'   independent of "is this observation BLQ" -- that logic, if needed,
#'   belongs in EXCLFLG upstream, same as nonmem_disposition_overview().)
#'
#' Columns (counts shown as "N (x.x%)", % over that row's total BLQ observations)
#'   Number of BLQ Observations             : all BLQ records (denominator)
#'   Number (%) of Excluded BLQ Observations: BLQ records dropped by EXCLFLG
#'   Number (%) of Included BLQ Observations: BLQ records retained
#'
#' Overall row = plain column sum of the per-study counts (percentages then
#' recomputed from the summed counts) -- a true pooled total, same convention
#' as nonmem_disposition_overview().
#'
#' @param dat        data.frame.
#' @param evid,mdv,ldv,exclflg,study  column-name overrides.
#' @param incl_value EXCLFLG value meaning "include" (default 0).
#'
#' @return data.frame, one row per study + Overall.
nonmem_blq_overview <- function(dat,
                                evid       = "EVID",
                                mdv        = "MDV",
                                ldv        = "BLQ",
                                exclflg    = "EXCLFLG",
                                study      = "STUDYID",
                                incl_value = 0) {

  # validate
  needed <- c(evid = evid, mdv = mdv, ldv = ldv, exclflg = exclflg, study = study)
  miss   <- needed[!needed %in% names(dat)]
  if (length(miss))
    stop("Column(s) not found in 'dat': ",
         paste(sprintf("'%s' (arg '%s')", miss, names(miss)), collapse = ", "))

  # normalise
  d <- data.frame(
    evid    = suppressWarnings(as.numeric(dat[[evid]])),
    mdv     = suppressWarnings(as.numeric(dat[[mdv]])),
    ldv     = suppressWarnings(as.numeric(dat[[ldv]])),
    exclflg = trimws(as.character(dat[[exclflg]])),
    study   = as.character(dat[[study]]),
    stringsAsFactors = FALSE
  )
  incl_chr <- trimws(as.character(incl_value))
  eq <- function(x, val) !is.na(x) & x == val

  d$kept <- eq(d$exclflg, incl_chr)              # retained per EXCLFLG, verbatim
  d$blq  <- eq(d$evid, 0) & eq(d$ldv, 1)         # BLQ observation

  # raw per-study counts
  counts <- function(s) {
    data.frame(
      study   = NA_character_,
      blq_tot = sum(s$blq),
      blq_exc = sum(s$blq & !s$kept),
      blq_inc = sum(s$blq &  s$kept),
      stringsAsFactors = FALSE
    )
  }
  studies <- sort(unique(d$study))
  raw <- do.call(rbind, lapply(studies, function(st) {
    r <- counts(d[d$study %in% st, ]); r$study <- st; r   # %in%, not == (avoids NA-in-study subsetting bug)
  }))

  # Overall = column sum of the study rows
  tot <- raw[1, , drop = FALSE]; tot$study <- "Overall"
  for (cn in setdiff(names(raw), "study")) tot[[cn]] <- sum(raw[[cn]])
  raw <- rbind(raw, tot)

  # format
  fmt <- function(n, denom) {
    pct <- ifelse(denom > 0, 100 * n / denom, 0)
    sprintf("%d (%.1f%%)", n, pct)
  }
  data.frame(
    Study                                     = raw$study,
    `Number of BLQ Observations`              = raw$blq_tot,
    `Number (%) of Excluded BLQ Observations` = fmt(raw$blq_exc, raw$blq_tot),
    `Number (%) of Included BLQ Observations` = fmt(raw$blq_inc, raw$blq_tot),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

#' Summarise missing covariate values by study (pre-imputation)
#'
#' Pass the one-row-per-subject dataset BEFORE imputation. A value is "missing"
#' if it is NA or equals one of `na_codes` (default -99). Rows are covariates,
#' columns are studies + Overall; each cell is N (x.x%) of that study's subjects
#' with the covariate missing. Only covariates with >= 1 missing value are shown.
#'
#' Study columns are decoded via the YAML block for `study`, using its parallel
#' `values` / `decode` lists, and ordered to follow that YAML order. Studies not
#' listed in the YAML keep their raw code and are appended (sorted) at the end.
#'
#' @param data_table one row per subject (pre-imputation).
#' @param variables  character vector of covariate columns to check.
#' @param study      stratifying column (default "STUDYIDN").
#' @param yaml_data  YAML list: covariate labels (short + unit) and the study
#'                   values/decode map. Optional; falls back to raw names.
#' @param na_codes   sentinel value(s) meaning missing, besides NA (default -99).
#'
#' @return data.frame: Covariate | <one column per decoded study> | Overall.
covariate_missing_summary <- function(data_table, variables,
                                      study     = "STUDYIDN",
                                      yaml_data = NULL,
                                      na_codes  = -99) {

  bad <- setdiff(c(variables, study), colnames(data_table))
  if (length(bad))
    stop("Column(s) not found in 'data_table': ", paste(bad, collapse = ", "))

  st_vec <- as.character(data_table[[study]])

  # study code -> label map + ordering from YAML
  smap <- character(0)
  if (!is.null(yaml_data) && !is.null(yaml_data[[study]]) &&
      !is.null(yaml_data[[study]]$values) && !is.null(yaml_data[[study]]$decode)) {
    smap <- setNames(as.character(unlist(yaml_data[[study]]$decode)),
                     as.character(unlist(yaml_data[[study]]$values)))
  }
  present <- unique(st_vec)
  ordered <- c(names(smap)[names(smap) %in% present],
               sort(setdiff(present, names(smap))))            # YAML order, extras last
  studies <- ordered
  labels  <- ifelse(studies %in% names(smap), smap[studies], studies)

  # label / format helpers
  lab_of <- function(v) {
    if (!is.null(yaml_data) && !is.null(yaml_data[[v]]))
      paste0(yaml_data[[v]]$short,
             ifelse(!is.null(yaml_data[[v]]$unit),
                    paste0(" (", yaml_data[[v]]$unit, ")"), ""))
    else v
  }
  is_missing <- function(x) is.na(x) | x %in% na_codes
  fmt        <- function(n, d) sprintf("%d (%.1f%%)", n, if (d > 0) 100 * n / d else 0)

  # one row per covariate that has missing values
  rows <- lapply(variables, function(v) {
    m <- is_missing(data_table[[v]])
    if (!any(m)) return(NULL)
    per_study <- vapply(studies, function(s) {
      sel <- st_vec == s
      fmt(sum(m & sel), sum(sel))
    }, character(1))
    names(per_study) <- labels
    vals <- c(Covariate = lab_of(v), per_study,
              Overall = fmt(sum(m), length(m)))
    as.data.frame(as.list(vals), check.names = FALSE, stringsAsFactors = FALSE)
  })

  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    message("No missing values found in the specified covariates.")
    return(invisible(NULL))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Extract flat single-row header labels from a table1 flextable
#'
#' table1's \code{t1flex()} output has a two-row header (grouping row +
#' level/N= row). This extracts the bottom header row as plain text, used as
#' the flat header when rebuilding the final single-header-row table.
#'
#' @param ft A flextable object produced by \code{table1::t1flex()}.
#'
#' @return A character vector of header labels, one per column.
#'
#' @keywords internal
#' @noRd
get_flat_header_labels <- function(ft) {
  header_data <- ft$header$dataset
  as.character(header_data[nrow(header_data), ])
}

#' Build one extra-stratification block as plain data
#'
#' Subsets \code{data_table} to the specified study, builds a table1 object
#' stratified by \code{extra_strat_var}, drops its own Overall column, and
#' renames the remaining level columns as \code{"<extra_study>(<level>) N=xx"}.
#'
#' @param data_table A data.frame containing the variables to summarise and
#'   the stratifying column named by \code{study}.
#' @param yaml_data A named list where each element has a \code{short} label
#'   and an optional \code{unit}.
#' @param variables A character vector of variable names to include.
#' @param study Name of the column identifying study membership.
#' @param extra_study A single value of \code{study} identifying which study
#'   to subset to before computing this stratification block.
#' @param extra_strat_var Name of the covariate column in \code{data_table}
#'   to stratify by within the \code{extra_study} subset.
#'
#' @return A list with \code{dataset} (data.frame: label col + level cols,
#'   Overall column dropped) and \code{headers} (character vector of display
#'   headers, e.g. \code{"ABC123(Asian) N=20"}, same length as \code{dataset}).
#'
#' @keywords internal
#' @noRd
build_extra_strat_block <- function(data_table, yaml_data, variables, study,
                                    extra_study, extra_strat_var) {
  subset_data <- data_table[data_table[[study]] == extra_study, ]

  for (var in variables) {
    if (var %in% colnames(subset_data)) {
      label(subset_data[[var]]) <- if (!is.null(yaml_data[[var]])) {
        paste0(yaml_data[[var]]$short,
               ifelse(!is.null(yaml_data[[var]]$unit),
                      paste0(" (", yaml_data[[var]]$unit, ")"), ""))
      } else {
        var
      }
    }
  }

  formula <- as.formula(paste("~", paste(variables, collapse = " + "), "|", extra_strat_var))

  block_ft <- table1(formula, data = subset_data, droplevels = TRUE,
                     topclass = "Rtable1-zebra Rtable1-grid Rtable1-shade Rtable1-times") %>%
    t1flex()

  block_dataset <- block_ft$body$dataset
  block_header  <- get_flat_header_labels(block_ft)

  # Drop the block's own Overall column (always last)
  n_col <- ncol(block_dataset)
  block_dataset <- block_dataset[, -n_col, drop = FALSE]
  block_header  <- block_header[-n_col]

  # Extract "N=xx" (without parens) separately, then rebuild as "<extra_study>(<level>) N=xx"
  level_labels <- block_header[-1]
  n_suffix <- sub("(?s)^.*\\(N=([0-9]+)\\)\\s*$", "N=\\1", level_labels, perl = TRUE)
  level_labels_clean <- trimws(sub("(?s)\\s*\\(N=.*\\)$", "", level_labels, perl = TRUE))
  new_headers <- paste0(extra_study, "(", level_labels_clean, ") ", n_suffix)

  list(dataset = block_dataset, headers = c("", new_headers))
}

#' Create a Table 1 summary as a styled flextable
#'
#' Builds a Table 1 summary with the table1 package, applies variable labels
#' (short name and optional unit) pulled from a YAML specification, converts
#' the result to a flextable, drops categorical rows whose Overall column is
#' zero, applies project styling, and optionally appends extra stratification
#' column blocks computed within a single specific study.
#'
#' @param data_table A data.frame containing the variables to summarise and
#'   the stratifying column named by `study`.
#' @param yaml_data A named list where each element has a `short` label and
#'   an optional `unit`.
#' @param variables A character vector of variable names to include.
#' @param study Name of the stratifying column. Defaults to "STUDYIDN".
#' @param extra_study Optional. A single value of `study` identifying the
#'   specific study to compute additional stratification blocks within.
#' @param extra_strat Optional. A character vector of covariate names in
#'   `data_table`. Each generates its own block of extra columns (one per
#'   level, excluding that block's own Overall), appended after the main
#'   Overall column, computed only on rows where `study == extra_study`.
#'
#' @return A styled flextable containing the Table 1 summary.
#'
#' @export
create_table1_summary <- function(data_table, yaml_data, variables, study = "STUDYIDN",
                                  extra_study = NULL, extra_strat = NULL) {

  # Main table: labels, formula, table1 -> t1flex
  label_list <- lapply(variables, function(var) {
    if (!is.null(yaml_data[[var]])) {
      paste0(yaml_data[[var]]$short,
             ifelse(!is.null(yaml_data[[var]]$unit),
                    paste0(" (", yaml_data[[var]]$unit, ")"), ""))
    } else {
      var
    }
  })
  names(label_list) <- variables

  for (var in variables) {
    if (var %in% colnames(data_table)) {
      label(data_table[[var]]) <- label_list[[var]]
    }
  }

  formula <- as.formula(paste("~", paste(variables, collapse = " + "), "|", study))

  main_ft <- table1(formula, data = data_table, droplevels = TRUE,
                    topclass = "Rtable1-zebra Rtable1-grid Rtable1-shade Rtable1-times") %>%
    t1flex()

  main_dataset <- main_ft$body$dataset
  main_headers <- get_flat_header_labels(main_ft)

  # Drop rows whose Overall (last) column is zero -> zero in all strata
  overall_col <- trimws(main_dataset[[ncol(main_dataset)]])
  zero_row <- grepl("^0([ (]|$)", overall_col)
  if (any(zero_row)) {
    main_dataset <- main_dataset[!zero_row, ]
  }

  # Use simple safe column names internally; display headers tracked separately
  colnames(main_dataset) <- paste0("col", seq_len(ncol(main_dataset)))
  final_dataset <- main_dataset
  final_headers <- main_headers

  # Extra stratification blocks (optional)
  if (!is.null(extra_study) && !is.null(extra_strat)) {
    for (i in seq_along(extra_strat)) {
      strat_var <- extra_strat[i]

      block <- build_extra_strat_block(data_table, yaml_data, variables, study,
                                       extra_study, strat_var)
      block_dataset <- block$dataset
      block_headers <- block$headers

      # Match rows by trimmed row-label text (first column)
      match_idx <- match(trimws(final_dataset[[1]]), trimws(block_dataset[[1]]))
      new_cols <- block_dataset[match_idx, -1, drop = FALSE]
      new_cols[is.na(new_cols)] <- ""
      colnames(new_cols) <- paste0("extra", i, "_", seq_len(ncol(new_cols)))
      rownames(new_cols) <- rownames(final_dataset)

      final_dataset <- cbind(final_dataset, new_cols)
      final_headers <- c(final_headers, block_headers[-1])
    }
  }

  # Build final flextable once, apply styling once
  ft <- flextable(final_dataset)

  header_map <- as.list(final_headers)
  names(header_map) <- colnames(final_dataset)
  ft <- set_header_labels(ft, values = header_map)

  ft <- ft %>%
    bold(j = 1, bold = TRUE, part = "body") %>%
    align(j = c(-1), align = "center", part = "all") %>%
    theme_pps_table()

  ft
}


# make table nice
make_table_nice <- function(ft) {
  ft %>%
    flextable() %>%
    bold(j = 1,  bold = TRUE, part = "body" )%>%
    #align( i = ~ is.na(Analyte), j = c(-1), align = "center", part = "body")   %>%
    align( j = c(-1),align = "center", part = "all")   %>%
    theme_pps_table()
}

# AZ colors
az_colors <- c('Graphite'    = "#3f4444",
               'Platinum'   = "#9db0ac",
               'Navy'       = "#0d375f",
               'Mulberry'   = "#7c144d",
               'Gold'       = "#f0ab00",
               'Magenta'    = "#d0006f" ,
               'Puprle'     = "#3c1053",
               'Lime_green' = "#bfd71c",
               'Light_blue' = "#68d2df")

az_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return(az_colors)

  az_colors[cols]
}

az_palettes <- list(
  `main`  = az_cols("Platinum","Navy","Mulberry", "Magenta", "Lime_green"),

  `cool`  = az_cols("Platinum",    "Navy",  "Light_blue","Light_blue"),

  `hot`   = az_cols( "Mulberry","Gold","Magenta","Puprle"),

  `mixed` = az_cols(  "Graphite", "Platinum",   "Navy", "Mulberry","Gold",
                      "Magenta","Lime_green",  "Light_blue" ),
  `day and night` = az_cols("Platinum", "Mulberry"),
  `grey`  = az_cols("Platinum")
)

az_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- az_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

scale_color_az <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- az_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("az_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_az <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- az_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("az_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


# -- Function for plotting in log scale
log10_minor_break = function (...){
  function(x) {
    minx         = floor(min(log10(x), na.rm=T))-1;
    maxx         = ceiling(max(log10(x), na.rm=T))+1;
    n_major      = maxx-minx+1;
    major_breaks = seq(minx, maxx, by=1)
    minor_breaks =
      rep(log10(seq(1, 9, by=1)), times = n_major)+
      rep(major_breaks, each = 9)
    return(10^(minor_breaks))
  }
}


gmean <- function (x, na.rm = TRUE) {
  if (is.null(nrow(x))) {
    exp(mean(log(x), na.rm = TRUE))
  }
  else {
    exp(apply(log(x), 2, mean, na.rm = na.rm))
  }
}

gmean_ci <- function (x) {
  x <- stats::na.omit(x)
  se <- sd(x)/sqrt(length(x))
  gmean <- gmean(x)
  data.frame(y = gmean, ymin = quantile(x,probs = c( .05)), ymax = quantile(x,probs = c( .95)))
}

SD_ci <- function (x) {
  x <- stats::na.omit(x)
  sd <- sd(x)
  median <- median(x)
  data.frame(y = median, ymin = quantile(x,probs = c( .05)), ymax = quantile(x,probs = c( .95)))
}

SE_ci <- function (x) {
  x <- stats::na.omit(x)
  se <- sd(x)/sqrt(length(x))
  median <- median(x)
  data.frame(y = median, ymin = quantile(x,probs = c( .05)), ymax = quantile(x,probs = c( .95)))
}

# NOTE: theme_pps_table() removed - it is provided by the CovariateSearcher
# package (self-contained version). Load it with library(CovariateSearcher).
# The border/font globals below are kept for reference / any other use.

#Table customization
big_border <- fp_border(color="black", width = 1)
std_border <- fp_border(color="gray",  width = 0.5)
font_name_table <- "Times New Roman"
font_size_table <- 10



set_flextable_defaults(big.mark = "")

# function to calculate median and IQR
.median_iqr = function(value, fmt = sig, ...) {
  median <- fmt(median(value, na.rm = TRUE), ...)
  p25 <- fmt(quantile(value, 0.25, na.rm = TRUE), ...)
  p75 <- fmt(quantile(value, 0.75, na.rm = TRUE), ...)
  paste0(median, " (", p25, ", ", p75, ")")
}

# custom function for pt_demographics that makes ANZ specific summaries
cont_long_custom <- function(value, ..., fmt = sig, digits = 3, maxex = 5) {
  value <- na.omit(value)
  ans <- tibble(
    `Mean (SD)` = pmtables:::.mean_sd(value, fmt = fmt, digits = digits, maxex = maxex),
    `Median (IQR)` = .median_iqr(value, fmt = fmt, digits = digits, maxex = maxex),
    `Min / Max` = pmtables:::.min_max(value, fmt = fmt, digits = digits, maxex = maxex),
    `Missing` = as.character(sum(is.na(value)), digits = digits, maxex = maxex)
  )
  ans
}
#function to extract list of categorical and continuous covariates to update pk.yml
get_parameters <- function(yaml_input, remove_vars = NULL) {
  remove_var  <- c("EVID","MDV","BLQ","CMT","NOMTIME","TIME","TAD","AMT","DV","LNDV")

  if (!is.null(remove_var)) {
    remove_var <- c( remove_var,remove_vars)
  }
  # Extract categotical covatiates
  cat <- names(yaml_input)[sapply(yaml_input, function(x) "decode" %in% names(x))]

  # Extract categotical covatiates
  con <- names(yaml_input)[sapply(yaml_input, function(x) "unit" %in% names(x))]

  if (!is.null(remove_var)) {
    con <- setdiff(con,remove_var)
    cat <- setdiff(cat,remove_var)
  }

  # Format as a comma-separated string
  result <- list (cat = paste(cat, collapse = ","), con = paste(con, collapse = ","))

  # Return the result
  return(result)
}







# Function to create density plots for multiple continuous variables by study
plot_density_by_study_multiple <- function(data_table, yaml_data, variables) {


  plots <- list() # List to store plots for each variable


  # Loop through each variable and create a separate plot
  for (variable in variables) {
    # Ensure the variable exists in the data table
    if (!variable %in% colnames(data_table)) {
      warning(paste("Variable", variable, "not found in the data table. Skipping."))
      next
    }

    # Extract labels for the variable from YAML
    variable_label <- if (!is.null(yaml_data[[variable]]) && "short" %in% names(yaml_data[[variable]])) {
      yaml_data[[variable]]$short
    } else {
      variable
    }
    variable_units <- if (!is.null(yaml_data[[variable]]) && "unit" %in% names(yaml_data[[variable]])) {
      paste0(" (", latex2exp::TeX(yaml_data[[variable]]$unit), ")")
    } else {
      ""
    }
    variable_full_label <- paste0(variable_label, variable_units)

    # Create long format data for faceting
    plot_data <- data_table %>%
      select(OSTUDYID, all_of(variable)) %>%
      rename(Value = all_of(variable))

    # Calculate medians for each study
    medians <- plot_data %>%
      group_by(OSTUDYID) %>%
      summarize(Median = median(Value, na.rm = TRUE), .groups = "drop")

    # Create the density plot with facet_wrap
    plot <- ggplot(plot_data, aes(x = Value)) +
      geom_histogram(bins = 30,color="#bfd71c", fill="#9db0ac", alpha = 0.9) +
      geom_vline(data = medians, aes(xintercept = Median), linetype = "dashed", linewidth = 0.8)
    labs(
      title = paste(" Distribution of", variable_label),
      x = variable_full_label,
      y = "Density",
      fill = "Study",
      color = "Median"
    ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 6),
        plot.title = element_text(hjust = 0.5, size = 7, face = "bold"),
        axis.title = element_text(size = 7),
        strip.background = element_rect(fill="gray"),
        plot.caption = element_text(hjust = 0, size =7, vjust = 0))

    # Add the plot to the list
    plots[[variable]] <- plot
  }

  return(plots)
}
plot_density_multiple <- function(data_table, yaml_data, variables, strat) {


  plots <- list() # List to store plots for each variable


  # Loop through each variable and create a separate plot
  for (variable in variables) {
    # Ensure the variable exists in the data table
    if (!variable %in% colnames(data_table)) {
      warning(paste("Variable", variable, "not found in the data table. Skipping."))
      next
    }

    # Extract labels for the variable from YAML
    variable_label <- if (!is.null(yaml_data[[variable]]) && "short" %in% names(yaml_data[[variable]])) {
      yaml_data[[variable]]$short
    } else {
      variable
    }
    variable_units <- tryCatch({
      if (!is.null(yaml_data[[variable]]$unit)) {
        unit_text <- yaml_data[[variable]]$unit
        # Convert ^2 to superscript
        unit_text <- gsub("\\^2", "²", unit_text)
        unit_text <- gsub("\\^3", "³", unit_text)
        paste0(" (", unit_text, ")")
      } else {
        ""
      }
    }, error = function(e) {
      cat("Error getting unit for", variable, ":", e$message, "\n")
      ""
    })
    variable_full_label <- TeX(paste0(variable_label, variable_units))

    # Create long format data for faceting
    plot_data <- data_table %>%
      select(strat, all_of(variable)) %>%
      rename(Value = all_of(variable))

    # Calculate medians for each study
    medians <- plot_data %>%
      #  group_by(STUDY) %>%
      summarize(Median = median(Value, na.rm = TRUE), .groups = "drop")

    # Create the density plot with facet_wrap
    plot <- ggplot(plot_data, aes(x = Value)) +
      geom_histogram(bins = 30,color="#bfd71c", fill="#9db0ac", alpha = 0.9) +
      geom_vline(data = medians, aes(xintercept = Median), linetype = "dashed", linewidth = 0.8) +

      # facet_wrap(~ STUDY, nrow = 3) +
      labs(
        title = paste(" Distribution of", variable_label),
        x = variable_full_label,
        y = "Density",
        # fill = "Study",
        color = "Median"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 7, face = "bold"),
        axis.title = element_text(size = 8),
        strip.background = element_rect(fill="gray"),
        plot.caption = element_text(hjust = 0, size = 5, vjust = 0))

    # Add the plot to the list
    plots[[variable]] <- plot
  }

  return(plots)
}
ggally_mysmooth <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) +
    geom_density(mapping = aes_string(), ...)
}

#' Association measures for categorical covariates
#'
#' Returns 0-1 association strengths analogous to a correlation matrix:
#'   categorical x categorical -> Cramer's V   (from chi-squared)
#'   continuous  x categorical -> eta          (correlation ratio; sqrt of the
#'                                              one-way ANOVA eta-squared)
#' Values are NA-coded (NA plus `missing_values`, default -99) and then use
#' pairwise complete observations. Values are returned UNROUNDED; round when
#' printing, e.g. round(res$cramersV, 2).
#'
#' @param data           data.frame.
#' @param cat_vars       categorical covariate column names.
#' @param con_vars       continuous covariate column names (optional, numeric).
#' @param missing_values sentinel value(s) to treat as missing, besides NA
#'                        (default -99). Set NULL to disable.
#' @param bias_correct   bias-corrected Cramer's V (Bergsma) if TRUE.
#'
#' @return list:
#'   $cramersV : symmetric cat x cat matrix of Cramer's V.
#'   $eta      : con (rows) x cat (cols) matrix of correlation ratios
#'               (only if con_vars supplied).
covariate_association <- function(data, cat_vars, con_vars = NULL,
                                  missing_values = -99,
                                  bias_correct = FALSE) {

  bad <- setdiff(c(cat_vars, con_vars), colnames(data))
  if (length(bad))
    stop("Column(s) not found in 'data': ", paste(bad, collapse = ", "))

  # (2) continuous vars must be numeric
  if (length(con_vars)) {
    bad_con <- con_vars[!vapply(data[con_vars], is.numeric, logical(1))]
    if (length(bad_con))
      stop("Continuous variable(s) are not numeric: ",
           paste(bad_con, collapse = ", "))
  }

  # (1) recode sentinel missing values to NA, on used columns only
  # Per-column so a -99 sentinel never survives, but factor levels and other
  # columns are left untouched.
  to_na <- function(x) {
    if (is.null(missing_values)) return(x)
    if (is.factor(x)) {                       # drop sentinel from factor levels
      x[as.character(x) %in% as.character(missing_values)] <- NA
      return(droplevels(x))
    }
    x[x %in% missing_values] <- NA
    x
  }
  for (v in unique(c(cat_vars, con_vars))) data[[v]] <- to_na(data[[v]])

  # Cramer's V (categorical vs categorical)
  cramers_v <- function(x, y) {
    ok <- !is.na(x) & !is.na(y)
    x <- factor(x[ok]); y <- factor(y[ok])
    if (nlevels(x) < 2 || nlevels(y) < 2) return(NA_real_)
    tab  <- table(x, y)
    n    <- sum(tab)
    if (n < 2) return(NA_real_)
    chi2 <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
    phi2 <- as.numeric(chi2 / n)
    r <- nrow(tab); k <- ncol(tab)
    if (bias_correct) {                       # Bergsma 2013 correction
      phi2 <- max(0, phi2 - (r - 1) * (k - 1) / (n - 1))
      r <- r - (r - 1)^2 / (n - 1)
      k <- k - (k - 1)^2 / (n - 1)
    }
    denom <- min(r - 1, k - 1)                # (3) guard degenerate denominator
    if (denom <= 0) return(NA_real_)
    sqrt(phi2 / denom)
  }

  # eta / correlation ratio (continuous vs categorical)
  eta <- function(continuous, group) {
    ok <- !is.na(continuous) & !is.na(group)
    y <- continuous[ok]; g <- factor(group[ok])
    if (nlevels(g) < 2 || length(y) < 2) return(NA_real_)
    grand  <- mean(y)
    ss_tot <- sum((y - grand)^2)
    if (ss_tot == 0) return(NA_real_)
    ss_bet <- sum(tapply(y, g, function(v) length(v) * (mean(v) - grand)^2))
    sqrt(ss_bet / ss_tot)
  }

  V <- outer(cat_vars, cat_vars,
             Vectorize(function(a, b) cramers_v(data[[a]], data[[b]])))
  dimnames(V) <- list(cat_vars, cat_vars)
  res <- list(cramersV = V)                   # (5) unrounded

  if (length(con_vars)) {
    E <- outer(con_vars, cat_vars,
               Vectorize(function(c, k) eta(data[[c]], data[[k]])))
    dimnames(E) <- list(con_vars, cat_vars)
    res$eta <- E
  }
  res
}
#' Boxplots of continuous covariates by categorical covariates, with eta
#'
#' facet_grid: continuous covariates = rows, categorical = columns. Each panel
#' shows boxplots of the continuous variable split by the categorical's levels,
#' annotated with the correlation ratio eta (top-left), coloured by strength:
#' black (< eta_moderate), yellow (moderate), green (>= eta_strong).
#'
#' Categorical values are used as-is from the data (already decoded, so their
#' factor levels are the intended order). Each covariate's x-axis follows its
#' OWN factor levels, independently per facet - a level name shared across
#' covariates (e.g. "Missing") is not forced to a common position. Never
#' re-derived or sorted. Strip labels: yaml short+unit.
#'
#' Outliers are not drawn and not allowed to inflate the axis: each continuous
#' row is trimmed to the whisker envelope across its groups, so the row scale
#' collapses to the visible boxes. Box quartiles unaffected; eta uses FULL data.
#'
#' @param data           data.frame.
#' @param con_vars       continuous covariate columns (numeric).
#' @param cat_vars       categorical covariate columns.
#' @param yaml_data      optional YAML list for facet labels (short + unit).
#' @param missing_values sentinel(s) treated as missing besides NA (default -99).
#' @param trim           drop points beyond whisker envelope per row (default TRUE).
#' @param label_width    chars per label line before wrapping (default 18).
#' @param eta_moderate   eta at/above which the label turns yellow (default 0.25).
#' @param eta_strong     eta at/above which the label turns green  (default 0.37).
#' @param box_fill,strip_fill  cosmetic colours.
#'
#' @return a ggplot object.
plot_cont_cat <- function(data, con_vars, cat_vars,
                          yaml_data      = NULL,
                          missing_values = -99,
                          trim           = TRUE,
                          label_width    = 16,
                          eta_moderate   = 0.25,
                          eta_strong     = 0.37,
                          box_fill       = "#cfe0db",
                          strip_fill     = "#9db0ac") {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required.")
  library(ggplot2)

  bad <- setdiff(c(con_vars, cat_vars), colnames(data))
  if (length(bad))
    stop("Column(s) not found in 'data': ", paste(bad, collapse = ", "))
  bad_con <- con_vars[!vapply(data[con_vars], is.numeric, logical(1))]
  if (length(bad_con))
    stop("Continuous variable(s) are not numeric: ", paste(bad_con, collapse = ", "))

  col_mod <- "#DAA520"   # yellow/goldenrod (moderate)
  col_str <- "#2E7D32"   # green (strong)

  # helpers
  wrap_lab <- function(s) paste(strwrap(s, width = label_width), collapse = "\n")
  to_na <- function(x) {
    if (is.null(missing_values)) return(x)
    if (is.factor(x)) { x[as.character(x) %in% as.character(missing_values)] <- NA
    return(droplevels(x)) }
    x[x %in% missing_values] <- NA; x
  }
  lab_of <- function(v) {
    base <- if (!is.null(yaml_data) && !is.null(yaml_data[[v]]))
      paste0(yaml_data[[v]]$short,
             ifelse(!is.null(yaml_data[[v]]$unit),
                    paste0(" (", yaml_data[[v]]$unit, ")"), ""))
    else v
    wrap_lab(base)
  }
  eta <- function(y, g) {
    ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- factor(g[ok])
    if (nlevels(g) < 2 || length(y) < 2) return(NA_real_)
    grand <- mean(y); ss_tot <- sum((y - grand)^2)
    if (ss_tot == 0) return(NA_real_)
    ss_bet <- sum(tapply(y, g, function(v) length(v) * (mean(v) - grand)^2))
    sqrt(ss_bet / ss_tot)
  }
  eta_colour <- function(e) {
    if (is.na(e)) return("black")
    if (e >= eta_strong)   return(col_str)
    if (e >= eta_moderate) return(col_mod)
    "black"
  }
  trim_row <- function(d) {                       # keep points inside whisker envelope
    whisk <- function(v) {
      qs <- quantile(v, c(.25, .75), na.rm = TRUE); iqr <- qs[2] - qs[1]
      c(min(v[v >= qs[1] - 1.5 * iqr]), max(v[v <= qs[2] + 1.5 * iqr]))
    }
    parts <- split(d$y, interaction(d$cat_name, d$x, drop = TRUE))
    b <- vapply(parts, whisk, numeric(2))
    d[d$y >= min(b[1, ]) & d$y <= max(b[2, ]), ]
  }

  for (v in unique(c(con_vars, cat_vars))) data[[v]] <- to_na(data[[v]])
  con_lab <- vapply(con_vars, lab_of, character(1))
  cat_lab <- vapply(cat_vars, lab_of, character(1))

  # Per-covariate x-axis ordering. Every facet column shares ONE x aesthetic, so
  # a level name that appears in more than one covariate (e.g. "Missing") would
  # be pinned to a single global position and jump to the wrong place in some
  # facets. Fix: tag each value with its covariate column so the levels are
  # unique per facet, order them by that covariate's own factor levels (decoded
  # upstream; non-factors keep data order), then strip the tag back off for the
  # axis labels (see scale_x_discrete below). Nothing is sorted.
  sep <- "___"                                    # tag separator; will not appear in level names
  ordered_levels_of <- function(ct) {
    col <- data[[ct]]; col <- col[!is.na(col)]
    if (is.factor(col)) levels(droplevels(col)) else unique(as.character(col))
  }
  x_levels  <- unlist(lapply(cat_vars, function(ct)
    paste(ct, ordered_levels_of(ct), sep = sep)))
  strip_tag <- function(k) sub(paste0("^.*?", sep), "", as.character(k))

  # long data + eta per pair
  long <- list(); eta_df <- list()
  for (cn in con_vars) for (ct in cat_vars) {
    y <- data[[cn]]; g <- data[[ct]]
    ok <- !is.na(y) & !is.na(g)
    if (!any(ok)) next
    e <- eta(y, g)
    long[[length(long) + 1]] <- data.frame(
      con_name = lab_of(cn), cat_name = lab_of(ct),
      x = paste(ct, as.character(g[ok]), sep = sep), y = y[ok],
      stringsAsFactors = FALSE)
    eta_df[[length(eta_df) + 1]] <- data.frame(
      con_name = lab_of(cn), cat_name = lab_of(ct),
      lab = sprintf("η = %.2f", e), col = eta_colour(e),
      stringsAsFactors = FALSE)
  }
  long   <- do.call(rbind, long)
  eta_df <- do.call(rbind, eta_df)
  long$x <- factor(long$x, levels = x_levels)

  if (isTRUE(trim))
    long <- do.call(rbind, lapply(split(long, long$con_name), trim_row))

  long$con_name   <- factor(long$con_name,   levels = con_lab)
  long$cat_name   <- factor(long$cat_name,   levels = cat_lab)
  eta_df$con_name <- factor(eta_df$con_name, levels = con_lab)
  eta_df$cat_name <- factor(eta_df$cat_name, levels = cat_lab)

  # plot
  ggplot(long, aes(x = x, y = y)) +
    geom_boxplot(fill = box_fill, outlier.shape = NA, linewidth = 0.3) +
    geom_text(data = eta_df, aes(x = -Inf, y = Inf, label = lab, colour = col),
              hjust = -0.1, vjust = 1.4, size = 2.2, inherit.aes = FALSE) +
    scale_colour_identity() +
    scale_x_discrete(labels = strip_tag) +
    facet_grid(con_name ~ cat_name, scales = "free", space = "free_x") +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
    theme_bw() +
    theme(
      text         = element_text(size = 5),
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 5),
      axis.text.y  = element_text(size = 5),
      axis.title   = element_blank(),
      strip.text   = element_text(size = 5, color = "white", face = "bold"),
      strip.background = element_rect(fill = strip_fill),
      panel.grid.minor = element_blank())
}


#' Screen ETA vs continuous covariate associations (Spearman rho)
#'
#' Diagnostic screening plot of model ETAs against continuous covariates. For
#' each ETA x covariate pair a rank correlation (Spearman by default, robust to
#' skew/outliers) is computed and annotated on the panel, coloured by strength
#' so candidate covariates stand out for manual review. Intended as a wide net
#' feeding a formal covariate analysis (e.g. SCM+), not for selection itself.
#'
#' Accepts variables in pmplots "col//label" format or as bare column names;
#' labels fall back to `yaml_data` (short + unit) and then to the column name.
#'
#' @param data A data.frame with one row per subject (ETA screening is per-ID).
#' @param eta_vars ETA column names (continuous), "col//label" or bare.
#' @param cov_vars Continuous covariate column names, "col//label" or bare.
#' @param yaml_data Optional named list; each element has `short` and optional `unit`.
#' @param method Correlation method passed to `cor()`; "spearman" (default) is
#'   rank-based and robust. Use "pearson" only for clean, linear data.
#' @param ncol Number of covariate plots per row in the arranged output.
#' @param label_width Character width for wrapping panel labels.
#' @param missing_values Optional values to recode to NA before computing.
#' @param cor_moderate,cor_strong Absolute-rho thresholds for the colour screen.
#' @param col_mod,col_str Colours for moderate / strong associations.
#' @param n_min Minimum complete pairs; below this the label gets a "*" flag.
#' @param .return_list If TRUE, return the per-covariate plots as a list
#'   (unarranged) instead of a single arranged patchwork.
#'
#' @return By default a patchwork object: one plot per covariate (faceted by
#'   ETA), arranged `ncol` per row. If `.return_list = TRUE`, a list of the
#'   per-covariate ggplots. Returns invisibly NULL if no pair has data.
#'
#' @export
eta_cont_screen <- function(data, eta_vars, cov_vars,
                            yaml_data      = NULL,
                            method         = "spearman",
                            ncol           = 2,
                            label_width    = 20,
                            missing_values = NULL,
                            cor_moderate   = 0.2,
                            cor_strong     = 0.3,
                            col_mod        = "blue",
                            col_str        = "red",
                            n_min          = 10,
                            .return_list   = FALSE) {

  # helpers
  wrap_lab <- function(s) paste(strwrap(s, width = label_width), collapse = "\n")

  to_na <- function(x) {
    if (is.null(missing_values)) return(x)
    if (is.factor(x)) {
      x[as.character(x) %in% as.character(missing_values)] <- NA
      return(droplevels(x))
    }
    x[x %in% missing_values] <- NA; x
  }

  lab_of <- function(v) {
    base <- if (!is.null(yaml_data) && !is.null(yaml_data[[v]]))
      paste0(yaml_data[[v]]$short,
             ifelse(!is.null(yaml_data[[v]]$unit),
                    paste0(" (", yaml_data[[v]]$unit, ")"), ""))
    else v
    wrap_lab(base)
  }

  # split "col//label" -> list(col, lab); plain "col" -> label from YAML
  parse_cl <- function(s) {
    parts <- strsplit(s, "//", fixed = TRUE)[[1]]
    col <- parts[1]
    lab <- if (length(parts) > 1) wrap_lab(parts[2]) else lab_of(col)
    list(col = col, lab = lab)
  }

  rho <- function(x, y) {                       # signed rank correlation + n
    ok <- !is.na(x) & !is.na(y); x <- x[ok]; y <- y[ok]
    if (length(x) < 3) return(c(r = NA_real_, n = length(x)))
    r <- suppressWarnings(cor(x, y, method = method))
    c(r = as.numeric(r), n = length(x))
  }

  cor_colour <- function(r) {                   # screen on magnitude
    if (is.na(r)) return("black")
    if (abs(r) >= cor_strong)   return(col_str)
    if (abs(r) >= cor_moderate) return(col_mod)
    "black"
  }

  # recode missing on the raw columns actually referenced
  raw_cols <- unique(vapply(c(eta_vars, cov_vars),
                            function(s) parse_cl(s)$col, character(1)))
  for (v in raw_cols) if (v %in% names(data)) data[[v]] <- to_na(data[[v]])

  # long data + correlation per pair
  long <- list(); cor_df <- list()
  for (en in eta_vars) for (cv in cov_vars) {
    pe <- parse_cl(en); pc <- parse_cl(cv)
    x <- data[[pc$col]]; y <- data[[pe$col]]
    if (is.null(x) || is.null(y)) next
    ok <- !is.na(x) & !is.na(y)
    if (!any(ok)) next
    rn   <- rho(x, y)
    flag <- if (!is.na(rn["n"]) && rn["n"] < n_min) "*" else ""
    long[[length(long) + 1]] <- data.frame(
      eta_name = pe$lab, cov_name = pc$lab,
      x = x[ok], y = y[ok], stringsAsFactors = FALSE)
    cor_df[[length(cor_df) + 1]] <- data.frame(
      eta_name = pe$lab, cov_name = pc$lab,
      lab = sprintf("ρ = %+.2f%s", rn["r"], flag),
      col = cor_colour(rn["r"]),
      stringsAsFactors = FALSE)
  }
  if (!length(long)) return(invisible(NULL))
  long   <- do.call(rbind, long)
  cor_df <- do.call(rbind, cor_df)

  # preserve input covariate order in the layout
  cov_levels <- unique(vapply(cov_vars, function(s) parse_cl(s)$lab, character(1)))
  cov_levels <- cov_levels[cov_levels %in% long$cov_name]

  # one plot per covariate, faceted by ETA
  plots <- lapply(cov_levels, function(cl) {
    d  <- long[long$cov_name == cl, ]
    an <- cor_df[cor_df$cov_name == cl, ]
    ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 1/5, size = 1) +
      ggplot2::geom_smooth(method = "loess", se = FALSE,
                           linewidth = 0.4, colour = "grey40") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
      ggplot2::geom_text(data = an,
                         ggplot2::aes(label = lab, colour = col),
                         x = -Inf, y = Inf, hjust = -0.1, vjust = 1.4,
                         inherit.aes = FALSE, size = 3, show.legend = FALSE) +
      ggplot2::scale_colour_identity() +
      ggplot2::facet_wrap(~ eta_name, ncol = ncol) +
      ggplot2::labs(x = cl, y = NULL) +
      ggplot2::theme_bw()
  })
  names(plots) <- cov_levels

  if (.return_list) return(plots)                      # list of per-covariate ggplots
  patchwork::wrap_plots(plots, ncol = ncol)            # arranged patchwork
}

#' @rdname eta_cont_screen
#' @export
eta_cont_screen_list <- function(data, eta_vars, cov_vars, ...) {
  eta_cont_screen(data, eta_vars, cov_vars, ..., .return_list = TRUE)
}

#' Exploratory ETA vs categorical covariate screening
#'
#' Non-gating diagnostic: boxplots of model ETAs across the levels of each
#' categorical covariate, annotated with a rank epsilon (sqrt of rank
#' epsilon-squared from a Kruskal-Wallis test) measuring group separation, plus
#' a "*" flag when the smallest level has fewer than `n_min_cell` subjects.
#' Per-group subject counts are shown under each box. The number is a faithful
#' scannable summary of group separation, but it does not gate: all covariates
#' pass to a formal analysis (e.g. SCM+), which decides on OFV.
#'
#' Accepts variables in pmplots "col//label" format or as bare column names;
#' labels fall back to `yaml_data` (short + unit) and then the column name.
#' Covariate columns are coerced to factors (numeric codes become levels).
#'
#' @param data A data.frame with one row per subject (ETA screening is per-ID).
#' @param eta_vars ETA columns (continuous y), "col//label" or bare.
#' @param cov_vars Categorical covariate columns, "col//label" or bare.
#' @param yaml_data Optional named list; each element has `short` and optional `unit`.
#' @param ncol Number of covariate plots per row in the arranged output.
#' @param label_width Character width for wrapping panel labels.
#' @param missing_values Optional values to recode to NA before computing.
#' @param n_min_cell Smallest level size below which the label gets a "*" flag.
#' @param .return_list If TRUE, return per-covariate plots as a list.
#'
#' @return A patchwork (default) or list of per-covariate ggplots.
#'   Invisibly NULL if no pair has data.
#'
#' @export
eta_cat_screen <- function(data, eta_vars, cov_vars,
                           yaml_data      = NULL,
                           ncol           = 2,
                           label_width    = 20,
                           missing_values = NULL,
                           n_min_cell     = 5,
                           .return_list   = FALSE) {

  # helpers
  wrap_lab <- function(s) paste(strwrap(s, width = label_width), collapse = "\n")

  to_na <- function(x) {
    if (is.null(missing_values)) return(x)
    if (is.factor(x)) {
      x[as.character(x) %in% as.character(missing_values)] <- NA
      return(droplevels(x))
    }
    x[x %in% missing_values] <- NA; x
  }

  lab_of <- function(v) {
    base <- if (!is.null(yaml_data) && !is.null(yaml_data[[v]]))
      paste0(yaml_data[[v]]$short,
             ifelse(!is.null(yaml_data[[v]]$unit),
                    paste0(" (", yaml_data[[v]]$unit, ")"), ""))
    else v
    wrap_lab(base)
  }

  parse_cl <- function(s) {
    parts <- strsplit(s, "//", fixed = TRUE)[[1]]
    col <- parts[1]
    lab <- if (length(parts) > 1) wrap_lab(parts[2]) else lab_of(col)
    list(col = col, lab = lab)
  }

  # group separation: rank epsilon (sqrt of rank eps^2) + smallest level n
  eps_cat <- function(y, g) {
    ok <- !is.na(y) & !is.na(g); y <- y[ok]; g <- droplevels(factor(g[ok]))
    n <- length(y)
    if (nlevels(g) < 2 || n < 2) return(c(e = NA_real_, nmin = NA_real_))
    H <- suppressWarnings(kruskal.test(y, g)$statistic)
    c(e = sqrt(max(0, as.numeric(H) / (n - 1))), nmin = min(table(g)))
  }

  # recode missing on the raw columns actually referenced
  raw_cols <- unique(vapply(c(eta_vars, cov_vars),
                            function(s) parse_cl(s)$col, character(1)))
  for (v in raw_cols) if (v %in% names(data)) data[[v]] <- to_na(data[[v]])

  # long data + stats per pair
  long <- list(); ann <- list(); lev_map <- list(); lab_map <- list()
  for (en in eta_vars) for (cv in cov_vars) {
    pe <- parse_cl(en); pc <- parse_cl(cv)
    y <- data[[pe$col]]; g <- data[[pc$col]]
    if (is.null(y) || is.null(g)) next
    g <- factor(g)                                   # numeric codes -> levels
    ok <- !is.na(y) & !is.na(g)
    if (!any(ok)) next
    g_ok <- droplevels(factor(g[ok]))
    if (nlevels(g_ok) < 1) next

    cnt     <- table(g_ok)
    lvl_lab <- setNames(sprintf("%s\nN=%d", names(cnt), as.integer(cnt)),
                        names(cnt))                  # level -> "level\nN=.."
    es   <- eps_cat(y, g)
    flag <- if (!is.na(es["nmin"]) && es["nmin"] < n_min_cell) "*" else ""

    long[[length(long) + 1]] <- data.frame(
      eta_name = pe$lab, cov_name = pc$lab,
      x = as.character(g_ok), y = y[ok], stringsAsFactors = FALSE)
    ann[[length(ann) + 1]] <- data.frame(
      eta_name = pe$lab, cov_name = pc$lab,
      lab = sprintf("ε = %.2f%s", es["e"], flag),
      stringsAsFactors = FALSE)
    lev_map[[pc$lab]] <- levels(g_ok)                # level order for this cov
    lab_map[[pc$lab]] <- lvl_lab                     # count labels for this cov
  }
  if (!length(long)) return(invisible(NULL))
  long <- do.call(rbind, long)
  ann  <- do.call(rbind, ann)

  cov_levels <- unique(vapply(cov_vars, function(s) parse_cl(s)$lab, character(1)))
  cov_levels <- cov_levels[cov_levels %in% long$cov_name]

  # one plot per covariate, faceted by ETA
  plots <- lapply(cov_levels, function(cl) {
    d  <- long[long$cov_name == cl, ]
    an <- ann[ann$cov_name == cl, ]
    d$x <- factor(d$x, levels = lev_map[[cl]])
    ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
      ggplot2::geom_boxplot(outlier.size = 0.6, linewidth = 0.3) +
      ggplot2::geom_text(data = an, ggplot2::aes(label = lab),
                         x = -Inf, y = Inf, hjust = -0.05, vjust = 1.4,
                         inherit.aes = FALSE, size = 2.8, colour = "grey20") +
      ggplot2::scale_x_discrete(labels = lab_map[[cl]]) +   # level\nN=.. ticks
      ggplot2::facet_wrap(~ eta_name, ncol = ncol) +
      ggplot2::labs(x = cl, y = NULL) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 7))
  })
  names(plots) <- cov_levels

  if (.return_list) return(plots)
  patchwork::wrap_plots(plots, ncol = ncol)
}

#' @rdname eta_cat_screen
#' @export
eta_cat_screen_list <- function(data, eta_vars, cov_vars, ...) {
  eta_cat_screen(data, eta_vars, cov_vars, ..., .return_list = TRUE)
}

# =============================================================================
# NOTE: create_covariate_boxplots() and its helper cast were REMOVED - they are
# now provided (in a newer form) by the CovariateSearcher package. Load it with
# library(CovariateSearcher). get_plot_dims() is kept below because
# add_covariate_figs_to_report() still uses it. library(ggplot2)/library(patchwork)
# are retained for the plotting helpers above; library(devEMF)/library(yaml) were
# only used by the removed function - drop them if nothing else needs them.
# =============================================================================

library(ggplot2)
library(patchwork)
library(devEMF)
library(yaml)

## -----------------------------------------------------------------------
## Helper: figure dimensions based on number of stratification levels.
##   1 level            -> 4 x 4   (w x h)
##   2 levels           -> 8 x 4   (1 row x 2 cols)
##   3-4 levels         -> 8 x 8   (2 rows x 2 cols)
##   5-6 levels         -> 8 x 12  (3 rows x 2 cols)
##   etc. (ncol fixed at 2; height grows by 4 per extra row)
## User-supplied width/height (if not NULL) always take precedence.
## Kept here because add_covariate_figs_to_report() calls it.
## -----------------------------------------------------------------------
get_plot_dims <- function(n_levels, width = NULL, height = NULL) {
  if (!is.null(width) && !is.null(height)) {
    return(list(width = width, height = height))
  }
  n_levels <- max(n_levels, 1)
  ncol <- min(n_levels, 2)
  nrow <- ceiling(n_levels / 2)
  list(width = 4 * ncol, height = 4 * nrow)
}

## =============================================================================
## add_covariate_figs_to_report()
##
## Scans the AUC/Cmax/Cmin subfolders created by create_covariate_boxplots(),
## extracts the covariate name from each filename via the
## "<drug>-<covariate>-<type>.emf" naming pattern, and appends one row per
## file to a report figure data frame via add_fig_df() -- mirroring the
## existing add_fig_df() reporting pattern used elsewhere in the workflow.
## =============================================================================
#' Add covariate boxplot figures to a report figure data frame
#'
#' @param fig_df existing figure data frame to append to (as used by
#'   add_fig_df() elsewhere in the reporting workflow).
#' @param output_folder base folder containing the AUC/Cmax/Cmin subfolders
#'   produced by create_covariate_boxplots() (same value used there).
#' @param drug character, the drug name used when the files were created
#'   (must match exactly what was passed to create_covariate_boxplots()).
#' @param type character vector of parameter types to pick up, e.g.
#'   c("AUC","Cmax","Cmin"). Each must have a corresponding subfolder
#'   <output_folder>/<type>/ and an entry in `fig_tag_map`.
#' @param covariates optional character vector of covariate names to
#'   include (matching the covariate portion of the filenames, e.g. "WT",
#'   "RACEN"). If supplied, only these are added -- in the order given --
#'   instead of everything found in the folder. If a requested covariate
#'   has no matching file for a given type, a warning is issued and it is
#'   skipped. Default NULL: include everything found.
#' @param fig_fn passed through to add_fig_df()'s `fig_fn` argument
#'   (e.g. a caption object/function, as in the existing reporting code).
#' @param stratification the same stratification info used when the plots
#'   were generated (via create_covariate_boxplots()), so the figure
#'   width/height recorded here match exactly. Accepts:
#'     - NULL (default): no stratification was used (single panel).
#'     - a single number: the number of stratification levels used.
#'     - a vector: the actual stratification column (e.g. data$COMB) --
#'       the number of unique values is computed automatically.
#'   Width/height are then derived the same way create_covariate_boxplots()
#'   derives them (via get_plot_dims(), plus the extra room added for the
#'   bottom x-axis strip), so there's no need to pass width/height manually.
#' @param fig_tag_map named character vector/list mapping each `type` to
#'   its fig_tag, e.g. c(AUC="FigBoxSimAUC", Cmax="FigBoxSimCmax",
#'   Cmin="FigBoxSimCmin"). Default uses exactly that scheme.
#' @param fig_tl figure title passed to add_fig_df(); default "" (empty).
#' @param scale passed through to add_fig_df(); default 1.
#' @param verbose logical; if TRUE (default), prints which files were found
#'   and added.
#'
#' @return the updated fig_df (with one row appended per matched file).
add_covariate_figs_to_report <- function(fig_df,
                                         output_folder,
                                         drug,
                                         type = c("AUC", "Cmax", "Cmin"),
                                         covariates = NULL,
                                         fig_fn,
                                         stratification = NULL,
                                         fig_tag_map = c(AUC  = "FigBoxSimAUC",
                                                         Cmax = "FigBoxSimCmax",
                                                         Cmin = "FigBoxSimCmin"),
                                         fig_tl = "",
                                         scale = 1,
                                         verbose = TRUE) {

  n_levels <- if (is.null(stratification)) {
    1
  } else if (is.numeric(stratification) && length(stratification) == 1) {
    stratification
  } else {
    length(unique(as.character(stratification)))
  }

  dims   <- get_plot_dims(n_levels)
  width  <- dims$width
  height <- dims$height + 0.5  # matches the extra room create_covariate_boxplots()
  # adds for the bottom x-axis label strip

  for (t in type) {

    tag <- fig_tag_map[[t]]
    if (is.null(tag)) {
      stop("No fig_tag mapping found for type '", t, "'. Update fig_tag_map.")
    }

    folder <- file.path(output_folder, t)
    if (!dir.exists(folder)) {
      warning("Folder not found, skipping: ", folder)
      next
    }

    ## pattern: <drug>-<covariate>-<type>.emf ; capture the covariate name
    pattern <- paste0("^", drug, "-(.+)-", t, "\\.emf$")
    files <- sort(list.files(folder, pattern = pattern, full.names = TRUE))

    if (length(files) == 0) {
      warning("No files matching '", pattern, "' found in ", folder)
      next
    }

    file_covs <- sub(pattern, "\\1", basename(files))

    if (!is.null(covariates)) {
      keep_idx <- match(covariates, file_covs)
      missing_covs <- covariates[is.na(keep_idx)]
      if (length(missing_covs) > 0) {
        warning("No file found for covariate(s) in type '", t, "': ",
                paste(missing_covs, collapse = ", "))
      }
      keep_idx  <- keep_idx[!is.na(keep_idx)]
      files     <- files[keep_idx]
      file_covs <- file_covs[keep_idx]
    }

    if (length(files) == 0) {
      warning("No matching covariates left to add for type '", t, "' after filtering.")
      next
    }

    if (verbose) {
      cat(sprintf("=== %s: adding %d file(s) from %s ===\n", t, length(files), folder))
      flush(stdout())
    }

    for (i in seq_along(files)) {
      f <- files[i]
      covariate <- file_covs[i]

      fig_df <- add_fig_df(
        fig_df,
        fig      = f,
        fig_tag  = tag,
        fig_fn   = fig_fn,
        fig_tl   = fig_tl,
        width    = width,
        height   = height,
        scale    = scale
      )

      if (verbose) {
        cat(sprintf("  added [%s] %s -> %s\n", tag, covariate, f))
        flush(stdout())
      }
    }
  }

  fig_df
}

## =============================================================================
## Example usage
## =============================================================================
# param_info <- list(
#   AUC  = list(label = "AUCss",   unit = "mg*hr/L"),
#   Cmax = list(label = "Cmax_ss", unit = "ng/mL"),
#   Cmin = list(label = "Cmin_ss", unit = "ng/mL")
# )
#
# results <- create_covariate_boxplots(   # <- now the CovariateSearcher version
#   data           = sim_data,
#   spec           = "pk-spec.yml",
#   con            = c("WT", "AGE", "CRCLI"),
#   cat            = c("RACEN", "SEXN", "ECOGBL"),
#   type           = c("AUC", "Cmax"),
#   drug           = "Camizestrant",
#   param_info     = param_info,
#   stratification = "COMB",
#   output_folder  = "results/figure/simulations"
# )
#
# ## Append the generated figures to a report figure data frame:
# fig_df <- add_covariate_figs_to_report(
#   fig_df         = fig_df,
#   output_folder  = "results/figure/simulations",
#   drug           = "Camizestrant",
#   type           = c("AUC", "Cmax"),
#   fig_fn         = caption_data,
#   stratification = sim_data$COMB   # or e.g. 2 (level count), or NULL
# )

# summarize_dose_regimen.R
#
# Standalone helper (NOT part of the CovariateSearcher package).
# Builds a subject-count table: one row per dose/regimen combination,
# one column per study, cells = number of distinct subjects.
# Adds a Total column (subjects per regimen) and a Total row (subjects
# per study).
#
# Requires: dplyr, tidyr (both already available in this project).

library(dplyr)
library(tidyr)

#' Summarise dose regimens by study and number of subjects
#'
#' Takes a decoded analysis dataset and returns a table with one row per
#' dose/regimen combination and one column per study, where each cell is the
#' number of distinct subjects.
#'
#' The regimen label is built by pasting the dose and frequency columns, e.g.
#' `DOSE = 100`, `FREQ = "BID"` -> `"100 mg BID"`.
#'
#' @param data Decoded dataset (data.frame / tibble). One or more rows per
#'   subject is fine - subjects are de-duplicated on `id_col`.
#' @param study_col,id_col,dose_col,freq_col Column names (as strings) for the
#'   study, subject id, dose amount, and dosing frequency/regimen.
#' @param dose_unit Unit string inserted into the regimen label (default "mg").
#'   Set to "" to drop it.
#' @param total Logical; append a `Total` column (distinct subjects per
#'   regimen, across studies) and a `Total` row (distinct subjects per study,
#'   across regimens). Default TRUE. Because a subject dosed under more than
#'   one regimen is counted once per regimen but only once in the totals,
#'   totals need not equal the sum of the cells.
#'
#' @return A tibble: `Regimen`, one integer column per study (ordered), and
#'   optionally a `Total` column and a trailing `Total` row. Empty cells are 0.
summarize_dose_regimen <- function(data,
                                   study_col = "STUDY",
                                   id_col    = "ID",
                                   dose_col  = "DOSE",
                                   freq_col  = "FREQ",
                                   dose_unit = "mg",
                                   total     = TRUE) {

  # validate columns
  needed  <- c(study_col, id_col, dose_col, freq_col)
  missing <- setdiff(needed, names(data))
  if (length(missing)) {
    stop(
      "Column(s) not found in data: ", paste(missing, collapse = ", "),
      ".\n  Available columns: ", paste(names(data), collapse = ", "),
      "\n  Pass the correct names via study_col/id_col/dose_col/freq_col.",
      call. = FALSE
    )
  }

  unit <- if (nzchar(dose_unit)) paste0(" ", dose_unit) else ""

  # one row per subject x regimen, with a regimen label
  long <- data %>%
    transmute(
      .study   = as.character(.data[[study_col]]),
      .id      = .data[[id_col]],
      .dose    = .data[[dose_col]],
      .freq    = .data[[freq_col]],
      .regimen = paste0(.data[[dose_col]], unit, " ", .data[[freq_col]])
    ) %>%
    distinct(.study, .id, .dose, .freq, .regimen)

  # row order: by dose (numeric where possible) then frequency
  reg_levels <- long %>%
    distinct(.dose, .freq, .regimen) %>%
    mutate(.dose_num = suppressWarnings(as.numeric(as.character(.dose)))) %>%
    arrange(.dose_num, .dose, .freq) %>%
    pull(.regimen)

  # column order: studies sorted
  study_levels <- sort(unique(long$.study))

  # count distinct subjects per regimen x study, pivot wide
  wide <- long %>%
    count(.regimen, .study, name = "n_subj") %>%
    mutate(
      .regimen = factor(.regimen, levels = reg_levels),
      .study   = factor(.study,   levels = study_levels)
    ) %>%
    pivot_wider(
      names_from  = .study,
      values_from = n_subj,
      values_fill = 0,
      names_sort  = FALSE
    ) %>%
    arrange(.regimen) %>%
    rename(Regimen = .regimen) %>%
    mutate(Regimen = as.character(Regimen))

  # optional Total column (subjects per regimen) and row (per study)
  if (total) {
    # Total column: distinct subjects per regimen, across studies
    reg_totals <- long %>%
      distinct(.regimen, .id) %>%
      count(.regimen, name = "Total") %>%
      rename(Regimen = .regimen) %>%
      mutate(Regimen = as.character(Regimen))
    wide <- wide %>% left_join(reg_totals, by = "Regimen")

    # Total row: distinct subjects per study, plus overall distinct subjects
    study_totals <- long %>%
      distinct(.study, .id) %>%
      count(.study, name = "n") %>%
      tidyr::complete(.study = study_levels, fill = list(n = 0)) %>%
      arrange(match(.study, study_levels))
    total_row <- as.list(setNames(study_totals$n, study_totals$.study))
    total_row$Regimen <- "Total"
    total_row$Total   <- dplyr::n_distinct(long$.id)
    wide <- bind_rows(wide, as_tibble(total_row)[, names(wide)])
  }

  wide
}


# example usage
# dat <- yspec::decode_dataset(dat, spec, c(flags$catcov))
# tab <- summarize_dose_regimen(dat,
#                               study_col = "STUDY",
#                               id_col    = "ID",
#                               dose_col  = "DOSE",
#                               freq_col  = "FREQ")
# print(tab)
#
# Resulting shape:
#   Regimen      D1234  D5678  Total
#   100 mg QD       42     55     97
#   200 mg BID      38      0     38
#   Total           80     55    120
