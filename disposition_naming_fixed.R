# disposition_naming_fixed.R
#
# Naming rule applied (popPK report convention):
#   "Overall" = the pooled all-studies row/column (one word, everywhere) -- unchanged.
#   "Total"   = only when it means a denominator sum. Kept verbatim in the
#               internal counts (subj_tot / obs_tot / blq_tot) and in the
#               "total = excluded + included" logic.
#
# Only change: the three usable-count column HEADERS are made parallel by
# dropping "Total" from the observation/BLQ headers so they match the subject
# header (which never said "Total"):
#   "Total Number of Observations"     -> "Number of Observations"
#   "Total Number of BLQ Observations" -> "Number of BLQ Observations"
#   "Number of Subjects"               -> (already correct, unchanged)
#
# Replace the two functions of the same name in your helper script with these.
# nonmem_exclusion_summary(), covariate_missing_summary() and
# create_table1_summary() already follow the rule (pooled = "Overall", no
# "Total Number of ..." headers) and need no change.


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

  # ---- validate -------------------------------------------------------------
  needed <- c(id = id, evid = evid, mdv = mdv, exclflg = exclflg, study = study)
  miss   <- needed[!needed %in% names(dat)]
  if (length(miss)) {
    stop("Column(s) not found in 'dat': ",
         paste(sprintf("'%s' (arg '%s')", miss, names(miss)), collapse = ", "))
  }

  # ---- normalise ------------------------------------------------------------
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

  # ---- raw per-study counts -------------------------------------------------
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
    r <- counts(d[d$study == st, ])
    r$study <- st
    r
  }))

  # Overall = column sum of the study rows
  tot <- raw[1, , drop = FALSE]
  tot$study <- "Overall"
  for (cn in setdiff(names(raw), "study")) tot[[cn]] <- sum(raw[[cn]])
  raw <- rbind(raw, tot)

  # ---- format ---------------------------------------------------------------
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

  # ---- validate -------------------------------------------------------------
  needed <- c(evid = evid, mdv = mdv, ldv = ldv, exclflg = exclflg, study = study)
  miss   <- needed[!needed %in% names(dat)]
  if (length(miss)) {
    stop("Column(s) not found in 'dat': ",
         paste(sprintf("'%s' (arg '%s')", miss, names(miss)), collapse = ", "))
  }

  # ---- normalise ------------------------------------------------------------
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

  # ---- raw per-study counts -------------------------------------------------
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
    # %in%, not == (avoids NA-in-study subsetting bug)
    r <- counts(d[d$study %in% st, ])
    r$study <- st
    r
  }))

  # Overall = column sum of the study rows
  tot <- raw[1, , drop = FALSE]
  tot$study <- "Overall"
  for (cn in setdiff(names(raw), "study")) tot[[cn]] <- sum(raw[[cn]])
  raw <- rbind(raw, tot)

  # ---- format ---------------------------------------------------------------
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
