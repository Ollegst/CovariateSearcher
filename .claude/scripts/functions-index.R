#!/usr/bin/env Rscript

# Ground-truth inventory of R/, used to keep FUNCTIONS.md honest.
#
# Usage:
#   Rscript .claude/scripts/functions-index.R [package_root] [--write-state]
#
# Parses every file in R/ with the R parser (not regex, which miscounts nested
# closures as top-level definitions), then prints a drift report:
#
#   COUNTS          current files / top-level functions / exports
#   MISSING_FROM_DOC defined in R/ but absent from FUNCTIONS.md
#   STALE_IN_DOC     listed in FUNCTIONS.md but no longer defined
#   SIGNATURE_DRIFT  argument list differs from the one FUNCTIONS.md shows
#   BODY_CHANGED     body hash differs from the last recorded state
#
# The recorded state lives in .claude/functions-index.tsv and is only rewritten
# when --write-state is passed, so a report can be read before it is accepted.

args <- commandArgs(trailingOnly = TRUE)
write_state <- "--write-state" %in% args
positional <- args[!grepl("^--", args)]
root <- if (length(positional) >= 1) positional[[1]] else getwd()

r_dir      <- file.path(root, "R")
doc_path   <- file.path(root, "FUNCTIONS.md")
ns_path    <- file.path(root, "NAMESPACE")
state_path <- file.path(root, ".claude", "functions-index.tsv")

if (!dir.exists(r_dir)) stop("No R/ directory under: ", root)

# ---- helpers ---------------------------------------------------------------

# In-memory md5 when digest is available. The tools::md5sum fallback writes one
# temp file per function, which on this machine costs seconds in AV scanning.
# The two methods produce different digests, so switching invalidates the
# recorded baseline — rerun with --write-state after changing this.
hash_text <- if (requireNamespace("digest", quietly = TRUE)) {
  function(txt) digest::digest(txt, algo = "md5", serialize = FALSE)
} else {
  function(txt) {
    tf <- tempfile()
    on.exit(unlink(tf), add = TRUE)
    writeLines(txt, tf, useBytes = TRUE)
    unname(tools::md5sum(tf))
  }
}

is_function_def <- function(expr) {
  is.call(expr) &&
    length(expr) == 3L &&
    is.name(expr[[1]]) &&
    as.character(expr[[1]]) %in% c("<-", "=", "<<-") &&
    is.call(expr[[3]]) &&
    is.name(expr[[3]][[1]]) &&
    identical(as.character(expr[[3]][[1]]), "function")
}

def_name <- function(expr) {
  target <- expr[[2]]
  if (is.name(target)) return(as.character(target))
  if (is.character(target) && length(target) == 1L) return(target)
  NA_character_
}

# "fn(x, y=1, ...)" — defaults deparsed, missing args bare
fun_signature <- function(name, fexpr) {
  fmls <- fexpr[[2]]
  parts <- character(0)
  if (length(fmls)) {
    nms <- names(fmls)
    for (i in seq_along(fmls)) {
      # An argument without a default is the empty symbol; binding it raises
      # "argument is missing", but deparsing it in place yields "".
      dtxt <- tryCatch(paste(deparse(fmls[[i]]), collapse = " "),
                       error = function(e) "")
      parts <- c(parts, if (!nzchar(dtxt)) {
        nms[[i]]
      } else {
        paste0(nms[[i]], "=", dtxt)
      })
    }
  }
  paste0(name, "(", paste(parts, collapse = ", "), ")")
}

# closures defined inside a body — informational, they are not top-level
count_nested <- function(expr) {
  if (!is.recursive(expr)) return(0L)
  n <- 0L
  if (is.call(expr) && is.name(expr[[1]]) &&
      identical(as.character(expr[[1]]), "function")) {
    n <- 1L
  }
  for (i in seq_along(expr)) {
    # Formals pairlists hold empty symbols for arguments without defaults;
    # touching one raises "argument is missing", so absorb it per element.
    n <- n + tryCatch({
      el <- expr[[i]]
      if (is.recursive(el)) count_nested(el) else 0L
    }, error = function(e) 0L)
  }
  n
}

# Names of closures assigned inside a body. FUNCTIONS.md documents some of
# these, so they must not be reported as stale just for being non-top-level.
collect_nested_names <- function(expr) {
  out <- character(0)
  if (!is.recursive(expr)) return(out)
  if (is_function_def(expr)) {
    nm <- def_name(expr)
    if (!is.na(nm)) out <- c(out, nm)
  }
  for (i in seq_along(expr)) {
    out <- c(out, tryCatch({
      el <- expr[[i]]
      if (is.recursive(el)) collect_nested_names(el) else character(0)
    }, error = function(e) character(0)))
  }
  out
}

# ---- inventory from R/ -----------------------------------------------------

r_files <- sort(list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE))
defs <- list()
nested_total <- 0L
nested_names <- character(0)

for (f in r_files) {
  exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) {
    stop("Parse error in ", basename(f), ": ", conditionMessage(e))
  })
  for (e in exprs) {
    if (!is_function_def(e)) next
    nm <- def_name(e)
    if (is.na(nm)) next
    fexpr <- e[[3]]
    nested_total <- nested_total + count_nested(fexpr[[3]])
    nested_names <- c(nested_names, collect_nested_names(fexpr[[3]]))
    defs[[length(defs) + 1L]] <- data.frame(
      file      = basename(f),
      name      = nm,
      signature = fun_signature(nm, fexpr),
      hash      = hash_text(paste(deparse(fexpr), collapse = "\n")),
      stringsAsFactors = FALSE
    )
  }
}

if (!length(defs)) stop("No function definitions found under ", r_dir)
inv <- do.call(rbind, defs)
inv <- inv[order(inv$file, inv$name), ]

# ---- exports from NAMESPACE ------------------------------------------------

ns <- if (file.exists(ns_path)) readLines(ns_path, warn = FALSE) else character(0)
export_lines <- grep("^export\\(", ns, value = TRUE)
exports <- gsub('^export\\(|\\)$|"', "", export_lines)
s3_lines <- grep("^S3method\\(", ns, value = TRUE)
inv$exported <- ifelse(inv$name %in% exports, "yes", "no")

# ---- names currently documented in FUNCTIONS.md ----------------------------

doc_names <- character(0)
doc_sigs <- character(0)
if (file.exists(doc_path)) {
  doc <- readLines(doc_path, warn = FALSE)
  # Only wide tables are function tables; the module-map table has 2 columns
  # and would otherwise contribute backticked *file* names as false entries.
  rows <- doc[grepl("^\\|", doc) &
                vapply(gregexpr("|", doc, fixed = TRUE),
                       function(m) sum(m > 0), integer(1)) >= 7]
  # Take the first backticked span rather than splitting on "|": the operator
  # `%||%` contains pipes and a naive split truncates it to "%". Rows with no
  # backtick before the first "|" are separators, not function rows.
  cell <- vapply(rows, function(r) {
    m <- regexpr("`+[^`]+`+", r)
    if (m < 0L) return("")
    if (grepl("|", substr(r, 2L, m - 1L), fixed = TRUE)) return("")
    trimws(gsub("`", "", regmatches(r, m)))
  }, character(1), USE.NAMES = FALSE)
  cell <- cell[nzchar(cell) & cell != "Function"]
  doc_sigs <- cell
  # Arguments are stripped first, so splitting on "/" only ever separates the
  # few rows that document several nested helpers as "a/b/c".
  bare <- trimws(sub("\\(.*$", "", cell))
  doc_names <- trimws(unlist(strsplit(bare, "/", fixed = TRUE)))
  doc_names <- doc_names[nzchar(doc_names)]
}

# ---- previous state --------------------------------------------------------

prev <- NULL
if (file.exists(state_path)) {
  # quote = "" is essential: signatures contain quoted defaults such as
  # models_folder="models", and the default quoting rule shreds those rows
  # into misaligned columns that then look like spurious changes.
  prev <- tryCatch(
    read.delim(state_path, quote = "", comment.char = "",
               stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
}

# ---- drift report ----------------------------------------------------------

emit <- function(title, items) {
  cat("\n## ", title, " (", length(items), ")\n", sep = "")
  if (!length(items)) {
    cat("  none\n")
  } else {
    cat(paste0("  - ", items, collapse = "\n"), "\n", sep = "")
  }
}

cat("# FUNCTIONS.md drift report\n")
cat("\n## COUNTS\n")
cat("  files in R/            : ", length(r_files), "\n", sep = "")
cat("  top-level functions    : ", nrow(inv), "\n", sep = "")
cat("  nested closures        : ", nested_total, "\n", sep = "")
cat("  export() entries       : ", length(exports), "\n", sep = "")
cat("  S3method() entries     : ", length(s3_lines), "\n", sep = "")
cat("  rows in FUNCTIONS.md   : ", length(doc_names), "\n", sep = "")

missing_from_doc <- sort(setdiff(inv$name, doc_names))
missing_lines <- vapply(missing_from_doc, function(n) {
  i <- match(n, inv$name)
  paste0(inv$file[i], "  ", inv$signature[i],
         if (inv$exported[i] == "yes") "  [exported]" else "")
}, character(1), USE.NAMES = FALSE)
emit("MISSING_FROM_DOC — defined in R/, absent from FUNCTIONS.md", missing_lines)

emit("STALE_IN_DOC — in FUNCTIONS.md, no longer defined in R/",
     sort(setdiff(doc_names, c(inv$name, nested_names))))

# signature drift: compare the doc's own parenthesised arg list to the source
sig_drift <- character(0)
for (s in doc_sigs) {
  nm <- trimws(sub("\\(.*$", "", s))
  i <- match(nm, inv$name)
  if (is.na(i) || !grepl("\\(", s)) next
  doc_args <- gsub("\\s+", "", sub("^[^(]*\\(", "", sub("\\)\\s*$", "", s)))
  src_args <- gsub("\\s+", "", sub("^[^(]*\\(", "", sub("\\)$", "", inv$signature[i])))
  if (!identical(doc_args, src_args)) {
    sig_drift <- c(sig_drift,
                   paste0(nm, "\n      doc: ", doc_args, "\n      src: ", src_args))
  }
}
emit("SIGNATURE_DRIFT — FUNCTIONS.md argument list differs from source", sig_drift)

if (is.null(prev)) {
  cat("\n## BODY_CHANGED\n  no previous state recorded (", state_path, ")\n", sep = "")
} else {
  merged <- merge(inv, prev[, c("name", "hash")], by = "name",
                  suffixes = c("", "_prev"), all.x = TRUE)
  changed <- merged[!is.na(merged$hash_prev) & merged$hash != merged$hash_prev, ]
  # paste0() recycles zero-length arguments to "", so an empty result set would
  # otherwise be reported as a single blank finding.
  changed_lines <- if (nrow(changed)) {
    paste0(changed$file, "  ", changed$name)
  } else {
    character(0)
  }
  emit("BODY_CHANGED — body differs from last recorded state", sort(changed_lines))
}

# ---- persist ---------------------------------------------------------------

if (write_state) {
  dir.create(dirname(state_path), recursive = TRUE, showWarnings = FALSE)
  write.table(inv[, c("file", "name", "signature", "exported", "hash")],
              state_path, sep = "\t", row.names = FALSE, quote = FALSE)
  cat("\nstate written: ", state_path, "\n", sep = "")
} else {
  cat("\nstate NOT written (pass --write-state to accept this inventory)\n")
}
