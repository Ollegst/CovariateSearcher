# =============================================================================
# BOOTSTRAP PARAMETER DISTRIBUTIONS
# File: R/plot-bootstrap-distributions.R
# Part of CovariateSearcher Package
# Histogram per parameter of a bbr bootstrap run, against the final model
# =============================================================================


#' Bootstrap draws as one long table
#'
#' @description Takes a bootstrap run in any of the shapes `bbr` hands it over
#'   in: a `bbi_nmboot_model` object (tabulated on the spot), the long table of
#'   `parameter_names`/`estimate` that `bootstrap_estimates(format_long = TRUE)`
#'   returns, or the wide table with one column per parameter. A path to a
#'   `.csv`/`.rds` holding either table is loaded first.
#' @param boot_run The bootstrap run object, table of draws, or path to one.
#' @return `data.frame(nonmem_name, value)`, one row per draw per parameter,
#'   with missing draws (a run that did not converge) dropped.
#' @keywords internal
#' @noRd
.bootstrap_draws <- function(boot_run) {
  if (inherits(boot_run, "bbi_nmboot_model")) {
    boot_run <- bbr::bootstrap_estimates(boot_run, format_long = TRUE)
  } else {
    boot_run <- .load_if_path(boot_run, "boot_run")
  }

  if (!is.data.frame(boot_run)) {
    stop(paste("`boot_run` must be a bbi_nmboot_model, a table of bootstrap",
               "draws, or a path to one."), call. = FALSE)
  }

  boot_run <- as.data.frame(boot_run, stringsAsFactors = FALSE)
  nm <- names(boot_run)

  if (all(c("parameter_names", "estimate") %in% nm)) {
    out <- data.frame(
      nonmem_name = as.character(boot_run$parameter_names),
      value = suppressWarnings(as.numeric(boot_run$estimate)),
      stringsAsFactors = FALSE
    )
  } else {
    param_cols <- grep("^(THETA|OMEGA|SIGMA)", nm)
    if (length(param_cols) == 0) {
      stop(sprintf(
        paste("`boot_run` holds no parameter columns. Expected either",
              "`parameter_names`/`estimate`, or columns named THETA*/OMEGA*/",
              "SIGMA*. Columns found: %s"), paste(nm, collapse = ", ")
      ), call. = FALSE)
    }
    out <- data.frame(
      nonmem_name = rep(nm[param_cols], each = nrow(boot_run)),
      value = unlist(lapply(boot_run[param_cols], function(x) {
        suppressWarnings(as.numeric(x))
      }), use.names = FALSE),
      stringsAsFactors = FALSE
    )
  }

  out <- out[!is.na(out$value), , drop = FALSE]
  if (nrow(out) == 0) {
    stop("`boot_run` holds no usable parameter draws.", call. = FALSE)
  }
  out
}


#' Scale-mapping inputs derived from NONMEM parameter names
#'
#' @description `.to_report_scale()` picks its branch from the columns
#'   `bbr::param_estimates()` supplies. A bootstrap draw arrives as a bare name
#'   and a number, so the same three inputs are read off the name instead: a
#'   `THETA` is a fixed effect, an `OMEGA`/`SIGMA` element with equal indices is
#'   a diagonal, and the `;LOG` annotation decides the rest. Both the draws and
#'   the estimate they are plotted against go through one mapping, so a panel
#'   cannot end up mixing scales.
#' @param nonmem_name Character vector of NONMEM parameter names, in either the
#'   `OMEGA(1,1)` or the `.ext`-sanitised `OMEGA.1.1.` spelling.
#' @param log_cols Character vector of THETA names annotated `;LOG`.
#' @return List of `random_effect_sd`, `trans` and `diag` vectors, the arguments
#'   `.to_report_scale()` expects.
#' @keywords internal
#' @noRd
.name_scale_inputs <- function(nonmem_name, log_cols) {
  is_theta <- grepl("^THETA", nonmem_name)
  block_pat <- "^(OMEGA|SIGMA)[.(](\\d+)[,.](\\d+)[.)]$"
  is_block <- grepl(block_pat, nonmem_name)
  is_diag <- is_block &
    sub(block_pat, "\\2", nonmem_name) == sub(block_pat, "\\3", nonmem_name)

  list(
    random_effect_sd = ifelse(is_theta, NA_real_, 1),
    trans = ifelse(is_theta & nonmem_name %in% log_cols, "LOG", "RATIO"),
    diag = ifelse(is_theta, NA, is_diag)
  )
}


#' Break a panel title across two lines
#'
#' @description A facet strip is only as wide as its panel, and a decoded
#'   categorical level makes a title long enough to be clipped
#'   (`PPI: With concomitant PPI~Vc/F`). A title over `width` characters is
#'   split at a space into two lines: the first line takes as many words as fit
#'   within `width`, the rest go on the second, the way `strwrap()` fills. That
#'   makes `width` the knob that decides where the break lands - raising it by a
#'   character can pull one more word up, which a balanced split would not do.
#'   Words are never broken: a first word already over `width` takes the line to
#'   itself, and a title with no space is left long, a clipped title being
#'   better than an unreadable one. Follows the `wrap_width` convention
#'   [create_covariate_table()] uses for its own scenario labels.
#' @param x Character vector of titles.
#' @param width Integer. Titles at or under this many characters are untouched.
#'   `NULL` or a non-finite value disables wrapping.
#' @return Character vector, each element on one or two lines.
#' @keywords internal
#' @noRd
.wrap_two_lines <- function(x, width = 22) {
  if (is.null(width) || !is.finite(width)) return(x)

  vapply(x, function(title) {
    if (is.na(title) || nchar(title) <= width) return(title)

    words <- strsplit(title, " +")[[1]]
    if (length(words) < 2) return(title)

    cumulative <- vapply(seq_along(words), function(k) {
      nchar(paste(words[seq_len(k)], collapse = " "))
    }, numeric(1))

    fits <- which(cumulative <= width)
    # No word fits: the first one takes the line rather than being broken.
    k <- if (length(fits) == 0) 1L else max(fits)
    k <- min(k, length(words) - 1L)

    paste0(paste(words[seq_len(k)], collapse = " "), "\n",
           paste(words[(k + 1L):length(words)], collapse = " "))
  }, character(1), USE.NAMES = FALSE)
}


#' NONMEM's own ordering of a set of parameter names
#'
#' @description THETAs first in numeric order (`THETA10` after `THETA9`), then
#'   OMEGA, then SIGMA, each by row and column. Alphabetical order would scatter
#'   them.
#' @param nonmem_name Character vector of NONMEM parameter names.
#' @return Integer vector of ranks, usable as a `factor` level ordering.
#' @keywords internal
#' @noRd
.nonmem_name_order <- function(nonmem_name) {
  block <- ifelse(grepl("^THETA", nonmem_name), 1L,
                  ifelse(grepl("^OMEGA", nonmem_name), 2L,
                         ifelse(grepl("^SIGMA", nonmem_name), 3L, 4L)))
  digits <- regmatches(nonmem_name, gregexpr("[0-9]+", nonmem_name))
  first <- vapply(digits, function(d) {
    if (length(d) >= 1) as.numeric(d[1]) else Inf
  }, numeric(1))
  second <- vapply(digits, function(d) {
    if (length(d) >= 2) as.numeric(d[2]) else 0
  }, numeric(1))
  order(order(block, first, second, nonmem_name))
}


#' Plot Bootstrap Parameter Distributions
#'
#' @description One histogram per parameter of a non-parametric bootstrap, each
#'   panel carrying the final model's estimate in red and the bootstrap's 2.5th,
#'   50th and 97.5th percentiles in blue. Reading the two together shows whether
#'   the bootstrap agrees with the fit: a red line sitting away from the blue
#'   median, or hard against a percentile, is the thing this plot is for.
#'
#'   Panels are titled and ordered exactly as [model_report()] names its rows,
#'   so a panel and its table row can be read against one another.
#'
#' @param boot_run A `bbi_nmboot_model` object, a table of bootstrap draws (long
#'   with `parameter_names`/`estimate`, or wide with one THETA/OMEGA/SIGMA
#'   column each), or a path to a `.csv`/`.rds` holding one.
#' @param model_name Character. The model the bootstrap was built from, whose
#'   estimates supply the red line and whose control stream supplies the labels.
#' @param models_folder Character. Path to the models folder (default "models").
#' @param spec_pk Optional yspec object, as passed to [model_report()]. Supplies
#'   the panel titles with their units.
#' @param lookup Optional covariate lookup spec, as passed to [model_report()],
#'   used to decode categorical covariate levels in the panel titles.
#' @param parameters Optional character vector selecting which parameters get a
#'   panel. Entries are matched against both the NONMEM name (`THETA1`) and the
#'   panel title (`CL/F (L/h)`); an entry matching neither exactly is tried as a
#'   regular expression against both. `NULL` (default) plots every parameter
#'   that moved across the draws.
#' @param bins Integer. Histogram bins per panel (default 30).
#' @param ncol Integer. Panel columns; `NULL` (default) lets ggplot2 choose.
#' @param footnote Character or `NULL`. Free text placed under the figure,
#'   left-aligned against the plot edge. Several elements go on their own
#'   lines. `NULL` (default) adds nothing. Rendered the same way
#'   [create_covariate_boxplots()] renders its own, so the two sit together in
#'   one report.
#' @param wrap_width Integer. A panel title longer than this is broken across
#'   two lines at a space, never mid-word; the first line takes as many words as
#'   fit within it. Default 22, sized for a facet strip rather than the table's
#'   wider column - raise it to pull another word onto the first line, lower it
#'   to push one down. `NULL` or `Inf` leaves titles on one line.
#'
#' @return A ggplot2 object.
#'
#' @details
#' Draws are plotted on the scale the report shows, not the scale NONMEM
#' estimated on: a `;LOG` THETA is exponentiated and an OMEGA/SIGMA diagonal
#' becomes a CV percentage, through the same mapping [model_report()] applies to
#' the estimate. The `;LOG` annotations are read with the count check
#' `plot_nonmem_iterations()` uses, so a record whose lines are not all
#' annotated contributes no rescaling and its panels stay as NONMEM wrote them.
#'
#' Two kinds of parameter are left out by default. One that did not move across
#' the draws has no distribution to show - naming it in `parameters` puts it
#' back. An OMEGA off-diagonal is plotted as the covariance NONMEM estimated,
#' where the report's own row shows a correlation; the two therefore do not
#' match, and it is only drawn when `parameters` asks for it.
#'
#' @examples
#' \dontrun{
#' boot_run <- bbr::read_model(file.path("models", "run100-boot"))
#'
#' p <- plot_bootstrap_distributions(boot_run, "run100", spec_pk = spec_pk,
#'                                   lookup = lookup)
#' print(p)
#'
#' # Covariate effects only, from an already-tabulated set of draws
#' plot_bootstrap_distributions(bbr::bootstrap_estimates(boot_run,
#'                                                       format_long = TRUE),
#'                              "run100", spec_pk = spec_pk,
#'                              parameters = "~")
#' }
#'
#' @import ggplot2
#' @export
plot_bootstrap_distributions <- function(boot_run,
                                         model_name,
                                         models_folder = "models",
                                         spec_pk = NULL,
                                         lookup = NULL,
                                         parameters = NULL,
                                         bins = 30,
                                         ncol = NULL,
                                         footnote = NULL,
                                         wrap_width = 22) {

  if (!is.character(model_name) || length(model_name) != 1) {
    stop("`model_name` must be a single model name.", call. = FALSE)
  }

  draws <- .bootstrap_draws(boot_run)
  param_names <- unique(draws$nonmem_name)

  # suppressWarnings: extract_params() warns from an internal min() whenever the
  # control stream's last record is $THETA/$OMEGA/$SIGMA. Upstream noise.
  log_cols <- suppressWarnings(
    .listing_column_meta(param_names, model_name, models_folder)$log_cols
  )

  # Panel titles from the same labelling the report's rows carry
  report <- get_param2(model_name, count_model = 1,
                       models_folder = models_folder, spec_pk = spec_pk,
                       lookup = lookup, include_ci = TRUE)
  report <- report[!is.na(report$nonmem_name), , drop = FALSE]
  titles <- stats::setNames(as.character(report$parameter_names),
                            report$nonmem_name)

  # The final model's estimates, as estimated. Both these and the draws are put
  # on the reported scale below by one call each to the same mapping.
  model_obj <- bbr::read_model(file.path(models_folder, model_name))
  finals <- bbr::param_estimates(bbr::model_summary(model_obj))

  scale_draws <- .name_scale_inputs(draws$nonmem_name, log_cols)
  draws$value <- .to_report_scale(draws$value, scale_draws$random_effect_sd,
                                  scale_draws$trans, scale_draws$diag)

  final_tbl <- data.frame(
    nonmem_name = as.character(finals$parameter_names),
    final_estimate = suppressWarnings(as.numeric(finals$estimate)),
    stringsAsFactors = FALSE
  )
  scale_final <- .name_scale_inputs(final_tbl$nonmem_name, log_cols)
  final_tbl$final_estimate <- .to_report_scale(final_tbl$final_estimate,
                                               scale_final$random_effect_sd,
                                               scale_final$trans,
                                               scale_final$diag)

  # A draw the mapping cannot place - an OMEGA whose variance is not positive -
  # is not a value on this scale and is dropped rather than shown at zero.
  lost <- draws$nonmem_name[is.na(draws$value)]
  if (length(lost) > 0) {
    warning(sprintf(
      "%d draw(s) had no value on the reported scale and were dropped: %s.",
      length(lost), paste(sort(unique(lost)), collapse = ", ")
    ), call. = FALSE)
    draws <- draws[!is.na(draws$value), , drop = FALSE]
  }

  draws$panel_title <- ifelse(draws$nonmem_name %in% names(titles),
                              titles[draws$nonmem_name], draws$nonmem_name)

  # Selection: a parameter that never moved has no distribution, and an
  # off-diagonal is on a different scale from its own report row. Both are
  # available by name.
  spread <- vapply(split(draws$value, draws$nonmem_name),
                   function(v) length(unique(v)) > 1, logical(1))
  block_pat <- "^(OMEGA|SIGMA)[.(](\\d+)[,.](\\d+)[.)]$"
  off_diag <- grepl(block_pat, names(spread)) &
    sub(block_pat, "\\2", names(spread)) != sub(block_pat, "\\3", names(spread))
  default_keep <- names(spread)[spread & !off_diag]

  if (is.null(parameters)) {
    keep <- default_keep
    dropped <- setdiff(names(spread), keep)
    if (length(dropped) > 0) {
      message(sprintf(
        "Not plotted (no spread across draws, or an off-diagonal): %s.",
        paste(dropped[.nonmem_name_order(dropped)], collapse = ", ")
      ))
    }
  } else {
    keep <- .select_boot_parameters(parameters, draws)
  }

  draws <- draws[draws$nonmem_name %in% keep, , drop = FALSE]
  if (nrow(draws) == 0) {
    stop("No parameters left to plot.", call. = FALSE)
  }

  # Panel order is NONMEM's, and a title used twice would be collapsed into one
  # facet, so a repeated one falls back to the name it came from.
  panel_names <- unique(draws$nonmem_name)
  panel_names <- panel_names[.nonmem_name_order(panel_names)]
  panel_titles <- ifelse(panel_names %in% names(titles),
                         titles[panel_names], panel_names)
  repeated <- panel_titles %in% panel_titles[duplicated(panel_titles)]
  panel_titles[repeated] <- panel_names[repeated]

  # Wrapped last, so the duplicate check above compares the titles themselves
  # and `parameters` still matches what the caller can read.
  panel_titles <- .wrap_two_lines(panel_titles, wrap_width)

  draws$panel <- factor(
    panel_titles[match(draws$nonmem_name, panel_names)],
    levels = panel_titles
  )

  quantile_tbl <- do.call(rbind, lapply(panel_names, function(p) {
    v <- draws$value[draws$nonmem_name == p]
    data.frame(
      panel = panel_titles[match(p, panel_names)],
      q_value = stats::quantile(v, c(0.025, 0.5, 0.975), names = FALSE),
      stringsAsFactors = FALSE
    )
  }))
  quantile_tbl$panel <- factor(quantile_tbl$panel, levels = panel_titles)

  final_tbl <- final_tbl[final_tbl$nonmem_name %in% panel_names &
                           !is.na(final_tbl$final_estimate), , drop = FALSE]
  final_tbl$panel <- factor(
    panel_titles[match(final_tbl$nonmem_name, panel_names)],
    levels = panel_titles
  )

  .bootstrap_distribution_plot(draws, quantile_tbl, final_tbl, bins, ncol,
                               footnote)
}


#' Assemble the faceted bootstrap histogram
#'
#' @description The drawing half of [plot_bootstrap_distributions()], kept apart
#'   from the reading half so it can be exercised without a NONMEM run.
#' @param draws Draw table with `value` and a `panel` factor.
#' @param quantile_tbl Per-panel `q_value` rows, three to a panel.
#' @param final_tbl Per-panel `final_estimate`, one to a panel.
#' @param bins Histogram bins per panel.
#' @param ncol Panel columns, or `NULL` to let ggplot2 choose.
#' @param footnote Caller's footnote, or `NULL`.
#' @return A ggplot2 object.
#' @keywords internal
#' @noRd
.bootstrap_distribution_plot <- function(draws, quantile_tbl, final_tbl,
                                         bins = 30, ncol = NULL,
                                         footnote = NULL) {
  p <- ggplot(draws, aes(x = value)) +
    geom_histogram(bins = bins, fill = "grey60", colour = "white",
                   linewidth = 0.2) +
    geom_vline(data = quantile_tbl, aes(xintercept = q_value),
               colour = "blue", linewidth = 0.7) +
    geom_vline(data = final_tbl, aes(xintercept = final_estimate),
               colour = "red", linewidth = 0.7) +
    facet_wrap(~ panel, scales = "free", ncol = ncol) +
    labs(x = "Value", y = "Count") +
    theme_bw()

  .add_footnote(p, footnote)
}


#' Resolve a `parameters` selection against draws
#'
#' @description Each entry is matched exactly against both the NONMEM name and
#'   the panel title; an entry matching neither is retried as a regular
#'   expression against both, which is what makes `"~"` select every covariate
#'   effect and `"^THETA"` every fixed effect. An entry that still matches
#'   nothing is an error rather than a silently smaller plot.
#' @param parameters Character vector of names, titles, or patterns.
#' @param draws Draw table carrying `nonmem_name` and `panel_title`.
#' @return Character vector of `nonmem_name` values to keep.
#' @keywords internal
#' @noRd
.select_boot_parameters <- function(parameters, draws) {
  if (!is.character(parameters)) {
    stop("`parameters` must be a character vector.", call. = FALSE)
  }

  key <- unique(draws[, c("nonmem_name", "panel_title")])
  keep <- character(0)
  unmatched <- character(0)

  for (p in parameters) {
    hit <- key$nonmem_name == p | key$panel_title == p
    if (!any(hit)) {
      hit <- tryCatch(
        grepl(p, key$nonmem_name) | grepl(p, key$panel_title),
        error = function(e) rep(FALSE, nrow(key))
      )
    }
    if (any(hit)) {
      keep <- c(keep, key$nonmem_name[hit])
    } else {
      unmatched <- c(unmatched, p)
    }
  }

  if (length(unmatched) > 0) {
    stop(sprintf(
      "`parameters` matched no bootstrap parameter: %s. Available: %s",
      paste(unmatched, collapse = ", "),
      paste(key$nonmem_name[.nonmem_name_order(key$nonmem_name)],
            collapse = ", ")
    ), call. = FALSE)
  }

  unique(keep)
}
