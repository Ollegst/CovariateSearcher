# =============================================================================
# COVARIATE EXPOSURE BOXPLOTS
# File: R/covariate-boxplots.R
# Part of CovariateSearcher Package
# Boxplots of an exposure metric (AUC / Cmax / Cmin) across covariate groups.
# =============================================================================

# Names used inside ggplot2 data-masking (facet/label mappings, after_stat(y)).
utils::globalVariables(c("y", "facet_grp", "xlev", "Freq"))

#' Create Covariate Boxplots for AUC / Cmax / Cmin
#'
#' @description
#' Builds boxplots of an exposure metric across covariate groups. Continuous
#' covariates are binned into quartile groups; categorical covariates use their
#' (decoded) factor levels. Each box is annotated with its median value at the
#' median level, and (when `total_panel = TRUE`) an extra right-most `Total`
#' panel pools all data as a reference. Optionally splits into side-by-side
#' panels by a stratification column, and can also emit a single multi-page PDF
#' of every plot.
#'
#' Covariate axis labels come straight from the yspec `spec` via
#' [yspec::ys_get_short_unit()] ("Short (unit)"). Categorical covariates are
#' expected to be decoded already (via [yspec::decode_dataset()]); if a
#' categorical covariate still holds raw numeric codes the function stops and
#' tells you how to decode.
#'
#' @param data One row per subject/replicate, containing columns for
#'   AUC/Cmax/Cmin (as named in `param_info`) and all baseline covariates listed
#'   in `con`/`cat`; categorical covariates should already be decoded (see
#'   [yspec::decode_dataset()]). Given as either a `data.frame` OR a character
#'   path to a `.csv`/`.rds` file to load.
#' @param spec A loaded yspec object (from [yspec::ys_load()]), or a path to
#'   the spec YAML file (loaded with [yspec::ys_load()]); used for the
#'   "Short (unit)" covariate axis labels.
#' @param con character vector of continuous covariate column names.
#' @param cat character vector of categorical covariate column names.
#' @param type character vector of the metric column(s) of `data` to plot
#'   (default `c("AUC","Cmax","Cmin")`); any numeric column works, and each name
#'   must have an entry in `param_info`.
#' @param drug character, full drug name used in titles AND filenames
#'   (e.g. "Camizestrant").
#' @param param_info Metric labels/units, keyed by the `type` names, as EITHER a
#'   named list `list(label=, unit=)` per metric (e.g.
#'   `list(AUC = list(label="AUCss", unit="mg*hr/L"))`) OR a spec supplying them
#'   - a yspec object, a path to a spec YAML, or a spec-shaped list (each entry's
#'   `short` becomes the label, plus its `unit`). Every `type` must have an
#'   entry or the call stops.
#' @param stratification optional column name (string) in `data` to split
#'   plots into side-by-side panels (e.g. "COMB"). NULL = single panel.
#' @param total_panel logical; if TRUE (default) add a right-most `Total` panel
#'   pooling all subjects as a reference. FALSE drops it.
#' @param output_folder base output directory. Default
#'   "results/figure/simulations". AUC/Cmax/Cmin subfolders are created
#'   inside it as needed.
#' @param width,height optional numeric; if both supplied, overrides the
#'   automatic sizing logic based on number of stratification levels.
#' @param base_size base font size passed to theme_bw().
#' @param verbose logical; if TRUE (default) prints progress messages
#'   ("[i/n] AUC: processing WT ... done -> path/to/file.emf") as each
#'   parameter type / covariate combination is processed.
#' @param combined_pdf logical; if TRUE (default), also saves a single
#'   multi-page PDF containing every plot generated in this call (one page
#'   per parameter type x covariate combination, in the order processed),
#'   directly under `output_folder`. Filename is
#'   "<prefix><drug>-<type1>-<type2>-...-<HHMM>.pdf", e.g.
#'   "Camizestrant-AUC-Cmax-Cmin-1405.pdf", where HHMM is the current time.
#' @param prefix optional string prepended to every output filename (both the
#'   per-plot image files and the combined PDF), so different simulation runs
#'   do not overwrite each other in the same folder. `NULL`/`""` (default) adds
#'   nothing. E.g. `prefix = "run19"` gives
#'   "run19-<drug>-<covariate>-<type>.emf".
#' @param output_format character vector, subset of c("emf","png"); which
#'   image format(s) to save each per-plot figure as. Default both. `.emf` is
#'   written with [devEMF::emf()], `.png` with the [ggplot2::ggsave()] default
#'   device. (The combined multi-page file is always a PDF.)
#'
#' @return A nested list: results[[type]][[covariate]] = the faceted ggplot
#'   object (also saved to disk as .emf/.png per `output_format`). Returned
#'   invisibly.
#'
#' @examples
#' \dontrun{
#' spec <- yspec::ys_load(here::here("data", "spec", "lookup.yml"))
#' flags <- yspec::pull_meta(spec, "flags")
#' dat  <- yspec::decode_dataset(dat, spec, c(flags$catcov))
#'
#' param_info <- list(
#'   AUC  = list(label = "AUCss",   unit = "mg*hr/L"),
#'   Cmax = list(label = "Cmax_ss", unit = "ng/mL"),
#'   Cmin = list(label = "Cmin_ss", unit = "ng/mL")
#' )
#' results <- create_covariate_boxplots(
#'   data           = dat,
#'   spec           = spec,
#'   con            = c("WT", "AGE", "CRCLI"),
#'   cat            = c("RACEN", "SEXN", "ECOGBL"),
#'   type           = c("AUC", "Cmax"),
#'   drug           = "Camizestrant",
#'   param_info     = param_info,
#'   stratification = "COMB"
#' )
#' }
#' @import ggplot2
#' @export
create_covariate_boxplots <- function(data,
                                      spec,
                                      con = NULL,
                                      cat = NULL,
                                      type = c("AUC", "Cmax", "Cmin"),
                                      drug,
                                      param_info,
                                      stratification = NULL,
                                      total_panel = TRUE,
                                      output_folder = "results/figure/simulations",
                                      width = NULL,
                                      height = NULL,
                                      base_size = 10,
                                      verbose = TRUE,
                                      combined_pdf = TRUE,
                                      prefix = NULL,
                                      output_format = c("emf", "png")) {

  type <- as.character(type)
  output_format <- match.arg(output_format, choices = c("emf", "png"),
                             several.ok = TRUE)

  # data may be given as an object OR a character path to load (.csv/.rds).
  # (spec is likewise accepted as an object or a path, below.)
  data <- .load_if_path(data, "data")

  # param_info may be a named list(label, unit) per metric OR a spec (yspec
  # object / path / spec-shaped list) supplying label (`short`) + unit per
  # metric - keyed by the `type` names.
  param_info <- .resolve_quantity_info(param_info)

  # Optional filename prefix so different simulation runs do not overwrite each
  # other in the same folder (e.g. prefix = "run19" -> run19-<drug>-<cov>-<type>).
  pfx <- if (is.null(prefix) || !nzchar(trimws(prefix))) "" else paste0(trimws(prefix), "-")

  if (is.null(con) && is.null(cat)) {
    stop("At least one of `con` or `cat` must be supplied.")
  }

  bad_type <- setdiff(type, names(data))
  if (length(bad_type) > 0) {
    stop("`type` column(s) not found in `data`: ", paste(bad_type, collapse = ", "))
  }
  missing_params <- setdiff(type, names(param_info))
  if (length(missing_params) > 0) {
    stop("param_info/spec has no entry (label + unit) for: ",
         paste(missing_params, collapse = ", "))
  }

  all_covs <- c(con, cat)
  missing_cols <- setdiff(all_covs, names(data))
  if (length(missing_cols) > 0) {
    stop("Covariate(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }
  if (!is.null(stratification) && !stratification %in% names(data)) {
    stop("Stratification column '", stratification, "' not found in data.")
  }

  # Categorical covariates (and a categorical stratification column) must be
  # decoded, i.e. NOT raw numeric codes. Fail fast with the decode recipe.
  needs_decode <- c(cat, if (!is.null(stratification)) stratification)
  raw_coded <- needs_decode[vapply(needs_decode,
                                    function(v) is.numeric(data[[v]]),
                                    logical(1))]
  if (length(raw_coded) > 0) {
    stop("Categorical column(s) still hold raw numeric codes: ",
         paste(raw_coded, collapse = ", "),
         ".\n  Decode first, e.g.:  dat <- decode_dataset(dat, spec, c(flags$catcov))")
  }

  # Accept either a loaded yspec object or a path to the spec YAML file.
  if (is.character(spec)) {
    spec <- yspec::ys_load(spec)
  }

  # "Short (unit)" axis labels, straight from the spec.
  covlab <- yspec::ys_get_short_unit(spec, title_case = TRUE)

  if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
  for (t in type) {
    dir.create(file.path(output_folder, t), showWarnings = FALSE, recursive = TRUE)
  }

  # Stratification panel order (decoded factor levels, else appearance order);
  # NULL when not stratifying. Panels run left-to-right: one per stratum (a
  # single blank panel when not stratifying), then - when `total_panel` - a
  # separate right-hand "Total" panel that pools ALL data.
  strat_levels <- if (!is.null(stratification)) {
    s <- data[[stratification]]
    if (is.factor(s)) levels(droplevels(s)) else unique(as.character(s))
  } else {
    NULL
  }
  panel_levels <- c(if (is.null(strat_levels)) "" else strat_levels,
                    if (isTRUE(total_panel)) "Total")

  # Figure size: explicit width/height override, else scale to the panel count
  # (all panels sit in one row: strata panels + the Total panel).
  if (!is.null(width) && !is.null(height)) {
    fig_w <- width
    fig_h <- height
  } else {
    fig_w <- 3.0 * length(panel_levels) + 1.5
    fig_h <- 4.5
  }

  results <- list()
  n_covs <- length(all_covs)

  for (t in type) {

    pinfo <- param_info[[t]]
    # Unit is used verbatim from param_info (e.g. "µg·day/mL"); no symbol
    # substitution here. The combined PDF uses cairo_pdf (below) so the unit's
    # unicode renders there.
    y_label_text <- paste0(drug, " ", pinfo$label, ", ", pinfo$unit)
    results[[t]] <- list()

    if (verbose) {
      cat(sprintf("\n=== Processing %s (%d covariates) ===\n", t, n_covs))
      flush(stdout())
    }

    for (i in seq_along(all_covs)) {

      covariate <- all_covs[i]
      is_cont <- covariate %in% con

      if (verbose) {
        cat(sprintf("  [%d/%d] %s: processing %s ...\n", i, n_covs, t, covariate))
        flush(stdout())
      }

      pdata <- data
      x_lab <- if (covariate %in% names(covlab)) covlab[[covariate]] else covariate

      # Continuous -> quartile groups labelled "Qk:[lo,hi]" (e.g.
      # "Q1:[34.2,61.3]"); `cut()` picks enough digits to keep breaks distinct.
      if (is_cont) {
        br <- unique(stats::quantile(pdata[[covariate]],
                                     probs = c(0, 0.25, 0.5, 0.75, 1),
                                     na.rm = TRUE))
        if (length(br) < 2) {
          pdata[[covariate]] <- factor(format(signif(br[1], 3), trim = TRUE))
        } else {
          grp <- cut(pdata[[covariate]], breaks = br,
                     include.lowest = TRUE, dig.lab = 4)
          iv  <- sub("^\\(", "[", levels(grp))        # square-bracket every bin
          levels(grp) <- paste0("Q", seq_along(iv), ":", iv)
          pdata[[covariate]] <- grp
        }
      } else {
        pdata[[covariate]] <- droplevels(as.factor(pdata[[covariate]]))
      }

      grp_levels <- levels(pdata[[covariate]])

      # Facet-panel column: the stratum (blank when not stratifying) for the
      # covariate-group rows, plus - when `total_panel` - a "Total" copy of EVERY
      # row (all strata pooled) placed in its own right-hand "Total" panel.
      pdata$facet_grp <- if (!is.null(stratification)) {
        as.character(pdata[[stratification]])
      } else {
        ""
      }
      pdata[[covariate]] <- as.character(pdata[[covariate]])
      if (isTRUE(total_panel)) {
        ref_rows <- pdata
        ref_rows$facet_grp    <- "Total"
        ref_rows[[covariate]] <- "All subjects"
        alld <- rbind(pdata, ref_rows)
        x_levels <- c(grp_levels, "All subjects")
      } else {
        alld <- pdata
        x_levels <- grp_levels
      }
      alld[[covariate]] <- factor(alld[[covariate]], levels = x_levels)
      alld$facet_grp    <- factor(alld$facet_grp, levels = panel_levels)

      # N per (panel, box) for the "N=" labels under each box.
      valid <- alld[!is.na(alld[[covariate]]) & !is.na(alld[[t]]), , drop = FALSE]
      cnt <- as.data.frame(
        table(facet_grp = valid$facet_grp, xlev = valid[[covariate]]),
        stringsAsFactors = FALSE
      )
      cnt <- cnt[cnt$Freq > 0, , drop = FALSE]
      cnt$facet_grp <- factor(cnt$facet_grp, levels = panel_levels)
      cnt$xlev      <- factor(cnt$xlev, levels = c(grp_levels, "All subjects"))

      # Boxes with all raw points jittered behind, the median value in a small
      # box on the median line, and N under each box; strata + REF as panels.
      combined <- ggplot(alld, aes(x = .data[[covariate]], y = .data[[t]])) +
        geom_jitter(width = 0.18, height = 0, size = 0.4, alpha = 0.3,
                    colour = "royalblue", na.rm = TRUE) +
        geom_boxplot(width = 0.6, fill = NA, colour = "grey20",
                     outlier.shape = NA, na.rm = TRUE) +
        stat_summary(
          fun   = stats::median,
          geom  = "label",
          aes(label = format(signif(after_stat(y), 3), trim = TRUE, big.mark = ",")),
          vjust = 0.5,                              # box centred on the median line
          size  = base_size / .pt * 0.65,           # a bit smaller than the axis text
          label.size = 0.25,                        # thin border box around the number
          label.padding = grid::unit(0.12, "lines"),
          fill = "white",
          colour = "grey15",
          na.rm = TRUE
        ) +
        geom_text(data = cnt, aes(x = xlev, label = paste0("n = ", Freq)),
                  y = -Inf, vjust = -0.5, size = base_size / .pt * 0.6,
                  colour = "grey30", inherit.aes = FALSE) +
        facet_grid(cols = vars(facet_grp), scales = "free_x", space = "free_x") +
        scale_y_continuous(expand = expansion(mult = c(0.10, 0.05))) +
        labs(x = x_lab, y = y_label_text) +
        theme_bw(base_size = base_size) +
        theme(panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1))

      # Without stratification the left panel is simply "the covariate" and
      # needs no strip label; the Total panel stays identified by its
      # "All subjects" tick.
      if (is.null(stratification)) {
        combined <- combined +
          theme(strip.background = element_blank(), strip.text = element_blank())
      }

      results[[t]][[covariate]] <- combined

      base_name <- paste0(pfx, drug, "-", covariate, "-", t)
      saved <- character(0)
      for (fmt in output_format) {
        fname <- file.path(output_folder, t, paste0(base_name, ".", fmt))
        if (fmt == "emf") {
          ggsave(filename = fname, plot = combined, device = devEMF::emf,
                 width = fig_w, height = fig_h)
        } else {
          ggsave(filename = fname, plot = combined,
                 width = fig_w, height = fig_h)
        }
        saved <- c(saved, fname)
      }

      if (verbose) {
        cat(sprintf("    done -> %s\n", paste(saved, collapse = ", ")))
        flush(stdout())
      }
    }
  }

  if (verbose) {
    cat(sprintf("\nAll done. %d parameter type(s) x %d covariate(s) saved under '%s'.\n",
                length(type), n_covs, output_folder))
    flush(stdout())
  }

  if (combined_pdf) {
    hhmm <- format(Sys.time(), "%H%M")
    pdf_name <- paste0(pfx, drug, "-", paste(type, collapse = "-"), "-", hhmm, ".pdf")
    pdf_path <- file.path(output_folder, pdf_name)

    if (verbose) {
      cat(sprintf("Writing combined PDF -> %s\n", pdf_path))
      flush(stdout())
    }

    # cairo_pdf (not base pdf) so unicode in units renders; onefile = TRUE keeps
    # it a single multi-page file.
    grDevices::cairo_pdf(pdf_path, width = fig_w, height = fig_h, onefile = TRUE)
    for (t in type) {
      for (covariate in all_covs) {
        print(results[[t]][[covariate]])
      }
    }
    grDevices::dev.off()

    if (verbose) {
      cat(sprintf("Combined PDF done -> %s\n", pdf_path))
      flush(stdout())
    }
  }

  invisible(results)
}
