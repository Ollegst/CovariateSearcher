# =============================================================================
# EXPOSURE FOREST PLOT
# File: R/plot-exposure-forest.R
# Part of CovariateSearcher Package
# Plotting step of the exposure-metrics forest-plot workflow.
# =============================================================================

# Column names used inside dplyr / ggplot2 data-masking.
utils::globalVariables(c("Scenario", "VALUE", "NAME", "COLOR", "MIN", "MAX",
                         "band", "xmin", "xmax", "ymin", "ymax"))

#' Forest Plot of a Covariate Effect on an Exposure Metric
#'
#' @description
#' Builds a forest plot of one exposure metric's covariate effects: each
#' scenario's metric distribution is normalised to the typical subject's median
#' and shown as a box (2.5 / 25 / 50 / 75 / 97.5 percentiles), with a reference
#' line at 1 and a shaded clinical-relevance band. Consumes an
#' already-calculated metrics table (`ID`, `AUC`, `Cmax`, `Cmin`, `Scenario`) -
#' e.g. a `group_by(Scenario, ID) %>% summarise(...)` result, saved to RDS or in
#' memory.
#'
#' @param data Data frame with a `Scenario` column and the metric column
#'   (`AUC`, `Cmax`, or `Cmin`), one row per sample per scenario.
#' @param metric Character. Which metric to plot: "AUC", "Cmax", or "Cmin".
#' @param ss Logical. Steady state? Adds an "ss" suffix to the metric in the
#'   title (e.g. "AUC" vs "AUCss"). Default `TRUE`.
#' @param ClinicalRelevanceLow,ClinicalRelevanceHigh Numeric. Bounds of the
#'   shaded clinical-relevance band (ratio scale). Defaults `0.8`, `1.25`.
#' @param reference Character. Scenario used as the reference (denominator of the
#'   ratio and highlighted colour). Default "Typical subject".
#' @param scenario_order Optional character vector giving the scenario order on
#'   the axis, overriding the default. When `NULL` (default), the factor levels
#'   of `data$Scenario` are used if it is a factor - which
#'   [simulate_scenario_profiles()] sets to the scenario definition order, so the
#'   correct order flows through automatically - otherwise the order scenarios
#'   first appear in `data`.
#' @param fontsize Numeric. Base font size. Default 9.
#' @param title Character or NULL. Plot title; when NULL it is built from
#'   `metric` and `ss` ("Covariate effects on <metric>[ss]").
#' @param filename Character or NULL. Base path for saved output; any extension
#'   is stripped and one file per `output_format` is written as
#'   `<stem>.<format>`. `NULL` (default) does not save.
#' @param output_format Character. Which format(s) to save when `filename` is
#'   given: one or both of `"emf"` and `"png"`. Default both. `.emf` is written
#'   with `devEMF::emf`, `.png` with the [ggplot2::ggsave()] default device.
#' @param width,height Numeric. Output size in inches. Mandatory (no default);
#'   required whenever `filename` is given.
#'
#' @return The ggplot object (invisibly saved to `filename` when supplied).
#'
#' @seealso [theme_forest()]
#' @examples
#' \dontrun{
#' raw <- readRDS("data/derived/exposure_metrics_run19.rds")
#' for (m in c("AUC", "Cmax", "Cmin")) {
#'   plot_exposure_forest(
#'     raw, metric = m, ss = TRUE,
#'     filename = paste0("results/run19/", m, "ss_forestplot"),
#'     width = 6, height = 6
#'   )
#' }
#' }
#' @import ggplot2
#' @export
plot_exposure_forest <- function(data,
                                 metric = c("AUC", "Cmax", "Cmin"),
                                 ss = TRUE,
                                 ClinicalRelevanceLow = 0.8,
                                 ClinicalRelevanceHigh = 1.25,
                                 reference = "Typical subject",
                                 scenario_order = NULL,
                                 fontsize = 9,
                                 title = NULL,
                                 filename = NULL,
                                 output_format = c("emf", "png"),
                                 width,
                                 height) {

  metric <- match.arg(metric)
  output_format <- match.arg(output_format, c("emf", "png"), several.ok = TRUE)
  if (!all(c("Scenario", metric) %in% names(data))) {
    stop("`data` must contain a 'Scenario' column and a '", metric, "' column.")
  }
  if (!reference %in% data$Scenario) {
    stop("Reference scenario '", reference, "' not found in `data$Scenario`.")
  }

  # Reference median (typical subject) -> normalise the metric to a ratio.
  ref_val <- stats::median(data[[metric]][data$Scenario == reference],
                           na.rm = TRUE)
  if (!is.finite(ref_val) || ref_val == 0) {
    stop("Reference median for '", metric, "' at scenario '", reference,
         "' is missing or zero.")
  }

  # Y-axis scenario order: explicit `scenario_order` override, else the factor
  # levels of data$Scenario (set by simulate_scenario_profiles()), else the order
  # scenarios first appear in the data.
  scen_levels <- if (!is.null(scenario_order)) {
    as.character(scenario_order)
  } else if (is.factor(data$Scenario)) {
    levels(data$Scenario)
  } else {
    unique(as.character(data$Scenario))
  }

  d <- data
  d$VALUE <- d[[metric]] / ref_val
  d <- dplyr::group_by(d, Scenario)
  d <- dplyr::mutate(
    d,
    MIN   = as.numeric(stats::quantile(VALUE, 0.025, na.rm = TRUE)),
    MAX   = as.numeric(stats::quantile(VALUE, 0.975, na.rm = TRUE)),
    NAME  = Scenario,
    COLOR = dplyr::case_when(
      Scenario == reference ~ "Typical value",
      .default = "Parameter\nfor defined\ncategory"
    )
  )
  d <- dplyr::ungroup(d)

  ttl <- if (is.null(title)) {
    paste0("Covariate effects on ", metric, if (isTRUE(ss)) "ss" else "")
  } else {
    title
  }
  boxlabel <- paste0(
    "Change in ", metric, " relative to reference individual\n",
    "Box plots: 2.5% / 25% / 50% / 75% / 97.5%"
  )

  # Shaded reference bands (behind the boxes), mapped to `fill` so they get
  # legend swatches next to the boxplot colours. Outer band listed first so it is
  # drawn underneath the (narrower) inner band.
  inner_lab <- paste0("Reference range\n", ClinicalRelevanceLow, "-",
                      ClinicalRelevanceHigh)
  outer_lab <- "Reference range\n0.5-2"
  bands <- data.frame(
    xmin = -Inf, xmax = Inf,
    ymin = c(0.5, ClinicalRelevanceLow),
    ymax = c(2,   ClinicalRelevanceHigh),
    band = factor(c(outer_lab, inner_lab), levels = c(inner_lab, outer_lab))
  )
  fill_values <- stats::setNames(
    c("#185AA9", "#7AC36A", "darkgrey", "lightgrey"),
    c("Typical value", "Parameter\nfor defined\ncategory", inner_lab, outer_lab)
  )

  p <- ggplot() +
    theme_forest(base_size = fontsize) +
    geom_rect(data = bands,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                  fill = band),
              alpha = 0.4) +
    stat_summary(
      data = d, fun.data = boxquantile, geom = "boxplot",
      aes(x = NAME, y = VALUE, fill = COLOR),
      width = 0.2, alpha = 0.5, na.rm = TRUE
    ) +
    scale_fill_manual(NULL, values = fill_values,
                      breaks = names(fill_values)) +
    scale_x_discrete(limits = rev(scen_levels)) +
    geom_hline(yintercept = 1, linetype = 2) +
    theme(
      legend.position   = "bottom",
      legend.background = element_rect(linetype = "solid", color = "black")
    ) +
    ylab(boxlabel) +
    xlab(NULL) +
    coord_flip(ylim = c(min(d$MIN, na.rm = TRUE) - 0.2,
                        max(d$MAX, na.rm = TRUE) + 0.2)) +
    ggtitle(ttl) +
    theme(
      plot.subtitle = element_text(size = fontsize),
      axis.text     = element_text(size = fontsize)
    )

  if (!is.null(filename)) {
    stem <- tools::file_path_sans_ext(filename)
    for (fmt in output_format) {
      out_file <- paste0(stem, ".", fmt)
      if (fmt == "emf") {
        ggsave(out_file, plot = p, device = devEMF::emf,
               width = width, height = height)
      } else {
        ggsave(out_file, plot = p, width = width, height = height)
      }
    }
  }

  p
}


# -----------------------------------------------------------------------------
# Support functions
# -----------------------------------------------------------------------------

#' Forest-plot ggplot2 Theme
#'
#' @description A clean black-on-white `theme_bw()` variant used for the covariate
#'   / exposure forest plots (inward ticks, bordered panel, black text).
#' @param base_size Base font size. Default 12.
#' @param base_family Base font family. Default "".
#' @return A ggplot2 theme object to add to a plot.
#' @import ggplot2
#' @export
theme_forest <- function(base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      line             = element_line(colour = "black"),
      rect             = element_rect(fill = "white", colour = NA),
      text             = element_text(colour = "black"),
      axis.text        = element_text(size = rel(1), colour = "black"),
      axis.text.x      = element_text(margin = grid::unit(c(4, 4, 0, 4), "mm")),
      axis.text.y      = element_text(margin = grid::unit(c(4, 4, 4, 0), "mm")),
      axis.ticks       = element_line(colour = "black"),
      axis.ticks.length = grid::unit(-2, "mm"),
      legend.key       = element_rect(colour = NA),
      panel.border     = element_rect(colour = "black"),
      strip.background = element_rect(fill = "white", colour = NA),
      strip.text       = element_text(size = rel(1))
    )
}


#' Box summary at the 2.5 / 25 / 50 / 75 / 97.5 percentiles
#'
#' @description Summary function for [ggplot2::stat_summary()] with
#'   `geom = "boxplot"`: returns the box statistics named as ggplot expects
#'   (`ymin`, `lower`, `middle`, `upper`, `ymax`).
#' @param y Numeric vector.
#' @return A named numeric vector of length 5.
#' @keywords internal
#' @noRd
boxquantile <- function(y) {
  structure(
    stats::quantile(y, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE),
    names = c("ymin", "lower", "middle", "upper", "ymax")
  )
}
