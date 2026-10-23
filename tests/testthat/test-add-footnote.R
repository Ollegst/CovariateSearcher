plain_plot <- function() {
  ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
}

caption_of <- function(p) {
  ggplot2::ggplot_build(p)$plot$labels$caption
}

test_that(".add_footnote puts the caller's text under the plot", {
  p <- .add_footnote(plain_plot(), "Boxes span the IQR.")
  expect_equal(caption_of(p), "Boxes span the IQR.")
  expect_s3_class(p, "ggplot")
})

test_that(".add_footnote puts several elements on their own lines", {
  p <- .add_footnote(plain_plot(), c("First line.", "Second line."))
  expect_equal(caption_of(p), "First line.\nSecond line.")
})

test_that(".add_footnote leaves the plot untouched when there is nothing to add", {
  base <- plain_plot()
  for (empty in list(NULL, character(0), "", "   ", NA_character_)) {
    p <- .add_footnote(base, empty)
    expect_null(caption_of(p))
  }
})

test_that(".add_footnote styles the caption against the plot edge", {
  p <- .add_footnote(plain_plot(), "note")
  built <- ggplot2::ggplot_build(p)$plot$theme
  expect_equal(built$plot.caption.position, "plot")
  expect_equal(built$plot.caption$hjust, 0)
})

test_that(".add_footnote sets the caption size in points when given one", {
  sized <- ggplot2::ggplot_build(.add_footnote(plain_plot(), "note", size = 6))
  expect_equal(sized$plot$theme$plot.caption$size, 6)

  default <- ggplot2::ggplot_build(.add_footnote(plain_plot(), "note"))
  expect_equal(unclass(default$plot$theme$plot.caption$size), 0.8)
})

boxplot_captions <- function(...) {
  skip_if_not_installed("yspec")
  out <- tempfile("boxplots")
  on.exit(unlink(out, recursive = TRUE))
  set.seed(1)
  dat <- data.frame(WT = runif(40, 50, 100), AUC = rlnorm(40, 7, 0.3))
  res <- create_covariate_boxplots(
    data = dat, spec = yspec::ys_help$spec(), con = "WT", type = "AUC",
    drug = "DrugX", param_info = list(AUC = list(label = "AUC", unit = "h")),
    total_panel = FALSE, output_folder = out, combined_pdf = FALSE,
    verbose = FALSE, ...
  )
  ggplot2::ggplot_build(res$AUC$WT)$plot
}

test_that("create_covariate_boxplots appends the Source line by default", {
  built <- boxplot_captions(footnote = "My note.", output_format = "png")
  expect_match(built$labels$caption, "^My note\\.\nSource: .*DrugX-WT-AUC_ss\\.png$")
})

test_that("with several formats the returned plot names only the first file", {
  skip_if_not_installed("devEMF")
  caption <- boxplot_captions(output_format = c("emf", "png"))$labels$caption
  expect_match(caption, "^Source: .*DrugX-WT-AUC_ss\\.emf$")
  expect_no_match(caption, "\\.png")
})

test_that("source_note = FALSE drops the Source line", {
  expect_equal(boxplot_captions(footnote = "My note.", source_note = FALSE,
                                output_format = "png")$labels$caption,
               "My note.")
  expect_null(boxplot_captions(source_note = FALSE,
                               output_format = "png")$labels$caption)
})

test_that("footnote_size sets the caption size in points", {
  built <- boxplot_captions(footnote_size = 5, output_format = "png")
  expect_equal(built$theme$plot.caption$size, 5)
})

test_that("the bootstrap plot carries a footnote through", {
  panels <- factor(c("KA (1/h)", "CL/F (L/h)"),
                   levels = c("KA (1/h)", "CL/F (L/h)"))
  draws <- data.frame(value = c(1:30, 31:60), panel = rep(panels, each = 30))
  quantile_tbl <- data.frame(panel = rep(panels, each = 3),
                             q_value = c(2, 15, 29, 32, 45, 59))
  final_tbl <- data.frame(panel = panels, final_estimate = c(14, 44))

  p <- .bootstrap_distribution_plot(draws, quantile_tbl, final_tbl,
                                    footnote = "300 bootstrap replicates.")
  expect_equal(caption_of(p), "300 bootstrap replicates.")

  bare <- .bootstrap_distribution_plot(draws, quantile_tbl, final_tbl)
  expect_null(caption_of(bare))
})

test_that("footnote is an argument of both plotting functions", {
  expect_true("footnote" %in% names(formals(plot_bootstrap_distributions)))
  expect_true("footnote" %in% names(formals(create_covariate_boxplots)))
  expect_null(eval(formals(plot_bootstrap_distributions)$footnote))
  expect_null(eval(formals(create_covariate_boxplots)$footnote))
})
