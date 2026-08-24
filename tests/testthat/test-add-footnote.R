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
