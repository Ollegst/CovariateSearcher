long_draws <- function() {
  data.frame(
    run = rep(1:4, times = 2),
    parameter_names = rep(c("THETA1", "OMEGA(1,1)"), each = 4),
    estimate = c(2.6, 2.7, 2.5, 2.8, 0.43, 0.45, 0.41, 0.47),
    stringsAsFactors = FALSE
  )
}

wide_draws <- function() {
  data.frame(
    absolute_model_path = "some/path",
    run = 1:4,
    THETA1 = c(2.6, 2.7, 2.5, 2.8),
    `OMEGA(1,1)` = c(0.43, 0.45, 0.41, 0.47),
    error_msg = NA_character_,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

test_that(".bootstrap_draws reads the long shape bbr returns", {
  out <- .bootstrap_draws(long_draws())
  expect_equal(names(out), c("nonmem_name", "value"))
  expect_equal(nrow(out), 8)
  expect_equal(sort(unique(out$nonmem_name)), c("OMEGA(1,1)", "THETA1"))
})

test_that(".bootstrap_draws pivots the wide shape, ignoring other columns", {
  out <- .bootstrap_draws(wide_draws())
  expect_equal(nrow(out), 8)
  expect_equal(out$value[out$nonmem_name == "THETA1"], c(2.6, 2.7, 2.5, 2.8))
  expect_false(any(c("run", "error_msg") %in% out$nonmem_name))
})

test_that(".bootstrap_draws drops draws a failed run left missing", {
  d <- long_draws()
  d$estimate[c(2, 5)] <- NA
  expect_equal(nrow(.bootstrap_draws(d)), 6)
})

test_that(".bootstrap_draws refuses a table holding no parameters", {
  expect_error(.bootstrap_draws(data.frame(run = 1:3, ofv = 4:6)),
               "no parameter columns")
  expect_error(.bootstrap_draws("not a data frame"), "does not exist")
})

test_that(".name_scale_inputs routes each parameter to its own branch", {
  nm <- c("THETA1", "THETA2", "OMEGA(1,1)", "OMEGA(2,1)", "SIGMA(1,1)")
  si <- .name_scale_inputs(nm, log_cols = "THETA2")

  expect_equal(si$trans, c("RATIO", "LOG", "RATIO", "RATIO", "RATIO"))
  expect_true(all(is.na(si$random_effect_sd[1:2])))
  expect_true(all(!is.na(si$random_effect_sd[3:5])))
  expect_equal(si$diag, c(NA, NA, TRUE, FALSE, TRUE))
})

test_that(".name_scale_inputs also reads the .ext-sanitised spelling", {
  si <- .name_scale_inputs(c("OMEGA.1.1.", "OMEGA.2.1."),
                           log_cols = character(0))
  expect_equal(si$diag, c(TRUE, FALSE))
})

test_that("names and draws map onto the scale the report shows", {
  nm <- c("THETA1", "THETA2", "OMEGA(1,1)", "OMEGA(2,1)")
  value <- c(2.6, log(43.9), 0.435, 0.02)
  si <- .name_scale_inputs(nm, log_cols = "THETA2")
  out <- .to_report_scale(value, si$random_effect_sd, si$trans, si$diag)

  expect_equal(out[1], 2.6)                              # plain theta, as is
  expect_equal(round(out[2], 2), 43.9)                   # ;LOG theta, exp()
  expect_equal(round(out[3], 2), round(100 * sqrt(exp(0.435) - 1), 2))
  expect_equal(out[4], 0.02)                             # off-diagonal, as is
})

test_that(".nonmem_name_order is NONMEM's order, not alphabetical", {
  nm <- c("SIGMA(1,1)", "THETA10", "OMEGA(2,2)", "THETA2", "OMEGA(1,1)")
  expect_equal(nm[order(.nonmem_name_order(nm))],
               c("THETA2", "THETA10", "OMEGA(1,1)", "OMEGA(2,2)",
                 "SIGMA(1,1)"))
})

selection_draws <- function() {
  data.frame(
    nonmem_name = c("THETA1", "THETA2", "THETA3", "OMEGA(1,1)"),
    panel_title = c("KA (1/h)", "CL/F (L/h)", "BBILI~CL/F", "KA CV%"),
    value = 1:4,
    stringsAsFactors = FALSE
  )
}

test_that(".select_boot_parameters takes a NONMEM name or a panel title", {
  expect_equal(.select_boot_parameters("THETA1", selection_draws()), "THETA1")
  expect_equal(.select_boot_parameters("CL/F (L/h)", selection_draws()),
               "THETA2")
})

test_that(".select_boot_parameters falls back to a pattern", {
  expect_equal(.select_boot_parameters("~", selection_draws()), "THETA3")
  expect_equal(.select_boot_parameters("^THETA", selection_draws()),
               c("THETA1", "THETA2", "THETA3"))
})

test_that(".select_boot_parameters errors rather than plotting less", {
  expect_error(.select_boot_parameters("THETA9", selection_draws()),
               "matched no bootstrap parameter")
  expect_error(.select_boot_parameters(1:3, selection_draws()),
               "must be a character vector")
})

test_that(".wrap_two_lines fills the first line up to the width", {
  expect_equal(.wrap_two_lines("PPI: With concomitant PPI~Vc/F"),
               "PPI: With concomitant\nPPI~Vc/F")
  expect_equal(.wrap_two_lines("Race2: Asian~CL/F", width = 10),
               "Race2:\nAsian~CL/F")
})

test_that(".wrap_two_lines lets width decide where the break lands", {
  title <- "PPI: With concomitant PPI~Vc/F"
  expect_equal(.wrap_two_lines(title, width = 20),
               "PPI: With\nconcomitant PPI~Vc/F")
  expect_equal(.wrap_two_lines(title, width = 21),
               "PPI: With concomitant\nPPI~Vc/F")
})

test_that(".wrap_two_lines gives an over-wide first word its own line", {
  expect_equal(.wrap_two_lines("concomitant PPI~Vc/F", width = 5),
               "concomitant\nPPI~Vc/F")
})

test_that(".wrap_two_lines leaves a title that already fits", {
  expect_equal(.wrap_two_lines("KA (1/h)"), "KA (1/h)")
  expect_equal(.wrap_two_lines("CL/F (L/h)"), "CL/F (L/h)")
})

test_that(".wrap_two_lines never breaks a word and never makes a third line", {
  long_word <- paste(rep("x", 40), collapse = "")
  expect_equal(.wrap_two_lines(long_word), long_word)

  wrapped <- .wrap_two_lines("one two three four five six seven eight nine")
  expect_equal(lengths(regmatches(wrapped, gregexpr("\n", wrapped))), 1)
  expect_equal(gsub("\n", " ", wrapped),
               "one two three four five six seven eight nine")
  expect_lte(nchar(strsplit(wrapped, "\n")[[1]][1]), 22)
})

test_that(".wrap_two_lines can be switched off", {
  title <- "PPI With concomitant PPI~Vc/F"
  expect_equal(.wrap_two_lines(title, width = NULL), title)
  expect_equal(.wrap_two_lines(title, width = Inf), title)
})

plot_inputs <- function() {
  panels <- factor(c("KA (1/h)", "CL/F (L/h)"),
                   levels = c("KA (1/h)", "CL/F (L/h)"))
  draws <- data.frame(
    value = c(rnorm(60, 2.6, 0.1), rnorm(60, 31.5, 0.8)),
    panel = rep(panels, each = 60)
  )
  quantile_tbl <- data.frame(
    panel = rep(panels, each = 3),
    q_value = c(2.4, 2.6, 2.8, 30, 31.5, 33)
  )
  final_tbl <- data.frame(panel = panels, final_estimate = c(2.62, 31.4))
  list(draws = draws, quantile_tbl = quantile_tbl, final_tbl = final_tbl)
}

test_that("the plot carries three blue lines and one red line per panel", {
  set.seed(1)
  inp <- plot_inputs()
  p <- .bootstrap_distribution_plot(inp$draws, inp$quantile_tbl, inp$final_tbl)
  built <- ggplot2::ggplot_build(p)

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(built$data[[2]]), 6)
  expect_true(all(built$data[[2]]$colour == "blue"))
  expect_equal(nrow(built$data[[3]]), 2)
  expect_true(all(built$data[[3]]$colour == "red"))
})

test_that("each panel gets its own x range", {
  set.seed(1)
  inp <- plot_inputs()
  p <- .bootstrap_distribution_plot(inp$draws, inp$quantile_tbl, inp$final_tbl)
  ranges <- ggplot2::ggplot_build(p)$layout$panel_params
  expect_length(ranges, 2)
  expect_false(identical(ranges[[1]]$x.range, ranges[[2]]$x.range))
})
