test_that(".reformat_boot_ci splits a hyphenated interval, negatives too", {
  expect_equal(.reformat_boot_ci("[65.2-81.1]"), "[65.2, 81.1]")
  expect_equal(.reformat_boot_ci("[-0.23--0.15]"), "[-0.23, -0.15]")
  expect_equal(.reformat_boot_ci("[0-0]"), "[0, 0]")
  expect_equal(.reformat_boot_ci("[1e-05-2e-05]"), "[1e-05, 2e-05]")
})

test_that(".reformat_boot_ci takes commas and bare intervals, junk passes on", {
  expect_equal(.reformat_boot_ci("[3.14, 3.71]"), "[3.14, 3.71]")
  expect_equal(.reformat_boot_ci("65.2-81.1"), "[65.2, 81.1]")
  expect_equal(.reformat_boot_ci("not an interval"), "not an interval")
  expect_true(is.na(.reformat_boot_ci(NA)))
})

test_that(".format_ci keeps three significant digits per bound", {
  expect_equal(.format_ci(-0.244123, -0.084612), "[-0.244, -0.0846]")
  expect_equal(.format_ci(197.4612, 219.0587), "[197, 219]")
})

test_that(".format_ci is NA when a bound is missing", {
  expect_true(is.na(.format_ci(NA_real_, 1)))
  expect_true(is.na(.format_ci(1, NA_real_)))
})

test_that(".to_report_scale maps an interval into the estimate's own units", {
  # A LOG theta is exponentiated; an OMEGA diagonal becomes a CV percentage.
  est <- log(43.9)
  se <- 0.0281
  bounds <- .to_report_scale(est + c(-1.96, 1.96) * se, c(NA, NA),
                             c("LOG", "LOG"), c(NA, NA))
  expect_equal(round(bounds, 2), round(exp(est + c(-1.96, 1.96) * se), 2))

  omega <- .to_report_scale(0.435, 0.66, NA_character_, TRUE)
  expect_equal(round(omega, 2), round(100 * sqrt(exp(0.435) - 1), 2))
})

test_that(".to_report_scale keeps a lower bound below its upper bound", {
  lo <- .to_report_scale(0.2, 0.66, NA_character_, TRUE)
  hi <- .to_report_scale(0.5, 0.66, NA_character_, TRUE)
  expect_lt(lo, hi)
})

test_that("an OMEGA whose interval reaches a negative variance has no CV%", {
  # exp(v) - 1 is negative for v < 0, which is not a CV. The estimate itself is
  # never affected - a variance is positive - but a wide interval around a small
  # one is, and the whole interval then reads as blank rather than as a bound
  # that does not exist.
  expect_true(is.na(.to_report_scale(-0.5, 0.66, NA_character_, TRUE)))
  expect_true(is.na(.format_ci(
    .to_report_scale(-0.5, 0.66, NA_character_, TRUE),
    .to_report_scale(1.0, 0.66, NA_character_, TRUE)
  )))
})

boot_fixture <- function() {
  data.frame(
    parameter_names = c("OMEGA(1,1)", "SIGMA(1,1)", "THETA1", "THETA10",
                        "THETA2"),
    n = 300,
    median = c(71.63, 1.00, 3.39, -0.19, 209.24),
    `95% CI-bootstrap` = c("[65.2-81.1]", "[1-1]", "[3.14-3.71]",
                           "[-0.23--0.15]", "[197.46-219.06]"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

report_fixture <- function() {
  data.frame(
    nonmem_name = c("THETA1", "THETA2", "THETA10", "OMEGA(1,1)", NA),
    parameter_names = c("Ka (1/h)", "Vc/F (L)", "WT~CL", "KA CV%", "OFV"),
    stringsAsFactors = FALSE
  )
}

test_that(".join_bootstrap_results matches on NONMEM names, not on row order", {
  out <- .join_bootstrap_results(report_fixture(), boot_fixture())

  # THETA10 precedes THETA2 in the bootstrap table and must not steal its row.
  expect_equal(out$boot_median[out$nonmem_name %in% "THETA2"], 209.24)
  expect_equal(out$boot_ci[out$nonmem_name %in% "THETA10"], "[-0.23, -0.15]")
  expect_equal(out$boot_ci[out$nonmem_name %in% "OMEGA(1,1)"], "[65.2, 81.1]")
})

test_that(".join_bootstrap_results empties rows the bootstrap does not cover", {
  out <- .join_bootstrap_results(report_fixture(), boot_fixture())
  ofv <- out[out$parameter_names == "OFV", ]
  expect_true(is.na(ofv$boot_median))
  expect_true(is.na(ofv$boot_ci))
  expect_equal(nrow(out), nrow(report_fixture()))
})

test_that(".join_bootstrap_results names bootstrap rows the report has not", {
  expect_output(
    .join_bootstrap_results(report_fixture(), boot_fixture()),
    "SIGMA\\(1,1\\)"
  )
})

test_that(".join_bootstrap_results finds its columns case-insensitively", {
  boot <- boot_fixture()
  names(boot) <- c("Parameter", "N", "Median", "95% ci-bootstrap")
  out <- .join_bootstrap_results(report_fixture(), boot)
  expect_equal(out$boot_median[out$nonmem_name %in% "THETA1"], 3.39)
})

test_that(".join_bootstrap_results refuses duplicated or incomplete input", {
  boot <- boot_fixture()
  expect_error(
    .join_bootstrap_results(report_fixture(), rbind(boot, boot[1, ])),
    "more than one row"
  )
  expect_error(.join_bootstrap_results(report_fixture(), boot[, 1:3]),
               "confidence interval")
  names(boot)[1] <- "thing"
  expect_error(.join_bootstrap_results(report_fixture(), boot),
               "parameter name")
})

test_that("model_report rejects a bootstrap request it cannot honour", {
  expect_error(model_report(c("run100", "run101"), bootstrap = TRUE,
                            boot_results = boot_fixture()),
               "one model against its bootstrap")
  expect_error(model_report("run100", bootstrap = TRUE), "requires `boot_results`")
})
