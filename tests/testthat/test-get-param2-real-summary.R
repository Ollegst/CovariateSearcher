# get_param2() driven with bbr's own captured model summary (bbr/test-refs,
# the basic/1 example: five THETAs, a BLOCK(2) OMEGA, one FIX SIGMA). Only
# bbr's three accessors are replaced, so every table rule is the package's own.

real_summary <- function() {
  ref <- file.path(system.file("test-refs", package = "bbr"), "1_summary_obj.R")
  skip_if_not(file.exists(ref), "bbr test-refs not installed")
  eval(parse(ref))
}

# A control stream annotated to match the summary's parameters, and the
# example's .cor so the condition number can be read. The summary's OMEGA(2,1)
# is FIX (a diagonal $OMEGA); `block = TRUE` writes it as an estimated BLOCK(2)
# for tests that un-fix it.
write_basic_model <- function(block = FALSE, ruva_trans = "RATIO") {
  models_dir <- tempfile("models")
  dir.create(file.path(models_dir, "b1"), recursive = TRUE)
  omega <- if (block) {
    c("$OMEGA BLOCK(2)",
      "0.05 ; IIV_CL ; - ; RATIO",
      "0.01 ; COR_CL_V2 ; - ; RATIO",
      "0.2  ; IIV_V2 ; - ; RATIO")
  } else {
    c("$OMEGA",
      "0.05 ; IIV_CL ; - ; RATIO",
      "0.2  ; IIV_V2 ; - ; RATIO")
  }
  writeLines(c(
    "$PROBLEM basic", "$THETA",
    "(0, 2)  ; KA ; 1/h ; RATIO",
    "(0, 3)  ; CL ; L/h ; RATIO",
    "(0, 10) ; V2 ; L ; RATIO",
    "(0.02)  ; RUVp ; - ; RATIO",
    paste("(1)     ; RUVa ; - ;", ruva_trans),
    omega,
    "$SIGMA", "1 FIX ; SIG ; - ; RATIO",
    "$EST METHOD=1"
  ), file.path(models_dir, "b1.ctl"))
  file.copy(file.path(system.file("model/nonmem/basic/1", package = "bbr"), "1.cor"),
            file.path(models_dir, "b1", "b1.cor"))
  models_dir
}

# get_param2() on the basic model, with `edit` applied to bbr's parameter table.
param2_with <- function(edit = identity, include_ci = FALSE, block = FALSE,
                        ruva_trans = "RATIO") {
  s <- real_summary()
  models_dir <- write_basic_model(block, ruva_trans)
  testthat::with_mocked_bindings(
    read_model = function(path) list(path = path),
    model_summary = function(mod) s,
    param_estimates = function(x, ...) edit(bbr:::param_estimates.bbi_nonmem_summary(s)),
    .package = "bbr",
    as.data.frame(get_param2("b1", 1, models_folder = models_dir,
                             include_ci = include_ci))
  )
}


test_that("a negative off-diagonal estimate still gives a positive RSE", {
  # REP-10: get_model_max_rse() takes the maximum RSE, so a negative value
  # could never trip the RSE limit.
  out <- param2_with(function(p) {
    i <- p$parameter_names == "OMEGA(2,1)"
    p$estimate[i] <- -0.03
    p$stderr[i] <- 0.06
    p$random_effect_sd[i] <- -0.24
    p$random_effect_sdse[i] <- 0.1
    p$fixed[i] <- FALSE
    p
  }, block = TRUE)

  # Off-diagonal RSE is relative to the correlation: 100 * 0.1 / 0.24.
  rse <- out$RSE[out$parameter_names == "COR_CL_V2"]
  expect_length(rse, 1)
  expect_equal(rse, 100 * 0.1 / 0.24, tolerance = 0.01)
})


test_that("a FIX parameter has no asymptotic interval", {
  # REP-14: bbr reports a FIX element's SE as 1e10, which produced intervals
  # like [-19600000000, 19600000000].
  out <- param2_with(function(p) {
    i <- p$parameter_names %in% c("THETA4", "THETA5")
    p$fixed[i] <- TRUE
    p$stderr[i] <- 1e10
    p
  }, include_ci = TRUE)

  expect_true(all(is.na(out$CI[out$nonmem_name %in% c("THETA4", "THETA5")])))
  expect_false(is.na(out$CI[out$nonmem_name %in% "THETA1"]))
})


test_that("model_report keeps a model that only raised a warning", {
  # REP-06: a warning used to exclude the model, so a one-model report ended
  # in "all models failed".
  s <- real_summary()
  models_dir <- write_basic_model()
  ft <- NULL
  printed <- capture.output(
    ft <- testthat::with_mocked_bindings(
      read_model = function(path) list(path = path),
      model_summary = function(mod) s,
      param_estimates = function(x, ...) {
        warning("a harmless warning from bbr")
        bbr:::param_estimates.bbi_nonmem_summary(s)
      },
      .package = "bbr",
      model_report("b1", models_folder = models_dir)
    )
  )

  expect_s3_class(ft, "flextable")
  expect_match(paste(printed, collapse = "\n"), "harmless warning")
})


test_that("a lowercase log tag gives the same row as LOG", {
  # REP-04: "log" passed validation but matched no scale branch, so the row was
  # blank and the RSE fed to the search's RSE test came from the wrong formula.
  upper <- param2_with(ruva_trans = "LOG")
  lower <- param2_with(ruva_trans = "log")

  row <- function(out) out[out$parameter_names == "RUVa", c("Parameter", "RSE")]
  expect_false(is.na(row(lower)$Parameter))
  expect_equal(row(lower), row(upper))
})
