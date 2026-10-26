# get_param2() driven with bbr's own captured model summary (bbr/test-refs,
# the basic/1 example: five THETAs, a BLOCK(2) OMEGA, one FIX SIGMA). Only
# bbr's three accessors are replaced, so every table rule is the package's own.

real_summary <- function() {
  ref <- file.path(system.file("test-refs", package = "bbr"), "1_summary_obj.R")
  skip_if_not(file.exists(ref), "bbr test-refs not installed")
  eval(parse(ref))
}

# A control stream annotated to match the summary's parameters, and the
# example's .cor so the condition number can be read.
write_basic_model <- function() {
  models_dir <- tempfile("models")
  dir.create(file.path(models_dir, "b1"), recursive = TRUE)
  writeLines(c(
    "$PROBLEM basic", "$THETA",
    "(0, 2)  ; KA ; 1/h ; RATIO",
    "(0, 3)  ; CL ; L/h ; RATIO",
    "(0, 10) ; V2 ; L ; RATIO",
    "(0.02)  ; RUVp ; - ; RATIO",
    "(1)     ; RUVa ; - ; RATIO",
    "$OMEGA BLOCK(2)",
    "0.05 ; IIV_CL ; - ; RATIO",
    "0.01 ; COR_CL_V2 ; - ; RATIO",
    "0.2  ; IIV_V2 ; - ; RATIO",
    "$SIGMA", "1 FIX ; SIG ; - ; RATIO",
    "$EST METHOD=1"
  ), file.path(models_dir, "b1.ctl"))
  file.copy(file.path(system.file("model/nonmem/basic/1", package = "bbr"), "1.cor"),
            file.path(models_dir, "b1", "b1.cor"))
  models_dir
}

# get_param2() on the basic model, with `edit` applied to bbr's parameter table.
param2_with <- function(edit = identity) {
  s <- real_summary()
  models_dir <- write_basic_model()
  testthat::with_mocked_bindings(
    read_model = function(path) list(path = path),
    model_summary = function(mod) s,
    param_estimates = function(x, ...) edit(bbr:::param_estimates.bbi_nonmem_summary(s)),
    .package = "bbr",
    as.data.frame(get_param2("b1", 1, models_folder = models_dir))
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
  })

  # Off-diagonal RSE is relative to the correlation: 100 * 0.1 / 0.24.
  rse <- out$RSE[out$parameter_names == "COR_CL_V2"]
  expect_length(rse, 1)
  expect_equal(rse, 100 * 0.1 / 0.24, tolerance = 0.01)
})
