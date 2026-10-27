# Removing a covariate must not renumber a THETA another equation still uses.

test_that("removal stops when a removed THETA is still referenced elsewhere", {
  # MOD-04: Q reused the covariate THETA first on its line, so the strip left it
  # and renumbering moved Q's exponent onto the error THETA.
  models_dir <- tempfile("models")
  dir.create(models_dir)
  original <- c(
    "$PROBLEM shared theta", "$INPUT ID DV WT", "$DATA d.csv",
    "$PK", "TV_CL = THETA(1) * (WT/70)**THETA(4)", "CL = TV_CL * EXP(ETA(1))",
    "Q = (WT/70)**THETA(4) * THETA(2)",
    "$ERROR", "Y = F + F*THETA(3)*EPS(1)",
    "$THETA", "(0,2.5) ; CL", "(0,30) ; Q", "(0,0.1) ; PROP",
    "0.75 ; beta_WT_CL ; ; RATIO",
    "$OMEGA", "0.1 ; IIV_CL ; ; RATIO",
    "$SIGMA", "1 FIX ; EPS ; ; RATIO"
  )
  writeLines(original, file.path(models_dir, "run1.ctl"))
  search_state <- list(
    models_folder = models_dir, model_counter = 1L,
    tags = list(cov_cl_wt = "WT_CL"),
    covariate_search = data.frame(COVARIATE = "WT", PARAMETER = "CL", STATUS = "con",
                                  FORMULA = "power", cov_to_test = "beta_WT_CL",
                                  stringsAsFactors = FALSE),
    search_database = data.frame(model_name = "run1", tags = I(list("WT_CL")),
                                 stringsAsFactors = FALSE)
  )

  expect_error(invisible(capture.output(
    remove_covariate_from_model(search_state, "run1", "cov_cl_wt",
                                save_as_new_model = FALSE, step_number = 2L)
  )), "THETA\\(4\\).*still referenced")
  expect_equal(readLines(file.path(models_dir, "run1.ctl")), original)
})

test_that("a reference only inside a comment does not block removal", {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c(
    "$PROBLEM comment", "$INPUT ID DV WT", "$DATA d.csv",
    "$PK", "TV_CL = THETA(1) * (WT/70)**THETA(3) ; was THETA(3) on V too",
    "CL = TV_CL * EXP(ETA(1))", "V = THETA(2)",
    "$THETA", "(0,2.5) ; CL", "(0,30) ; V",
    "0.75 ; beta_WT_CL ; ; RATIO",
    "$OMEGA", "0.1 ; IIV_CL ; ; RATIO",
    "$SIGMA", "1 FIX ; EPS ; ; RATIO"
  ), file.path(models_dir, "run1.ctl"))
  search_state <- list(
    models_folder = models_dir, model_counter = 1L,
    tags = list(cov_cl_wt = "WT_CL"),
    covariate_search = data.frame(COVARIATE = "WT", PARAMETER = "CL", STATUS = "con",
                                  FORMULA = "power", cov_to_test = "beta_WT_CL",
                                  stringsAsFactors = FALSE),
    search_database = data.frame(model_name = "run1", tags = I(list("WT_CL")),
                                 stringsAsFactors = FALSE)
  )
  invisible(capture.output(
    res <- remove_covariate_from_model(search_state, "run1", "cov_cl_wt",
                                       save_as_new_model = FALSE, step_number = 2L)
  ))
  expect_equal(res$status, "success")
})
