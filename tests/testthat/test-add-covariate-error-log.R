# add_covariate_to_model() writes an error log when model_add_cov() fails, and
# hands back the real cause rather than an error from writing that log.

test_that("a failed covariate addition writes its error log and reports the real cause", {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  # No $OMEGA record: model_add_cov() refuses to insert the THETA line.
  writeLines(c(
    "$PROBLEM no omega",
    "$INPUT ID WT",
    "$DATA d.csv",
    "$PK",
    "TV_CL = THETA(1)",
    "CL = TV_CL * EXP(ETA(1))",
    "$ERROR",
    "Y = F",
    "$THETA",
    "2.5 ; CL ; ; RATIO",
    "$SIGMA",
    "1 FIX"
  ), file.path(models_dir, "run1.ctl"))
  suppressMessages(bbr::new_model(file.path(models_dir, "run1")))

  search_state <- list(
    models_folder = models_dir,
    model_counter = 1L,
    idcol = "ID",
    data_file = data.frame(ID = 1:4, WT = c(60, 70, 80, 90)),
    tags = list(beta_WT_CL = "WT_CL"),
    covariate_search = data.frame(
      PARAMETER = "CL", COVARIATE = "WT", STATUS = "con", FORMULA = "power",
      REFERENCE = 70, cov_to_test = "beta_WT_CL", stringsAsFactors = FALSE
    ),
    search_database = data.frame(model_name = "run1",
                                 tags = I(list(character(0))),
                                 stringsAsFactors = FALSE),
    search_config = list()
  )

  had_global <- exists("log_filename", envir = globalenv(), inherits = FALSE)
  invisible(capture.output(
    res <- add_covariate_to_model(search_state, "run1", "beta_WT_CL", step_number = 1L)
  ))

  expect_equal(res$status, "error")
  expect_match(res$error_message, "OMEGA")
  expect_true(file.exists(file.path(models_dir, "run2_add_WT_CL_error_log.txt")))
  # The log path must not leak into the caller's global environment.
  expect_identical(exists("log_filename", envir = globalenv(), inherits = FALSE), had_global)
})
