# .parse_theta_records(): one row per THETA value, and the readers built on it.

test_that("values are counted per group and per bare number, FIX ignored", {
  expect_equal(.count_theta_values("2.5"), 1)
  expect_equal(.count_theta_values("(0, 2.5, 100) FIX"), 1)
  expect_equal(.count_theta_values("(0,2.5) (0,30)"), 2)
  expect_equal(.count_theta_values("2.5 30"), 2)
  expect_equal(.count_theta_values("(0,2.5) 30"), 2)
  expect_equal(.count_theta_values("0.75 FIX"), 1)
  expect_equal(.count_theta_values("0.75FIX"), 1)
  expect_equal(.count_theta_values("(0,1)FIXED 2"), 2)
  expect_equal(.count_theta_values("1.5E-02"), 1)
  expect_equal(.count_theta_values(""), 0)
})

test_that("every value is numbered across records, including the record line", {
  lines <- c(
    "$PK", "CL = THETA(1)",
    "$THETA 2.5 ; CL ; L/h ; RATIO",
    "; a comment",
    "(0,30) (0,1) ; V KA ; ; RATIO",
    "$THETAP 2.5 FIX",
    "$THETA",
    "",
    "0.1 ; PROP ; ; log",
    "$OMEGA 0.1"
  )
  p <- .parse_theta_records(lines)

  expect_equal(p$index, 1:4)
  expect_equal(p$line, c(3L, 5L, 5L, 9L))
  expect_equal(p$on_record_line, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(p$n_on_line, c(1L, 2L, 2L, 1L))
  expect_equal(p$name, c("CL", "V KA", "", "PROP"))
  expect_equal(p$trans, c("RATIO", "RATIO", "", "LOG"))
})


test_that("model_add_cov does not reuse a THETA written first on its line", {
  # MOD-01: only the last THETA(n) per line was read, so with the additive SD
  # (THETA(5)) written before THETA(4) the new covariate became THETA(5).
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c(
    "$PROBLEM additive first", "$INPUT ID DV WT", "$DATA d.csv",
    "$PK", "TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1))",
    "TV_V = THETA(2)", "V = TV_V * EXP(ETA(2))", "KA = THETA(3)",
    "$ERROR", "IPRED = F",
    "W = SQRT(THETA(5)**2 + THETA(4)**2*IPRED**2)",
    "Y = IPRED + W*EPS(1)",
    "$THETA",
    "2.5 ; CL ; L/h ; RATIO", "30 ; V ; L ; RATIO", "1 ; KA ; 1/h ; RATIO",
    "0.1 ; PROP ; ; RATIO", "0.5 ; ADD ; ; RATIO",
    "$OMEGA", "0.1 ; IIV_CL ; ; RATIO", "0.1 ; IIV_V ; ; RATIO",
    "$SIGMA", "1 FIX ; EPS ; ; RATIO"
  ), file.path(models_dir, "run1.ctl"))
  cs <- data.frame(PARAMETER = "CL", COVARIATE = "WT", STATUS = "con",
                   FORMULA = "power", REFERENCE = 70, LEVELS = NA,
                   cov_to_test = "beta_WT_CL", stringsAsFactors = FALSE)

  invisible(capture.output(
    model_add_cov(list(models_folder = models_dir, search_config = list()), "run1",
                  "WT_CL", data_file = data.frame(ID = 1:4, WT = c(60, 70, 80, 90)),
                  covariate_search = cs)
  ))
  ctl <- readLines(file.path(models_dir, "run1.ctl"))

  expect_true("TV_CL = THETA(1) * (WT/70)**THETA(6)" %in% trimws(ctl))
  expect_equal(nrow(.parse_theta_records(ctl)), 6)
})


test_that("removal numbers a covariate correctly after lines holding several values", {
  # MOD-12: one THETA was counted per line, so beta_WT_CL (THETA 4) read as 2.
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c(
    "$PROBLEM multi-value line", "$INPUT ID DV WT", "$DATA d.csv",
    "$PK", "TV_CL = THETA(1) * (WT/70)**THETA(4)", "CL = TV_CL * EXP(ETA(1))",
    "V = THETA(2)",
    "$ERROR", "Y = F + F*THETA(3)*EPS(1)",
    "$THETA",
    "(0,2.5) (0,30) (0,0.1)",
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
  ctl <- readLines(file.path(models_dir, "run1.ctl"))

  expect_equal(res$status, "success")
  expect_true("TV_CL = THETA(1)" %in% trimws(ctl))
  expect_true("Y = F + F*THETA(3)*EPS(1)" %in% ctl)
  expect_true("(0,2.5) (0,30) (0,0.1)" %in% ctl)
})


test_that("validation rejects several values on one $THETA line, but allows one on the record line", {
  # XAR-04: bounded groups were deleted before counting, so these passed.
  check <- function(theta_lines) {
    f <- tempfile(fileext = ".ctl")
    writeLines(c("$PROBLEM v", theta_lines, "$OMEGA", "0.1 ; IIV ; ; RATIO"), f)
    validate_parameter_blocks(f)$valid
  }
  expect_false(check(c("$THETA", "(0,2.5) (0,30) ; CL ; L/h ; RATIO")))
  expect_false(check(c("$THETA", "(0,2.5) 30 ; CL ; L/h ; RATIO")))
  expect_false(check(c("$THETA (0,2.5) (0,30)", "1 ; KA ; ; RATIO")))
  expect_false(check(c("$THETA 2.5 30", "1 ; KA ; ; RATIO")))
  expect_true(check(c("$THETA 0.5 ; CL ; L/h ; RATIO", "1 ; KA ; ; RATIO")))
  expect_true(check(c("$THETA", "(0, 2.5, 100) FIX ; CL ; L/h ; RATIO")))
})


test_that("apply_covariate_model keeps every THETA when a line holds several", {
  # XAR-04 consequence: V on the same line as CL was dropped from the result.
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c(
    "$PROBLEM", "$PK", "TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1))",
    "TV_V = THETA(2)", "V = TV_V * EXP(ETA(2))",
    "$THETA (0,2.5) (0,30)", "$OMEGA 0.1 0.1"
  ), file.path(models_dir, "run1.ctl"))

  out <- apply_covariate_model("run1", data.frame(COVARIATE = "WT", REFERENCE = 70),
                               data.frame(THETA1 = 2.5, THETA2 = 30), c(WT = 70),
                               models_folder = models_dir)
  expect_named(out, c("ID", "THETA1", "THETA2"))
  expect_equal(out$THETA2, 30)
})
