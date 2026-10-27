test_that("decode_cov_level returns the decoded category for a matched level", {
  lookup <- list(
    SEXN = list(values = c(0, 1, 2), decode = c("Ref", "Male", "Female"))
  )
  expect_equal(decode_cov_level("SEXN", "2", lookup), "Female")
  expect_equal(decode_cov_level("SEXN", 1, lookup), "Male")
})

test_that("decode_cov_level returns NA when no lookup is supplied", {
  expect_true(is.na(decode_cov_level("SEXN", "2", NULL)))
})

test_that("decode_cov_level returns NA when the covariate is absent from lookup", {
  lookup <- list(RACE = list(values = c(1, 2), decode = c("White", "Black")))
  expect_true(is.na(decode_cov_level("SEXN", "2", lookup)))
})

test_that("decode_cov_level returns NA when the level value is not in `values`", {
  lookup <- list(SEXN = list(values = c(0, 1), decode = c("Female", "Male")))
  expect_true(is.na(decode_cov_level("SEXN", "2", lookup)))
})

test_that("decode_cov_level returns NA when the entry lacks values/decode", {
  lookup <- list(SEXN = list(short = "Sex"))
  expect_true(is.na(decode_cov_level("SEXN", "2", lookup)))
})

test_that("an older decoded THETA-name suffix maps back to its level", {
  # XAR-08: beta_SEX_CL_MALE was read as a multi-theta effect "SEX_MALE".
  lk <- list(SEX = list(values = list(1, 2), decode = list("Female", "Male")),
             RACE = list(values = list(1, 2), decode = list("White", "Black or African")))
  expect_equal(.level_from_decoded_name("SEX", "MALE", lk), "2")
  expect_equal(.level_from_decoded_name("RACE", c("BLACK", "OR", "AFRICAN"), lk), "2")
  expect_true(is.na(.level_from_decoded_name("SEX", "EMAX", lk)))
  expect_true(is.na(.level_from_decoded_name("WT", "MALE", lk)))
})

test_that("model_add_cov names categorical THETAs by the numeric level, even with a lookup", {
  # XAR-08: with a lookup file the THETA was named beta_SEX_CL_MALE, which the
  # reports then read as a multi-theta effect.
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c("$PROBLEM cat", "$INPUT ID DV SEX", "$DATA d.csv",
               "$PK", "TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1))",
               "$THETA", "2.5 ; CL ; L/h ; RATIO",
               "$OMEGA", "0.1 ; IIV_CL ; ; RATIO", "$SIGMA", "1 ; EPS ; ; RATIO"),
             file.path(models_dir, "run1.ctl"))
  lk <- tempfile(fileext = ".yaml")
  writeLines(c("SEX:", "  values: [1, 2]", "  decode: [Female, Male]"), lk)
  cs <- data.frame(PARAMETER = "CL", COVARIATE = "SEX", STATUS = "cat", FORMULA = "linear",
                   LEVELS = "1;2", REFERENCE = 1, TIME_DEPENDENT = "No",
                   cov_to_test = "beta_SEX_CL", stringsAsFactors = FALSE)
  invisible(capture.output(
    model_add_cov(list(models_folder = models_dir, search_config = list()), "run1", "SEX_CL",
                  data_file = data.frame(ID = 1:4, SEX = c(1, 2, 1, 2)),
                  covariate_search = cs, lookup_file = lk)))
  ctl <- readLines(file.path(models_dir, "run1.ctl"))
  expect_true(any(grepl("beta_SEX_CL_2", ctl, fixed = TRUE)))
  expect_false(any(grepl("MALE", ctl, ignore.case = TRUE)))
})
