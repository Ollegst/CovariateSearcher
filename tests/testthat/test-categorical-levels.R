# Categorical LEVELS must match the observed levels, and there must be a level
# other than REFERENCE to estimate (MOD-07).

cat_table <- function(levels, ref = 1) {
  data.frame(PARAMETER = "CL", COVARIATE = "SEX", STATUS = "cat", FORMULA = "linear",
             LEVELS = levels, REFERENCE = ref, TIME_DEPENDENT = "No",
             stringsAsFactors = FALSE)
}
validate_with <- function(t, sex) {
  dat <- data.frame(ID = seq_along(sex), SEX = sex)
  invisible(capture.output(out <- validate_covariate_search_table(t, dat)))
  out
}

test_that("a declared level never observed stops validation", {
  # df was counted from 3 declared levels while only 2 got THETAs.
  expect_error(validate_with(cat_table("1;2;3"), c(1, 2, 1)), "never observed.*3")
  expect_equal(nrow(validate_with(cat_table("1;2"), c(1, 2, 1))), 1)
})

test_that("a categorical covariate with one observed level stops", {
  # Only the reference observed: an unreferenced beta THETA was written at df 1.
  expect_error(validate_with(cat_table("0", ref = 0), c(0, 0, 0)), "only one observed level")
})

test_that("model_add_cov stops when no non-reference level is observed", {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c(
    "$PROBLEM cat", "$INPUT ID DV SEX", "$DATA d.csv",
    "$PK", "TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1))",
    "$THETA", "2.5 ; CL ; L/h ; RATIO",
    "$OMEGA", "0.1 ; IIV_CL ; ; RATIO", "$SIGMA", "1 ; EPS ; ; RATIO"
  ), file.path(models_dir, "run1.ctl"))
  cs <- cat_table("0;1", ref = 0)
  cs$cov_to_test <- "beta_SEX_CL"
  expect_error(invisible(capture.output(
    model_add_cov(list(models_folder = models_dir, search_config = list()), "run1",
                  "SEX_CL", data_file = data.frame(ID = 1:3, SEX = 0), covariate_search = cs)
  )), "no observed level other than REFERENCE")
})
