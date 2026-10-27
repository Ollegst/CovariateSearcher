# Single-line IF statements in $PK: the condition may contain parentheses, and
# a condition that cannot be evaluated is reported instead of silently FALSE.

write_if_model <- function(if_line) {
  models_dir <- tempfile("models")
  dir.create(file.path(models_dir, "run1"), recursive = TRUE)
  writeLines(c("$THETA", "1.0 ; CL ; L/h ; RATIO",
               "$OMEGA", "0.09 ; IIV_CL",
               "$PK", "TV_CL = THETA(1)", if_line, "CL = TV_CL * EXP(ETA(1))"),
             file.path(models_dir, "run1.ctl"))
  writeLines(c("TABLE NO.     1: First Order Conditional Estimation: Goal Function=MIN",
               " ITERATION THETA1 OMEGA(1,1) OBJ",
               " -1000000000 1.0 0.09 100.0"),
             file.path(models_dir, "run1", "run1.ext"))
  models_dir
}
cs <- data.frame(COVARIATE = c("WT", "SEX"), REFERENCE = c(70, 1), stringsAsFactors = FALSE)

test_that("a single-line IF with nested parentheses is applied", {
  # SIM-03: the lazy match stopped at the first ")", the condition failed to
  # evaluate and was silently FALSE, so CL stayed 1.0.
  md <- write_if_model("IF (SEX.EQ.2.AND.(WT.GT.50)) TV_CL = TV_CL + 0.5")
  out <- apply_covariate_model("run1", cs, data.frame(THETA1 = 1), c(WT = 70, SEX = 2),
                               models_folder = md)
  expect_equal(out$THETA1, 1.5, tolerance = 1e-8)
  out <- apply_covariate_model("run1", cs, data.frame(THETA1 = 1), c(WT = 40, SEX = 2),
                               models_folder = md)
  expect_equal(out$THETA1, 1.0, tolerance = 1e-8)
})

test_that("a condition that cannot be evaluated is reported", {
  md <- write_if_model("IF (FOO.GT.1) TV_CL = TV_CL + 0.5")
  expect_warning(
    out <- apply_covariate_model("run1", cs, data.frame(THETA1 = 1), c(WT = 70, SEX = 2),
                                 models_folder = md),
    "could not be evaluated and were treated as FALSE: FOO.GT.1", fixed = TRUE)
  expect_equal(out$THETA1, 1.0, tolerance = 1e-8)
})

test_that(".split_single_line_if keeps nested parentheses in the condition", {
  expect_equal(.split_single_line_if("IF (A.EQ.1.AND.(B.GT.2)) X = Y + (1)")[2:3],
               c("A.EQ.1.AND.(B.GT.2)", "X = Y + (1)"))
  expect_length(.split_single_line_if("IF (A.EQ.1 X = 1"), 0)
  expect_length(.split_single_line_if("IF ("), 0)
})
