# The "value ; name ; unit ; transform" annotation is read the same way by
# .theta_tag_for, extract_params and the THETA parser.

test_that("fields are split with empty ones kept and the transform upper-cased", {
  a <- .parse_annotation("2.5 ; CL ; ; ratio ; typical clearance")
  expect_equal(a$value, "2.5")
  expect_equal(a$name, "CL")
  expect_equal(a$unit, "")
  expect_equal(a$trans, "RATIO")
  expect_equal(.parse_annotation("2.5")$name, "")
})

test_that(".theta_tag_for reads the 4th field, so a trailing comment does not hide it", {
  # XAR-01: the LAST field was taken, so "RATIO ; typical clearance" gave NA.
  expect_equal(.theta_tag_for("2.5 ; CL ; L/h ; RATIO ; typical clearance", "CL"), "RATIO")
  expect_equal(.theta_tag_for("2.5 ; CL ; ; RATIO", "CL"), "RATIO")    # blank unit
  expect_equal(.theta_tag_for("2.5 ; CL ; RATIO", "CL"), "RATIO")      # no unit field
  expect_equal(.theta_tag_for("2.5 ; CL ; L/h ; log", "CL"), "LOG")
  expect_true(is.na(.theta_tag_for("2.5 ; CLF ; L/h ; RATIO", "CL")))
})

test_that("a MU-referenced parameter tagged RATIO with a trailing comment stays on the normal scale", {
  # XAR-01 end to end: detect_param_transform fell back to "log", so the
  # covariate was added to a natural-scale typical value.
  mu_model <- function(theta_line) c(
    "$PK", "TV_CL = THETA(1)", "MU_1 = LOG(TV_CL)", "CL = EXP(MU_1 + ETA(1))",
    "$THETA", theta_line
  )
  expect_equal(detect_param_transform(mu_model("2.5 ; CL ; L/h ; RATIO"), "CL"), "normal")
  expect_equal(detect_param_transform(
    mu_model("2.5 ; CL ; L/h ; RATIO ; typical clearance"), "CL"), "normal")
})

test_that("extract_params upper-cases the transform", {
  # REP-04: a lowercase "log" passed validation but matched no scale branch.
  lines <- c("$THETA", "0.7 ; KA ; 1/h ; log", "2.5 ; CL ; L/h ; Ratio")
  expect_equal(extract_params(lines, "THETA")$trans, c("LOG", "RATIO"))
})
