# validate_covariate_search_table() normalises the table once: STATUS lower
# case, FORMULA a real covariate form, REFERENCE numeric, cov_to_test prefixed.

base_table <- function(...) {
  t <- data.frame(PARAMETER = c("CL", "V"), COVARIATE = c("WT", "SEX"),
                  STATUS = c("con", "cat"), FORMULA = c("power", "linear"),
                  LEVELS = c(NA, "1;2"), REFERENCE = c("75", "1"),
                  TIME_DEPENDENT = "No", stringsAsFactors = FALSE)
  mods <- list(...)
  for (nm in names(mods)) t[[nm]] <- mods[[nm]]
  t
}
dat <- data.frame(ID = 1:4, WT = c(60, 70, 80, 90), SEX = c(1, 2, 1, 2))
validate <- function(t) {
  out <- NULL
  invisible(capture.output(out <- validate_covariate_search_table(t, dat)))
  out
}


test_that("a FORMULA that is not a covariate form stops", {
  # STA-01: these were written into $PK as factors with no covariate in them.
  expect_error(validate(base_table(FORMULA = c("power", "exponential"))), "neither a built-in")
  expect_error(validate(base_table(FORMULA = c("powr", "linear"))), "neither a built-in")
  expect_error(validate(base_table(FORMULA = c("BETA*2", "linear"))), "neither a built-in")
})

test_that("built-in names in any case and expressions in cov are accepted", {
  out <- validate(base_table(FORMULA = c("Power", "LINEAR")))
  expect_equal(nrow(out), 2)
  out <- validate(base_table(FORMULA = c("EMAX*cov/(EC50+cov)", "linear")))
  expect_equal(nrow(out), 2)
})

test_that("STATUS is lower-cased, so the categorical checks apply to 'CAT'", {
  # MOD-09: "CAT" skipped the LEVELS/REFERENCE checks here.
  out <- validate(base_table(STATUS = c("con", "CAT")))
  expect_equal(out$STATUS, c("con", "cat"))
  expect_error(validate(base_table(STATUS = c("con", "CAT"), LEVELS = c(NA, NA))), "no LEVELS")
  expect_error(validate(base_table(STATUS = c("con", "category"))), "STATUS must be")
})

test_that("REFERENCE is stored as a number", {
  # MOD-08: a character REFERENCE was written as WT/"75" in user expressions.
  out <- validate(base_table())
  expect_true(is.numeric(out$REFERENCE))
  expect_equal(out$REFERENCE, c(75, 1))
  expect_error(validate(base_table(REFERENCE = c("seventy", "1"))), "REFERENCE must be numeric")
  expect_true(is.na(validate(base_table(REFERENCE = c(NA, "1")))$REFERENCE[1]))
})

test_that("a user expression using ref is written with a numeric reference", {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c(
    "$PROBLEM ref", "$INPUT ID DV WT", "$DATA d.csv",
    "$PK", "TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1))",
    "$THETA", "2.5 ; CL ; L/h ; RATIO",
    "$OMEGA", "0.1 ; IIV_CL ; ; RATIO", "$SIGMA", "1 ; EPS ; ; RATIO"
  ), file.path(models_dir, "run1.ctl"))
  cs <- validate(base_table(FORMULA = c("1 + SLOPE*log(cov/ref)", "linear")))[1, ]

  invisible(capture.output(
    model_add_cov(list(models_folder = models_dir, search_config = list()), "run1",
                  "WT_CL", data_file = dat, covariate_search = cs)
  ))
  ctl <- paste(readLines(file.path(models_dir, "run1.ctl")), collapse = "\n")
  expect_match(ctl, "LOG(WT/75)", fixed = TRUE)
  expect_false(grepl('"', ctl, fixed = TRUE))
})

test_that("cov_to_test gets the beta_ prefix the tags use", {
  # XAR-07: an unprefixed value matched nothing after the tags were prefixed.
  out <- validate(base_table(cov_to_test = c("WT_CL", "beta_SEX_V")))
  expect_equal(out$cov_to_test, c("beta_WT_CL", "beta_SEX_V"))
})
