# create_covariate_table: a categorical covariate always has LEVELS.

test_that("a categorical covariate with no LEVELS stops instead of being skipped", {
  # SIM-06: the covariate silently got no forest scenario.
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c("$PROBLEM t", "$PK", "TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1))",
               "$THETA", "1 ; CL ; L/h ; RATIO", "0.1 ; beta_SEX_CL_2 ; ; RATIO",
               "$OMEGA", "0.09 ; IIV_CL ; ; RATIO"),
             file.path(models_dir, "run1.ctl"))
  cs <- data.frame(COVARIATE = "SEX", STATUS = "cat", REFERENCE = 1, LEVELS = NA_character_,
                   PARAMETER = "CL", stringsAsFactors = FALSE)
  d <- data.frame(ID = 1:4, SEX = c(1, 2, 1, 2))
  expect_error(
    suppressWarnings(create_covariate_table("run1", cs, d, models_folder = models_dir)),
    "STATUS == 'cat' but no LEVELS specified", fixed = TRUE)

  cs$LEVELS <- "1;2"
  tbl <- suppressWarnings(create_covariate_table("run1", cs, d, models_folder = models_dir))
  expect_true(nrow(tbl) >= 2)
})

test_that("fractional percentiles are labelled as written, not rounded", {
  # SIM-05: 2.5 printed as "2nd" and 97.5 as "98th".
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c("$PROBLEM t", "$PK", "TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1))",
               "$THETA", "1 ; CL ; L/h ; RATIO", "0.75 ; beta_WT_CL ; ; RATIO",
               "$OMEGA", "0.09 ; IIV_CL ; ; RATIO"),
             file.path(models_dir, "run1.ctl"))
  cs <- data.frame(COVARIATE = "WT", STATUS = "con", REFERENCE = 70, LEVELS = NA_character_,
                   PARAMETER = "CL", stringsAsFactors = FALSE)
  d <- data.frame(ID = 1:100, WT = seq(40, 120, length.out = 100))
  tbl <- suppressWarnings(create_covariate_table("run1", cs, d, percentiles = c(0.025, 0.975),
                                                 models_folder = models_dir))
  lab <- paste(unlist(tbl[sapply(tbl, is.character)]), collapse = " ")
  expect_match(lab, "2.5th percentile")
  expect_match(lab, "97.5th percentile")
})
