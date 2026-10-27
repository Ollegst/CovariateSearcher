# A missing record (NA, or NONMEM's ".") is not a covariate value: it does not
# make a covariate time-varying and does not become a categorical level.

write_run1 <- function() {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c(
    "$PROBLEM tv", "$INPUT ID DV WT SEX", "$DATA d.csv",
    "$PK", "TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1))",
    "$THETA", "2.5 ; CL ; L/h ; RATIO",
    "$OMEGA", "0.1 ; IIV_CL ; ; RATIO", "$SIGMA", "1 ; EPS ; ; RATIO"
  ), file.path(models_dir, "run1.ctl"))
  models_dir
}
cov_row <- function(cov, status, formula, levels, ref) {
  data.frame(PARAMETER = "CL", COVARIATE = cov, STATUS = status, FORMULA = formula,
             LEVELS = levels, REFERENCE = ref, TIME_DEPENDENT = "No",
             cov_to_test = paste0("beta_", cov, "_CL"), stringsAsFactors = FALSE)
}
add_cov <- function(models_dir, tag, dat, cs) {
  invisible(capture.output(
    model_add_cov(list(models_folder = models_dir, search_config = list()), "run1",
                  tag, data_file = dat, covariate_search = cs)
  ))
  readLines(file.path(models_dir, "run1.ctl"))
}


test_that("NA and '.' records do not make a covariate time-varying", {
  d <- data.frame(ID = c(1, 1, 2, 2), WT = c(70, NA, 80, 80))
  expect_false(.covariate_is_time_varying(d, "WT", "ID"))
  d$WT <- c("70", ".", "80", "80")
  expect_false(.covariate_is_time_varying(d, "WT", "ID"))
  d$WT <- c(70, 71, 80, 80)
  expect_true(.covariate_is_time_varying(d, "WT", "ID"))
  expect_error(.covariate_is_time_varying(d, "MISSING", "ID"), "not found in the dataset: MISSING")
  expect_error(.covariate_is_time_varying(d, "WT", "SUBJ"), "not found in the dataset: SUBJ")
})

test_that("a constant covariate with a missing record goes on the TV_ line", {
  # STA-05: the NA counted as a second value, so the effect went on the
  # individual line as if WT were time-varying.
  models_dir <- write_run1()
  dat <- data.frame(ID = c(1, 1, 2, 2), WT = c(70, NA, 80, 80), SEX = 1)
  ctl <- add_cov(models_dir, "WT_CL", dat, cov_row("WT", "con", "power", NA, 75))
  expect_true(any(grepl("^TV_CL.*WT", ctl)))
  expect_false(any(grepl("^CL =.*WT", ctl)))
})

test_that("a missing record does not become a categorical level", {
  # MOD-05: NA got its own THETA and an IF(SEX.EQ.NA) branch.
  models_dir <- write_run1()
  dat <- data.frame(ID = 1:5, WT = 70, SEX = c(1, 2, 2, NA, NA))
  ctl <- add_cov(models_dir, "SEX_CL", dat, cov_row("SEX", "cat", "linear", "1;2", 1))
  expect_false(any(grepl("NA", ctl[grepl("SEX", ctl)])))
  expect_equal(sum(grepl("SEX", ctl[seq(which(ctl == "$THETA"), length(ctl))])), 1)
})

test_that("the dataset is read with '.' as missing", {
  # UTL-03: "." turned a numeric column into text.
  f <- tempfile(fileext = ".csv")
  writeLines(c("ID,WT", "1,70", "1,.", "2,80"), f)
  d <- .load_if_path(f, "data_file_path", na = c("", "NA", "."))
  expect_true(is.numeric(d$WT))
  expect_equal(sum(is.na(d$WT)), 1)
  expect_true(is.character(.load_if_path(f, "table")$WT))
})
