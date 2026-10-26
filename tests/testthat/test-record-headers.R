# Record headers ($THETA, $PK, ...) are found the same way everywhere: leading
# blanks and any case allowed, $PRIOR records ($THETAP, ...) excluded.

write_indented_model <- function() {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c(
    " $PROBLEM indented records",
    " $INPUT ID TIME DV AMT WT",
    " $DATA d.csv IGNORE=@",
    " $PK",
    "TV_CL = THETA(1)",
    "CL = TV_CL * EXP(ETA(1))",
    "TV_V = THETA(2)",
    "V = TV_V * EXP(ETA(2))",
    " $ERROR",
    "Y = F + F*THETA(3)*EPS(1)",
    " $THETA",
    "2.5 ; CL ; L/h ; RATIO",
    "30 ; V ; L ; RATIO",
    "0.1 ; PROP ; ; RATIO",
    " $OMEGA",
    "0.1 ; IIV_CL ; ; RATIO",
    "0.1 ; IIV_V ; ; RATIO",
    " $SIGMA",
    "1 FIX ; EPS ; ; RATIO"
  ), file.path(models_dir, "run1.ctl"))
  models_dir
}

wt_search <- function(status = "con", formula = "power", cov = "WT", ref = 70) {
  data.frame(PARAMETER = "CL", COVARIATE = cov, STATUS = status,
             FORMULA = formula, REFERENCE = ref, LEVELS = NA,
             cov_to_test = paste0("beta_", cov, "_CL"), stringsAsFactors = FALSE)
}

add_cov <- function(models_dir, cs, data_file) {
  invisible(capture.output(
    model_add_cov(list(models_folder = models_dir, search_config = list()), "run1",
                  sub("^beta_", "", cs$cov_to_test), data_file = data_file,
                  covariate_search = cs)
  ))
  readLines(file.path(models_dir, "run1.ctl"))
}


test_that("indented records are found by the parameter-table reader", {
  # XAR-03: extract_params found no rows, so get_param2 failed and every
  # model got rse_max NA.
  lines <- readLines(file.path(write_indented_model(), "run1.ctl"))
  expect_equal(trimws(extract_params(lines, "THETA")$param), c("CL", "V", "PROP"))
  expect_equal(trimws(extract_params(lines, "OMEGA")$param), c("IIV_CL", "IIV_V"))
})


test_that("an indented model can have a covariate added and removed", {
  # MOD-10: the remover stopped with "No $THETA section found".
  models_dir <- write_indented_model()
  original <- readLines(file.path(models_dir, "run1.ctl"))
  add_cov(models_dir, wt_search(),
          data.frame(ID = rep(1:4, each = 2), WT = rep(c(60, 70, 80, 90), each = 2)))

  search_state <- list(
    models_folder = models_dir, model_counter = 1L,
    tags = list(cov_cl_wt = "WT_CL"), covariate_search = wt_search(),
    search_database = data.frame(model_name = "run1", tags = I(list("WT_CL")),
                                 stringsAsFactors = FALSE)
  )
  invisible(capture.output(
    res <- remove_covariate_from_model(search_state, "run1", "cov_cl_wt",
                                       save_as_new_model = FALSE, step_number = 2L)
  ))

  expect_equal(res$status, "success")
  expect_identical(trimws(readLines(file.path(models_dir, "run1.ctl")), "right"), original)
})


test_that("a categorical covariate on a $PRED model gets its IF block", {
  # MOD-06: the block was appended to a $PK record that does not exist, so the
  # model referenced an undefined beta_SEX_CL.
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c(
    "$PROBLEM pred model", "$INPUT ID DV SEX", "$DATA d.csv",
    "$PRED",
    "TV_CL = THETA(1)",
    "CL = TV_CL * EXP(ETA(1))",
    "Y = CL + EPS(1)",
    "$THETA", "2.5 ; CL ; L/h ; RATIO",
    "$OMEGA", "0.1 ; IIV_CL ; ; RATIO",
    "$SIGMA", "1 ; EPS ; ; RATIO"
  ), file.path(models_dir, "run1.ctl"))
  cs <- wt_search(status = "cat", formula = "linear", cov = "SEX", ref = 1)
  cs$LEVELS <- "1;2"

  ctl <- add_cov(models_dir, cs, data.frame(ID = 1:4, SEX = c(1, 2, 1, 2)))

  pred <- which(ctl == "$PRED")
  expect_match(paste(ctl, collapse = "\n"), "IF\\(SEX\\.EQ\\.1\\)THEN")
  expect_true(grep("beta_SEX_CL = 1 \\+ THETA\\(2\\)", ctl)[1] > pred)
})


test_that("a model with no $PK or $PRED record stops instead of losing the block", {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c("$PROBLEM none", "$THETA", "2.5 ; CL ; ; RATIO",
               "$OMEGA", "0.1 ; IIV_CL ; ; RATIO"), file.path(models_dir, "run1.ctl"))
  cs <- wt_search(status = "cat", formula = "linear", cov = "SEX", ref = 1)
  cs$LEVELS <- "1;2"

  expect_error(add_cov(models_dir, cs, data.frame(ID = 1:4, SEX = c(1, 2, 1, 2))),
               "no \\$PK or \\$PRED")
})


test_that("record names are matched in any case", {
  lines <- c("$theta", "2.5 ; CL ; ; RATIO", "$omega", "0.1 ; IIV_CL ; ; RATIO")
  expect_equal(.record_starts(lines, "THETA"), 1L)
  expect_equal(trimws(extract_params(lines, "THETA")$param), "CL")
  expect_equal(.record_starts(c("$THETAP", "$THETA (0,1)"), "THETA"), 2L)
})
