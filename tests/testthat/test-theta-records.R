# NONMEM numbers THETAs consecutively across every $THETA record, and a value
# may sit on the record line itself ("$THETA 2.5 ; CL"). These tests cover the
# readers that number THETAs from the control stream: covariate removal, the
# $PK evaluator behind the forest plots, and the forest plots' axis labels.

# A model whose covariate THETA(4) lives in the SECOND $THETA record, where
# model_add_cov() puts it (just before $OMEGA). `header_value = TRUE` writes the
# first THETA on the first record line, so a reader that skips record lines
# numbers everything after it one too low. `cov_on_record = TRUE` writes the
# covariate THETA on the second record line instead, numbering unchanged.
write_two_record_model <- function(header_value = FALSE, cov_on_record = FALSE) {
  models_dir <- tempfile("models")
  dir.create(models_dir)

  first_record <- if (header_value) {
    c("$THETA 2.5 ; CL ; L/h ; RATIO",
      "30 ; V ; L ; RATIO")
  } else {
    c("$THETA",
      "2.5 ; CL ; L/h ; RATIO",
      "30 ; V ; L ; RATIO")
  }
  second_record <- if (cov_on_record) {
    c("0.1 ; PROP ; ; RATIO",
      "$THETA 0.75 ; beta_WT_CL ; ; RATIO")
  } else {
    c("$THETA",
      "0.1 ; PROP ; ; RATIO",
      "0.75 ; beta_WT_CL ; ; RATIO")
  }

  writeLines(c(
    "$PROBLEM two THETA records",
    "$INPUT ID TIME DV AMT WT",
    "$DATA data.csv IGNORE=@",
    "$PK",
    "TV_CL = THETA(1) * (WT/70)**THETA(4)",
    "CL = TV_CL * EXP(ETA(1))",
    "TV_V = THETA(2)",
    "V = TV_V * EXP(ETA(2))",
    "$ERROR",
    "IPRED = F",
    "Y = IPRED + IPRED*THETA(3)*EPS(1)",
    first_record,
    second_record,
    "$OMEGA",
    "0.1 ; IIV_CL ; ; RATIO",
    "0.1 ; IIV_V ; ; RATIO",
    "$SIGMA",
    "1 FIX ; EPS ; ; RATIO"
  ), file.path(models_dir, "run1.ctl"))

  models_dir
}

removal_state <- function(models_dir) {
  list(
    models_folder = models_dir,
    model_counter = 1L,
    tags = list(cov_cl_wt = "WT_CL"),
    covariate_search = data.frame(
      COVARIATE = "WT", PARAMETER = "CL", STATUS = "con", FORMULA = "power",
      cov_to_test = "beta_WT_CL", stringsAsFactors = FALSE
    ),
    search_database = data.frame(
      model_name = "run1", tags = I(list("WT_CL")), stringsAsFactors = FALSE
    )
  )
}

remove_wt <- function(models_dir) {
  invisible(capture.output(
    res <- remove_covariate_from_model(removal_state(models_dir), "run1",
                                       "cov_cl_wt", save_as_new_model = FALSE,
                                       step_number = 2L)
  ))
  res
}


test_that("a covariate THETA in a later $THETA record is found and removed", {
  models_dir <- write_two_record_model()

  res <- remove_wt(models_dir)
  ctl <- readLines(file.path(models_dir, "run1.ctl"))

  expect_equal(res$status, "success")
  expect_false(any(grepl("beta_WT_CL", ctl)))
  expect_true("TV_CL = THETA(1)" %in% trimws(ctl))
  # Every other THETA line survives, in both records
  expect_true(all(c("2.5 ; CL ; L/h ; RATIO", "30 ; V ; L ; RATIO",
                    "0.1 ; PROP ; ; RATIO") %in% ctl))
  expect_equal(sum(grepl("^\\$THETA", ctl)), 2L)
})


test_that("a THETA written on the $THETA record line is counted", {
  models_dir <- write_two_record_model(header_value = TRUE)

  res <- remove_wt(models_dir)
  ctl <- readLines(file.path(models_dir, "run1.ctl"))

  expect_equal(res$status, "success")
  expect_false(any(grepl("beta_WT_CL", ctl)))
  expect_true("TV_CL = THETA(1)" %in% trimws(ctl))
  expect_true("$THETA 2.5 ; CL ; L/h ; RATIO" %in% ctl)
})


test_that("a covariate THETA on a record line is refused, not deleted with the header", {
  models_dir <- write_two_record_model(cov_on_record = TRUE)
  ctl_path <- file.path(models_dir, "run1.ctl")
  ctl <- readLines(ctl_path)

  expect_error(
    invisible(capture.output(
      remove_covariate_from_model(removal_state(models_dir), "run1", "cov_cl_wt",
                                  save_as_new_model = FALSE, step_number = 2L)
    )),
    "record line itself"
  )
  expect_identical(readLines(ctl_path), ctl)
})


# CL on the first record line, the covariate THETA between CL and V, and V in a
# second record. Skipping the record line shifts every name down one THETA, so
# THETA2 would read as V (structural) and THETA3 would vanish.
write_interleaved_model <- function() {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c(
    "$PROBLEM record-line THETA",
    "$PK",
    "TV_CL = THETA(1) * (WT/70)**THETA(2)",
    "CL = TV_CL * EXP(ETA(1))",
    "TV_V = THETA(3)",
    "V = TV_V * EXP(ETA(2))",
    "$THETA 2.5 ; CL ; L/h ; RATIO",
    "0.75 ; beta_WT_CL ; ; RATIO",
    "$THETA",
    "30 ; V ; L ; RATIO",
    "$OMEGA",
    "0.1 ; IIV_CL ; ; RATIO",
    "0.1 ; IIV_V ; ; RATIO"
  ), file.path(models_dir, "run1.ctl"))
  models_dir
}


test_that("apply_covariate_model numbers a THETA on the record line", {
  models_dir <- write_interleaved_model()
  thetas <- data.frame(THETA1 = 2.5, THETA2 = 0.75, THETA3 = 30)
  cov_search <- data.frame(COVARIATE = "WT", REFERENCE = 70,
                           stringsAsFactors = FALSE)

  out <- apply_covariate_model("run1", cov_search, thetas, c(WT = 140),
                               models_folder = models_dir)

  expect_named(out, c("ID", "THETA1", "THETA3"))             # beta_ THETA2 excluded
  expect_equal(out$THETA1, 2.5 * 2^0.75, tolerance = 1e-8)   # CL, WT effect applied
  expect_equal(out$THETA3, 30)                               # V
})


# $PRIOR records sitting BETWEEN the two $THETA records. Their values are priors
# on existing THETAs/OMEGAs, not parameters of their own, so a reader that
# matched "$THETA" as a prefix would count $THETAP/$THETAPV lines as THETA3..6.
write_prior_model <- function() {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c(
    "$PROBLEM priors between THETA records",
    "$PK",
    "TV_CL = THETA(1) * (WT/70)**THETA(4)",
    "CL = TV_CL * EXP(ETA(1))",
    "TV_V = THETA(2)",
    "V = TV_V * EXP(ETA(2))",
    "$ERROR",
    "Y = F + F*THETA(3)*EPS(1)",
    "$PRIOR NWPRI",
    "$THETA",
    "2.5 ; CL ; L/h ; RATIO",
    "30 ; V ; L ; RATIO",
    "$THETAP",
    "2.5 FIX ; CL prior",
    "30 FIX ; V prior",
    "$THETAPV BLOCK(2)",
    "0.1 FIX",
    "0 0.1",
    "$THETA",
    "0.1 ; PROP ; ; RATIO",
    "0.75 ; beta_WT_CL ; ; RATIO",
    "$OMEGA",
    "0.1 ; IIV_CL ; ; RATIO",
    "0.1 ; IIV_V ; ; RATIO",
    "$OMEGAP",
    "0.1 FIX",
    "0.1 FIX",
    "$OMEGAPD",
    "5 FIX",
    "$SIGMA",
    "1 FIX ; EPS ; ; RATIO"
  ), file.path(models_dir, "run1.ctl"))
  models_dir
}


test_that("removal skips $THETAP/$THETAPV when numbering THETAs", {
  models_dir <- write_prior_model()

  res <- remove_wt(models_dir)
  ctl <- readLines(file.path(models_dir, "run1.ctl"))

  expect_equal(res$status, "success")
  expect_false(any(grepl("beta_WT_CL", ctl)))
  expect_true("TV_CL = THETA(1)" %in% trimws(ctl))
  expect_true(all(c("$THETAP", "2.5 FIX ; CL prior", "$THETAPV BLOCK(2)",
                    "0.1 FIX", "0 0.1") %in% ctl))
})


test_that("apply_covariate_model skips $THETAP/$THETAPV", {
  models_dir <- write_prior_model()
  thetas <- data.frame(THETA1 = 2.5, THETA2 = 30, THETA3 = 0.1, THETA4 = 0.75)
  cov_search <- data.frame(COVARIATE = "WT", REFERENCE = 70,
                           stringsAsFactors = FALSE)

  out <- apply_covariate_model("run1", cov_search, thetas, c(WT = 140),
                               models_folder = models_dir)

  expect_named(out, c("ID", "THETA1", "THETA2", "THETA3"))   # THETA4 is beta_
  expect_equal(out$THETA1, 2.5 * 2^0.75, tolerance = 1e-8)
})


test_that("extract_params skips the $PRIOR records of every kind", {
  lines <- readLines(file.path(write_prior_model(), "run1.ctl"))

  expect_equal(trimws(extract_params(lines, "THETA")$param),
               c("CL", "V", "PROP", "beta_WT_CL"))
  expect_equal(trimws(extract_params(lines, "OMEGA")$param),
               c("IIV_CL", "IIV_V"))
})


# Comments everywhere a modeller puts them: on the record line, as section
# notes, and a commented-out THETA that still looks like a full THETA line.
write_commented_model <- function() {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c(
    "$PROBLEM comments in THETA records",
    "$PK",
    "TV_CL = THETA(1) * (WT/70)**THETA(4)",
    "CL = TV_CL * EXP(ETA(1))",
    "TV_V = THETA(2)",
    "V = TV_V * EXP(ETA(2))",
    "$ERROR",
    "Y = F + F*THETA(3)*EPS(1)",
    "$THETA ; structural parameters",
    "; clearance first",
    "; CL ; L/h ; LOG",
    "2.5 ; CL ; L/h ; RATIO",
    "; 0.5 ; OLD_V ; L ; LOG",
    "   ; indented note",
    "30 ; V ; L ; RATIO",
    "$THETA ; residual error and covariates",
    "0.1 ; PROP ; ; RATIO",
    "; covariate effects",
    "0.75 ; beta_WT_CL ; ; RATIO",
    "$OMEGA",
    "; IIV",
    "0.1 ; IIV_CL ; ; RATIO",
    "0.1 ; IIV_V ; ; RATIO",
    "$SIGMA",
    "1 FIX ; EPS ; ; RATIO"
  ), file.path(models_dir, "run1.ctl"))
  models_dir
}


test_that("comment lines in $THETA records are not counted as THETAs", {
  models_dir <- write_commented_model()
  lines <- readLines(file.path(models_dir, "run1.ctl"))

  expect_equal(trimws(extract_params(lines, "THETA")$param),
               c("CL", "V", "PROP", "beta_WT_CL"))
  expect_equal(trimws(extract_params(lines, "OMEGA")$param),
               c("IIV_CL", "IIV_V"))
  expect_equal(.structural_param_names("run1", models_dir),
               c(THETA1 = "CL", THETA2 = "V"))

  thetas <- data.frame(THETA1 = 2.5, THETA2 = 30, THETA3 = 0.1, THETA4 = 0.75)
  out <- apply_covariate_model("run1",
                               data.frame(COVARIATE = "WT", REFERENCE = 70),
                               thetas, c(WT = 140), models_folder = models_dir)
  expect_named(out, c("ID", "THETA1", "THETA2", "THETA3"))
})


test_that("removal numbers THETAs past comment lines and keeps the comments", {
  models_dir <- write_commented_model()

  res <- remove_wt(models_dir)
  ctl <- readLines(file.path(models_dir, "run1.ctl"))

  expect_equal(res$status, "success")
  expect_false(any(grepl("beta_WT_CL", ctl)))
  expect_true("TV_CL = THETA(1)" %in% trimws(ctl))
  expect_true(all(c("; clearance first", "; 0.5 ; OLD_V ; L ; LOG",
                    "; CL ; L/h ; LOG") %in% ctl))
})


test_that("a commented-out line does not supply a parameter's RATIO/LOG tag", {
  lines <- readLines(file.path(write_commented_model(), "run1.ctl"))

  expect_equal(.theta_tag_for(lines, "CL"), "RATIO")
})


test_that("a value glued to the record name, $THETA(0,2.5), keeps its THETA", {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  lines <- c(
    "$PK",
    "TV_CL = THETA(1)",
    "CL = TV_CL * EXP(ETA(1))",
    "TV_V = THETA(2)",
    "V = TV_V * EXP(ETA(2))",
    "$THETA(0,2.5) ; CL ; L/h ; RATIO",
    "30 ; V ; L ; RATIO",
    "$OMEGA(0.1) ; IIV_CL ; ; RATIO",
    "0.1 ; IIV_V ; ; RATIO",
    "$SIGMA",
    "1 FIX ; EPS ; ; RATIO"
  )
  writeLines(lines, file.path(models_dir, "run1.ctl"))

  expect_equal(trimws(extract_params(lines, "THETA")$param), c("CL", "V"))
  expect_equal(trimws(extract_params(lines, "OMEGA")$param), c("IIV_CL", "IIV_V"))
  expect_equal(.structural_param_names("run1", models_dir),
               c(THETA1 = "CL", THETA2 = "V"))
  out <- apply_covariate_model("run1", data.frame(COVARIATE = "WT", REFERENCE = 70),
                               data.frame(THETA1 = 2.5, THETA2 = 30), c(WT = 70),
                               models_folder = models_dir)
  expect_named(out, c("ID", "THETA1", "THETA2"))
})


test_that("validate_parameter_blocks does not check $PRIOR records as parameters", {
  models_dir <- write_prior_model()

  res <- validate_parameter_blocks(file.path(models_dir, "run1.ctl"))

  # The $THETAP/$THETAPV/$OMEGAP/$OMEGAPD lines carry no "NAME ; units ; RATIO"
  # tags; read as parameter lines, each would be reported as an issue.
  expect_true(res$valid)
  expect_length(res$issues, 0)
})


# model_add_cov() needs only the model file, a dataset and the search table.
add_wt <- function(models_dir) {
  cs <- data.frame(PARAMETER = "CL", COVARIATE = "WT", STATUS = "con",
                   FORMULA = "power", REFERENCE = 70, LEVELS = NA,
                   cov_to_test = "beta_WT_CL", stringsAsFactors = FALSE)
  dat <- data.frame(ID = rep(1:4, each = 2), WT = rep(c(60, 70, 80, 90), each = 2))
  invisible(capture.output(
    model_add_cov(list(models_folder = models_dir, search_config = list()), "run1",
                  "WT_CL", data_file = dat, covariate_search = cs)
  ))
  readLines(file.path(models_dir, "run1.ctl"))
}

# write_prior_model() without its covariate: THETA(4) is what model_add_cov adds.
write_base_prior_model <- function() {
  models_dir <- write_prior_model()
  ctl_path <- file.path(models_dir, "run1.ctl")
  ctl <- readLines(ctl_path)
  ctl <- sub("TV_CL = THETA(1) * (WT/70)**THETA(4)", "TV_CL = THETA(1)", ctl, fixed = TRUE)
  writeLines(ctl[ctl != "0.75 ; beta_WT_CL ; ; RATIO"], ctl_path)
  models_dir
}


test_that("model_add_cov puts the new THETA at the end of the last $THETA record", {
  ctl <- add_wt(write_base_prior_model())

  new_line <- which(grepl("beta_WT_CL", ctl))
  expect_length(new_line, 1)
  # Directly after PROP, the last line of the second $THETA record -- not after
  # $OMEGA's predecessor and never inside $THETAP/$THETAPV.
  expect_equal(ctl[new_line - 1], "0.1 ; PROP ; ; RATIO")
  expect_equal(ctl[new_line + 1], "$OMEGA")
  expect_true("TV_CL = THETA(1) * (WT/70)**THETA(4)" %in% ctl)
})


test_that("model_add_cov does not write into $THETAP/$THETAPV placed before $OMEGA", {
  models_dir <- write_base_prior_model()
  ctl_path <- file.path(models_dir, "run1.ctl")
  ctl <- readLines(ctl_path)
  # Move the second $THETA record above the priors, so the priors end right at
  # $OMEGA: "insert before $OMEGA" would now land inside $THETAPV.
  rec2 <- which(ctl == "$THETA")[2]
  moved <- ctl[rec2:(rec2 + 1)]
  ctl <- ctl[-(rec2:(rec2 + 1))]
  ctl <- append(ctl, moved, after = which(ctl == "$THETAP") - 1)
  writeLines(ctl, ctl_path)

  out <- add_wt(models_dir)
  new_line <- which(grepl("beta_WT_CL", out))

  expect_equal(out[new_line - 1], "0.1 ; PROP ; ; RATIO")
  expect_equal(out[new_line + 1], "$THETAP")
  pv <- which(out == "$THETAPV BLOCK(2)")
  expect_equal(out[(pv + 1):(pv + 3)], c("0.1 FIX", "0 0.1", "$OMEGA"))
})


test_that("adding then removing a covariate restores the model exactly", {
  for (models_dir in list(write_base_prior_model(), write_commented_model())) {
    ctl_path <- file.path(models_dir, "run1.ctl")
    original <- readLines(ctl_path)
    if (any(grepl("beta_WT_CL", original))) {
      # write_commented_model() ships with the covariate; strip it to a base.
      original <- sub("TV_CL = THETA(1) * (WT/70)**THETA(4)", "TV_CL = THETA(1)",
                      original, fixed = TRUE)
      original <- original[original != "0.75 ; beta_WT_CL ; ; RATIO"]
      writeLines(original, ctl_path)
    }

    add_wt(models_dir)
    remove_wt(models_dir)

    # strip_covariate_factors() leaves a trailing space where the factor was
    # ("TV_CL = THETA(1) "); harmless to NONMEM, so compare without it.
    expect_identical(trimws(readLines(ctl_path), "right"), original)
  }
})


test_that(".structural_param_names reads both records, including a record-line THETA", {
  models_dir <- write_interleaved_model()

  expect_equal(
    .structural_param_names("run1", models_dir),
    c(THETA1 = "CL", THETA3 = "V")
  )
})
