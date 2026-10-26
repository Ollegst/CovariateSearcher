# detect_param_transform(): which scale the parameter's typical value is on,
# deciding whether a population covariate is multiplied in or added.

pk <- function(...) c("$PK", ...)

test_that("a typical value multiplying the EXP is normal scale, whatever shares the EXP", {
  # MOD-02: an IOV term or a scaled ETA inside the EXP made these "log".
  iov <- pk("IOVCL = 0", "IF(OCC.EQ.1) IOVCL = ETA(3)",
            "TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1) + IOVCL)")
  expect_equal(detect_param_transform(iov, "CL"), "normal")
  expect_equal(detect_param_transform(
    pk("TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1) * THETA(9))"), "CL"), "normal")
  expect_equal(detect_param_transform(
    pk("TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1) * SQRT(FLAG))"), "CL"), "normal")
})

test_that("log-scale shapes with extra terms in the EXP stay log", {
  # The checker's cases a stricter rule would have broken.
  expect_equal(detect_param_transform(
    pk("TV_CL = THETA(1)", "CL = EXP(TV_CL + ETA(1) + IOVCL)"), "CL"), "log")
  expect_equal(detect_param_transform(
    pk("TV_CL = THETA(1)", "CL = EXP(TV_CL + ETA(1) * THETA(9))"), "CL"), "log")
  mu_iov <- c(pk("TV_CL = THETA(1)", "MU_1 = LOG(TV_CL)", "CL = EXP(MU_1 + ETA(1) + IOVCL)"),
              "$THETA", "2.5 ; CL ; L/h ; LOG")
  expect_equal(detect_param_transform(mu_iov, "CL"), "log")
  mu_iov_ratio <- sub("; LOG$", "; RATIO", mu_iov)
  expect_equal(detect_param_transform(mu_iov_ratio, "CL"), "normal")
})

test_that("a covariate factor outside the EXP is not a typical value", {
  # Phase 3 audit: any THETA( outside the EXP counted, so a log parameter that
  # already had a time-varying factor appended became "normal".
  mu_wt <- c(pk("TV_CL = THETA(1)", "MU_1 = LOG(TV_CL)",
                "CL = EXP(MU_1 + ETA(1)) * (WT/70)**THETA(3)"),
             "$THETA", "2.5 ; CL ; L/h ; LOG")
  expect_equal(detect_param_transform(mu_wt, "CL"), "log")
  expect_equal(detect_param_transform(
    pk("CL = EXP(TVCL + ETA(1)) * (WT/70)**THETA(2)"), "CL"), "log")
  expect_equal(detect_param_transform(pk("CL = EXP(THETA(1) + ETA(1)) * THETA(3)"), "CL"), "log")
  # A bare THETA multiplying an EXP with no typical value inside is normal.
  expect_equal(detect_param_transform(pk("CL = THETA(1) * EXP(ETA(1) + IOVCL)"), "CL"), "normal")
  expect_equal(detect_param_transform(
    pk("TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1) + IOVCL) * (WT/70)**THETA(3)"), "CL"), "normal")
})

test_that("an unrecognised mix inside the EXP is unknown rather than guessed", {
  expect_equal(detect_param_transform(
    pk("CL = EXP(LOGCL + ETA(1) + IOVCL)"), "CL"), "unknown")
})

test_that("the existing shapes keep their classification", {
  expect_equal(detect_param_transform(pk("TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1))"), "CL"), "normal")
  expect_equal(detect_param_transform(pk("TV_CL = THETA(1)", "CL = EXP(TV_CL + ETA(1))"), "CL"), "log")
  expect_equal(detect_param_transform(pk("CL = EXP(THETA(1) + ETA(1))"), "CL"), "log")
  expect_equal(detect_param_transform(pk("CL = THETA(1)"), "CL"), "normal")
  expect_equal(detect_param_transform(pk("CL = THETA(1) + ETA(1)"), "CL"), "unknown")
  mu <- c(pk("TV_CL = THETA(1)", "MU_1 = LOG(TV_CL)", "CL = EXP(MU_1 + ETA(1))"),
          "$THETA", "2.5 ; CL ; L/h ; RATIO")
  expect_equal(detect_param_transform(mu, "CL"), "normal")
  expect_equal(detect_param_transform(sub("; RATIO$", "; LOG", mu), "CL"), "log")
})

test_that("a covariate on an IOV parameter is multiplied onto the typical value", {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  writeLines(c(
    "$PROBLEM iov", "$INPUT ID DV WT OCC", "$DATA d.csv",
    "$PK", "IOVCL = 0", "IF(OCC.EQ.1) IOVCL = ETA(2)",
    "TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1) + IOVCL)",
    "$THETA", "2.5 ; CL ; L/h ; RATIO",
    "$OMEGA", "0.1 ; IIV_CL ; ; RATIO", "0.05 ; IOV_CL ; ; RATIO",
    "$SIGMA", "1 ; EPS ; ; RATIO"
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
  expect_true("TV_CL = THETA(1) * (WT/70)**THETA(2)" %in% trimws(ctl))
})
