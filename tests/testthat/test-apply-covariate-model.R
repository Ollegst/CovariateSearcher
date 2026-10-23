# A minimal NONMEM run on disk for apply_covariate_model(): <models_dir>/<model>.ctl
# carries $THETA/$PK, <models_dir>/<model>/<model>.ext carries the FINAL row
# .read_final_omega_matrix() reads OMEGA from. Two structural parameters (CL,
# V), each log-parameterised with its own ETA; CL also carries a covariate
# (beta_WT_CL) so covariate effect and ETA can be checked together.
setup_apply_cov_models <- function() {
  models_dir <- tempfile("models")

  theta1 <- round(log(0.5), 7)   # CL reference value
  theta2 <- round(log(10), 7)    # V  reference value
  theta3 <- 0.75                 # beta_WT_CL

  pk_ok <- c(
    "$PK",
    "TVCL = THETA(1) + THETA(3)*LOG(WT/70)",
    "CL = EXP(TVCL + ETA(1))",
    "TVV = THETA(2)",
    "V = EXP(TVV + ETA(2))"
  )
  # Same model, but V's equation references ETA(3), which the (2-element)
  # OMEGA below never estimates.
  pk_badeta <- c(
    "$PK",
    "TVCL = THETA(1) + THETA(3)*LOG(WT/70)",
    "CL = EXP(TVCL + ETA(1))",
    "TVV = THETA(2)",
    "V = EXP(TVV + ETA(3))"
  )
  theta_block <- c(
    "$THETA",
    sprintf("%.7f ; CL ; L/h ; LOG", theta1),
    sprintf("%.7f ; V ; L ; LOG", theta2),
    sprintf("%.7f ; beta_WT_CL ; ; LOG", theta3)
  )

  write_model <- function(model, pk_lines) {
    dir.create(file.path(models_dir, model), recursive = TRUE)
    writeLines(c(theta_block, "$OMEGA", "0.09 ; IIV_CL", "0.16 ; IIV_V", pk_lines),
               file.path(models_dir, paste0(model, ".ctl")))
    writeLines(
      c(
        "TABLE NO.     1: First Order Conditional Estimation: Goal Function=MIN",
        " ITERATION THETA1 THETA2 THETA3 OMEGA(1,1) OMEGA(2,2) OBJ",
        sprintf(" -1000000000 %.7f %.7f %.7f 0.09 0.16 100.0", theta1, theta2, theta3)
      ),
      file.path(models_dir, model, paste0(model, ".ext"))
    )
  }
  write_model("run1", pk_ok)
  write_model("run_badeta", pk_badeta)

  # A third, standalone .ext (no .ctl needed) for .read_final_omega_matrix()
  # alone: a correlated 2x2 BLOCK, to check the off-diagonal is filled and
  # kept symmetric rather than only ever exercising a diagonal OMEGA.
  dir.create(file.path(models_dir, "run_block"), recursive = TRUE)
  writeLines(
    c(
      "TABLE NO.     1: First Order Conditional Estimation: Goal Function=MIN",
      " ITERATION OMEGA(1,1) OMEGA(2,1) OMEGA(2,2) OBJ",
      " -1000000000 0.09 0.03 0.16 50.0"
    ),
    file.path(models_dir, "run_block", "run_block.ext")
  )

  list(models_dir = models_dir, theta1 = theta1, theta2 = theta2, theta3 = theta3)
}

cov_search <- data.frame(COVARIATE = "WT", REFERENCE = 70, stringsAsFactors = FALSE)

thetas_df <- function(N, theta1, theta2, theta3) {
  data.frame(THETA1 = rep(theta1, N), THETA2 = rep(theta2, N), THETA3 = rep(theta3, N))
}


test_that("eta = FALSE (default) reproduces the typical-subject value, unchanged", {
  m <- setup_apply_cov_models()
  ind <- thetas_df(5, m$theta1, m$theta2, m$theta3)

  out <- apply_covariate_model("run1", cov_search, ind, c(WT = 70),
                               models_folder = m$models_dir)

  expect_equal(out$THETA1, rep(0.5, 5), tolerance = 1e-6)   # CL, covariate at reference
  expect_equal(out$THETA2, rep(10, 5), tolerance = 1e-6)    # V
})


test_that("eta = TRUE draws match a manual MVN(0, OMEGA) draw with the same seed", {
  m <- setup_apply_cov_models()
  N <- 200
  ind <- thetas_df(N, m$theta1, m$theta2, m$theta3)

  out <- apply_covariate_model("run1", cov_search, ind, c(WT = 70),
                               models_folder = m$models_dir, eta = TRUE, seed = 123)

  set.seed(123)
  omega_mat <- .make_psd(matrix(c(0.09, 0, 0, 0.16), 2, 2))
  expected_eta <- mvtnorm::rmvnorm(N, mean = c(0, 0), sigma = omega_mat, method = "svd")

  # Back out the realised ETA from the natural-scale output (WT at reference,
  # so CL's covariate term is exactly 0): log(CL) - TVCL = ETA(1), etc.
  expect_equal(log(out$THETA1) - m$theta1, expected_eta[, 1], tolerance = 1e-6)
  expect_equal(log(out$THETA2) - m$theta2, expected_eta[, 2], tolerance = 1e-6)

  # Genuine per-row variability, not one draw repeated.
  expect_gt(length(unique(round(out$THETA1, 8))), 1)
})


test_that("varying THETA rows still pair with the matching ETA row (no row shuffling)", {
  m <- setup_apply_cov_models()
  N <- 30
  # THETA1/THETA2 vary per row (unlike thetas_df()'s constant rows), so this
  # checks row PAIRING specifically: if THETA draw i were ever combined with
  # another row's ETA draw, the back-solved ETA below would land on the wrong
  # value and fail to match the independently reconstructed draw at that row.
  theta1_rows <- m$theta1 + seq_len(N) * 0.01
  theta2_rows <- m$theta2 - seq_len(N) * 0.02
  ind <- data.frame(THETA1 = theta1_rows, THETA2 = theta2_rows,
                    THETA3 = rep(m$theta3, N))

  out <- apply_covariate_model("run1", cov_search, ind, c(WT = 70),
                               models_folder = m$models_dir, eta = TRUE, seed = 55)

  set.seed(55)
  omega_mat <- .make_psd(matrix(c(0.09, 0, 0, 0.16), 2, 2))
  expected_eta <- mvtnorm::rmvnorm(N, mean = c(0, 0), sigma = omega_mat, method = "svd")

  # WT at reference, so CL's covariate term is 0 for every row.
  expect_equal(log(out$THETA1) - theta1_rows, expected_eta[, 1], tolerance = 1e-6)
  expect_equal(log(out$THETA2) - theta2_rows, expected_eta[, 2], tolerance = 1e-6)
})


test_that("eta = TRUE is reproducible with the same seed, differs across seeds", {
  m <- setup_apply_cov_models()
  ind <- thetas_df(50, m$theta1, m$theta2, m$theta3)

  a <- apply_covariate_model("run1", cov_search, ind, c(WT = 70),
                             models_folder = m$models_dir, eta = TRUE, seed = 42)
  b <- apply_covariate_model("run1", cov_search, ind, c(WT = 70),
                             models_folder = m$models_dir, eta = TRUE, seed = 42)
  d <- apply_covariate_model("run1", cov_search, ind, c(WT = 70),
                             models_folder = m$models_dir, eta = TRUE, seed = 99)

  expect_identical(a, b)
  expect_false(isTRUE(all.equal(a$THETA1, d$THETA1)))
})


test_that("covariate effect and ETA combine on the log scale", {
  m <- setup_apply_cov_models()
  N <- 100
  ind <- thetas_df(N, m$theta1, m$theta2, m$theta3)

  out <- apply_covariate_model("run1", cov_search, ind, c(WT = 140),
                               models_folder = m$models_dir, eta = TRUE, seed = 7)

  set.seed(7)
  omega_mat <- .make_psd(matrix(c(0.09, 0, 0, 0.16), 2, 2))
  expected_eta <- mvtnorm::rmvnorm(N, mean = c(0, 0), sigma = omega_mat, method = "svd")
  cov_term <- m$theta3 * log(140 / 70)

  expect_equal(log(out$THETA1) - m$theta1 - cov_term, expected_eta[, 1], tolerance = 1e-6)
})


test_that(".read_final_omega_matrix fills a symmetric matrix, zero where unestimated", {
  m <- setup_apply_cov_models()

  mat <- .read_final_omega_matrix("run1", m$models_dir)
  expect_equal(dim(mat), c(2L, 2L))
  expect_equal(mat[1, 1], 0.09)
  expect_equal(mat[2, 2], 0.16)
  expect_equal(mat[1, 2], 0)
  expect_equal(mat[2, 1], 0)

  block <- .read_final_omega_matrix("run_block", m$models_dir)
  expect_equal(block[1, 1], 0.09)
  expect_equal(block[2, 2], 0.16)
  expect_equal(block[2, 1], 0.03)
  expect_equal(block[1, 2], 0.03)   # symmetric fill, not just the lower triangle
})


test_that("eta = TRUE stops if $PK references an ETA beyond OMEGA's size", {
  m <- setup_apply_cov_models()
  ind <- thetas_df(5, m$theta1, m$theta2, m$theta3)

  expect_error(
    apply_covariate_model("run_badeta", cov_search, ind, c(WT = 70),
                          models_folder = m$models_dir, eta = TRUE, seed = 1),
    "ETA\\(3\\).*only has 2"
  )
})


test_that("eta = FALSE never consults OMEGA, so an out-of-range ETA(k) is harmless", {
  m <- setup_apply_cov_models()
  ind <- thetas_df(5, m$theta1, m$theta2, m$theta3)

  # ETA(k) is unconditionally 0 on this path for any k, regardless of what
  # the model's OMEGA actually estimates -- run_badeta's ETA(3) reference
  # (which only errors when eta = TRUE, see the test above) evaluates fine.
  out <- apply_covariate_model("run_badeta", cov_search, ind, c(WT = 70),
                               models_folder = m$models_dir)

  expect_equal(out$THETA2, rep(10, 5), tolerance = 1e-6)
})


# ---- Per-row `covariates` (real per-subject combinations) --------------------

# A model with BOTH a continuous covariate (WT, log-additive on CL) and a
# categorical one (SEXN, via IF/ELSEIF in $PK, re-assigning TVCL inside the
# branch) - exercises the masked partial-vector assignment do_assign() uses
# for per-row covariates, not just "create the variable inside a branch".
setup_categorical_model <- function() {
  models_dir <- tempfile("models")
  model <- "run_cat"
  dir.create(file.path(models_dir, model), recursive = TRUE)

  theta1 <- round(log(0.5), 7)   # CL reference (WT = 70, SEXN = 1)
  theta2 <- round(log(10), 7)    # V reference
  theta3 <- 0.75                 # beta_WT_CL (continuous, log-additive)
  theta4 <- 0.3                  # beta_SEXN_CL (SEXN == 2, log-additive)

  writeLines(
    c(
      "$THETA",
      sprintf("%.7f ; CL ; L/h ; LOG", theta1),
      sprintf("%.7f ; V ; L ; LOG", theta2),
      sprintf("%.7f ; beta_WT_CL ; ; LOG", theta3),
      sprintf("%.7f ; beta_SEXN_CL ; ; LOG", theta4),
      "$OMEGA",
      "0.09 ; IIV_CL",
      "0.16 ; IIV_V",
      "$PK",
      "TVCL = THETA(1) + THETA(3)*LOG(WT/70)",
      "IF (SEXN.EQ.2) THEN",
      "   TVCL = TVCL + THETA(4)",
      "ENDIF",
      "CL = EXP(TVCL + ETA(1))",
      "TVV = THETA(2)",
      "V = EXP(TVV + ETA(2))"
    ),
    file.path(models_dir, paste0(model, ".ctl"))
  )
  writeLines(
    c(
      "TABLE NO.     1: First Order Conditional Estimation: Goal Function=MIN",
      " ITERATION THETA1 THETA2 THETA3 THETA4 OMEGA(1,1) OMEGA(2,2) OBJ",
      sprintf(" -1000000000 %.7f %.7f %.7f %.7f 0.09 0.16 100.0",
             theta1, theta2, theta3, theta4)
    ),
    file.path(models_dir, model, paste0(model, ".ext"))
  )

  list(models_dir = models_dir, model = model,
       theta1 = theta1, theta2 = theta2, theta3 = theta3, theta4 = theta4)
}

cat_cov_search <- data.frame(COVARIATE = c("WT", "SEXN"), REFERENCE = c(70, 1),
                             stringsAsFactors = FALSE)

cat_thetas_df <- function(N, m) {
  data.frame(THETA1 = rep(m$theta1, N), THETA2 = rep(m$theta2, N),
            THETA3 = rep(m$theta3, N), THETA4 = rep(m$theta4, N))
}


test_that("scalar categorical covariate (IF/ELSEIF) selects the right branch", {
  m <- setup_categorical_model()
  ind <- cat_thetas_df(5, m)

  ref   <- apply_covariate_model(m$model, cat_cov_search, ind, c(WT = 70, SEXN = 1),
                                 models_folder = m$models_dir)
  sexn2 <- apply_covariate_model(m$model, cat_cov_search, ind, c(WT = 70, SEXN = 2),
                                 models_folder = m$models_dir)

  expect_equal(ref$THETA1, rep(0.5, 5), tolerance = 1e-6)
  expect_equal(sexn2$THETA1, rep(exp(m$theta1 + m$theta4), 5), tolerance = 1e-6)
})


test_that("per-row covariates (continuous + categorical) pair with their own row", {
  m <- setup_categorical_model()
  ind <- cat_thetas_df(4, m)
  covs <- data.frame(WT = c(70, 70, 140, 140), SEXN = c(1, 2, 1, 2),
                     stringsAsFactors = FALSE)

  out <- apply_covariate_model(m$model, cat_cov_search, ind, covs,
                               models_folder = m$models_dir)

  expected <- c(
    exp(m$theta1),
    exp(m$theta1 + m$theta4),
    exp(m$theta1 + m$theta3 * log(2)),
    exp(m$theta1 + m$theta3 * log(2) + m$theta4)
  )
  expect_equal(out$THETA1, expected, tolerance = 1e-6)
})


test_that("per-row covariates combine correctly with eta = TRUE", {
  m <- setup_categorical_model()
  N <- 4
  ind <- cat_thetas_df(N, m)
  covs <- data.frame(WT = c(70, 70, 140, 140), SEXN = c(1, 2, 1, 2),
                     stringsAsFactors = FALSE)

  out <- apply_covariate_model(m$model, cat_cov_search, ind, covs,
                               models_folder = m$models_dir, eta = TRUE, seed = 321)

  set.seed(321)
  omega_mat <- .make_psd(matrix(c(0.09, 0, 0, 0.16), 2, 2))
  expected_eta <- mvtnorm::rmvnorm(N, mean = c(0, 0), sigma = omega_mat, method = "svd")

  tvcl <- c(
    m$theta1,
    m$theta1 + m$theta4,
    m$theta1 + m$theta3 * log(2),
    m$theta1 + m$theta3 * log(2) + m$theta4
  )
  expect_equal(log(out$THETA1) - tvcl, expected_eta[, 1], tolerance = 1e-6)
})


test_that("an NA anywhere in a per-row covariate column stops, rather than falling back", {
  m <- setup_categorical_model()
  ind <- cat_thetas_df(3, m)
  covs <- data.frame(WT = c(70, 70, 70), SEXN = c(1, NA, 1), stringsAsFactors = FALSE)

  expect_error(
    apply_covariate_model(m$model, cat_cov_search, ind, covs,
                          models_folder = m$models_dir),
    "SEXN.*NA.*row"
  )
})


test_that("a column that is entirely non-numeric (e.g. a Scenario label) is ignored", {
  m <- setup_categorical_model()
  ind <- cat_thetas_df(4, m)
  covs <- data.frame(
    Scenario = c("A", "B", "C", "D"),   # not a real covariate - must be ignored
    WT   = c(70, 70, 140, 140),
    SEXN = c(1,  2,  1,   2),
    stringsAsFactors = FALSE
  )

  out <- apply_covariate_model(m$model, cat_cov_search, ind, covs,
                               models_folder = m$models_dir)

  expected <- c(
    exp(m$theta1),
    exp(m$theta1 + m$theta4),
    exp(m$theta1 + m$theta3 * log(2)),
    exp(m$theta1 + m$theta3 * log(2) + m$theta4)
  )
  expect_equal(out$THETA1, expected, tolerance = 1e-6)
})


# ---- Silent-NA warning: a $PK branch with no ELSE, unrelated to covariate NA -

setup_noelse_model <- function() {
  models_dir <- tempfile("models")
  model <- "run_noelse"
  dir.create(file.path(models_dir, model), recursive = TRUE)

  theta1 <- round(log(0.5), 7)
  theta2 <- round(log(10), 7)

  writeLines(
    c(
      "$THETA",
      sprintf("%.7f ; CL ; L/h ; LOG", theta1),
      sprintf("%.7f ; V ; L ; LOG", theta2),
      "$OMEGA",
      "0.09 ; IIV_CL",
      "0.16 ; IIV_V",
      "$PK",
      "IF (SEXN.EQ.1) THEN",
      "   TVCL = THETA(1)",
      "ENDIF",
      "CL = EXP(TVCL + ETA(1))",
      "TVV = THETA(2)",
      "V = EXP(TVV + ETA(2))"
    ),
    file.path(models_dir, paste0(model, ".ctl"))
  )
  writeLines(
    c(
      "TABLE NO.     1: First Order Conditional Estimation: Goal Function=MIN",
      " ITERATION THETA1 THETA2 OMEGA(1,1) OMEGA(2,2) OBJ",
      sprintf(" -1000000000 %.7f %.7f 0.09 0.16 100.0", theta1, theta2)
    ),
    file.path(models_dir, model, paste0(model, ".ext"))
  )

  list(models_dir = models_dir, model = model, theta1 = theta1, theta2 = theta2)
}


test_that("a row matching no branch, with no ELSE, warns and comes back NA", {
  m <- setup_noelse_model()
  ind <- data.frame(THETA1 = rep(m$theta1, 3), THETA2 = rep(m$theta2, 3))
  cs   <- data.frame(COVARIATE = "SEXN", REFERENCE = 1, stringsAsFactors = FALSE)
  covs <- data.frame(SEXN = c(1, 2, 1), stringsAsFactors = FALSE)   # all real values, no NA

  expect_warning(
    out <- apply_covariate_model(m$model, cs, ind, covs, models_folder = m$models_dir),
    "contain NA"
  )
  expect_false(is.na(out$THETA1[1]))
  expect_true(is.na(out$THETA1[2]))    # SEXN = 2 matches no branch, no ELSE
  expect_false(is.na(out$THETA1[3]))
})


# ---- ELSEIF / ELSE coverage (run_cat above only ever exercises IF/ENDIF) -----

# Every branch SETS TVCL outright (no unconditional baseline line outside the
# block), so ELSEIF and ELSE each have an observable, distinguishable effect.
setup_elseif_model <- function() {
  models_dir <- tempfile("models")
  model <- "run_elseif"
  dir.create(file.path(models_dir, model), recursive = TRUE)

  theta1 <- round(log(0.5), 7)   # CL, SEXN == 1
  theta2 <- round(log(10), 7)    # V
  theta3 <- 0.3                  # beta_SEXN2_CL (SEXN == 2)
  theta4 <- -0.2                 # beta_SEXN3_CL (SEXN == anything else)

  writeLines(
    c(
      "$THETA",
      sprintf("%.7f ; CL ; L/h ; LOG", theta1),
      sprintf("%.7f ; V ; L ; LOG", theta2),
      sprintf("%.7f ; beta_SEXN2_CL ; ; LOG", theta3),
      sprintf("%.7f ; beta_SEXN3_CL ; ; LOG", theta4),
      "$OMEGA",
      "0.09 ; IIV_CL",
      "0.16 ; IIV_V",
      "$PK",
      "IF (SEXN.EQ.1) THEN",
      "   TVCL = THETA(1)",
      "ELSEIF (SEXN.EQ.2) THEN",
      "   TVCL = THETA(1) + THETA(3)",
      "ELSE",
      "   TVCL = THETA(1) + THETA(4)",
      "ENDIF",
      "CL = EXP(TVCL + ETA(1))",
      "TVV = THETA(2)",
      "V = EXP(TVV + ETA(2))"
    ),
    file.path(models_dir, paste0(model, ".ctl"))
  )
  writeLines(
    c(
      "TABLE NO.     1: First Order Conditional Estimation: Goal Function=MIN",
      " ITERATION THETA1 THETA2 THETA3 THETA4 OMEGA(1,1) OMEGA(2,2) OBJ",
      sprintf(" -1000000000 %.7f %.7f %.7f %.7f 0.09 0.16 100.0",
             theta1, theta2, theta3, theta4)
    ),
    file.path(models_dir, model, paste0(model, ".ext"))
  )

  list(models_dir = models_dir, model = model,
       theta1 = theta1, theta2 = theta2, theta3 = theta3, theta4 = theta4)
}

elseif_cov_search <- data.frame(COVARIATE = "SEXN", REFERENCE = 1, stringsAsFactors = FALSE)

elseif_thetas_df <- function(N, m) {
  data.frame(THETA1 = rep(m$theta1, N), THETA2 = rep(m$theta2, N),
            THETA3 = rep(m$theta3, N), THETA4 = rep(m$theta4, N))
}


test_that("scalar IF/ELSEIF/ELSE chain selects each branch correctly", {
  m <- setup_elseif_model()
  ind <- elseif_thetas_df(3, m)

  r1 <- apply_covariate_model(m$model, elseif_cov_search, ind, c(SEXN = 1),
                              models_folder = m$models_dir)
  r2 <- apply_covariate_model(m$model, elseif_cov_search, ind, c(SEXN = 2),
                              models_folder = m$models_dir)
  r3 <- apply_covariate_model(m$model, elseif_cov_search, ind, c(SEXN = 3),
                              models_folder = m$models_dir)

  expect_equal(r1$THETA1, rep(exp(m$theta1), 3), tolerance = 1e-6)
  expect_equal(r2$THETA1, rep(exp(m$theta1 + m$theta3), 3), tolerance = 1e-6)
  expect_equal(r3$THETA1, rep(exp(m$theta1 + m$theta4), 3), tolerance = 1e-6)
})


test_that("per-row covariates route each row through its own IF/ELSEIF/ELSE branch", {
  m <- setup_elseif_model()
  N <- 5
  ind <- elseif_thetas_df(N, m)
  covs <- data.frame(SEXN = c(1, 2, 3, 2, 1), stringsAsFactors = FALSE)

  out <- apply_covariate_model(m$model, elseif_cov_search, ind, covs,
                               models_folder = m$models_dir)

  expected <- exp(m$theta1 + c(0, m$theta3, m$theta4, m$theta3, 0))
  expect_equal(out$THETA1, expected, tolerance = 1e-6)
})


test_that("covariates with a row count matching neither 1 nor N errors", {
  m <- setup_categorical_model()
  ind <- cat_thetas_df(5, m)
  covs <- data.frame(WT = c(70, 90), SEXN = c(1, 2), stringsAsFactors = FALSE)

  expect_error(
    apply_covariate_model(m$model, cat_cov_search, ind, covs,
                          models_folder = m$models_dir),
    "same number of rows"
  )
})


# ---- Overlapping ELSEIF and nested IF (masking edge cases) -------------------

# WT.GT.100 and WT.GT.50 overlap for a high WT - only the `already` exclusion
# (has this row matched an earlier branch of the SAME chain?) can tell them
# apart; a condition-only check would wrongly let a later ELSEIF re-claim a
# row an earlier IF already matched.
setup_overlap_model <- function() {
  models_dir <- tempfile("models")
  model <- "run_overlap"
  dir.create(file.path(models_dir, model), recursive = TRUE)

  theta1 <- round(log(0.5), 7)
  theta2 <- round(log(10), 7)
  theta3 <- 0.4     # WT > 100
  theta4 <- 0.2     # WT > 50 (but not > 100)

  writeLines(
    c(
      "$THETA",
      sprintf("%.7f ; CL ; L/h ; LOG", theta1),
      sprintf("%.7f ; V ; L ; LOG", theta2),
      sprintf("%.7f ; beta_WTHIGH_CL ; ; LOG", theta3),
      sprintf("%.7f ; beta_WTMID_CL ; ; LOG", theta4),
      "$OMEGA", "0.09 ; IIV_CL", "0.16 ; IIV_V",
      "$PK",
      "IF (WT.GT.100) THEN",
      "   TVCL = THETA(1) + THETA(3)",
      "ELSEIF (WT.GT.50) THEN",
      "   TVCL = THETA(1) + THETA(4)",
      "ELSE",
      "   TVCL = THETA(1)",
      "ENDIF",
      "CL = EXP(TVCL + ETA(1))",
      "TVV = THETA(2)",
      "V = EXP(TVV + ETA(2))"
    ),
    file.path(models_dir, paste0(model, ".ctl"))
  )
  writeLines(
    c(
      "TABLE NO.     1: First Order Conditional Estimation: Goal Function=MIN",
      " ITERATION THETA1 THETA2 THETA3 THETA4 OMEGA(1,1) OMEGA(2,2) OBJ",
      sprintf(" -1000000000 %.7f %.7f %.7f %.7f 0.09 0.16 100.0",
             theta1, theta2, theta3, theta4)
    ),
    file.path(models_dir, model, paste0(model, ".ext"))
  )

  list(models_dir = models_dir, model = model,
       theta1 = theta1, theta2 = theta2, theta3 = theta3, theta4 = theta4)
}

overlap_cov_search <- data.frame(COVARIATE = "WT", REFERENCE = 70, stringsAsFactors = FALSE)


test_that("per-row ELSEIF excludes rows an earlier, overlapping IF already matched", {
  m <- setup_overlap_model()
  N <- 3
  ind <- data.frame(THETA1 = rep(m$theta1, N), THETA2 = rep(m$theta2, N),
                    THETA3 = rep(m$theta3, N), THETA4 = rep(m$theta4, N))
  # WT = 120 satisfies BOTH WT.GT.100 and WT.GT.50 - condition truth alone
  # cannot distinguish it from WT = 70; only `already` can.
  covs <- data.frame(WT = c(120, 70, 30), stringsAsFactors = FALSE)

  out <- apply_covariate_model(m$model, overlap_cov_search, ind, covs,
                               models_folder = m$models_dir)

  expected <- exp(m$theta1 + c(m$theta3, m$theta4, 0))
  expect_equal(out$THETA1, expected, tolerance = 1e-6)
})


test_that("a factor covariate column uses its labels, not internal level codes", {
  m <- setup_categorical_model()
  ind <- cat_thetas_df(3, m)
  # Factor levels sort alphabetically as "1", "10", "2" - very different from
  # numeric order. as.numeric() applied directly to a factor returns level
  # codes (1, 2, 3 here), not the displayed values (1, 10, 2): silently wrong
  # data, not NA, so the NA check alone can't catch it.
  covs <- data.frame(WT = c(70, 70, 70), SEXN = factor(c("1", "10", "2")),
                     stringsAsFactors = FALSE)

  out <- apply_covariate_model(m$model, cat_cov_search, ind, covs,
                               models_folder = m$models_dir)

  # Only SEXN == 2 (row 3) should trigger the beta_SEXN_CL effect - under the
  # level-code bug, row 2 (labelled "10", level-code 2) would get it instead.
  expected <- c(exp(m$theta1), exp(m$theta1), exp(m$theta1 + m$theta4))
  expect_equal(out$THETA1, expected, tolerance = 1e-6)
})


