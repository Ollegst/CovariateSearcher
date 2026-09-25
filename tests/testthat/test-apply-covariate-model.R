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
