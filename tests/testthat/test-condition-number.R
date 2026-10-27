# The condition number is NONMEM's: largest over smallest eigenvalue of the
# correlation matrix, which has 1s on its diagonal.

test_that("the condition number matches NONMEM's for bbr's example model", {
  # REP-01: NONMEM's .cor holds standard errors on the diagonal; reading it
  # as is gave 10013 here where NONMEM's eigenvalues give 4353.
  folder <- system.file("model", "nonmem", "complex", package = "bbr")
  skip_if_not(file.exists(file.path(folder, "1001", "1001.cor")), "bbr example model not installed")
  cn <- calculate_condition_number("1001", models_folder = folder)
  expect_equal(cn, 5.18 / 1.19e-3, tolerance = 0.01)   # NONMEM-printed eigenvalues, 3 s.f.
})

test_that("a .cov fallback gives the same answer as the correlation matrix", {
  mf <- tempfile("models")
  dir.create(file.path(mf, "run1"), recursive = TRUE)
  cv <- matrix(c(4, 0.6, 0.6, 0.25), 2)            # SDs 2 and 0.5, correlation 0.6
  writeLines(c("TABLE NO.     1: First Order",
               " NAME           THETA1        THETA2",
               sprintf(" THETA1   %.6E  %.6E", cv[1, 1], cv[1, 2]),
               sprintf(" THETA2   %.6E  %.6E", cv[2, 1], cv[2, 2])),
             file.path(mf, "run1", "run1.cov"))
  # Correlation 0.6 -> eigenvalues 1.6 and 0.4 -> condition number 4.
  expect_equal(calculate_condition_number("run1", models_folder = mf), 4, tolerance = 1e-6)
})
