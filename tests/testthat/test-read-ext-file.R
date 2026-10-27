# read_ext_file (monitoring): the OFV of a finished run comes from the
# final-estimate row, and a run is completed only when that row exists.

write_ext <- function(lines) {
  mf <- tempfile("models")
  dir.create(file.path(mf, "run1"), recursive = TRUE)
  writeLines(lines, file.path(mf, "run1", "run1.ext"))
  list(models_folder = mf)
}
hdr <- " ITERATION    THETA1    OMEGA(1,1)    OBJ"

test_that("a finished run reports the final-estimate OFV, not the SE row's 0", {
  # NIO-06: the last row is -1000000001 (standard errors), whose OBJ is 0.
  st <- write_ext(c("TABLE NO.     1: First Order Conditional Estimation with Interaction",
                    hdr,
                    "            0   1.0   0.1   1500.5",
                    "           10   1.2   0.1   1234.5",
                    "  -1000000000   1.2   0.1   1234.5",
                    "  -1000000001   0.05  0.01     0.0"))
  r <- read_ext_file(st, "run1")
  expect_equal(r$status, "completed")
  expect_equal(r$current_ofv, 1234.5)
  expect_equal(r$iterations, 10)
})

test_that("SAEM burn-in (negative iterations) is still progressing", {
  # NIO-06: any negative iteration counted as completed.
  st <- write_ext(c("TABLE NO.     1: Stochastic Approximation Expectation-Maximization",
                    hdr,
                    "        -2000   1.0   0.1   1800.0",
                    "        -1900   1.1   0.1   1700.0"))
  r <- read_ext_file(st, "run1")
  expect_equal(r$status, "progressing")
  expect_equal(r$current_ofv, 1700)
})

test_that("with several $EST steps the last table decides", {
  st <- write_ext(c("TABLE NO.     1: Stochastic Approximation Expectation-Maximization",
                    hdr,
                    "  -1000000000   1.1   0.1   1650.0",
                    "TABLE NO.     2: Importance Sampling",
                    hdr,
                    "            0   1.1   0.1   1620.0",
                    "            5   1.1   0.1   1610.0"))
  r <- read_ext_file(st, "run1")
  expect_equal(r$status, "progressing")
  expect_equal(r$estimation_method, "IMP")
  expect_equal(r$current_ofv, 1610)
})
