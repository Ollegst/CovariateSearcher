# read_ext_iterations() on .ext files caught while NONMEM is still writing them.

ext_header <- " ITERATION    THETA1       THETA2       OMEGA(1,1)   OBJ"

test_that("a TABLE with a column header but no rows yet is skipped", {
  ext <- tempfile(fileext = ".ext")
  writeLines(c(
    "TABLE NO.     1: First Order Conditional Estimation: Goal Function=MIN",
    ext_header,
    "            0  1.0E+00  2.0E+00  1.0E-01  1500.0",
    "            5  1.1E+00  2.1E+00  1.1E-01  1400.0",
    "  -1000000000  1.2E+00  2.2E+00  1.2E-01  1390.0",
    "TABLE NO.     2: Importance Sampling: Goal Function=MIN",
    ext_header
  ), ext)

  out <- read_ext_iterations(ext)

  expect_equal(nrow(out), 3)
  expect_true(all(out$EST.NO == 1))
})


test_that("a file whose only TABLE has no rows yet gives an empty result", {
  ext <- tempfile(fileext = ".ext")
  writeLines(c(
    "TABLE NO.     1: First Order Conditional Estimation: Goal Function=MIN",
    ext_header
  ), ext)

  expect_equal(nrow(read_ext_iterations(ext)), 0)
})
