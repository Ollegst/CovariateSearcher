test_that("decode_cov_level returns the decoded category for a matched level", {
  lookup <- list(
    SEXN = list(values = c(0, 1, 2), decode = c("Ref", "Male", "Female"))
  )
  expect_equal(decode_cov_level("SEXN", "2", lookup), "Female")
  expect_equal(decode_cov_level("SEXN", 1, lookup), "Male")
})

test_that("decode_cov_level returns NA when no lookup is supplied", {
  expect_true(is.na(decode_cov_level("SEXN", "2", NULL)))
})

test_that("decode_cov_level returns NA when the covariate is absent from lookup", {
  lookup <- list(RACE = list(values = c(1, 2), decode = c("White", "Black")))
  expect_true(is.na(decode_cov_level("SEXN", "2", lookup)))
})

test_that("decode_cov_level returns NA when the level value is not in `values`", {
  lookup <- list(SEXN = list(values = c(0, 1), decode = c("Female", "Male")))
  expect_true(is.na(decode_cov_level("SEXN", "2", lookup)))
})

test_that("decode_cov_level returns NA when the entry lacks values/decode", {
  lookup <- list(SEXN = list(short = "Sex"))
  expect_true(is.na(decode_cov_level("SEXN", "2", lookup)))
})
