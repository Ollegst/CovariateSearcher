asian_lookup <- function() {
  list(ASIAN = list(short = "Race2", type = "numeric",
                    values = c(0, 1), decode = c("Non-Asian", "Asian")))
}

test_that(".cov_display_name prefers the lookup's short name", {
  expect_equal(.cov_display_name("ASIAN", asian_lookup()), "Race2")
})

test_that(".cov_display_name falls back to label, then spec_pk, then the column", {
  lk <- list(ASIAN = list(label = "Asian race indicator"))
  expect_equal(.cov_display_name("ASIAN", lk), "Asian race indicator")

  spec <- list(ASIAN = list(short = "Race2", unit = ""))
  expect_equal(.cov_display_name("ASIAN", NULL, spec), "Race2")

  # The lookup wins when both spell one out
  expect_equal(.cov_display_name("ASIAN", asian_lookup(), list(
    ASIAN = list(short = "FromSpec")
  )), "Race2")
})

test_that(".cov_display_name keeps the column name when nothing names it", {
  expect_equal(.cov_display_name("ASIAN", NULL, NULL), "ASIAN")
  expect_equal(.cov_display_name("ASIAN", list(WT = list(short = "Weight"))),
               "ASIAN")
  expect_equal(.cov_display_name("ASIAN", list(ASIAN = list(short = ""))),
               "ASIAN")
  expect_equal(.cov_display_name("ASIAN", list(ASIAN = list(type = "numeric"))),
               "ASIAN")
})

test_that("the lookup key stays the column name, not the display name", {
  # decode_cov_level must keep resolving on ASIAN after the label says Race2
  expect_equal(decode_cov_level("ASIAN", 1, asian_lookup()), "Asian")
  expect_true(is.na(decode_cov_level("Race2", 1, asian_lookup())))
})
