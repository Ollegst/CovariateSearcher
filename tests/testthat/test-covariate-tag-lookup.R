# get_covariate_tag_from_name maps a covariate name back to its tag.

test_that("the lookup is exact first, then case-insensitive", {
  # SCM-10: the fallback built "beta_cl_wt", which no tag is named, so a case
  # mismatch returned NULL.
  st <- list(tags = list(cov_cl_wt = "WT_CL", cov_v_sex = "SEX_V"))
  expect_equal(get_covariate_tag_from_name(st, "WT_CL"), "cov_cl_wt")
  expect_equal(get_covariate_tag_from_name(st, "wt_cl"), "cov_cl_wt")
  expect_null(get_covariate_tag_from_name(st, "AGE_CL"))
})
