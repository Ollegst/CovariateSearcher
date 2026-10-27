# get_covariate_tag_from_name maps a covariate name back to its tag.

test_that("the lookup is exact first, then case-insensitive", {
  # SCM-10: the fallback built "beta_cl_wt", which no tag is named, so a case
  # mismatch returned NULL.
  st <- list(tags = list(cov_cl_wt = "WT_CL", cov_v_sex = "SEX_V"))
  expect_equal(get_covariate_tag_from_name(st, "WT_CL"), "cov_cl_wt")
  expect_equal(get_covariate_tag_from_name(st, "wt_cl"), "cov_cl_wt")
  expect_null(get_covariate_tag_from_name(st, "AGE_CL"))
})

test_that("load_tags reads the tags file it is given", {
  # STA-09: add_covariates_to_search(tags_yaml_path = ...) wrote that file but
  # load_tags always read data/spec/tags.yaml.
  f <- tempfile(fileext = ".yaml")
  writeLines('beta_WT_CL: "WT_CL"', f)
  invisible(capture.output(st <- load_tags(list(), f)))
  expect_equal(st$tags, list(beta_WT_CL = "WT_CL"))
})

test_that("tags can be generated from a covariate table saved as .rds", {
  # UTL-07: the loader read every path as CSV, so an .rds table failed with
  # "Missing required columns".
  cs <- data.frame(cov_to_test = "beta_WT_CL", COVARIATE = "WT", PARAMETER = "CL",
                   STATUS = "con", LEVELS = NA, FORMULA = "power", TIME_DEPENDENT = "No",
                   stringsAsFactors = FALSE)
  rds <- tempfile(fileext = ".rds")
  saveRDS(cs, rds)
  yml <- tempfile(fileext = ".yaml")
  invisible(capture.output(generate_tags_from_covariate_search(rds, tags_yaml_path = yml, verbose = FALSE)))
  expect_true(any(grepl('^beta_WT_CL: "WT_CL"', readLines(yml))))
})

test_that("discovered rows store the tag name, as search-created rows do", {
  # XAR-10: discovery stored the tag value (WT_CL), which the df lookup and the
  # exclusion/redemption checks (all keyed on tag names) do not recognise.
  st <- list(tags = list(beta_WT_CL = "WT_CL", beta_SEX_CL = "SEX_CL"))
  expect_equal(.tag_name_for(st, c("WT_CL", "SEX_CL")), c("beta_WT_CL", "beta_SEX_CL"))
  expect_equal(.tag_name_for(st, "beta_WT_CL"), "beta_WT_CL")   # already a name
  expect_equal(.tag_name_for(st, "AGE_V"), "AGE_V")             # no such tag
  expect_equal(.tag_name_for(list(), "WT_CL"), "WT_CL")
})
