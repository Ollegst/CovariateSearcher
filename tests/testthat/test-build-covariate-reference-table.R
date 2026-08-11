# build_covariate_reference_table: LEVELS must reflect the FULL dataset, not
# just baseline (Time == 0). A categorical covariate that only reaches some
# of its levels away from baseline (e.g. a dose captured under a
# time-varying "power" formula) must still get every level in its LEVELS
# string, since validate_covariate_search_table checks LEVELS against the
# full dataset. REFERENCE stays baseline-derived by design; a time-dependent
# covariate warns that its REFERENCE reflects baseline only.

ref_table_data <- function() {
  ids <- 1:5
  do.call(rbind, lapply(ids, function(i) {
    times <- c(0, 24, 48)
    data.frame(
      ID   = i,
      TIME = times,
      # time-independent categorical, baseline majority 0 (subjects 1-3)
      SEX  = rep(if (i <= 3) 0 else 1, 3),
      # time-dependent categorical: subjects 4-5 escalate past baseline
      DOSE = if (i <= 3) c(50, 50, 50) else c(150, 300, 450),
      # time-independent categorical whose codes sort differently as strings
      RACE = rep(c(3, 20, 100, 20, 3)[i], 3),
      # time-independent continuous
      WT   = rep(69 + i, 3),
      # time-dependent continuous
      CRCL = 90 - times / 2 + i,
      stringsAsFactors = FALSE
    )
  }))
}

# DOSE/RACE/SEX values match exactly what ref_table_data() observes, so tests
# that aren't about the yaml cross-check itself don't pick up incidental
# extra_in_yaml warnings.
ref_table_yaml <- function() {
  list(
    SEX  = list(values = c(0, 1)),
    DOSE = list(values = c(50, 150, 300, 450)),
    RACE = list(values = c(3, 20, 100))
  )
}


test_that("categorical LEVELS captures values only seen away from baseline", {
  out <- suppressWarnings(build_covariate_reference_table(
    data = ref_table_data(), id = "ID", time = "TIME",
    Parameter = "CL", Covariate = "DOSE", Category = "cat", Formula = "power",
    yaml_data = ref_table_yaml()
  ))

  # Baseline alone only ever sees 50 and 150 - subjects 4-5 reach 300/450 later.
  expect_equal(out$LEVELS, "50;150;300;450")
  expect_equal(out$TIME_DEPENDENT, "Yes")
  # REFERENCE stays baseline-derived: 3 of 5 subjects start on 50.
  expect_equal(out$REFERENCE, "50")
})


test_that("LEVELS follows yaml order, not appearance order or a string sort", {
  # DOSE: appearance order is 300,450,50,150; yaml order is 50,150,300,450;
  # a lexicographic string sort would give 150,300,450,50. All three differ,
  # so matching "50;150;300;450" is only possible via yaml order.
  # RACE: appearance order is 100,3,20; yaml order is 3,20,100; a
  # lexicographic sort would give 100,20,3. Same three-way separation.
  dat <- data.frame(
    ID   = c(1, 1, 2, 3),
    TIME = c(0, 24, 0, 0),
    DOSE = c(300, 450, 50, 150),
    RACE = c(100, 100, 3, 20),
    stringsAsFactors = FALSE
  )
  yaml_data <- list(
    DOSE = list(values = c(50, 150, 300, 450)),
    RACE = list(values = c(3, 20, 100))
  )

  dose_out <- suppressWarnings(build_covariate_reference_table(
    data = dat, id = "ID", time = "TIME",
    Parameter = "CL", Covariate = "DOSE", Category = "cat", Formula = "power",
    yaml_data = yaml_data
  ))
  expect_equal(dose_out$LEVELS, "50;150;300;450")
  expect_equal(dose_out$TIME_DEPENDENT, "Yes")  # subject 1: 300 then 450

  race_out <- suppressWarnings(build_covariate_reference_table(
    data = dat, id = "ID", time = "TIME",
    Parameter = "CL", Covariate = "RACE", Category = "cat", Formula = "linear",
    yaml_data = yaml_data
  ))
  expect_equal(race_out$LEVELS, "3;20;100")
  expect_equal(race_out$TIME_DEPENDENT, "No")  # subject 1's RACE never changes
})


test_that("a time-dependent covariate warns REFERENCE reflects baseline only", {
  dat <- ref_table_data()
  yaml_data <- ref_table_yaml()

  # REFERENCE (50) confirms the interpolated value, not just the wording.
  expect_warning(
    build_covariate_reference_table(
      data = dat, id = "ID", time = "TIME",
      Parameter = "CL", Covariate = "DOSE", Category = "cat", Formula = "power",
      yaml_data = yaml_data
    ),
    "DOSE.*time-dependent.*REFERENCE \\(50\\).*baseline"
  )

  # Baseline CRCL is 91:95 across the 5 subjects; median REFERENCE is 93.
  expect_warning(
    build_covariate_reference_table(
      data = dat, id = "ID", time = "TIME",
      Parameter = "CL", Covariate = "CRCL", Category = "con", Formula = "linear",
      yaml_data = yaml_data
    ),
    "CRCL.*time-dependent.*REFERENCE \\(93\\).*baseline"
  )
})


test_that("a time-independent covariate does not warn about baseline REFERENCE", {
  dat <- ref_table_data()
  yaml_data <- ref_table_yaml()

  expect_warning(
    build_covariate_reference_table(
      data = dat, id = "ID", time = "TIME",
      Parameter = "CL", Covariate = "SEX", Category = "cat", Formula = "linear",
      yaml_data = yaml_data
    ),
    NA
  )

  expect_warning(
    build_covariate_reference_table(
      data = dat, id = "ID", time = "TIME",
      Parameter = "CL", Covariate = "WT", Category = "con", Formula = "linear",
      yaml_data = yaml_data
    ),
    NA
  )
})


test_that("LEVELS/REFERENCE/TIME_DEPENDENT stay matched to their own row", {
  # Guards against a mis-keyed lookup across covariates: DOSE, SEX, and WT
  # each need their own row's values, not another row's.
  out <- suppressWarnings(build_covariate_reference_table(
    data = ref_table_data(), id = "ID", time = "TIME",
    Parameter = c("CL", "CL", "V"),
    Covariate = c("DOSE", "SEX", "WT"),
    Category  = c("cat", "cat", "con"),
    Formula   = c("power", "linear", "linear"),
    yaml_data = ref_table_yaml()
  ))

  expect_equal(out$COVARIATE, c("DOSE", "SEX", "WT"))
  expect_equal(out$LEVELS, c("50;150;300;450", "0;1", NA_character_))
  expect_equal(out$REFERENCE, c("50", "0", "72"))
  expect_equal(out$TIME_DEPENDENT, c("Yes", "No", "No"))

  # Only the time-dependent row (DOSE) should trigger the baseline warning.
  expect_warning(
    build_covariate_reference_table(
      data = ref_table_data(), id = "ID", time = "TIME",
      Parameter = c("CL", "CL", "V"),
      Covariate = c("DOSE", "SEX", "WT"),
      Category  = c("cat", "cat", "con"),
      Formula   = c("power", "linear", "linear"),
      yaml_data = ref_table_yaml()
    ),
    "^Covariate 'DOSE' is time-dependent"
  )
})


test_that("a data level absent from yaml_data stops with an actionable message", {
  dat <- ref_table_data()
  yaml_data <- ref_table_yaml()
  yaml_data$DOSE$values <- c(50, 75)  # 150/300/450 now undeclared

  expect_error(
    build_covariate_reference_table(
      data = dat, id = "ID", time = "TIME",
      Parameter = "CL", Covariate = "DOSE", Category = "cat", Formula = "power",
      yaml_data = yaml_data
    ),
    "level\\(s\\) found in data not defined in yaml_data"
  )
})


test_that("a yaml_data level never observed in data warns rather than stops", {
  dat <- ref_table_data()
  yaml_data <- ref_table_yaml()
  yaml_data$SEX$values <- c(0, 1, 2)  # 2 is never observed

  expect_warning(
    build_covariate_reference_table(
      data = dat, id = "ID", time = "TIME",
      Parameter = "CL", Covariate = "SEX", Category = "cat", Formula = "linear",
      yaml_data = yaml_data
    ),
    "yaml_data defines level\\(s\\) never observed in data"
  )
})


test_that("continuous covariates keep LEVELS as NA and REFERENCE from baseline", {
  out <- build_covariate_reference_table(
    data = ref_table_data(), id = "ID", time = "TIME",
    Parameter = "CL", Covariate = "WT", Category = "con", Formula = "linear",
    yaml_data = ref_table_yaml()
  )

  expect_true(is.na(out$LEVELS))
  # Baseline WT is 70:74 across the 5 subjects; median = 72.
  expect_equal(out$REFERENCE, "72")
  expect_equal(out$TIME_DEPENDENT, "No")
})
