# Every parameter whose ETA sits inside EXP() must reach its typical value
# through TV_<param> (owner decision on SIM-01), and only ENDIF is NM-TRAN.

pk <- function(...) c("$PK", ...)

test_that("MU-referencing must go through a TV_ line", {
  # SIM-01: MU_1 = THETA(1) + ...; CL = EXP(MU_1 + ETA(1)) was read on the log scale.
  expect_match(.typical_value_form_issues(pk("MU_1 = LOG(THETA(1))", "CL = EXP(MU_1 + ETA(1))")),
               "define MU_1 = LOG(TV_CL)", fixed = TRUE)
  expect_length(.typical_value_form_issues(pk("TV_CL = THETA(1)", "MU_1 = LOG(TV_CL)",
                                              "CL = EXP(MU_1 + ETA(1))")), 0)
})

test_that("a typical-value variable inside EXP must be TV_<param>", {
  expect_match(.typical_value_form_issues(pk("TVV = THETA(2)", "V2 = EXP(TVV + ETA(2))")),
               "name the typical value TV_V2", fixed = TRUE)
  expect_length(.typical_value_form_issues(pk("TV_V2 = THETA(2)", "V2 = EXP(TV_V2 + ETA(2))")), 0)
  expect_length(.typical_value_form_issues(pk("TVCL = THETA(1)", "CL = EXP(TVCL + ETA(1))")), 0)
})

test_that("inline THETA, IOV terms, data items and extra terms are allowed", {
  expect_length(.typical_value_form_issues(pk("KA = EXP(THETA(3) + ETA(3))")), 0)
  expect_length(.typical_value_form_issues(pk("TV_CL = THETA(1)", "IOV = ETA(4)",
                                              "CL = EXP(TV_CL + IOV + ETA(1))")), 0)
  expect_length(.typical_value_form_issues(pk("CL = EXP(THETA(1) + THETA(5)*LOG(WT/70) + ETA(1))")), 0)
  expect_length(.typical_value_form_issues(pk("TV_CL = THETA(1)", "COVEFF = THETA(5)*(WT-70)",
                                              "CL = EXP(TV_CL + COVEFF + ETA(1))")), 0)
  expect_length(.typical_value_form_issues(pk("TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1))")), 0)
})

test_that("END IF is rejected, ENDIF accepted", {
  expect_match(.endif_spelling_issues(pk("IF (SEX.EQ.1) THEN", "CL = 1", "END IF")),
               "line 4: 'END IF' is not NM-TRAN syntax", fixed = TRUE)
  expect_length(.endif_spelling_issues(pk("IF (SEX.EQ.1) THEN", "CL = 1", "ENDIF",
                                          "; END IF in a comment")), 0)
})

test_that("validate_param_transformations stops on these forms in the base model", {
  mf <- tempfile("models")
  dir.create(mf)
  writeLines(c("$PROBLEM x", "$PK", "TVV = THETA(2)", "V2 = EXP(TVV + ETA(2))",
               "TV_CL = THETA(1)", "CL = TV_CL * EXP(ETA(1))",
               "$THETA", "1 ; CL ; ; RATIO", "2 ; V2 ; ; LOG"), file.path(mf, "run1.ctl"))
  cs <- data.frame(COVARIATE = "WT", PARAMETER = "CL", stringsAsFactors = FALSE)
  d <- data.frame(ID = 1:2, WT = c(60, 80))
  expect_error(validate_param_transformations(cs, d, "ID", "run1", mf),
               "name the typical value TV_V2", fixed = TRUE)
})

test_that("shapes outside P = EXP(...) are not checked, and indirect MU forms are", {
  # diff-auditor (Phase 6): a scaled ETA and a logit form were rejected; an
  # indirect LOG term not named MU_k slipped through.
  expect_length(.typical_value_form_issues(pk("TVCL = THETA(1)", "SC = THETA(4)",
                                              "CL = TVCL * EXP(ETA(1) * SC)")), 0)
  expect_length(.typical_value_form_issues(pk("LGT = THETA(5)",
                                              "F1 = EXP(LGT + ETA(4))/(1 + EXP(LGT + ETA(4)))")), 0)
  expect_match(.typical_value_form_issues(pk("LCL = LOG(THETA(1))", "CL = EXP(LCL + ETA(1))")),
               "define LCL = LOG(TV_CL)", fixed = TRUE)
  expect_length(.typical_value_form_issues(pk("TV_CL = THETA(1)", "LCL = LOG(TV_CL)",
                                              "CL = EXP(LCL + ETA(1))")), 0)
  expect_length(.typical_value_form_issues(pk("TV_CL = THETA(1)", "MU_1 = LOG(TV_CL)",
                                              "CL = exp(MU_1 + eta(1))")), 0)
})
