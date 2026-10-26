# Control streams saved by a Windows editor in "ANSI" (Windows-1252) carry
# bytes such as 0xB5 (µ) that are not valid UTF-8. Under a UTF-8 session the
# readers used to get invalid strings on which grepl() silently returns FALSE.

# Two covariates; a comment-only $THETA line written with a cp1252 "µ".
write_cp1252_model <- function() {
  models_dir <- tempfile("models")
  dir.create(models_dir)
  lines <- c(
    "$PROBLEM cp1252 comment",
    "$INPUT ID TIME DV AMT WT AGE",
    "$DATA d.csv IGNORE=@",
    "$PK",
    "TV_CL = THETA(1) * (WT/70)**THETA(4)",
    "CL = TV_CL * EXP(ETA(1))",
    "TV_V = THETA(2) * (AGE/40)**THETA(5)",
    "V = TV_V * EXP(ETA(2))",
    "$ERROR",
    "Y = F + F*THETA(3)*EPS(1)",
    "$THETA",
    "2.5 ; CL ; L/h ; RATIO",
    "; volume in L (conc in @MU@g/L)",
    "30 ; V ; L ; RATIO",
    "0.1 ; PROP ; ; RATIO",
    "0.75 ; beta_WT_CL ; ; RATIO",
    "0.5 ; beta_AGE_V ; ; RATIO",
    "$OMEGA",
    "0.1 ; IIV_CL ; ; RATIO",
    "0.1 ; IIV_V ; ; RATIO",
    "$SIGMA",
    "1 FIX ; EPS ; ; RATIO"
  )
  bytes <- charToRaw(paste0(paste(lines, collapse = "\n"), "\n"))
  at <- grepRaw("@MU@", bytes)
  bytes <- c(bytes[seq_len(at - 1)], as.raw(0xB5), bytes[(at + 4):length(bytes)])
  writeBin(bytes, file.path(models_dir, "run1.ctl"))
  models_dir
}


test_that("the control-stream reader repairs cp1252 lines and keeps UTF-8 ones", {
  models_dir <- write_cp1252_model()
  lines <- .read_control_stream(file.path(models_dir, "run1.ctl"))

  expect_true(all(validUTF8(lines)))
  expect_true("; volume in L (conc in µg/L)" %in% lines)

  utf8_file <- tempfile(fileext = ".ctl")
  writeLines(enc2utf8(c("$THETA", "2.5 ; CL ; µg/L ; RATIO")), utf8_file, useBytes = TRUE)
  expect_identical(.read_control_stream(utf8_file)[2], "2.5 ; CL ; µg/L ; RATIO")
})


test_that("validation accepts the cp1252 comment line", {
  models_dir <- write_cp1252_model()
  res <- validate_parameter_blocks(file.path(models_dir, "run1.ctl"))
  expect_true(res$valid)
})


test_that("removing one covariate leaves the other intact despite the cp1252 comment", {
  models_dir <- write_cp1252_model()
  search_state <- list(
    models_folder = models_dir,
    model_counter = 1L,
    tags = list(cov_cl_wt = "WT_CL", cov_v_age = "AGE_V"),
    covariate_search = data.frame(
      COVARIATE = c("WT", "AGE"), PARAMETER = c("CL", "V"),
      STATUS = "con", FORMULA = "power",
      cov_to_test = c("beta_WT_CL", "beta_AGE_V"), stringsAsFactors = FALSE
    ),
    search_database = data.frame(model_name = "run1",
                                 tags = I(list(c("WT_CL", "AGE_V"))),
                                 stringsAsFactors = FALSE)
  )

  invisible(capture.output(
    res <- remove_covariate_from_model(search_state, "run1", "cov_cl_wt",
                                       save_as_new_model = FALSE, step_number = 2L)
  ))
  ctl <- .read_control_stream(file.path(models_dir, "run1.ctl"))

  expect_equal(res$status, "success")
  expect_true("TV_CL = THETA(1)" %in% trimws(ctl))
  # The AGE effect on V survives and is renumbered onto its own THETA.
  expect_true("TV_V = THETA(2) * (AGE/40)**THETA(4)" %in% trimws(ctl))
  expect_true("0.5 ; beta_AGE_V ; ; RATIO" %in% ctl)
  expect_false(any(grepl("beta_WT_CL", ctl)))
})
