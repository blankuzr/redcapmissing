test_that("summary returns the unmodified validation-set tibble", {
  records <- tibble::tibble(
    record_id = "r1",
    branch_flag = "0",
    required_note = "",
    optional_note = "",
    checkbox_field___1 = "1",
    checkbox_field___2 = "0",
    checkbox_other = "",
    conditional_note = ""
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    form = "baseline_form"
  )

  summary_tbl <- summary(report)
  validation_set <- report$agent$validation_set

  expect_s3_class(summary_tbl, "summary.redcapmissing")
  expect_s3_class(summary_tbl, "tbl_df")
  expect_identical(names(summary_tbl), names(validation_set))
  expect_identical(lapply(summary_tbl, class), lapply(validation_set, class))
  expect_identical(unclass(summary_tbl), unclass(validation_set))
})

test_that("summary print method delegates to tibble printing", {
  records <- tibble::tibble(
    record_id = "r1",
    branch_flag = "0",
    required_note = "",
    optional_note = "",
    checkbox_field___1 = "1",
    checkbox_field___2 = "0",
    checkbox_other = "",
    conditional_note = ""
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    form = "baseline_form"
  )
  summary_tbl <- summary(report)

  expect_output(
    print_return <- print(summary_tbl),
    "# A tibble"
  )
  expect_identical(print_return, summary_tbl)
})

test_that("summary rejects invalid inputs", {
  expect_error(
    redcapmissing:::summary.redcapmissing(list()),
    "`object` must be a `redcapmissing` object"
  )
})
