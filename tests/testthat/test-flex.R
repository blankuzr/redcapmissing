test_that("flex rejects invalid inputs", {
  expect_error(
    flex(list()),
    "no applicable method"
  )
})

test_that("flex returns a formatted table when optional packages are available", {
  testthat::skip_if_not_installed("flextable")
  testthat::skip_if_not_installed("glue")

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

  flex_out <- flex(report)
  tidy_tbl <- tidy(report)

  expect_s3_class(flex_out, "flextable")
  expect_equal(flex_out$body$dataset$Assessed, tidy_tbl$assessed)
  expect_true(all(c(
    "Form",
    "Form Label",
    "Validation",
    "Event",
    "Repeat Instrument",
    "Repeat Instance"
  ) %in% names(flex_out$body$dataset)))
  expect_true(any(flex_out$body$dataset$Form == "baseline_form"))
  expect_true(any(flex_out$body$dataset$`Form Label` == "baseline_form label"))
  expect_true(any(
    flex_out$body$dataset$Validation ==
      "Event row for record exists" &
      flex_out$body$dataset$Passed == "0 (0%)" &
      flex_out$body$dataset$Failed == "0 (0%)"
  ))
})

test_that("flex no longer accepts summary objects", {
  testthat::skip_if_not_installed("flextable")
  testthat::skip_if_not_installed("glue")

  summary_tbl <- tibble::tibble(
    label = "Legacy validation",
    n = 0,
    n_passed = 0,
    n_failed = 0,
    f_passed = 0,
    f_failed = 0
  )
  class(summary_tbl) <- c("summary.redcapmissing", class(summary_tbl))

  expect_error(
    flex(summary_tbl),
    "no applicable method"
  )
})
