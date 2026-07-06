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
    forms = "baseline_form"
  )

  flex_out <- flex(report)
  tidy_tbl <- tidy(report)

  expect_s3_class(flex_out, "flextable")
  expect_equal(flex_out$body$dataset$Assessed, tidy_tbl$assessed)
  expect_identical(names(flex_out$body$dataset), c(
    "Form",
    "Form Label",
    "Event",
    "Repeat Instrument",
    "Repeat Instance",
    "Validation Level",
    "Validation Check",
    "Check Type",
    "Assessed",
    "Passed",
    "Failed"
  ))
  expect_true(any(flex_out$body$dataset$Form == "baseline_form"))
  expect_true(any(flex_out$body$dataset$`Form Label` == "baseline_form label"))
  expect_true(any(
    flex_out$body$dataset$Form == "" &
      flex_out$body$dataset$`Form Label` == "" &
      flex_out$body$dataset$`Validation Check` == "Event complete"
  ))
  expect_true(any(
    flex_out$body$dataset$`Validation Check` ==
      "Event row started" &
      flex_out$body$dataset$Passed == "0 (0%)" &
      flex_out$body$dataset$Failed == "0 (0%)"
  ))
  expect_true(any(flex_out$body$dataset$`Validation Level` == "event:form"))
  expect_true(any(flex_out$body$dataset$`Validation Level` == "event"))
  expect_true(any(flex_out$body$dataset$`Check Type` == "on-route"))
  expect_true(any(flex_out$body$dataset$`Check Type` == "detour"))
})

test_that("flex keeps multi-form summaries in one flat table", {
  testthat::skip_if_not_installed("flextable")
  testthat::skip_if_not_installed("glue")

  metadata <- dplyr::bind_rows(
    meta_row("record_id", "alpha_form", field_label = "Record ID", required = "y"),
    meta_row("alpha_value", "alpha_form", field_label = "Alpha value", required = "y"),
    meta_row("beta_value", "beta_form", field_label = "Beta value", required = "y")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    alpha_value = c("entered", ""),
    beta_value = c("", "entered")
  )

  report <- find_missing(
    data = records,
    rcon = fake_rcon(metadata),
    forms = c("alpha_form", "beta_form")
  )
  flex_out <- flex(report)

  expect_s3_class(flex_out, "flextable")
  expect_setequal(flex_out$body$dataset$Form, c("alpha_form", "beta_form", ""))
  expect_setequal(
    flex_out$body$dataset$`Form Label`,
    c("alpha_form label", "beta_form label", "")
  )
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
