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

  expect_s3_class(flex_out, "flextable")
  expect_true("Context" %in% names(flex_out$body$dataset))
  expect_true(any(flex_out$body$dataset$Context == "overall"))
  expect_true(any(
    flex_out$body$dataset$Evaluation ==
      "Offered REDCap event row exists for the form" &
      flex_out$body$dataset$Passed == "0 (0%)" &
      flex_out$body$dataset$Failed == "0 (0%)"
  ))
})

test_that("flex falls back to overall context for legacy validation sets", {
  testthat::skip_if_not_installed("flextable")
  testthat::skip_if_not_installed("glue")

  report <- list(
    agent = list(
      validation_set = tibble::tibble(
        label = "Legacy validation",
        n = 0,
        n_passed = 0,
        n_failed = 0,
        f_passed = 0,
        f_failed = 0
      )
    )
  )
  class(report) <- "redcapmissing"

  flex_out <- flex(report)

  expect_s3_class(flex_out, "flextable")
  expect_equal(flex_out$body$dataset$Context, "overall")
  expect_equal(flex_out$body$dataset$Passed, "0 (0%)")
  expect_equal(flex_out$body$dataset$Failed, "0 (0%)")
})
