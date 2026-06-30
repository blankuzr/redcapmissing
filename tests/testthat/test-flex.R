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
})
