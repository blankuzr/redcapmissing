test_that("flex_html rejects invalid inputs", {
  expect_error(
    flex_html(list()),
    "`x` must be a `flextable` object"
  )
})

test_that("flex_html renders formatted summary HTML when optional packages are available", {
  testthat::skip_if_not_installed("flextable")
  testthat::skip_if_not_installed("glue")
  testthat::skip_if_not_installed("htmltools")

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

  html_out <- flex_html(flex(report))

  expect_type(html_out, "character")
  expect_true(nchar(html_out) > 0)
})
