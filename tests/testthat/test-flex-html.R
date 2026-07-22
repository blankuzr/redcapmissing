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
    forms = "baseline_form"
  )

  html_out <- flex_html(flexify(get_summary(report)))

  expect_type(html_out, "character")
  expect_true(nchar(html_out) > 0)
})

test_that("flex_html renders multi-form formatted missing rows", {
  testthat::skip_if_not_installed("flextable")
  testthat::skip_if_not_installed("glue")
  testthat::skip_if_not_installed("htmltools")

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
  html_out <- flex_html(flexify(get_missing(report)))

  expect_type(html_out, "character")
  expect_true(grepl("alpha_form", html_out, fixed = TRUE))
  expect_true(grepl("beta_form", html_out, fixed = TRUE))
})
