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

  data <- run_plan_data(required_note = "")
  rcon <- run_plan_rcon()
  plan <- plan_from_data(data, rcon, "baseline_form")
  report <- run_plan(plan, data, rcon, progress = FALSE)

  html_out <- flex_html(flexify(get_summary(report)))

  expect_type(html_out, "character")
  expect_true(nchar(html_out) > 0)
})

test_that("flex_html renders multi-instrument formatted missing rows", {
  testthat::skip_if_not_installed("flextable")
  testthat::skip_if_not_installed("glue")
  testthat::skip_if_not_installed("htmltools")

  metadata <- dplyr::bind_rows(
    meta_row("record_id", "alpha_instrument", field_label = "Record ID", required = "y"),
    meta_row("alpha_value", "alpha_instrument", field_label = "Alpha value", required = "y"),
    meta_row("beta_value", "beta_instrument", field_label = "Beta value", required = "y")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    alpha_value = c("entered", ""),
    beta_value = c("", "entered")
  )
  rcon <- list(
    metadata = function() metadata,
    instruments = function() tibble::tibble(
      instrument_name = c("alpha_instrument", "beta_instrument"),
      instrument_label = c("Alpha instrument", "Beta instrument")
    ),
    projectInformation = function() tibble::tibble(
      project_id = 88L,
      is_longitudinal = 0L
    ),
    repeatInstrumentEvent = function() tibble::tibble()
  )

  plan <- plan_from_data(
    records,
    rcon,
    instruments = c("alpha_instrument", "beta_instrument")
  )
  report <- run_plan(
    plan,
    records,
    rcon,
    exclude_types = NULL,
    progress = FALSE
  )
  html_out <- flex_html(flexify(get_missing(report)))

  expect_type(html_out, "character")
  expect_true(grepl("Alpha instrument", html_out, fixed = TRUE))
  expect_true(grepl("Beta instrument", html_out, fixed = TRUE))
})