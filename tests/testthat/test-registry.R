test_that("registry returns the canonical validation taxonomy", {
  reg <- registry()

  expect_s3_class(reg, "redcapmissing_registry")
  expect_s3_class(reg, "tbl_df")
  expect_identical(
    reg$validation_check,
    c(
      "event-row-started",
      "instance-row-started",
      "form-started",
      "form-complete",
      "field-complete"
    )
  )
  expect_identical(reg$validation_level, c("row", "row", "form", "form", "field"))
  expect_identical(
    reg$validation_check_type,
    c("on-route", "on-route", "on-route", "detour", "on-route")
  )
  expect_identical(
    reg$component_stem,
    c(
      "event_row_started",
      "instance_row_started",
      "form_started",
      "form_complete",
      "field_complete"
    )
  )
  expect_true(all(c(
    "validation_order",
    "downstream_order",
    "validation_label",
    "flex_label",
    "description",
    "r_identifier",
    "step_suffix",
    "gates_downstream"
  ) %in% names(reg)))
  expect_true(all(reg$gates_downstream[reg$validation_check_type == "on-route"]))
  expect_false(any(reg$gates_downstream[reg$validation_check_type == "detour"]))
})

test_that("registry prints an organized cli summary", {
  printed <- cli::ansi_strip(paste(
    capture.output(print(registry()), type = "message"),
    collapse = "\n"
  ))

  expect_true(grepl("redcapmissing validation registry", printed, fixed = TRUE))
  expect_true(grepl("row-level checks", printed, fixed = TRUE))
  expect_true(grepl("form-level checks", printed, fixed = TRUE))
  expect_true(grepl("field-level checks", printed, fixed = TRUE))
  expect_true(grepl("event-row-started", printed, fixed = TRUE))
  expect_true(grepl("form-complete", printed, fixed = TRUE))
})
