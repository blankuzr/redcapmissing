test_that("registry exposes the plan and run validation taxonomy", {
  reg <- registry()

  expect_s3_class(reg, "redcapmissing_registry")
  expect_s3_class(reg, "tbl_df")
  expect_identical(
    unique(reg$validation_check),
    c(
      "event-row-started",
      "repeat-instance-row-started",
      "instrument-started",
      "field-complete"
    )
  )
  expect_identical(
    unique(reg$validation_level),
    c("event:instrument", "event:instrument:instance")
  )
  expect_identical(nrow(reg), 8L)
  expect_identical(
    unique(reg$component_stem),
    c(
      "event_row_started",
      "repeat_instance_row_started",
      "instrument_started",
      "field_complete"
    )
  )
  expect_identical(
    unique(reg$flex_label),
    c(
      "Event row started",
      "Repeat instance row started",
      "Instrument started",
      "Field complete"
    )
  )
  expect_true(all(reg$gates_downstream))
  expect_identical(
    unique(reg$description[reg$validation_check == "field-complete"]),
    paste0(
      "Every applicable ordinary field has a present response or eligible ",
      "verification override, and every applicable checkbox has a selected ",
      "child or eligible verification override."
    )
  )
})

test_that("retired monolithic and form formatter APIs are not exported", {
  exports <- getNamespaceExports("redcapmissing")
  expect_false(any(c("find_missing", "flex_event_forms") %in% exports))
  expect_false(exists(
    "find_missing",
    envir = asNamespace("redcapmissing"),
    inherits = FALSE
  ))
  expect_false(exists(
    "flex_event_forms",
    envir = asNamespace("redcapmissing"),
    inherits = FALSE
  ))
})

test_that("context validation levels use instrument terminology", {
  expect_identical(
    .redcapmissing_context_validation_level(
      c("event-row-started", "repeat-instance-row-started"),
      c(NA_character_, "2")
    ),
    c("event:instrument", "event:instrument:instance")
  )
})

test_that("registry print returns its input and shows complete check codes", {
  reg <- registry()
  visibility <- NULL
  output <- capture.output(visibility <- withVisible(print(reg)))
  printed <- cli::ansi_strip(paste(output, collapse = "\n"))

  expect_false(visibility$visible)
  expect_identical(visibility$value, reg)
  expect_s3_class(visibility$value, "redcapmissing_registry")
  expect_match(printed, "validation registry", fixed = TRUE)
  expect_match(printed, "repeat-instance-row-started", fixed = TRUE)
  expect_match(printed, "instrument-started", fixed = TRUE)
  expect_match(printed, "checkbox child selected", fixed = TRUE)
  expect_false(grepl("repeat-instance-row-~", printed, fixed = TRUE))
  expect_false(grepl("form-started", printed, fixed = TRUE))
  expect_false(grepl("event:form", printed, fixed = TRUE))
})
