test_that("registry returns the canonical validation taxonomy", {
  reg <- registry()

  expect_s3_class(reg, "redcapmissing_registry")
  expect_s3_class(reg, "tbl_df")
  expect_equal(nrow(reg), 6)
  expect_identical(
    reg$validation_check,
    c(
      "event-row-started",
      "instance-row-started",
      "form-started",
      "form-complete",
      "field-complete",
      "event-complete"
    )
  )
  expect_identical(
    unique(reg$validation_level),
    c("event:form / event:form:instance", "event")
  )
  level_counts <- table(reg$validation_level)
  expect_equal(
    as.integer(level_counts[c("event:form / event:form:instance", "event")]),
    c(5L, 1L)
  )
  expect_equal(sum(reg$validation_check == "form-started"), 1)
  expect_equal(sum(reg$validation_check == "form-complete"), 1)
  expect_equal(sum(reg$validation_check == "field-complete"), 1)
  expect_identical(
    reg$validation_check_type,
    c("on-route", "on-route", "on-route", "detour", "on-route", "detour")
  )
  expect_identical(
    reg$component_stem,
    c(
      "event_row_started",
      "instance_row_started",
      "form_started",
      "form_complete",
      "field_complete",
      "event_complete"
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

  descriptions <- stats::setNames(reg$description, reg$validation_check)
  expect_identical(descriptions[["form-complete"]], "all form fields complete")
  expect_identical(descriptions[["field-complete"]], "field complete")
  expect_identical(descriptions[["event-complete"]], "all forms on event complete")
})

test_that("registry prints an organized cli summary", {
  printed <- cli::ansi_strip(paste(
    capture.output(print(registry())),
    collapse = "\n"
  ))

  expect_true(grepl("validation registry", printed, fixed = TRUE))
  expect_true(grepl("6 checks; 2 levels", printed, fixed = TRUE))
  expect_true(grepl("level", printed, fixed = TRUE))
  expect_true(grepl("check", printed, fixed = TRUE))
  expect_true(grepl("meaning", printed, fixed = TRUE))
  expect_true(grepl("event:form / event:form:instance", printed, fixed = TRUE))
  expect_true(grepl("event", printed, fixed = TRUE))
  expect_true(grepl("event-row-started", printed, fixed = TRUE))
  expect_true(grepl("form-complete", printed, fixed = TRUE))
  expect_true(grepl("event-complete", printed, fixed = TRUE))
  expect_true(grepl("event row exists", printed, fixed = TRUE))
  expect_true(grepl("all form fields complete", printed, fixed = TRUE))
  expect_true(grepl("field complete", printed, fixed = TRUE))
  expect_true(grepl("all forms on event complete", printed, fixed = TRUE))
  expect_false(grepl("Expected event row exists.", printed, fixed = TRUE))
  expect_false(grepl("All expected fields complete.", printed, fixed = TRUE))
  expect_false(grepl("| fields complete", printed, fixed = TRUE))
  expect_false(grepl("field complete after branching", printed, fixed = TRUE))
  expect_false(grepl("on-route checks pass in event", printed, fixed = TRUE))
  expect_false(grepl("component", printed, fixed = TRUE))
  expect_false(grepl("event_complete", printed, fixed = TRUE))
  expect_false(grepl("gate", printed, fixed = TRUE))
  expect_equal(
    length(gregexpr("form-started", printed, fixed = TRUE)[[1]]),
    1
  )
  expect_false(grepl("The expected REDCap", printed, fixed = TRUE))
})
