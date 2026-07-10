test_that("registry returns the canonical validation taxonomy", {
  reg <- registry()

  expect_s3_class(reg, "redcapmissing_registry")
  expect_s3_class(reg, "tbl_df")
  expect_equal(nrow(reg), 4)
  expect_identical(
    reg$validation_check,
    c(
      "event-row-started",
      "instance-row-started",
      "form-started",
      "field-complete"
    )
  )
  expect_identical(
    unique(reg$validation_level),
    "event:form / event:form:instance"
  )
  level_counts <- table(reg$validation_level)
  expect_equal(
    as.integer(level_counts["event:form / event:form:instance"]),
    4L
  )
  expect_equal(sum(reg$validation_check == "form-started"), 1)
  expect_equal(sum(reg$validation_check == "field-complete"), 1)
  expect_false("validation_check_type" %in% names(reg))
  expect_identical(
    reg$component_stem,
    c(
      "event_row_started",
      "instance_row_started",
      "form_started",
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
  expect_true(all(reg$gates_downstream))

  descriptions <- stats::setNames(reg$description, reg$validation_check)
  expect_identical(descriptions[["field-complete"]], "field complete")
})

test_that("registry prints an organized cli summary", {
  printed <- cli::ansi_strip(paste(
    capture.output(print(registry())),
    collapse = "\n"
  ))

  expect_true(grepl("validation registry", printed, fixed = TRUE))
  expect_true(grepl("4 checks; 1 levels", printed, fixed = TRUE))
  expect_true(grepl("level", printed, fixed = TRUE))
  expect_true(grepl("check", printed, fixed = TRUE))
  expect_true(grepl("meaning", printed, fixed = TRUE))
  expect_true(grepl("event:form / event:form:instance", printed, fixed = TRUE))
  expect_true(grepl("event-row-started", printed, fixed = TRUE))
  expect_true(grepl("event row exists", printed, fixed = TRUE))
  expect_true(grepl("field complete", printed, fixed = TRUE))
  expect_false(grepl("form-complete", printed, fixed = TRUE))
  expect_false(grepl("event-complete", printed, fixed = TRUE))
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
