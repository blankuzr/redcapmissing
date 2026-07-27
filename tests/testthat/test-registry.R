test_that("registry returns validation check metadata", {
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
    names(reg),
    c(
      "validation_order", "validation_level", "validation_check",
      "flex_label", "description"
    )
  )
  expect_identical(reg$validation_order, rep(1:4, each = 2L))
  expect_identical(
    unname(vapply(reg, typeof, character(1))),
    c("integer", rep("character", 4L))
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

  expected_descriptions <- c(
    paste("At least one physical row exists in the export for the target",
          "record and event."),
    paste("The exact physical row exists in the export for the target record,",
          "event, repeat instrument, and repeat instance."),
    paste("The exact target row exists, and at least one ordinary detection",
          "field has a present response or one checkbox detection field has",
          "a selected child."),
    paste("Every field selected by field policy and branching logic is",
          "complete: each ordinary field has a present response or eligible",
          "verification override, and each checkbox has a selected child or",
          "eligible verification override.")
  )
  expect_identical(reg$description, rep(expected_descriptions, each = 2L))
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

test_that("registry print returns its input and shows complete conditions", {
  reg <- registry()
  captured <- local({
    old_options <- options(width = 80L)
    on.exit(options(old_options), add = TRUE)
    visibility <- NULL
    output <- capture.output(visibility <- withVisible(print(reg)))
    list(output = output, visibility = visibility)
  })
  output <- captured$output
  visibility <- captured$visibility
  printed <- cli::ansi_strip(paste(output, collapse = "\n"))
  normalized <- gsub("|", " ", printed, fixed = TRUE)
  normalized <- trimws(gsub("[[:space:]]+", " ", normalized))

  expect_false(visibility$visible)
  expect_identical(visibility$value, reg)
  expect_s3_class(visibility$value, "redcapmissing_registry")
  expect_match(printed, "validation registry", fixed = TRUE)
  expect_match(printed, "condition", fixed = TRUE)
  expect_false(grepl("meaning", printed, fixed = TRUE))
  expect_match(printed, "repeat-instance-row-started", fixed = TRUE)
  expect_match(printed, "instrument-started", fixed = TRUE)
  for (description in unique(reg$description)) {
    expect_match(normalized, description, fixed = TRUE)
  }
  expect_true(any(grepl("^\\|\\s+\\|\\s+\\|", cli::ansi_strip(output))))
  expect_false(grepl("~", printed, fixed = TRUE))
  expect_false(grepl("form-started", printed, fixed = TRUE))
  expect_false(grepl("event:form", printed, fixed = TRUE))
})

test_that("registry print takes conditions from description", {
  reg <- registry()
  sentinel <- "Sentinel condition comes directly from description."
  reg$description[[1L]] <- sentinel

  output <- local({
    old_options <- options(width = 80L)
    on.exit(options(old_options), add = TRUE)
    capture.output(print(reg))
  })
  printed <- cli::ansi_strip(paste(output, collapse = "\n"))
  normalized <- gsub("|", " ", printed, fixed = TRUE)
  normalized <- trimws(gsub("[[:space:]]+", " ", normalized))

  expect_match(normalized, sentinel, fixed = TRUE)
  expect_false(exists(
    ".redcapmissing_registry_print_meaning",
    envir = asNamespace("redcapmissing"),
    inherits = FALSE
  ))
})
