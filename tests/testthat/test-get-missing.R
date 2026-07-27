missing_report_fixture <- function() {
  missing <- tibble::tibble(
    record_id = c("r3", "r1", "r2", "r1"),
    redcap_event_name = c("followup_event", "baseline_event", "baseline_event", NA_character_),
    repeat_instrument = c(NA_character_, "repeat", NA_character_, NA_character_),
    repeat_instance = c(NA_integer_, 2L, NA_integer_, NA_integer_),
    validation_context = c(
      "event: followup_event", "event: baseline_event; repeat: 2",
      "event: baseline_event", "classic"
    ),
    instrument = c("status", "repeat", "status", "status"),
    validation_check = c(
      "event-row-started", "repeat-instance-row-started",
      "instrument-started", "field-complete"
    ),
    field_name = c(NA_character_, NA_character_, NA_character_, "status_value"),
    field_label = c(NA_character_, NA_character_, NA_character_, "Status value"),
    field_type = c(NA_character_, NA_character_, NA_character_, "text"),
    branching_logic = c(NA_character_, NA_character_, NA_character_, "[started] = '1'"),
    url = c(NA_character_, "https://example.test/repeat/r1", NA_character_, "https://example.test/status/r1")
  )
  targets <- tibble::tibble(
    record_id = c("r3", "r1", "r2"),
    instrument = c("status", "repeat", "status"),
    redcap_event_name = c("followup_event", "baseline_event", "baseline_event"),
    repeat_instrument = c(NA_character_, "repeat", NA_character_),
    repeat_instance = c(NA_integer_, 2L, NA_integer_),
    target_source = rep("explicit", 3L)
  )
  plan <- structure(
    list(
      schema_version = 1L,
      construction = "explicit",
      instruments = c("status", "repeat", "empty"),
      assessible_targets = targets,
      project = list(
        project_id = "12", record_id_field = "record_id", longitudinal = TRUE,
        event_labels = c(baseline_event = "Baseline", followup_event = "Follow up"),
        instrument_labels = c(empty = "empty", "repeat" = "Repeat", status = "Status")
      ),
      structure_fingerprint = strrep("0", 64L)
    ),
    class = "redcapmissing_plan"
  )
  complete_report_fixture(plan = plan, missing = missing)
}

test_that("get_missing exposes normalized typed structural values", {
  report <- missing_report_fixture()
  result <- get_missing(report)

  expect_identical(
    as.list(formals(get_missing)),
    alist(report = , validation_check = NULL, events = NULL, instruments = NULL)
  )
  expect_identical(names(result), .missing_list_columns())
  expect_identical(
    unname(vapply(result, typeof, character(1))),
    c(rep("character", 3), "integer", rep("character", 8))
  )
  expect_true(is.na(result$redcap_event_name[[4]]))
  expect_true(is.na(result$repeat_instance[[1]]))
  expect_identical(
    attr(result, "redcapmissing_labels"),
    list(
      events = c(baseline_event = "Baseline", followup_event = "Follow up"),
      instruments = c(status = "Status", "repeat" = "Repeat", empty = "empty")
    )
  )
})

test_that("get_missing filters only the completed result", {
  report <- missing_report_fixture()
  original <- report

  result <- get_missing(
    report,
    validation_check = "repeat-instance-row-started",
    events = "baseline_event",
    instruments = "repeat"
  )
  expect_equal(nrow(result), 1L)
  expect_identical(result$record_id, "r1")
  expect_identical(result$repeat_instance, 2L)
  expect_identical(report, original)

  expect_equal(nrow(get_missing(report, instruments = "empty")), 0L)
})

test_that("get_missing rejects retired filters and malformed storage", {
  report <- missing_report_fixture()
  expect_error(get_missing(report, validation_check = "form-started"), "Unknown")
  expect_error(get_missing(report, instruments = "status "), "whitespace")

  report$missing$repeat_instance <- as.character(report$missing$repeat_instance)
  expect_error(get_missing(report), "storage types")

  report <- missing_report_fixture()
  report$missing$validation_check[[1]] <- "old-check"
  expect_error(get_missing(report), "unknown validation check")
})
