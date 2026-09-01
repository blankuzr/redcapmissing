summary_report_fixture <- function() {
  summary <- tibble::tibble(
    redcap_event_name = c("baseline_event", "followup_event", "baseline_event", NA_character_),
    instrument = c("alpha", "alpha", "beta", "alpha"),
    repeat_instrument = c(NA_character_, NA_character_, "beta", NA_character_),
    repeat_instance = c(NA_integer_, NA_integer_, 2L, NA_integer_),
    validation_level = c(
      "event:instrument", "event:instrument", "event:instrument:instance",
      "event:instrument"
    ),
    validation_check = c(
      "event-row-started", "field-complete", "repeat-instance-row-started",
      "field-complete"
    ),
    status = c("assessed", "not applicable", "assessed", "assessed"),
    reason = c(NA_character_, "no fields remain after field policy", NA_character_, NA_character_),
    assessed = c(2L, 0L, 2L, 1L),
    passed = c(2L, 0L, 1L, 1L),
    failed = c(0L, 0L, 1L, 0L),
    pass_rate = c(1, NA_real_, 0.5, 1),
    fail_rate = c(0, NA_real_, 0.5, 0)
  )
  targets <- tibble::tibble(
    record_id = c("r1", "r2", "r1"),
    instrument = c("alpha", "alpha", "beta"),
    redcap_event_name = c("baseline_event", "followup_event", "baseline_event"),
    repeat_instrument = c(NA_character_, NA_character_, "beta"),
    repeat_instance = c(NA_integer_, NA_integer_, 2L),
    target_source = c("observed", "observed", "extended")
  )
  plan <- structure(
    list(
      schema_version = 1L,
      construction = "from_data",
      instruments = c("alpha", "beta", "unrepresented"),
      assessible_targets = targets,
      project = list(
        project_id = "12",
        record_id_field = "record_id",
        longitudinal = TRUE,
        event_labels = c(baseline_event = "Baseline", followup_event = "Follow up"),
        instrument_labels = c(alpha = "Alpha", beta = "Beta", unrepresented = "unrepresented")
      ),
      structure_fingerprint = strrep("0", 64L)
    ),
    class = "redcapmissing_plan"
  )
  complete_report_fixture(plan = plan, summary = summary)
}

test_that("get_summary exposes the exact typed plan and run schema", {
  report <- summary_report_fixture()
  result <- get_summary(report)

  expect_identical(names(result), .summary_list_columns())
  expect_identical(
    unname(vapply(result, typeof, character(1))),
    c(rep("character", 3), "integer", rep("character", 4), rep("integer", 3), rep("double", 2))
  )
  expect_identical(result$reason[[2]], "no fields remain after field policy")
  expect_true(is.na(result$pass_rate[[2]]))
  expect_identical(
    attr(result, "redcapmissing_labels"),
    list(
      events = c(baseline_event = "Baseline", followup_event = "Follow up"),
      instruments = c(alpha = "Alpha", beta = "Beta", unrepresented = "unrepresented")
    )
  )
})

test_that("get_summary filters by checks, events, and instruments", {
  report <- summary_report_fixture()

  result <- get_summary(
    report,
    validation_check = "field-complete",
    events = "followup_event",
    instruments = "alpha"
  )
  expect_equal(nrow(result), 1L)
  expect_identical(result$instrument, "alpha")
  expect_identical(result$redcap_event_name, "followup_event")

  expect_equal(nrow(get_summary(report, instruments = "unrepresented")), 0L)
  expect_error(get_summary(report, instruments = "unknown"), "Unknown `instruments`")
  expect_error(get_summary(report, events = "unknown"), "Unknown `events`")
  expect_error(get_summary(report, validation_check = "form-started"), "Unknown `validation_check`")
})

test_that("get_summary normalizes duplicate filters without reordering rows", {
  report <- summary_report_fixture()
  original <- report
  result <- get_summary(
    report,
    validation_check = c(
      "field-complete", "event-row-started", "field-complete"
    ),
    events = c(
      "followup_event", "baseline_event", "followup_event"
    ),
    instruments = c("alpha", "alpha")
  )

  expect_identical(
    result$validation_check,
    c("event-row-started", "field-complete")
  )
  expect_identical(
    result$redcap_event_name,
    c("baseline_event", "followup_event")
  )
  expect_identical(report, original)
  expect_error(
    get_summary(report, validation_check = "Field-complete"),
    "Unknown `validation_check`"
  )
})

test_that("get_summary empty intersections retain types and labels", {
  report <- summary_report_fixture()
  result <- get_summary(report, instruments = "unrepresented")

  expect_identical(nrow(result), 0L)
  expect_identical(names(result), .summary_list_columns())
  expect_identical(
    unname(vapply(result, typeof, character(1))),
    c(
      rep("character", 3), "integer", rep("character", 4),
      rep("integer", 3), rep("double", 2)
    )
  )
  expect_identical(
    attr(result, "redcapmissing_labels"),
    attr(get_summary(report), "redcapmissing_labels")
  )
})

test_that("get_summary rejects malformed stored summaries", {
  report <- summary_report_fixture()
  report$summary$repeat_instance <- as.character(report$summary$repeat_instance)
  expect_error(get_summary(report), "storage types")

  report <- summary_report_fixture()
  report$summary$status[[1]] <- "indeterminate"
  expect_error(get_summary(report), "unsupported values")

  report <- summary_report_fixture()
  report$summary <- report$summary[rev(names(report$summary))]
  expect_error(get_summary(report), "column names and order")
})

test_that("accessor filters enforce invalid value rules", {
  report <- summary_report_fixture()
  accessors <- list(
    get_summary = get_summary,
    get_missing = get_missing
  )
  valid_values <- list(
    validation_check = "field-complete",
    events = "baseline_event",
    instruments = "alpha"
  )
  unknown_values <- list(
    validation_check = "unknown-check",
    events = "unknown_event",
    instruments = "unknown_instrument"
  )

  call_with_filter <- function(accessor, argument, value) {
    arguments <- list(report = report)
    arguments[[argument]] <- value
    do.call(accessor, arguments)
  }

  for (accessor_name in names(accessors)) {
    accessor <- accessors[[accessor_name]]
    for (argument in names(valid_values)) {
      valid <- valid_values[[argument]]
      cases <- list(
        empty = list(
          value = character(),
          regexp = paste0("`", argument, "` must be")
        ),
        missing = list(
          value = NA_character_,
          regexp = paste0("`", argument, "` may not contain")
        ),
        mixed_missing = list(
          value = c(valid, NA_character_),
          regexp = paste0("`", argument, "` may not contain")
        ),
        blank = list(
          value = "",
          regexp = paste0("`", argument, "` may not contain")
        ),
        whitespace_only = list(
          value = " \t",
          regexp = paste0("`", argument, "` may not contain")
        ),
        padded_left = list(
          value = paste0(" ", valid),
          regexp = paste0("`", argument, "` values may not contain")
        ),
        padded_right = list(
          value = paste0(valid, " "),
          regexp = paste0("`", argument, "` values may not contain")
        ),
        unknown = list(
          value = unknown_values[[argument]],
          regexp = paste0("Unknown `", argument, "`")
        ),
        factor = list(
          value = factor(valid),
          regexp = paste0("`", argument, "` must be")
        ),
        integer = list(
          value = 1L,
          regexp = paste0("`", argument, "` must be")
        ),
        logical = list(
          value = TRUE,
          regexp = paste0("`", argument, "` must be")
        ),
        list = list(
          value = list(valid),
          regexp = paste0("`", argument, "` must be")
        )
      )

      for (case_name in names(cases)) {
        case <- cases[[case_name]]
        expect_error(
          call_with_filter(accessor, argument, case$value),
          regexp = case$regexp,
          info = paste(accessor_name, argument, case_name, sep = ": ")
        )
      }
    }
  }
})

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
      instruments = c("status", "repeat"),
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
      instruments = c(status = "Status", "repeat" = "Repeat")
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

  expect_equal(
    nrow(get_missing(
      report,
      events = "followup_event",
      instruments = "repeat"
    )),
    0L
  )
})

test_that("get_missing normalizes duplicate filters without reordering rows", {
  report <- missing_report_fixture()
  original <- report
  result <- get_missing(
    report,
    validation_check = c(
      "instrument-started", "event-row-started", "instrument-started"
    ),
    events = c(
      "baseline_event", "followup_event", "baseline_event"
    ),
    instruments = c("status", "status")
  )

  expect_identical(result$record_id, c("r3", "r2"))
  expect_identical(
    result$validation_check,
    c("event-row-started", "instrument-started")
  )
  expect_identical(report, original)
  expect_error(
    get_missing(report, instruments = "Status"),
    "Unknown `instruments`"
  )
})

test_that("get_missing empty intersections retain types and labels", {
  report <- missing_report_fixture()
  result <- get_missing(
    report,
    events = "followup_event",
    instruments = "repeat"
  )

  expect_identical(nrow(result), 0L)
  expect_identical(names(result), .missing_list_columns())
  expect_identical(
    unname(vapply(result, typeof, character(1))),
    c(rep("character", 3), "integer", rep("character", 8))
  )
  expect_identical(
    attr(result, "redcapmissing_labels"),
    attr(get_missing(report), "redcapmissing_labels")
  )
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
