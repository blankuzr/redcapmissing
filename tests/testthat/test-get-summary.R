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
    reason = c(NA_character_, "no assessible fields after field policy", NA_character_, NA_character_),
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

  expect_identical(
    as.list(formals(get_summary)),
    alist(report = , validation_check = NULL, events = NULL, instruments = NULL)
  )
  expect_identical(names(result), .redcapmissing_get_summary_columns())
  expect_identical(
    unname(vapply(result, typeof, character(1))),
    c(rep("character", 3), "integer", rep("character", 4), rep("integer", 3), rep("double", 2))
  )
  expect_identical(result$reason[[2]], "no assessible fields after field policy")
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
