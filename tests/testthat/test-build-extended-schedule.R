test_that("build_extended_schedule validates arguments and returns its exact schema", {
  rcon <- .schedule_helper_connection(longitudinal = FALSE)$rcon

  expect_error(
    build_extended_schedule(),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    build_extended_schedule(NULL, "baseline"),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    build_extended_schedule(
      structure(list(), class = "redcapConnection"), "baseline"
    ),
    class = "redcapmissing_error_project"
  )
  expect_error(
    build_extended_schedule(rcon),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    build_extended_schedule(rcon, NULL),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    build_extended_schedule(rcon, character()),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    build_extended_schedule(rcon, c("baseline", "baseline")),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    build_extended_schedule(rcon, factor("baseline")),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    build_extended_schedule(rcon, " baseline"),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    build_extended_schedule(rcon, "unknown"),
    class = "redcapmissing_error_schedule"
  )

  invalid_counts <- list(
    NULL, numeric(), TRUE, "2", c(1, 2), NA_real_, NaN, Inf,
    0, -1, 1.5, .Machine$integer.max + 1, as.Date("2026-01-01"),
    as.difftime(1, units = "days")
  )
  for (count in invalid_counts) {
    expect_error(
      build_extended_schedule(rcon, "baseline", count),
      class = "redcapmissing_error_argument"
    )
  }

  expect_identical(
    build_extended_schedule(rcon, "baseline", 1),
    tibble::tibble(
      instrument = "baseline",
      redcap_event_name = NA_character_,
      repeat_instance = NA_integer_
    )
  )
})

test_that("build_extended_schedule tolerates padded optional instrument labels", {
  fixture <- .schedule_helper_connection()
  instruments <- fixture$rcon$instruments()
  instruments$instrument_label <- c(
    " Baseline ", " Partial ", " Diary ",
    " Event  form ", "   ", NA_character_
  )
  fixture$rcon$instruments <- function() instruments

  expect_no_warning(
    schedule <- build_extended_schedule(fixture$rcon, "baseline")
  )
  expect_identical(
    schedule,
    tibble::tibble(
      instrument = rep("baseline", 2L),
      redcap_event_name = c("baseline_arm_1", "baseline_arm_2"),
      repeat_instance = rep(NA_integer_, 2L)
    )
  )
})

test_that("classic schedules preserve native eventless crossings", {
  rcon <- .schedule_helper_connection(longitudinal = FALSE)$rcon

  expect_no_warning(
    schedule <- build_extended_schedule(
      rcon,
      instruments = c("diary", "baseline", "inactive"),
      n_repeat_instances = 3L
    )
  )
  expect_identical(
    schedule,
    tibble::tibble(
      instrument = c(rep("diary", 3L), "baseline", "inactive"),
      redcap_event_name = rep(NA_character_, 5L),
      repeat_instance = c(1:3, NA_integer_, NA_integer_)
    )
  )
})

test_that("longitudinal schedules expand every allowable crossing in native order", {
  rcon <- .schedule_helper_connection()$rcon

  expect_no_warning(
    schedule <- build_extended_schedule(
      rcon,
      instruments = c("baseline", "diary", "event_form", "partial"),
      n_repeat_instances = 2L
    )
  )
  expect_identical(
    schedule,
    tibble::tibble(
      instrument = c(
        "baseline", "baseline", "diary", "diary", "diary", "diary",
        "event_form", "event_form", "partial"
      ),
      redcap_event_name = c(
        "baseline_arm_1", "baseline_arm_2",
        "diary_arm_1", "diary_arm_1",
        "repeat_visit_arm_1", "repeat_visit_arm_1",
        "repeat_visit_arm_1", "repeat_visit_arm_1", "followup_arm_1"
      ),
      repeat_instance = c(
        NA_integer_, NA_integer_, 1L, 2L, 1L, 2L, 1L, 2L, NA_integer_
      )
    )
  )
  expect_false(anyNA(schedule$redcap_event_name))
})

test_that("undesignated longitudinal extension requests warn once and omit rows", {
  rcon <- .schedule_helper_connection()$rcon
  warnings <- list()
  mixed <- withCallingHandlers(
    build_extended_schedule(
      rcon,
      instruments = c("retired", "partial", "inactive"),
      n_repeat_instances = 2L
    ),
    warning = function(warning) {
      warnings[[length(warnings) + 1L]] <<- warning
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1L)
  expect_s3_class(
    warnings[[1L]],
    "redcapmissing_warning_undesignated_extension"
  )
  expect_s3_class(warnings[[1L]], "redcapmissing_warning")
  expect_identical(warnings[[1L]]$instruments, c("retired", "inactive"))
  expect_identical(
    conditionMessage(warnings[[1L]]),
    paste0(
      "`build_extended_schedule()` could not add rows for requested ",
      "longitudinal instrument(s) designated to no REDCap event: ",
      "retired, inactive."
    )
  )
  expect_identical(
    mixed,
    tibble::tibble(
      instrument = "partial",
      redcap_event_name = "followup_arm_1",
      repeat_instance = NA_integer_
    )
  )

  empty_warnings <- list()
  empty <- withCallingHandlers(
    build_extended_schedule(rcon, c("inactive", "retired"), 4L),
    warning = function(warning) {
      empty_warnings[[length(empty_warnings) + 1L]] <<- warning
      invokeRestart("muffleWarning")
    }
  )
  expect_length(empty_warnings, 1L)
  expect_identical(empty_warnings[[1L]]$instruments, c("inactive", "retired"))
  expect_identical(
    empty,
    tibble::tibble(
      instrument = character(),
      redcap_event_name = character(),
      repeat_instance = integer()
    )
  )
})

test_that("built extended schedules compose with plan_from_data", {
  fixture <- .schedule_helper_connection()
  rcon <- fixture$rcon
  data <- .schedule_helper_longitudinal_data()
  extension <- build_extended_schedule(rcon, "partial")

  expect_identical(
    names(extension),
    c("instrument", "redcap_event_name", "repeat_instance")
  )
  expect_no_warning(
    plan <- plan_from_data(
      data,
      rcon,
      instruments = all_instruments(rcon),
      extended_schedule = extension
    )
  )
  expect_identical(
    plan$assessible_targets[
      , c("record_id", "instrument", "redcap_event_name", "target_source")
    ],
    tibble::tibble(
      record_id = c("r1", "r2", "r1"),
      instrument = c("baseline", "baseline", "partial"),
      redcap_event_name = c(
        "baseline_arm_1", "baseline_arm_2", "followup_arm_1"
      ),
      target_source = c("observed", "observed", "extended")
    )
  )
  expect_false(any(plan$assessible_targets$instrument %in% c("inactive", "retired")))

  expect_error(
    plan_from_data(data, rcon, "baseline", extension),
    class = "redcapmissing_error_schedule"
  )

  expect_no_warning(
    inactive_plan <- plan_from_data(data, rcon, "inactive")
  )
  expect_identical(nrow(inactive_plan$assessible_targets), 0L)

  invalid_crossing <- tibble::tibble(
    instrument = "inactive",
    redcap_event_name = "followup_arm_1",
    repeat_instance = NA_integer_
  )
  expect_error(
    plan_from_data(data, rcon, "inactive", invalid_crossing),
    class = "redcapmissing_error_schedule"
  )
  invalid_eventless <- invalid_crossing
  invalid_eventless$redcap_event_name <- NA_character_
  expect_error(
    plan_from_data(data, rcon, "inactive", invalid_eventless),
    class = "redcapmissing_error_schedule"
  )
})
