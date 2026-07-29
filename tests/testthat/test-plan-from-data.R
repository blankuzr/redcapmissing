test_that("classic repeating instruments produce exact observed and extended instances", {
  rcon <- .plan_fake_rcon(
    repeats = tibble::tibble(
      event_name = NA_character_,
      form_name = "diary"
    )
  )
  data <- tibble::tibble(
    record_id = c("r1", "r1", "r1"),
    redcap_repeat_instrument = c(NA, "diary", "diary"),
    redcap_repeat_instance = c(NA, "1", "2"),
    age = "",
    diary_value = ""
  )
  extension <- tibble::tibble(
    instrument = c("diary", "demographics"),
    redcap_event_name = c(NA_character_, NA_character_),
    repeat_instance = c(3L, NA_integer_)
  )
  plan <- plan_from_data(data, rcon, c("diary", "demographics"), extension)

  diary <- plan$assessible_targets[plan$assessible_targets$instrument == "diary", ]
  expect_identical(diary$repeat_instance, 1:3)
  expect_identical(diary$repeat_instrument, rep("diary", 3))
  expect_identical(diary$target_source, c("observed", "observed", "extended"))
  demo <- plan$assessible_targets[plan$assessible_targets$instrument == "demographics", ]
  expect_identical(demo$target_source, "observed+extended")
})

test_that("plan_from_data unions observed and applicable arm extensions", {
  rcon <- .plan_longitudinal_rcon()
  data <- .plan_longitudinal_data()
  extension <- tibble::tibble(
    instrument = "diary",
    redcap_event_name = "visit_arm_1",
    repeat_instance = 3L
  )
  plan <- plan_from_data(data, rcon, c("diary", "demographics", "notes"), extension)

  diary <- plan$assessible_targets[plan$assessible_targets$instrument == "diary", ]
  expect_equal(
    diary[, c("record_id", "redcap_event_name", "repeat_instance", "target_source")],
    tibble::tibble(
      record_id = c("r1", "r1", "r1"),
      redcap_event_name = c("baseline_arm_1", "visit_arm_1", "visit_arm_1"),
      repeat_instance = c(NA_integer_, 2L, 3L),
      target_source = c("observed", "observed", "extended")
    )
  )
  expect_false(any(plan$assessible_targets$record_id == "r2" &
    plan$assessible_targets$redcap_event_name == "visit_arm_1"))
})

test_that("extensions into an arm with no observed records warn and add no targets", {
  rcon <- .plan_longitudinal_rcon()
  original_events <- rcon$events()
  original_mapping <- rcon$mapping()
  rcon$events <- function() {
    dplyr::bind_rows(
      original_events,
      tibble::tibble(
        event_id = 202L,
        unique_event_name = "visit_arm_2",
        event_name = "Visit",
        arm_num = 2L
      )
    )
  }
  rcon$mapping <- function() {
    dplyr::bind_rows(
      original_mapping,
      tibble::tibble(
        arm_num = 2L,
        unique_event_name = "visit_arm_2",
        form = "demographics"
      )
    )
  }
  data <- .plan_longitudinal_data()
  data <- data[data$record_id == "r1", ]
  extension <- tibble::tibble(
    instrument = rep("demographics", 2L),
    redcap_event_name = c("baseline_arm_2", "visit_arm_2"),
    repeat_instance = rep(NA_integer_, 2L)
  )
  warnings <- list()
  plan <- withCallingHandlers(
    plan_from_data(data, rcon, "demographics", extension),
    warning = function(warning) {
      warnings[[length(warnings) + 1L]] <<- warning
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1L)
  expect_s3_class(warnings[[1L]], "redcapmissing_warning_empty_arm_extension")
  expect_s3_class(warnings[[1L]], "redcapmissing_warning")
  expect_identical(
    warnings[[1L]]$events,
    c("baseline_arm_2", "visit_arm_2")
  )
  expect_false(any(plan$assessible_targets$redcap_event_name %in%
    c("baseline_arm_2", "visit_arm_2")))
})

test_that("unknown identifiers and non-designated crossings fail before intersection", {
  rcon <- .plan_longitudinal_rcon()
  data <- .plan_longitudinal_data()
  schedules <- list(
    tibble::tibble(
      instrument = "diary", redcap_event_name = "unknown_arm_1",
      repeat_instance = 1L
    ),
    tibble::tibble(
      instrument = "demographics", redcap_event_name = "visit_arm_1",
      repeat_instance = NA_integer_
    ),
    tibble::tibble(
      instrument = "unknown", redcap_event_name = "visit_arm_1",
      repeat_instance = NA_integer_
    )
  )
  for (schedule in schedules) {
    expect_error(
      plan_from_data(data, rcon, c("diary", "demographics"), schedule),
      class = "redcapmissing_error_schedule"
    )
  }
})
