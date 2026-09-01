test_that("plan_explicit freezes exact targets including records absent from data", {
  rcon <- .plan_longitudinal_rcon()
  data <- .plan_longitudinal_data()
  schedule <- tibble::tibble(
    record_id = c("r1", "not_exported"),
    instrument = c("diary", "demographics"),
    redcap_event_name = c("visit_arm_1", "baseline_arm_2"),
    repeat_instance = c(4L, NA_integer_)
  )
  plan <- plan_explicit(data, rcon, schedule)

  expect_identical(plan$construction, "explicit")
  expect_identical(plan$instruments, c("diary", "demographics"))

  edited <- plan
  edited$instruments <- rev(plan$instruments)
  expect_error(
    redcapmissing:::.plan_validate_object(edited),
    class = "redcapmissing_error_plan"
  )
  expect_identical(plan$assessible_targets$record_id, c("r1", "not_exported"))
  expect_identical(plan$assessible_targets$target_source, c("explicit", "explicit"))
  expect_error(
    plan_explicit(
      data,
      rcon,
      tibble::tibble(
        record_id = "r1",
        instrument = "demographics",
        redcap_event_name = "baseline_arm_2",
        repeat_instance = NA_integer_
      )
    ),
    class = "redcapmissing_error_schedule"
  )
})

test_that("a typed empty explicit schedule assesses nothing", {
  rcon <- .plan_fake_rcon()
  data <- tibble::tibble(record_id = character())
  schedule <- tibble::tibble(
    record_id = character(),
    instrument = character(),
    redcap_event_name = character(),
    repeat_instance = integer()
  )
  plan <- plan_explicit(data, rcon, schedule)
  expect_s3_class(plan, "redcapmissing_plan")
  expect_identical(plan$instruments, character())
  expect_identical(plan$assessible_targets, redcapmissing:::.assessible_target_build_prototype())
  expect_error(
    plan_explicit(data, rcon, NULL),
    class = "redcapmissing_error_argument"
  )
  wrong_storage <- tibble::tibble(
    record_id = character(),
    instrument = character(),
    redcap_event_name = logical(),
    repeat_instance = integer()
  )
  expect_error(
    plan_explicit(data, rcon, wrong_storage),
    class = "redcapmissing_error_schema"
  )
})

test_that("explicit schedule rows are the complete derived instrument scope", {
  rcon <- .plan_fake_rcon()
  data <- tibble::tibble(record_id = "r1")
  schedule <- tibble::tibble(
    record_id = "r1",
    instrument = "demographics",
    redcap_event_name = NA_character_,
    repeat_instance = NA_integer_
  )
  plan <- plan_explicit(data, rcon, schedule)
  expect_identical(plan$instruments, "demographics")
  expect_identical(plan$assessible_targets$instrument, "demographics")

  empty <- schedule[0, ]
  empty_plan <- plan_explicit(data, rcon, empty)
  expect_identical(empty_plan$instruments, character())
  expect_identical(empty_plan$assessible_targets, redcapmissing:::.assessible_target_build_prototype())
})
