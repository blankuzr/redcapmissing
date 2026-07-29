test_that("repeating events expand every mapped instrument at the exact instance", {
  repeats <- tibble::tibble(
    event_name = "visit_arm_1",
    form_name = NA_character_
  )
  rcon <- .plan_longitudinal_rcon(repeats)
  data <- .plan_longitudinal_data()
  data <- data[-2, ]
  data$redcap_repeat_instrument[[2]] <- NA_character_
  plan <- plan_from_data(data, rcon, c("notes", "diary"))
  repeated <- plan$assessible_targets[
    plan$assessible_targets$redcap_event_name == "visit_arm_1",
  ]
  expect_identical(
    repeated,
    tibble::tibble(
      record_id = c("r1", "r1"),
      instrument = c("notes", "diary"),
      redcap_event_name = c("visit_arm_1", "visit_arm_1"),
      repeat_instrument = c(NA_character_, NA_character_),
      repeat_instance = c(2L, 2L),
      target_source = c("observed", "observed")
    )
  )
})

test_that("native target identities do not collide on delimiter values", {
  separator <- intToUtf8(31L)
  second_instrument <- paste0("b", separator, "c")
  metadata <- tibble::tibble(
    field_name = c("record_id", "c_value", "second_value"),
    form_name = c("c", "c", second_instrument),
    field_type = rep("text", 3L)
  )
  rcon <- .plan_fake_rcon(metadata = metadata)
  data <- tibble::tibble(
    record_id = c(paste0("alpha", separator, "b"), "alpha")
  )

  plan <- plan_from_data(data, rcon, c("c", second_instrument))

  expect_identical(nrow(plan$assessible_targets), 4L)
  expect_false(anyDuplicated(
    plan$assessible_targets[, c(
      "record_id", "instrument", "redcap_event_name",
      "repeat_instrument", "repeat_instance"
    )]
  ) > 0L)
})

test_that("planning materializes moderate record expansions with exact provenance", {
  rcon <- .plan_fake_rcon(
    repeats = tibble::tibble(
      event_name = NA_character_,
      form_name = "diary"
    )
  )
  record_count <- 500L
  data <- tibble::tibble(
    record_id = sprintf("r%04d", seq_len(record_count)),
    redcap_repeat_instrument = rep(NA_character_, record_count),
    redcap_repeat_instance = rep(NA_integer_, record_count)
  )
  extension <- tibble::tibble(
    instrument = c("demographics", "diary"),
    redcap_event_name = c(NA_character_, NA_character_),
    repeat_instance = c(NA_integer_, 2L)
  )

  plan <- plan_from_data(
    data,
    rcon,
    c("demographics", "diary"),
    extension
  )

  expect_identical(nrow(plan$assessible_targets), 2L * record_count)
  expect_identical(
    as.integer(table(plan$assessible_targets$target_source)),
    c(record_count, record_count)
  )
  expect_identical(
    names(table(plan$assessible_targets$target_source)),
    c("extended", "observed+extended")
  )
  diary <- plan$assessible_targets$instrument == "diary"
  expect_true(all(plan$assessible_targets$repeat_instance[diary] == 2L))
})

test_that("target dimensions normalize across classic longitudinal and repeat modes", {
  classic <- plan_from_data(
    tibble::tibble(record_id = c("2", "1")),
    .plan_fake_rcon(),
    c("notes", "demographics")
  )
  expect_identical(
    unname(vapply(classic$assessible_targets, typeof, character(1))),
    c("character", "character", "character", "character", "integer", "character")
  )
  expect_identical(classic$assessible_targets$record_id, c("1", "2", "1", "2"))
  expect_identical(classic$assessible_targets$instrument, c("notes", "notes", "demographics", "demographics"))
  expect_true(all(is.na(classic$assessible_targets$redcap_event_name)))
  expect_true(all(is.na(classic$assessible_targets$repeat_instrument)))
  expect_true(all(is.na(classic$assessible_targets$repeat_instance)))

  classic_repeat_rcon <- .plan_fake_rcon(
    repeats = tibble::tibble(event_name = NA_character_, form_name = "diary")
  )
  classic_repeat <- plan_from_data(
    tibble::tibble(
      record_id = "r1", redcap_repeat_instrument = "diary",
      redcap_repeat_instance = "2"
    ),
    classic_repeat_rcon,
    "diary"
  )
  expect_true(is.na(classic_repeat$assessible_targets$redcap_event_name[[1]]))
  expect_identical(classic_repeat$assessible_targets$repeat_instrument, "diary")
  expect_identical(classic_repeat$assessible_targets$repeat_instance, 2L)

  longitudinal <- plan_from_data(
    .plan_longitudinal_data()[c(4, 3, 1, 2), ],
    .plan_longitudinal_rcon(),
    c("diary", "demographics", "notes")
  )
  expect_equal(
    longitudinal$assessible_targets,
    tibble::tibble(
      record_id = c("r1", "r1", "r1", "r2", "r1"),
      instrument = c("diary", "diary", "demographics", "demographics", "notes"),
      redcap_event_name = c(
        "baseline_arm_1", "visit_arm_1", "baseline_arm_1",
        "baseline_arm_2", "visit_arm_1"
      ),
      repeat_instrument = c(NA_character_, "diary", NA_character_, NA_character_, NA_character_),
      repeat_instance = c(NA_integer_, 2L, NA_integer_, NA_integer_, NA_integer_),
      target_source = rep("observed", 5)
    )
  )

  repeat_event_rcon <- .plan_longitudinal_rcon(
    tibble::tibble(event_name = "visit_arm_1", form_name = NA_character_)
  )
  repeat_event <- plan_from_data(
    tibble::tibble(
      record_id = "r1", redcap_event_name = "visit_arm_1",
      redcap_repeat_instrument = NA_character_, redcap_repeat_instance = 3L
    ),
    repeat_event_rcon,
    c("diary", "notes")
  )
  expect_identical(repeat_event$assessible_targets$instrument, c("diary", "notes"))
  expect_true(all(is.na(repeat_event$assessible_targets$repeat_instrument)))
  expect_identical(repeat_event$assessible_targets$repeat_instance, c(3L, 3L))
})

test_that("selected longitudinal instruments use only designated physical contexts", {
  rcon <- .plan_longitudinal_rcon()
  data <- .plan_longitudinal_data()[c(1, 2), ]
  plan <- plan_from_data(data, rcon, "notes")
  expect_identical(
    plan$assessible_targets,
    tibble::tibble(
      record_id = "r1",
      instrument = "notes",
      redcap_event_name = "visit_arm_1",
      repeat_instrument = NA_character_,
      repeat_instance = NA_integer_,
      target_source = "observed"
    )
  )
})
