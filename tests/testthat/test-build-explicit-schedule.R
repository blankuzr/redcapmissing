.explicit_builder_classic_rcon <- function(repeats = tibble::tibble()) {
  metadata <- .plan_metadata()
  metadata$field_name[[1L]] <- "participant_id"
  .plan_fake_rcon(
    metadata = metadata,
    repeats = repeats,
    record_id_field = "participant_id"
  )
}

test_that("build_explicit_schedule has a project-aware pipe-first contract", {
  rcon <- .explicit_builder_classic_rcon()
  specification <- tibble::tibble(
    arm_num = c(2L, 1L),
    unique_event_name = c(NA_character_, NA_character_),
    form = c("notes", "demographics")
  )
  data <- tibble::tibble(
    participant_id = c("002", "001", "002"),
    ignored = c("a", "b", "c")
  )

  schedule <- build_explicit_schedule(data, rcon, specification)

  expect_identical(
    schedule,
    tibble::tibble(
      record_id = c("002", "002", "001", "001"),
      instrument = rep(c("notes", "demographics"), 2L),
      redcap_event_name = rep(NA_character_, 4L),
      repeat_instance = rep(NA_integer_, 4L)
    )
  )
  expect_identical(names(schedule), c(
    "record_id", "instrument", "redcap_event_name", "repeat_instance"
  ))
  expect_type(schedule$record_id, "character")
  expect_type(schedule$instrument, "character")
  expect_type(schedule$redcap_event_name, "character")
  expect_type(schedule$repeat_instance, "integer")

  numeric_ids <- build_explicit_schedule(
    tibble::tibble(participant_id = c(7L, 8L)),
    rcon,
    specification[1L, ]
  )
  expect_identical(numeric_ids$record_id, c("7", "8"))

  duplicate_extras <- data.frame(
    participant_id = "003",
    ignored = 1L,
    ignored = 2L,
    check.names = FALSE
  )
  specification_extras <- data.frame(
    unique_event_name = NA_character_,
    form = "demographics",
    arm_num = 1L,
    arm_num = 2L,
    check.names = FALSE
  )
  expect_identical(
    build_explicit_schedule(duplicate_extras, rcon, specification_extras)$record_id,
    "003"
  )
})

test_that("build_explicit_schedule validates its data and project ID field", {
  rcon <- .explicit_builder_classic_rcon()
  specification <- tibble::tibble(
    unique_event_name = NA_character_,
    form = "demographics"
  )

  expect_error(
    build_explicit_schedule(),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    build_explicit_schedule(NULL, rcon, specification),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    build_explicit_schedule(tibble::tibble(participant_id = "1"), NULL, specification),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    build_explicit_schedule(tibble::tibble(participant_id = "1"), rcon),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    build_explicit_schedule(
      tibble::tibble(record_id = "1"),
      rcon,
      specification
    ),
    class = "redcapmissing_error_schema"
  )
  duplicate_id <- data.frame(
    participant_id = "1",
    participant_id = "2",
    check.names = FALSE
  )
  expect_error(
    build_explicit_schedule(duplicate_id, rcon, specification),
    class = "redcapmissing_error_schema"
  )
  expect_error(
    build_explicit_schedule(
      tibble::tibble(participant_id = " 1"),
      rcon,
      specification
    ),
    class = "redcapmissing_error_schema"
  )

  invalid_ids <- list(
    NA_character_, "", " ", Inf, NaN, TRUE,
    as.Date("2026-01-01"), as.POSIXct("2026-01-01", tz = "UTC")
  )
  for (record_id in invalid_ids) {
    expect_error(
      build_explicit_schedule(
        data.frame(participant_id = record_id),
        rcon,
        specification
      ),
      class = "redcapmissing_error_schema"
    )
  }
  matrix_id <- data.frame(participant_id = I(matrix("1", nrow = 1L)))
  expect_error(
    build_explicit_schedule(matrix_id, rcon, specification),
    class = "redcapmissing_error_schema"
  )
})

test_that("explicit specification aliases and optional instances normalize", {
  rcon <- .explicit_builder_classic_rcon()
  cohort <- tibble::tibble(participant_id = "001")

  canonical <- build_explicit_schedule(
    cohort,
    rcon,
    tibble::tibble(
      redcap_event_name = NA_character_,
      instrument = "demographics"
    )
  )
  mixed_one <- build_explicit_schedule(
    cohort,
    rcon,
    tibble::tibble(
      unique_event_name = NA_character_,
      instrument = "demographics"
    )
  )
  mixed_two <- build_explicit_schedule(
    cohort,
    rcon,
    tibble::tibble(
      redcap_event_name = NA_character_,
      form = "demographics"
    )
  )
  expect_identical(mixed_one, canonical)
  expect_identical(mixed_two, canonical)
  expect_identical(canonical$repeat_instance, NA_integer_)

  ambiguous_event <- tibble::tibble(
    unique_event_name = NA_character_,
    redcap_event_name = NA_character_,
    form = "demographics"
  )
  ambiguous_instrument <- tibble::tibble(
    unique_event_name = NA_character_,
    form = "demographics",
    instrument = "demographics"
  )
  missing_event <- tibble::tibble(form = "demographics")
  missing_instrument <- tibble::tibble(unique_event_name = NA_character_)
  for (bad_specification in list(
    ambiguous_event,
    ambiguous_instrument,
    missing_event,
    missing_instrument
  )) {
    expect_error(
      build_explicit_schedule(cohort, rcon, bad_specification),
      class = "redcapmissing_error_schedule"
    )
  }

  duplicate <- tibble::tibble(
    unique_event_name = c(NA_character_, ""),
    form = c("demographics", "demographics")
  )
  expect_error(
    build_explicit_schedule(cohort, rcon, duplicate),
    "`explicit_spec` contains duplicate normalized rows",
    class = "redcapmissing_error_schedule"
  )

  list_instrument <- tibble::tibble(
    unique_event_name = NA_character_,
    form = list("demographics")
  )
  matrix_event <- data.frame(
    unique_event_name = I(matrix(NA_character_, nrow = 1L)),
    form = "demographics"
  )
  for (bad_storage in list(list_instrument, matrix_event)) {
    expect_error(
      build_explicit_schedule(cohort, rcon, bad_storage),
      class = "redcapmissing_error_schema"
    )
  }
})

test_that("empty record and specification sets return the typed prototype", {
  rcon <- .explicit_builder_classic_rcon()
  empty <- tibble::tibble(
    record_id = character(),
    instrument = character(),
    redcap_event_name = character(),
    repeat_instance = integer()
  )
  specification <- tibble::tibble(
    unique_event_name = NA_character_,
    form = "demographics"
  )
  empty_specification <- tibble::tibble(
    unique_event_name = character(),
    form = character()
  )

  expect_identical(
    build_explicit_schedule(
      tibble::tibble(participant_id = character()),
      rcon,
      specification
    ),
    empty
  )
  expect_identical(
    build_explicit_schedule(
      tibble::tibble(participant_id = "1"),
      rcon,
      empty_specification
    ),
    empty
  )
})

test_that("classic repeat rules are enforced before record expansion", {
  rcon <- .explicit_builder_classic_rcon(tibble::tibble(
    event_name = NA_character_,
    form_name = "diary"
  ))
  cohort <- tibble::tibble(participant_id = "1")

  expect_error(
    build_explicit_schedule(
      cohort,
      rcon,
      tibble::tibble(unique_event_name = NA_character_, form = "diary")
    ),
    "requires a positive `repeat_instance`",
    class = "redcapmissing_error_schedule"
  )
  repeated <- build_explicit_schedule(
    cohort,
    rcon,
    tibble::tibble(
      unique_event_name = NA_character_,
      form = "diary",
      repeat_instance = 2
    )
  )
  expect_identical(repeated$repeat_instance, 2L)

  expect_error(
    build_explicit_schedule(
      cohort,
      rcon,
      tibble::tibble(
        unique_event_name = NA_character_,
        form = "demographics",
        repeat_instance = 1L
      )
    ),
    "requires a missing `repeat_instance`",
    class = "redcapmissing_error_schedule"
  )
  expect_error(
    build_explicit_schedule(
      cohort,
      rcon,
      tibble::tibble(unique_event_name = "event_arm_1", form = "demographics")
    ),
    "must be missing in a classic project",
    class = "redcapmissing_error_schedule"
  )
})

test_that("longitudinal crossings support mapping rows and both repeat modes", {
  fixture <- .schedule_helper_connection()
  rcon <- fixture$rcon
  mapping_specification <- rcon$mapping()
  mapping_specification <- mapping_specification[
    mapping_specification$form == "baseline",
    ,
    drop = FALSE
  ]
  schedule <- build_explicit_schedule(
    tibble::tibble(record_id = c("r2", "r1")),
    rcon,
    mapping_specification
  )
  expect_identical(
    schedule,
    tibble::tibble(
      record_id = c("r2", "r2", "r1", "r1"),
      instrument = rep("baseline", 4L),
      redcap_event_name = rep(
        c("baseline_arm_2", "baseline_arm_1"),
        2L
      ),
      repeat_instance = rep(NA_integer_, 4L)
    )
  )

  repeating_instrument <- tibble::tibble(
    unique_event_name = "diary_arm_1",
    form = "diary",
    repeat_instance = 3L
  )
  repeating_event <- tibble::tibble(
    redcap_event_name = "repeat_visit_arm_1",
    instrument = "event_form",
    repeat_instance = 2L
  )
  expect_identical(
    build_explicit_schedule(
      tibble::tibble(record_id = "r1"),
      rcon,
      repeating_instrument
    )$repeat_instance,
    3L
  )
  expect_identical(
    build_explicit_schedule(
      tibble::tibble(record_id = "r1"),
      rcon,
      repeating_event
    )$repeat_instance,
    2L
  )

  invalid_specifications <- list(
    tibble::tibble(unique_event_name = "diary_arm_1", form = "diary"),
    tibble::tibble(
      unique_event_name = "baseline_arm_1",
      form = "baseline",
      repeat_instance = 1L
    ),
    tibble::tibble(unique_event_name = NA_character_, form = "baseline"),
    tibble::tibble(unique_event_name = "unknown_arm_1", form = "baseline"),
    tibble::tibble(unique_event_name = "followup_arm_1", form = "baseline"),
    tibble::tibble(unique_event_name = "baseline_arm_1", form = "unknown")
  )
  for (bad_specification in invalid_specifications) {
    expect_error(
      build_explicit_schedule(
        tibble::tibble(record_id = "r1"),
        rcon,
        bad_specification
      ),
      class = "redcapmissing_error_schedule"
    )
  }
  expect_identical(fixture$calls$exportRecords, 0L)
})

test_that("independent explicit schedules compose for plan_explicit", {
  rcon <- .plan_longitudinal_rcon()
  data <- .plan_longitudinal_data()
  schedule_one <- tibble::tibble(record_id = "r1") |>
    build_explicit_schedule(
      rcon,
      tibble::tibble(
        unique_event_name = "baseline_arm_1",
        form = "demographics"
      )
    )
  schedule_two <- tibble::tibble(record_id = "r2") |>
    build_explicit_schedule(
      rcon,
      tibble::tibble(
        unique_event_name = "baseline_arm_2",
        form = "demographics"
      )
    )
  schedule_three <- tibble::tibble(record_id = "r1") |>
    build_explicit_schedule(
      rcon,
      tibble::tibble(
        unique_event_name = "visit_arm_1",
        form = "notes"
      )
    )
  combined <- dplyr::bind_rows(
    schedule_one,
    schedule_two,
    schedule_three
  )

  expect_no_warning(
    plan <- plan_explicit(
      data,
      rcon,
      combined
    )
  )
  expect_identical(plan$assessible_targets$record_id, c("r1", "r2", "r1"))
  expect_error(
    plan_explicit(
      data,
      rcon,
      dplyr::bind_rows(combined, schedule_one)
    ),
    class = "redcapmissing_error_schedule"
  )
})

test_that("explicit expansion size is checked before allocation", {
  expect_error(
    redcapmissing:::.schedule_preflight_expansion_size(
      .Machine$integer.max,
      2L
    ),
    class = "redcapmissing_error_schedule"
  )
  expect_identical(
    redcapmissing:::.schedule_preflight_expansion_size(3L, 4L),
    invisible(12L)
  )
})
