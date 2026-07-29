test_that("schedule schemas and allowable crossings fail closed", {
  rcon <- .plan_longitudinal_rcon()
  data <- .plan_longitudinal_data()
  wrong_order <- tibble::tibble(
    redcap_event_name = "visit_arm_1",
    instrument = "diary",
    repeat_instance = 2L
  )
  expect_error(
    plan_from_data(data, rcon, "diary", wrong_order),
    class = "redcapmissing_error_schedule"
  )
  duplicate <- tibble::tibble(
    instrument = c("diary", "diary"),
    redcap_event_name = c("visit_arm_1", "visit_arm_1"),
    repeat_instance = c("2", "2")
  )
  expect_error(
    plan_from_data(data, rcon, "diary", duplicate),
    class = "redcapmissing_error_schedule"
  )
  instance_on_crossing_without_repeat <- tibble::tibble(
    instrument = "demographics",
    redcap_event_name = "baseline_arm_1",
    repeat_instance = 1L
  )
  expect_error(
    plan_from_data(
      data,
      rcon,
      "demographics",
      instance_on_crossing_without_repeat
    ),
    class = "redcapmissing_error_schedule"
  )
  unselected <- tibble::tibble(
    instrument = "notes",
    redcap_event_name = "visit_arm_1",
    repeat_instance = NA_integer_
  )
  expect_error(
    plan_from_data(data, rcon, "diary", unselected),
    class = "redcapmissing_error_schedule"
  )
})

test_that("structural values normalize strictly without silent row loss", {
  rcon <- .plan_fake_rcon(
    repeats = tibble::tibble(event_name = NA_character_, form_name = "diary")
  )
  accepted <- tibble::tibble(
    record_id = c("001", "001"),
    redcap_event_name = c("", NA_character_),
    redcap_repeat_instrument = c(NA_character_, "diary"),
    redcap_repeat_instance = c(NA_character_, "2")
  )
  plan <- plan_from_data(accepted, rcon, c("demographics", "diary"))
  expect_true("001" %in% plan$assessible_targets$record_id)
  expect_identical(
    plan$assessible_targets$repeat_instance[plan$assessible_targets$instrument == "diary"],
    2L
  )
  rejected_instance <- accepted
  rejected_instance$redcap_repeat_instance[[2]] <- "02"
  expect_error(
    plan_from_data(rejected_instance, rcon, "diary"),
    class = "redcapmissing_error_schema"
  )
  rejected_id <- accepted
  rejected_id$record_id[[1]] <- " 001"
  expect_error(
    plan_from_data(rejected_id, rcon, "diary"),
    class = "redcapmissing_error_schema"
  )
  duplicate <- accepted[c(1, 1), ]
  duplicate$redcap_event_name <- c("", NA_character_)
  expect_error(
    plan_from_data(duplicate, rcon, "demographics"),
    class = "redcapmissing_error_schema"
  )
})

test_that("nullable structural dimensions accept every typed NA representation", {
  rcon <- .plan_fake_rcon()
  missing_values <- list(
    NA,
    NA_character_,
    NA_integer_,
    NA_real_,
    factor(NA_character_)
  )
  for (missing_value in missing_values) {
    data <- tibble::tibble(
      record_id = "r1",
      redcap_event_name = missing_value,
      redcap_repeat_instrument = missing_value,
      redcap_repeat_instance = missing_value
    )
    plan <- plan_from_data(data, rcon, "demographics")
    expect_true(is.na(plan$assessible_targets$redcap_event_name[[1]]))
    expect_true(is.na(plan$assessible_targets$repeat_instance[[1]]))

    extension <- tibble::tibble(
      instrument = "demographics",
      redcap_event_name = missing_value,
      repeat_instance = missing_value
    )
    extended <- plan_from_data(data, rcon, "demographics", extension)
    expect_identical(extended$assessible_targets$target_source, "observed+extended")

    explicit <- tibble::tibble(
      record_id = "r1",
      instrument = "demographics",
      redcap_event_name = missing_value,
      repeat_instance = missing_value
    )
    declared <- plan_explicit(data, rcon, "demographics", explicit)
    expect_identical(declared$assessible_targets$target_source, "explicit")
  }

  all_missing_atomic <- list(
    logical = c(NA, NA),
    character = c(NA_character_, NA_character_),
    integer = c(NA_integer_, NA_integer_),
    double = c(NA_real_, NA_real_),
    complex = c(NA_complex_, NA_complex_)
  )
  for (storage in all_missing_atomic) {
    data <- tibble::tibble(
      record_id = c("r1", "r2"),
      redcap_event_name = storage,
      redcap_repeat_instrument = storage,
      redcap_repeat_instance = c(NA_integer_, NA_integer_)
    )
    plan <- plan_from_data(data, rcon, "demographics")
    expect_identical(
      plan$assessible_targets$redcap_event_name,
      c(NA_character_, NA_character_)
    )
    expect_identical(
      plan$assessible_targets$repeat_instrument,
      c(NA_character_, NA_character_)
    )
  }
})

test_that("nullable structural dimensions reject Date POSIXt NaN and nonmissing values", {
  rcon <- .plan_fake_rcon()
  rejected_nullable_values <- list(
    nonmissing_number = 1,
    date = as.Date(NA_character_),
    timestamp = as.POSIXct(NA_character_, tz = "UTC"),
    nan = NaN
  )
  for (value in rejected_nullable_values) {
    expect_error(
      plan_from_data(
        tibble::tibble(record_id = "r1", redcap_event_name = value),
        rcon,
        "demographics"
      ),
      class = "redcapmissing_error_schema"
    )
    expect_error(
      plan_from_data(
        tibble::tibble(
          record_id = "r1",
          redcap_repeat_instrument = value,
          redcap_repeat_instance = NA_integer_
        ),
        rcon,
        "demographics"
      ),
      class = "redcapmissing_error_schema"
    )
  }

  for (value in list(
    as.Date(NA_character_),
    as.POSIXct(NA_character_, tz = "UTC"),
    NaN
  )) {
    expect_error(
      plan_from_data(
        tibble::tibble(
          record_id = "r1",
          redcap_repeat_instrument = NA_character_,
          redcap_repeat_instance = value
        ),
        rcon,
        "demographics"
      ),
      class = "redcapmissing_error_schema"
    )
  }
})

test_that("record identifiers normalize accepted storage and reject invalid values", {
  rcon <- .plan_fake_rcon()
  accepted <- list(
    leading_zero = "001",
    factor = factor("factor_id"),
    integer = 7L,
    double = 8,
    literal_na = "NA",
    literal_null = "NULL",
    literal_dot = ".",
    literal_sentinel = "-999"
  )
  expected <- c(
    leading_zero = "001", factor = "factor_id", integer = "7", double = "8",
    literal_na = "NA", literal_null = "NULL", literal_dot = ".",
    literal_sentinel = "-999"
  )
  for (case in names(accepted)) {
    plan <- plan_from_data(
      tibble::tibble(record_id = accepted[[case]]),
      rcon,
      "demographics"
    )
    expect_identical(unique(plan$assessible_targets$record_id), expected[[case]])
  }

  rejected <- list(
    NA, NA_character_, NA_integer_, NA_real_, NaN, Inf, -Inf,
    "", " ", " 1", "1 ", TRUE, as.Date("2026-01-01")
  )
  for (value in rejected) {
    expect_error(
      plan_from_data(tibble::tibble(record_id = value), rcon, "demographics"),
      class = "redcapmissing_error_schema"
    )
  }
  expect_error(
    plan_from_data(tibble::tibble(other = "1"), rcon, "demographics"),
    class = "redcapmissing_error_schema"
  )
})

test_that("repeat columns and instances enforce contextual missingness", {
  rcon <- .plan_fake_rcon(
    repeats = tibble::tibble(event_name = NA_character_, form_name = "diary")
  )
  accepted <- list(1L, 2, "3")
  for (i in seq_along(accepted)) {
    data <- tibble::tibble(
      record_id = "r1",
      redcap_repeat_instrument = "diary",
      redcap_repeat_instance = accepted[[i]]
    )
    plan <- plan_from_data(data, rcon, "diary")
    expect_identical(plan$assessible_targets$repeat_instance, as.integer(i))
    expect_identical(plan$assessible_targets$repeat_instrument, "diary")
  }

  rejected <- list(
    NA, NA_character_, NA_integer_, NA_real_, factor(NA_character_),
    "", " ", "01", "0", "-1", "1.0", "one", factor("4"),
    0L, -1L, 1.5, NaN, Inf, -Inf, .Machine$integer.max + 1
  )
  for (value in rejected) {
    data <- tibble::tibble(
      record_id = "r1",
      redcap_repeat_instrument = "diary",
      redcap_repeat_instance = value
    )
    expect_error(
      plan_from_data(data, rcon, "diary"),
      class = "redcapmissing_error_schema"
    )
  }

  expect_error(
    plan_from_data(
      tibble::tibble(record_id = "r1", redcap_repeat_instrument = "diary"),
      rcon,
      "diary"
    ),
    class = "redcapmissing_error_schema"
  )
  expect_error(
    plan_from_data(
      tibble::tibble(record_id = "r1", redcap_repeat_instance = 1L),
      rcon,
      "diary"
    ),
    class = "redcapmissing_error_schema"
  )
})

test_that("empty schedules require complete ordered schemas and allowed storage", {
  rcon <- .plan_fake_rcon()
  data <- tibble::tibble(record_id = "r1")
  observed <- plan_from_data(data, rcon, "demographics")
  expect_error(
    plan_from_data(tibble::tibble(record_id = character()), rcon, "demographics"),
    class = "redcapmissing_error_schema"
  )

  valid_extensions <- list(
    tibble::tibble(
      instrument = character(), redcap_event_name = character(),
      repeat_instance = integer()
    ),
    tibble::tibble(
      instrument = factor(character()), redcap_event_name = factor(character()),
      repeat_instance = numeric()
    )
  )
  for (schedule in valid_extensions) {
    expect_identical(
      plan_from_data(data, rcon, "demographics", schedule)$assessible_targets,
      observed$assessible_targets
    )
  }
  expect_identical(
    plan_from_data(data, rcon, "demographics", NULL)$assessible_targets,
    observed$assessible_targets
  )

  invalid_extension_schemas <- list(
    tibble::tibble(instrument = character(), redcap_event_name = character()),
    tibble::tibble(
      instrument = character(), redcap_event_name = character(),
      repeat_instance = integer(), extra = character()
    ),
    tibble::tibble(
      redcap_event_name = character(), instrument = character(),
      repeat_instance = integer()
    )
  )
  for (schedule in invalid_extension_schemas) {
    expect_error(
      plan_from_data(data, rcon, "demographics", schedule),
      class = "redcapmissing_error_schedule"
    )
  }
  invalid_extension_storage <- list(
    tibble::tibble(
      instrument = integer(), redcap_event_name = character(),
      repeat_instance = integer()
    ),
    tibble::tibble(
      instrument = character(), redcap_event_name = integer(),
      repeat_instance = integer()
    ),
    tibble::tibble(
      instrument = character(), redcap_event_name = character(),
      repeat_instance = logical()
    )
  )
  for (schedule in invalid_extension_storage) {
    expect_error(
      plan_from_data(data, rcon, "demographics", schedule),
      class = "redcapmissing_error_schema"
    )
  }

  empty_data <- tibble::tibble(record_id = character())
  valid_explicit <- tibble::tibble(
    record_id = integer(), instrument = factor(character()),
    redcap_event_name = character(), repeat_instance = numeric()
  )
  explicit_plan <- plan_explicit(empty_data, rcon, "demographics", valid_explicit)
  expect_identical(explicit_plan$assessible_targets, redcapmissing:::.assessible_target_build_prototype())
  absent_explicit <- tibble::tibble(
    record_id = "not_exported",
    instrument = "demographics",
    redcap_event_name = NA_character_,
    repeat_instance = NA_integer_
  )
  absent_plan <- plan_explicit(
    empty_data, rcon, "demographics", absent_explicit
  )
  expect_identical(absent_plan$assessible_targets$record_id, "not_exported")
  expect_identical(absent_plan$assessible_targets$instrument, "demographics")
  expect_true(is.na(absent_plan$assessible_targets$redcap_event_name[[1]]))
  expect_true(is.na(absent_plan$assessible_targets$repeat_instance[[1]]))
  expect_identical(absent_plan$assessible_targets$target_source, "explicit")


  invalid_explicit_schemas <- list(
    valid_explicit[-1],
    dplyr::mutate(valid_explicit, extra = character()),
    valid_explicit[c("instrument", "record_id", "redcap_event_name", "repeat_instance")]
  )
  for (schedule in invalid_explicit_schemas) {
    expect_error(
      plan_explicit(empty_data, rcon, "demographics", schedule),
      class = "redcapmissing_error_schedule"
    )
  }
  invalid_record_storage <- tibble::tibble(
    record_id = logical(), instrument = character(),
    redcap_event_name = character(), repeat_instance = integer()
  )
  expect_error(
    plan_explicit(empty_data, rcon, "demographics", invalid_record_storage),
    class = "redcapmissing_error_schema"
  )
})

test_that("normalized schedule collisions error before target construction", {
  rcon <- .plan_fake_rcon()
  data <- tibble::tibble(record_id = "r1")
  extension <- tibble::tibble(
    instrument = factor(c("demographics", "demographics")),
    redcap_event_name = c("", NA_character_),
    repeat_instance = c("", NA_character_)
  )
  expect_error(
    plan_from_data(data, rcon, "demographics", extension),
    class = "redcapmissing_error_schedule"
  )

  explicit <- tibble::tibble(
    record_id = factor(c("r1", "r1")),
    instrument = factor(c("demographics", "demographics")),
    redcap_event_name = c("", NA_character_),
    repeat_instance = c("", NA_character_)
  )
  expect_error(
    plan_explicit(data, rcon, "demographics", explicit),
    class = "redcapmissing_error_schedule"
  )
})

test_that("event missingness is contextual for classic and longitudinal planning", {
  classic_rcon <- .plan_fake_rcon()
  expect_error(
    plan_from_data(
      tibble::tibble(record_id = "r1", redcap_event_name = "baseline_arm_1"),
      classic_rcon,
      "demographics"
    ),
    class = "redcapmissing_error_schema"
  )

  longitudinal_rcon <- .plan_longitudinal_rcon()
  missing_events <- list(
    NA, NA_character_, NA_integer_, NA_real_, factor(NA_character_), "", " "
  )
  for (event in missing_events) {
    expect_error(
      plan_from_data(
        tibble::tibble(record_id = "r1", redcap_event_name = event),
        longitudinal_rcon,
        "demographics"
      ),
      class = "redcapmissing_error_schema"
    )
  }
  for (event in c(" baseline_arm_1", "baseline_arm_1 ", "unknown_arm_1")) {
    expect_error(
      plan_from_data(
        tibble::tibble(record_id = "r1", redcap_event_name = event),
        longitudinal_rcon,
        "demographics"
      ),
      class = "redcapmissing_error_schema"
    )
  }

  missing_schedule_events <- list(
    NA, NA_character_, NA_integer_, NA_real_, factor(NA_character_), "", " "
  )
  for (event in missing_schedule_events) {
    schedule <- tibble::tibble(
      instrument = "demographics",
      redcap_event_name = event,
      repeat_instance = NA_integer_
    )
    expect_error(
      plan_from_data(
        tibble::tibble(
          record_id = "r1",
          redcap_event_name = "baseline_arm_1",
          redcap_repeat_instrument = NA_character_,
          redcap_repeat_instance = NA_integer_
        ),
        longitudinal_rcon,
        "demographics",
        schedule
      ),
      class = "redcapmissing_error_schedule"
    )
  }

  classic_extended <- tibble::tibble(
    instrument = "demographics",
    redcap_event_name = "baseline_arm_1",
    repeat_instance = NA_integer_
  )
  expect_error(
    plan_from_data(
      tibble::tibble(record_id = "r1"),
      classic_rcon,
      "demographics",
      classic_extended
    ),
    class = "redcapmissing_error_schedule"
  )
  classic_explicit <- tibble::add_column(
    classic_extended,
    record_id = "r1",
    .before = 1L
  )
  expect_error(
    plan_explicit(
      tibble::tibble(record_id = "r1"),
      classic_rcon,
      "demographics",
      classic_explicit
    ),
    class = "redcapmissing_error_schedule"
  )

  longitudinal_explicit <- tibble::tibble(
    record_id = "r1",
    instrument = "demographics",
    redcap_event_name = NA_character_,
    repeat_instance = NA_integer_
  )
  expect_error(
    plan_explicit(
      .plan_longitudinal_data(),
      longitudinal_rcon,
      "demographics",
      longitudinal_explicit
    ),
    class = "redcapmissing_error_schedule"
  )

  edited_plan <- plan_from_data(
    .plan_longitudinal_data(),
    longitudinal_rcon,
    "demographics"
  )
  edited_plan$assessible_targets$redcap_event_name <- NA_character_
  expect_error(
    run_plan(
      edited_plan,
      .plan_longitudinal_data(),
      longitudinal_rcon,
      progress = FALSE
    ),
    class = "redcapmissing_error_plan"
  )
})

test_that("large numeric record vectors normalize without row loss", {
  record_count <- 10000L
  numeric_ids <- as.double(seq_len(record_count))
  expected_ids <- as.character(seq_len(record_count))

  plan <- plan_from_data(
    tibble::tibble(record_id = numeric_ids),
    .plan_fake_rcon(),
    "demographics"
  )

  expect_identical(nrow(plan$assessible_targets), record_count)
  expect_identical(
    typeof(plan$assessible_targets$record_id),
    "character"
  )
  expect_setequal(
    plan$assessible_targets$record_id,
    expected_ids
  )
  expect_false(anyDuplicated(plan$assessible_targets$record_id) > 0L)
})

test_that("repeat configuration must be explicit and consistent with project status", {
  data <- tibble::tibble(record_id = "r1")

  absent <- .plan_fake_rcon()
  absent$repeatInstrumentEvent <- function() NULL
  expect_error(
    plan_from_data(data, absent, "demographics"),
    class = "redcapmissing_error_project"
  )

  flagged <- .plan_fake_rcon()
  flagged$projectInformation <- function() tibble::tibble(
    project_id = 41L,
    is_longitudinal = 0L,
    has_repeating_instruments_or_events = 1L
  )
  expect_error(
    plan_from_data(data, flagged, "demographics"),
    class = "redcapmissing_error_project"
  )

  contradictory <- .plan_fake_rcon(repeats = tibble::tibble(
    event_name = NA_character_,
    form_name = "diary"
  ))
  contradictory$projectInformation <- function() tibble::tibble(
    project_id = 41L,
    is_longitudinal = 0L,
    has_repeating_instruments_or_events = 0L
  )
  repeat_data <- tibble::tibble(
    record_id = "r1",
    redcap_repeat_instrument = "diary",
    redcap_repeat_instance = 1L
  )
  expect_error(
    plan_from_data(repeat_data, contradictory, "diary"),
    class = "redcapmissing_error_project"
  )
  classic_concrete_event <- .plan_fake_rcon(repeats = tibble::tibble(
    event_name = "baseline_arm_1",
    form_name = "diary"
  ))
  expect_error(
    plan_from_data(data, classic_concrete_event, "diary"),
    class = "redcapmissing_error_project"
  )

  classic_repeating_event <- .plan_fake_rcon(repeats = tibble::tibble(
    event_name = "baseline_arm_1",
    form_name = NA_character_
  ))
  expect_error(
    plan_from_data(data, classic_repeating_event, "demographics"),
    class = "redcapmissing_error_project"
  )

  longitudinal_missing_event <- .plan_longitudinal_rcon(tibble::tibble(
    event_name = NA_character_,
    form_name = "diary"
  ))
  expect_error(
    plan_from_data(.plan_longitudinal_data(), longitudinal_missing_event, "diary"),
    class = "redcapmissing_error_project"
  )

  longitudinal_undesignated_repeat <- .plan_longitudinal_rcon(tibble::tibble(
    event_name = "baseline_arm_1",
    form_name = "notes"
  ))
  expect_error(
    plan_from_data(
      .plan_longitudinal_data(),
      longitudinal_undesignated_repeat,
      "notes"
    ),
    class = "redcapmissing_error_project"
  )
})

test_that("nonmissing factor repeat instances are rejected", {
  rcon <- .plan_fake_rcon(repeats = tibble::tibble(
    event_name = NA_character_,
    form_name = "diary"
  ))
  data <- tibble::tibble(
    record_id = "r1",
    redcap_repeat_instrument = "diary",
    redcap_repeat_instance = factor("2")
  )
  expect_error(
    plan_from_data(data, rcon, "diary"),
    class = "redcapmissing_error_schema"
  )
})
