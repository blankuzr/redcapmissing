.plan_metadata <- function() {
  tibble::tibble(
    field_name = c("record_id", "age", "note", "diary_value"),
    form_name = c("demographics", "demographics", "notes", "diary"),
    field_type = rep("text", 4L)
  )
}

.plan_fake_rcon <- function(
  longitudinal = FALSE,
  metadata = .plan_metadata(),
  instruments = NULL,
  arms = NULL,
  events = NULL,
  mapping = NULL,
  repeats = tibble::tibble(),
  project_id = 41L,
  record_id_field = NULL
) {
  if (is.null(instruments)) {
    instrument_names <- unique(metadata$form_name)
    instruments <- tibble::tibble(
      instrument_name = instrument_names,
      instrument_label = paste(instrument_names, "label")
    )
  }
  info <- tibble::tibble(
    project_id = project_id,
    is_longitudinal = as.integer(longitudinal)
  )
  if (!is.null(record_id_field)) info$record_id_field <- record_id_field
  connection <- list(
    metadata = function() metadata,
    instruments = function() instruments,
    projectInformation = function() info,
    arms = function() arms,
    events = function() events,
    mapping = function() mapping,
    repeatInstrumentEvent = function() repeats,
    exportRecords = function(...) stop("constructors must not export records")
  )
  redcap_api_connection_fixture(connection)
}

.plan_longitudinal_rcon <- function(repeats = NULL) {
  metadata <- .plan_metadata()
  arms <- tibble::tibble(
    arm_num = c(1L, 2L),
    name = c("Treatment", "Comparator")
  )
  events <- tibble::tibble(
    event_id = c(101L, 102L, 201L),
    unique_event_name = c("baseline_arm_1", "visit_arm_1", "baseline_arm_2"),
    event_name = c("Baseline", "Visit", "Baseline"),
    arm_num = c(1L, 1L, 2L)
  )
  mapping <- tibble::tibble(
    arm_num = c(1L, 1L, 1L, 1L, 2L),
    unique_event_name = c(
      "baseline_arm_1", "baseline_arm_1", "visit_arm_1",
      "visit_arm_1", "baseline_arm_2"
    ),
    form = c("demographics", "diary", "notes", "diary", "demographics")
  )
  if (is.null(repeats)) {
    repeats <- tibble::tibble(
      event_name = "visit_arm_1",
      form_name = "diary"
    )
  }
  .plan_fake_rcon(
    longitudinal = TRUE,
    metadata = metadata,
    arms = arms,
    events = events,
    mapping = mapping,
    repeats = repeats
  )
}

.plan_longitudinal_data <- function() {
  tibble::tibble(
    record_id = c("r1", "r1", "r1", "r2"),
    redcap_event_name = c(
      "baseline_arm_1", "visit_arm_1", "visit_arm_1", "baseline_arm_2"
    ),
    redcap_repeat_instrument = c(NA, NA, "diary", NA),
    redcap_repeat_instance = c(NA, NA, 2L, NA),
    age = c("", "", "", ""),
    note = c("", "", "", ""),
    diary_value = c("", "", "", "")
  )
}

test_that("plan constructors expose the exact public signatures", {
  expect_identical(
    names(formals(plan_from_data)),
    c("data", "rcon", "instruments", "extended_schedule")
  )
  expect_null(formals(plan_from_data)$extended_schedule)
  expect_identical(
    names(formals(plan_explicit)),
    c("data", "rcon", "instruments", "explicit_schedule")
  )
})

test_that("plan constructors require supported redcapAPI connection classes", {
  rcon <- .plan_fake_rcon()
  class(rcon) <- "redcapConnection"

  expect_error(
    plan_from_data(tibble::tibble(record_id = "1"), rcon, "demographics"),
    regexp = "redcapApiConnection.*redcapOfflineConnection",
    class = "redcapmissing_error_project"
  )
})

test_that("plan_from_data creates an exact compact plan from physical classic rows", {
  rcon <- .plan_fake_rcon()
  data <- tibble::tibble(
    record_id = c("01", "02"),
    age = c("", ""),
    note = c(NA_character_, NA_character_),
    diary_value = c("", "")
  )
  plan <- plan_from_data(data, rcon, c("notes", "demographics"))

  expect_s3_class(plan, "redcapmissing_plan")
  expect_identical(
    names(plan),
    c(
      "schema_version", "construction", "instruments",
      "assessible_targets", "project", "structure_fingerprint"
    )
  )
  expect_identical(plan$schema_version, 1L)
  expect_identical(plan$construction, "from_data")
  expect_identical(plan$instruments, c("notes", "demographics"))
  expect_identical(
    names(plan$assessible_targets),
    c(
      "record_id", "instrument", "redcap_event_name",
      "repeat_instrument", "repeat_instance", "target_source"
    )
  )
  expect_identical(plan$assessible_targets$record_id, c("01", "02", "01", "02"))
  expect_identical(plan$assessible_targets$instrument, c("notes", "notes", "demographics", "demographics"))
  expect_true(all(is.na(plan$assessible_targets$redcap_event_name)))
  expect_true(all(is.na(plan$assessible_targets$repeat_instance)))
  expect_identical(plan$assessible_targets$target_source, rep("observed", 4))
  expect_false(any(vapply(plan, identical, logical(1), data)))
  expect_false(any(vapply(plan, identical, logical(1), rcon)))

  printed <- capture.output(print(plan))
  printed_text <- paste(printed, collapse = "\n")
  expect_match(printed[[1]], "<redcapmissing_plan>", fixed = TRUE)
  expect_match(printed_text, "Construction: from_data", fixed = TRUE)
  expect_match(printed_text, "Instruments:  2", fixed = TRUE)
  expect_match(printed_text, "Targets:      4", fixed = TRUE)
  expect_false(any(grepl("01|02", printed)))
})

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
  data <- .plan_longitudinal_data()
  data <- data[data$record_id == "r1", ]
  extension <- tibble::tibble(
    instrument = "demographics",
    redcap_event_name = "baseline_arm_2",
    repeat_instance = NA_integer_
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
  expect_false(any(plan$assessible_targets$redcap_event_name == "baseline_arm_2"))
})

test_that("plan_explicit freezes exact targets including records absent from data", {
  rcon <- .plan_longitudinal_rcon()
  data <- .plan_longitudinal_data()
  schedule <- tibble::tibble(
    record_id = c("r1", "not_exported"),
    instrument = c("diary", "demographics"),
    redcap_event_name = c("visit_arm_1", "baseline_arm_2"),
    repeat_instance = c(4L, NA_integer_)
  )
  plan <- plan_explicit(data, rcon, c("diary", "demographics"), schedule)

  expect_identical(plan$construction, "explicit")
  expect_identical(plan$assessible_targets$record_id, c("r1", "not_exported"))
  expect_identical(plan$assessible_targets$target_source, c("explicit", "explicit"))
  expect_error(
    plan_explicit(
      data,
      rcon,
      "demographics",
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
  plan <- plan_explicit(data, rcon, "demographics", schedule)
  expect_s3_class(plan, "redcapmissing_plan")
  expect_identical(plan$assessible_targets, redcapmissing:::.assessible_target_build_prototype())
  expect_error(
    plan_explicit(data, rcon, "demographics", NULL),
    class = "redcapmissing_error_argument"
  )
  wrong_storage <- tibble::tibble(
    record_id = character(),
    instrument = character(),
    redcap_event_name = logical(),
    repeat_instance = integer()
  )
  expect_error(
    plan_explicit(data, rcon, "demographics", wrong_storage),
    class = "redcapmissing_error_schema"
  )
})

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
  expect_setequal(repeated$instrument, c("notes", "diary"))
  expect_true(all(repeated$repeat_instance == 2L))
  expect_true(all(is.na(repeated$repeat_instrument)))
})

test_that("fingerprint table normalization ignores row and column order", {
  original <- tibble::tibble(
    second = c(2L, 1L),
    first = factor(c("b", "a"))
  )
  reordered <- original[2:1, c("first", "second")]

  expect_identical(
    redcapmissing:::.structure_fingerprint_encode_table(original),
    redcapmissing:::.structure_fingerprint_encode_table(reordered)
  )
})

test_that("project fingerprints are stable to table row order with explicit record identity", {
  rcon <- .plan_longitudinal_rcon()
  rcon$projectInformation <- function() tibble::tibble(
    project_id = 41L,
    is_longitudinal = 1L,
    record_id_field = "record_id"
  )
  shuffled <- rcon
  original_metadata <- rcon$metadata()
  original_instruments <- rcon$instruments()
  original_arms <- rcon$arms()
  original_events <- rcon$events()
  original_mapping <- rcon$mapping()
  shuffled$metadata <- function() original_metadata[rev(seq_len(nrow(original_metadata))), ]
  shuffled$instruments <- function() original_instruments[rev(seq_len(nrow(original_instruments))), ]
  shuffled$arms <- function() original_arms[rev(seq_len(nrow(original_arms))), ]
  shuffled$events <- function() original_events[rev(seq_len(nrow(original_events))), ]
  shuffled$mapping <- function() original_mapping[rev(seq_len(nrow(original_mapping))), ]

  first <- redcapmissing:::.project_structure_build_snapshot(rcon)
  second <- redcapmissing:::.project_structure_build_snapshot(shuffled)
  expect_identical(first$project, second$project)
  expect_identical(first$structure_fingerprint, second$structure_fingerprint)
  fingerprint_input <- list(
    project = first$project,
    metadata = redcapmissing:::.structure_fingerprint_encode_table(first$metadata),
    instruments = redcapmissing:::.structure_fingerprint_encode_table(first$instruments),
    arms = redcapmissing:::.structure_fingerprint_encode_table(first$arms),
    events = redcapmissing:::.structure_fingerprint_encode_table(first$events),
    mapping = redcapmissing:::.structure_fingerprint_encode_table(first$mapping),
    repeat_configuration = redcapmissing:::.structure_fingerprint_encode_table(first$repeat_configuration)
  )
  expect_identical(
    first$structure_fingerprint,
    digest::digest(fingerprint_input, algo = "sha256", serialize = TRUE)
  )
  expect_match(first$structure_fingerprint, "^[0-9a-f]{64}$")

  data <- .plan_longitudinal_data()
  first_plan <- plan_from_data(data, rcon, c("diary", "notes"))
  second_plan <- plan_from_data(data, shuffled, c("diary", "notes"))
  expect_identical(first_plan, second_plan)
  expect_silent(
    redcapmissing:::.plan_validate_object(first_plan, second)
  )
  expect_s3_class(
    run_plan(
      first_plan,
      data,
      shuffled,
      required_fields = FALSE,
      exclude_types = NULL,
      progress = FALSE
    ),
    "redcapmissing"
  )
})

test_that("fingerprints distinguish missing values and structured delimiter values", {
  missing_metadata <- .plan_metadata()
  missing_metadata$fingerprint_value <- c(
    NA_character_,
    rep("same", nrow(missing_metadata) - 1L)
  )
  literal_metadata <- missing_metadata
  literal_metadata$fingerprint_value[[1L]] <- "<NA>"

  missing_snapshot <- redcapmissing:::.project_structure_build_snapshot(
    .plan_fake_rcon(metadata = missing_metadata)
  )
  literal_snapshot <- redcapmissing:::.project_structure_build_snapshot(
    .plan_fake_rcon(metadata = literal_metadata)
  )
  expect_false(identical(
    missing_snapshot$structure_fingerprint,
    literal_snapshot$structure_fingerprint
  ))

  separator <- intToUtf8(31L)
  first_metadata <- .plan_metadata()
  first_values <- rep(list("same"), nrow(first_metadata))
  first_values[[1L]] <- c(paste0("a", separator, "b"), "c")
  first_metadata$fingerprint_value <- first_values
  second_metadata <- first_metadata
  second_metadata$fingerprint_value[[1L]] <- c(
    "a",
    paste0("b", separator, "c")
  )

  first_snapshot <- redcapmissing:::.project_structure_build_snapshot(
    .plan_fake_rcon(
      metadata = first_metadata,
      record_id_field = "record_id"
    )
  )
  second_snapshot <- redcapmissing:::.project_structure_build_snapshot(
    .plan_fake_rcon(
      metadata = second_metadata,
      record_id_field = "record_id"
    )
  )
  shuffled_snapshot <- redcapmissing:::.project_structure_build_snapshot(
    .plan_fake_rcon(
      metadata = first_metadata[rev(seq_len(nrow(first_metadata))), ],
      record_id_field = "record_id"
    )
  )
  expect_false(identical(
    first_snapshot$structure_fingerprint,
    second_snapshot$structure_fingerprint
  ))
  expect_identical(
    first_snapshot$structure_fingerprint,
    shuffled_snapshot$structure_fingerprint
  )
})

test_that("plan validation rejects hand edits and changed project structure", {
  rcon <- .plan_fake_rcon()
  plan <- plan_from_data(tibble::tibble(record_id = "r1"), rcon, "demographics")
  malformed <- plan
  malformed$assessible_targets <- malformed$assessible_targets[c(1, 1), ]
  expect_error(
    redcapmissing:::.plan_validate_object(malformed),
    class = "redcapmissing_error_plan"
  )
  changed <- .plan_fake_rcon(project_id = 99L)
  expect_error(
    redcapmissing:::.plan_validate_object(
      plan,
      redcapmissing:::.project_structure_build_snapshot(changed)
    ),
    class = "redcapmissing_error_plan"
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
  invalid_event <- tibble::tibble(record_id = "r1", redcap_event_name = 1)
  expect_error(
    plan_from_data(invalid_event, rcon, "demographics"),
    class = "redcapmissing_error_schema"
  )
  invalid_instance <- tibble::tibble(
    record_id = "r1",
    redcap_repeat_instrument = NA_character_,
    redcap_repeat_instance = NaN
  )
  expect_error(
    plan_from_data(invalid_instance, rcon, "demographics"),
    class = "redcapmissing_error_schema"
  )
})

test_that("required constructor arguments and instrument vectors fail with classed errors", {
  rcon <- .plan_fake_rcon()
  data <- tibble::tibble(record_id = "r1")
  expect_error(
    plan_from_data(rcon = rcon, instruments = "demographics"),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    plan_from_data(data = data, instruments = "demographics"),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    plan_from_data(data = data, rcon = rcon),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    plan_from_data(data, rcon, factor("demographics")),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    plan_explicit(data, rcon, "demographics"),
    class = "redcapmissing_error_argument"
  )
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

test_that("plan project label maps are complete and protected", {
  rcon <- .plan_longitudinal_rcon()
  rcon$instruments <- function() tibble::tibble(
    instrument_name = c("notes", "diary", "demographics"),
    instrument_label = c("Notes label", "Diary label", NA_character_)
  )
  plan <- plan_from_data(
    .plan_longitudinal_data(),
    rcon,
    c("diary", "demographics", "notes")
  )

  expect_identical(
    names(plan$project),
    c("project_id", "record_id_field", "longitudinal", "event_labels", "instrument_labels")
  )
  expect_identical(plan$project$project_id, "41")
  expect_identical(plan$project$record_id_field, "record_id")
  expect_true(plan$project$longitudinal)
  expect_identical(
    plan$project$instrument_labels,
    c(
      demographics = "demographics",
      diary = "Diary label",
      notes = "Notes label"
    )
  )
  expect_identical(
    plan$project$event_labels,
    c(
      baseline_arm_1 = "Baseline",
      baseline_arm_2 = "Baseline",
      visit_arm_1 = "Visit"
    )
  )
  expect_match(plan$structure_fingerprint, "^[0-9a-f]{64}$")

  malformed <- list()
  malformed$unnamed_instruments <- plan
  malformed$unnamed_instruments$project$instrument_labels <- unname(
    malformed$unnamed_instruments$project$instrument_labels
  )
  malformed$duplicate_instrument_keys <- plan
  names(malformed$duplicate_instrument_keys$project$instrument_labels)[2] <-
    names(malformed$duplicate_instrument_keys$project$instrument_labels)[1]
  malformed$missing_instrument_label <- plan
  malformed$missing_instrument_label$project$instrument_labels[[1]] <- NA_character_
  malformed$absent_selected_instrument <- plan
  malformed$absent_selected_instrument$project$instrument_labels <-
    malformed$absent_selected_instrument$project$instrument_labels[-1]
  malformed$unnamed_events <- plan
  malformed$unnamed_events$project$event_labels <- unname(
    malformed$unnamed_events$project$event_labels
  )
  malformed$duplicate_event_keys <- plan
  names(malformed$duplicate_event_keys$project$event_labels)[2] <-
    names(malformed$duplicate_event_keys$project$event_labels)[1]
  malformed$missing_event_label <- plan
  malformed$missing_event_label$project$event_labels[[1]] <- NA_character_

  for (candidate in malformed) {
    expect_error(
      redcapmissing:::.plan_validate_object(candidate),
      class = "redcapmissing_error_plan"
    )
  }
})

test_that("plan validation rejects malformed components and target invariants", {
  rcon <- .plan_fake_rcon()
  plan <- plan_from_data(tibble::tibble(record_id = "r1"), rcon, "demographics")

  malformed <- list(
    classless = unclass(plan),
    missing_component = plan[-1],
    reordered_components = plan[rev(names(plan))]
  )
  malformed$schema_version <- plan
  malformed$schema_version$schema_version <- 1
  malformed$construction <- plan
  malformed$construction$construction <- "observed"
  malformed$empty_instruments <- plan
  malformed$empty_instruments$instruments <- character()
  malformed$duplicate_instruments <- plan
  malformed$duplicate_instruments$instruments <- c("demographics", "demographics")
  malformed$padded_instrument <- plan
  malformed$padded_instrument$instruments <- " demographics"
  malformed$project_components <- plan
  malformed$project_components$project <- malformed$project_components$project[rev(names(plan$project))]
  malformed$missing_project_id <- plan
  malformed$missing_project_id$project$project_id <- NA_character_
  malformed$blank_project_id <- plan
  malformed$blank_project_id$project$project_id <- ""
  malformed$missing_record_field <- plan
  malformed$missing_record_field$project$record_id_field <- NA_character_
  malformed$blank_record_field <- plan
  malformed$blank_record_field$project$record_id_field <- ""
  malformed$fingerprint <- plan
  malformed$fingerprint$structure_fingerprint <- toupper(plan$structure_fingerprint)
  malformed$target_column_order <- plan
  malformed$target_column_order$assessible_targets <-
    malformed$target_column_order$assessible_targets[rev(names(plan$assessible_targets))]
  malformed$target_storage <- plan
  malformed$target_storage$assessible_targets$repeat_instance <-
    as.numeric(malformed$target_storage$assessible_targets$repeat_instance)
  malformed$target_source <- plan
  malformed$target_source$assessible_targets$target_source <- "explicit"
  malformed$blank_record <- plan
  malformed$blank_record$assessible_targets$record_id <- ""
  malformed$unknown_instrument <- plan
  malformed$unknown_instrument$assessible_targets$instrument <- "unknown"
  malformed$zero_instance <- plan
  malformed$zero_instance$assessible_targets$repeat_instance <- 0L
  malformed$duplicate_target <- plan
  malformed$duplicate_target$assessible_targets <- dplyr::bind_rows(
    plan$assessible_targets,
    plan$assessible_targets
  )

  failures <- character()
  for (name in names(malformed)) {
    failed <- inherits(
      tryCatch(
        redcapmissing:::.plan_validate_object(malformed[[name]]),
        error = identity
      ),
      "redcapmissing_error_plan"
    )
    if (!failed) failures <- c(failures, name)
  }
  expect_identical(failures, character())

  empty_data <- tibble::tibble(record_id = character())
  empty_schedule <- tibble::tibble(
    record_id = character(),
    instrument = character(),
    redcap_event_name = character(),
    repeat_instance = integer()
  )
  empty_plan <- plan_explicit(
    empty_data,
    rcon,
    "demographics",
    empty_schedule
  )
  empty_plan$construction <- factor("explicit")
  expect_error(
    redcapmissing:::.plan_validate_object(empty_plan),
    class = "redcapmissing_error_plan"
  )

  snapshot <- redcapmissing:::.project_structure_build_snapshot(rcon)
  disallowed <- plan
  disallowed$assessible_targets$repeat_instrument <- "demographics"
  disallowed$assessible_targets$repeat_instance <- 1L
  expect_error(
    redcapmissing:::.plan_validate_object(disallowed, snapshot),
    class = "redcapmissing_error_plan"
  )
})

test_that("constructors retrieve project surfaces and make zero record export calls", {
  classic <- .plan_fake_rcon()
  for (surface in c("metadata", "instruments", "projectInformation", "repeatInstrumentEvent")) {
    incomplete <- classic
    incomplete[[surface]] <- NULL
    expect_error(
      plan_from_data(tibble::tibble(record_id = "r1"), incomplete, "demographics"),
      class = "redcapmissing_error_project"
    )
  }

  duplicate_surface <- .plan_fake_rcon()
  metadata <- duplicate_surface$metadata()
  metadata$duplicate <- "first"
  metadata$second_duplicate <- "second"
  names(metadata)[(ncol(metadata) - 1L):ncol(metadata)] <-
    c("duplicate", "duplicate")
  duplicate_surface$metadata <- function() metadata
  expect_error(
    plan_from_data(tibble::tibble(record_id = "r1"), duplicate_surface, "demographics"),
    class = "redcapmissing_error_project"
  )

  longitudinal <- .plan_longitudinal_rcon()
  for (surface in c("arms", "events", "mapping")) {
    incomplete <- longitudinal
    incomplete[[surface]] <- NULL
    expect_error(
      plan_from_data(.plan_longitudinal_data(), incomplete, "diary"),
      class = "redcapmissing_error_project"
    )
  }

  counted <- .plan_longitudinal_rcon()
  counts <- new.env(parent = emptyenv())
  surfaces <- c(
    "metadata", "instruments", "projectInformation", "repeatInstrumentEvent",
    "arms", "events", "mapping"
  )
  for (surface in surfaces) {
    original <- counted[[surface]]
    counted[[surface]] <- local({
      surface_name <- surface
      surface_function <- original
      function() {
        current <- if (exists(surface_name, counts, inherits = FALSE)) {
          get(surface_name, counts, inherits = FALSE)
        } else 0L
        assign(surface_name, current + 1L, counts)
        surface_function()
      }
    })
  }
  counted$exportRecords <- function(...) {
    assign("exportRecords", 1L, counts)
    stop("constructors must not export records")
  }
  expect_s3_class(
    plan_from_data(.plan_longitudinal_data(), counted, c("diary", "notes")),
    "redcapmissing_plan"
  )
  expect_identical(
    vapply(surfaces, function(surface) get(surface, counts), integer(1)),
    stats::setNames(rep(1L, length(surfaces)), surfaces)
  )
  expect_false(exists("exportRecords", counts, inherits = FALSE))
})

test_that("target construction excludes unselected and unmapped physical rows", {
  rcon <- .plan_longitudinal_rcon()
  data <- .plan_longitudinal_data()[c(1, 3), ]
  plan <- plan_from_data(data, rcon, "notes")
  expect_identical(plan$assessible_targets, redcapmissing:::.assessible_target_build_prototype())
})

test_that("explicit omissions exclude observed crossings and absent selected instruments", {
  rcon <- .plan_fake_rcon()
  data <- tibble::tibble(record_id = "r1")
  schedule <- tibble::tibble(
    record_id = "r1",
    instrument = "demographics",
    redcap_event_name = NA_character_,
    repeat_instance = NA_integer_
  )
  plan <- plan_explicit(data, rcon, c("notes", "demographics"), schedule)
  expect_identical(plan$assessible_targets$instrument, "demographics")
  expect_false("notes" %in% plan$assessible_targets$instrument)

  empty <- schedule[0, ]
  empty_plan <- plan_explicit(data, rcon, c("notes", "demographics"), empty)
  expect_identical(empty_plan$assessible_targets, redcapmissing:::.assessible_target_build_prototype())
})

test_that("unknown and unmapped schedule crossings fail before intersection", {
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

test_that("numeric identifiers and fingerprints are option independent", {
  old <- options()
  on.exit(options(old), add = TRUE)
  rcon <- .plan_fake_rcon(project_id = 100000)

  options(scipen = -9L, OutDec = ",")
  first <- plan_from_data(
    tibble::tibble(record_id = 100000),
    rcon,
    "demographics"
  )

  options(scipen = 999L, OutDec = ".")
  second <- plan_from_data(
    tibble::tibble(record_id = 100000L),
    rcon,
    "demographics"
  )

  expect_identical(first$project$project_id, "100000")
  expect_identical(first$assessible_targets$record_id, "100000")
  expect_identical(second$assessible_targets$record_id, "100000")
  expect_identical(first$structure_fingerprint, second$structure_fingerprint)

  special <- redcapmissing:::.schema_format_numeric(c(
    NA_real_, NaN, Inf, -Inf, 0, -0
  ))
  expect_identical(
    special,
    c("NA", "NaN", "Inf", "-Inf", "0", "0")
  )
  extrema <- c(.Machine$double.xmax, -.Machine$double.xmax)
  normalized_extrema <- redcapmissing:::.schema_format_numeric(extrema)
  expect_identical(
    as.numeric(normalized_extrema),
    extrema
  )
  expect_false(any(grepl("[eE]", normalized_extrema)))

  precise_ids <- c(1.234567890123456, 1.234567890123457)
  precise <- plan_from_data(
    tibble::tibble(record_id = precise_ids),
    rcon,
    "demographics"
  )
  expect_identical(
    length(unique(precise$assessible_targets$record_id)),
    2L
  )
  expect_equal(
    as.numeric(precise$assessible_targets$record_id),
    precise_ids,
    tolerance = 0
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
test_that("all public condition subclasses inherit from package base classes", {
  rcon <- .plan_fake_rcon()
  data <- tibble::tibble(record_id = "r1")
  plan <- plan_from_data(data, rcon, "demographics")
  incomplete_rcon <- rcon
  incomplete_rcon$repeatInstrumentEvent <- NULL
  bad_schedule <- tibble::tibble(
    instrument = "demographics",
    redcap_event_name = NA_character_,
    repeat_instance = NA_integer_,
    extra = "not allowed"
  )
  malformed_plan <- plan
  malformed_plan$schema_version <- 2L

  run_rcon <- run_plan_rcon()
  run_data <- run_plan_data()
  run_assessment_plan <- plan_from_data(
    run_data,
    run_rcon,
    "baseline_form"
  )
  verification <- run_plan_verified_row()

  conditions <- list(
    argument = tryCatch(
      plan_from_data(data, rcon, NULL),
      error = identity
    ),
    schema = tryCatch(
      plan_from_data(data[0, , drop = FALSE], rcon, "demographics"),
      error = identity
    ),
    project = tryCatch(
      plan_from_data(data, incomplete_rcon, "demographics"),
      error = identity
    ),
    schedule = tryCatch(
      plan_from_data(data, rcon, "demographics", bad_schedule),
      error = identity
    ),
    plan = tryCatch(
      redcapmissing:::.plan_validate_object(malformed_plan),
      error = identity
    ),
    verification = tryCatch(
      run_plan(
        run_assessment_plan,
        run_data,
        run_rcon,
        verified = verification,
        progress = FALSE
      ),
      error = identity
    )
  )

  for (subclass in names(conditions)) {
    expect_s3_class(
      conditions[[subclass]],
      paste0("redcapmissing_error_", subclass)
    )
    expect_s3_class(conditions[[subclass]], "redcapmissing_error")
    expect_s3_class(conditions[[subclass]], "error")
  }
})
