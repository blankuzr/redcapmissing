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

  printed <- capture.output(plan_print <- withVisible(print(plan)))
  printed_text <- paste(printed, collapse = "\n")
  expect_match(printed[[1]], "<redcapmissing_plan>", fixed = TRUE)
  expect_match(printed_text, "Construction: from_data", fixed = TRUE)
  expect_match(printed_text, "Instruments:  2", fixed = TRUE)
  expect_match(printed_text, "Targets:      4", fixed = TRUE)
  expect_false(any(grepl("01|02", printed)))
  expect_false(plan_print$visible)
  expect_identical(plan_print$value, plan)
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
    plan_explicit(data, rcon),
    class = "redcapmissing_error_argument"
  )
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
    empty_schedule
  )
  invalid_empty_scope <- empty_plan
  invalid_empty_scope$instruments <- "demographics"
  expect_error(
    redcapmissing:::.plan_validate_object(invalid_empty_scope),
    class = "redcapmissing_error_plan"
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
