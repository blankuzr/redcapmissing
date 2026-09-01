test_that("all_instruments returns the complete ordered project inventory", {
  fixture <- .schedule_helper_connection()

  expect_identical(all_instruments(fixture$rcon), fixture$instruments)
  expect_identical(fixture$calls$instruments, 1L)
  unrelated <- c(
    "metadata", "projectInformation", "repeatInstrumentEvent",
    "mapping", "events", "arms", "exportRecords"
  )
  expect_identical(
    vapply(unrelated, function(surface) fixture$calls[[surface]], integer(1)),
    stats::setNames(rep(0L, length(unrelated)), unrelated)
  )

  expect_error(all_instruments(), class = "redcapmissing_error_argument")
  expect_error(all_instruments(NULL), class = "redcapmissing_error_argument")
  expect_error(
    all_instruments(structure(list(), class = "redcapConnection")),
    class = "redcapmissing_error_project"
  )

  malformed <- .schedule_helper_connection()
  malformed$rcon$instruments <- function() {
    tibble::tibble(instrument_name = c("baseline", "baseline"))
  }
  expect_error(
    all_instruments(malformed$rcon),
    class = "redcapmissing_error_project"
  )
})

test_that("all_instruments ignores display labels but keeps raw names strict", {
  padded_labels <- .schedule_helper_connection()
  instrument_names <- padded_labels$instruments
  padded_labels$rcon$instruments <- function() {
    tibble::tibble(
      instrument_name = instrument_names,
      instrument_label = c(
        " Baseline ", "  ", NA_character_,
        "Event  form", " Inactive", "Retired "
      )
    )
  }
  expect_identical(
    all_instruments(padded_labels$rcon),
    instrument_names
  )

  malformed_names <- list(
    c("baseline", " baseline"),
    c("baseline", ""),
    c("baseline", NA_character_),
    c("baseline", "baseline")
  )
  for (instrument_name in malformed_names) {
    malformed <- .schedule_helper_connection()
    malformed$rcon$instruments <- function() {
      tibble::tibble(instrument_name = instrument_name)
    }
    expect_error(
      all_instruments(malformed$rcon),
      class = "redcapmissing_error_project"
    )
  }
})

test_that("planning trims optional project display labels and applies fallbacks", {
  rcon <- .plan_longitudinal_rcon()
  instruments <- rcon$instruments()
  instruments$instrument_label <- c(" Demographics ", "   ", "Diary  form")
  events <- rcon$events()
  events$event_name <- c(" Baseline ", "   ", " Comparator baseline ")
  arms <- rcon$arms()
  arms$name <- c(" Treatment  arm ", "   ")
  rcon$instruments <- function() instruments
  rcon$events <- function() events
  rcon$arms <- function() arms

  snapshot <- redcapmissing:::.project_structure_build_snapshot(rcon)
  plan <- plan_from_data(
    .plan_longitudinal_data(),
    rcon,
    c("demographics", "notes", "diary")
  )

  expect_identical(
    unname(plan$project$instrument_labels[c("demographics", "diary", "notes")]),
    c("Demographics", "Diary  form", "notes")
  )
  expect_identical(
    unname(plan$project$event_labels[c(
      "baseline_arm_1", "baseline_arm_2", "visit_arm_1"
    )]),
    c("Baseline", "Comparator baseline", "visit_arm_1")
  )
  expect_identical(snapshot$arms$arm_name, c("Treatment  arm", NA_character_))
})
