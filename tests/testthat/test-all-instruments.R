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
