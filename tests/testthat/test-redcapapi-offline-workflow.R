test_that("a genuine offline connection supports helper plan and run workflow", {
  metadata <- data.frame(
    field_name = c("record_id", "started", "value"),
    form_name = "baseline",
    field_type = "text",
    field_label = c("Record ID", "Started", "Value"),
    required_field = c("y", "", "y"),
    stringsAsFactors = FALSE
  )
  project <- data.frame(
    project_id = "1",
    is_longitudinal = "0",
    has_repeating_instruments_or_events = "0",
    stringsAsFactors = FALSE
  )
  rcon <- suppressWarnings(redcapAPI::offlineConnection(
    meta_data = metadata,
    project_info = project,
    repeat_instrument = redcapAPI::REDCAP_REPEAT_INSTRUMENT_STRUCTURE
  ))
  data <- data.frame(
    record_id = c("1", "2"),
    started = c("yes", "yes"),
    value = c("complete", ""),
    stringsAsFactors = FALSE
  )

  instruments <- all_instruments(rcon)
  expect_no_warning(
    extended_schedule <- build_extended_schedule(
      rcon,
      instruments
    )
  )
  plan <- plan_from_data(data, rcon, instruments, extended_schedule)
  result <- run_plan(plan, data, rcon, progress = FALSE)
  summary <- get_summary(result)
  missing <- get_missing(result)

  expect_s3_class(rcon, "redcapOfflineConnection")
  expect_s3_class(plan, "redcapmissing_plan")
  expect_s3_class(result, "redcapmissing")
  expect_identical(plan$assessible_targets$record_id, c("1", "2"))
  expect_identical(instruments, "baseline")
  expect_identical(
    extended_schedule,
    tibble::tibble(
      instrument = "baseline",
      redcap_event_name = NA_character_,
      repeat_instance = NA_integer_
    )
  )
  expect_identical(
    summary$validation_check,
    c(
      "event-row-started", "repeat-instance-row-started",
      "instrument-started", "field-complete"
    )
  )
  expect_identical(summary$failed, c(0L, 0L, 0L, 1L))
  expect_identical(missing$record_id, "2")
  expect_identical(missing$validation_check, "field-complete")
  expect_identical(missing$field_name, "value")
})
