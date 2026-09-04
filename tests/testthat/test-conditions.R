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
    export = tryCatch(export_data_quality(NULL), error = identity),
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
  warning_rcon <- .plan_longitudinal_rcon()
  warning_metadata <- dplyr::bind_rows(
    .plan_metadata(),
    tibble::tibble(
      field_name = "inactive_value",
      form_name = "inactive_form",
      field_type = "text"
    )
  )
  warning_instruments <- tibble::tibble(
    instrument_name = c("demographics", "notes", "diary", "inactive_form"),
    instrument_label = c(
      "demographics label", "notes label", "diary label", "inactive form label"
    )
  )
  warning_rcon$metadata <- function() warning_metadata
  warning_rcon$instruments <- function() warning_instruments
  undesignated_warning <- tryCatch(
    build_extended_schedule(warning_rcon, "inactive_form"),
    warning = identity
  )
  expect_s3_class(
    undesignated_warning,
    "redcapmissing_warning_undesignated_extension"
  )
  expect_s3_class(undesignated_warning, "redcapmissing_warning")
  expect_s3_class(undesignated_warning, "warning")
  expect_identical(undesignated_warning$instruments, "inactive_form")
})
