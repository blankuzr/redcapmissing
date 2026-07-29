test_that("instrument detection uses the complete independent field set", {
  metadata <- dplyr::bind_rows(
    run_plan_metadata(),
    meta_row("calculated_field", "baseline_form", field_type = "calc")
  )
  detection <- redcapmissing:::.instrument_started_build_detection_plan(
    metadata,
    "baseline_form",
    "record_id"
  )$export_fields$baseline_form

  expect_identical(
    detection,
    c(
      "start_marker",
      "branch_flag",
      "required_note",
      "optional_note",
      "checkbox_field___1",
      "checkbox_field___2",
      "conditional_note"
    )
  )
  expect_false(any(c(
    "record_id", "descriptive_text", "calculated_field"
  ) %in% detection))
})

test_that("checkbox metadata must define unambiguous exported children", {
  missing_choices <- run_plan_rcon()
  metadata <- missing_choices$metadata()
  metadata$select_choices_or_calculations <- NULL
  missing_choices$metadata <- function() metadata
  expect_error(
    plan_from_data(run_plan_data(), missing_choices, "baseline_form"),
    class = "redcapmissing_error_project"
  )

  invalid_choices <- run_plan_rcon()
  metadata <- invalid_choices$metadata()
  metadata$select_choices_or_calculations[
    metadata$field_name == "checkbox_field"
  ] <- "1 First | malformed"
  invalid_choices$metadata <- function() metadata
  expect_error(
    plan_from_data(run_plan_data(), invalid_choices, "baseline_form"),
    class = "redcapmissing_error_project"
  )
})

test_that("checkbox detection requires a selected child", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "checkbox_only", required = "y"),
    meta_row(
      "choices",
      "checkbox_only",
      field_type = "checkbox",
      choices = "1, First | 2, Second",
      required = "y"
    )
  )
  rcon <- list(
    metadata = function() metadata,
    instruments = function() tibble::tibble(
      instrument_name = "checkbox_only",
      instrument_label = "Checkbox only"
    ),
    projectInformation = function() tibble::tibble(
      project_id = "77",
      is_longitudinal = 0L
    ),
    repeatInstrumentEvent = function() tibble::tibble(
      event_name = character(),
      form_name = character()
    )
  )
  rcon <- redcap_api_connection_fixture(rcon)
  assess_start <- function(first, second) {
    data <- tibble::tibble(
      record_id = "1",
      choices___1 = first,
      choices___2 = second
    )
    plan <- plan_from_data(data, rcon, "checkbox_only")
    run_plan(plan, data, rcon, progress = FALSE)$target_results$instrument_started
  }

  for (value in c("0", "unchecked", "FALSE", "No")) {
    expect_identical(assess_start(value, value), "failed")
  }
  expect_identical(assess_start("1", "0"), "passed")
})

test_that("selected instruments require at least one usable start detection field", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "display_only", required = "y"),
    meta_row("instructions", "display_only", field_type = "descriptive"),
    meta_row("calculated", "display_only", field_type = "calc")
  )
  rcon <- list(
    metadata = function() metadata,
    instruments = function() tibble::tibble(
      instrument_name = "display_only",
      instrument_label = "Display only"
    ),
    projectInformation = function() tibble::tibble(
      project_id = "77",
      is_longitudinal = 0L
    ),
    repeatInstrumentEvent = function() tibble::tibble(
      event_name = character(),
      form_name = character()
    )
  )
  rcon <- redcap_api_connection_fixture(rcon)
  data <- tibble::tibble(
    record_id = "1",
    instructions = "",
    calculated = ""
  )
  plan <- plan_from_data(data, rcon, "display_only")

  expect_error(
    run_plan(plan, data, rcon, progress = FALSE),
    class = "redcapmissing_error_project"
  )
})

test_that("instrument-started requires every ordinary field used for detection", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")

  expect_error(
    run_plan(
      plan,
      data[, names(data) != "optional_note", drop = FALSE],
      rcon,
      progress = FALSE
    ),
    class = "redcapmissing_error_schema"
  )
})

test_that("instrument-started requires every exported checkbox child", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")

  expect_error(
    run_plan(
      plan,
      data[, names(data) != "checkbox_field___2", drop = FALSE],
      rcon,
      progress = FALSE
    ),
    regexp = "checkbox_field___2",
    class = "redcapmissing_error_schema"
  )
})
