test_that("field policy changes only field-complete assessment", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "", start_marker = "started")
  plan <- plan_from_data(data, rcon, "baseline_form")
  baseline <- run_plan(plan, data, rcon, progress = FALSE)
  ignored <- run_plan(
    plan, data, rcon, ignore_fields = "start_marker",
    required_fields = FALSE, progress = FALSE
  )

  expect_identical(
    baseline$target_results[c("record_id", "event_row_started",
      "repeat_instance_row_started", "instrument_started")],
    ignored$target_results[c("record_id", "event_row_started",
      "repeat_instance_row_started", "instrument_started")]
  )
  expect_identical(ignored$target_results$instrument_started, "passed")
})

test_that("an empty field policy reports not applicable", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(
    plan, data, rcon, required_fields = FALSE, exclude_types = NULL,
    ignore_fields = run_plan_metadata()$field_name,
    progress = FALSE
  )
  expect_identical(result$target_results$field_complete, "not applicable")
  expect_identical(result$target_results$fields_assessed, 0L)
  expect_identical(result$target_results$fields_failed, 0L)
  expect_identical(
    result$target_results$field_applicability_reason,
    "no fields remain after field policy"
  )
  field_summary <- result$summary[result$summary$validation_check == "field-complete", ]
  expect_identical(field_summary$status, "not applicable")
  expect_identical(
    field_summary[c("assessed", "passed", "failed")],
    tibble::tibble(assessed = 0L, passed = 0L, failed = 0L)
  )
  expect_true(is.na(field_summary$pass_rate))
  expect_true(is.na(field_summary$fail_rate))
})

test_that("response missingness distinguishes R missing values from literal text", {
  rcon <- run_plan_rcon(); base <- run_plan_data(required_note = "complete")
  plan <- plan_from_data(base, rcon, "baseline_form")
  ignored <- c("record_id", "branch_flag", "checkbox_field", "conditional_note")
  evaluate <- function(value) {
    data <- base; data$required_note <- value
    run_plan(plan, data, rcon, ignore_fields = ignored,
             progress = FALSE)$target_results$field_complete
  }
  expect_identical(vapply(list(NA_character_, "", "   ", NaN), evaluate, character(1)),
                   rep("failed", 4L))
  expect_identical(vapply(list("NA", "N/A", "NULL", ".", "-999", Inf, -Inf),
                          evaluate, character(1)), rep("passed", 7L))
})

test_that("every field policy argument leaves targets and upstream checks invariant", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  runs <- list(
    default = run_plan(plan, data, rcon, progress = FALSE),
    all_fields = run_plan(plan, data, rcon, required_fields = FALSE,
                          exclude_types = NULL, progress = FALSE),
    excluded = run_plan(plan, data, rcon, required_fields = FALSE,
                        exclude_types = c("descriptive", "text"), progress = FALSE),
    ignored = run_plan(plan, data, rcon, required_fields = FALSE,
                       ignore_fields = "optional_note", progress = FALSE)
  )
  invariant <- c(
    "record_id", "instrument", "redcap_event_name", "repeat_instrument",
    "repeat_instance", "target_source", "event_row_started",
    "repeat_instance_row_started", "instrument_started"
  )
  expected <- runs$default$target_results[invariant]
  for (result in runs[-1]) expect_identical(result$target_results[invariant], expected)
})

test_that("typed response missing values fail while nonfinite values remain literal", {
  rcon <- run_plan_rcon(); base <- run_plan_data(required_note = "complete")
  plan <- plan_from_data(base, rcon, "baseline_form")
  ignored <- c("record_id", "branch_flag", "checkbox_field", "conditional_note")
  evaluate <- function(value) {
    data <- base; data$required_note <- value
    run_plan(plan, data, rcon, ignore_fields = ignored,
             progress = FALSE)$target_results$field_complete
  }
  missing_values <- list(factor(NA_character_), as.Date(NA_character_),
                         as.POSIXct(NA_character_), NA, NA_integer_, NA_real_)
  expect_identical(vapply(missing_values, evaluate, character(1)),
                   rep("failed", length(missing_values)))
  expect_identical(vapply(list(Inf, -Inf), evaluate, character(1)),
                   c("passed", "passed"))
})

test_that("explicit exclusions must remain relevant after the required fields filter", {
  rcon <- run_plan_rcon()
  original_metadata <- rcon$metadata()
  metadata <- dplyr::bind_rows(
    original_metadata,
    meta_row("optional_special", "baseline_form", field_type = "special")
  )
  rcon$metadata <- function() metadata
  data <- dplyr::mutate(run_plan_data(), optional_special = "")
  plan <- plan_from_data(data, rcon, "baseline_form")

  expect_error(
    run_plan(
      plan,
      data,
      rcon,
      exclude_types = "special",
      progress = FALSE
    ),
    class = "redcapmissing_error_argument"
  )
  expect_s3_class(
    run_plan(
      plan,
      data,
      rcon,
      required_fields = FALSE,
      exclude_types = "special",
      progress = FALSE
    ),
    "redcapmissing"
  )
  expect_error(
    run_plan(
      plan,
      data,
      rcon,
      required_fields = FALSE,
      exclude_types = "descriptive",
      ignore_fields = "descriptive_text",
      progress = FALSE
    ),
    class = "redcapmissing_error_argument"
  )
})

test_that("the default descriptive exclusion is safe when the type is absent", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "baseline_form", required = "y"),
    meta_row("value", "baseline_form", required = "y")
  )
  rcon <- list(
    metadata = function() metadata,
    instruments = function() tibble::tibble(
      instrument_name = "baseline_form",
      instrument_label = "Baseline form"
    ),
    projectInformation = function() tibble::tibble(
      project_id = 77L,
      is_longitudinal = 0L
    ),
    repeatInstrumentEvent = function() tibble::tibble(
      event_name = character(),
      form_name = character()
    )
  )
  rcon <- redcap_api_connection_fixture(rcon)
  data <- tibble::tibble(record_id = "1", value = "entered")
  plan <- plan_from_data(data, rcon, "baseline_form")

  expect_s3_class(run_plan(plan, data, rcon, progress = FALSE), "redcapmissing")
  expect_error(
    run_plan(
      plan,
      data,
      rcon,
      exclude_types = "descriptive",
      progress = FALSE
    ),
    class = "redcapmissing_error_argument"
  )
})

test_that("field-complete requires every resolved response field", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")

  expect_error(
    run_plan(
      plan,
      data[, names(data) != "descriptive_text", drop = FALSE],
      rcon,
      required_fields = FALSE,
      exclude_types = NULL,
      progress = FALSE
    ),
    regexp = "field-complete assessment",
    class = "redcapmissing_error_schema"
  )
})

test_that("required_fields validates its scalar value", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")

  expect_error(
    run_plan(plan, data, rcon, required_fields = NA, progress = FALSE),
    class = "redcapmissing_error_argument"
  )
})

test_that("ignore_fields validates resolved root field names", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")

  expect_error(
    run_plan(plan, data, rcon, ignore_fields = " unknown ", progress = FALSE),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    run_plan(plan, data, rcon, ignore_fields = "optional_note", progress = FALSE),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    run_plan(
      plan,
      data,
      rcon,
      ignore_fields = "checkbox_field___1",
      progress = FALSE
    ),
    class = "redcapmissing_error_argument"
  )
})

test_that("exclude_types validates active metadata types", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")

  expect_error(
    run_plan(plan, data, rcon, exclude_types = "unused", progress = FALSE),
    class = "redcapmissing_error_argument"
  )
})

test_that("zero-target plans reject irrelevant field policies", {
  rcon <- run_plan_rcon()
  metadata <- rcon$metadata()
  metadata$required_field <- NULL
  rcon$metadata <- function() metadata
  empty_schedule <- run_plan_explicit_schedule()[0, , drop = FALSE]
  plan <- plan_explicit(
    run_plan_data(),
    rcon,
    empty_schedule
  )
  data <- tibble::tibble(record_id = "1")

  expect_error(
    run_plan(
      plan,
      data,
      rcon,
      ignore_fields = "start_marker",
      progress = FALSE
    ),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    run_plan(
      plan,
      data,
      rcon,
      exclude_types = "text",
      progress = FALSE
    ),
    class = "redcapmissing_error_argument"
  )
})

test_that("required_fields rejects non-scalar logical controls", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  invalid <- list(NA, logical(), c(TRUE, FALSE), 1, "TRUE", NULL)

  for (value in invalid) {
    expect_error(
      run_plan(
        plan,
        data,
        rcon,
        required_fields = value,
        progress = FALSE
      ),
      class = "redcapmissing_error_argument"
    )
  }
})

test_that("field-name controls enforce character-vector rules", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  invalid <- list(
    NA_character_, "", " ", " padded", "padded ", c("x", "x"), factor("x"),
    1L, TRUE, list("x"), as.Date("2026-07-25")
  )

  for (argument in c("ignore_fields", "exclude_types")) {
    for (value in invalid) {
      arguments <- list(plan = plan, data = data, rcon = rcon, progress = FALSE)
      arguments[argument] <- list(value)
      expect_error(
        do.call(run_plan, arguments),
        class = "redcapmissing_error_argument"
      )
    }
  }
  expect_s3_class(
    run_plan(
      plan,
      data,
      rcon,
      ignore_fields = character(),
      exclude_types = NULL,
      progress = FALSE
    ),
    "redcapmissing"
  )
})

test_that("checkbox roots fail completeness when no child is selected", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(branch_flag = "0", checkbox_1 = "0", checkbox_2 = "0")
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, progress = FALSE, details = TRUE)
  fields <- result$details[
    result$details$validation_check == "field-complete",
  ]
  checkbox <- fields[fields$field_name == "checkbox_field", ]

  expect_identical(checkbox$effective_disposition, "failed")
})

test_that("ignore_fields rejects fields outside frozen target scope", {
  rcon <- run_plan_target_scope_rcon()
  planner_data <- run_plan_target_scope_data()
  plan <- plan_from_data(
    planner_data,
    rcon,
    c("active_form", "inactive_form")
  )
  runtime_data <- planner_data[
    ,
    !names(planner_data) %in% c(
      "inactive_note",
      "inactive_checkbox___1",
      "inactive_checkbox___2"
    ),
    drop = FALSE
  ]

  expect_error(
    run_plan(
      plan,
      runtime_data,
      rcon,
      ignore_fields = "inactive_note",
      progress = FALSE
    ),
    class = "redcapmissing_error_argument"
  )
})

test_that("exclude_types rejects types outside frozen target scope", {
  rcon <- run_plan_target_scope_rcon()
  planner_data <- run_plan_target_scope_data()
  plan <- plan_from_data(
    planner_data,
    rcon,
    c("active_form", "inactive_form")
  )
  runtime_data <- planner_data[
    ,
    !names(planner_data) %in% c(
      "inactive_note",
      "inactive_checkbox___1",
      "inactive_checkbox___2"
    ),
    drop = FALSE
  ]

  expect_error(
    run_plan(
      plan,
      runtime_data,
      rcon,
      exclude_types = "notes",
      progress = FALSE
    ),
    class = "redcapmissing_error_argument"
  )
})
