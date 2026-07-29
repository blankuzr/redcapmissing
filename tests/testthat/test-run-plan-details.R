test_that("compact and detailed runs have identical assessment results", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  compact <- run_plan(plan, data, rcon, progress = FALSE)
  detailed <- run_plan(plan, data, rcon, details = TRUE, progress = FALSE)
  expect_identical(compact$plan, detailed$plan)
  expect_identical(compact$target_results, detailed$target_results)
  expect_identical(compact$summary, detailed$summary)
  expect_identical(compact$missing, detailed$missing)
  expect_identical(compact$verification, detailed$verification)
  expect_identical(
    compact$diagnostics[setdiff(names(compact$diagnostics), "elapsed_seconds")],
    detailed$diagnostics[setdiff(names(detailed$diagnostics), "elapsed_seconds")]
  )
  expect_null(compact$details)
  expect_true(is.data.frame(detailed$details))
})

test_that("details have exact target and field row cardinality and values", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(
    required_note = 42,
    checkbox_1 = "0",
    checkbox_2 = "0"
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, details = TRUE, progress = FALSE)
  target_rows <- result$details[
    result$details$validation_check != "field-complete",
  ]
  fields <- result$details[
    result$details$validation_check == "field-complete",
  ]

  expect_identical(
    target_rows$validation_check,
    c(
      "event-row-started", "repeat-instance-row-started",
      "instrument-started"
    )
  )
  expect_identical(
    target_rows$raw_disposition,
    c("not applicable", "not applicable", "passed")
  )
  expect_identical(
    target_rows$effective_disposition,
    target_rows$raw_disposition
  )
  expect_identical(
    target_rows$reason,
    c(
      "not applicable for classic project", "not a repeating target",
      NA_character_
    )
  )
  for (column in c(
    "field_name", "field_label", "field_type", "branching_logic",
    "value_summary"
  )) {
    expect_type(target_rows[[column]], "character")
    expect_true(all(is.na(target_rows[[column]])), info = column)
  }
  expect_type(target_rows$branch_satisfied, "logical")
  expect_true(all(is.na(target_rows$branch_satisfied)))

  expect_identical(nrow(fields), result$target_results$fields_assessed)
  expect_identical(
    fields$field_name,
    c("record_id", "branch_flag", "required_note", "checkbox_field")
  )
  expect_false("conditional_note" %in% fields$field_name)
  expect_true(all(fields$branch_satisfied))
  expect_identical(fields$value_summary, c("1", "0", "42", ""))
  expect_identical(
    fields$raw_disposition,
    c("passed", "passed", "passed", "failed")
  )
  expect_identical(fields$effective_disposition, fields$raw_disposition)
  expect_true(all(is.na(fields$reason)))
  expect_false(any(fields$verification_applied))
  expect_identical(
    result$target_results$fields_failed,
    sum(fields$effective_disposition == "failed")
  )

  selected <- run_plan_data(
    required_note = "synthetic free text",
    checkbox_1 = "1",
    checkbox_2 = "1"
  )
  selected_details <- run_plan(
    plan,
    selected,
    rcon,
    details = TRUE,
    progress = FALSE
  )$details
  expect_identical(
    selected_details$value_summary[
      selected_details$field_name %in% "checkbox_field"
    ],
    "checkbox_field___1, checkbox_field___2"
  )

  dated <- run_plan_data(required_note = as.Date("2026-01-02"))
  dated_details <- run_plan(
    plan,
    dated,
    rcon,
    details = TRUE,
    progress = FALSE
  )$details
  expect_identical(
    dated_details$value_summary[
      dated_details$field_name %in% "required_note"
    ],
    "2026-01-02"
  )
})

test_that("field details retain target provenance for instruments in one context", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "alpha", required = "y"),
    meta_row("alpha_value", "alpha", required = "y"),
    meta_row("beta_value", "beta", required = "y")
  )
  instruments <- tibble::tibble(
    instrument_name = c("alpha", "beta"),
    instrument_label = c("Alpha", "Beta")
  )
  info <- tibble::tibble(project_id = "77", is_longitudinal = 0L)
  repeat_table <- tibble::tibble(event_name = character(), form_name = character())
  rcon <- list(
    url = "https://example.test/api/", metadata = function() metadata,
    instruments = function() instruments, projectInformation = function() info,
    repeatInstrumentEvent = function() repeat_table,
    version = function() "15.0.0"
  )
  rcon <- redcap_api_connection_fixture(rcon)
  data <- tibble::tibble(record_id = "1", alpha_value = "a", beta_value = "b")
  extension <- tibble::tibble(
    instrument = "alpha", redcap_event_name = NA_character_,
    repeat_instance = NA_integer_
  )
  plan <- plan_from_data(data, rcon, c("alpha", "beta"), extension)
  result <- run_plan(plan, data, rcon, required_fields = FALSE,
                     exclude_types = NULL, details = TRUE, progress = FALSE)
  fields <- result$details[result$details$validation_check == "field-complete", ]
  expect_identical(unique(fields$instrument), c("alpha", "beta"))
  expect_identical(unique(fields$target_source[fields$instrument == "alpha"]), "observed+extended")
  expect_identical(unique(fields$target_source[fields$instrument == "beta"]), "observed")
})

test_that("verification results agree across detail settings", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  issue <- run_plan_verified_row()
  ignored <- c("record_id", "branch_flag", "checkbox_field", "conditional_note")
  run <- function(details) run_plan(
    plan,
    data,
    rcon,
    ignore_fields = ignored,
    verified = issue,
    verified_user = "alice",
    details = details,
    progress = FALSE
  )

  compact <- run(FALSE)
  detailed <- run(TRUE)

  expect_identical(compact$plan, detailed$plan)
  expect_identical(compact$target_results, detailed$target_results)
  expect_identical(compact$summary, detailed$summary)
  expect_identical(compact$missing, detailed$missing)
  expect_identical(compact$verification, detailed$verification)
  expect_identical(
    compact$diagnostics[setdiff(names(compact$diagnostics), "elapsed_seconds")],
    detailed$diagnostics[setdiff(names(detailed$diagnostics), "elapsed_seconds")]
  )
  expect_null(compact$details)
  expect_true(is.data.frame(detailed$details))
  expect_identical(compact$verification$overrides_applied, 1L)
})

test_that("details are returned as a data frame when requested", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, progress = FALSE, details = TRUE)

  expect_true(is.data.frame(result$details))
})

test_that("zero-row targets retain typed detail dispositions and reasons", {
  rcon <- run_plan_rcon()
  planner_data <- run_plan_data()
  plan <- plan_explicit(
    planner_data,
    rcon,
    "baseline_form",
    run_plan_explicit_schedule("absent")
  )
  result <- run_plan(
    plan,
    planner_data[0, , drop = FALSE],
    rcon,
    details = TRUE,
    progress = FALSE
  )

  expect_identical(
    result$details$validation_check,
    c(
      "event-row-started", "repeat-instance-row-started",
      "instrument-started", "field-complete"
    )
  )
  expect_identical(
    result$details$raw_disposition,
    c("not applicable", "not applicable", "failed", "not reached")
  )
  expect_identical(
    result$details$reason,
    c(
      "not applicable for classic project", "not a repeating target",
      NA_character_, NA_character_
    )
  )
  expect_true(all(is.na(result$details$field_name)))
  expect_type(result$details$field_name, "character")
  expect_true(all(is.na(result$details$branch_satisfied)))
  expect_type(result$details$branch_satisfied, "logical")
  expect_true(all(is.na(result$details$value_summary)))
  expect_type(result$details$value_summary, "character")
})

test_that("field-complete details align record_id and target_source with passing and failing dispositions", {
  rcon <- run_plan_rcon()
  data <- dplyr::bind_rows(
    run_plan_data(record_id = "pass", required_note = "complete"),
    run_plan_data(record_id = "fail", required_note = "")
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(
    plan,
    data,
    rcon,
    ignore_fields = c(
      "record_id", "branch_flag", "checkbox_field", "conditional_note"
    ),
    details = TRUE,
    progress = FALSE
  )
  field_details <- result$details[
    result$details$validation_check == "field-complete",
  ]

  expect_identical(
    field_details[c(
      "record_id", "target_source", "raw_disposition",
      "effective_disposition"
    )],
    tibble::tibble(
      record_id = c("fail", "pass"),
      target_source = c("observed", "observed"),
      raw_disposition = c("failed", "passed"),
      effective_disposition = c("failed", "passed")
    )
  )
})

test_that("failed field details retain record and target provenance", {
  rcon <- run_plan_rcon()
  data <- dplyr::bind_rows(
    run_plan_data(record_id = "one", required_note = ""),
    run_plan_data(record_id = "two", required_note = "")
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(
    plan,
    data,
    rcon,
    ignore_fields = c(
      "record_id", "branch_flag", "checkbox_field", "conditional_note"
    ),
    details = TRUE,
    progress = FALSE
  )
  failed_details <- result$details[
    result$details$validation_check == "field-complete" &
      result$details$effective_disposition == "failed",
  ]

  expect_setequal(failed_details$record_id, c("one", "two"))
  expect_true(all(failed_details$target_source == "observed"))
})
