test_that("fields closed by branching report not applicable", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(branch_flag = "0", start_marker = "started")
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(
    plan,
    data,
    rcon,
    ignore_fields = c(
      "record_id", "branch_flag", "required_note", "checkbox_field"
    ),
    details = TRUE,
    progress = FALSE
  )

  expect_identical(result$target_results$field_complete, "not applicable")
  expect_identical(result$target_results$fields_assessed, 0L)
  expect_identical(result$target_results$fields_failed, 0L)
  expect_identical(
    result$target_results$field_applicability_reason,
    "no fields apply after branching logic"
  )
  field_summary <- result$summary[
    result$summary$validation_check == "field-complete",
  ]
  expect_identical(field_summary$status, "not applicable")
  expect_identical(
    field_summary$reason,
    "no fields apply after branching logic"
  )
  expect_identical(
    field_summary[c("assessed", "passed", "failed")],
    tibble::tibble(assessed = 0L, passed = 0L, failed = 0L)
  )
  expect_true(is.na(field_summary$pass_rate))
  expect_true(is.na(field_summary$fail_rate))
})

test_that("closed branches are excluded from field completeness", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(branch_flag = "0", checkbox_1 = "0", checkbox_2 = "0")
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, progress = FALSE, details = TRUE)
  fields <- result$details[result$details$validation_check == "field-complete", ]

  expect_false("conditional_note" %in% fields$field_name)
})

test_that("malformed branching logic raises a package project condition", {
  rcon <- run_plan_rcon()
  metadata <- rcon$metadata()
  metadata$branching_logic[metadata$field_name == "conditional_note"] <-
    "[branch_flag] ="
  rcon$metadata <- function() metadata
  data <- run_plan_data(branch_flag = "1")
  plan <- plan_from_data(data, rcon, "baseline_form")

  expect_error(
    run_plan(plan, data, rcon, progress = FALSE),
    regexp = "branching logic",
    class = "redcapmissing_error_project"
  )
})

run_plan_cross_event_fixture <- function() {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "baseline", required = "y"),
    meta_row("trigger", "baseline"),
    meta_row("follow_start", "followup"),
    meta_row(
      "conditional",
      "followup",
      branching = "[baseline_arm_1][trigger] = '1'",
      required = "y"
    )
  )
  rcon <- list(
    metadata = function() metadata,
    instruments = function() tibble::tibble(
      instrument_name = c("baseline", "followup"),
      instrument_label = c("Baseline", "Follow up")
    ),
    projectInformation = function() tibble::tibble(
      project_id = "77",
      is_longitudinal = 1L
    ),
    arms = function() tibble::tibble(arm_num = 1L, name = "Arm 1"),
    events = function() tibble::tibble(
      event_id = c(101L, 102L),
      unique_event_name = c("baseline_arm_1", "followup_arm_1"),
      event_name = c("Baseline", "Follow up"),
      arm_num = c(1L, 1L)
    ),
    mapping = function() tibble::tibble(
      arm_num = c(1L, 1L),
      unique_event_name = c("baseline_arm_1", "followup_arm_1"),
      form = c("baseline", "followup")
    ),
    repeatInstrumentEvent = function() tibble::tibble(
      event_name = character(),
      form_name = character()
    )
  )
  data <- tibble::tibble(
    record_id = c("1", "1"),
    redcap_event_name = c("baseline_arm_1", "followup_arm_1"),
    trigger = c("1", ""),
    follow_start = c("", "started"),
    conditional = c("", "")
  )
  list(
    metadata = metadata,
    rcon = redcap_api_connection_fixture(rcon),
    data = data
  )
}

run_plan_repeated_source_fixture <- function() {
  fixture <- run_plan_cross_event_fixture()
  metadata <- dplyr::bind_rows(
    fixture$metadata,
    meta_row("repeat_value", "source_repeat")
  )
  rcon <- fixture$rcon
  rcon$metadata <- function() metadata
  rcon$instruments <- function() tibble::tibble(
    instrument_name = c("baseline", "source_repeat", "followup"),
    instrument_label = c("Baseline", "Source repeat", "Follow up")
  )
  base_mapping <- fixture$rcon$mapping
  rcon$mapping <- function() dplyr::bind_rows(
    base_mapping(),
    tibble::tibble(
      arm_num = 1L,
      unique_event_name = "baseline_arm_1",
      form = "source_repeat"
    )
  )
  rcon$repeatInstrumentEvent <- function() tibble::tibble(
    event_name = "baseline_arm_1",
    form_name = "source_repeat"
  )

  data_without_repeat_instance <- fixture$data
  data_without_repeat_instance$redcap_repeat_instrument <- NA_character_
  data_without_repeat_instance$redcap_repeat_instance <- NA_integer_
  data_without_repeat_instance$repeat_value <- ""
  data_with_repeat_instance <- data_without_repeat_instance[
    1L,
    ,
    drop = FALSE
  ][rep.int(1L, 2L), ]
  data_with_repeat_instance$trigger <- "0"
  data_with_repeat_instance$redcap_repeat_instrument <- "source_repeat"
  data_with_repeat_instance$redcap_repeat_instance <- 1:2
  data_with_repeat_instance$repeat_value <- c("first", "second")

  list(
    rcon = rcon,
    data = dplyr::bind_rows(
      data_with_repeat_instance,
      data_without_repeat_instance
    )
  )
}

test_that("cross-event branching uses the matching record and event context", {
  fixture <- run_plan_cross_event_fixture()
  plan <- plan_from_data(fixture$data, fixture$rcon, "followup")

  open <- run_plan(
    plan,
    fixture$data,
    fixture$rcon,
    details = TRUE,
    progress = FALSE
  )
  expect_identical(open$target_results$event_row_started, "passed")
  expect_identical(open$target_results$instrument_started, "passed")
  expect_identical(open$target_results$field_complete, "failed")
  conditional <- open$details[
    open$details$validation_check == "field-complete",
  ]
  expect_identical(conditional$field_name, "conditional")
  expect_true(conditional$branch_satisfied)

  closed_data <- fixture$data
  closed_data$trigger[[1L]] <- "0"
  closed <- run_plan(
    plan,
    closed_data,
    fixture$rcon,
    details = TRUE,
    progress = FALSE
  )
  expect_identical(closed$target_results$event_row_started, "passed")
  expect_identical(closed$target_results$instrument_started, "passed")
  expect_identical(closed$target_results$field_complete, "not applicable")
  expect_identical(
    closed$target_results$field_applicability_reason,
    "no fields apply after branching logic"
  )
})

test_that("cross-event branching requires every referenced source field", {
  fixture <- run_plan_cross_event_fixture()
  plan <- plan_from_data(fixture$data, fixture$rcon, "followup")
  missing_dependency <- fixture$data[
    ,
    names(fixture$data) != "trigger",
    drop = FALSE
  ]

  expect_error(
    run_plan(plan, missing_dependency, fixture$rcon, progress = FALSE),
    regexp = "branching logic evaluation",
    class = "redcapmissing_error_schema"
  )
})

test_that("cross-event branching preserves vectorized record order", {
  fixture <- run_plan_cross_event_fixture()
  record_n <- 60L
  data <- fixture$data[
    rep(seq_len(nrow(fixture$data)), times = record_n),
    ,
    drop = FALSE
  ]
  data$record_id <- rep(sprintf("%03d", seq_len(record_n)), each = 2L)
  baseline_rows <- seq.int(1L, nrow(data), by = 2L)
  followup_rows <- baseline_rows + 1L
  trigger <- rep(c("1", "0"), length.out = record_n)
  data$trigger <- ""
  data$trigger[baseline_rows] <- trigger
  data$follow_start <- ""
  data$follow_start[followup_rows] <- "started"
  plan <- plan_from_data(data, fixture$rcon, "followup")

  result <- run_plan(
    plan,
    data,
    fixture$rcon,
    details = FALSE,
    progress = FALSE
  )
  expect_identical(result$target_results$record_id, sprintf(
    "%03d", seq_len(record_n)
  ))
  expect_true(all(result$target_results$instrument_started == "passed"))
  expect_identical(
    result$target_results$field_complete,
    ifelse(trigger == "1", "failed", "not applicable")
  )
  expect_identical(
    result$target_results$fields_assessed,
    as.integer(trigger == "1")
  )
})

test_that("unqualified repeated sources resolve one eligible source row", {
  fixture <- run_plan_repeated_source_fixture()
  plan <- plan_from_data(fixture$data, fixture$rcon, "followup")
  resolved <- run_plan(
    plan,
    fixture$data,
    fixture$rcon,
    details = TRUE,
    progress = FALSE
  )
  expect_identical(resolved$target_results$field_complete, "failed")
  resolved_field <- resolved$details[
    resolved$details$validation_check == "field-complete",
    ,
    drop = FALSE
  ]
  expect_true(resolved_field$branch_satisfied)

  sole_repeated_data <- fixture$data[!(
    fixture$data$redcap_event_name == "baseline_arm_1" &
      (
        is.na(fixture$data$redcap_repeat_instance) |
          fixture$data$redcap_repeat_instance == 2L
      )
  ), , drop = FALSE]
  sole_repeated_plan <- plan_from_data(
    sole_repeated_data,
    fixture$rcon,
    "followup"
  )
  sole_repeated <- run_plan(
    sole_repeated_plan,
    sole_repeated_data,
    fixture$rcon,
    progress = FALSE
  )
  expect_identical(
    sole_repeated$target_results$field_complete,
    "not applicable"
  )
})

test_that("ambiguous unqualified repeated sources fail closed", {
  fixture <- run_plan_repeated_source_fixture()
  ambiguous_data <- fixture$data[!(
    fixture$data$redcap_event_name == "baseline_arm_1" &
      is.na(fixture$data$redcap_repeat_instance)
  ), , drop = FALSE]
  plan <- plan_from_data(ambiguous_data, fixture$rcon, "followup")

  expect_error(
    run_plan(
      plan,
      ambiguous_data,
      fixture$rcon,
      progress = FALSE
    ),
    regexp = "unqualified event reference is ambiguous",
    class = "redcapmissing_error_project"
  )
})

test_that("shared branching plans preserve vectorized results and row order", {
  rcon <- run_plan_rcon()
  record_n <- 80L
  data <- run_plan_data()[rep.int(1L, record_n), , drop = FALSE]
  data$record_id <- sprintf("%03d", seq_len(record_n))
  data$branch_flag <- rep(c("0", "1"), length.out = record_n)
  data$conditional_note <- ""
  plan <- plan_from_data(data, rcon, "baseline_form")

  result <- run_plan(
    plan,
    data,
    rcon,
    details = TRUE,
    progress = FALSE
  )
  expect_identical(
    result$target_results$field_complete,
    ifelse(data$branch_flag == "1", "failed", "passed")
  )
  field_details <- result$details[
    result$details$validation_check == "field-complete",
    ,
    drop = FALSE
  ]
  detail_record_order <- match(field_details$record_id, data$record_id)
  expect_false(is.unsorted(detail_record_order))
})
