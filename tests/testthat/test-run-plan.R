test_that("run_plan exposes the exact plan execution API and report schemas", {
  expect_identical(names(formals(run_plan)), c(
    "plan", "data", "rcon", "required_fields", "ignore_fields",
    "exclude_types", "verified", "verified_user", "details", "progress"
  ))
  expect_identical(
    as.list(formals(run_plan)),
    alist(
      plan =,
      data =,
      rcon =,
      required_fields = TRUE,
      ignore_fields = NULL,
      exclude_types = "descriptive",
      verified = NULL,
      verified_user = NULL,
      details = FALSE,
      progress = interactive()
    )
  )
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, progress = FALSE, details = TRUE)

  expect_s3_class(result, "redcapmissing")
  expect_identical(names(result), c(
    "plan", "target_results", "summary", "missing", "verification",
    "diagnostics", "details"
  ))
  expect_identical(names(result$target_results), c(
    "record_id", "instrument", "redcap_event_name", "repeat_instrument",
    "repeat_instance", "target_source", "event_row_started",
    "repeat_instance_row_started", "instrument_started", "field_complete",
    "fields_assessed", "fields_failed", "field_applicability_reason"
  ))
  expect_identical(names(result$summary), c(
    "redcap_event_name", "instrument", "repeat_instrument", "repeat_instance",
    "validation_level", "validation_check", "status", "reason", "assessed",
    "passed", "failed", "pass_rate", "fail_rate"
  ))
  expect_identical(names(result$missing), c(
    "record_id", "redcap_event_name", "repeat_instrument", "repeat_instance",
    "validation_context", "instrument", "validation_check", "field_name",
    "field_label", "field_type", "branching_logic", "url"
  ))
  event_summary <- result$summary[result$summary$validation_check == "event-row-started", ]
  expect_identical(event_summary$status, "not applicable")
  expect_identical(event_summary$reason, "not applicable for classic project")
  expect_identical(result$diagnostics$stage, 1:12)
  expect_true(all(result$diagnostics$completed))
  expect_identical(result$diagnostics$operation, c(
    "Validate plan, data, and rcon",
    "Validate and normalize verification",
    "Resolve instrument-start fields",
    "Resolve field-complete fields",
    "Join Assessible targets to physical rows",
    "Run event-row-started",
    "Run repeat-instance-row-started",
    "Run instrument-started",
    "Run raw field-complete",
    "Apply verification",
    "Aggregate effective results",
    "Construct the report"
  ))
  expect_identical(
    result$summary$validation_check,
    c(
      "event-row-started",
      "repeat-instance-row-started",
      "instrument-started",
      "field-complete"
    )
  )
  expect_true(is.data.frame(result$details))
})

test_that("run_plan retains no source data, verification rows, connection, or token", {
  rcon <- run_plan_rcon()
  rcon$token <- "DO_NOT_RETAIN_THIS_TOKEN"
  data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  verified <- run_plan_verified_row()
  result <- run_plan(
    plan,
    data,
    rcon,
    verified = verified,
    verified_user = "alice",
    details = TRUE,
    progress = FALSE
  )

  expect_false(any(vapply(result, identical, logical(1), data)))
  expect_false(any(vapply(result, identical, logical(1), verified)))
  expect_false(any(vapply(result, identical, logical(1), rcon)))
  expect_false(any(c("data", "rcon", "token", "verified") %in% names(result)))
  serialized <- serialize(result, connection = NULL)
  token <- charToRaw("DO_NOT_RETAIN_THIS_TOKEN")
  windows <- if (length(serialized) < length(token)) integer() else
    seq_len(length(serialized) - length(token) + 1L)
  contains_token <- any(vapply(
    windows,
    function(start) identical(
      serialized[seq.int(start, length.out = length(token))],
      token
    ),
    logical(1)
  ))
  expect_false(contains_token)
})

test_that("run_plan rejects malformed plans, changed projects, and runtime structure", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  malformed <- list(
    schema = plan,
    fingerprint = plan,
    duplicates = plan
  )
  malformed$schema$schema_version <- 2L
  malformed$fingerprint$structure_fingerprint <- paste0(
    substr(plan$structure_fingerprint, 1L, 63L),
    if (substr(plan$structure_fingerprint, 64L, 64L) == "0") "1" else "0"
  )
  malformed$duplicates$assessible_targets <- dplyr::bind_rows(
    plan$assessible_targets,
    plan$assessible_targets
  )
  for (candidate in malformed) {
    expect_error(
      run_plan(candidate, data, rcon, progress = FALSE),
      class = "redcapmissing_error_plan"
    )
  }

  changed <- run_plan_rcon()
  info <- changed$projectInformation()
  info$project_id <- "88"
  changed$projectInformation <- function() info
  expect_error(
    run_plan(plan, data, changed, progress = FALSE),
    class = "redcapmissing_error_plan"
  )

  invalid_data <- data
  invalid_data$record_id <- " 1"
  expect_error(
    run_plan(plan, invalid_data, rcon, progress = FALSE),
    class = "redcapmissing_error_schema"
  )
})
test_that("newer data snapshots cannot add targets and absent planned rows still fail", {
  rcon <- run_plan_rcon()
  planned_data <- run_plan_data(record_id = "1")
  plan <- plan_from_data(planned_data, rcon, "baseline_form")
  newer_data <- dplyr::bind_rows(
    planned_data,
    run_plan_data(record_id = "2")
  )

  newer <- run_plan(plan, newer_data, rcon, progress = FALSE)
  expect_identical(newer$target_results$record_id, "1")
  expect_identical(nrow(newer$target_results), 1L)

  absent <- run_plan(
    plan,
    run_plan_data(record_id = "2"),
    rcon,
    progress = FALSE
  )
  expect_identical(absent$target_results$record_id, "1")
  expect_identical(absent$target_results$instrument_started, "failed")
  expect_identical(absent$target_results$field_complete, "not reached")
})

test_that("run_plan freezes targets and gates absent physical rows", {
  rcon <- run_plan_rcon()
  data <- run_plan_data("1")
  plan <- plan_explicit(data, rcon, "baseline_form", run_plan_explicit_schedule("2"))
  result <- run_plan(plan, data, rcon, progress = FALSE)

  expect_identical(result$target_results$record_id, "2")
  expect_identical(result$target_results$event_row_started, "not applicable")
  expect_identical(result$target_results$repeat_instance_row_started, "not applicable")
  expect_identical(result$target_results$instrument_started, "failed")
  expect_identical(result$target_results$field_complete, "not reached")
  expect_true("instrument-started" %in% result$missing$validation_check)
})

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

test_that("an empty field policy is not applicable rather than complete", {
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
    "no assessible fields after field policy"
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

test_that("all branch-closed fields are not applicable rather than complete", {
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
    "no assessible fields after branching logic"
  )
  field_summary <- result$summary[
    result$summary$validation_check == "field-complete",
  ]
  expect_identical(field_summary$status, "not applicable")
  expect_identical(
    field_summary$reason,
    "no assessible fields after branching logic"
  )
  expect_identical(
    field_summary[c("assessed", "passed", "failed")],
    tibble::tibble(assessed = 0L, passed = 0L, failed = 0L)
  )
  expect_true(is.na(field_summary$pass_rate))
  expect_true(is.na(field_summary$fail_rate))
})

test_that("branching and checkbox roots retain REDCap completeness semantics", {
  rcon <- run_plan_rcon()
  data <- run_plan_data(branch_flag = "0", checkbox_1 = "0", checkbox_2 = "0")
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, progress = FALSE, details = TRUE)
  fields <- result$details[result$details$validation_check == "field-complete", ]

  expect_false("conditional_note" %in% fields$field_name)
  checkbox <- fields[fields$field_name == "checkbox_field", ]
  expect_identical(checkbox$effective_disposition, "failed")
})

test_that("cross-event branching uses the matching record and event context", {
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
      instrument_label = c("Baseline", "Follow-up")
    ),
    projectInformation = function() tibble::tibble(
      project_id = "77",
      is_longitudinal = 1L
    ),
    arms = function() tibble::tibble(arm_num = 1L, name = "Arm 1"),
    events = function() tibble::tibble(
      event_id = c(101L, 102L),
      unique_event_name = c("baseline_arm_1", "followup_arm_1"),
      event_name = c("Baseline", "Follow-up"),
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
  plan <- plan_from_data(data, rcon, "followup")

  open <- run_plan(plan, data, rcon, details = TRUE, progress = FALSE)
  expect_identical(open$target_results$event_row_started, "passed")
  expect_identical(open$target_results$instrument_started, "passed")
  expect_identical(open$target_results$field_complete, "failed")
  conditional <- open$details[
    open$details$validation_check == "field-complete",
  ]
  expect_identical(conditional$field_name, "conditional")
  expect_true(conditional$branch_satisfied)

  closed_data <- data
  closed_data$trigger[[1L]] <- "0"
  closed <- run_plan(plan, closed_data, rcon, details = TRUE, progress = FALSE)
  expect_identical(closed$target_results$event_row_started, "passed")
  expect_identical(closed$target_results$instrument_started, "passed")
  expect_identical(closed$target_results$field_complete, "not applicable")
  expect_identical(
    closed$target_results$field_applicability_reason,
    "no assessible fields after branching logic"
  )

  missing_dependency <- data[, names(data) != "trigger", drop = FALSE]
  expect_error(
    run_plan(plan, missing_dependency, rcon, progress = FALSE),
    regexp = "branching-logic evaluation",
    class = "redcapmissing_error_schema"
  )
})

test_that("instrument detection uses the complete independent field set", {
  metadata <- dplyr::bind_rows(
    run_plan_metadata(),
    meta_row("calculated_field", "baseline_form", field_type = "calc")
  )
  detection <- redcapmissing:::.rcm_run_detection_plan(
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

test_that("instrument detection requires every exported detection column", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  expect_error(
    run_plan(plan, data[, names(data) != "optional_note"], rcon, progress = FALSE),
    class = "redcapmissing_error_schema"
  )
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

test_that("planner and runner data reject non-atomic response columns", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  list_data <- data
  list_data$required_note <- I(list("complete"))

  expect_error(
    plan_from_data(list_data, rcon, "baseline_form"),
    class = "redcapmissing_error_schema"
  )
  expect_error(
    run_plan(plan, list_data, rcon, progress = FALSE),
    class = "redcapmissing_error_schema"
  )

  duplicate_data <- data
  duplicate_data$first_duplicate <- "a"
  duplicate_data$second_duplicate <- "b"
  names(duplicate_data)[(ncol(duplicate_data) - 1L):ncol(duplicate_data)] <-
    c("duplicate", "duplicate")
  expect_error(
    plan_from_data(duplicate_data, rcon, "baseline_form"),
    class = "redcapmissing_error_schema"
  )
  expect_error(
    run_plan(plan, duplicate_data, rcon, progress = FALSE),
    class = "redcapmissing_error_schema"
  )
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

test_that("selected instruments require at least one usable start-detection field", {
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

test_that("run_plan validates scalar and named field-policy arguments", {
  rcon <- run_plan_rcon(); data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  expect_error(run_plan(plan, data, rcon, required_fields = NA, progress = FALSE),
               class = "redcapmissing_error_argument")
  expect_error(run_plan(plan, data, rcon, ignore_fields = " unknown ", progress = FALSE),
               class = "redcapmissing_error_argument")
  expect_error(run_plan(plan, data, rcon, exclude_types = "unused", progress = FALSE),
               class = "redcapmissing_error_argument")
  expect_error(run_plan(plan, data, rcon, ignore_fields = "optional_note", progress = FALSE),
               class = "redcapmissing_error_argument")
  expect_error(
    run_plan(plan, data, rcon, ignore_fields = "checkbox_field___1", progress = FALSE),
    class = "redcapmissing_error_argument"
  )
  expect_error(
    run_plan(plan, data, rcon, progress = NA),
    class = "redcapmissing_error_argument"
  )
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

test_that("compact and detailed runs have identical assessment results", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  compact <- run_plan(plan, data, rcon, progress = FALSE)
  detailed <- run_plan(plan, data, rcon, details = TRUE, progress = FALSE)
  expect_identical(compact$target_results, detailed$target_results)
  expect_identical(compact$summary, detailed$summary)
  expect_identical(compact$missing, detailed$missing)
  expect_null(compact$details)
  expect_true(is.data.frame(detailed$details))
})

test_that("zero-target explicit plans return typed empty report tables", {
  rcon <- run_plan_rcon(); data <- run_plan_data()
  schedule <- run_plan_explicit_schedule()[0, ]
  plan <- plan_explicit(data, rcon, "baseline_form", schedule)
  result <- run_plan(plan, data, rcon, progress = FALSE)
  expect_equal(nrow(result$target_results), 0L)
  expect_equal(nrow(result$summary), 0L)
  expect_equal(nrow(result$missing), 0L)
  expect_type(result$target_results$repeat_instance, "integer")
  expect_type(result$summary$pass_rate, "double")
})

test_that("longitudinal event gates use any physical row in the record-event", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "alpha", required = "y"),
    meta_row("alpha_value", "alpha"),
    meta_row("beta_value", "beta", required = "y")
  )
  rcon <- list(
    metadata = function() metadata,
    instruments = function() tibble::tibble(
      instrument_name = c("alpha", "beta"),
      instrument_label = c("Alpha", "Beta")
    ),
    projectInformation = function() tibble::tibble(
      project_id = "77", is_longitudinal = 1L
    ),
    arms = function() tibble::tibble(arm_num = 1L, name = "Arm 1"),
    events = function() tibble::tibble(
      event_id = 101L,
      unique_event_name = "baseline_arm_1",
      event_name = "Baseline",
      arm_num = 1L
    ),
    mapping = function() tibble::tibble(
      arm_num = c(1L, 1L),
      unique_event_name = c("baseline_arm_1", "baseline_arm_1"),
      form = c("alpha", "beta")
    ),
    repeatInstrumentEvent = function() tibble::tibble(
      event_name = character(), form_name = character()
    )
  )
  data <- tibble::tibble(
    record_id = "1",
    redcap_event_name = "baseline_arm_1",
    alpha_value = "entered",
    beta_value = ""
  )
  schedule <- tibble::tibble(
    record_id = "1",
    instrument = "beta",
    redcap_event_name = "baseline_arm_1",
    repeat_instance = NA_integer_
  )
  plan <- plan_explicit(data, rcon, "beta", schedule)
  result <- run_plan(plan, data, rcon, progress = FALSE)

  expect_identical(result$target_results$event_row_started, "passed")
  expect_identical(
    result$target_results$repeat_instance_row_started,
    "not applicable"
  )
  expect_identical(result$target_results$instrument_started, "failed")
  expect_identical(result$target_results$field_complete, "not reached")
})

test_that("repeating-event gates distinguish absent events from absent instances", {
  rcon <- run_plan_repeat_event_rcon()
  data <- run_plan_repeat_event_data()
  schedule <- tibble::tibble(
    record_id = c("1", "2"),
    instrument = c("diary", "diary"),
    redcap_event_name = c("visit_arm_1", "visit_arm_1"),
    repeat_instance = c(2L, 2L)
  )
  plan <- plan_explicit(data, rcon, "diary", schedule)
  result <- run_plan(plan, data, rcon, progress = FALSE)

  expect_identical(result$target_results$record_id, c("1", "2"))
  expect_identical(
    result$target_results$event_row_started,
    c("failed", "passed")
  )
  expect_identical(
    result$target_results$repeat_instance_row_started,
    c("not reached", "failed")
  )
  expect_identical(
    result$target_results$instrument_started,
    c("not reached", "not reached")
  )
  expect_identical(
    result$target_results$field_complete,
    c("not reached", "not reached")
  )
})

test_that("longitudinal unresolved rows receive canonical REDCap URLs", {
  rcon <- run_plan_rcon(longitudinal = TRUE)
  data <- dplyr::mutate(run_plan_data(required_note = ""),
                        redcap_event_name = "baseline_arm_1")
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon,
    ignore_fields = c("record_id", "branch_flag", "checkbox_field", "conditional_note"),
    progress = FALSE)
  field <- result$missing[result$missing$field_name %in% "required_note", ]
  expect_match(field$url, "pid=77", fixed = TRUE)
  expect_match(field$url, "event_id=101", fixed = TRUE)
})
test_that("run_plan result components preserve every documented storage type", {
  rcon <- run_plan_rcon(); data <- run_plan_data(required_note = "")
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, details = TRUE, progress = FALSE)

  expect_identical(vapply(result$target_results, typeof, character(1)), c(
    record_id = "character", instrument = "character",
    redcap_event_name = "character", repeat_instrument = "character",
    repeat_instance = "integer", target_source = "character",
    event_row_started = "character",
    repeat_instance_row_started = "character",
    instrument_started = "character", field_complete = "character",
    fields_assessed = "integer", fields_failed = "integer",
    field_applicability_reason = "character"
  ))
  expect_identical(vapply(result$summary, typeof, character(1)), c(
    redcap_event_name = "character", instrument = "character",
    repeat_instrument = "character", repeat_instance = "integer",
    validation_level = "character", validation_check = "character",
    status = "character", reason = "character", assessed = "integer",
    passed = "integer", failed = "integer", pass_rate = "double",
    fail_rate = "double"
  ))
  expect_identical(vapply(result$missing, typeof, character(1)), c(
    record_id = "character", redcap_event_name = "character",
    repeat_instrument = "character", repeat_instance = "integer",
    validation_context = "character", instrument = "character",
    validation_check = "character", field_name = "character",
    field_label = "character", field_type = "character",
    branching_logic = "character", url = "character"
  ))
  expect_identical(vapply(result$diagnostics, typeof, character(1)), c(
    stage = "integer", operation = "character", completed = "logical",
    elapsed_seconds = "double"
  ))
  expect_identical(names(result$verification), c(
    "enabled", "verified_user", "input_rows", "user_rows",
    "latest_user_rows", "verified_rows", "overrides_applied"
  ))
  expect_identical(vapply(result$verification, typeof, character(1)), c(
    enabled = "logical", verified_user = "character", input_rows = "integer",
    user_rows = "integer", latest_user_rows = "integer",
    verified_rows = "integer", overrides_applied = "integer"
  ))
})

test_that("zero targets and fully gated targets reconcile across report components", {
  rcon <- run_plan_rcon(longitudinal = TRUE)
  data <- dplyr::mutate(run_plan_data(), redcap_event_name = "baseline_arm_1")
  empty_schedule <- tibble::tibble(
    record_id = character(), instrument = character(),
    redcap_event_name = character(), repeat_instance = integer()
  )
  empty_plan <- plan_explicit(data, rcon, "baseline_form", empty_schedule)
  empty <- run_plan(empty_plan, data, rcon, details = TRUE, progress = FALSE)
  expect_identical(nrow(empty$target_results), 0L)
  expect_identical(nrow(empty$summary), 0L)
  expect_identical(nrow(empty$missing), 0L)
  expect_identical(nrow(empty$details), 0L)

  absent_schedule <- tibble::tibble(
    record_id = "absent", instrument = "baseline_form",
    redcap_event_name = "baseline_arm_1", repeat_instance = NA_integer_
  )
  absent_plan <- plan_explicit(data, rcon, "baseline_form", absent_schedule)
  gated <- run_plan(absent_plan, data, rcon, details = TRUE, progress = FALSE)
  expect_identical(gated$target_results$event_row_started, "failed")
  expect_identical(gated$target_results$instrument_started, "not reached")
  expect_identical(gated$target_results$field_complete, "not reached")
  event <- gated$summary[gated$summary$validation_check == "event-row-started", ]
  expect_identical(event[c("assessed", "passed", "failed")],
                   tibble::tibble(assessed = 1L, passed = 0L, failed = 1L))
  expect_identical(gated$missing$validation_check, "event-row-started")
  expect_false(any(gated$missing$validation_check %in%
    c("instrument-started", "field-complete")))
})

test_that("every field-policy argument leaves targets and upstream checks invariant", {
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

test_that("field details retain target provenance for same-context instruments", {
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
test_that("run_plan rejects incomplete metadata before field resolution", {
  rcon <- run_plan_rcon(); data <- run_plan_data()
  incomplete <- rcon
  incomplete$metadata <- function() run_plan_metadata()[, setdiff(
    names(run_plan_metadata()), "field_type"
  )]
  plan <- plan_from_data(data, rcon, "baseline_form")
  expect_error(
    run_plan(plan, data, incomplete, progress = FALSE),
    regexp = "field_type",
    class = "redcapmissing_error_schema"
  )
})

test_that("canonical structural IDs never overwrite a raw record_id response field", {
  metadata <- dplyr::bind_rows(
    meta_row("study_id", "baseline_form", required = "y"),
    meta_row("record_id", "baseline_form", required = "y"),
    meta_row("start_marker", "baseline_form", required = "y")
  )
  rcon <- list(
    metadata = function() metadata,
    instruments = function() tibble::tibble(
      instrument_name = "baseline_form",
      instrument_label = "Baseline form"
    ),
    projectInformation = function() tibble::tibble(
      project_id = 77L,
      is_longitudinal = 0L,
      record_id_field = "study_id"
    ),
    repeatInstrumentEvent = function() tibble::tibble(
      event_name = character(),
      form_name = character()
    )
  )
  data <- tibble::tibble(
    study_id = "A1",
    record_id = "",
    start_marker = "started"
  )

  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(plan, data, rcon, details = TRUE, progress = FALSE)
  record_field <- result$details[
    result$details$validation_check == "field-complete" &
      result$details$field_name == "record_id",
    ,
    drop = FALSE
  ]

  expect_identical(result$target_results$record_id, "A1")
  expect_identical(result$target_results$instrument_started, "passed")
  expect_identical(record_field$value_summary, "")
  expect_identical(record_field$effective_disposition, "failed")
  expect_true("record_id" %in% result$missing$field_name)
})

test_that("explicit exclusions must remain relevant after the required-only filter", {
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

test_that("runner reads every project structure surface once", {
  base_rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, base_rcon, "baseline_form")
  counted <- run_plan_rcon()
  counts <- new.env(parent = emptyenv())
  surfaces <- c(
    "metadata", "instruments", "projectInformation", "repeatInstrumentEvent"
  )
  for (surface in surfaces) {
    original <- counted[[surface]]
    counted[[surface]] <- local({
      surface_name <- surface
      surface_function <- original
      function() {
        current <- if (exists(surface_name, counts, inherits = FALSE)) {
          get(surface_name, counts, inherits = FALSE)
        } else {
          0L
        }
        assign(surface_name, current + 1L, counts)
        surface_function()
      }
    })
  }
  counted$exportRecords <- function(...) {
    assign("exportRecords", 1L, counts)
    stop("runner must not export records")
  }

  expect_s3_class(
    run_plan(plan, data, counted, progress = FALSE),
    "redcapmissing"
  )
  expect_identical(
    vapply(surfaces, function(surface) get(surface, counts), integer(1)),
    stats::setNames(rep(1L, length(surfaces)), surfaces)
  )
  expect_false(exists("exportRecords", counts, inherits = FALSE))
})

test_that("logical and character controls enforce their complete scalar/vector contracts", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  invalid_logical <- list(NA, logical(), c(TRUE, FALSE), 1, "TRUE", NULL)
  for (argument in c("required_fields", "details", "progress")) {
    for (value in invalid_logical) {
      arguments <- list(plan = plan, data = data, rcon = rcon, progress = FALSE)
      arguments[argument] <- list(value)
      expect_error(
        do.call(run_plan, arguments),
        class = "redcapmissing_error_argument"
      )
    }
  }
  invalid_character <- list(
    NA_character_, "", " ", " padded", "padded ", c("x", "x"), factor("x"),
    1L, TRUE, list("x"), as.Date("2026-07-25")
  )
  for (argument in c("ignore_fields", "exclude_types")) {
    for (value in invalid_character) {
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

test_that("progress updates all stages and cleans up on success and error", {
  rcon <- run_plan_rcon()
  data <- run_plan_data()
  plan <- plan_from_data(data, rcon, "baseline_form")
  expect_silent(run_plan(plan, data, rcon, progress = FALSE))
  baseline <- run_plan(plan, data, rcon, progress = FALSE)

  events <- character()
  testthat::local_mocked_bindings(
    cli_progress_bar = function(...) {
      events <<- c(events, "bar")
      "mock-progress"
    },
    cli_progress_update = function(...) {
      events <<- c(events, "update")
      invisible(NULL)
    },
    cli_progress_done = function(...) {
      events <<- c(events, "done")
      invisible(NULL)
    },
    .package = "cli"
  )
  progressed <- run_plan(plan, data, rcon, progress = TRUE)
  expect_identical(sum(events == "update"), 12L)
  expect_identical(tail(events, 1L), "done")
  expect_identical(progressed$target_results, baseline$target_results)
  expect_identical(progressed$summary, baseline$summary)
  expect_identical(progressed$missing, baseline$missing)

  events <- character()
  broken <- data[, names(data) != "optional_note", drop = FALSE]
  expect_error(
    run_plan(plan, broken, rcon, progress = TRUE),
    class = "redcapmissing_error_schema"
  )
  expect_identical(tail(events, 1L), "done")
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
test_that("mixed target outcomes reconcile statuses summaries missing rows and provenance", {
  rcon <- run_plan_rcon()
  data <- dplyr::bind_rows(
    run_plan_data(record_id = "pass", required_note = "complete"),
    run_plan_data(record_id = "fail", required_note = "")
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(
    plan, data, rcon,
    ignore_fields = c("record_id", "branch_flag", "checkbox_field", "conditional_note"),
    details = TRUE, progress = FALSE
  )

  legal <- c("passed", "failed", "not applicable", "not reached")
  status_columns <- c(
    "event_row_started", "repeat_instance_row_started",
    "instrument_started", "field_complete"
  )
  expect_true(all(unlist(result$target_results[status_columns], use.names = FALSE) %in% legal))
  expect_identical(
    result$target_results$field_complete[match(c("pass", "fail"), result$target_results$record_id)],
    c("passed", "failed")
  )
  expect_identical(result$target_results$target_source, c("observed", "observed"))

  field_summary <- result$summary[result$summary$validation_check == "field-complete", ]
  expect_identical(
    field_summary[c("assessed", "passed", "failed", "pass_rate", "fail_rate")],
    tibble::tibble(
      assessed = 2L, passed = 1L, failed = 1L,
      pass_rate = 0.5, fail_rate = 0.5
    )
  )
  expect_identical(result$missing$record_id, "fail")
  expect_identical(result$missing$validation_check, "field-complete")
  expect_identical(result$missing$field_name, "required_note")
  expect_false("pass" %in% result$missing$record_id)
  expect_identical(sum(result$target_results$fields_failed), nrow(result$missing))

  expect_type(result$target_results$redcap_event_name, "character")
  expect_type(result$target_results$repeat_instrument, "character")
  expect_type(result$target_results$repeat_instance, "integer")
  expect_true(all(is.na(result$target_results$redcap_event_name)))
  expect_true(all(is.na(result$target_results$repeat_instrument)))
  expect_true(all(is.na(result$target_results$repeat_instance)))
  expect_true(is.na(result$missing$redcap_event_name))
  expect_true(is.na(result$missing$repeat_instrument))
  expect_true(is.na(result$missing$repeat_instance))

  field_details <- result$details[result$details$validation_check == "field-complete", ]
  expect_identical(field_details$target_source, c("observed", "observed"))
})

test_that("all failing targets reconcile exactly without losing structural absence", {
  rcon <- run_plan_rcon()
  data <- dplyr::bind_rows(
    run_plan_data(record_id = "one", required_note = ""),
    run_plan_data(record_id = "two", required_note = "")
  )
  plan <- plan_from_data(data, rcon, "baseline_form")
  result <- run_plan(
    plan, data, rcon,
    ignore_fields = c("record_id", "branch_flag", "checkbox_field", "conditional_note"),
    details = TRUE, progress = FALSE
  )

  expect_identical(result$target_results$target_source, c("observed", "observed"))
  expect_identical(result$target_results$event_row_started,
                   c("not applicable", "not applicable"))
  expect_identical(result$target_results$repeat_instance_row_started,
                   c("not applicable", "not applicable"))
  expect_identical(result$target_results$instrument_started, c("passed", "passed"))
  expect_identical(result$target_results$field_complete, c("failed", "failed"))
  expect_identical(result$target_results$fields_assessed, c(1L, 1L))
  expect_identical(result$target_results$fields_failed, c(1L, 1L))

  field_summary <- result$summary[result$summary$validation_check == "field-complete", ]
  expect_identical(
    field_summary[c("assessed", "passed", "failed", "pass_rate", "fail_rate")],
    tibble::tibble(
      assessed = 2L, passed = 0L, failed = 2L,
      pass_rate = 0, fail_rate = 1
    )
  )
  expect_setequal(result$missing$record_id, c("one", "two"))
  expect_true(all(result$missing$validation_check == "field-complete"))
  expect_true(all(result$missing$field_name == "required_note"))
  expect_identical(nrow(result$missing), sum(result$target_results$fields_failed))
  expect_true(all(is.na(result$missing$redcap_event_name)))
  expect_true(all(is.na(result$missing$repeat_instrument)))
  expect_true(all(is.na(result$missing$repeat_instance)))
  expect_type(result$missing$redcap_event_name, "character")
  expect_type(result$missing$repeat_instrument, "character")
  expect_type(result$missing$repeat_instance, "integer")

  failed_details <- result$details[
    result$details$validation_check == "field-complete" &
      result$details$effective_disposition == "failed",
  ]
  expect_setequal(failed_details$record_id, c("one", "two"))
  expect_true(all(failed_details$target_source == "observed"))
})