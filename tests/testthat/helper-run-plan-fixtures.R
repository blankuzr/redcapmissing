run_plan_metadata <- function() {
  dplyr::bind_rows(
    meta_row("record_id", "baseline_form", required = "y"),
    meta_row("start_marker", "baseline_form"),
    meta_row("branch_flag", "baseline_form", field_type = "yesno", required = "y"),
    meta_row("required_note", "baseline_form", required = "y"),
    meta_row("optional_note", "baseline_form"),
    meta_row("checkbox_field", "baseline_form", field_type = "checkbox",
             choices = "1, First | 2, Second", required = "y"),
    meta_row("conditional_note", "baseline_form", branching = "[branch_flag] = '1'", required = "y"),
    meta_row("descriptive_text", "baseline_form", field_type = "descriptive", required = "y")
  )
}

run_plan_rcon <- function(longitudinal = FALSE, repeat_table = NULL) {
  metadata <- run_plan_metadata()
  instruments <- tibble::tibble(
    instrument_name = "baseline_form",
    instrument_label = "Baseline form"
  )
  if (is.null(repeat_table)) {
    repeat_table <- tibble::tibble(event_name = character(), form_name = character())
  }
  info <- tibble::tibble(project_id = "77", is_longitudinal = as.integer(longitudinal))
  events <- if (longitudinal) tibble::tibble(
    arm_num = 1L, unique_event_name = "baseline_arm_1",
    event_name = "Baseline", event_id = 101L
  ) else NULL
  mapping <- if (longitudinal) tibble::tibble(
    arm_num = 1L, unique_event_name = "baseline_arm_1", form = "baseline_form"
  ) else NULL
  arms <- if (longitudinal) tibble::tibble(arm_num = 1L, name = "Arm 1") else NULL
  connection <- list(
    url = "https://example.test/api/",
    metadata = function() metadata,
    instruments = function() instruments,
    projectInformation = function() info,
    repeatInstrumentEvent = function() repeat_table,
    events = function() events,
    mapping = function() mapping,
    arms = function() arms,
    version = function() "15.0.0"
  )
  redcap_api_connection_fixture(connection)
}

run_plan_data <- function(record_id = "1", required_note = "complete",
                          start_marker = "started", branch_flag = "0",
                          checkbox_1 = "1", checkbox_2 = "0") {
  tibble::tibble(
    record_id = record_id,
    start_marker = start_marker,
    branch_flag = branch_flag,
    required_note = required_note,
    optional_note = "",
    checkbox_field___1 = checkbox_1,
    checkbox_field___2 = checkbox_2,
    conditional_note = "",
    descriptive_text = ""
  )
}

run_plan_explicit_schedule <- function(record_id = "1") {
  tibble::tibble(
    record_id = record_id,
    instrument = "baseline_form",
    redcap_event_name = NA_character_,
    repeat_instance = NA_integer_
  )
}

run_plan_verified_row <- function(
  record = "1", field_name = "required_note",
  ts = "2026-07-25 12:00:00", status = "VERIFIED", username = "alice"
) {
  tibble::tibble(
    project_id = "77", record = record, event_id = NA_character_,
    field_name = field_name, repeat_instrument = NA_character_,
    instance = NA_integer_, ts = ts, current_query_status = status,
    username = username
  )
}
empty_target_results_fixture <- function() {
  tibble::tibble(
    record_id = character(), instrument = character(),
    redcap_event_name = character(), repeat_instrument = character(),
    repeat_instance = integer(), target_source = character(),
    event_row_started = character(),
    repeat_instance_row_started = character(),
    instrument_started = character(), field_complete = character(),
    fields_assessed = integer(), fields_failed = integer(),
    field_applicability_reason = character()
  )
}

empty_diagnostics_fixture <- function() {
  tibble::tibble(
    stage = integer(), operation = character(), completed = logical(),
    elapsed_seconds = double()
  )
}

complete_report_fixture <- function(
  plan,
  target_results = empty_target_results_fixture(),
  summary = .redcapmissing_get_summary_prototype(),
  missing = .redcapmissing_get_missing_prototype(),
  verification = .rcm_verification_audit(),
  diagnostics = empty_diagnostics_fixture(),
  details = NULL
) {
  structure(
    list(
      plan = plan,
      target_results = target_results,
      summary = summary,
      missing = missing,
      verification = verification,
      diagnostics = diagnostics,
      details = details
    ),
    class = c("redcapmissing", "list")
  )
}
run_plan_repeat_event_rcon <- function() {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "screening", required = "y"),
    meta_row("screen_value", "screening"),
    meta_row("diary_value", "diary", required = "y")
  )
  connection <- list(
    url = "https://example.test/api/",
    metadata = function() metadata,
    instruments = function() tibble::tibble(
      instrument_name = c("screening", "diary"),
      instrument_label = c("Screening", "Diary")
    ),
    projectInformation = function() tibble::tibble(
      project_id = "77",
      is_longitudinal = 1L,
      has_repeating_instruments_or_events = 1L
    ),
    arms = function() tibble::tibble(arm_num = 1L, name = "Arm 1"),
    events = function() tibble::tibble(
      event_id = c(101L, 102L),
      unique_event_name = c("screening_arm_1", "visit_arm_1"),
      event_name = c("Screening", "Visit"),
      arm_num = c(1L, 1L)
    ),
    mapping = function() tibble::tibble(
      arm_num = c(1L, 1L),
      unique_event_name = c("screening_arm_1", "visit_arm_1"),
      form = c("screening", "diary")
    ),
    repeatInstrumentEvent = function() tibble::tibble(
      event_name = "visit_arm_1",
      form_name = NA_character_
    ),
    version = function() "15.0.0"
  )
  redcap_api_connection_fixture(connection)
}

run_plan_repeat_event_data <- function() {
  tibble::tibble(
    record_id = c("1", "2", "2"),
    redcap_event_name = c(
      "screening_arm_1", "screening_arm_1", "visit_arm_1"
    ),
    redcap_repeat_instrument = c(NA_character_, NA_character_, NA_character_),
    redcap_repeat_instance = c(NA_integer_, NA_integer_, 1L),
    screen_value = c("entered", "entered", ""),
    diary_value = c("", "", "")
  )
}