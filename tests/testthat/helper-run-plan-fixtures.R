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

run_plan_target_scope_rcon <- function() {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "active_form", required = "y"),
    meta_row("active_start", "active_form"),
    meta_row("active_value", "active_form", required = "y"),
    meta_row("inactive_note", "inactive_form", field_type = "notes"),
    meta_row(
      "inactive_checkbox",
      "inactive_form",
      field_type = "checkbox",
      choices = "1, First | 2, Second"
    )
  )
  connection <- list(
    url = "https://example.test/api/",
    metadata = function() metadata,
    instruments = function() tibble::tibble(
      instrument_name = c("active_form", "inactive_form"),
      instrument_label = c("Active form", "Inactive form")
    ),
    projectInformation = function() tibble::tibble(
      project_id = "77",
      is_longitudinal = 1L
    ),
    arms = function() tibble::tibble(arm_num = 1L, name = "Arm 1"),
    events = function() tibble::tibble(
      event_id = 101L,
      unique_event_name = "baseline_arm_1",
      event_name = "Baseline",
      arm_num = 1L
    ),
    mapping = function() tibble::tibble(
      arm_num = 1L,
      unique_event_name = "baseline_arm_1",
      form = "active_form"
    ),
    repeatInstrumentEvent = function() tibble::tibble(
      event_name = character(),
      form_name = character()
    ),
    version = function() "15.0.0"
  )
  redcap_api_connection_fixture(connection)
}

run_plan_target_scope_data <- function() {
  tibble::tibble(
    record_id = "1",
    redcap_event_name = "baseline_arm_1",
    active_start = "started",
    active_value = "complete",
    inactive_note = "not targeted",
    inactive_checkbox___1 = "1",
    inactive_checkbox___2 = "0"
  )
}
