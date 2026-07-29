.plan_metadata <- function() {
  tibble::tibble(
    field_name = c("record_id", "age", "note", "diary_value"),
    form_name = c("demographics", "demographics", "notes", "diary"),
    field_type = rep("text", 4L)
  )
}

.plan_fake_rcon <- function(
  longitudinal = FALSE,
  metadata = .plan_metadata(),
  instruments = NULL,
  arms = NULL,
  events = NULL,
  mapping = NULL,
  repeats = tibble::tibble(),
  project_id = 41L,
  record_id_field = NULL
) {
  if (is.null(instruments)) {
    instrument_names <- unique(metadata$form_name)
    instruments <- tibble::tibble(
      instrument_name = instrument_names,
      instrument_label = paste(instrument_names, "label")
    )
  }
  info <- tibble::tibble(
    project_id = project_id,
    is_longitudinal = as.integer(longitudinal)
  )
  if (!is.null(record_id_field)) info$record_id_field <- record_id_field
  connection <- list(
    metadata = function() metadata,
    instruments = function() instruments,
    projectInformation = function() info,
    arms = function() arms,
    events = function() events,
    mapping = function() mapping,
    repeatInstrumentEvent = function() repeats,
    exportRecords = function(...) stop("constructors must not export records")
  )
  redcap_api_connection_fixture(connection)
}

.plan_longitudinal_rcon <- function(repeats = NULL) {
  metadata <- .plan_metadata()
  arms <- tibble::tibble(
    arm_num = c(1L, 2L),
    name = c("Treatment", "Comparator")
  )
  events <- tibble::tibble(
    event_id = c(101L, 102L, 201L),
    unique_event_name = c("baseline_arm_1", "visit_arm_1", "baseline_arm_2"),
    event_name = c("Baseline", "Visit", "Baseline"),
    arm_num = c(1L, 1L, 2L)
  )
  mapping <- tibble::tibble(
    arm_num = c(1L, 1L, 1L, 1L, 2L),
    unique_event_name = c(
      "baseline_arm_1", "baseline_arm_1", "visit_arm_1",
      "visit_arm_1", "baseline_arm_2"
    ),
    form = c("demographics", "diary", "notes", "diary", "demographics")
  )
  if (is.null(repeats)) {
    repeats <- tibble::tibble(
      event_name = "visit_arm_1",
      form_name = "diary"
    )
  }
  .plan_fake_rcon(
    longitudinal = TRUE,
    metadata = metadata,
    arms = arms,
    events = events,
    mapping = mapping,
    repeats = repeats
  )
}

.plan_longitudinal_data <- function() {
  tibble::tibble(
    record_id = c("r1", "r1", "r1", "r2"),
    redcap_event_name = c(
      "baseline_arm_1", "visit_arm_1", "visit_arm_1", "baseline_arm_2"
    ),
    redcap_repeat_instrument = c(NA, NA, "diary", NA),
    redcap_repeat_instance = c(NA, NA, 2L, NA),
    age = c("", "", "", ""),
    note = c("", "", "", ""),
    diary_value = c("", "", "", "")
  )
}
