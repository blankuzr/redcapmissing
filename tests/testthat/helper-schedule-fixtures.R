.schedule_helper_connection <- function(longitudinal = TRUE) {
  calls <- new.env(parent = emptyenv())
  surface_names <- c(
    "metadata", "instruments", "projectInformation",
    "repeatInstrumentEvent", "mapping", "events", "arms",
    "exportRecords"
  )
  for (surface in surface_names) calls[[surface]] <- 0L
  bump <- function(surface) {
    calls[[surface]] <- calls[[surface]] + 1L
    invisible(NULL)
  }

  instrument_names <- c(
    "baseline", "partial", "diary", "event_form", "inactive", "retired"
  )
  instruments <- tibble::tibble(
    instrument_name = instrument_names,
    instrument_label = paste(instrument_names, "label")
  )
  metadata <- tibble::tibble(
    field_name = c(
      "record_id", "partial_value", "diary_value", "event_value",
      "inactive_value", "retired_value"
    ),
    form_name = instrument_names,
    field_type = rep("text", length(instrument_names))
  )

  if (isTRUE(longitudinal)) {
    project_information <- tibble::tibble(
      project_id = 700L,
      is_longitudinal = 1L
    )
    arms <- tibble::tibble(
      arm_num = c(2L, 1L),
      name = c("Second", "First")
    )
    events <- tibble::tibble(
      event_id = c(201L, 104L, 101L, 103L, 102L),
      unique_event_name = c(
        "baseline_arm_2", "repeat_visit_arm_1", "baseline_arm_1",
        "diary_arm_1", "followup_arm_1"
      ),
      event_name = c(
        "Baseline 2", "Repeating visit", "Baseline 1", "Diary", "Follow-up"
      ),
      arm_num = c(2L, 1L, 1L, 1L, 1L)
    )
    mapping <- tibble::tibble(
      arm_num = c(1L, 1L, 2L, 1L, 1L, 1L),
      unique_event_name = c(
        "repeat_visit_arm_1", "repeat_visit_arm_1", "baseline_arm_2",
        "diary_arm_1", "followup_arm_1", "baseline_arm_1"
      ),
      form = c(
        "event_form", "diary", "baseline", "diary", "partial", "baseline"
      )
    )
    repeats <- tibble::tibble(
      event_name = c("diary_arm_1", "repeat_visit_arm_1"),
      form_name = c("diary", NA_character_)
    )
  } else {
    project_information <- tibble::tibble(
      project_id = 701L,
      is_longitudinal = 0L
    )
    arms <- NULL
    events <- NULL
    mapping <- NULL
    repeats <- tibble::tibble(
      event_name = NA_character_,
      form_name = "diary"
    )
  }

  connection <- list(
    metadata = function() {
      bump("metadata")
      metadata
    },
    instruments = function() {
      bump("instruments")
      instruments
    },
    projectInformation = function() {
      bump("projectInformation")
      project_information
    },
    repeatInstrumentEvent = function() {
      bump("repeatInstrumentEvent")
      repeats
    },
    mapping = function() {
      bump("mapping")
      mapping
    },
    events = function() {
      bump("events")
      events
    },
    arms = function() {
      bump("arms")
      arms
    },
    exportRecords = function(...) {
      bump("exportRecords")
      stop("schedule helpers and plan constructors must not export records")
    }
  )
  connection <- structure(
    connection,
    class = c("redcapApiConnection", "redcapConnection")
  )
  list(rcon = connection, calls = calls, instruments = instrument_names)
}

.schedule_helper_longitudinal_data <- function() {
  tibble::tibble(
    record_id = c("r1", "r2"),
    redcap_event_name = c("baseline_arm_1", "baseline_arm_2"),
    redcap_repeat_instrument = c(NA_character_, NA_character_),
    redcap_repeat_instance = c(NA_integer_, NA_integer_)
  )
}
