# Extended and explicit assessment-plan schedule contracts.

.schedule_warn_empty_arm <- function(events) {
  events <- unique(events)
  condition <- structure(
    list(
      message = paste0(
        "`extended_schedule` could not add targets for event(s) with no ",
        "records observed in the applicable arm: ",
        paste(events, collapse = ", "), "."
      ),
      call = NULL,
      events = events
    ),
    class = c(
      "redcapmissing_warning_empty_arm_extension",
      "redcapmissing_warning", "warning", "condition"
    )
  )
  warning(condition)
}

.schedule_validate_instruments <- function(instruments, snapshot) {
  if (missing(instruments) || is.null(instruments) || !length(instruments)) {
    .condition_signal_error("`instruments` must contain at least one REDCap instrument name.", "argument")
  }
  if (!is.character(instruments)) {
    .condition_signal_error("`instruments` must be a character vector.", "argument")
  }
  invalid <- is.na(instruments) | instruments == "" | grepl("^\\s+$", instruments) | trimws(instruments) != instruments
  if (any(invalid) || anyDuplicated(instruments)) {
    .condition_signal_error("`instruments` must contain unique, nonblank, unpadded names.", "argument")
  }
  unknown <- setdiff(instruments, snapshot$instrument_order)
  if (length(unknown)) {
    .condition_signal_error(paste0("Unknown `instruments`: ", paste(unknown, collapse = ", "), "."), "schedule")
  }
  instruments
}

.schedule_list_columns <- function(type) {
  if (identical(type, "extended")) {
    c("instrument", "redcap_event_name", "repeat_instance")
  } else {
    c("record_id", "instrument", "redcap_event_name", "repeat_instance")
  }
}

.schedule_normalize_rows <- function(schedule, type, snapshot, instruments, data) {
  source <- paste0(type, "_schedule")
  expected <- .schedule_list_columns(type)
  if (!is.data.frame(schedule)) .condition_signal_error(paste0("`", source, "` must be a data frame."), "argument")
  if (!identical(names(schedule), expected)) {
    .condition_signal_error(
      paste0("`", source, "` must contain exactly these columns in order: ", paste(expected, collapse = ", "), "."),
      "schedule"
    )
  }
  instrument <- .schema_normalize_character(schedule$instrument, paste0(source, "$instrument"))
  bad_instrument <- is.na(instrument) | instrument == "" | grepl("^\\s+$", instrument) | trimws(instrument) != instrument
  if (any(bad_instrument)) .condition_signal_error(paste0("`", source, "$instrument` must be nonmissing and unpadded."), "schedule")
  unknown <- setdiff(unique(instrument), instruments)
  if (length(unknown)) .condition_signal_error(paste0("`", source, "$instrument` must be a subset of `instruments`."), "schedule")
  event <- .schema_normalize_nullable_character(schedule$redcap_event_name, paste0(source, "$redcap_event_name"))
  if (isTRUE(snapshot$project$longitudinal) && anyNA(event)) {
    .condition_signal_error(paste0("`", source, "$redcap_event_name` requires raw event names."), "schedule")
  }
  if (!isTRUE(snapshot$project$longitudinal) && any(!is.na(event))) {
    .condition_signal_error(paste0("`", source, "$redcap_event_name` must be missing in a classic project."), "schedule")
  }
  instance <- .schema_normalize_repeat_instance(schedule$repeat_instance, paste0(source, "$repeat_instance"))
  record_id <- if (identical(type, "explicit")) {
    .schema_normalize_required_id(schedule$record_id, "explicit_schedule$record_id")
  } else rep(NA_character_, nrow(schedule))
  out <- tibble::tibble(
    record_id = record_id,
    instrument = instrument,
    redcap_event_name = event,
    repeat_instance = instance,
    repeat_mode = rep(NA_character_, nrow(schedule))
  )
  if (!nrow(out)) return(out)
  allowable <- snapshot$allowable_crossings[
    , c("instrument", "redcap_event_name", "repeat_mode"),
    drop = FALSE
  ]
  allowable$.allowed <- TRUE
  candidate <- out
  candidate$.input_row <- seq_len(nrow(candidate))
  candidate$repeat_mode <- NULL
  matched <- merge(
    as.data.frame(candidate),
    as.data.frame(allowable),
    by = c("instrument", "redcap_event_name"),
    all.x = TRUE,
    sort = FALSE
  )
  matched <- matched[order(matched$.input_row), , drop = FALSE]
  crossing_invalid <- is.na(matched$.allowed)
  requires_instance <- !crossing_invalid & matched$repeat_mode != "no_repeat"
  instance_invalid <- !crossing_invalid &
    (requires_instance == is.na(matched$repeat_instance))
  invalid <- which(crossing_invalid | instance_invalid)
  if (length(invalid)) {
    row <- invalid[[1L]]
    message <- if (crossing_invalid[[row]]) {
      paste0("`", source, "` row ", row, " is not an allowable crossing.")
    } else if (requires_instance[[row]]) {
      paste0(
        "`", source, "` row ", row,
        " requires a positive `repeat_instance` because its instrument ",
        "and event crossing repeats."
      )
    } else {
      paste0(
        "`", source, "` row ", row,
        " requires a missing `repeat_instance` because its instrument ",
        "and event crossing does not repeat."
      )
    }
    .condition_signal_error(message, "schedule")
  }
  out <- tibble::as_tibble(matched[, c(
    "record_id", "instrument", "redcap_event_name",
    "repeat_instance", "repeat_mode"
  ), drop = FALSE])
  schedule_key <- if (identical(type, "explicit")) {
    c("record_id", "instrument", "redcap_event_name", "repeat_instance")
  } else {
    c("instrument", "redcap_event_name", "repeat_instance")
  }
  if (.record_detect_duplicate_rows(out, schedule_key)) {
    .condition_signal_error(
      paste0("`", source, "` contains duplicate normalized rows."),
      "schedule"
    )
  }
  if (identical(type, "explicit") && nrow(out) && isTRUE(snapshot$project$longitudinal) && nrow(data)) {
    record_arms <- .record_map_arms(data, snapshot)
    observed_arm <- record_arms$arm_num[match(out$record_id, record_arms$record_id)]
    scheduled_arm <- snapshot$event_arms$arm_num[
      match(out$redcap_event_name, snapshot$event_arms$redcap_event_name)
    ]
    if (any(!is.na(observed_arm) & observed_arm != scheduled_arm)) {
      .condition_signal_error("An explicit target cannot place an observed record in a contradictory arm.", "schedule")
    }
  }
  out
}
