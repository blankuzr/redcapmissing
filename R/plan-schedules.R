# Extended and explicit assessment-plan schedule contracts.

#' Build an extended assessment schedule
#'
#' `build_extended_schedule()` creates every allowable REDCap event crossing
#' for the requested instruments. Pass its result to [plan_from_data()] as
#' `extended_schedule` to add those crossings for records observed in the
#' applicable arm.
#'
#' @param rcon A `redcapAPI` connection inheriting from
#'   `redcapApiConnection`, as created by [redcapAPI::redcapConnection()], or
#'   `redcapOfflineConnection`, as created by [redcapAPI::offlineConnection()]
#'   or [redcapAPI::readPreservedProject()]. It must expose project
#'   information, metadata, instruments, and applicable arms, events,
#'   mappings, and repeat configuration.
#' @param instruments A nonempty character vector of unique, nonmissing,
#'   nonblank, unpadded raw REDCap instrument names. Values must exist in the
#'   project. Use [all_instruments()] explicitly to request the complete
#'   inventory. `NULL` does not mean all instruments.
#' @param n_repeat_instances One positive finite whole-number numeric scalar
#'   within R's integer range. Every repeating event or repeating instrument
#'   crossing is expanded to instances `1` through `n_repeat_instances`.
#'
#' @details
#' In a classic project, every requested instrument has one native eventless
#' crossing, represented by `redcap_event_name = NA_character_`. A
#' nonrepeating instrument receives `repeat_instance = NA_integer_`; a
#' repeating instrument receives instances `1:n_repeat_instances`.
#'
#' In a longitudinal project, only instrument-event crossings designated by
#' REDCap are returned. A repeating event or instrument receives instances
#' `1:n_repeat_instances`. The function never creates an eventless
#' longitudinal crossing.
#'
#' Requested instrument order determines the primary row order, followed by
#' REDCap event order and ascending repeat instance. The builder creates
#' schedule rows, not record-level targets. [plan_from_data()] expands each row
#' across records observed in its applicable arm and verifies that schedule
#' instruments are a subset of the plan's `instruments`.
#'
#' @return A tibble with exactly three columns: character `instrument`,
#'   character `redcap_event_name`, and integer `repeat_instance`, in that
#'   order. When no crossing can be produced, the result is a correctly typed
#'   zero-row tibble.
#'
#' @section Conditions:
#' Argument, project, and schedule validation failures inherit from
#' `redcapmissing_error`. If one or more valid requested instruments in a
#' longitudinal project are designated to no event, the function omits them
#' and emits one `redcapmissing_warning_undesignated_extension` warning. The
#' warning also inherits from `redcapmissing_warning` and stores the affected
#' raw names in its `instruments` field.
#'
#' @examples
#' \dontrun{
#' plan_instruments <- all_instruments(rcon)
#' extended_schedule <- build_extended_schedule(
#'   rcon,
#'   instruments = c("followup", "diary"),
#'   n_repeat_instances = 3L
#' )
#' plan <- plan_from_data(
#'   records,
#'   rcon,
#'   instruments = plan_instruments,
#'   extended_schedule = extended_schedule
#' )
#' }
#'
#' @seealso [all_instruments()], [plan_from_data()]
#' @export
build_extended_schedule <- function(
  rcon,
  instruments,
  n_repeat_instances = 1L
) {
  if (missing(rcon) || is.null(rcon)) {
    .condition_signal_error("`rcon` is required and cannot be `NULL`.", "argument")
  }
  if (missing(instruments) || is.null(instruments)) {
    .condition_signal_error("`instruments` is required and cannot be `NULL`.", "argument")
  }
  n_repeat_instances <- .schedule_normalize_instance_count(n_repeat_instances)
  snapshot <- .project_structure_build_snapshot(rcon)
  instruments <- .schedule_validate_instruments(instruments, snapshot)
  schedule <- .schedule_expand_allowable_crossings(
    snapshot$allowable_crossings,
    instruments,
    snapshot$event_order,
    n_repeat_instances
  )
  if (isTRUE(snapshot$project$longitudinal)) {
    undesignated <- instruments[
      !instruments %in% snapshot$allowable_crossings$instrument
    ]
    if (length(undesignated)) {
      .schedule_warn_undesignated_instruments(undesignated)
    }
  }
  schedule
}

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

.schedule_warn_undesignated_instruments <- function(instruments) {
  instruments <- unique(instruments)
  condition <- structure(
    list(
      message = paste0(
        "`build_extended_schedule()` could not add rows for requested ",
        "longitudinal instrument(s) designated to no REDCap event: ",
        paste(instruments, collapse = ", "), "."
      ),
      call = NULL,
      instruments = instruments
    ),
    class = c(
      "redcapmissing_warning_undesignated_extension",
      "redcapmissing_warning", "warning", "condition"
    )
  )
  warning(condition)
}

.schedule_normalize_instance_count <- function(n_repeat_instances) {
  valid_storage <- is.numeric(n_repeat_instances) &&
    !is.logical(n_repeat_instances) &&
    !is.object(n_repeat_instances)
  valid <- valid_storage &&
    length(n_repeat_instances) == 1L &&
    !is.na(n_repeat_instances) &&
    is.finite(n_repeat_instances) &&
    n_repeat_instances >= 1 &&
    n_repeat_instances == floor(n_repeat_instances) &&
    n_repeat_instances <= .Machine$integer.max
  if (!valid) {
    .condition_signal_error(
      paste0(
        "`n_repeat_instances` must be one positive finite whole number ",
        "within integer range."
      ),
      "argument"
    )
  }
  as.integer(n_repeat_instances)
}

.schedule_detect_repeating_crossings <- function(repeat_mode) {
  repeat_mode %in% c("repeating_event", "repeating_instrument")
}

.schedule_expand_allowable_crossings <- function(
  crossings,
  instruments,
  event_order,
  n_repeat_instances
) {
  output <- tibble::tibble(
    instrument = character(),
    redcap_event_name = character(),
    repeat_instance = integer()
  )
  selected <- crossings[
    crossings$instrument %in% instruments,
    c("instrument", "redcap_event_name", "repeat_mode"),
    drop = FALSE
  ]
  if (!nrow(selected)) return(output)
  instrument_rank <- match(selected$instrument, instruments)
  event_rank <- ifelse(
    is.na(selected$redcap_event_name),
    0L,
    match(selected$redcap_event_name, event_order)
  )
  selected <- selected[
    order(instrument_rank, event_rank, na.last = TRUE),
    ,
    drop = FALSE
  ]
  rows <- lapply(seq_len(nrow(selected)), function(row) {
    instances <- if (
      .schedule_detect_repeating_crossings(selected$repeat_mode[[row]])
    ) {
      seq_len(n_repeat_instances)
    } else {
      NA_integer_
    }
    tibble::tibble(
      instrument = rep(selected$instrument[[row]], length(instances)),
      redcap_event_name = rep(
        selected$redcap_event_name[[row]],
        length(instances)
      ),
      repeat_instance = instances
    )
  })
  dplyr::bind_rows(rows)
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
  requires_instance <- !crossing_invalid &
    .schedule_detect_repeating_crossings(matched$repeat_mode)
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
