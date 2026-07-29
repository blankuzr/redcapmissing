# REDCap connection and normalized project-structure contracts.

#' List every REDCap instrument in a project
#'
#' `all_instruments()` returns the project instrument inventory in REDCap
#' order. Inventory membership is distinct from schedulability: in a
#' longitudinal project, the result includes instruments that are currently
#' designated to no event.
#'
#' @param rcon A `redcapAPI` connection inheriting from
#'   `redcapApiConnection`, as created by [redcapAPI::redcapConnection()], or
#'   `redcapOfflineConnection`, as created by [redcapAPI::offlineConnection()]
#'   or [redcapAPI::readPreservedProject()]. The connection must expose its
#'   project instrument surface.
#'
#' @return A character vector of unique raw REDCap instrument names, in the
#'   order supplied by `rcon`.
#'
#' @section Conditions:
#' A missing or `NULL` connection produces
#' `redcapmissing_error_argument`. An unsupported connection or malformed
#' instrument surface produces `redcapmissing_error_project`.
#'
#' @examples
#' \dontrun{
#' instruments <- all_instruments(rcon)
#' }
#'
#' @seealso [build_extended_schedule()], [plan_from_data()],
#'   [plan_explicit()]
#' @export
all_instruments <- function(rcon) {
  if (missing(rcon) || is.null(rcon)) {
    .condition_signal_error("`rcon` is required and cannot be `NULL`.", "argument")
  }
  .rcon_validate_class(rcon)
  .project_structure_read_instruments(rcon)$instrument
}

.rcon_validate_class <- function(rcon) {
  supported_classes <- c("redcapApiConnection", "redcapOfflineConnection")
  is_supported <- any(vapply(
    supported_classes,
    function(class) inherits(rcon, class),
    logical(1)
  ))
  if (!is_supported) {
    .condition_signal_error(
      paste0(
        "`rcon` must inherit from `redcapApiConnection` or ",
        "`redcapOfflineConnection`."
      ),
      "project"
    )
  }
  invisible(rcon)
}

.project_structure_read_surface <- function(rcon, methods, label, required = TRUE) {
  if (is.null(rcon)) {
    .condition_signal_error("Provide a REDCap connection object as `rcon`.", "project")
  }
  for (method in methods) {
    candidate <- tryCatch(rcon[[method]], error = function(e) NULL)
    if (is.null(candidate)) next
    value <- if (is.function(candidate)) {
      tryCatch(candidate(), error = function(e) {
        .condition_signal_error(
          paste0("`rcon$", method, "()` failed while reading ", label, ": ", conditionMessage(e)),
          "project"
        )
      })
    } else candidate
    if (is.null(value)) {
      .condition_signal_error(
        paste0(
          "The ", label, " surface returned `NULL`; supply an explicit ",
          "data frame, including an empty data frame for an empty structure."
        ),
        "project"
      )
    }
    if (!is.data.frame(value)) {
      .condition_signal_error(paste0("The ", label, " surface must be a data frame."), "project")
    }
    if (is.null(names(value)) || anyNA(names(value)) ||
        any(names(value) == "") || anyDuplicated(names(value))) {
      .condition_signal_error(
        paste0("The ", label, " surface must have unique, nonblank column names."),
        "project"
      )
    }
    return(tibble::as_tibble(value))
  }
  if (isTRUE(required)) {
    .condition_signal_error(
      paste0("`rcon` must provide ", label, " through: ", paste0("`", methods, "()`", collapse = ", "), "."),
      "project"
    )
  }
  tibble::tibble()
}

.project_structure_normalize_longitudinal <- function(x) {
  if (length(x) != 1 || is.na(x)) {
    .condition_signal_error("Project information must contain one valid `is_longitudinal` value.", "project")
  }
  value <- tolower(as.character(x))
  if (value %in% c("1", "true")) return(TRUE)
  if (value %in% c("0", "false")) return(FALSE)
  .condition_signal_error("`is_longitudinal` must be 0/1 or FALSE/TRUE.", "project")
}

.project_structure_normalize_instruments <- function(data) {
  name_col <- .schema_resolve_column(data, c("instrument_name", "form_name", "form"), "rcon$instruments()")
  label_col <- .schema_resolve_column(data, c("instrument_label", "form_label"), "rcon$instruments()", FALSE)
  instrument <- .schema_normalize_required_id(data[[name_col]], "rcon$instruments() instrument names")
  if (!length(instrument) || anyDuplicated(instrument)) {
    .condition_signal_error("`rcon$instruments()` must contain unique instruments.", "project")
  }
  label <- if (is.null(label_col)) instrument else .schema_normalize_nullable_character(data[[label_col]], "instrument labels")
  tibble::tibble(instrument = instrument, instrument_label = label)
}

.project_structure_read_instruments <- function(rcon) {
  .project_structure_normalize_instruments(
    .project_structure_read_surface(rcon, "instruments", "project instruments")
  )
}

.project_structure_normalize_mapping <- function(data) {
  event_col <- .schema_resolve_column(data, c("unique_event_name", "event_name"), "rcon mapping")
  form_col <- .schema_resolve_column(data, c("form", "form_name", "instrument", "instrument_name"), "rcon mapping")
  arm_col <- .schema_resolve_column(data, c("arm_num", "arm_number"), "rcon mapping")
  out <- tibble::tibble(
    arm_num = .schema_normalize_required_id(data[[arm_col]], "mapping arm numbers"),
    redcap_event_name = .schema_normalize_required_id(data[[event_col]], "mapping event names"),
    instrument = .schema_normalize_required_id(data[[form_col]], "mapping instruments")
  )
  if (!nrow(out) || anyDuplicated(out)) {
    .condition_signal_error("The longitudinal instrument to event mapping must be nonempty and unique.", "project")
  }
  out
}

.project_structure_normalize_events <- function(data, mapping) {
  event_col <- .schema_resolve_column(data, c("unique_event_name", "event_name"), "rcon$events()")
  event <- .schema_normalize_required_id(data[[event_col]], "rcon$events() raw event names")
  if (!length(event) || anyDuplicated(event)) {
    .condition_signal_error("`rcon$events()` must contain unique raw event names.", "project")
  }
  arm_col <- .schema_resolve_column(data, c("arm_num", "arm_number"), "rcon$events()", FALSE)
  arm <- if (is.null(arm_col)) rep(NA_character_, length(event)) else .schema_normalize_required_id(data[[arm_col]], "event arm numbers")
  for (i in which(is.na(arm))) {
    candidate <- unique(mapping$arm_num[mapping$redcap_event_name == event[[i]]])
    if (length(candidate) == 1) arm[[i]] <- candidate
  }
  if (anyNA(arm)) .condition_signal_error("Every longitudinal event must map to exactly one arm.", "project")
  id_col <- .schema_resolve_column(data, "event_id", "rcon$events()")
  event_id_input <- data[[id_col]]
  if (is.factor(event_id_input)) event_id_input <- as.character(event_id_input)
  event_id <- .schema_normalize_repeat_instance(event_id_input, "rcon$events() event IDs")
  if (anyNA(event_id) || anyDuplicated(event_id)) {
    .condition_signal_error("Longitudinal event IDs must be unique positive integers.", "project")
  }
  event_id <- as.character(event_id)
  label <- if (all(c("unique_event_name", "event_name") %in% names(data))) {
    .schema_normalize_nullable_character(data$event_name, "event labels")
  } else event
  tibble::tibble(redcap_event_name = event, event_label = label, event_id = event_id, arm_num = arm)
}

.project_structure_normalize_arms <- function(data) {
  arm_col <- .schema_resolve_column(data, c("arm_num", "arm_number"), "rcon$arms()")
  arm <- .schema_normalize_required_id(data[[arm_col]], "rcon$arms() arm numbers")
  if (!length(arm) || anyDuplicated(arm)) .condition_signal_error("`rcon$arms()` must contain unique arms.", "project")
  name_col <- .schema_resolve_column(data, c("name", "arm_name"), "rcon$arms()", FALSE)
  name <- if (is.null(name_col)) rep(NA_character_, length(arm)) else .schema_normalize_nullable_character(data[[name_col]], "arm names")
  tibble::tibble(arm_num = arm, arm_name = name)
}

.project_structure_normalize_repeats <- function(data, longitudinal) {
  if (!nrow(data) && !ncol(data)) {
    return(tibble::tibble(redcap_event_name = character(), instrument = character()))
  }
  event_col <- .schema_resolve_column(data, c("event_name", "unique_event_name", "redcap_event_name"), "repeat configuration", FALSE)
  form_col <- .schema_resolve_column(data, c("form_name", "form", "instrument", "instrument_name"), "repeat configuration", FALSE)
  event <- if (is.null(event_col)) rep(NA_character_, nrow(data)) else .schema_normalize_nullable_character(data[[event_col]], "repeat event names")
  instrument <- if (is.null(form_col)) rep(NA_character_, nrow(data)) else .schema_normalize_nullable_character(data[[form_col]], "repeat instruments")
  out <- tibble::tibble(redcap_event_name = event, instrument = instrument)
  if (isTRUE(longitudinal) && anyNA(event)) {
    .condition_signal_error("Every longitudinal repeat row requires a raw event name.", "project")
  }
  if (!isTRUE(longitudinal) && (any(!is.na(event)) || any(is.na(instrument)))) {
    .condition_signal_error("Classic repeat rows require an instrument and no event.", "project")
  }
  if (anyDuplicated(out)) .condition_signal_error("The repeat configuration contains duplicate crossings.", "project")
  out
}

.project_structure_resolve_record_id_field <- function(metadata, project_information) {
  info_column <- c("record_id_field", "record_id_field_name")
  info_column <- info_column[info_column %in% names(project_information)]
  if (length(info_column)) {
    value <- .schema_normalize_required_id(
      project_information[[info_column[[1]]]],
      "project information record ID field"
    )
    if (length(value) != 1 || !value %in% metadata$field_name) {
      .condition_signal_error("The project information record ID field is invalid.", "project")
    }
    return(value[[1]])
  }
  attribute_value <- attr(metadata, "record_id_field", exact = TRUE)
  if (!is.null(attribute_value)) {
    value <- .schema_normalize_required_id(attribute_value, "metadata record ID attribute")
    if (length(value) != 1 || !value %in% metadata$field_name) {
      .condition_signal_error("The metadata record ID attribute is invalid.", "project")
    }
    return(value[[1]])
  }
  order_column <- c("field_order", "field_order_number")
  order_column <- order_column[order_column %in% names(metadata)]
  if (length(order_column)) {
    ordering <- suppressWarnings(as.numeric(metadata[[order_column[[1]]]]))
    if (length(ordering) == nrow(metadata) && all(is.finite(ordering)) && !anyDuplicated(ordering)) {
      return(metadata$field_name[[which.min(ordering)]])
    }
    .condition_signal_error("Metadata field order is not a unique finite ordering.", "project")
  }
  # redcapAPI metadata follows REDCap field order; the first field is the
  # REDCap record ID field when no explicit identity surface is supplied.
  metadata$field_name[[1]]
}

.project_structure_detect_present_rows <- function(data, table, columns) {
  if (!nrow(data)) return(logical())
  if (!nrow(table)) return(rep(FALSE, nrow(data)))
  candidate <- as.data.frame(data[, columns, drop = FALSE])
  candidate$.input_row <- seq_len(nrow(candidate))
  lookup <- unique(as.data.frame(table[, columns, drop = FALSE]))
  lookup$.present <- TRUE
  matched <- merge(
    candidate,
    lookup,
    by = columns,
    all.x = TRUE,
    sort = FALSE
  )
  present <- rep(FALSE, nrow(candidate))
  present[matched$.input_row] <- !is.na(matched$.present)
  present
}

.project_structure_build_allowable_crossings <- function(instruments, mapping, repeat_configuration, longitudinal) {
  if (!isTRUE(longitudinal)) {
    return(tibble::tibble(
      instrument = instruments$instrument,
      redcap_event_name = rep(NA_character_, nrow(instruments)),
      arm_num = rep(NA_character_, nrow(instruments)),
      repeat_mode = ifelse(
        instruments$instrument %in% repeat_configuration$instrument,
        "repeating_instrument",
        "no_repeat"
      )
    ))
  }
  repeating_events <- repeat_configuration$redcap_event_name[is.na(repeat_configuration$instrument)]
  repeating_instruments <- repeat_configuration[
    !is.na(repeat_configuration$instrument),
    c("redcap_event_name", "instrument"),
    drop = FALSE
  ]
  mapping_repeats <- .project_structure_detect_present_rows(
    mapping, repeating_instruments,
    c("redcap_event_name", "instrument")
  )
  tibble::tibble(
    instrument = mapping$instrument,
    redcap_event_name = mapping$redcap_event_name,
    arm_num = mapping$arm_num,
    repeat_mode = ifelse(
      mapping$redcap_event_name %in% repeating_events,
      "repeating_event",
      ifelse(mapping_repeats, "repeating_instrument", "no_repeat")
    )
  )
}

.project_structure_build_snapshot <- function(rcon) {
  .rcon_validate_class(rcon)
  metadata <- .project_structure_read_surface(rcon, "metadata", "project metadata")
  .schema_require_columns(metadata, c("field_name", "form_name", "field_type"), "rcon$metadata()")
  if (!nrow(metadata)) .condition_signal_error("`rcon$metadata()` cannot be empty.", "project")
  metadata$field_name <- .schema_normalize_required_id(metadata$field_name, "metadata field names")
  metadata$form_name <- .schema_normalize_required_id(metadata$form_name, "metadata form names")
  metadata$field_type <- .schema_normalize_character(metadata$field_type, "metadata field types")
  invalid_field_type <- is.na(metadata$field_type) | metadata$field_type == "" |
    trimws(metadata$field_type) != metadata$field_type
  if (any(invalid_field_type)) {
    .condition_signal_error("Metadata field types must be nonblank and unpadded.", "schema")
  }
  if (anyDuplicated(metadata$field_name)) .condition_signal_error("Metadata field names must be unique.", "project")
  .metadata_validate_checkbox_fields(metadata)
  instruments <- .project_structure_read_instruments(rcon)
  info <- .project_structure_read_surface(rcon, c("projectInformation", "project_information", "projectInfo"), "project information")
  .schema_require_columns(info, c("project_id", "is_longitudinal"), "rcon$projectInformation()")
  if (nrow(info) != 1) .condition_signal_error("Project information must contain exactly one row.", "project")
  project_id <- .schema_normalize_required_id(info$project_id, "project ID")[[1]]
  longitudinal <- .project_structure_normalize_longitudinal(info$is_longitudinal[[1]])
  repeat_configuration <- .project_structure_normalize_repeats(.project_structure_read_surface(
    rcon,
    c("repeatInstrumentEvent", "repeat_instrument", "repeatInstrumentsEvents", "repeatingInstrumentsEvents", "repeat_instruments_events"),
    "repeat configuration"
  ), longitudinal)
  if ("has_repeating_instruments_or_events" %in% names(info)) {
    flag <- tolower(as.character(info$has_repeating_instruments_or_events[[1L]]))
    if (!flag %in% c("0", "1", "false", "true")) {
      .condition_signal_error("Project repeat status information must be 0/1 or FALSE/TRUE.", "project")
    }
    flagged_repeating <- flag %in% c("1", "true")
    if (!identical(flagged_repeating, nrow(repeat_configuration) > 0L)) {
      .condition_signal_error("Project repeat status information contradicts the repeat configuration.", "project")
    }
  }
  if (isTRUE(longitudinal)) {
    mapping <- .project_structure_normalize_mapping(.project_structure_read_surface(rcon, c("mapping", "mappings"), "instrument to event mappings"))
    events <- .project_structure_normalize_events(.project_structure_read_surface(rcon, c("events", "exportEvents", "event_data", "eventData"), "events"), mapping)
    arms <- .project_structure_normalize_arms(.project_structure_read_surface(rcon, c("arms", "exportArms", "arm_data", "armData"), "arms"))
  } else {
    mapping <- tibble::tibble(arm_num = character(), redcap_event_name = character(), instrument = character())
    events <- tibble::tibble(redcap_event_name = character(), event_label = character(), event_id = character(), arm_num = character())
    arms <- tibble::tibble(arm_num = character(), arm_name = character())
  }
  if (length(setdiff(unique(metadata$form_name), instruments$instrument))) {
    .condition_signal_error("Metadata contains an unknown instrument.", "project")
  }
  if (!isTRUE(longitudinal)) {
    if (length(setdiff(repeat_configuration$instrument, instruments$instrument))) .condition_signal_error("Repeat configuration contains an unknown instrument.", "project")
  } else {
    event_arm <- stats::setNames(events$arm_num, events$redcap_event_name)
    invalid_mapping <- length(setdiff(mapping$instrument, instruments$instrument)) ||
      length(setdiff(mapping$redcap_event_name, events$redcap_event_name)) ||
      length(setdiff(events$arm_num, arms$arm_num)) ||
      any(mapping$arm_num != unname(event_arm[mapping$redcap_event_name]))
    if (invalid_mapping) .condition_signal_error("The event mapping contains an unknown or contradictory crossing.", "project")
    if (length(setdiff(repeat_configuration$redcap_event_name, events$redcap_event_name))) .condition_signal_error("Repeat configuration contains an unknown event.", "project")
    repeating_forms <- !is.na(repeat_configuration$instrument)
    if (any(repeating_forms)) {
      repeating_crossings <- repeat_configuration[
        repeating_forms,
        c("redcap_event_name", "instrument"),
        drop = FALSE
      ]
      mapped <- .project_structure_detect_present_rows(
        repeating_crossings, mapping,
        c("redcap_event_name", "instrument")
      )
      if (any(!mapped)) .condition_signal_error(
        "A repeating instrument is not mapped to its event.",
        "project"
      )
    }
  }
  record_id_field <- .project_structure_resolve_record_id_field(metadata, info)
  instrument_labels <- instruments$instrument_label
  instrument_labels[is.na(instrument_labels)] <- instruments$instrument[is.na(instrument_labels)]
  instrument_labels <- stats::setNames(instrument_labels, instruments$instrument)
  instrument_labels <- instrument_labels[order(names(instrument_labels))]
  event_labels <- events$event_label
  event_labels[is.na(event_labels)] <- events$redcap_event_name[is.na(event_labels)]
  event_labels <- stats::setNames(event_labels, events$redcap_event_name)
  event_labels <- event_labels[order(names(event_labels))]
  project <- list(
    project_id = project_id,
    record_id_field = record_id_field,
    longitudinal = longitudinal,
    event_labels = event_labels,
    instrument_labels = instrument_labels
  )
  allowable_crossings <- .project_structure_build_allowable_crossings(
    instruments,
    mapping,
    repeat_configuration,
    longitudinal
  )
  physical_contexts <- tibble::tibble(
    redcap_event_name = allowable_crossings$redcap_event_name,
    repeat_mode = allowable_crossings$repeat_mode,
    context_instrument = ifelse(
      allowable_crossings$repeat_mode == "repeating_instrument",
      allowable_crossings$instrument,
      NA_character_
    )
  )
  physical_contexts <- tibble::as_tibble(unique(as.data.frame(
    physical_contexts,
    stringsAsFactors = FALSE
  )))
  event_arms <- events[, c("redcap_event_name", "arm_num"), drop = FALSE]
  list(
    project = project, metadata = metadata, instruments = instruments,
    arms = arms, events = events, mapping = mapping,
    repeat_configuration = repeat_configuration,
    allowable_crossings = allowable_crossings,
    physical_contexts = physical_contexts,
    event_arms = event_arms,
    event_order = events$redcap_event_name[order(as.integer(events$event_id))],
    instrument_order = instruments$instrument,
    structure_fingerprint = .structure_fingerprint_compute_digest(
      project,
      metadata,
      instruments,
      arms,
      events,
      mapping,
      repeat_configuration
    )
  )
}
