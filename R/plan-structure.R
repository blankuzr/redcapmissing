## Internal helpers: plan construction and structural normalization ---------

.rcm_plan_abort <- function(message, subclass = "argument") {
  condition <- structure(
    list(message = as.character(message), call = NULL),
    class = c(
      paste0("redcapmissing_error_", subclass),
      "redcapmissing_error", "error", "condition"
    )
  )
  stop(condition)
}

.rcm_plan_warn_empty_arm <- function(events) {
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

.rcm_surface <- function(rcon, methods, label, required = TRUE) {
  if (is.null(rcon)) {
    .rcm_plan_abort("Provide a REDCap connection-like object as `rcon`.", "project")
  }
  for (method in methods) {
    candidate <- tryCatch(rcon[[method]], error = function(e) NULL)
    if (is.null(candidate)) next
    value <- if (is.function(candidate)) {
      tryCatch(candidate(), error = function(e) {
        .rcm_plan_abort(
          paste0("`rcon$", method, "()` failed while reading ", label, ": ", conditionMessage(e)),
          "project"
        )
      })
    } else candidate
    if (is.null(value)) {
      .rcm_plan_abort(
        paste0(
          "The ", label, " surface returned `NULL`; supply an explicit ",
          "data frame, including a zero-row data frame for an empty structure."
        ),
        "project"
      )
    }
    if (!is.data.frame(value)) {
      .rcm_plan_abort(paste0("The ", label, " surface must be a data frame."), "project")
    }
    if (is.null(names(value)) || anyNA(names(value)) ||
        any(names(value) == "") || anyDuplicated(names(value))) {
      .rcm_plan_abort(
        paste0("The ", label, " surface must have unique, nonblank column names."),
        "project"
      )
    }
    return(tibble::as_tibble(value))
  }
  if (isTRUE(required)) {
    .rcm_plan_abort(
      paste0("`rcon` must provide ", label, " through: ", paste0("`", methods, "()`", collapse = ", "), "."),
      "project"
    )
  }
  tibble::tibble()
}

.rcm_required_columns <- function(data, columns, source) {
  absent <- setdiff(columns, names(data))
  if (length(absent)) {
    .rcm_plan_abort(
      paste0("`", source, "` is missing required column(s): ", paste(absent, collapse = ", "), "."),
      "schema"
    )
  }
  invisible(data)
}

.rcm_char <- function(x, source) {
  if (is.character(x)) return(x)
  if (is.factor(x)) return(as.character(x))
  .rcm_plan_abort(paste0("`", source, "` must use character or factor storage."), "schema")
}

.rcm_numeric_character <- function(x) {
  vapply(
    x,
    format,
    character(1),
    scientific = FALSE,
    trim = TRUE,
    digits = 17L,
    decimal.mark = ".",
    big.mark = "",
    small.mark = ""
  )
}

.rcm_required_id <- function(x, source) {
  valid <- is.character(x) || is.factor(x) || (is.numeric(x) && !is.logical(x))
  if (!valid || inherits(x, c("Date", "POSIXt"))) {
    .rcm_plan_abort(
      paste0("`", source, "` must use character, factor, integer, or numeric storage."),
      "schema"
    )
  }
  if (is.numeric(x) && any(is.na(x) | is.nan(x) | !is.finite(x))) {
    .rcm_plan_abort(paste0("`", source, "` cannot contain missing or non-finite values."), "schema")
  }
  value <- if (is.factor(x)) {
    as.character(x)
  } else if (is.numeric(x)) {
    .rcm_numeric_character(x)
  } else {
    x
  }
  invalid <- is.na(value) | value == "" | grepl("^\\s+$", value) | trimws(value) != value
  if (any(invalid)) {
    .rcm_plan_abort(
      paste0("`", source, "` requires non-missing, non-blank identifiers without surrounding whitespace."),
      "schema"
    )
  }
  value
}

.rcm_nullable_chr <- function(x, source) {
  if (!is.character(x) && !is.factor(x)) {
    all_typed_missing <- is.atomic(x) &&
      !inherits(x, c("Date", "POSIXt")) &&
      (length(x) > 0 && all(is.na(x)) && !(is.numeric(x) && any(is.nan(x))))
    if (all_typed_missing) return(rep(NA_character_, length(x)))
    .rcm_plan_abort(
      paste0("`", source, "` must use character/factor storage or contain only typed NA values."),
      "schema"
    )
  }
  value <- if (is.factor(x)) as.character(x) else x
  blank <- is.na(value) | grepl("^\\s*$", value)
  if (any(!blank & trimws(value) != value)) {
    .rcm_plan_abort(paste0("`", source, "` cannot contain surrounding whitespace."), "schema")
  }
  value[blank] <- NA_character_
  value
}

.rcm_instance <- function(x, source) {
  if (is.logical(x) && length(x) > 0 && all(is.na(x))) {
    return(rep(NA_integer_, length(x)))
  }
  if (is.factor(x)) {
    value <- as.character(x)
    if (length(value) > 0 && all(is.na(value))) {
      return(rep(NA_integer_, length(value)))
    }
    .rcm_plan_abort(
      paste0("`", source, "` may use factor storage only when every value is missing."),
      "schema"
    )
  }
  valid <- is.character(x) || (is.numeric(x) && !is.logical(x))
  if (!valid || inherits(x, c("Date", "POSIXt"))) {
    .rcm_plan_abort(
      paste0("`", source, "` must use character, factor, integer, or numeric storage."),
      "schema"
    )
  }
  if (!length(x)) return(integer())
  if (is.numeric(x)) {
    if (any(is.nan(x))) {
      .rcm_plan_abort(paste0("`", source, "` cannot contain NaN."), "schema")
    }
    missing <- is.na(x)
    bad <- !missing & (!is.finite(x) | x < 1 | x != floor(x) | x > .Machine$integer.max)
    if (any(bad)) {
      .rcm_plan_abort(paste0("`", source, "` requires positive whole-number IDs within integer range."), "schema")
    }
    out <- rep(NA_integer_, length(x))
    out[!missing] <- as.integer(x[!missing])
    return(out)
  }
  value <- if (is.factor(x)) as.character(x) else x
  blank <- is.na(value) | grepl("^\\s*$", value)
  canonical <- blank | grepl("^[1-9][0-9]*$", value)
  number <- suppressWarnings(as.numeric(value))
  bad <- (!blank & trimws(value) != value) | !canonical | (!blank & (!is.finite(number) | number > .Machine$integer.max))
  if (any(bad)) {
    .rcm_plan_abort(paste0("`", source, "` requires canonical positive integer IDs or missing values."), "schema")
  }
  out <- rep(NA_integer_, length(value))
  out[!blank] <- as.integer(number[!blank])
  out
}

.rcm_longitudinal <- function(x) {
  if (length(x) != 1 || is.na(x)) {
    .rcm_plan_abort("Project information must contain one valid `is_longitudinal` value.", "project")
  }
  value <- tolower(as.character(x))
  if (value %in% c("1", "true")) return(TRUE)
  if (value %in% c("0", "false")) return(FALSE)
  .rcm_plan_abort("`is_longitudinal` must be 0/1 or FALSE/TRUE.", "project")
}

.rcm_column <- function(data, choices, source, required = TRUE) {
  found <- choices[choices %in% names(data)]
  if (length(found)) return(found[[1]])
  if (isTRUE(required)) {
    .rcm_plan_abort(
      paste0("`", source, "` must provide one of: ", paste0("`", choices, "`", collapse = ", "), "."),
      "project"
    )
  }
  NULL
}

.rcm_instruments <- function(data) {
  name_col <- .rcm_column(data, c("instrument_name", "form_name", "form"), "rcon$instruments()")
  label_col <- .rcm_column(data, c("instrument_label", "form_label"), "rcon$instruments()", FALSE)
  instrument <- .rcm_required_id(data[[name_col]], "rcon$instruments() instrument names")
  if (!length(instrument) || anyDuplicated(instrument)) {
    .rcm_plan_abort("`rcon$instruments()` must contain unique instruments.", "project")
  }
  label <- if (is.null(label_col)) instrument else .rcm_nullable_chr(data[[label_col]], "instrument labels")
  tibble::tibble(instrument = instrument, instrument_label = label)
}

.rcm_mapping <- function(data) {
  event_col <- .rcm_column(data, c("unique_event_name", "event_name"), "rcon mapping")
  form_col <- .rcm_column(data, c("form", "form_name", "instrument", "instrument_name"), "rcon mapping")
  arm_col <- .rcm_column(data, c("arm_num", "arm_number"), "rcon mapping")
  out <- tibble::tibble(
    arm_num = .rcm_required_id(data[[arm_col]], "mapping arm numbers"),
    redcap_event_name = .rcm_required_id(data[[event_col]], "mapping event names"),
    instrument = .rcm_required_id(data[[form_col]], "mapping instruments")
  )
  if (!nrow(out) || anyDuplicated(out)) {
    .rcm_plan_abort("The longitudinal instrument-event mapping must be nonempty and unique.", "project")
  }
  out
}

.rcm_events <- function(data, mapping) {
  event_col <- .rcm_column(data, c("unique_event_name", "event_name"), "rcon$events()")
  event <- .rcm_required_id(data[[event_col]], "rcon$events() raw event names")
  if (!length(event) || anyDuplicated(event)) {
    .rcm_plan_abort("`rcon$events()` must contain unique raw event names.", "project")
  }
  arm_col <- .rcm_column(data, c("arm_num", "arm_number"), "rcon$events()", FALSE)
  arm <- if (is.null(arm_col)) rep(NA_character_, length(event)) else .rcm_required_id(data[[arm_col]], "event arm numbers")
  for (i in which(is.na(arm))) {
    candidate <- unique(mapping$arm_num[mapping$redcap_event_name == event[[i]]])
    if (length(candidate) == 1) arm[[i]] <- candidate
  }
  if (anyNA(arm)) .rcm_plan_abort("Every longitudinal event must map to exactly one arm.", "project")
  id_col <- .rcm_column(data, "event_id", "rcon$events()")
  event_id_input <- data[[id_col]]
  if (is.factor(event_id_input)) event_id_input <- as.character(event_id_input)
  event_id <- .rcm_instance(event_id_input, "rcon$events() event IDs")
  if (anyNA(event_id) || anyDuplicated(event_id)) {
    .rcm_plan_abort("Longitudinal event IDs must be unique positive integers.", "project")
  }
  event_id <- as.character(event_id)
  label <- if (all(c("unique_event_name", "event_name") %in% names(data))) {
    .rcm_nullable_chr(data$event_name, "event labels")
  } else event
  tibble::tibble(redcap_event_name = event, event_label = label, event_id = event_id, arm_num = arm)
}

.rcm_arms <- function(data) {
  arm_col <- .rcm_column(data, c("arm_num", "arm_number"), "rcon$arms()")
  arm <- .rcm_required_id(data[[arm_col]], "rcon$arms() arm numbers")
  if (!length(arm) || anyDuplicated(arm)) .rcm_plan_abort("`rcon$arms()` must contain unique arms.", "project")
  name_col <- .rcm_column(data, c("name", "arm_name"), "rcon$arms()", FALSE)
  name <- if (is.null(name_col)) rep(NA_character_, length(arm)) else .rcm_nullable_chr(data[[name_col]], "arm names")
  tibble::tibble(arm_num = arm, arm_name = name)
}

.rcm_repeat <- function(data, longitudinal) {
  if (!nrow(data) && !ncol(data)) {
    return(tibble::tibble(redcap_event_name = character(), instrument = character()))
  }
  event_col <- .rcm_column(data, c("event_name", "unique_event_name", "redcap_event_name"), "repeat configuration", FALSE)
  form_col <- .rcm_column(data, c("form_name", "form", "instrument", "instrument_name"), "repeat configuration", FALSE)
  event <- if (is.null(event_col)) rep(NA_character_, nrow(data)) else .rcm_nullable_chr(data[[event_col]], "repeat event names")
  instrument <- if (is.null(form_col)) rep(NA_character_, nrow(data)) else .rcm_nullable_chr(data[[form_col]], "repeat instruments")
  out <- tibble::tibble(redcap_event_name = event, instrument = instrument)
  if (isTRUE(longitudinal) && anyNA(event)) {
    .rcm_plan_abort("Every longitudinal repeat row requires a raw event name.", "project")
  }
  if (!isTRUE(longitudinal) && (any(!is.na(event)) || any(is.na(instrument)))) {
    .rcm_plan_abort("Classic repeat rows require an instrument and no event.", "project")
  }
  if (anyDuplicated(out)) .rcm_plan_abort("The repeat configuration contains duplicate crossings.", "project")
  out
}

.rcm_record_id_field <- function(metadata, project_information) {
  info_column <- c("record_id_field", "record_id_field_name")
  info_column <- info_column[info_column %in% names(project_information)]
  if (length(info_column)) {
    value <- .rcm_required_id(
      project_information[[info_column[[1]]]],
      "project-information record-ID field"
    )
    if (length(value) != 1 || !value %in% metadata$field_name) {
      .rcm_plan_abort("The project-information record-ID field is invalid.", "project")
    }
    return(value[[1]])
  }
  attribute_value <- attr(metadata, "record_id_field", exact = TRUE)
  if (!is.null(attribute_value)) {
    value <- .rcm_required_id(attribute_value, "metadata record-ID attribute")
    if (length(value) != 1 || !value %in% metadata$field_name) {
      .rcm_plan_abort("The metadata record-ID attribute is invalid.", "project")
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
    .rcm_plan_abort("Metadata field order is not a unique finite ordering.", "project")
  }
  # redcapAPI metadata is canonically field-ordered; the first field is the
  # REDCap record-ID field when no explicit identity surface is supplied.
  metadata$field_name[[1]]
}
.rcm_canonical <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data <- data[, sort(names(data)), drop = FALSE]
  for (name in names(data)) {
    value <- data[[name]]
    out <- if (is.list(value)) {
      vapply(value, function(x) paste(as.character(x), collapse = "\u001f"), character(1))
    } else if (is.numeric(value) && !inherits(value, c("Date", "POSIXt"))) {
      .rcm_numeric_character(value)
    } else as.character(value)
    out[is.na(out)] <- "<NA>"
    data[[name]] <- out
  }
  if (nrow(data) > 1 && ncol(data)) data <- data[do.call(order, unname(data)), , drop = FALSE]
  rownames(data) <- NULL
  data
}

.rcm_allowable <- function(instruments, mapping, repeat_configuration, longitudinal) {
  if (!isTRUE(longitudinal)) {
    return(tibble::tibble(
      instrument = instruments$instrument,
      redcap_event_name = rep(NA_character_, nrow(instruments)),
      arm_num = rep(NA_character_, nrow(instruments)),
      repeat_mode = ifelse(instruments$instrument %in% repeat_configuration$instrument, "repeating_instrument", "regular")
    ))
  }
  repeating_events <- repeat_configuration$redcap_event_name[is.na(repeat_configuration$instrument)]
  repeat_pairs <- paste(repeat_configuration$redcap_event_name[!is.na(repeat_configuration$instrument)], repeat_configuration$instrument[!is.na(repeat_configuration$instrument)], sep = "\u001f")
  mapping_pairs <- paste(mapping$redcap_event_name, mapping$instrument, sep = "\u001f")
  tibble::tibble(
    instrument = mapping$instrument,
    redcap_event_name = mapping$redcap_event_name,
    arm_num = mapping$arm_num,
    repeat_mode = ifelse(
      mapping$redcap_event_name %in% repeating_events,
      "repeating_event",
      ifelse(mapping_pairs %in% repeat_pairs, "repeating_instrument", "regular")
    )
  )
}

.rcm_validate_checkbox_metadata <- function(metadata) {
  checkbox_rows <- which(metadata$field_type == "checkbox")
  if (!length(checkbox_rows)) return(invisible(metadata))
  choices_column <- "select_choices_or_calculations"
  if (!choices_column %in% names(metadata)) {
    .rcm_plan_abort(
      "Checkbox metadata requires `select_choices_or_calculations`.",
      "project"
    )
  }
  for (row in checkbox_rows) {
    field <- metadata$field_name[[row]]
    raw <- metadata[[choices_column]][[row]]
    if (.miss_is_blank_scalar(raw)) {
      .rcm_plan_abort(
        paste0("Checkbox field `", field, "` requires a nonblank choice definition."),
        "project"
      )
    }
    parts <- strsplit(as.character(raw), "\\s*\\|\\s*", perl = TRUE)[[1L]]
    choices <- .miss_parse_choices(raw)
    suffixes <- if (nrow(choices)) .miss_choice_suffix(choices$code) else character()
    invalid <- nrow(choices) != length(parts) || !nrow(choices) ||
      any(is.na(choices$code) | !nzchar(trimws(choices$code))) ||
      any(is.na(choices$label) | !nzchar(trimws(choices$label))) ||
      any(!nzchar(suffixes)) || anyDuplicated(suffixes)
    if (invalid) {
      .rcm_plan_abort(
        paste0("Checkbox field `", field, "` has an invalid or ambiguous choice definition."),
        "project"
      )
    }
  }
  invisible(metadata)
}
#' @keywords internal
.rcm_project_snapshot <- function(rcon) {
  metadata <- .rcm_surface(rcon, "metadata", "project metadata")
  .rcm_required_columns(metadata, c("field_name", "form_name", "field_type"), "rcon$metadata()")
  if (!nrow(metadata)) .rcm_plan_abort("`rcon$metadata()` cannot be empty.", "project")
  metadata$field_name <- .rcm_required_id(metadata$field_name, "metadata field names")
  metadata$form_name <- .rcm_required_id(metadata$form_name, "metadata form names")
  metadata$field_type <- .rcm_char(metadata$field_type, "metadata field types")
  invalid_field_type <- is.na(metadata$field_type) | metadata$field_type == "" |
    trimws(metadata$field_type) != metadata$field_type
  if (any(invalid_field_type)) {
    .rcm_plan_abort("Metadata field types must be nonblank and unpadded.", "schema")
  }
  if (anyDuplicated(metadata$field_name)) .rcm_plan_abort("Metadata field names must be unique.", "project")
  .rcm_validate_checkbox_metadata(metadata)
  instruments <- .rcm_instruments(.rcm_surface(rcon, "instruments", "project instruments"))
  info <- .rcm_surface(rcon, c("projectInformation", "project_information", "projectInfo"), "project information")
  .rcm_required_columns(info, c("project_id", "is_longitudinal"), "rcon$projectInformation()")
  if (nrow(info) != 1) .rcm_plan_abort("Project information must contain exactly one row.", "project")
  project_id <- .rcm_required_id(info$project_id, "project ID")[[1]]
  longitudinal <- .rcm_longitudinal(info$is_longitudinal[[1]])
  repeat_configuration <- .rcm_repeat(.rcm_surface(
    rcon,
    c("repeatInstrumentEvent", "repeat_instrument", "repeatInstrumentsEvents", "repeatingInstrumentsEvents", "repeat_instruments_events"),
    "repeat configuration"
  ), longitudinal)
  if ("has_repeating_instruments_or_events" %in% names(info)) {
    flag <- tolower(as.character(info$has_repeating_instruments_or_events[[1L]]))
    if (!flag %in% c("0", "1", "false", "true")) {
      .rcm_plan_abort("Project repeat-status information must be 0/1 or FALSE/TRUE.", "project")
    }
    flagged_repeating <- flag %in% c("1", "true")
    if (!identical(flagged_repeating, nrow(repeat_configuration) > 0L)) {
      .rcm_plan_abort("Project repeat-status information contradicts the repeat configuration.", "project")
    }
  }
  if (isTRUE(longitudinal)) {
    mapping <- .rcm_mapping(.rcm_surface(rcon, c("mapping", "mappings"), "instrument-event mappings"))
    events <- .rcm_events(.rcm_surface(rcon, c("events", "exportEvents", "event_data", "eventData"), "events"), mapping)
    arms <- .rcm_arms(.rcm_surface(rcon, c("arms", "exportArms", "arm_data", "armData"), "arms"))
  } else {
    mapping <- tibble::tibble(arm_num = character(), redcap_event_name = character(), instrument = character())
    events <- tibble::tibble(redcap_event_name = character(), event_label = character(), event_id = character(), arm_num = character())
    arms <- tibble::tibble(arm_num = character(), arm_name = character())
  }
  if (length(setdiff(unique(metadata$form_name), instruments$instrument))) {
    .rcm_plan_abort("Metadata contains an unknown instrument.", "project")
  }
  if (!isTRUE(longitudinal)) {
    if (length(setdiff(repeat_configuration$instrument, instruments$instrument))) .rcm_plan_abort("Repeat configuration contains an unknown instrument.", "project")
  } else {
    event_arm <- stats::setNames(events$arm_num, events$redcap_event_name)
    invalid_mapping <- length(setdiff(mapping$instrument, instruments$instrument)) ||
      length(setdiff(mapping$redcap_event_name, events$redcap_event_name)) ||
      length(setdiff(events$arm_num, arms$arm_num)) ||
      any(mapping$arm_num != unname(event_arm[mapping$redcap_event_name]))
    if (invalid_mapping) .rcm_plan_abort("The event mapping contains an unknown or contradictory crossing.", "project")
    if (length(setdiff(repeat_configuration$redcap_event_name, events$redcap_event_name))) .rcm_plan_abort("Repeat configuration contains an unknown event.", "project")
    repeating_forms <- !is.na(repeat_configuration$instrument)
    if (any(repeating_forms)) {
      repeat_pairs <- paste(repeat_configuration$redcap_event_name[repeating_forms], repeat_configuration$instrument[repeating_forms], sep = "\u001f")
      mapping_pairs <- paste(mapping$redcap_event_name, mapping$instrument, sep = "\u001f")
      if (length(setdiff(repeat_pairs, mapping_pairs))) .rcm_plan_abort("A repeating instrument is not mapped to its event.", "project")
    }
  }
  record_id_field <- .rcm_record_id_field(metadata, info)
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
  fingerprint_input <- list(
    project = project,
    metadata = .rcm_canonical(metadata), instruments = .rcm_canonical(instruments),
    arms = .rcm_canonical(arms), events = .rcm_canonical(events),
    mapping = .rcm_canonical(mapping), repeat_configuration = .rcm_canonical(repeat_configuration)
  )
  list(
    project = project, metadata = metadata, instruments = instruments,
    arms = arms, events = events, mapping = mapping,
    repeat_configuration = repeat_configuration,
    allowable_crossings = .rcm_allowable(instruments, mapping, repeat_configuration, longitudinal),
    event_order = events$redcap_event_name[order(as.integer(events$event_id))],
    instrument_order = instruments$instrument,
    structure_fingerprint = digest::digest(fingerprint_input, algo = "sha256", serialize = TRUE)
  )
}

.rcm_event_equal <- function(x, y) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}

.rcm_context_key <- function(record_id, event, repeat_instrument, instance) {
  part <- function(x) {
    out <- as.character(x)
    out[is.na(x)] <- "<NA>"
    out
  }
  paste(part(record_id), part(event), part(repeat_instrument), part(instance), sep = "\u001f")
}

.rcm_validate_physical_contexts <- function(data, snapshot) {
  allowable <- snapshot$allowable_crossings
  for (i in seq_len(nrow(data))) {
    event <- data$redcap_event_name[[i]]
    repeat_instrument <- data$redcap_repeat_instrument[[i]]
    instance <- data$redcap_repeat_instance[[i]]
    at_event <- .rcm_event_equal(allowable$redcap_event_name, event)
    valid <- if (!is.na(repeat_instrument)) {
      !is.na(instance) && any(
        at_event & allowable$instrument == repeat_instrument &
          allowable$repeat_mode == "repeating_instrument"
      )
    } else if (!is.na(instance)) {
      any(at_event & allowable$repeat_mode == "repeating_event")
    } else {
      any(at_event & allowable$repeat_mode == "regular")
    }
    if (!isTRUE(valid)) {
      .rcm_plan_abort(
        paste0("Data row ", i, " has a regular/repeating context not allowed by `rcon`."),
        "schema"
      )
    }
  }
  invisible(data)
}

#' Normalize exported REDCap rows for planning and execution
#'
#' @param data A REDCap record-export data frame.
#' @param snapshot A project snapshot from `.rcm_project_snapshot()`.
#' @param require_nonempty Whether zero rows are invalid.
#' @param response_columns Additional response columns that must be present.
#'
#' @return A tibble retaining response columns and canonical structural columns.
#' @keywords internal
.rcm_normalize_data <- function(
  data,
  snapshot,
  require_nonempty = FALSE,
  response_columns = NULL
) {
  if (!is.data.frame(data)) .rcm_plan_abort("`data` must be a data frame.", "argument")
  if (is.null(names(data)) || anyNA(names(data)) ||
      any(names(data) == "") || anyDuplicated(names(data))) {
    .rcm_plan_abort("`data` must have unique, nonblank column names.", "schema")
  }
  atomic_columns <- vapply(
    data,
    function(column) is.atomic(column) && is.null(dim(column)),
    logical(1)
  )
  if (any(!atomic_columns)) {
    .rcm_plan_abort(
      paste0(
        "`data` columns must use ordinary atomic vector storage; invalid column(s): ",
        paste(names(data)[!atomic_columns], collapse = ", "),
        "."
      ),
      "schema"
    )
  }
  if (!is.logical(require_nonempty) || length(require_nonempty) != 1 || is.na(require_nonempty)) {
    .rcm_plan_abort("`require_nonempty` must be TRUE or FALSE.", "argument")
  }
  if (isTRUE(require_nonempty) && !nrow(data)) .rcm_plan_abort("`data` must contain at least one row.", "schema")
  if (is.null(response_columns)) response_columns <- character()
  if (!is.character(response_columns) || anyNA(response_columns)) {
    .rcm_plan_abort("`response_columns` must be a character vector.", "argument")
  }
  .rcm_required_columns(data, c(snapshot$project$record_id_field, response_columns), "data")
  repeat_columns <- c("redcap_repeat_instrument", "redcap_repeat_instance")
  repeat_present <- repeat_columns %in% names(data)
  if ((nrow(snapshot$repeat_configuration) > 0 && !all(repeat_present)) ||
      (any(repeat_present) && !all(repeat_present))) {
    .rcm_plan_abort(
      "`data` must provide `redcap_repeat_instrument` and `redcap_repeat_instance` together.",
      "schema"
    )
  }
  record_id <- .rcm_required_id(
    data[[snapshot$project$record_id_field]],
    paste0("data$", snapshot$project$record_id_field)
  )
  if (isTRUE(snapshot$project$longitudinal)) {
    .rcm_required_columns(data, "redcap_event_name", "data")
    event <- .rcm_nullable_chr(data$redcap_event_name, "data$redcap_event_name")
    if (anyNA(event)) .rcm_plan_abort("Longitudinal event names cannot be missing or blank.", "schema")
  } else if ("redcap_event_name" %in% names(data)) {
    event <- .rcm_nullable_chr(data$redcap_event_name, "data$redcap_event_name")
    if (any(!is.na(event))) .rcm_plan_abort("Classic-project event values must be missing or blank.", "schema")
  } else {
    event <- rep(NA_character_, nrow(data))
  }
  repeat_instrument <- if (all(repeat_present)) {
    .rcm_nullable_chr(data$redcap_repeat_instrument, "data$redcap_repeat_instrument")
  } else rep(NA_character_, nrow(data))
  repeat_instance <- if (all(repeat_present)) {
    .rcm_instance(data$redcap_repeat_instance, "data$redcap_repeat_instance")
  } else rep(NA_integer_, nrow(data))
  if (any(!is.na(repeat_instrument) & is.na(repeat_instance))) {
    .rcm_plan_abort("A repeating instrument requires a positive repeat instance.", "schema")
  }
  normalized <- tibble::as_tibble(data)
  normalized$.rcm_record_id <- record_id
  normalized$redcap_event_name <- event
  normalized$redcap_repeat_instrument <- repeat_instrument
  normalized$redcap_repeat_instance <- repeat_instance
  key <- .rcm_context_key(record_id, event, repeat_instrument, repeat_instance)
  if (anyDuplicated(key)) .rcm_plan_abort("`data` contains duplicate normalized physical row keys.", "schema")
  .rcm_validate_physical_contexts(normalized, snapshot)
  normalized
}

.rcm_record_arms <- function(data, snapshot) {
  records <- unique(data$.rcm_record_id)
  if (!isTRUE(snapshot$project$longitudinal)) {
    return(tibble::tibble(record_id = records, arm_num = rep(NA_character_, length(records))))
  }
  event_arm <- stats::setNames(snapshot$events$arm_num, snapshot$events$redcap_event_name)
  arms <- unname(event_arm[data$redcap_event_name])
  result <- vector("list", length(records))
  for (i in seq_along(records)) {
    candidate <- unique(arms[data$.rcm_record_id == records[[i]]])
    candidate <- candidate[!is.na(candidate)]
    if (length(candidate) != 1) {
      .rcm_plan_abort(paste0("Record `", records[[i]], "` is observed in more than one arm."), "schema")
    }
    result[[i]] <- tibble::tibble(record_id = records[[i]], arm_num = candidate[[1]])
  }
  dplyr::bind_rows(result)
}

.rcm_selected_instruments <- function(instruments, snapshot) {
  if (missing(instruments) || is.null(instruments) || !length(instruments)) {
    .rcm_plan_abort("`instruments` must contain at least one REDCap instrument name.", "argument")
  }
  if (!is.character(instruments)) {
    .rcm_plan_abort("`instruments` must be a character vector.", "argument")
  }
  invalid <- is.na(instruments) | instruments == "" | grepl("^\\s+$", instruments) | trimws(instruments) != instruments
  if (any(invalid) || anyDuplicated(instruments)) {
    .rcm_plan_abort("`instruments` must contain unique, non-blank, unpadded names.", "argument")
  }
  unknown <- setdiff(instruments, snapshot$instrument_order)
  if (length(unknown)) {
    .rcm_plan_abort(paste0("Unknown `instruments`: ", paste(unknown, collapse = ", "), "."), "schedule")
  }
  instruments
}

.rcm_schedule_columns <- function(type) {
  if (identical(type, "extended")) {
    c("instrument", "redcap_event_name", "repeat_instance")
  } else {
    c("record_id", "instrument", "redcap_event_name", "repeat_instance")
  }
}

.rcm_normalize_schedule <- function(schedule, type, snapshot, instruments, data) {
  source <- paste0(type, "_schedule")
  expected <- .rcm_schedule_columns(type)
  if (!is.data.frame(schedule)) .rcm_plan_abort(paste0("`", source, "` must be a data frame."), "argument")
  if (!identical(names(schedule), expected)) {
    .rcm_plan_abort(
      paste0("`", source, "` must contain exactly these columns in order: ", paste(expected, collapse = ", "), "."),
      "schedule"
    )
  }
  instrument <- .rcm_char(schedule$instrument, paste0(source, "$instrument"))
  bad_instrument <- is.na(instrument) | instrument == "" | grepl("^\\s+$", instrument) | trimws(instrument) != instrument
  if (any(bad_instrument)) .rcm_plan_abort(paste0("`", source, "$instrument` must be non-missing and unpadded."), "schedule")
  unknown <- setdiff(unique(instrument), instruments)
  if (length(unknown)) .rcm_plan_abort(paste0("`", source, "$instrument` must be a subset of `instruments`."), "schedule")
  event <- .rcm_nullable_chr(schedule$redcap_event_name, paste0(source, "$redcap_event_name"))
  if (isTRUE(snapshot$project$longitudinal) && anyNA(event)) {
    .rcm_plan_abort(paste0("`", source, "$redcap_event_name` requires raw event names."), "schedule")
  }
  if (!isTRUE(snapshot$project$longitudinal) && any(!is.na(event))) {
    .rcm_plan_abort(paste0("`", source, "$redcap_event_name` must be missing in a classic project."), "schedule")
  }
  instance <- .rcm_instance(schedule$repeat_instance, paste0(source, "$repeat_instance"))
  record_id <- if (identical(type, "explicit")) {
    .rcm_required_id(schedule$record_id, "explicit_schedule$record_id")
  } else rep(NA_character_, nrow(schedule))
  out <- tibble::tibble(
    record_id = record_id,
    instrument = instrument,
    redcap_event_name = event,
    repeat_instance = instance,
    repeat_mode = rep(NA_character_, nrow(schedule))
  )
  allowable <- snapshot$allowable_crossings
  for (i in seq_len(nrow(out))) {
    row <- allowable[
      allowable$instrument == out$instrument[[i]] &
        .rcm_event_equal(allowable$redcap_event_name, out$redcap_event_name[[i]]),
      , drop = FALSE
    ]
    if (nrow(row) != 1) {
      .rcm_plan_abort(paste0("`", source, "` row ", i, " is not an allowable crossing."), "schedule")
    }
    out$repeat_mode[[i]] <- row$repeat_mode[[1]]
    requires_instance <- row$repeat_mode[[1]] != "regular"
    if (requires_instance == is.na(out$repeat_instance[[i]])) {
      .rcm_plan_abort(paste0("`", source, "` row ", i, " has an invalid instance specification."), "schedule")
    }
  }
  key <- paste(
    if (identical(type, "explicit")) out$record_id else "",
    out$instrument,
    ifelse(is.na(out$redcap_event_name), "<NA>", out$redcap_event_name),
    ifelse(is.na(out$repeat_instance), "<NA>", out$repeat_instance),
    sep = "\u001f"
  )
  if (anyDuplicated(key)) .rcm_plan_abort(paste0("`", source, "` contains duplicate normalized rows."), "schedule")
  if (identical(type, "explicit") && nrow(out) && isTRUE(snapshot$project$longitudinal) && nrow(data)) {
    record_arms <- .rcm_record_arms(data, snapshot)
    event_arm <- stats::setNames(snapshot$events$arm_num, snapshot$events$redcap_event_name)
    observed_arm <- record_arms$arm_num[match(out$record_id, record_arms$record_id)]
    scheduled_arm <- unname(event_arm[out$redcap_event_name])
    if (any(!is.na(observed_arm) & observed_arm != scheduled_arm)) {
      .rcm_plan_abort("An explicit target cannot place an observed record in a contradictory arm.", "schedule")
    }
  }
  out
}

.rcm_empty_targets <- function() {
  tibble::tibble(
    record_id = character(), instrument = character(),
    redcap_event_name = character(), repeat_instrument = character(),
    repeat_instance = integer(), target_source = character()
  )
}

.rcm_make_targets <- function(record_id, allowable, instance, source) {
  if (!nrow(allowable)) return(.rcm_empty_targets())
  tibble::tibble(
    record_id = rep(record_id, nrow(allowable)),
    instrument = allowable$instrument,
    redcap_event_name = allowable$redcap_event_name,
    repeat_instrument = ifelse(
      allowable$repeat_mode == "repeating_instrument",
      allowable$instrument,
      NA_character_
    ),
    repeat_instance = rep(as.integer(instance), nrow(allowable)),
    target_source = rep(source, nrow(allowable))
  )
}

.rcm_observed_targets <- function(data, snapshot, instruments) {
  allowable <- snapshot$allowable_crossings
  allowable <- allowable[allowable$instrument %in% instruments, , drop = FALSE]
  result <- vector("list", nrow(data))
  for (i in seq_len(nrow(data))) {
    at_event <- .rcm_event_equal(allowable$redcap_event_name, data$redcap_event_name[[i]])
    repeat_instrument <- data$redcap_repeat_instrument[[i]]
    instance <- data$redcap_repeat_instance[[i]]
    keep <- if (!is.na(repeat_instrument)) {
      at_event & allowable$repeat_mode == "repeating_instrument" & allowable$instrument == repeat_instrument
    } else if (!is.na(instance)) {
      at_event & allowable$repeat_mode == "repeating_event"
    } else {
      at_event & allowable$repeat_mode == "regular"
    }
    selected <- allowable[keep, , drop = FALSE]
    target_instance <- if (!nrow(selected) || all(selected$repeat_mode == "regular")) NA_integer_ else instance
    result[[i]] <- .rcm_make_targets(data$.rcm_record_id[[i]], selected, target_instance, "observed")
  }
  if (!length(result)) return(.rcm_empty_targets())
  dplyr::bind_rows(result)
}

.rcm_scheduled_targets <- function(schedule, snapshot, data, type) {
  if (!nrow(schedule)) return(.rcm_empty_targets())
  allowable <- snapshot$allowable_crossings
  record_arms <- .rcm_record_arms(data, snapshot)
  event_arm <- stats::setNames(snapshot$events$arm_num, snapshot$events$redcap_event_name)
  result <- list()
  empty_events <- character()
  index <- 0L
  for (i in seq_len(nrow(schedule))) {
    allowed <- allowable[
      allowable$instrument == schedule$instrument[[i]] &
        .rcm_event_equal(allowable$redcap_event_name, schedule$redcap_event_name[[i]]),
      , drop = FALSE
    ]
    records <- if (identical(type, "explicit")) {
      schedule$record_id[[i]]
    } else if (!isTRUE(snapshot$project$longitudinal)) {
      unique(data$.rcm_record_id)
    } else {
      arm <- unname(event_arm[schedule$redcap_event_name[[i]]])
      record_arms$record_id[record_arms$arm_num == arm]
    }
    if (!length(records)) {
      empty_events <- c(empty_events, ifelse(is.na(schedule$redcap_event_name[[i]]), "<classic>", schedule$redcap_event_name[[i]]))
      next
    }
    for (record in records) {
      index <- index + 1L
      result[[index]] <- .rcm_make_targets(
        record, allowed, schedule$repeat_instance[[i]],
        if (identical(type, "explicit")) "explicit" else "extended"
      )
    }
  }
  if (identical(type, "extended") && length(empty_events)) .rcm_plan_warn_empty_arm(empty_events)
  if (!length(result)) return(.rcm_empty_targets())
  dplyr::bind_rows(result)
}

.rcm_target_key <- function(x) {
  paste(
    x$record_id, x$instrument,
    ifelse(is.na(x$redcap_event_name), "<NA>", x$redcap_event_name),
    ifelse(is.na(x$repeat_instrument), "<NA>", x$repeat_instrument),
    ifelse(is.na(x$repeat_instance), "<NA>", x$repeat_instance),
    sep = "\u001f"
  )
}

.rcm_merge_targets <- function(observed, scheduled, construction) {
  combined <- dplyr::bind_rows(observed, scheduled)
  if (!nrow(combined)) return(.rcm_empty_targets())
  key <- .rcm_target_key(combined)
  unique_key <- unique(key)
  result <- vector("list", length(unique_key))
  for (i in seq_along(unique_key)) {
    rows <- combined[key == unique_key[[i]], , drop = FALSE]
    source <- unique(rows$target_source)
    row <- rows[1, , drop = FALSE]
    if (identical(construction, "from_data") && all(c("observed", "extended") %in% source)) {
      row$target_source <- "observed+extended"
    }
    result[[i]] <- row
  }
  dplyr::bind_rows(result)
}

.rcm_order_targets <- function(targets, snapshot, instruments) {
  if (!nrow(targets)) return(.rcm_empty_targets())
  instrument_order <- match(targets$instrument, instruments)
  event_order <- ifelse(is.na(targets$redcap_event_name), 0L, match(targets$redcap_event_name, snapshot$event_order))
  repeat_kind <- ifelse(is.na(targets$repeat_instance), 0L, ifelse(is.na(targets$repeat_instrument), 1L, 2L))
  ordering <- order(
    instrument_order, event_order, targets$record_id,
    repeat_kind, targets$repeat_instance, na.last = TRUE
  )
  targets <- targets[ordering, , drop = FALSE]
  rownames(targets) <- NULL
  tibble::as_tibble(targets)
}

.rcm_new_plan <- function(construction, instruments, targets, snapshot) {
  plan <- structure(
    list(
      schema_version = 1L,
      construction = construction,
      instruments = instruments,
      assessible_targets = targets,
      project = snapshot$project,
      structure_fingerprint = snapshot$structure_fingerprint
    ),
    class = "redcapmissing_plan"
  )
  .rcm_validate_plan(plan, snapshot)
}

#' Validate a redcapmissing assessment plan
#'
#' @param plan An object intended to represent a `redcapmissing_plan`.
#' @param snapshot An optional current project snapshot.
#'
#' @return The validated plan.
#' @keywords internal
.rcm_validate_plan <- function(plan, snapshot = NULL) {
  expected_names <- c(
    "schema_version", "construction", "instruments",
    "assessible_targets", "project", "structure_fingerprint"
  )
  if (!inherits(plan, "redcapmissing_plan") || !is.list(plan) || !identical(names(plan), expected_names)) {
    .rcm_plan_abort("`plan` is not a valid `redcapmissing_plan` representation.", "plan")
  }
  if (!identical(plan$schema_version, 1L) ||
      !is.character(plan$construction) ||
      length(plan$construction) != 1 ||
      !plan$construction %in% c("from_data", "explicit")) {
    .rcm_plan_abort("`plan` has an unsupported schema version or construction type.", "plan")
  }
  if (!is.character(plan$instruments) || !length(plan$instruments) || anyNA(plan$instruments) ||
      any(plan$instruments == "") || any(trimws(plan$instruments) != plan$instruments) ||
      anyDuplicated(plan$instruments)) {
    .rcm_plan_abort("`plan$instruments` is malformed.", "plan")
  }
  valid_label_map <- function(x) {
    is.character(x) && !anyNA(x) &&
      (!length(x) || (
        !is.null(names(x)) && !anyNA(names(x)) &&
          all(names(x) != "") &&
          all(trimws(names(x)) == names(x)) &&
          !anyDuplicated(names(x)) &&
          identical(names(x), sort(names(x)))
      ))
  }
  if (!is.list(plan$project) ||
      !identical(
        names(plan$project),
        c("project_id", "record_id_field", "longitudinal", "event_labels", "instrument_labels")
      ) ||
      !is.character(plan$project$project_id) || length(plan$project$project_id) != 1 ||
      is.na(plan$project$project_id) || plan$project$project_id == "" ||
      trimws(plan$project$project_id) != plan$project$project_id ||
      !is.character(plan$project$record_id_field) || length(plan$project$record_id_field) != 1 ||
      is.na(plan$project$record_id_field) || plan$project$record_id_field == "" ||
      trimws(plan$project$record_id_field) != plan$project$record_id_field ||
      !is.logical(plan$project$longitudinal) || length(plan$project$longitudinal) != 1 ||
      is.na(plan$project$longitudinal) ||
      !valid_label_map(plan$project$event_labels) ||
      !valid_label_map(plan$project$instrument_labels) ||
      (!plan$project$longitudinal && length(plan$project$event_labels)) ||
      (plan$project$longitudinal && !length(plan$project$event_labels)) ||
      length(setdiff(plan$instruments, names(plan$project$instrument_labels)))) {
    .rcm_plan_abort("`plan$project` is malformed.", "plan")
  }
  if (!is.character(plan$structure_fingerprint) || length(plan$structure_fingerprint) != 1 ||
      is.na(plan$structure_fingerprint) ||
      !grepl("^[0-9a-f]{64}$", plan$structure_fingerprint)) {
    .rcm_plan_abort("`plan$structure_fingerprint` is malformed.", "plan")
  }
  targets <- plan$assessible_targets
  target_names <- c(
    "record_id", "instrument", "redcap_event_name",
    "repeat_instrument", "repeat_instance", "target_source"
  )
  valid_schema <- is.data.frame(targets) && identical(names(targets), target_names) &&
    is.character(targets$record_id) && is.character(targets$instrument) &&
    is.character(targets$redcap_event_name) && is.character(targets$repeat_instrument) &&
    is.integer(targets$repeat_instance) && is.character(targets$target_source)
  if (!valid_schema) .rcm_plan_abort("`plan$assessible_targets` has an invalid schema.", "plan")
  if (nrow(targets)) {
    valid_sources <- if (identical(plan$construction, "explicit")) "explicit" else c("observed", "extended", "observed+extended")
    event_invalid <- if (isTRUE(plan$project$longitudinal)) {
      is.na(targets$redcap_event_name) |
        targets$redcap_event_name == "" |
        trimws(targets$redcap_event_name) != targets$redcap_event_name |
        !targets$redcap_event_name %in% names(plan$project$event_labels)
    } else {
      !is.na(targets$redcap_event_name)
    }
    repeat_name_invalid <- !is.na(targets$repeat_instrument) & (
      targets$repeat_instrument == "" |
        trimws(targets$repeat_instrument) != targets$repeat_instrument |
        targets$repeat_instrument != targets$instrument
    )
    repeat_shape_invalid <-
      (is.na(targets$repeat_instance) & !is.na(targets$repeat_instrument)) |
      (!is.na(targets$repeat_instance) & targets$repeat_instance < 1L)
    bad <- is.na(targets$record_id) | targets$record_id == "" |
      trimws(targets$record_id) != targets$record_id |
      is.na(targets$instrument) | !targets$instrument %in% plan$instruments |
      event_invalid | repeat_name_invalid | repeat_shape_invalid |
      is.na(targets$target_source) | !targets$target_source %in% valid_sources
    if (any(bad) || anyDuplicated(.rcm_target_key(targets))) {
      .rcm_plan_abort("`plan$assessible_targets` contains invalid or duplicate targets.", "plan")
    }
  }
  if (!is.null(snapshot)) {
    if (!identical(plan$project, snapshot$project) ||
        !identical(plan$structure_fingerprint, snapshot$structure_fingerprint)) {
      .rcm_plan_abort("`plan` and `rcon` do not represent the same unchanged project structure.", "plan")
    }
    if (length(setdiff(plan$instruments, snapshot$instrument_order))) {
      .rcm_plan_abort("`plan` contains unavailable instruments.", "plan")
    }
    ordered_targets <- .rcm_order_targets(targets, snapshot, plan$instruments)
    if (!identical(.rcm_target_key(targets), .rcm_target_key(ordered_targets))) {
      .rcm_plan_abort("`plan$assessible_targets` is not in deterministic target order.", "plan")
    }
    for (i in seq_len(nrow(targets))) {
      allowed <- snapshot$allowable_crossings[
        snapshot$allowable_crossings$instrument == targets$instrument[[i]] &
          .rcm_event_equal(snapshot$allowable_crossings$redcap_event_name, targets$redcap_event_name[[i]]),
        , drop = FALSE
      ]
      mode <- if (is.na(targets$repeat_instance[[i]])) {
        "regular"
      } else if (is.na(targets$repeat_instrument[[i]])) {
        "repeating_event"
      } else "repeating_instrument"
      if (nrow(allowed) != 1 || allowed$repeat_mode[[1]] != mode ||
          (mode == "repeating_instrument" && targets$repeat_instrument[[i]] != targets$instrument[[i]])) {
        .rcm_plan_abort("`plan` contains a target disallowed by current project structure.", "plan")
      }
    }
  }
  plan
}
