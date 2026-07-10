## Internal helpers: redcapAPI project structure ---------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Read metadata through the redcapAPI connection surface used by the package.
.miss_get_metadata <- function(rcon) {
  if (is.null(rcon)) {
    stop(
      "Provide a REDCap connection object with a `metadata()` method.",
      call. = FALSE
    )
  }
  if (is.null(rcon$metadata) || !is.function(rcon$metadata)) {
    stop(
      "`rcon` must provide metadata through `rcon$metadata()`.",
      call. = FALSE
    )
  }

  tibble::as_tibble(rcon$metadata())
}

.miss_check_metadata <- function(meta) {
  needed <- c(
    "field_name",
    "form_name",
    "field_type",
    "field_label",
    "select_choices_or_calculations",
    "branching_logic"
  )
  missing_cols <- setdiff(needed, names(meta))
  if (length(missing_cols) > 0) {
    stop(
      "rcon$metadata() is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(meta)
}

.miss_get_project <- function(rcon, meta, form, project_cache = NULL) {
  project_cache <- project_cache %||% .miss_get_project_cache(rcon = rcon)
  system_fields <- project_cache$system_fields
  form_label <- .miss_get_form_label(
    rcon = rcon,
    form = form,
    project_cache = project_cache
  )
  mapping <- project_cache$mapping
  repeat_instrument_event <- project_cache$repeat_instrument_event
  event_labels <- project_cache$event_labels
  project_information <- project_cache$project_information
  form_events <- .miss_form_events(mapping = mapping, form = form)
  repeat_form_events <- .miss_repeat_form_events(
    repeat_instrument_event,
    form = form
  )
  repeating_events <- .miss_repeating_events(repeat_instrument_event)

  list(
    id_col = meta$field_name[[1]],
    form_label = form_label,
    system_fields = system_fields,
    mapping = mapping,
    repeat_instrument_event = repeat_instrument_event,
    event_labels = event_labels,
    project_information = project_information,
    form_events = form_events,
    repeat_form_events = repeat_form_events,
    repeating_events = repeating_events,
    form_repeats = length(repeat_form_events) > 0 ||
      length(intersect(form_events, repeating_events)) > 0,
    events = NULL
  )
}

.miss_get_project_cache <- function(rcon) {
  system_fields <- .miss_system_fields()
  mapping <- .miss_normalize_mapping(.miss_get_rcon_table(
    rcon,
    c("mapping", "mappings")
  ))
  repeat_instrument_event <- .miss_normalize_repeat(
    .miss_get_rcon_table(
      rcon,
      c(
        "repeatInstrumentEvent",
        "repeat_instrument",
        "repeatInstrumentsEvents",
        "repeatingInstrumentsEvents",
        "repeat_instruments_events"
      )
    )
  )
  events <- .miss_normalize_events(.miss_get_rcon_table(
    rcon,
    c("events", "exportEvents", "event_data", "eventData")
  ))
  instruments <- .miss_get_instruments(rcon)
  project_information <- .miss_get_rcon_table(
    rcon,
    c("projectInformation", "project_information", "projectInfo")
  )

  list(
    system_fields = system_fields,
    mapping = mapping,
    repeat_instrument_event = repeat_instrument_event,
    events = events,
    instruments = instruments,
    event_labels = .miss_event_label_vector(events),
    project_information = project_information
  )
}

.miss_get_form_label <- function(rcon, form, project_cache = NULL) {
  instruments <- if (is.null(project_cache)) {
    .miss_get_instruments(rcon)
  } else {
    project_cache$instruments
  }
  needed <- c("instrument_name", "instrument_label")
  missing_cols <- setdiff(needed, names(instruments))
  if (length(missing_cols) > 0) {
    stop(
      "`rcon$instruments()` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  matches <- instruments[.miss_chr_vec(instruments$instrument_name) == form, , drop = FALSE]
  if (nrow(matches) != 1) {
    stop(
      "`rcon$instruments()` must contain exactly one row for form `",
      form,
      "`.",
      call. = FALSE
    )
  }

  form_label <- .miss_chr(matches$instrument_label[[1]])
  if (.miss_is_blank_scalar(form_label)) {
    stop(
      "`rcon$instruments()` must provide a non-blank `instrument_label` ",
      "for form `",
      form,
      "`.",
      call. = FALSE
    )
  }

  form_label
}

.miss_get_instruments <- function(rcon) {
  if (is.null(rcon$instruments) || !is.function(rcon$instruments)) {
    stop(
      "`rcon` must provide instrument labels through `rcon$instruments()`.",
      call. = FALSE
    )
  }

  instruments <- tibble::as_tibble(rcon$instruments())
  instruments
}

.miss_get_event_labels <- function(rcon) {
  event_data <- .miss_normalize_events(.miss_get_rcon_table(
    rcon,
    c("events", "exportEvents", "event_data", "eventData")
  ))
  .miss_event_label_vector(event_data)
}

.miss_get_project_event_names <- function(rcon = NULL, project_cache = NULL) {
  project_cache <- project_cache %||% .miss_get_project_cache(rcon = rcon)
  event_data <- project_cache$events
  mapping <- project_cache$mapping
  repeat_instrument_event <- project_cache$repeat_instrument_event

  event_names <- unique(c(
    event_data$unique_event_name,
    mapping$unique_event_name,
    repeat_instrument_event$event_name
  ))
  event_names[!.miss_is_blank_vec(event_names)]
}

.miss_resolve_events <- function(project, events, form) {
  offered_events <- union(project$form_events, project$repeat_form_events)
  offered_events <- unique(offered_events[!.miss_is_blank_vec(offered_events)])

  if (length(offered_events) <= 1) {
    project$events <- offered_events
    if (length(offered_events) > 0) {
      project$form_repeats <- .miss_project_has_repeat_contexts(project)
    }
    return(project)
  }

  if (is.null(events)) {
    project$events <- offered_events
    project$form_repeats <- .miss_project_has_repeat_contexts(project)
    return(project)
  }

  if (!is.character(events)) {
    stop("`events` must be NULL or a character vector of REDCap event names.", call. = FALSE)
  }

  events <- unique(.miss_chr_vec(events))
  events <- events[!.miss_is_blank_vec(events)]
  if (length(events) == 0) {
    stop("`events` must contain at least one non-blank REDCap event name.", call. = FALSE)
  }

  unknown_events <- setdiff(events, offered_events)
  if (length(unknown_events) > 0) {
    stop(
      "`events` must be a subset of the REDCap events where form `",
      form,
      "` is offered. Unknown event(s): ",
      paste(unknown_events, collapse = ", "),
      call. = FALSE
    )
  }

  project$form_events <- intersect(project$form_events, events)
  project$repeat_form_events <- intersect(project$repeat_form_events, events)
  project$events <- events
  project$form_repeats <- .miss_project_has_repeat_contexts(project)
  project
}

.miss_resolve_instances <- function(instances, project, form, explicit = FALSE) {
  if (!isTRUE(project$form_repeats)) {
    if (isTRUE(explicit) && !is.null(instances)) {
      stop(
        "`instances` was supplied for form `",
        form,
        "`, but that form does not include any requested REDCap repeating ",
        "event or instrument contexts.",
        call. = FALSE
      )
    }
    return(list(values = NULL, defaulted = FALSE))
  }

  if (is.null(instances)) {
    return(list(values = "1", defaulted = TRUE))
  }

  list(
    values = .miss_expand_instances(instances),
    defaulted = FALSE
  )
}

.miss_expand_instances <- function(instances) {
  if (!is.numeric(instances) && !is.character(instances)) {
    stop(
      "`instances` must be a positive whole-number scalar count or ",
      "a vector of positive whole-number instance IDs.",
      call. = FALSE
    )
  }

  instance_values <- .miss_chr_vec(instances)
  instance_values <- instance_values[!.miss_is_blank_vec(instance_values)]
  if (length(instance_values) == 0) {
    stop(
      "`instances` must contain at least one positive whole-number value.",
      call. = FALSE
    )
  }

  numeric_values <- suppressWarnings(as.numeric(instance_values))
  invalid <- is.na(numeric_values) |
    !is.finite(numeric_values) |
    numeric_values < 1 |
    numeric_values != floor(numeric_values)
  if (any(invalid)) {
    stop(
      "`instances` must contain only positive whole-number values.",
      call. = FALSE
    )
  }

  if (length(numeric_values) == 1) {
    return(as.character(seq_len(numeric_values[[1]])))
  }

  as.character(as.integer(numeric_values))
}

.miss_system_fields <- function() {
  list(
    event_col = "redcap_event_name",
    repeat_instrument_col = "redcap_repeat_instrument",
    repeat_instance_col = "redcap_repeat_instance"
  )
}

.miss_get_rcon_table <- function(rcon, methods) {
  for (method in methods) {
    candidate <- rcon[[method]]
    if (is.function(candidate)) {
      value <- tryCatch(candidate(), error = function(e) NULL)
      if (!is.null(value)) {
        return(tibble::as_tibble(value))
      }
    }
  }

  tibble::tibble()
}

.miss_normalize_mapping <- function(mapping) {
  if (ncol(mapping) == 0) {
    return(tibble::tibble(
      arm_num = integer(),
      unique_event_name = character(),
      form = character()
    ))
  }

  if (
    "event_name" %in% names(mapping) && !"unique_event_name" %in% names(mapping)
  ) {
    mapping$unique_event_name <- mapping$event_name
  }
  if ("form_name" %in% names(mapping) && !"form" %in% names(mapping)) {
    mapping$form <- mapping$form_name
  }
  if (!"unique_event_name" %in% names(mapping)) {
    mapping$unique_event_name <- NA_character_
  }
  if (!"form" %in% names(mapping)) {
    mapping$form <- NA_character_
  }

  tibble::tibble(
    arm_num = if ("arm_num" %in% names(mapping)) {
      mapping$arm_num
    } else {
      NA_integer_
    },
    unique_event_name = .miss_chr_vec(mapping$unique_event_name),
    form = .miss_chr_vec(mapping$form)
  )
}

.miss_normalize_events <- function(events) {
  if (ncol(events) == 0) {
    return(tibble::tibble(
      unique_event_name = character(),
      event_name = character()
    ))
  }

  if (!"unique_event_name" %in% names(events)) {
    events$unique_event_name <- if ("event_name" %in% names(events)) {
      events$event_name
    } else {
      NA_character_
    }
  }
  if (!"event_name" %in% names(events)) {
    events$event_name <- NA_character_
  }

  tibble::tibble(
    unique_event_name = .miss_chr_vec(events$unique_event_name),
    event_name = .miss_chr_vec(events$event_name)
  )
}

.miss_event_label_vector <- function(events) {
  if (nrow(events) == 0) {
    return(character())
  }

  keep <- !.miss_is_blank_vec(events$unique_event_name) &
    !.miss_is_blank_vec(events$event_name)
  unique_event_name <- events$unique_event_name[keep]
  event_name <- events$event_name[keep]
  duplicated_event <- duplicated(unique_event_name)

  stats::setNames(
    event_name[!duplicated_event],
    unique_event_name[!duplicated_event]
  )
}

.miss_normalize_repeat <- function(repeat_instrument_event) {
  if (ncol(repeat_instrument_event) == 0) {
    return(tibble::tibble(
      event_name = character(),
      form_name = character(),
      custom_form_label = character()
    ))
  }

  if (
    "unique_event_name" %in%
      names(repeat_instrument_event) &&
      !"event_name" %in% names(repeat_instrument_event)
  ) {
    repeat_instrument_event$event_name <- repeat_instrument_event$unique_event_name
  }
  if (
    "form" %in%
      names(repeat_instrument_event) &&
      !"form_name" %in% names(repeat_instrument_event)
  ) {
    repeat_instrument_event$form_name <- repeat_instrument_event$form
  }
  if (!"event_name" %in% names(repeat_instrument_event)) {
    repeat_instrument_event$event_name <- NA_character_
  }
  if (!"form_name" %in% names(repeat_instrument_event)) {
    repeat_instrument_event$form_name <- NA_character_
  }
  if (!"custom_form_label" %in% names(repeat_instrument_event)) {
    repeat_instrument_event$custom_form_label <- NA_character_
  }

  tibble::tibble(
    event_name = .miss_chr_vec(repeat_instrument_event$event_name),
    form_name = .miss_chr_vec(repeat_instrument_event$form_name),
    custom_form_label = .miss_chr_vec(repeat_instrument_event$custom_form_label)
  )
}

.miss_form_events <- function(mapping, form) {
  if (nrow(mapping) == 0) {
    return(character())
  }

  unique(mapping$unique_event_name[
    mapping$form == form & !.miss_is_blank_vec(mapping$unique_event_name)
  ])
}

.miss_repeat_form_events <- function(repeat_instrument_event, form) {
  if (nrow(repeat_instrument_event) == 0) {
    return(character())
  }

  rows <- .miss_chr_vec(repeat_instrument_event$form_name) == form
  unique(repeat_instrument_event$event_name[
    rows & !.miss_is_blank_vec(repeat_instrument_event$event_name)
  ])
}

.miss_repeating_events <- function(repeat_instrument_event) {
  if (nrow(repeat_instrument_event) == 0) {
    return(character())
  }

  rows <- .miss_is_blank_vec(repeat_instrument_event$form_name)
  unique(repeat_instrument_event$event_name[
    rows & !.miss_is_blank_vec(repeat_instrument_event$event_name)
  ])
}

.miss_form_repeating_events <- function(project) {
  intersect(project$form_events, project$repeating_events)
}

.miss_project_has_repeat_contexts <- function(project) {
  length(project$repeat_form_events) > 0 ||
    length(.miss_form_repeating_events(project)) > 0
}

.miss_filter_form_rows <- function(records, form, project) {
  n <- nrow(records)
  if (n == 0) {
    return(records)
  }

  fields <- project$system_fields
  has_event <- fields$event_col %in% names(records)
  has_repeat <- fields$repeat_instrument_col %in%
    names(records) ||
    fields$repeat_instance_col %in% names(records)

  event <- if (has_event) {
    .miss_chr_vec(records[[fields$event_col]])
  } else {
    rep(NA_character_, n)
  }
  repeat_instrument <- if (fields$repeat_instrument_col %in% names(records)) {
    .miss_chr_vec(records[[fields$repeat_instrument_col]])
  } else {
    rep(NA_character_, n)
  }
  repeat_blank <- .miss_is_blank_vec(repeat_instrument)

  event_keep <- rep(TRUE, n)
  if (has_event && length(project$form_events) > 0) {
    event_keep <- event %in% project$form_events
  }

  if (!has_repeat) {
    return(records[event_keep, , drop = FALSE])
  }

  form_repeating_events <- .miss_form_repeating_events(project)
  nonrepeat_events <- setdiff(
    project$form_events,
    union(project$repeat_form_events, form_repeating_events)
  )

  repeat_form_keep <- repeat_instrument == form
  if (has_event && length(project$repeat_form_events) > 0) {
    repeat_form_keep <- repeat_form_keep & event %in% project$repeat_form_events
  }

  repeat_event_keep <- rep(FALSE, n)
  if (has_event && length(form_repeating_events) > 0) {
    repeat_event_keep <- repeat_blank & event %in% form_repeating_events
  }

  if (has_event && length(project$form_events) > 0) {
    nonrepeat_keep <- repeat_blank & event %in% nonrepeat_events
  } else {
    nonrepeat_keep <- repeat_blank & !project$form_repeats
  }

  records[
    event_keep & (repeat_form_keep | repeat_event_keep | nonrepeat_keep),
    ,
    drop = FALSE
  ]
}

.miss_filter_record_eligibility_rows <- function(
  records,
  project,
  record_eligibility
) {
  if (nrow(record_eligibility) == 0 || nrow(records) == 0) {
    return(records)
  }

  record_keys <- .miss_record_context_key(records, project)
  eligibility_keys <- .miss_report_context_key(record_eligibility)
  records[record_keys %in% eligibility_keys, , drop = FALSE]
}
