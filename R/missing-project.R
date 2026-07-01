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

.miss_get_project <- function(rcon, meta, form) {
  system_fields <- .miss_system_fields()
  form_label <- .miss_get_form_label(rcon = rcon, form = form)
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
  project_information <- .miss_get_rcon_table(
    rcon,
    c("projectInformation", "project_information", "projectInfo")
  )
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
    project_information = project_information,
    form_events = form_events,
    repeat_form_events = repeat_form_events,
    repeating_events = repeating_events,
    form_repeats = length(repeat_form_events) > 0 ||
      length(intersect(form_events, repeating_events)) > 0,
    events = NULL
  )
}

.miss_get_form_label <- function(rcon, form) {
  if (is.null(rcon$instruments) || !is.function(rcon$instruments)) {
    stop(
      "`rcon` must provide instrument labels through `rcon$instruments()`.",
      call. = FALSE
    )
  }

  instruments <- tibble::as_tibble(rcon$instruments())
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

.miss_resolve_instances <- function(instances, project, form) {
  if (!isTRUE(project$form_repeats)) {
    if (!is.null(instances)) {
      warning(
        "`instances` was supplied, but the requested assessment for form `",
        form,
        "` does not include any REDCap repeating event or instrument contexts. ",
        "Repeat-instance missingness will not be assessed.",
        call. = FALSE
      )
    }
    return(NULL)
  }

  if (is.null(instances)) {
    warning(
      "Form `",
      form,
      "` is repeating on at least one requested REDCap event, but ",
      "`instances` was not provided. Assuming `instances = 1L` ",
      "for the requested repeating-event contexts.",
      call. = FALSE
    )
    return(1L)
  }

  if (
    !is.numeric(instances) ||
      length(instances) != 1 ||
      is.na(instances) ||
      !is.finite(instances) ||
      instances < 1 ||
      instances != floor(instances)
  ) {
    stop(
      "`instances` must be a positive whole-number scalar, such as 1L or 2L.",
      call. = FALSE
    )
  }

  as.integer(instances)
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
