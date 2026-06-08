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

  list(
    id_col = meta$field_name[[1]],
    system_fields = system_fields,
    mapping = mapping,
    repeat_instrument_event = repeat_instrument_event,
    project_information = project_information,
    form_events = .miss_form_events(mapping = mapping, form = form),
    repeat_form_events = .miss_repeat_form_events(
      repeat_instrument_event,
      form = form
    ),
    repeating_events = .miss_repeating_events(repeat_instrument_event),
    form_repeats = any(
      .miss_chr_vec(repeat_instrument_event$form_name) == form,
      na.rm = TRUE
    )
  )
}

.miss_resolve_expected_repeats <- function(expected_repeats, project, form) {
  if (!isTRUE(project$form_repeats)) {
    if (!is.null(expected_repeats)) {
      warning(
        "`expected_repeats` was supplied, but form `",
        form,
        "` is not configured as a REDCap repeating instrument. ",
        "Repeat-instance missingness will not be assessed.",
        call. = FALSE
      )
    }
    return(NULL)
  }

  if (is.null(expected_repeats)) {
    warning(
      "Form `",
      form,
      "` is configured as a REDCap repeating instrument, but ",
      "`expected_repeats` was not provided. Assuming `expected_repeats = 1L`.",
      call. = FALSE
    )
    return(1L)
  }

  if (
    !is.numeric(expected_repeats) ||
      length(expected_repeats) != 1 ||
      is.na(expected_repeats) ||
      !is.finite(expected_repeats) ||
      expected_repeats < 1 ||
      expected_repeats != floor(expected_repeats)
  ) {
    stop(
      "`expected_repeats` must be a positive whole-number scalar, such as 1L or 2L.",
      call. = FALSE
    )
  }

  as.integer(expected_repeats)
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

  form_repeating_events <- intersect(
    project$form_events,
    project$repeating_events
  )
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
