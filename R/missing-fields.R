## Internal helpers: field planning and validation rows -------------------

.miss_get_field_names <- function(meta) {
  .miss_derive_field_names(meta)
}

.miss_get_ignore_roots <- function(form_meta, field_names, ignore_fields) {
  if (length(ignore_fields) == 0) {
    return(character())
  }

  direct <- form_meta$field_name[form_meta$field_name %in% ignore_fields]
  from_export <- field_names$original_field_name[
    field_names$export_field_name %in% ignore_fields
  ]
  unique(stats::na.omit(c(direct, from_export)))
}

.miss_extract_logic_fields <- function(logic) {
  logic <- logic[!vapply(logic, .miss_is_blank_scalar, logical(1))]
  if (length(logic) == 0) {
    return(character())
  }

  event_fields <- unlist(
    lapply(logic, function(x) {
      matches <- gregexpr("\\[[^\\]]+\\]\\[([^\\]]+)\\]", x, perl = TRUE)[[1]]
      if (identical(matches[[1]], -1L)) {
        return(character())
      }
      refs <- regmatches(x, list(matches))[[1]]
      sub("^\\[[^\\]]+\\]\\[([^\\]]+)\\]$", "\\1", refs, perl = TRUE)
    }),
    use.names = FALSE
  )

  same_logic <- gsub("\\[[^\\]]+\\]\\[[^\\]]+\\]", "", logic, perl = TRUE)
  same_fields <- unlist(
    lapply(same_logic, function(x) {
      matches <- gregexpr("\\[([^\\]]+)\\]", x, perl = TRUE)[[1]]
      if (identical(matches[[1]], -1L)) {
        return(character())
      }
      refs <- regmatches(x, list(matches))[[1]]
      gsub("^\\[|\\]$", "", refs, perl = TRUE)
    }),
    use.names = FALSE
  )

  refs <- c(event_fields, same_fields)
  refs <- refs[!is.na(refs) & nzchar(refs)]
  parsed <- lapply(refs, .miss_parse_ref)
  unique(vapply(parsed, `[[`, character(1), "field"))
}

.miss_derive_field_names <- function(meta) {
  rows <- lapply(seq_len(nrow(meta)), function(i) {
    field <- meta$field_name[[i]]
    field_type <- meta$field_type[[i]]

    if (identical(field_type, "checkbox")) {
      choices <- .miss_parse_choices(meta$select_choices_or_calculations[[i]])
      if (nrow(choices) == 0) {
        return(tibble::tibble(
          original_field_name = field,
          choice_value = NA_character_,
          export_field_name = field
        ))
      }

      return(tibble::tibble(
        original_field_name = field,
        choice_value = choices$code,
        export_field_name = paste0(
          field,
          "___",
          .miss_choice_suffix(choices$code)
        )
      ))
    }

    tibble::tibble(
      original_field_name = field,
      choice_value = NA_character_,
      export_field_name = field
    )
  })

  dplyr::bind_rows(rows)
}

.miss_build_field_plan <- function(form_meta, field_names) {
  rows <- lapply(seq_len(nrow(form_meta)), function(i) {
    field <- form_meta$field_name[[i]]
    exports <- field_names[
      field_names$original_field_name == field,
      ,
      drop = FALSE
    ]

    tibble::tibble(
      field_name = field,
      form = form_meta$form_name[[i]],
      field_type = form_meta$field_type[[i]],
      field_label = form_meta$field_label[[i]],
      branching_logic = .miss_chr(form_meta$branching_logic[[i]]),
      branch_plan = list(.miss_compile_branch_logic(
        .miss_chr(form_meta$branching_logic[[i]])
      )),
      child_fields = list(as.character(exports$export_field_name)),
      choice_values = list(as.character(stats::na.omit(exports$choice_value)))
    )
  })

  dplyr::bind_rows(rows)
}

.miss_build_form_presence_plan <- function(form_meta, field_names) {
  rows <- lapply(seq_len(nrow(form_meta)), function(i) {
    field <- form_meta$field_name[[i]]
    exports <- field_names[
      field_names$original_field_name == field,
      ,
      drop = FALSE
    ]

    tibble::tibble(
      field_name = field,
      field_type = form_meta$field_type[[i]],
      child_fields = list(as.character(exports$export_field_name))
    )
  })

  dplyr::bind_rows(rows)
}

.miss_build_choice_map <- function(meta) {
  rows <- lapply(seq_len(nrow(meta)), function(i) {
    field <- meta$field_name[[i]]
    field_type <- meta$field_type[[i]]

    choices <- .miss_parse_choices(meta$select_choices_or_calculations[[i]])
    if (identical(field_type, "yesno")) {
      choices <- tibble::tibble(code = c("1", "0"), label = c("Yes", "No"))
    }
    if (identical(field_type, "truefalse")) {
      choices <- tibble::tibble(code = c("1", "0"), label = c("True", "False"))
    }
    if (nrow(choices) == 0) {
      return(NULL)
    }

    dplyr::mutate(choices, field_name = field, .before = 1)
  })

  out <- dplyr::bind_rows(rows)
  if (ncol(out) == 0) {
    return(tibble::tibble(
      field_name = character(),
      code = character(),
      label = character()
    ))
  }
  out
}

.miss_parse_choices <- function(x) {
  x <- .miss_chr(x)
  if (.miss_is_blank_scalar(x)) {
    return(tibble::tibble(code = character(), label = character()))
  }

  parts <- unlist(strsplit(x, "\\s*\\|\\s*", perl = TRUE), use.names = FALSE)
  rows <- lapply(parts, function(part) {
    match <- regexec("^\\s*([^,]+?)\\s*,\\s*(.*?)\\s*$", part, perl = TRUE)
    bits <- regmatches(part, match)[[1]]
    if (length(bits) == 0) {
      return(NULL)
    }
    tibble::tibble(
      code = trimws(bits[[2]]),
      label = trimws(bits[[3]])
    )
  })

  dplyr::bind_rows(rows)
}

.miss_build_expected <- function(
  records,
  lookup_records,
  field_plan,
  meta,
  choice_map,
  project,
  form
) {
  records <- tibble::as_tibble(records)
  if (nrow(records) == 0 || nrow(field_plan) == 0) {
    return(.miss_empty_expected())
  }

  fields <- project$system_fields
  row_base <- tibble::tibble(
    record_id = .miss_chr_vec(records[[project$id_col]]),
    redcap_event_name = .miss_col_vec(records, fields$event_col),
    redcap_repeat_instrument = .miss_col_vec(
      records,
      fields$repeat_instrument_col
    ),
    redcap_repeat_instance = .miss_col_vec(records, fields$repeat_instance_col)
  )

  branch_cache <- .miss_new_branch_cache(
    records = records,
    lookup_records = lookup_records,
    project = project
  )
  pieces <- lapply(seq_len(nrow(field_plan)), function(field_i) {
    logic <- field_plan$branching_logic[[field_i]]
    branch_satisfied <- .miss_branch_satisfied(
      logic = logic,
      branch_plan = field_plan$branch_plan[[field_i]],
      branch_cache = branch_cache,
      records = records,
      lookup_records = lookup_records,
      meta = meta,
      choice_map = choice_map,
      project = project
    )

    keep <- which(branch_satisfied)
    if (length(keep) == 0) {
      return(NULL)
    }

    child_fields <- field_plan$child_fields[[field_i]]
    present <- .miss_field_present(
      records = records,
      field = field_plan$field_name[[field_i]],
      field_type = field_plan$field_type[[field_i]],
      child_fields = child_fields,
      choice_map = choice_map
    )

    n_keep <- length(keep)
    dplyr::bind_cols(
      row_base[keep, , drop = FALSE],
      tibble::tibble(form = rep(form, n_keep)),
      .redcapmissing_validation_metadata(
        "field-complete",
        n_keep,
        repeat_instance = row_base$redcap_repeat_instance[keep]
      ),
      tibble::tibble(
        validation_passed = present$field_complete[keep],
        field_name = rep(field_plan$field_name[[field_i]], n_keep),
        field_label = rep(field_plan$field_label[[field_i]], n_keep),
        field_type = rep(field_plan$field_type[[field_i]], n_keep),
        branching_logic = rep(logic, n_keep),
        branch_satisfied = rep(TRUE, n_keep),
        value_summary = present$value_summary[keep],
        export_fields = rep(paste(child_fields, collapse = ", "), n_keep)
      )
    )
  })

  out <- dplyr::bind_rows(pieces)
  if (nrow(out) == 0) {
    return(.miss_empty_expected())
  }
  out
}

.miss_empty_expected <- function() {
  tibble::tibble(
    record_id = character(),
    redcap_event_name = character(),
    redcap_repeat_instrument = character(),
    redcap_repeat_instance = character(),
    form = character(),
    validation_level = character(),
    validation_check = character(),
    validation_label = character(),
    validation_passed = logical(),
    field_name = character(),
    field_label = character(),
    field_type = character(),
    branching_logic = character(),
    branch_satisfied = logical(),
    value_summary = character(),
    export_fields = character(),
    validation_context = character()
  )
}

.miss_build_form_check_rows <- function(
  records,
  form_meta,
  project,
  form,
  expected_contexts = NULL
) {
  records <- tibble::as_tibble(records)
  if (nrow(records) == 0 || nrow(form_meta) == 0) {
    if (is.null(expected_contexts) || nrow(expected_contexts) == 0) {
      return(.miss_empty_expected())
    }
  }

  data_meta <- form_meta[form_meta$field_name != project$id_col, , drop = FALSE]
  if (nrow(data_meta) == 0) {
    return(.miss_empty_expected())
  }

  field_names <- .miss_get_field_names(data_meta)
  presence_plan <- .miss_build_form_presence_plan(
    form_meta = data_meta,
    field_names = field_names
  )
  if (nrow(presence_plan) == 0) {
    return(.miss_empty_expected())
  }

  export_fields <- unique(unlist(presence_plan$child_fields, use.names = FALSE))
  actual_contexts <- .miss_context_from_records(records, project)
  actual_started <- logical(nrow(records))

  if (nrow(records) > 0) {
    present <- vapply(
      seq_len(nrow(presence_plan)),
      function(field_i) {
        .miss_field_present(
          records = records,
          field = presence_plan$field_name[[field_i]],
          field_type = presence_plan$field_type[[field_i]],
          child_fields = presence_plan$child_fields[[field_i]],
          choice_map = tibble::tibble()
        )$field_complete
      },
      logical(nrow(records))
    )
    if (is.null(dim(present))) {
      present <- matrix(present, ncol = nrow(presence_plan))
    }

    actual_started <- rowSums(present, na.rm = TRUE) > 0
  }

  contexts <- if (
    is.null(expected_contexts) ||
      nrow(expected_contexts) == 0
  ) {
    actual_contexts
  } else {
    expected_contexts
  }
  if (nrow(contexts) == 0) {
    return(.miss_empty_expected())
  }

  form_started <- rep(TRUE, nrow(contexts))
  value_summary <- rep(
    "Form-started status is handled by an upstream row-presence check.",
    nrow(contexts)
  )
  actual_match <- match(
    .miss_report_context_key(contexts),
    .miss_report_context_key(actual_contexts)
  )
  has_actual <- !is.na(actual_match)
  form_started[has_actual] <- actual_started[actual_match[has_actual]]
  value_summary[has_actual] <- ifelse(
    form_started[has_actual],
    "At least one exported form field is present.",
    "All exported form fields are blank or unchecked."
  )

  .miss_build_issue_rows(
    contexts = contexts,
    form = form,
    validation_check = "form-started",
    validation_passed = form_started
  )
}

.miss_build_form_started_failure_rows <- function(records, form_meta, project, form) {
  form_checks <- .miss_build_form_check_rows(
    records = records,
    form_meta = form_meta,
    project = project,
    form = form
  )

  form_checks[!form_checks$validation_passed, , drop = FALSE]
}

.miss_build_event_check_rows <- function(
  records,
  form_records,
  project,
  form,
  record_eligibility = NULL
) {
  records <- tibble::as_tibble(records)
  form_records <- tibble::as_tibble(form_records)
  fields <- project$system_fields

  if (!fields$event_col %in% names(records)) {
    return(.miss_empty_expected())
  }

  form_events <- .miss_missing_event_form_events(project)
  if (length(form_events) == 0) {
    return(.miss_empty_expected())
  }

  expected <- .miss_expected_record_events_from_eligibility(
    record_eligibility = record_eligibility,
    form = form,
    events = form_events,
    include_repeat = FALSE
  )
  if (nrow(expected) == 0) {
    return(.miss_empty_expected())
  }

  existing <- tibble::tibble(
    record_id = .miss_chr_vec(form_records[[project$id_col]]),
    redcap_event_name = .miss_chr_vec(form_records[[fields$event_col]])
  )
  existing <- unique(existing[
    !.miss_is_blank_vec(existing$record_id),
    ,
    drop = FALSE
  ])

  expected$.event_row_started <- .miss_context_key(
    expected$record_id,
    expected$redcap_event_name,
    "",
    ""
  ) %in% .miss_context_key(
    existing$record_id,
    existing$redcap_event_name,
    "",
    ""
  )
  expected$redcap_repeat_instrument <- ""
  expected$redcap_repeat_instance <- ""

  .miss_build_issue_rows(
    contexts = expected,
    form = form,
    validation_check = "event-row-started",
    validation_passed = expected$.event_row_started
  )
}

.miss_build_repeat_check_rows <- function(
  records,
  form_records,
  project,
  form,
  instances,
  record_eligibility = NULL
) {
  if (is.null(instances) || !isTRUE(project$form_repeats)) {
    return(.miss_empty_expected())
  }

  records <- tibble::as_tibble(records)
  form_records <- tibble::as_tibble(form_records)

  contexts <- .miss_expected_record_events_from_eligibility(
    record_eligibility = record_eligibility,
    form = form,
    events = project$events,
    include_repeat = TRUE
  )
  if (nrow(contexts) == 0) {
    return(.miss_empty_expected())
  }

  existing <- .miss_context_from_records(form_records, project)
  existing <- existing[
    !.miss_is_blank_vec(existing$record_id) &
      !.miss_is_blank_vec(existing$redcap_repeat_instance),
    ,
    drop = FALSE
  ]
  existing <- unique(existing[
    .miss_report_context_key(existing) %in% .miss_report_context_key(contexts),
    ,
    drop = FALSE
  ])

  contexts$.instance_row_started <- .miss_report_context_key(contexts) %in%
    .miss_report_context_key(existing)

  .miss_build_issue_rows(
    contexts = contexts,
    form = form,
    validation_check = "instance-row-started",
    validation_passed = contexts$.instance_row_started
  )
}

.miss_build_expected_contexts <- function(
  records,
  event_checks,
  repeat_checks,
  project,
  record_eligibility = NULL
) {
  context_cols <- c(
    "record_id",
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  )
  has_upstream_row_checks <- nrow(event_checks) > 0 || nrow(repeat_checks) > 0
  event_checks <- event_checks[
    event_checks$validation_passed,
    ,
    drop = FALSE
  ]
  repeat_checks <- repeat_checks[
    repeat_checks$validation_passed,
    ,
    drop = FALSE
  ]
  contexts <- dplyr::bind_rows(
    event_checks[, context_cols, drop = FALSE],
    repeat_checks[, context_cols, drop = FALSE]
  )
  contexts <- unique(contexts)
  if (nrow(contexts) > 0) {
    return(tibble::as_tibble(contexts))
  }
  if (isTRUE(has_upstream_row_checks)) {
    return(.miss_empty_expected()[, context_cols])
  }

  if (!is.null(record_eligibility) && nrow(record_eligibility) > 0) {
    return(tibble::as_tibble(unique(record_eligibility[
      ,
      context_cols,
      drop = FALSE
    ])))
  }

  records <- tibble::as_tibble(records)
  if (nrow(records) == 0) {
    return(.miss_empty_expected()[, context_cols])
  }

  record_ids <- unique(.miss_chr_vec(records[[project$id_col]]))
  record_ids <- record_ids[!.miss_is_blank_vec(record_ids)]
  if (length(record_ids) == 0) {
    return(.miss_empty_expected()[, context_cols])
  }

  tibble::tibble(
    record_id = record_ids,
    redcap_event_name = "",
    redcap_repeat_instrument = "",
    redcap_repeat_instance = ""
  )
}

.miss_missing_event_form_events <- function(project) {
  if (length(project$form_events) == 0) {
    return(character())
  }

  setdiff(
    project$form_events,
    union(project$repeat_form_events, project$repeating_events)
  )
}

.miss_expected_record_events_from_eligibility <- function(
  record_eligibility,
  form,
  events,
  include_repeat = FALSE
) {
  if (is.null(record_eligibility) || nrow(record_eligibility) == 0) {
    if (isTRUE(include_repeat)) {
      return(.miss_empty_expected()[, c(
        "record_id",
        "redcap_event_name",
        "redcap_repeat_instrument",
        "redcap_repeat_instance"
      )])
    }
    return(tibble::tibble(
      record_id = character(),
      redcap_event_name = character()
    ))
  }

  events <- .miss_chr_vec(events %||% character())
  event_keep <- if (length(events) == 0) {
    rep(TRUE, nrow(record_eligibility))
  } else {
    record_eligibility$redcap_event_name %in% events
  }
  rows <- record_eligibility[
    record_eligibility$form == form & event_keep,
    ,
    drop = FALSE
  ]
  if (isTRUE(include_repeat)) {
    rows <- rows[!.miss_is_blank_vec(rows$redcap_repeat_instance), , drop = FALSE]
    out <- rows[, c(
      "record_id",
      "redcap_event_name",
      "redcap_repeat_instrument",
      "redcap_repeat_instance"
    ), drop = FALSE]
    return(tibble::as_tibble(unique(out)))
  }

  rows <- rows[.miss_is_blank_vec(rows$redcap_repeat_instance), , drop = FALSE]
  out <- rows[, c("record_id", "redcap_event_name"), drop = FALSE]
  tibble::as_tibble(unique(out))
}

.miss_expected_record_events_from_data <- function(records, project, events) {
  fields <- project$system_fields
  record_ids <- unique(.miss_chr_vec(records[[project$id_col]]))
  record_ids <- record_ids[!.miss_is_blank_vec(record_ids)]
  if (length(record_ids) == 0 || length(events) == 0) {
    return(tibble::tibble(
      record_id = character(),
      redcap_event_name = character()
    ))
  }

  mapping <- unique(project$mapping[,
    c("unique_event_name", "arm_num"),
    drop = FALSE
  ])
  target_events <- unique(mapping[
    mapping$unique_event_name %in% events,
    ,
    drop = FALSE
  ])
  if (
    nrow(target_events) == 0 ||
      !"arm_num" %in% names(target_events) ||
      all(is.na(target_events$arm_num))
  ) {
    return(.miss_cross_record_events(record_ids, events))
  }

  record_events <- tibble::tibble(
    record_id = .miss_chr_vec(records[[project$id_col]]),
    unique_event_name = .miss_chr_vec(records[[fields$event_col]])
  )
  record_arms <- dplyr::inner_join(
    record_events,
    mapping,
    by = "unique_event_name"
  )
  record_arms <- unique(record_arms[
    !.miss_is_blank_vec(record_arms$record_id) & !is.na(record_arms$arm_num),
    c("record_id", "arm_num"),
    drop = FALSE
  ])

  if (nrow(record_arms) == 0) {
    return(.miss_cross_record_events(record_ids, events))
  }

  expected <- merge(
    record_arms,
    target_events,
    by = "arm_num",
    all = FALSE
  )
  expected <- unique(tibble::tibble(
    record_id = expected$record_id,
    redcap_event_name = expected$unique_event_name
  ))

  no_arm_ids <- setdiff(record_ids, record_arms$record_id)
  if (length(no_arm_ids) > 0) {
    expected <- dplyr::bind_rows(
      expected,
      .miss_cross_record_events(no_arm_ids, events)
    )
  }

  unique(expected)
}

.miss_cross_record_events <- function(record_ids, events) {
  out <- expand.grid(
    record_id = record_ids,
    redcap_event_name = events,
    stringsAsFactors = FALSE
  )
  tibble::as_tibble(out)
}

.miss_context_from_records <- function(records, project) {
  fields <- project$system_fields
  tibble::tibble(
    record_id = .miss_chr_vec(records[[project$id_col]]),
    redcap_event_name = .miss_chr_vec(.miss_col_vec(records, fields$event_col)),
    redcap_repeat_instrument = .miss_chr_vec(.miss_col_vec(
      records,
      fields$repeat_instrument_col
    )),
    redcap_repeat_instance = .miss_chr_vec(.miss_col_vec(
      records,
      fields$repeat_instance_col
    ))
  )
}

.miss_build_issue_rows <- function(
  contexts,
  form,
  validation_check,
  validation_passed
) {
  contexts <- tibble::as_tibble(contexts)
  if (nrow(contexts) == 0) {
    return(.miss_empty_expected())
  }

  n <- nrow(contexts)
  form_value <- form
  tibble::tibble(
    record_id = .miss_chr_vec(contexts$record_id),
    redcap_event_name = .miss_chr_vec(contexts$redcap_event_name),
    redcap_repeat_instrument = .miss_chr_vec(contexts$redcap_repeat_instrument),
    redcap_repeat_instance = .miss_chr_vec(contexts$redcap_repeat_instance),
    form = rep(form_value, n)
  ) |>
    dplyr::bind_cols(
      .redcapmissing_validation_metadata(
        validation_check,
        n,
        repeat_instance = contexts$redcap_repeat_instance
      ),
      tibble::tibble(
        validation_passed = rep(validation_passed, length.out = n),
        field_name = rep(NA_character_, n),
        field_label = rep(NA_character_, n),
        field_type = rep(NA_character_, n),
        branching_logic = rep(NA_character_, n),
        branch_satisfied = rep(NA, n),
        value_summary = rep(NA_character_, n),
        export_fields = rep(NA_character_, n)
      )
    )
}

.miss_add_validation_context <- function(rows) {
  rows <- tibble::as_tibble(rows)
  rows <- .miss_normalize_validation_context_columns(rows)
  if (nrow(rows) == 0) {
    rows$validation_context <- character()
    return(rows)
  }

  event <- .miss_chr_vec(rows$redcap_event_name)
  repeat_instance <- .miss_chr_vec(rows$redcap_repeat_instance)
  has_event <- !.miss_is_blank_vec(event)
  has_repeat <- !.miss_is_blank_vec(repeat_instance)

  context <- rep("overall", nrow(rows))
  context[has_event & !has_repeat] <- paste0(
    "event: ",
    event[has_event & !has_repeat]
  )
  context[!has_event & has_repeat] <- paste0(
    "repeat: ",
    repeat_instance[!has_event & has_repeat]
  )
  context[has_event & has_repeat] <- paste0(
    "event: ",
    event[has_event & has_repeat],
    "; repeat: ",
    repeat_instance[has_event & has_repeat]
  )

  rows$validation_context <- context
  rows
}

.miss_normalize_validation_context_columns <- function(rows) {
  rows <- tibble::as_tibble(rows)
  context_cols <- c(
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "form"
  )
  for (column in intersect(context_cols, names(rows))) {
    rows[[column]] <- .miss_chr_vec(rows[[column]])
    rows[[column]][is.na(rows[[column]])] <- ""
  }
  rows
}

.miss_drop_unstarted_form_records <- function(records, form_started_failures, project) {
  if (nrow(records) == 0 || nrow(form_started_failures) == 0) {
    return(records)
  }

  record_keys <- .miss_record_context_key(records, project)
  failure_keys <- .miss_report_context_key(form_started_failures)
  records[!record_keys %in% failure_keys, , drop = FALSE]
}

.miss_record_context_key <- function(records, project) {
  contexts <- .miss_context_from_records(records, project)
  .miss_context_key(
    contexts$record_id,
    contexts$redcap_event_name,
    contexts$redcap_repeat_instrument,
    contexts$redcap_repeat_instance
  )
}

.miss_report_context_key <- function(rows) {
  .miss_context_key(
    rows$record_id,
    rows$redcap_event_name,
    rows$redcap_repeat_instrument,
    rows$redcap_repeat_instance
  )
}

.miss_context_key <- function(
  record_id,
  event,
  repeat_instrument,
  repeat_instance
) {
  paste(
    .miss_key_part(record_id),
    .miss_key_part(event),
    .miss_key_part(repeat_instrument),
    .miss_key_part(repeat_instance),
    sep = "\r"
  )
}

.miss_key_part <- function(x) {
  out <- .miss_chr_vec(x)
  out[is.na(out)] <- ""
  out
}
