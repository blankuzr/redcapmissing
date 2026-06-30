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
      form_name = form_meta$form_name[[i]],
      field_type = form_meta$field_type[[i]],
      field_label = form_meta$field_label[[i]],
      branching_logic = .miss_chr(form_meta$branching_logic[[i]]),
      child_fields = list(as.character(exports$export_field_name)),
      choice_values = list(as.character(stats::na.omit(exports$choice_value)))
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

  pieces <- lapply(seq_len(nrow(field_plan)), function(field_i) {
    logic <- field_plan$branching_logic[[field_i]]
    branch_satisfied <- .miss_branch_satisfied(
      logic = logic,
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
      tibble::tibble(
        form_name = rep(form, n_keep),
        field_name = rep(field_plan$field_name[[field_i]], n_keep),
        field_label = rep(field_plan$field_label[[field_i]], n_keep),
        field_type = rep(field_plan$field_type[[field_i]], n_keep),
        check_scope = rep("field", n_keep),
        missing_scope = rep("field", n_keep),
        branching_logic = rep(logic, n_keep),
        branch_satisfied = rep(TRUE, n_keep),
        value_present = present$value_present[keep],
        form_started = rep(TRUE, n_keep),
        event_row_present = rep(TRUE, n_keep),
        repeat_instance_present = rep(TRUE, n_keep),
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
    form_name = character(),
    field_name = character(),
    field_label = character(),
    field_type = character(),
    check_scope = character(),
    missing_scope = character(),
    branching_logic = character(),
    branch_satisfied = logical(),
    value_present = logical(),
    form_started = logical(),
    event_row_present = logical(),
    repeat_instance_present = logical(),
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
  field_plan <- .miss_build_field_plan(
    form_meta = data_meta,
    field_names = field_names
  )
  if (nrow(field_plan) == 0) {
    return(.miss_empty_expected())
  }

  export_fields <- unique(unlist(field_plan$child_fields, use.names = FALSE))
  actual_contexts <- .miss_context_from_records(records, project)
  actual_started <- logical(nrow(records))

  if (nrow(records) > 0) {
    present <- vapply(
      seq_len(nrow(field_plan)),
      function(field_i) {
        .miss_field_present(
          records = records,
          field = field_plan$field_name[[field_i]],
          field_type = field_plan$field_type[[field_i]],
          child_fields = field_plan$child_fields[[field_i]],
          choice_map = tibble::tibble()
        )$value_present
      },
      logical(nrow(records))
    )
    if (is.null(dim(present))) {
      present <- matrix(present, ncol = nrow(field_plan))
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
    "Form startedness is handled by an upstream row-presence check.",
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
    check_scope = "form_started",
    missing_scope = ifelse(form_started, "form_started", "form_blank"),
    field_label = "Entire form is missing",
    value_summary = value_summary,
    export_fields = paste(export_fields, collapse = ", "),
    form_started = form_started,
    event_row_present = TRUE
  )
}

.miss_build_form_missing_rows <- function(records, form_meta, project, form) {
  form_checks <- .miss_build_form_check_rows(
    records = records,
    form_meta = form_meta,
    project = project,
    form = form
  )

  form_checks[form_checks$missing_scope == "form_blank", , drop = FALSE]
}

.miss_build_event_check_rows <- function(
  records,
  form_records,
  project,
  form
) {
  records <- tibble::as_tibble(records)
  form_records <- tibble::as_tibble(form_records)
  fields <- project$system_fields

  if (!fields$event_col %in% names(records)) {
    return(.miss_empty_expected())
  }

  form_events <- .miss_missing_event_form_events(project)
  if (length(form_events) == 0 || nrow(records) == 0) {
    return(.miss_empty_expected())
  }

  expected <- .miss_expected_record_events(
    records = records,
    project = project,
    events = form_events
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

  expected$.event_row_present <- .miss_context_key(
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
    check_scope = "event_row_present",
    missing_scope = ifelse(expected$.event_row_present, "event_present", "event_absent"),
    field_label = "Entire form is missing",
    value_summary = ifelse(
      expected$.event_row_present,
      "An exported row exists for the REDCap event where this form is offered.",
      "No exported row exists for the REDCap event where this form is offered."
    ),
    export_fields = "",
    form_started = TRUE,
    event_row_present = expected$.event_row_present
  )
}

.miss_build_event_missing_rows <- function(records, form_records, project, form) {
  event_checks <- .miss_build_event_check_rows(
    records = records,
    form_records = form_records,
    project = project,
    form = form
  )

  event_checks[event_checks$missing_scope == "event_absent", , drop = FALSE]
}

.miss_build_repeat_check_rows <- function(
  records,
  form_records,
  project,
  form,
  expected_repeats
) {
  if (is.null(expected_repeats) || !isTRUE(project$form_repeats)) {
    return(.miss_empty_expected())
  }

  records <- tibble::as_tibble(records)
  form_records <- tibble::as_tibble(form_records)
  if (nrow(records) == 0) {
    return(.miss_empty_expected())
  }

  contexts <- .miss_expected_repeat_contexts(
    records = records,
    project = project,
    form = form,
    expected_repeats = expected_repeats
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

  contexts$.repeat_instance_present <- .miss_report_context_key(contexts) %in%
    .miss_report_context_key(existing)

  fields <- project$system_fields
  has_event <- fields$event_col %in% names(records)
  if (has_event) {
    event_rows <- unique(tibble::tibble(
      record_id = .miss_chr_vec(records[[project$id_col]]),
      redcap_event_name = .miss_chr_vec(records[[fields$event_col]])
    ))
    contexts$.event_row_present <- .miss_context_key(
      contexts$record_id,
      contexts$redcap_event_name,
      "",
      ""
    ) %in% .miss_context_key(
      event_rows$record_id,
      event_rows$redcap_event_name,
      "",
      ""
    )
  } else {
    contexts$.event_row_present <- TRUE
  }

  .miss_build_issue_rows(
    contexts = contexts,
    form = form,
    check_scope = "repeat_instance_present",
    missing_scope = ifelse(
      contexts$.repeat_instance_present,
      "repeat_present",
      "repeat_absent"
    ),
    field_label = "Repeat instance is missing",
    value_summary = ifelse(
      contexts$.repeat_instance_present,
      "An exported row exists for this REDCap repeat instance.",
      "No exported row exists for this expected REDCap repeat instance."
    ),
    export_fields = "",
    form_started = TRUE,
    event_row_present = contexts$.event_row_present,
    repeat_instance_present = contexts$.repeat_instance_present
  )
}

.miss_expected_repeat_contexts <- function(
  records,
  project,
  form,
  expected_repeats
) {
  fields <- project$system_fields
  instances <- as.character(seq_len(expected_repeats))

  if (fields$event_col %in% names(records)) {
    repeat_event_contexts <- .miss_form_repeating_events(project)
    if (
      length(project$repeat_form_events) == 0 &&
        length(repeat_event_contexts) == 0
    ) {
      return(.miss_empty_expected()[, c(
        "record_id",
        "redcap_event_name",
        "redcap_repeat_instrument",
        "redcap_repeat_instance"
      )])
    }

    context_pieces <- list()
    if (length(project$repeat_form_events) > 0) {
      record_events <- .miss_expected_record_events(
        records = records,
        project = project,
        events = project$repeat_form_events
      )
      if (nrow(record_events) > 0) {
        context_pieces <- c(context_pieces, list(
          dplyr::mutate(record_events, redcap_repeat_instrument = form)
        ))
      }
    }
    if (length(repeat_event_contexts) > 0) {
      record_events <- .miss_expected_record_events(
        records = records,
        project = project,
        events = repeat_event_contexts
      )
      if (nrow(record_events) > 0) {
        context_pieces <- c(context_pieces, list(
          dplyr::mutate(record_events, redcap_repeat_instrument = "")
        ))
      }
    }
    record_events <- dplyr::bind_rows(context_pieces)
  } else {
    record_ids <- unique(.miss_chr_vec(records[[project$id_col]]))
    record_ids <- record_ids[!.miss_is_blank_vec(record_ids)]
    record_events <- tibble::tibble(
      record_id = record_ids,
      redcap_event_name = "",
      redcap_repeat_instrument = form
    )
  }

  if (nrow(record_events) == 0) {
    return(.miss_empty_expected()[, c(
      "record_id",
      "redcap_event_name",
      "redcap_repeat_instrument",
      "redcap_repeat_instance"
    )])
  }

  out <- merge(
    record_events,
    tibble::tibble(redcap_repeat_instance = instances),
    all = TRUE
  )
  out <- out[, c(
    "record_id",
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  )]

  tibble::as_tibble(unique(out))
}

.miss_build_expected_contexts <- function(
  records,
  event_checks,
  repeat_checks,
  project
) {
  context_cols <- c(
    "record_id",
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  )
  contexts <- dplyr::bind_rows(
    event_checks[, context_cols, drop = FALSE],
    repeat_checks[, context_cols, drop = FALSE]
  )
  contexts <- unique(contexts)
  if (nrow(contexts) > 0) {
    return(tibble::as_tibble(contexts))
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

.miss_build_any_field_check_rows <- function(expected, form) {
  expected <- .miss_add_validation_context(expected)
  if (nrow(expected) == 0) {
    return(.miss_empty_expected())
  }

  context_cols <- c(
    "record_id",
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "validation_context"
  )
  contexts <- unique(expected[, context_cols, drop = FALSE])
  context_keys <- .miss_report_context_key(contexts)
  expected_keys <- .miss_report_context_key(expected)
  any_missing <- vapply(
    context_keys,
    function(context_key) {
      any(!expected$value_present[expected_keys == context_key], na.rm = TRUE)
    },
    logical(1)
  )

  out <- .miss_build_issue_rows(
    contexts = contexts,
    form = form,
    check_scope = "any_field_missing",
    missing_scope = ifelse(
      any_missing,
      "any_field_missing",
      "all_fields_present"
    ),
    field_label = "Any expected field is missing",
    value_summary = ifelse(
      any_missing,
      "At least one expected field is blank.",
      "All expected fields are present."
    ),
    export_fields = "",
    form_started = TRUE,
    event_row_present = TRUE,
    repeat_instance_present = TRUE
  )
  out$value_present <- !any_missing
  .miss_add_validation_context(out)
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

.miss_expected_record_events <- function(records, project, events) {
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
  check_scope,
  missing_scope,
  field_label,
  value_summary,
  export_fields,
  form_started,
  event_row_present,
  repeat_instance_present = TRUE
) {
  contexts <- tibble::as_tibble(contexts)
  if (nrow(contexts) == 0) {
    return(.miss_empty_expected())
  }

  n <- nrow(contexts)
  tibble::tibble(
    record_id = .miss_chr_vec(contexts$record_id),
    redcap_event_name = .miss_chr_vec(contexts$redcap_event_name),
    redcap_repeat_instrument = .miss_chr_vec(contexts$redcap_repeat_instrument),
    redcap_repeat_instance = .miss_chr_vec(contexts$redcap_repeat_instance),
    form_name = rep(form, n),
    field_name = rep(form, n),
    field_label = rep(field_label, n),
    field_type = rep("form", n),
    check_scope = rep(check_scope, length.out = n),
    missing_scope = rep(missing_scope, length.out = n),
    branching_logic = rep("", n),
    branch_satisfied = rep(TRUE, n),
    value_present = rep(TRUE, n),
    form_started = rep(form_started, length.out = n),
    event_row_present = rep(event_row_present, length.out = n),
    repeat_instance_present = rep(repeat_instance_present, length.out = n),
    value_summary = rep(value_summary, length.out = n),
    export_fields = rep(export_fields, length.out = n)
  )
}

.miss_add_validation_context <- function(rows) {
  rows <- tibble::as_tibble(rows)
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

.miss_add_validation_step <- function(
  agent,
  rows,
  check_scope,
  column,
  step_id,
  label,
  keep_zero = FALSE
) {
  preconditions <- .miss_scope_precondition(check_scope)
  if (nrow(rows) > 0) {
    return(pointblank::col_vals_equal(
      x = agent,
      columns = dplyr::all_of(column),
      value = TRUE,
      preconditions = preconditions,
      segments = pointblank::vars(validation_context),
      step_id = step_id,
      label = label
    ))
  }

  if (isTRUE(keep_zero)) {
    return(pointblank::col_vals_equal(
      x = agent,
      columns = dplyr::all_of(column),
      value = TRUE,
      preconditions = preconditions,
      step_id = step_id,
      label = label
    ))
  }

  agent
}

.miss_scope_precondition <- function(check_scope) {
  force(check_scope)
  function(tbl) {
    tbl[tbl$check_scope == check_scope, , drop = FALSE]
  }
}

.miss_annotate_agent_validation_set <- function(agent, validation_rows) {
  validation_set <- agent$validation_set
  if (nrow(validation_set) == 0) {
    agent$validation_set <- validation_set
    return(agent)
  }

  lookup_cols <- c(
    "validation_context",
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  )
  lookup <- unique(validation_rows[, lookup_cols, drop = FALSE])
  if (nrow(lookup) == 0) {
    lookup <- tibble::tibble(
      validation_context = "overall",
      redcap_event_name = "",
      redcap_repeat_instrument = "",
      redcap_repeat_instance = ""
    )
  }

  validation_context <- .miss_chr_vec(validation_set$seg_val)
  blank_context <- .miss_is_blank_vec(validation_context)
  validation_context[blank_context] <- "overall"
  context_match <- match(validation_context, lookup$validation_context)

  validation_set$validation_context <- validation_context
  validation_set$redcap_event_name <- lookup$redcap_event_name[context_match]
  validation_set$redcap_repeat_instrument <-
    lookup$redcap_repeat_instrument[context_match]
  validation_set$redcap_repeat_instance <-
    lookup$redcap_repeat_instance[context_match]

  zero_n <- !is.na(validation_set$n) & validation_set$n == 0
  validation_set$f_passed[zero_n] <- 0
  validation_set$f_failed[zero_n] <- 0

  agent$validation_set <- validation_set
  agent
}

.miss_drop_form_missing_records <- function(records, form_missing, project) {
  if (nrow(records) == 0 || nrow(form_missing) == 0) {
    return(records)
  }

  record_keys <- .miss_record_context_key(records, project)
  missing_keys <- .miss_report_context_key(form_missing)
  records[!record_keys %in% missing_keys, , drop = FALSE]
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
