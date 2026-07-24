## Internal helpers: compiled missingness engine --------------------------

.miss_elapsed_now <- function() {
  unname(proc.time()[["elapsed"]])
}

.miss_new_timer <- function(clock = .miss_elapsed_now) {
  if (!is.function(clock)) {
    stop("`clock` must be a function.", call. = FALSE)
  }
  timer <- new.env(parent = emptyenv())
  timer$clock <- clock
  timer$n <- 0L
  timer$scope <- character()
  timer$form <- character()
  timer$stage <- character()
  timer$elapsed_seconds <- double()
  timer
}

.miss_timer_start <- function(timer) {
  unname(as.numeric(timer$clock()))
}

.miss_timer_add <- function(
  timer,
  scope,
  stage,
  elapsed_seconds,
  form = NA_character_
) {
  row <- timer$n + 1L
  timer$scope[[row]] <- .miss_chr(scope)
  timer$form[[row]] <- .miss_chr(form)
  timer$stage[[row]] <- .miss_chr(stage)
  timer$elapsed_seconds[[row]] <- max(
    0,
    unname(as.numeric(elapsed_seconds))
  )
  timer$n <- row
  invisible(timer)
}

.miss_timer_record <- function(
  timer,
  scope,
  stage,
  started_at,
  form = NA_character_
) {
  finished_at <- .miss_timer_start(timer)
  .miss_timer_add(
    timer = timer,
    scope = scope,
    stage = stage,
    elapsed_seconds = finished_at - started_at,
    form = form
  )
  invisible(finished_at)
}

.miss_timer_rows <- function(timer) {
  if (timer$n == 0L) {
    return(tibble::tibble(
      scope = character(),
      form = character(),
      stage = character(),
      elapsed_seconds = double()
    ))
  }
  tibble::tibble(
    scope = timer$scope,
    form = timer$form,
    stage = timer$stage,
    elapsed_seconds = timer$elapsed_seconds
  )
}

.miss_extract_logic_references <- function(logic) {
  logic <- .miss_chr_vec(logic)
  logic <- logic[!.miss_is_blank_vec(logic)]
  if (length(logic) == 0) {
    return(tibble::tibble(
      logic = character(),
      event = character(),
      field = character(),
      choice = character()
    ))
  }

  rows <- list()
  for (logic_i in seq_along(logic)) {
    value <- logic[[logic_i]]
    event_pattern <- "\\[([^\\]]+)\\]\\[([^\\]]+)\\]"
    event_matches <- gregexpr(event_pattern, value, perl = TRUE)[[1]]
    if (!identical(event_matches[[1]], -1L)) {
      refs <- regmatches(value, list(event_matches))[[1]]
      for (ref in refs) {
        bits <- regmatches(ref, regexec(event_pattern, ref, perl = TRUE))[[1]]
        parsed <- .miss_parse_ref(bits[[3]])
        rows[[length(rows) + 1L]] <- tibble::tibble(
          logic = value,
          event = bits[[2]],
          field = parsed$field,
          choice = parsed$choice %||% NA_character_
        )
      }
    }

    same_value <- gsub(event_pattern, "", value, perl = TRUE)
    same_pattern <- "\\[([^\\]]+)\\]"
    same_matches <- gregexpr(same_pattern, same_value, perl = TRUE)[[1]]
    if (!identical(same_matches[[1]], -1L)) {
      refs <- regmatches(same_value, list(same_matches))[[1]]
      for (ref in refs) {
        parsed <- .miss_parse_ref(gsub("^\\[|\\]$", "", ref))
        rows[[length(rows) + 1L]] <- tibble::tibble(
          logic = value,
          event = NA_character_,
          field = parsed$field,
          choice = parsed$choice %||% NA_character_
        )
      }
    }
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      logic = character(),
      event = character(),
      field = character(),
      choice = character()
    ))
  }
  unique(dplyr::bind_rows(rows))
}

.miss_build_field_index <- function(meta, child_fields_by_root) {
  field_rows <- !duplicated(.miss_chr_vec(meta$field_name))
  fields <- meta[field_rows, , drop = FALSE]
  field_name <- .miss_chr_vec(fields$field_name)
  child_fields <- lapply(field_name, function(field) {
    child_fields_by_root[[field]] %||% character()
  })
  is_numeric <- vapply(
    field_name,
    function(field) .miss_numeric_field(meta = meta, field = field),
    logical(1)
  )

  tibble::tibble(
    field_name = field_name,
    field_type = .miss_chr_vec(fields$field_type),
    is_numeric = is_numeric,
    child_fields = child_fields
  )
}

.miss_build_compiled_field_plan <- function(
  form_meta,
  compiled_logic,
  child_fields_by_root,
  choice_values_by_root
) {
  field_name <- .miss_chr_vec(form_meta$field_name)
  branching_logic <- vapply(
    form_meta$branching_logic,
    .miss_chr,
    character(1)
  )
  child_fields <- lapply(field_name, function(field) {
    child_fields_by_root[[field]] %||% character()
  })
  choice_values <- lapply(field_name, function(field) {
    choice_values_by_root[[field]] %||% character()
  })
  blank_plan <- .miss_compile_branch_logic("")
  branch_plan <- lapply(branching_logic, function(logic) {
    if (.miss_is_blank_scalar(logic)) {
      return(blank_plan)
    }
    compiled_logic[[match(logic, names(compiled_logic))]]
  })

  tibble::tibble(
    field_name = field_name,
    form = .miss_chr_vec(form_meta$form_name),
    field_type = .miss_chr_vec(form_meta$field_type),
    field_label = .miss_chr_vec(form_meta$field_label),
    branching_logic = branching_logic,
    branch_plan = branch_plan,
    child_fields = child_fields,
    choice_values = choice_values
  )
}

.miss_compile_project_contexts <- function(meta, forms, project_cache) {
  mapping <- project_cache$mapping
  repeat_instrument_event <- project_cache$repeat_instrument_event
  split_events <- function(form_value, event_value) {
    form_value <- .miss_chr_vec(form_value)
    event_value <- .miss_chr_vec(event_value)
    keep <- !.miss_is_blank_vec(form_value) & !.miss_is_blank_vec(event_value)
    if (!any(keep)) {
      return(list())
    }
    lapply(split(event_value[keep], form_value[keep]), unique)
  }
  form_event_map <- split_events(mapping$form, mapping$unique_event_name)
  repeat_form_event_map <- split_events(
    repeat_instrument_event$form_name,
    repeat_instrument_event$event_name
  )
  repeating_events <- .miss_repeating_events(repeat_instrument_event)
  form_labels <- stats::setNames(vapply(forms, function(form) {
    .miss_get_form_label(
      rcon = NULL,
      form = form,
      project_cache = project_cache
    )
  }, character(1)), forms)

  projects <- vector("list", length(forms))
  names(projects) <- forms
  for (form in forms) {
    form_events <- form_event_map[[form]] %||% character()
    repeat_form_events <- repeat_form_event_map[[form]] %||% character()
    projects[[form]] <- list(
      id_col = meta$field_name[[1]],
      form_label = form_labels[[form]],
      system_fields = project_cache$system_fields,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event,
      event_labels = project_cache$event_labels,
      project_information = project_cache$project_information,
      form_events = form_events,
      repeat_form_events = repeat_form_events,
      repeating_events = repeating_events,
      form_repeats = length(repeat_form_events) > 0 ||
        length(intersect(form_events, repeating_events)) > 0,
      events = NULL
    )
  }
  projects
}

.miss_compile_report_plan <- function(
  meta,
  forms,
  required_fields,
  ignore_fields,
  exclude_types,
  rcon,
  project_cache
) {
  if (isTRUE(required_fields) && !"required_field" %in% names(meta)) {
    stop(
      "rcon$metadata() must include `required_field` when `required_fields = TRUE`.",
      call. = FALSE
    )
  }

  all_field_names <- .miss_derive_field_names(meta)
  field_root <- .miss_chr_vec(all_field_names$original_field_name)
  child_fields_by_root <- split(
    .miss_chr_vec(all_field_names$export_field_name),
    field_root
  )
  has_choice <- !is.na(all_field_names$choice_value)
  choice_values_by_root <- split(
    as.character(all_field_names$choice_value[has_choice]),
    field_root[has_choice]
  )
  ignore_fields <- unique(as.character(ignore_fields %||% character()))
  prepared <- vector("list", length(forms))
  names(prepared) <- forms
  assessable_logic <- character()

  for (form in forms) {
    all_meta <- meta[meta$form_name == form, , drop = FALSE]
    form_meta <- all_meta
    if (length(exclude_types) > 0) {
      form_meta <- form_meta[
        !form_meta$field_type %in% exclude_types,
        ,
        drop = FALSE
      ]
    }
    if (isTRUE(required_fields)) {
      form_meta <- form_meta[
        .miss_required_vec(form_meta$required_field),
        ,
        drop = FALSE
      ]
    }

    candidate_names <- if (length(ignore_fields) > 0L) {
      all_field_names[
        all_field_names$original_field_name %in% form_meta$field_name,
        ,
        drop = FALSE
      ]
    } else {
      all_field_names[0, , drop = FALSE]
    }
    ignore_roots <- .miss_get_ignore_roots(
      form_meta = form_meta,
      field_names = candidate_names,
      ignore_fields = ignore_fields
    )
    if (length(ignore_roots) > 0) {
      form_meta <- form_meta[
        !form_meta$field_name %in% ignore_roots,
        ,
        drop = FALSE
      ]
    }
    assessable_logic <- c(
      assessable_logic,
      .miss_chr_vec(form_meta$branching_logic)
    )
    prepared[[form]] <- list(
      all_meta = all_meta,
      form_meta = form_meta,
      ignore_roots = ignore_roots
    )
  }

  logic_values <- unique(assessable_logic[!.miss_is_blank_vec(assessable_logic)])
  compiled_logic <- stats::setNames(
    lapply(logic_values, .miss_compile_branch_logic),
    logic_values
  )
  branch_references <- .miss_extract_logic_references(logic_values)
  choice_fields <- unique(branch_references$field)
  choice_map <- .miss_build_choice_map(meta[
    meta$field_name %in% choice_fields,
    ,
    drop = FALSE
  ])
  field_index <- .miss_build_field_index(
    meta = meta,
    child_fields_by_root = child_fields_by_root
  )
  project_contexts <- .miss_compile_project_contexts(
    meta = meta,
    forms = forms,
    project_cache = project_cache
  )

  form_plans <- vector("list", length(forms))
  names(form_plans) <- forms
  for (form in forms) {
    pieces <- prepared[[form]]
    presence_meta <- pieces$all_meta[
      pieces$all_meta$field_name != meta$field_name[[1]],
      ,
      drop = FALSE
    ]
    presence_plan <- .miss_build_form_presence_plan(
      form_meta = presence_meta,
      child_fields_by_root = child_fields_by_root
    )
    field_plan <- .miss_build_compiled_field_plan(
      form_meta = pieces$form_meta,
      compiled_logic = compiled_logic,
      child_fields_by_root = child_fields_by_root,
      choice_values_by_root = choice_values_by_root
    )
    is_checkbox <- field_plan$field_type == "checkbox"
    is_checkbox[is.na(is_checkbox)] <- FALSE
    is_branched <- !.miss_is_blank_vec(field_plan$branching_logic)

    form_plans[[form]] <- list(
      form = form,
      project = project_contexts[[form]],
      presence_plan = presence_plan,
      field_plan = field_plan,
      ignore_roots = pieces$ignore_roots,
      ordinary_unbranched = which(!is_checkbox & !is_branched),
      ordinary_branched = which(!is_checkbox & is_branched),
      checkbox_unbranched = which(is_checkbox & !is_branched),
      checkbox_branched = which(is_checkbox & is_branched)
    )
  }

  list(
    meta = meta,
    field_names = all_field_names,
    field_index = field_index,
    field_positions = stats::setNames(
      seq_len(nrow(field_index)),
      field_index$field_name
    ),
    validation_labels = stats::setNames(
      .redcapmissing_registry_data()$validation_label,
      .redcapmissing_registry_data()$validation_check
    ),
    choice_map = choice_map,
    branch_references = branch_references,
    branch_plans = compiled_logic,
    form_plans = form_plans
  )
}

.miss_new_record_store <- function(
  data,
  id_col,
  ignore_ids,
  system_fields = .miss_system_fields()
) {
  data <- tibble::as_tibble(data)
  source_row <- seq_len(nrow(data))
  keep <- !.miss_chr_vec(data[[id_col]]) %in% ignore_ids
  active_rows <- source_row[keep]

  records <- tibble::tibble(.source_row = active_rows)
  records[[id_col]] <- data[[id_col]][active_rows]
  system_columns <- unname(unlist(system_fields, use.names = FALSE))
  for (column in system_columns[system_columns %in% names(data)]) {
    records[[column]] <- data[[column]][active_rows]
  }
  records <- records[, c(id_col, setdiff(names(records), id_col)), drop = FALSE]
  records$.context_key <- .miss_context_key(
    record_id = records[[id_col]],
    event = .miss_col_vec(records, system_fields$event_col),
    repeat_instrument = .miss_col_vec(
      records,
      system_fields$repeat_instrument_col
    ),
    repeat_instance = .miss_col_vec(records, system_fields$repeat_instance_col)
  )

  source_position <- integer(nrow(data))
  source_position[active_rows] <- seq_along(active_rows)
  cache <- new.env(parent = emptyenv())
  cache$raw_values <- list()
  cache$event_matches <- list()

  list(
    data = data,
    records = records,
    active_rows = active_rows,
    source_position = source_position,
    id_col = id_col,
    system_fields = system_fields,
    cache = cache
  )
}

.miss_store_column <- function(record_store, field, source_rows) {
  if (!field %in% names(record_store$data)) {
    return(rep(NA_character_, length(source_rows)))
  }
  record_store$data[[field]][source_rows]
}

.miss_store_event_match <- function(record_store, event) {
  key <- .miss_chr(event)
  cached <- record_store$cache$event_matches[[key]]
  if (!is.null(cached)) {
    return(cached)
  }

  event_col <- record_store$system_fields$event_col
  if (!event_col %in% names(record_store$data)) {
    out <- rep(NA_integer_, length(record_store$active_rows))
    record_store$cache$event_matches[[key]] <- out
    return(out)
  }
  event_value <- .miss_chr_vec(record_store$data[[event_col]])
  active_event <- event_value[record_store$active_rows]
  event_source_rows <- record_store$active_rows[
    !is.na(active_event) & active_event == event
  ]
  if (length(event_source_rows) == 0) {
    out <- rep(NA_integer_, length(record_store$active_rows))
    record_store$cache$event_matches[[key]] <- out
    return(out)
  }

  active_ids <- .miss_chr_vec(record_store$data[[record_store$id_col]][
    record_store$active_rows
  ])
  event_ids <- .miss_chr_vec(record_store$data[[record_store$id_col]][
    event_source_rows
  ])
  out <- event_source_rows[match(active_ids, event_ids)]
  record_store$cache$event_matches[[key]] <- out
  out
}

.miss_store_branch_raw <- function(record_store, source_rows, event, field) {
  if (is.null(event)) {
    return(.miss_store_column(record_store, field, source_rows))
  }

  key <- .miss_branch_cache_key(event = event, field = field)
  cached <- record_store$cache$raw_values[[key]]
  if (is.null(cached)) {
    event_match <- .miss_store_event_match(record_store, event)
    cached <- rep(NA_character_, length(record_store$active_rows))
    hit <- !is.na(event_match) & field %in% names(record_store$data)
    if (any(hit)) {
      cached[hit] <- .miss_chr_vec(record_store$data[[field]][event_match[hit]])
    }
    record_store$cache$raw_values[[key]] <- cached
  }

  cached[record_store$source_position[source_rows]]
}

.miss_field_index_row <- function(report_plan, field) {
  position <- unname(report_plan$field_positions[field])
  report_plan$field_index[
    position,
    ,
    drop = FALSE
  ]
}

.miss_build_form_blankness <- function(
  record_store,
  source_rows,
  presence_plan,
  field_plan,
  report_plan
) {
  root_fields <- unique(c(
    .miss_chr_vec(presence_plan$field_name),
    .miss_chr_vec(field_plan$field_name)
  ))
  blank <- matrix(
    TRUE,
    nrow = length(source_rows),
    ncol = length(root_fields),
    dimnames = list(NULL, root_fields)
  )
  checkbox_selected <- list()
  presence_field <- root_fields %in% .miss_chr_vec(presence_plan$field_name)
  actual_started <- rep(FALSE, length(source_rows))

  for (field_i in seq_along(root_fields)) {
    field <- root_fields[[field_i]]
    descriptor <- .miss_field_index_row(report_plan, field)
    field_type <- descriptor$field_type[[1]]
    if (!is.na(field_type) && identical(field_type, "checkbox")) {
      child_fields <- descriptor$child_fields[[1]]
      selected <- matrix(
        FALSE,
        nrow = length(source_rows),
        ncol = length(child_fields),
        dimnames = list(NULL, child_fields)
      )
      for (child_i in seq_along(child_fields)) {
        selected[, child_i] <- .miss_checkbox_selected_vec(
          .miss_store_column(record_store, child_fields[[child_i]], source_rows)
        )
      }
      blank[, field_i] <- rowSums(selected, na.rm = TRUE) == 0
      checkbox_selected[[field]] <- selected
    } else {
      blank[, field_i] <- .miss_is_blank_vec(
        .miss_store_column(record_store, field, source_rows)
      )
    }
    if (presence_field[[field_i]]) {
      actual_started <- actual_started | !blank[, field_i]
    }
  }

  list(
    field_names = root_fields,
    blank = blank,
    checkbox_selected = checkbox_selected,
    actual_started = actual_started
  )
}

.miss_build_form_check_rows_cached <- function(
  form_records,
  blankness,
  presence_plan,
  project,
  form,
  expected_contexts = NULL,
  include_context = TRUE
) {
  actual_contexts <- .miss_context_from_records(form_records, project)
  actual_started <- blankness$actual_started

  contexts <- if (
    is.null(expected_contexts) || nrow(expected_contexts) == 0
  ) {
    actual_contexts
  } else {
    expected_contexts
  }
  if (nrow(contexts) == 0) {
    return(.miss_empty_expected())
  }

  form_started <- rep(TRUE, nrow(contexts))
  actual_match <- match(
    .miss_report_context_key(contexts),
    .miss_record_context_key(form_records, project)
  )
  has_actual <- !is.na(actual_match)
  form_started[has_actual] <- actual_started[actual_match[has_actual]]

  .miss_build_issue_rows(
    contexts = contexts,
    form = form,
    validation_check = "form-started",
    validation_passed = form_started,
    include_context = include_context
  )
}

.miss_normalize_branch_value <- function(value, field, report_plan) {
  value <- .miss_chr_vec(value)
  blank <- .miss_is_blank_vec(value)
  choices <- report_plan$choice_map[
    report_plan$choice_map$field_name == field,
    ,
    drop = FALSE
  ]
  if (nrow(choices) > 0) {
    code_match <- match(value, choices$code)
    label_match <- match(value, choices$label)
    out <- value
    out[!is.na(label_match)] <- choices$code[label_match[!is.na(label_match)]]
    out[!is.na(code_match)] <- choices$code[code_match[!is.na(code_match)]]
    out[blank] <- ""
    return(out)
  }

  descriptor <- .miss_field_index_row(report_plan, field)
  if (nrow(descriptor) > 0 && isTRUE(descriptor$is_numeric[[1]])) {
    out <- suppressWarnings(as.numeric(value))
    out[blank] <- NA_real_
    return(out)
  }
  value[blank] <- ""
  value
}

.miss_store_branch_value <- function(
  record_store,
  source_rows,
  event,
  field,
  choice,
  report_plan
) {
  descriptor <- .miss_field_index_row(report_plan, field)
  field_type <- if (nrow(descriptor) == 0) {
    NA_character_
  } else {
    descriptor$field_type[[1]]
  }

  if (!is.null(choice) || identical(field_type, "checkbox")) {
    if (is.null(choice)) {
      child_fields <- descriptor$child_fields[[1]]
      selected <- matrix(
        FALSE,
        nrow = length(source_rows),
        ncol = length(child_fields)
      )
      for (child_i in seq_along(child_fields)) {
        selected[, child_i] <- .miss_checkbox_selected_vec(
          .miss_store_branch_raw(
            record_store = record_store,
            source_rows = source_rows,
            event = event,
            field = child_fields[[child_i]]
          )
        )
      }
      return(as.integer(rowSums(selected, na.rm = TRUE) > 0))
    }

    child <- paste0(field, "___", .miss_choice_suffix(choice))
    return(as.integer(.miss_checkbox_selected_vec(.miss_store_branch_raw(
      record_store = record_store,
      source_rows = source_rows,
      event = event,
      field = child
    ))))
  }

  .miss_normalize_branch_value(
    value = .miss_store_branch_raw(
      record_store = record_store,
      source_rows = source_rows,
      event = event,
      field = field
    ),
    field = field,
    report_plan = report_plan
  )
}

.miss_branch_satisfied_store <- function(
  branch_plan,
  record_store,
  source_rows,
  report_plan
) {
  if (is.null(branch_plan$parsed_expr)) {
    return(rep(TRUE, length(source_rows)))
  }

  env <- new.env(parent = baseenv())
  env$.v <- function(event, field, choice = NULL) {
    .miss_store_branch_value(
      record_store = record_store,
      source_rows = source_rows,
      event = event,
      field = field,
      choice = choice,
      report_plan = report_plan
    )
  }
  env$contains <- function(x, pattern) grepl(pattern, x, fixed = TRUE)
  env$isblank <- function(x) .miss_is_blank_vec(x)
  env$notblank <- function(x) !.miss_is_blank_vec(x)
  env$datediff <- function(date1, date2, units = "d", dateformat = "ymd", ...) {
    .miss_datediff(date1 = date1, date2 = date2, units = units)
  }
  env$.redcap_if <- function(condition, yes, no) ifelse(condition, yes, no)

  value <- tryCatch(
    eval(branch_plan$parsed_expr, envir = env),
    error = function(e) {
      stop(
        "Could not evaluate branching logic `",
        branch_plan$logic,
        "`. Translated R was `",
        branch_plan$expr,
        "`. ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  value <- rep(value, length.out = length(source_rows))
  value <- as.logical(value)
  !is.na(value) & value
}

.miss_checkbox_value_summary <- function(selected, child_fields, rows) {
  if (length(rows) == 0) {
    return(character())
  }
  vapply(rows, function(row_i) {
    picked <- child_fields[selected[row_i, , drop = TRUE]]
    if (length(picked) == 0) "" else paste(picked, collapse = ", ")
  }, character(1))
}

.miss_build_expected_cached <- function(
  form_records,
  started_positions,
  blankness,
  form_plan,
  report_plan,
  record_store,
  project,
  form,
  details,
  progress_callback = NULL,
  timer = NULL
) {
  field_plan <- form_plan$field_plan
  source_rows <- form_records$.source_row[started_positions]
  if (length(source_rows) == 0 || nrow(field_plan) == 0) {
    if (!is.null(timer)) {
      for (stage in c(
        "ordinary_unbranched",
        "ordinary_branched",
        "checkbox_unbranched",
        "checkbox_branched"
      )) {
        .miss_timer_add(timer, "form", stage, 0, form)
      }
    }
    return(list(
      rows = .miss_empty_expected(),
      record_row = integer(),
      validation_passed = logical()
    ))
  }

  chunks <- list()
  chunk_i <- 0L
  add_chunk <- function(field_rows, record_rows, validation_passed, value_summary) {
    if (length(record_rows) == 0) {
      return(invisible(NULL))
    }
    chunk_i <<- chunk_i + 1L
    chunks[[chunk_i]] <<- list(
      field_row = field_rows,
      record_row = record_rows,
      validation_passed = validation_passed,
      value_summary = value_summary
    )
    invisible(NULL)
  }
  completed_fields <- 0L
  report_progress <- function(processed) {
    completed_fields <<- completed_fields + processed
    if (!is.null(progress_callback)) {
      progress_callback(completed_fields / nrow(field_plan), force = FALSE)
    }
  }

  process_ordinary <- function(field_indices, branched) {
    if (length(field_indices) == 0) {
      return(invisible(NULL))
    }
    blank_columns <- match(
      field_plan$field_name[field_indices],
      blankness$field_names
    )
    complete <- !blankness$blank[
      started_positions,
      blank_columns,
      drop = FALSE
    ]
    if (!isTRUE(branched)) {
      row_count <- length(source_rows)
      field_count <- length(field_indices)
      value_summary <- if (isTRUE(details)) {
        unlist(lapply(
          field_plan$field_name[field_indices],
          function(field) .miss_chr_vec(.miss_store_column(
            record_store,
            field,
            source_rows
          ))
        ), use.names = FALSE)
      } else {
        rep(NA_character_, row_count * field_count)
      }
      add_chunk(
        field_rows = rep(field_indices, each = row_count),
        record_rows = rep.int(seq_len(row_count), times = field_count),
        validation_passed = as.vector(complete),
        value_summary = value_summary
      )
      report_progress(field_count)
      return(invisible(NULL))
    }

    for (column_i in seq_along(field_indices)) {
      field_i <- field_indices[[column_i]]
      keep <- which(.miss_branch_satisfied_store(
        branch_plan = field_plan$branch_plan[[field_i]],
        record_store = record_store,
        source_rows = source_rows,
        report_plan = report_plan
      ))
      value_summary <- if (isTRUE(details)) {
        .miss_chr_vec(.miss_store_column(
          record_store,
          field_plan$field_name[[field_i]],
          source_rows
        ))[keep]
      } else {
        rep(NA_character_, length(keep))
      }
      add_chunk(
        field_rows = rep.int(field_i, length(keep)),
        record_rows = keep,
        validation_passed = complete[keep, column_i],
        value_summary = value_summary
      )
    }
    report_progress(length(field_indices))
    invisible(NULL)
  }

  process_checkboxes <- function(field_indices, branched) {
    if (length(field_indices) == 0) {
      return(invisible(NULL))
    }
    for (field_i in field_indices) {
      field <- field_plan$field_name[[field_i]]
      keep <- seq_along(source_rows)
      if (isTRUE(branched)) {
        keep <- which(.miss_branch_satisfied_store(
          branch_plan = field_plan$branch_plan[[field_i]],
          record_store = record_store,
          source_rows = source_rows,
          report_plan = report_plan
        ))
      }
      column_i <- match(field, blankness$field_names)
      selected <- blankness$checkbox_selected[[field]]
      value_summary <- if (isTRUE(details)) {
        .miss_checkbox_value_summary(
          selected = selected,
          child_fields = colnames(selected),
          rows = started_positions[keep]
        )
      } else {
        rep(NA_character_, length(keep))
      }
      add_chunk(
        field_rows = rep.int(field_i, length(keep)),
        record_rows = keep,
        validation_passed = !blankness$blank[
          started_positions[keep],
          column_i
        ],
        value_summary = value_summary
      )
    }
    report_progress(length(field_indices))
    invisible(NULL)
  }

  kernels <- list(
    ordinary_unbranched = function() process_ordinary(
      form_plan$ordinary_unbranched,
      branched = FALSE
    ),
    ordinary_branched = function() process_ordinary(
      form_plan$ordinary_branched,
      branched = TRUE
    ),
    checkbox_unbranched = function() process_checkboxes(
      form_plan$checkbox_unbranched,
      branched = FALSE
    ),
    checkbox_branched = function() process_checkboxes(
      form_plan$checkbox_branched,
      branched = TRUE
    )
  )
  for (stage in names(kernels)) {
    stage_started <- if (is.null(timer)) NA_real_ else .miss_timer_start(timer)
    kernels[[stage]]()
    if (!is.null(timer)) {
      .miss_timer_record(timer, "form", stage, stage_started, form)
    }
  }

  if (length(chunks) == 0) {
    return(list(
      rows = .miss_empty_expected(),
      record_row = integer(),
      validation_passed = logical()
    ))
  }

  field_row <- unlist(lapply(chunks, `[[`, "field_row"), use.names = FALSE)
  record_row <- unlist(lapply(chunks, `[[`, "record_row"), use.names = FALSE)
  validation_passed <- unlist(
    lapply(chunks, `[[`, "validation_passed"),
    use.names = FALSE
  )
  value_summary <- unlist(
    lapply(chunks, `[[`, "value_summary"),
    use.names = FALSE
  )
  row_order <- order(field_row, record_row)
  field_row <- field_row[row_order]
  record_row <- record_row[row_order]
  validation_passed <- validation_passed[row_order]
  value_summary <- value_summary[row_order]
  validation_check <- "field-complete"
  validation_label <- report_plan$validation_labels[[validation_check]]
  export_fields <- vapply(
    field_plan$child_fields,
    paste,
    collapse = ", ",
    character(1)
  )
  fields <- project$system_fields
  row_base <- data.frame(
    record_id = .miss_chr_vec(form_records[[project$id_col]][started_positions]),
    redcap_event_name = .miss_key_part(
      .miss_col_vec(form_records, fields$event_col)[started_positions]
    ),
    redcap_repeat_instrument = .miss_key_part(.miss_col_vec(
      form_records,
      fields$repeat_instrument_col
    )[started_positions]),
    redcap_repeat_instance = .miss_key_part(.miss_col_vec(
      form_records,
      fields$repeat_instance_col
    )[started_positions]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  n <- length(record_row)

  rows <- tibble::new_tibble(list(
    record_id = row_base$record_id[record_row],
    redcap_event_name = row_base$redcap_event_name[record_row],
    redcap_repeat_instrument = row_base$redcap_repeat_instrument[record_row],
    redcap_repeat_instance = row_base$redcap_repeat_instance[record_row],
    form = rep.int(form, n),
    validation_level = .redcapmissing_context_validation_level(
      validation_check = validation_check,
      repeat_instance = row_base$redcap_repeat_instance[record_row]
    ),
    validation_check = rep.int(validation_check, n),
    validation_label = rep.int(validation_label, n),
    validation_passed = validation_passed,
    field_name = field_plan$field_name[field_row],
    field_label = field_plan$field_label[field_row],
    field_type = field_plan$field_type[field_row],
    branching_logic = field_plan$branching_logic[field_row],
    branch_satisfied = rep.int(TRUE, n),
    value_summary = value_summary,
    export_fields = export_fields[field_row],
    validation_context = if (isTRUE(details)) {
      .miss_validation_context_vec(
        event = row_base$redcap_event_name[record_row],
        repeat_instance = row_base$redcap_repeat_instance[record_row]
      )
    } else {
      rep(NA_character_, n)
    }
  ), nrow = n)
  list(
    rows = rows,
    record_row = record_row,
    validation_passed = validation_passed
  )
}

.miss_build_indexed_field_counts <- function(
  record_eligibility,
  form_records,
  started_positions,
  expected_result,
  project,
  form
) {
  context_columns <- c(
    "record_id",
    "redcap_event_name",
    "form",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  )
  out <- unique(record_eligibility[, context_columns, drop = FALSE])
  out <- tibble::as_tibble(out)
  out$field_assessed <- rep.int(0L, nrow(out))
  out$field_failed <- rep.int(0L, nrow(out))
  if (nrow(out) == 0L || length(expected_result$record_row) == 0L) {
    return(out)
  }

  row_count <- length(started_positions)
  assessed_by_row <- tabulate(expected_result$record_row, nbins = row_count)
  failed_by_row <- tabulate(
    expected_result$record_row[
      !(expected_result$validation_passed %in% TRUE)
    ],
    nbins = row_count
  )
  record_keys <- if (".context_key" %in% names(form_records)) {
    .miss_chr_vec(form_records$.context_key[started_positions])
  } else {
    .miss_record_context_key(form_records[started_positions, , drop = FALSE], project)
  }
  row_key <- paste(record_keys, form, sep = "\r")
  unique_key <- unique(row_key)
  group_id <- match(row_key, unique_key)
  assessed <- as.integer(rowsum(
    assessed_by_row,
    group = group_id,
    reorder = FALSE
  )[, 1])
  failed <- as.integer(rowsum(
    failed_by_row,
    group = group_id,
    reorder = FALSE
  )[, 1])
  out_key <- paste(
    .miss_context_key(
      out$record_id,
      out$redcap_event_name,
      out$redcap_repeat_instrument,
      out$redcap_repeat_instance
    ),
    out$form,
    sep = "\r"
  )
  count_match <- match(out_key, unique_key)
  matched <- !is.na(count_match)
  out$field_assessed[matched] <- assessed[count_match[matched]]
  out$field_failed[matched] <- failed[count_match[matched]]
  out
}

.miss_form_workload_row <- function(
  form,
  form_records,
  expected_contexts,
  started_positions,
  form_plan,
  validation_rows
) {
  tibble::tibble(
    form = form,
    record_rows = nrow(form_records),
    expected_contexts = nrow(expected_contexts),
    started_rows = length(started_positions),
    total_fields = nrow(form_plan$presence_plan),
    assessable_fields = nrow(form_plan$field_plan),
    ordinary_unbranched_fields = length(form_plan$ordinary_unbranched),
    ordinary_branched_fields = length(form_plan$ordinary_branched),
    checkbox_unbranched_fields = length(form_plan$checkbox_unbranched),
    checkbox_branched_fields = length(form_plan$checkbox_branched),
    validation_rows = validation_rows
  )
}

.miss_build_form_report <- function(
  record_store,
  report_plan,
  form_plan,
  form,
  events,
  record_specs,
  instances,
  instances_explicit,
  details,
  verified_keys = character(),
  progress_callback = NULL,
  defer_assembly = FALSE
) {
  timer <- .miss_new_timer()
  stage_started_at <- .miss_timer_start(timer)

  project <- form_plan$project
  .miss_check_record_specs_for_form(
    record_specs = record_specs,
    project = project,
    form = form
  )
  project <- .miss_resolve_events(
    project = project,
    events = events,
    form = form
  )
  resolved_instances <- .miss_resolve_instances(
    instances = instances,
    project = project,
    form = form,
    explicit = instances_explicit
  )
  instances <- resolved_instances$values
  .miss_timer_record(timer, "form", "context", stage_started_at, form)
  .miss_cli_report_form_progress(
    progress_callback,
    stage = "context",
    force = FALSE
  )
  .miss_cli_report_form_progress(
    progress_callback,
    stage = "metadata",
    force = FALSE
  )

  records <- record_store$records
  stage_started_at <- .miss_timer_start(timer)
  record_eligibility <- .miss_build_record_eligibility(
    records = records,
    project = project,
    form = form,
    instances = instances,
    record_specs = record_specs
  )
  form_records <- .miss_filter_form_rows(
    records = records,
    form = form,
    project = project
  )
  form_records <- .miss_filter_record_eligibility_rows(
    records = form_records,
    project = project,
    record_eligibility = record_eligibility
  )
  .miss_timer_record(timer, "form", "eligibility", stage_started_at, form)
  .miss_cli_report_form_progress(
    progress_callback,
    stage = "eligibility",
    force = FALSE
  )

  stage_started_at <- .miss_timer_start(timer)
  event_checks <- .miss_build_event_check_rows(
    records = records,
    form_records = form_records,
    project = project,
    form = form,
    record_eligibility = record_eligibility,
    include_context = details
  )
  if (isTRUE(details)) {
    event_checks <- .miss_add_validation_context(event_checks)
  }
  event_row_started_failures <- if (isTRUE(details) && !isTRUE(defer_assembly)) {
    event_checks[!event_checks$validation_passed, , drop = FALSE]
  } else {
    .miss_empty_expected()
  }

  repeat_checks <- .miss_build_repeat_check_rows(
    records = records,
    form_records = form_records,
    project = project,
    form = form,
    instances = instances,
    record_eligibility = record_eligibility,
    include_context = details
  )
  if (isTRUE(details)) {
    repeat_checks <- .miss_add_validation_context(repeat_checks)
  }
  instance_row_started_failures <- if (
    isTRUE(details) && !isTRUE(defer_assembly)
  ) {
    repeat_checks[!repeat_checks$validation_passed, , drop = FALSE]
  } else {
    .miss_empty_expected()
  }
  expected_contexts <- .miss_build_expected_contexts(
    records = records,
    event_checks = event_checks,
    repeat_checks = repeat_checks,
    project = project,
    record_eligibility = record_eligibility
  )
  .miss_timer_record(timer, "form", "row_checks", stage_started_at, form)
  .miss_cli_report_form_progress(
    progress_callback,
    stage = "row_checks",
    force = FALSE
  )

  stage_started_at <- .miss_timer_start(timer)
  blankness <- .miss_build_form_blankness(
    record_store = record_store,
    source_rows = form_records$.source_row,
    presence_plan = form_plan$presence_plan,
    field_plan = form_plan$field_plan,
    report_plan = report_plan
  )
  .miss_timer_record(timer, "form", "blankness", stage_started_at, form)

  stage_started_at <- .miss_timer_start(timer)
  form_checks <- if (nrow(form_plan$presence_plan) == 0) {
    .miss_empty_expected()
  } else {
    .miss_build_form_check_rows_cached(
      form_records = form_records,
      blankness = blankness,
      presence_plan = form_plan$presence_plan,
      project = project,
      form = form,
      expected_contexts = expected_contexts,
      include_context = details
    )
  }
  if (isTRUE(details) && !isTRUE(defer_assembly)) {
    form_checks <- .miss_add_validation_context(form_checks)
  }
  form_started_failures <- form_checks[
    !form_checks$validation_passed,
    ,
    drop = FALSE
  ]
  started_positions <- if (
    nrow(form_records) == 0 || nrow(form_started_failures) == 0
  ) {
    seq_len(nrow(form_records))
  } else {
    which(
      !.miss_record_context_key(form_records, project) %in%
        .miss_report_context_key(form_started_failures)
    )
  }
  zero_field_complete_contexts <- tibble::tibble(
    redcap_event_name = character(),
    form = character(),
    redcap_repeat_instrument = character(),
    redcap_repeat_instance = character()
  )
  if (
    nrow(form_plan$field_plan) == 0L &&
      length(started_positions) > 0L
  ) {
    started_contexts <- .miss_context_from_records(
      form_records[started_positions, , drop = FALSE],
      project
    )
    zero_field_complete_contexts <- unique(tibble::tibble(
      redcap_event_name = started_contexts$redcap_event_name,
      form = rep(form, nrow(started_contexts)),
      redcap_repeat_instrument = started_contexts$redcap_repeat_instrument,
      redcap_repeat_instance = started_contexts$redcap_repeat_instance
    ))
  }
  .miss_timer_record(timer, "form", "form_started", stage_started_at, form)
  .miss_cli_report_form_progress(
    progress_callback,
    stage = "form_checks",
    force = FALSE
  )
  .miss_cli_report_form_progress(
    progress_callback,
    stage = "field_checks",
    field_fraction = 0,
    force = FALSE
  )

  field_progress_callback <- if (is.null(progress_callback)) {
    NULL
  } else {
    function(field_fraction, force = FALSE) {
      .miss_cli_report_form_progress(
        progress_callback,
        stage = "field_checks",
        field_fraction = field_fraction,
        force = force
      )
    }
  }

  expected_result <- .miss_build_expected_cached(
    form_records = form_records,
    started_positions = started_positions,
    blankness = blankness,
    form_plan = form_plan,
    report_plan = report_plan,
    record_store = record_store,
    project = project,
    form = form,
    details = details,
    progress_callback = field_progress_callback,
    timer = timer
  )
  verification_result <- .miss_apply_verified_field_checks(
    expected_result = expected_result,
    verified_keys = verified_keys
  )
  expected_result <- verification_result$expected_result
  expected <- expected_result$rows
  if (isTRUE(details)) {
    expected <- .miss_add_validation_context(expected)
  }
  field_complete_failures <- if (isTRUE(details) && !isTRUE(defer_assembly)) {
    expected[!expected$validation_passed, , drop = FALSE]
  } else {
    .miss_empty_expected()
  }
  flex_event_forms_field_counts <- .miss_build_indexed_field_counts(
    record_eligibility = record_eligibility,
    form_records = form_records,
    started_positions = started_positions,
    expected_result = expected_result,
    project = project,
    form = form
  )
  .miss_cli_report_form_progress(
    progress_callback,
    stage = "field_complete",
    force = FALSE
  )

  stage_started_at <- .miss_timer_start(timer)
  check_rows <- list(
    event_row_started_checks = event_checks,
    instance_row_started_checks = repeat_checks,
    form_started_checks = form_checks,
    field_complete_checks = expected
  )
  row_counts <- vapply(check_rows, nrow, integer(1))
  out <- list(
    summary = if (isTRUE(defer_assembly)) {
      .miss_empty_validation_summary()
    } else {
      .miss_build_form_validation_summary(c(
        check_rows,
        list(
          form = form,
          zero_field_complete_contexts = zero_field_complete_contexts
        )
      ))
    },
    missing = if (isTRUE(defer_assembly)) {
      .miss_empty_missing_rows()
    } else {
      .miss_build_form_missing_rows(check_rows)
    },
    check_rows = check_rows,
    row_counts = row_counts,
    record_ids = unique(.miss_chr_vec(record_eligibility$record_id[
      !.miss_is_blank_vec(record_eligibility$record_id)
    ])),
    events = project$events %||% character(),
    event_labels = project$event_labels,
    instances = instances,
    instances_defaulted = resolved_instances$defaulted,
    record_eligibility = record_eligibility,
    flex_event_forms_field_counts = flex_event_forms_field_counts,
    verification_overrides_applied =
      verification_result$overrides_applied,
    zero_field_complete_contexts = zero_field_complete_contexts,
    used_record_spec_keys = unique(record_eligibility$record_spec_key[
      !.miss_is_blank_vec(record_eligibility$record_spec_key)
    ]),
    ignored_fields = form_plan$ignore_roots,
    project = project,
    form = form,
    form_label = project$form_label,
    id_col = project$id_col,
    system_fields = project$system_fields
  )

  if (isTRUE(details) && !isTRUE(defer_assembly)) {
    validation_rows <- dplyr::bind_rows(check_rows)
    validation_rows <- .miss_add_validation_context(validation_rows)
    out <- c(out, list(
      validation_rows = validation_rows,
      event_row_started_checks = event_checks,
      event_row_started_failures = event_row_started_failures,
      instance_row_started_checks = repeat_checks,
      instance_row_started_failures = instance_row_started_failures,
      form_started_checks = form_checks,
      form_started_failures = form_started_failures,
      field_complete_checks = expected,
      field_complete_failures = field_complete_failures,
      field_plan = form_plan$field_plan
    ))
  }
  .miss_timer_record(timer, "form", "form_assembly", stage_started_at, form)

  out$stage_timings <- .miss_timer_rows(timer)
  out$form_workload <- .miss_form_workload_row(
    form = form,
    form_records = form_records,
    expected_contexts = expected_contexts,
    started_positions = started_positions,
    form_plan = form_plan,
    validation_rows = sum(row_counts)
  )

  .miss_cli_report_form_progress(
    progress_callback,
    stage = "complete",
    force = FALSE
  )
  out
}
