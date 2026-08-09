# REDCap branching-logic parsing, resolution, and evaluation.

.branching_logic_extract_references <- function(logic) {
  logic <- .schema_normalize_character_vector(logic)
  logic <- logic[!.schema_detect_blank_values(logic)]
  logic <- unique(logic)
  if (!length(logic)) {
    return(tibble::tibble(
      logic = character(), event = character(),
      field = character(), choice = character()
    ))
  }

  event_pattern <- "\\[([^\\]]+)\\]\\[([^\\]]+)\\]"
  same_pattern <- "\\[([^\\]]+)\\]"
  logic_chunks <- vector("list", length(logic))
  event_chunks <- vector("list", length(logic))
  field_chunks <- vector("list", length(logic))
  choice_chunks <- vector("list", length(logic))

  for (logic_index in seq_along(logic)) {
    value <- logic[[logic_index]]
    event_values <- event_fields <- event_choices <- character()
    event_matches <- gregexpr(event_pattern, value, perl = TRUE)[[1]]
    if (!identical(event_matches[[1]], -1L)) {
      refs <- regmatches(value, list(event_matches))[[1]]
      bits <- lapply(refs, function(ref) {
        regmatches(ref, regexec(event_pattern, ref, perl = TRUE))[[1]]
      })
      event_values <- vapply(bits, `[[`, character(1), 2L)
      event_references <- vapply(bits, `[[`, character(1), 3L)
      parsed <- lapply(event_references, .branching_logic_parse_reference)
      event_fields <- vapply(parsed, function(ref) ref$field, character(1))
      event_choices <- vapply(
        parsed,
        function(ref) ref$choice %||% NA_character_,
        character(1)
      )
    }

    same_value <- gsub(event_pattern, "", value, perl = TRUE)
    same_fields <- same_choices <- character()
    same_matches <- gregexpr(same_pattern, same_value, perl = TRUE)[[1]]
    if (!identical(same_matches[[1]], -1L)) {
      refs <- regmatches(same_value, list(same_matches))[[1]]
      same_references <- gsub("^\\[|\\]$", "", refs)
      parsed <- lapply(same_references, .branching_logic_parse_reference)
      same_fields <- vapply(parsed, function(ref) ref$field, character(1))
      same_choices <- vapply(
        parsed,
        function(ref) ref$choice %||% NA_character_,
        character(1)
      )
    }

    reference_n <- length(event_fields) + length(same_fields)
    logic_chunks[[logic_index]] <- rep.int(value, reference_n)
    event_chunks[[logic_index]] <- c(
      event_values,
      rep.int(NA_character_, length(same_fields))
    )
    field_chunks[[logic_index]] <- c(event_fields, same_fields)
    choice_chunks[[logic_index]] <- c(event_choices, same_choices)
  }

  if (!sum(lengths(logic_chunks))) {
    return(tibble::tibble(
      logic = character(), event = character(),
      field = character(), choice = character()
    ))
  }

  references <- tibble::tibble(
    logic = unlist(logic_chunks, use.names = FALSE),
    event = unlist(event_chunks, use.names = FALSE),
    field = unlist(field_chunks, use.names = FALSE),
    choice = unlist(choice_chunks, use.names = FALSE)
  )
  unique(references)
}

.branching_logic_evaluate_rows <- function(
  logic,
  branch_plan = NULL,
  branch_cache = NULL,
  records,
  lookup_records,
  meta,
  choice_map,
  project,
  field_dictionary = NULL
) {
  if (.schema_detect_blank_value(logic)) {
    return(rep(TRUE, nrow(records)))
  }

  branch_plan <- branch_plan %||% .branching_logic_compile_expression(logic)
  expr <- branch_plan$expr
  env <- new.env(parent = baseenv())

  env$.v <- function(event, field, choice = NULL) {
    .branching_logic_resolve_value(
      records = records,
      lookup_records = lookup_records,
      event = event,
      field = field,
      choice = choice,
      meta = meta,
      choice_map = choice_map,
      project = project,
      branch_cache = branch_cache,
      field_dictionary = field_dictionary
    )
  }
  env$contains <- function(x, pattern) {
    grepl(pattern, x, fixed = TRUE)
  }
  env$isblank <- function(x) {
    .schema_detect_blank_values(x)
  }
  env$notblank <- function(x) {
    !.schema_detect_blank_values(x)
  }
  env$datediff <- function(date1, date2, units = "d", dateformat = "ymd", ...) {
    .branching_logic_compute_date_difference(date1 = date1, date2 = date2, units = units)
  }
  env$.redcap_if <- function(condition, yes, no) {
    ifelse(condition, yes, no)
  }

  value <- tryCatch(
    eval(branch_plan$parsed_expr, envir = env),
    error = function(e) {
      stop(
        "Could not evaluate branching logic `",
        logic,
        "`. Translated R was `",
        expr,
        "`. ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  value <- rep(value, length.out = nrow(records))
  value <- as.logical(value)
  !is.na(value) & value
}

.branching_logic_compile_expression <- function(logic) {
  if (.schema_detect_blank_value(logic)) {
    return(list(logic = logic, expr = NULL, parsed_expr = NULL))
  }

  expr <- .branching_logic_translate_expression(logic)
  list(
    logic = logic,
    expr = expr,
    parsed_expr = parse(text = expr)
  )
}

.branching_logic_translate_expression <- function(logic) {
  logic <- .branching_logic_replace_references(logic)
  quoted <- .branching_logic_protect_quoted_values(logic)
  expr <- quoted$text

  expr <- gsub("<>", "!=", expr, fixed = TRUE)
  expr <- gsub(
    "\\bif\\s*\\(",
    ".redcap_if(",
    expr,
    perl = TRUE,
    ignore.case = TRUE
  )
  expr <- gsub("\\band\\b", "&", expr, perl = TRUE, ignore.case = TRUE)
  expr <- gsub("\\bor\\b", "|", expr, perl = TRUE, ignore.case = TRUE)
  expr <- gsub("(?<![!<>=])=(?!=)", "==", expr, perl = TRUE)

  .branching_logic_restore_quoted_values(expr, quoted$values)
}

.branching_logic_replace_references <- function(logic) {
  event_ref <- "\\[([^\\]]+)\\]\\[([^\\]]+)\\]"
  logic <- stringr::str_replace_all(logic, event_ref, function(x) {
    bits <- stringr::str_match(x, event_ref)
    .branching_logic_format_reference_calls(refs = bits[, 3], events = bits[, 2])
  })

  same_ref <- "\\[([^\\]]+)\\]"
  stringr::str_replace_all(logic, same_ref, function(x) {
    bits <- stringr::str_match(x, same_ref)
    .branching_logic_format_reference_calls(refs = bits[, 2])
  })
}

.branching_logic_format_reference_calls <- function(refs, events = NULL) {
  refs <- as.character(refs)
  parsed <- lapply(refs, .branching_logic_parse_reference)
  fields <- vapply(
    parsed,
    function(ref) .branching_logic_quote_string(ref$field),
    character(1)
  )
  choices <- vapply(
    parsed,
    function(ref) .branching_logic_quote_string(ref$choice),
    character(1)
  )
  events <- if (is.null(events)) {
    rep("NULL", length(refs))
  } else {
    vapply(as.character(events), .branching_logic_quote_string, character(1))
  }

  paste0(".v(", events, ", ", fields, ", ", choices, ")")
}

.branching_logic_parse_reference <- function(ref) {
  match <- regexec("^([^()]+?)(?:\\((.*)\\))?$", ref, perl = TRUE)
  bits <- regmatches(ref, match)[[1]]
  if (length(bits) == 0) {
    stop("Could not parse REDCap field reference `", ref, "`.", call. = FALSE)
  }

  choice <- if (length(bits) >= 3 && nzchar(bits[[3]])) bits[[3]] else NULL
  list(field = bits[[2]], choice = choice)
}

.branching_logic_resolve_field_entry <- function(field_dictionary, field) {
  if (is.null(field_dictionary) ||
      !is.environment(field_dictionary$field_entries)) {
    return(NULL)
  }
  field <- as.character(field)
  if (length(field) != 1L || is.na(field) || !nzchar(field)) {
    return(NULL)
  }
  field_entry <- get0(
    field,
    envir = field_dictionary$field_entries,
    inherits = FALSE,
    ifnotfound = NULL
  )
  required_names <- c(
    "export_fields", "field_type", "numeric_field", "choices"
  )
  if (!is.list(field_entry) ||
      !all(required_names %in% names(field_entry))) {
    return(NULL)
  }
  field_entry
}

.branching_logic_resolve_value <- function(
  records,
  lookup_records,
  event,
  field,
  choice,
  meta,
  choice_map,
  project,
  branch_cache = NULL,
  field_dictionary = NULL
) {
  field_entry <- .branching_logic_resolve_field_entry(
    field_dictionary = field_dictionary,
    field = field
  )
  field_type <- .branching_logic_resolve_field_type(
    meta = meta,
    field = field,
    field_dictionary = field_dictionary,
    field_entry = field_entry
  )

  if (!is.null(choice) || identical(field_type, "checkbox")) {
    if (is.null(choice)) {
      child_fields <- if (!is.null(field_entry)) {
        unname(as.character(field_entry$export_fields))
      } else {
        .metadata_expand_field_names(meta[
          meta$field_name == field,
          ,
          drop = FALSE
        ])$export_field_name
      }
      selected <- vapply(
        child_fields,
        function(child) {
          .branching_logic_detect_selected_checkbox(.branching_logic_resolve_raw_value(
            records = records,
            lookup_records = lookup_records,
            event = event,
            field = child,
            project = project,
            branch_cache = branch_cache
          ))
        },
        logical(nrow(records))
      )
      if (is.null(dim(selected))) {
        selected <- matrix(selected, ncol = length(child_fields))
      }
      return(as.integer(rowSums(selected, na.rm = TRUE) > 0))
    }

    child <- paste0(field, "___", .branching_logic_format_choice_suffix(choice))
    return(as.integer(.branching_logic_detect_selected_checkbox(.branching_logic_resolve_raw_value(
      records = records,
      lookup_records = lookup_records,
      event = event,
      field = child,
      project = project,
      branch_cache = branch_cache
    ))))
  }

  raw <- .branching_logic_resolve_raw_value(
    records = records,
    lookup_records = lookup_records,
    event = event,
    field = field,
    project = project,
    branch_cache = branch_cache
  )

  .branching_logic_normalize_value(
    value = raw,
    field = field,
    meta = meta,
    choice_map = choice_map,
    field_dictionary = field_dictionary,
    field_entry = field_entry
  )
}

.branching_logic_resolve_raw_value <- function(
  records,
  lookup_records,
  event,
  field,
  project,
  branch_cache = NULL
) {
  if (is.null(event)) {
    return(.schema_extract_column_vector(records, field))
  }
  cached <- .branching_logic_get_cached_value(branch_cache, event = event, field = field)
  if (!is.null(cached)) {
    return(cached)
  }

  event_index <- .branching_logic_match_event_rows(
    branch_cache = branch_cache,
    records = records,
    lookup_records = lookup_records,
    event = event,
    project = project
  )
  if (!any(!is.na(event_index$source_row)) ||
      !field %in% names(lookup_records)) {
    value <- rep(NA_character_, nrow(records))
    .branching_logic_set_cached_value(
      branch_cache,
      event = event,
      field = field,
      value = value
    )
    return(value)
  }

  value <- rep(NA_character_, nrow(records))
  hit <- !is.na(event_index$source_row)
  value[hit] <- .schema_normalize_character_vector(lookup_records[[field]][event_index$source_row[hit]])
  .branching_logic_set_cached_value(
    branch_cache,
    event = event,
    field = field,
    value = value
  )
  value
}

.branching_logic_build_cache <- function(records, lookup_records, project) {
  env <- new.env(hash = TRUE, parent = emptyenv())
  env$values <- new.env(hash = TRUE, parent = emptyenv())
  env$event_indices <- new.env(hash = TRUE, parent = emptyenv())
  env
}

.branching_logic_get_cached_value <- function(branch_cache, event, field) {
  if (is.null(branch_cache) || length(event) != 1L || is.na(event) ||
      length(field) != 1L || is.na(field)) return(NULL)

  event <- as.character(event)
  field <- as.character(field)
  if (!exists(event, envir = branch_cache$values, inherits = FALSE)) {
    return(NULL)
  }
  event_cache <- get(event, envir = branch_cache$values, inherits = FALSE)
  if (!exists(field, envir = event_cache, inherits = FALSE)) return(NULL)
  get(field, envir = event_cache, inherits = FALSE)
}

.branching_logic_set_cached_value <- function(branch_cache, event, field, value) {
  if (is.null(branch_cache) || length(event) != 1L || is.na(event) ||
      length(field) != 1L || is.na(field)) return(invisible(value))

  event <- as.character(event)
  field <- as.character(field)
  if (exists(event, envir = branch_cache$values, inherits = FALSE)) {
    event_cache <- get(event, envir = branch_cache$values, inherits = FALSE)
  } else {
    event_cache <- new.env(hash = TRUE, parent = emptyenv())
    assign(event, event_cache, envir = branch_cache$values)
  }
  assign(field, value, envir = event_cache)
  invisible(value)
}

.branching_logic_match_event_rows <- function(
  branch_cache,
  records,
  lookup_records,
  event,
  project
) {
  empty <- list(source_row = rep.int(NA_integer_, nrow(records)))
  if (length(event) != 1L || is.na(event)) return(empty)
  event <- as.character(event)
  if (!nzchar(event)) return(empty)

  if (!is.null(branch_cache) && exists(
    event,
    envir = branch_cache$event_indices,
    inherits = FALSE
  )) {
    return(get(
      event,
      envir = branch_cache$event_indices,
      inherits = FALSE
    ))
  }

  event_col <- project$system_fields$event_col
  repeat_instance_col <- project$system_fields$repeat_instance_col
  id_col <- project$id_col
  result <- empty
  if (event_col %in% names(lookup_records) &&
      id_col %in% names(records) && id_col %in% names(lookup_records)) {
    event_value <- as.character(lookup_records[[event_col]])
    event_rows <- which(!is.na(event_value) & event_value == event)
    if (length(event_rows)) {
      query_id <- .schema_normalize_character_vector(records[[id_col]])
      source_id <- .schema_normalize_character_vector(lookup_records[[id_col]][event_rows])
      repeat_instance <- if (repeat_instance_col %in% names(lookup_records)) {
        lookup_records[[repeat_instance_col]][event_rows]
      } else {
        rep.int(NA_integer_, length(event_rows))
      }

      missing_repeat_instance_positions <- which(is.na(repeat_instance))
      rows_with_missing_repeat_instance <-
        event_rows[missing_repeat_instance_positions]
      ids_with_missing_repeat_instance <-
        source_id[missing_repeat_instance_positions]
      duplicated_ids_with_missing_repeat_instance <- unique(
        ids_with_missing_repeat_instance[
          duplicated(ids_with_missing_repeat_instance) |
            duplicated(ids_with_missing_repeat_instance, fromLast = TRUE)
        ]
      )
      if (any(query_id %in% duplicated_ids_with_missing_repeat_instance)) {
        stop(
          "Cross event branching source event `",
          event,
          "` has multiple rows with a missing `redcap_repeat_instance` ",
          "for the same record.",
          call. = FALSE
        )
      }

      missing_repeat_instance_match <-
        match(query_id, ids_with_missing_repeat_instance)
      source_row <- rep.int(NA_integer_, length(query_id))
      missing_repeat_instance_hit <- !is.na(missing_repeat_instance_match)
      source_row[missing_repeat_instance_hit] <-
        rows_with_missing_repeat_instance[
          missing_repeat_instance_match[missing_repeat_instance_hit]
        ]

      positive_repeat_instance_positions <- which(!is.na(repeat_instance))
      rows_with_positive_repeat_instance <-
        event_rows[positive_repeat_instance_positions]
      ids_with_positive_repeat_instance <-
        source_id[positive_repeat_instance_positions]
      duplicated_ids_with_positive_repeat_instance <- unique(
        ids_with_positive_repeat_instance[
          duplicated(ids_with_positive_repeat_instance) |
            duplicated(ids_with_positive_repeat_instance, fromLast = TRUE)
        ]
      )
      ambiguous <- is.na(source_row) &
        query_id %in% duplicated_ids_with_positive_repeat_instance
      if (any(ambiguous)) {
        stop(
          "Cross event branching source event `",
          event,
          "` has multiple rows with a positive `redcap_repeat_instance` ",
          "and no row with a missing `redcap_repeat_instance` for ",
          sum(ambiguous),
          " assessed target(s); an unqualified event reference is ambiguous.",
          call. = FALSE
        )
      }

      positive_repeat_instance_match <-
        match(query_id, ids_with_positive_repeat_instance)
      positive_repeat_instance_hit <-
        is.na(source_row) & !is.na(positive_repeat_instance_match)
      source_row[positive_repeat_instance_hit] <-
        rows_with_positive_repeat_instance[
          positive_repeat_instance_match[positive_repeat_instance_hit]
        ]
      result <- list(
        source_row = as.integer(source_row)
      )
    }
  }

  if (!is.null(branch_cache)) {
    assign(event, result, envir = branch_cache$event_indices)
  }
  result
}

.branching_logic_normalize_value <- function(
  value,
  field,
  meta,
  choice_map,
  field_dictionary = NULL,
  field_entry = NULL
) {
  value <- .schema_normalize_character_vector(value)
  blank <- .schema_detect_blank_values(value)

  if (missing(field_entry)) {
    field_entry <- .branching_logic_resolve_field_entry(
      field_dictionary = field_dictionary,
      field = field
    )
  }
  choices <- NULL
  if (!is.null(field_entry)) {
    choices <- field_entry$choices
  }
  if (is.null(choices) ||
      !all(c("code", "label") %in% names(choices))) {
    if (!all(c("field_name", "code", "label") %in% names(choice_map))) {
      choice_map <- tibble::tibble(
        field_name = character(),
        code = character(),
        label = character()
      )
    }
    choices <- choice_map[choice_map$field_name == field, , drop = FALSE]
  }

  if (nrow(choices) > 0) {
    code_match <- match(value, choices$code)
    label_match <- match(value, choices$label)
    out <- value
    out[!is.na(label_match)] <- choices$code[label_match[!is.na(label_match)]]
    out[!is.na(code_match)] <- choices$code[code_match[!is.na(code_match)]]
    out[blank] <- ""
    return(out)
  }

  if (.branching_logic_detect_numeric_field(
    meta = meta,
    field = field,
    field_dictionary = field_dictionary,
    field_entry = field_entry
  )) {
    out <- suppressWarnings(as.numeric(value))
    out[blank] <- NA_real_
    return(out)
  }

  value[blank] <- ""
  value
}

.branching_logic_detect_numeric_field <- function(
  meta,
  field,
  field_dictionary = NULL,
  field_entry = NULL
) {
  if (missing(field_entry)) {
    field_entry <- .branching_logic_resolve_field_entry(
      field_dictionary = field_dictionary,
      field = field
    )
  }
  if (!is.null(field_entry)) {
    return(isTRUE(field_entry$numeric_field))
  }

  idx <- which(meta$field_name == field)
  if (length(idx) == 0) {
    return(FALSE)
  }
  validation <- if (
    "text_validation_type_or_show_slider_number" %in% names(meta)
  ) {
    .schema_normalize_character_scalar(meta$text_validation_type_or_show_slider_number[[idx[[1]]]])
  } else {
    ""
  }
  field_type <- .schema_normalize_character_scalar(meta$field_type[[idx[[1]]]])
  validation %in%
    c("number", "integer", "float", "int") ||
    field_type %in% c("calc")
}

.branching_logic_resolve_field_type <- function(
  meta,
  field,
  field_dictionary = NULL,
  field_entry = NULL
) {
  if (missing(field_entry)) {
    field_entry <- .branching_logic_resolve_field_entry(
      field_dictionary = field_dictionary,
      field = field
    )
  }
  if (!is.null(field_entry)) {
    return(.schema_normalize_character_scalar(
      field_entry$field_type
    ))
  }

  idx <- which(meta$field_name == field)
  if (length(idx) == 0) {
    return(NA_character_)
  }
  .schema_normalize_character_scalar(meta$field_type[[idx[[1]]]])
}

.branching_logic_detect_selected_checkbox <- function(value) {
  value <- tolower(.schema_normalize_character_vector(value))
  !.schema_detect_blank_values(value) & !value %in% c("0", "unchecked", "false", "no")
}

.branching_logic_format_choice_suffix <- function(code) {
  gsub("[[:punct:]]", "_", trimws(as.character(code)))
}

.branching_logic_quote_string <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("NULL")
  }
  paste0("\"", gsub("([\\\\\"])", "\\\\\\1", x, perl = TRUE), "\"")
}

.branching_logic_protect_quoted_values <- function(x) {
  matches <- gregexpr(
    "'([^'\\\\]|\\\\.)*'|\"([^\"\\\\]|\\\\.)*\"",
    x,
    perl = TRUE
  )[[1]]
  if (identical(matches[[1]], -1L)) {
    return(list(text = x, values = character()))
  }

  values <- regmatches(x, list(matches))[[1]]
  placeholders <- paste0("__MISS_QUOTED_", seq_along(values), "__")
  regmatches(x, list(matches)) <- list(placeholders)
  list(text = x, values = stats::setNames(values, placeholders))
}

.branching_logic_restore_quoted_values <- function(x, values) {
  if (length(values) == 0) {
    return(x)
  }

  for (placeholder in names(values)) {
    x <- gsub(placeholder, values[[placeholder]], x, fixed = TRUE)
  }
  x
}

.branching_logic_compute_date_difference <- function(date1, date2, units = "d") {
  blank <- .schema_detect_blank_values(date1) | .schema_detect_blank_values(date2)
  diff_days <- as.numeric(as.Date(date2) - as.Date(date1))
  diff_days[blank] <- NA_real_

  units <- tolower(.schema_normalize_character_scalar(units))
  switch(
    units,
    d = diff_days,
    day = diff_days,
    days = diff_days,
    w = diff_days / 7,
    week = diff_days / 7,
    weeks = diff_days / 7,
    m = diff_days / 30.4375,
    month = diff_days / 30.4375,
    months = diff_days / 30.4375,
    y = diff_days / 365.25,
    year = diff_days / 365.25,
    years = diff_days / 365.25,
    diff_days
  )
}
