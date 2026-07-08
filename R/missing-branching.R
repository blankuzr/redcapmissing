## Internal helpers: branching logic --------------------------------------

.miss_branch_satisfied <- function(
  logic,
  branch_plan = NULL,
  branch_cache = NULL,
  records,
  lookup_records,
  meta,
  choice_map,
  project
) {
  if (.miss_is_blank_scalar(logic)) {
    return(rep(TRUE, nrow(records)))
  }

  branch_plan <- branch_plan %||% .miss_compile_branch_logic(logic)
  expr <- branch_plan$expr
  env <- new.env(parent = baseenv())

  env$.v <- function(event, field, choice = NULL) {
    .miss_branch_value(
      records = records,
      lookup_records = lookup_records,
      event = event,
      field = field,
      choice = choice,
      meta = meta,
      choice_map = choice_map,
      project = project,
      branch_cache = branch_cache
    )
  }
  env$contains <- function(x, pattern) {
    grepl(pattern, x, fixed = TRUE)
  }
  env$isblank <- function(x) {
    .miss_is_blank_vec(x)
  }
  env$notblank <- function(x) {
    !.miss_is_blank_vec(x)
  }
  env$datediff <- function(date1, date2, units = "d", dateformat = "ymd", ...) {
    .miss_datediff(date1 = date1, date2 = date2, units = units)
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

.miss_compile_branch_logic <- function(logic) {
  if (.miss_is_blank_scalar(logic)) {
    return(list(logic = logic, expr = NULL, parsed_expr = NULL))
  }

  expr <- .miss_logic_to_r(logic)
  list(
    logic = logic,
    expr = expr,
    parsed_expr = parse(text = expr)
  )
}

.miss_logic_to_r <- function(logic) {
  logic <- .miss_replace_refs(logic)
  quoted <- .miss_protect_quotes(logic)
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

  .miss_restore_quotes(expr, quoted$values)
}

.miss_replace_refs <- function(logic) {
  event_ref <- "\\[([^\\]]+)\\]\\[([^\\]]+)\\]"
  logic <- stringr::str_replace_all(logic, event_ref, function(x) {
    bits <- stringr::str_match(x, event_ref)
    .miss_format_ref_calls(refs = bits[, 3], events = bits[, 2])
  })

  same_ref <- "\\[([^\\]]+)\\]"
  stringr::str_replace_all(logic, same_ref, function(x) {
    bits <- stringr::str_match(x, same_ref)
    .miss_format_ref_calls(refs = bits[, 2])
  })
}

.miss_format_ref_calls <- function(refs, events = NULL) {
  refs <- as.character(refs)
  parsed <- lapply(refs, .miss_parse_ref)
  fields <- vapply(
    parsed,
    function(ref) .miss_quote(ref$field),
    character(1)
  )
  choices <- vapply(
    parsed,
    function(ref) .miss_quote(ref$choice),
    character(1)
  )
  events <- if (is.null(events)) {
    rep("NULL", length(refs))
  } else {
    vapply(as.character(events), .miss_quote, character(1))
  }

  paste0(".v(", events, ", ", fields, ", ", choices, ")")
}

.miss_parse_ref <- function(ref) {
  match <- regexec("^([^()]+?)(?:\\((.*)\\))?$", ref, perl = TRUE)
  bits <- regmatches(ref, match)[[1]]
  if (length(bits) == 0) {
    stop("Could not parse REDCap field reference `", ref, "`.", call. = FALSE)
  }

  choice <- if (length(bits) >= 3 && nzchar(bits[[3]])) bits[[3]] else NULL
  list(field = bits[[2]], choice = choice)
}

.miss_branch_value <- function(
  records,
  lookup_records,
  event,
  field,
  choice,
  meta,
  choice_map,
  project,
  branch_cache = NULL
) {
  field_type <- .miss_field_type(meta = meta, field = field)

  if (!is.null(choice) || identical(field_type, "checkbox")) {
    if (is.null(choice)) {
      child_fields <- .miss_derive_field_names(meta[
        meta$field_name == field,
        ,
        drop = FALSE
      ])$export_field_name
      selected <- vapply(
        child_fields,
        function(child) {
          .miss_checkbox_selected_vec(.miss_branch_raw_value(
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

    child <- paste0(field, "___", .miss_choice_suffix(choice))
    return(as.integer(.miss_checkbox_selected_vec(.miss_branch_raw_value(
      records = records,
      lookup_records = lookup_records,
      event = event,
      field = child,
      project = project,
      branch_cache = branch_cache
    ))))
  }

  raw <- .miss_branch_raw_value(
    records = records,
    lookup_records = lookup_records,
    event = event,
    field = field,
    project = project,
    branch_cache = branch_cache
  )

  .miss_normalize_value(
    value = raw,
    field = field,
    meta = meta,
    choice_map = choice_map
  )
}

.miss_branch_raw_value <- function(
  records,
  lookup_records,
  event,
  field,
  project,
  branch_cache = NULL
) {
  if (is.null(event)) {
    return(.miss_col_vec(records, field))
  }
  cached <- .miss_branch_cache_get(branch_cache, event = event, field = field)
  if (!is.null(cached)) {
    return(cached)
  }

  fields <- project$system_fields
  event_col <- fields$event_col
  if (!event_col %in% names(lookup_records)) {
    value <- rep(NA_character_, nrow(records))
    .miss_branch_cache_set(
      branch_cache,
      event = event,
      field = field,
      value = value
    )
    return(value)
  }

  event_rows <- as.character(lookup_records[[event_col]]) == event
  event_records <- lookup_records[event_rows, , drop = FALSE]
  if (nrow(event_records) == 0 || !field %in% names(event_records)) {
    value <- rep(NA_character_, nrow(records))
    .miss_branch_cache_set(
      branch_cache,
      event = event,
      field = field,
      value = value
    )
    return(value)
  }

  row_match <- match(records[[project$id_col]], event_records[[project$id_col]])
  value <- rep(NA_character_, nrow(records))
  hit <- !is.na(row_match)
  value[hit] <- .miss_chr_vec(event_records[[field]][row_match[hit]])
  .miss_branch_cache_set(
    branch_cache,
    event = event,
    field = field,
    value = value
  )
  value
}

.miss_new_branch_cache <- function(records, lookup_records, project) {
  env <- new.env(parent = emptyenv())
  env$values <- list()
  env
}

.miss_branch_cache_key <- function(event, field) {
  paste(.miss_chr(event), .miss_chr(field), sep = "\r")
}

.miss_branch_cache_get <- function(branch_cache, event, field) {
  if (is.null(branch_cache)) {
    return(NULL)
  }

  key <- .miss_branch_cache_key(event = event, field = field)
  branch_cache$values[[key]] %||% NULL
}

.miss_branch_cache_set <- function(branch_cache, event, field, value) {
  if (is.null(branch_cache)) {
    return(invisible(value))
  }

  key <- .miss_branch_cache_key(event = event, field = field)
  branch_cache$values[[key]] <- value
  invisible(value)
}

.miss_field_present <- function(
  records,
  field,
  field_type,
  child_fields,
  choice_map
) {
  if (identical(field_type, "checkbox")) {
    child_fields <- child_fields[child_fields %in% names(records)]
    if (length(child_fields) == 0) {
      return(list(
        field_complete = rep(FALSE, nrow(records)),
        value_summary = rep("", nrow(records))
      ))
    }

    selected <- vapply(
      child_fields,
      function(child) .miss_checkbox_selected_vec(records[[child]]),
      logical(nrow(records))
    )
    if (is.null(dim(selected))) {
      selected <- matrix(selected, ncol = length(child_fields))
    }

    field_complete <- rowSums(selected, na.rm = TRUE) > 0
    value_summary <- apply(selected, 1, function(row) {
      picked <- child_fields[row]
      if (length(picked) == 0) "" else paste(picked, collapse = ", ")
    })

    return(list(
      field_complete = field_complete,
      value_summary = value_summary
    ))
  }

  value <- .miss_col_vec(records, field)
  list(
    field_complete = !.miss_is_blank_vec(value),
    value_summary = .miss_chr_vec(value)
  )
}

.miss_normalize_value <- function(value, field, meta, choice_map) {
  value <- .miss_chr_vec(value)
  blank <- .miss_is_blank_vec(value)

  if (!all(c("field_name", "code", "label") %in% names(choice_map))) {
    choice_map <- tibble::tibble(
      field_name = character(),
      code = character(),
      label = character()
    )
  }

  choices <- choice_map[choice_map$field_name == field, , drop = FALSE]
  if (nrow(choices) > 0) {
    code_match <- match(value, choices$code)
    label_match <- match(value, choices$label)
    out <- value
    out[!is.na(label_match)] <- choices$code[label_match[!is.na(label_match)]]
    out[!is.na(code_match)] <- choices$code[code_match[!is.na(code_match)]]
    out[blank] <- ""
    return(out)
  }

  if (.miss_numeric_field(meta = meta, field = field)) {
    out <- suppressWarnings(as.numeric(value))
    out[blank] <- NA_real_
    return(out)
  }

  value[blank] <- ""
  value
}

.miss_numeric_field <- function(meta, field) {
  idx <- which(meta$field_name == field)
  if (length(idx) == 0) {
    return(FALSE)
  }
  validation <- if (
    "text_validation_type_or_show_slider_number" %in% names(meta)
  ) {
    .miss_chr(meta$text_validation_type_or_show_slider_number[[idx[[1]]]])
  } else {
    ""
  }
  field_type <- .miss_chr(meta$field_type[[idx[[1]]]])
  validation %in%
    c("number", "integer", "float", "int") ||
    field_type %in% c("calc")
}

.miss_field_type <- function(meta, field) {
  idx <- which(meta$field_name == field)
  if (length(idx) == 0) {
    return(NA_character_)
  }
  .miss_chr(meta$field_type[[idx[[1]]]])
}

.miss_col_vec <- function(records, column) {
  if (is.null(column) || !column %in% names(records)) {
    return(rep(NA_character_, nrow(records)))
  }
  records[[column]]
}

.miss_chr_vec <- function(x) {
  if (length(x) == 0) {
    return(character())
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return(as.character(x))
  }
  if (is.list(x)) {
    return(vapply(x, .miss_chr, character(1)))
  }
  out <- as.character(x)
  out[is.na(x)] <- NA_character_
  out
}

.miss_checkbox_selected_vec <- function(value) {
  value <- tolower(.miss_chr_vec(value))
  !.miss_is_blank_vec(value) & !value %in% c("0", "unchecked", "false", "no")
}

.miss_is_blank_vec <- function(x) {
  if (length(x) == 0) {
    return(logical())
  }
  isNAorBlank(.miss_chr_vec(x))
}

.miss_is_blank_scalar <- function(x) {
  if (length(x) == 0) {
    return(TRUE)
  }
  .miss_is_blank_vec(x)[[1]]
}

.miss_required_vec <- function(x) {
  required <- tolower(trimws(.miss_chr_vec(x)))
  !.miss_is_blank_vec(required) & required %in% c("y", "yes", "true", "1")
}

.miss_chr <- function(x) {
  if (length(x) == 0 || is.null(x) || is.na(x[[1]])) {
    return(NA_character_)
  }
  as.character(x[[1]])
}

.miss_choice_suffix <- function(code) {
  gsub("[[:punct:]]", "_", trimws(as.character(code)))
}

.miss_quote <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("NULL")
  }
  paste0("\"", gsub("([\\\\\"])", "\\\\\\1", x, perl = TRUE), "\"")
}

.miss_protect_quotes <- function(x) {
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

.miss_restore_quotes <- function(x, values) {
  if (length(values) == 0) {
    return(x)
  }

  for (placeholder in names(values)) {
    x <- gsub(placeholder, values[[placeholder]], x, fixed = TRUE)
  }
  x
}

.miss_datediff <- function(date1, date2, units = "d") {
  blank <- .miss_is_blank_vec(date1) | .miss_is_blank_vec(date2)
  diff_days <- as.numeric(as.Date(date2) - as.Date(date1))
  diff_days[blank] <- NA_real_

  units <- tolower(.miss_chr(units))
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
