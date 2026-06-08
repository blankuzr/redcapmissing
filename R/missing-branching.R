## Internal helpers: branching logic --------------------------------------

.miss_branch_satisfied <- function(
  logic,
  records,
  lookup_records,
  meta,
  choice_map,
  project
) {
  if (.miss_is_blank_scalar(logic)) {
    return(rep(TRUE, nrow(records)))
  }

  expr <- .miss_logic_to_r(logic)
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
      project = project
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
    eval(parse(text = expr), envir = env),
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
    ref <- .miss_parse_ref(bits[[3]])
    paste0(
      ".v(",
      .miss_quote(bits[[2]]),
      ", ",
      .miss_quote(ref$field),
      ", ",
      .miss_quote(ref$choice),
      ")"
    )
  })

  same_ref <- "\\[([^\\]]+)\\]"
  stringr::str_replace_all(logic, same_ref, function(x) {
    bits <- stringr::str_match(x, same_ref)
    ref <- .miss_parse_ref(bits[[2]])
    paste0(
      ".v(NULL, ",
      .miss_quote(ref$field),
      ", ",
      .miss_quote(ref$choice),
      ")"
    )
  })
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
  project
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
            project = project
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
      project = project
    ))))
  }

  raw <- .miss_branch_raw_value(
    records = records,
    lookup_records = lookup_records,
    event = event,
    field = field,
    project = project
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
  project
) {
  if (is.null(event)) {
    return(.miss_col_vec(records, field))
  }

  fields <- project$system_fields
  event_col <- fields$event_col
  if (!event_col %in% names(lookup_records)) {
    return(rep(NA_character_, nrow(records)))
  }

  event_rows <- as.character(lookup_records[[event_col]]) == event
  event_records <- lookup_records[event_rows, , drop = FALSE]
  if (nrow(event_records) == 0 || !field %in% names(event_records)) {
    return(rep(NA_character_, nrow(records)))
  }

  row_match <- match(records[[project$id_col]], event_records[[project$id_col]])
  value <- rep(NA_character_, nrow(records))
  hit <- !is.na(row_match)
  value[hit] <- .miss_chr_vec(event_records[[field]][row_match[hit]])
  value
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
        value_present = rep(FALSE, nrow(records)),
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

    value_present <- rowSums(selected, na.rm = TRUE) > 0
    value_summary <- apply(selected, 1, function(row) {
      picked <- child_fields[row]
      if (length(picked) == 0) "" else paste(picked, collapse = ", ")
    })

    return(list(
      value_present = value_present,
      value_summary = value_summary
    ))
  }

  value <- .miss_col_vec(records, field)
  list(
    value_present = !.miss_is_blank_vec(value),
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

.miss_get_failed_rows <- function(agent) {
  extracts <- pointblank::get_data_extracts(agent)
  if (length(extracts) == 0) {
    return(tibble::tibble())
  }

  out <- dplyr::bind_rows(extracts, .id = "pointblank_extract")
  if (all(c("missing_scope", "form_name") %in% names(out))) {
    out$pointblank_step <- dplyr::case_when(
      out$missing_scope == "form_blank" ~ paste0(
        out$form_name,
        "_entire_form_blank"
      ),
      out$missing_scope == "event_absent" ~ paste0(
        out$form_name,
        "_event_row_missing"
      ),
      out$missing_scope == "repeat_absent" ~ paste0(
        out$form_name,
        "_repeat_instance_missing"
      ),
      TRUE ~ paste0(out$form_name, "_missing_fields")
    )
  } else {
    out$pointblank_step <- out$pointblank_extract
  }

  select_cols <- c(
    "pointblank_step",
    "pointblank_extract",
    "record_id",
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "form_name",
    "field_name",
    "field_label",
    "field_type",
    "check_scope",
    "missing_scope",
    "branching_logic",
    "form_started",
    "event_row_present",
    "repeat_instance_present",
    "value_present",
    "value_summary",
    "export_fields"
  )

  out |>
    dplyr::select(dplyr::all_of(select_cols), dplyr::everything())
}
