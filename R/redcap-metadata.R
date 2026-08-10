# REDCap metadata, field, choice, and checkbox contracts.

.metadata_validate_checkbox_fields <- function(metadata) {
  checkbox_rows <- which(metadata$field_type == "checkbox")
  if (!length(checkbox_rows)) return(invisible(metadata))
  choices_column <- "select_choices_or_calculations"
  if (!choices_column %in% names(metadata)) {
    .condition_signal_error(
      "Checkbox metadata requires `select_choices_or_calculations`.",
      "project"
    )
  }
  for (row in checkbox_rows) {
    field <- metadata$field_name[[row]]
    raw <- metadata[[choices_column]][[row]]
    if (.schema_detect_blank_value(raw)) {
      .condition_signal_error(
        paste0("Checkbox field `", field, "` requires a nonblank choice definition."),
        "project"
      )
    }
    parts <- strsplit(as.character(raw), "\\s*\\|\\s*", perl = TRUE)[[1L]]
    choices <- .metadata_parse_choices(raw)
    suffixes <- if (nrow(choices)) .branching_logic_format_choice_suffix(choices$code) else character()
    invalid <- nrow(choices) != length(parts) || !nrow(choices) ||
      any(is.na(choices$code) | !nzchar(trimws(choices$code))) ||
      any(is.na(choices$label) | !nzchar(trimws(choices$label))) ||
      any(!nzchar(suffixes)) || anyDuplicated(suffixes)
    if (invalid) {
      .condition_signal_error(
        paste0("Checkbox field `", field, "` has an invalid or ambiguous choice definition."),
        "project"
      )
    }
  }
  invisible(metadata)
}

.metadata_expand_field_names <- function(meta) {
  if (!nrow(meta)) {
    return(tibble::tibble(
      original_field_name = character(),
      choice_value = character(),
      export_field_name = character()
    ))
  }

  field_name <- .schema_normalize_character_vector(meta$field_name)
  field_type <- .schema_normalize_character_vector(meta$field_type)
  checkbox <- !is.na(field_type) & field_type == "checkbox"
  rows <- list(data.frame(
    original_field_name = field_name[!checkbox],
    choice_value = rep(NA_character_, sum(!checkbox)),
    export_field_name = field_name[!checkbox],
    .field_order = which(!checkbox),
    .choice_order = rep.int(1L, sum(!checkbox)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))

  for (i in which(checkbox)) {
    field <- field_name[[i]]
    choices <- .metadata_parse_choices(meta$select_choices_or_calculations[[i]])
    if (!nrow(choices)) {
      rows[[length(rows) + 1L]] <- data.frame(
        original_field_name = field,
        choice_value = NA_character_,
        export_field_name = field,
        .field_order = i,
        .choice_order = 1L,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      rows[[length(rows) + 1L]] <- data.frame(
        original_field_name = rep.int(field, nrow(choices)),
        choice_value = choices$code,
        export_field_name = paste0(
          field, "___", .branching_logic_format_choice_suffix(choices$code)
        ),
        .field_order = rep.int(i, nrow(choices)),
        .choice_order = seq_len(nrow(choices)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }

  out <- dplyr::bind_rows(rows)
  out <- out[order(out$.field_order, out$.choice_order), , drop = FALSE]
  tibble::as_tibble(out[, c(
    "original_field_name", "choice_value", "export_field_name"
  ), drop = FALSE])
}

.metadata_build_choice_map <- function(meta) {
  rows <- lapply(seq_len(nrow(meta)), function(i) {
    field <- meta$field_name[[i]]
    field_type <- meta$field_type[[i]]
    if (identical(field_type, "yesno")) {
      choices <- tibble::tibble(
        code = c("1", "0"), label = c("Yes", "No")
      )
    } else if (identical(field_type, "truefalse")) {
      choices <- tibble::tibble(
        code = c("1", "0"), label = c("True", "False")
      )
    } else {
      choice_text <- if ("select_choices_or_calculations" %in% names(meta)) {
        meta[["select_choices_or_calculations"]][[i]]
      } else {
        NA_character_
      }
      if (.schema_detect_blank_value(choice_text)) return(NULL)
      choices <- .metadata_parse_choices(choice_text)
    }
    if (!nrow(choices)) return(NULL)
    choices$field_name <- field
    choices[, c("field_name", "code", "label"), drop = FALSE]
  })

  out <- dplyr::bind_rows(rows)
  if (!ncol(out)) {
    return(tibble::tibble(
      field_name = character(), code = character(), label = character()
    ))
  }
  out
}

.metadata_parse_choices <- function(x) {
  x <- .schema_normalize_character_scalar(x)
  if (.schema_detect_blank_value(x)) {
    return(tibble::tibble(code = character(), label = character()))
  }
  parts <- unlist(strsplit(x, "\\s*\\|\\s*", perl = TRUE), use.names = FALSE)
  rows <- lapply(parts, function(part) {
    match <- regexec("^\\s*([^,]+?)\\s*,\\s*(.*?)\\s*$", part, perl = TRUE)
    bits <- regmatches(part, match)[[1]]
    if (!length(bits)) return(NULL)
    tibble::tibble(code = trimws(bits[[2]]), label = trimws(bits[[3]]))
  })
  dplyr::bind_rows(rows)
}

.metadata_build_field_dictionary <- function(metadata) {
  field_names <- as.character(metadata$field_name)
  duplicated_fields <- unique(field_names[duplicated(field_names)])
  if (length(duplicated_fields)) {
    .condition_signal_error(
      paste0(
        "Metadata must define every field exactly once; duplicated field(s): ",
        paste(duplicated_fields, collapse = ", "),
        "."
      ),
      "project"
    )
  }

  derived <- .metadata_expand_field_names(metadata)
  export_fields <- split(
    as.character(derived$export_field_name),
    factor(derived$original_field_name, levels = field_names),
    drop = FALSE
  )

  choice_map <- .metadata_build_choice_map(metadata)
  field_entries <- new.env(
    hash = TRUE,
    parent = emptyenv()
  )

  list(
    export_fields = export_fields,
    choice_map = choice_map,
    field_entries = field_entries
  )
}

.metadata_populate_field_dictionary_entries <- function(
  field_dictionary,
  metadata,
  fields
) {
  if (is.null(field_dictionary) ||
      !is.environment(field_dictionary$field_entries)) {
    return(invisible(field_dictionary))
  }

  fields <- unique(as.character(fields))
  fields <- fields[!is.na(fields) & nzchar(fields)]
  if (!length(fields)) return(invisible(field_dictionary))

  field_indices <- match(fields, as.character(metadata$field_name))
  known <- !is.na(field_indices)
  fields <- fields[known]
  field_indices <- field_indices[known]
  if (!length(fields)) return(invisible(field_dictionary))

  field_type <- .schema_normalize_character_vector(
    metadata$field_type[field_indices]
  )
  validation <- if (
    "text_validation_type_or_show_slider_number" %in% names(metadata)
  ) {
    .schema_normalize_character_vector(
      metadata$text_validation_type_or_show_slider_number[field_indices]
    )
  } else {
    rep.int("", length(fields))
  }
  numeric_field <-
    validation %in% c("number", "integer", "float", "int") |
    field_type %in% "calc"

  choice_map <- field_dictionary$choice_map
  choice_rows <- choice_map$field_name %in% fields
  choices_by_field <- split(
    choice_map[choice_rows, c("code", "label"), drop = FALSE],
    factor(choice_map$field_name[choice_rows], levels = fields),
    drop = FALSE
  )
  export_fields <- unname(field_dictionary$export_fields[fields])

  for (field_index in seq_along(fields)) {
    assign(
      fields[[field_index]],
      list(
        export_fields = unname(as.character(export_fields[[field_index]])),
        field_type = field_type[[field_index]],
        numeric_field = isTRUE(numeric_field[[field_index]]),
        choices = choices_by_field[[field_index]]
      ),
      envir = field_dictionary$field_entries
    )
  }
  invisible(field_dictionary)
}
