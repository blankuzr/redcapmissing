## Internal helpers shared by plan execution and report presentation --------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.miss_extract_logic_references <- function(logic) {
  logic <- .miss_chr_vec(logic)
  logic <- logic[!.miss_is_blank_vec(logic)]
  if (!length(logic)) {
    return(tibble::tibble(
      logic = character(), event = character(),
      field = character(), choice = character()
    ))
  }

  rows <- list()
  for (value in logic) {
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

  if (!length(rows)) {
    return(tibble::tibble(
      logic = character(), event = character(),
      field = character(), choice = character()
    ))
  }
  unique(dplyr::bind_rows(rows))
}

.miss_derive_field_names <- function(meta) {
  if (!nrow(meta)) {
    return(tibble::tibble(
      original_field_name = character(),
      choice_value = character(),
      export_field_name = character()
    ))
  }

  field_name <- .miss_chr_vec(meta$field_name)
  field_type <- .miss_chr_vec(meta$field_type)
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
    choices <- .miss_parse_choices(meta$select_choices_or_calculations[[i]])
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
          field, "___", .miss_choice_suffix(choices$code)
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

.miss_build_choice_map <- function(meta) {
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
      if (.miss_is_blank_scalar(choice_text)) return(NULL)
      choices <- .miss_parse_choices(choice_text)
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

.miss_parse_choices <- function(x) {
  x <- .miss_chr(x)
  if (.miss_is_blank_scalar(x)) {
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

.miss_system_fields <- function() {
  list(
    event_col = "redcap_event_name",
    repeat_instrument_col = "redcap_repeat_instrument",
    repeat_instance_col = "redcap_repeat_instance"
  )
}

.miss_validation_context_vec <- function(event, repeat_instance) {
  event <- as.character(event)
  repeat_instance <- as.character(repeat_instance)
  n <- max(length(event), length(repeat_instance))
  if (!n) return(character())
  event <- rep(event, length.out = n)
  repeat_instance <- rep(repeat_instance, length.out = n)
  has_event <- !is.na(event) & nzchar(event)
  has_repeat <- !is.na(repeat_instance) & nzchar(repeat_instance)

  context <- rep("overall", n)
  context[has_event & !has_repeat] <- paste0(
    "event: ", event[has_event & !has_repeat]
  )
  context[!has_event & has_repeat] <- paste0(
    "repeat: ", repeat_instance[!has_event & has_repeat]
  )
  context[has_event & has_repeat] <- paste0(
    "event: ", event[has_event & has_repeat],
    "; repeat: ", repeat_instance[has_event & has_repeat]
  )
  context
}