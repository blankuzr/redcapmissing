## Verification evidence for plan execution --------------------------------

.verification_list_columns <- function() c(
  "project_id", "record", "event_id", "field_name", "repeat_instrument",
  "instance", "ts", "current_query_status", "username"
)

.verification_build_audit <- function(enabled = FALSE, verified_user = NA_character_,
                                    input_rows = 0L, user_rows = 0L,
                                    latest_user_rows = 0L, verified_rows = 0L,
                                    overrides_applied = 0L) {
  list(
    enabled = isTRUE(enabled),
    verified_user = if (isTRUE(enabled)) as.character(verified_user) else NA_character_,
    input_rows = as.integer(input_rows),
    user_rows = as.integer(user_rows),
    latest_user_rows = as.integer(latest_user_rows),
    verified_rows = as.integer(verified_rows),
    overrides_applied = as.integer(overrides_applied)
  )
}

.verification_signal_error <- function(message) {
  .condition_signal_error(message, subclass = "verification")
}

.verification_validate_input_pair <- function(verified, verified_user) {
  supplied_verified <- !is.null(verified)
  supplied_user <- !is.null(verified_user)
  if (xor(supplied_verified, supplied_user)) {
    .verification_signal_error("`verified` and `verified_user` must be supplied together or both be NULL.")
  }
  if (!supplied_verified) return(invisible(NULL))
  if (!is.data.frame(verified)) {
    .verification_signal_error("`verified` must be a data frame or tibble.")
  }
  if (!is.character(verified_user) || length(verified_user) != 1L ||
      is.na(verified_user) || !nzchar(verified_user) ||
      !identical(verified_user, trimws(verified_user))) {
    .verification_signal_error("`verified_user` must be one nonblank, unpadded character value.")
  }
  invisible(NULL)
}

.verification_normalize_nonblank_character <- function(x, column) {
  if (!is.character(x)) {
    .verification_signal_error(paste0("`verified$", column, "` must be a character column."))
  }
  if (any(is.na(x) | !nzchar(x) | x != trimws(x))) {
    .verification_signal_error(paste0("`verified$", column, "` must contain nonblank, unpadded values."))
  }
  x
}

.verification_normalize_identifier <- function(x, column, allow_factor = FALSE,
                                     whole_numeric = FALSE) {
  if (is.factor(x) && isTRUE(allow_factor)) x <- as.character(x)
  if (!(is.character(x) || is.integer(x) || is.double(x))) {
    .verification_signal_error(paste0("`verified$", column, "` has an unsupported storage type."))
  }
  if (is.numeric(x)) {
    invalid <- is.na(x) | !is.finite(x)
    if (isTRUE(whole_numeric)) invalid <- invalid | x != floor(x)
    if (any(invalid)) {
      .verification_signal_error(paste0("`verified$", column, "` contains an invalid numeric value."))
    }
    x <- .schema_format_numeric(x)
  }
  if (any(is.na(x) | !nzchar(x) | x != trimws(x))) {
    .verification_signal_error(paste0("`verified$", column, "` must contain nonblank, unpadded values."))
  }
  as.character(x)
}

.verification_normalize_nullable_character <- function(x, column) {
  if (is.factor(x)) {
    .verification_signal_error(paste0("`verified$", column, "` cannot use factor storage."))
  }
  if (is.logical(x) || is.integer(x) || is.double(x)) {
    if ((is.double(x) && any(is.nan(x))) || !all(is.na(x))) {
      .verification_signal_error(paste0("`verified$", column,
        "` may use a type other than character only when every value is a typed NA."))
    }
    return(rep(NA_character_, length(x)))
  }
  if (!is.character(x)) {
    .verification_signal_error(paste0("`verified$", column, "` has an unsupported storage type."))
  }
  blank <- is.na(x) | !nzchar(trimws(x))
  if (any(!blank & x != trimws(x))) {
    .verification_signal_error(paste0("`verified$", column, "` contains leading or trailing whitespace."))
  }
  x[blank] <- NA_character_
  x
}

.verification_normalize_positive_integer <- function(x, column, nullable = TRUE) {
  if (is.factor(x)) {
    .verification_signal_error(paste0("`verified$", column, "` cannot use factor storage."))
  }
  if (!(is.character(x) || is.integer(x) || is.double(x) || is.logical(x))) {
    .verification_signal_error(paste0("`verified$", column, "` has an unsupported storage type."))
  }
  if (is.logical(x) && !all(is.na(x))) {
    .verification_signal_error(paste0("`verified$", column,
      "` may be logical only when every value is missing."))
  }
  if (is.double(x) && any(is.nan(x))) {
    .verification_signal_error(paste0("`verified$", column, "` cannot contain NaN."))
  }
  missing_value <- is.na(x)
  if (is.character(x)) {
    blank <- !missing_value & !nzchar(trimws(x))
    if (any(!missing_value & !blank & x != trimws(x))) {
      .verification_signal_error(paste0("`verified$", column, "` contains leading or trailing whitespace."))
    }
    missing_value <- missing_value | blank
    if (any(!(missing_value | grepl("^[1-9][0-9]*$", x)))) {
      .verification_signal_error(paste0("`verified$", column,
        "` must contain positive integer digit strings."))
    }
  }
  numeric_value <- suppressWarnings(as.numeric(x))
  invalid <- !missing_value & (is.na(numeric_value) | !is.finite(numeric_value) |
    numeric_value < 1 | numeric_value != floor(numeric_value) |
    numeric_value > .Machine$integer.max)
  if (any(invalid) || (!isTRUE(nullable) && any(missing_value))) {
    .verification_signal_error(paste0("`verified$", column, "` must contain ",
      if (isTRUE(nullable)) "missing values or " else "", "positive whole numbers."))
  }
  out <- as.integer(numeric_value)
  out[missing_value] <- NA_integer_
  out
}

.verification_normalize_timestamp <- function(x) {
  if (inherits(x, "POSIXct")) {
    seconds <- as.numeric(x)
    if (any(!is.finite(seconds))) {
      .verification_signal_error("`verified$ts` cannot contain missing timestamps; finite timestamps are required.")
    }
    return(as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC"))
  }
  if (!is.character(x)) .verification_signal_error("`verified$ts` must be character or POSIXct.")
  if (any(is.na(x) | !nzchar(x) | x != trimws(x))) {
    .verification_signal_error("`verified$ts` must contain nonblank, unpadded timestamps.")
  }

  normalized <- sub("Z$", "+0000", x)
  normalized <- sub(
    "([+-][0-9]{2}):([0-9]{2})$",
    "\\1\\2",
    normalized,
    perl = TRUE
  )
  with_offset <- grepl("[+-][0-9]{4}$", normalized)
  shape_without_offset <-
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?$"
  shape_with_offset <- paste0(
    substr(shape_without_offset, 1L, nchar(shape_without_offset) - 1L),
    "[+-][0-9]{4}$"
  )
  valid_shape <- ifelse(
    with_offset,
    grepl(shape_with_offset, normalized, perl = TRUE),
    grepl(shape_without_offset, normalized, perl = TRUE)
  )
  if (any(!valid_shape)) {
    .verification_signal_error("`verified$ts` contains an unparseable timestamp.")
  }

  substr(normalized, 11L, 11L) <- " "
  seconds <- rep(NA_real_, length(normalized))
  for (offset in c(FALSE, TRUE)) {
    index <- which(with_offset == offset)
    if (!length(index)) next
    parsed <- suppressWarnings(tryCatch(
      strptime(
        normalized[index],
        format = if (offset) {
          "%Y-%m-%d %H:%M:%OS%z"
        } else {
          "%Y-%m-%d %H:%M:%OS"
        },
        tz = "UTC"
      ),
      error = function(e) rep(as.POSIXct(NA), length(index))
    ))
    seconds[index] <- as.numeric(as.POSIXct(parsed, tz = "UTC"))
  }
  if (any(!is.finite(seconds))) {
    .verification_signal_error("`verified$ts` contains an unparseable timestamp.")
  }
  as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC")
}

.verification_map_events <- function(event_id, snapshot) {
  longitudinal <- isTRUE(snapshot$project$longitudinal)
  normalized_id <- .verification_normalize_positive_integer(event_id, "event_id", nullable = !longitudinal)
  if (!longitudinal) {
    if (length(unique(normalized_id[!is.na(normalized_id)])) > 1L) {
      .verification_signal_error("A classic project's internal event ID must be consistent.")
    }
    return(list(
      event_id = rep(NA_character_, length(normalized_id)),
      redcap_event_name = rep(NA_character_, length(normalized_id))
    ))
  }
  events <- snapshot$events
  if (!all(c("event_id", "redcap_event_name") %in% names(events))) {
    .verification_signal_error("`rcon` does not provide event IDs and raw event names required by `verified`.")
  }
  source_id <- as.character(events$event_id)
  if (anyDuplicated(source_id)) .verification_signal_error("`rcon` maps at least one event ID more than once.")
  index <- match(as.character(normalized_id), source_id)
  list(event_id = as.character(normalized_id),
       redcap_event_name = as.character(events$redcap_event_name[index]))
}

.verification_build_context_prototype <- function() {
  tibble::tibble(
    record_id = character(),
    redcap_event_name = character(),
    repeat_instrument = character(),
    repeat_instance = integer(),
    field_name = character()
  )
}

.verification_build_group_id <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  row_count <- nrow(x)
  if (!row_count) return(integer())
  ordering <- do.call(
    order,
    c(unname(x), list(na.last = TRUE, method = "radix"))
  )
  if (row_count == 1L) return(1L)

  same_as_previous <- rep(TRUE, row_count - 1L)
  for (column in x) {
    sorted <- column[ordering]
    previous <- sorted[-row_count]
    following <- sorted[-1L]
    equal <- (is.na(previous) & is.na(following)) |
      (!is.na(previous) & !is.na(following) & previous == following)
    same_as_previous <- same_as_previous & equal
  }
  sorted_id <- cumsum(c(TRUE, !same_as_previous))
  group_id <- integer(row_count)
  group_id[ordering] <- sorted_id
  group_id
}

.verification_prepare_contexts <- function(verified, verified_user, snapshot, plan) {
  .verification_validate_input_pair(verified, verified_user)
  if (is.null(verified)) {
    return(list(
      contexts = .verification_build_context_prototype(),
      audit = .verification_build_audit()
    ))
  }
  required <- .verification_list_columns()
  required_counts <- vapply(
    required,
    function(column) sum(names(verified) == column, na.rm = TRUE),
    integer(1)
  )
  invalid_required <- names(required_counts)[required_counts != 1L]
  if (length(invalid_required)) {
    .verification_signal_error(paste0(
      "`verified` must contain each required column exactly once; invalid column(s): ",
      paste(invalid_required, collapse = ", "),
      "."
    ))
  }
  rows <- verified[, required, drop = FALSE]
  input_rows <- nrow(rows)
  if (!input_rows) {
    return(list(
      contexts = .verification_build_context_prototype(),
      audit = .verification_build_audit(
        enabled = TRUE,
        verified_user = verified_user
      )
    ))
  }

  rows$project_id <- .verification_normalize_identifier(rows$project_id, "project_id", whole_numeric = TRUE)
  rows$record <- .verification_normalize_identifier(rows$record, "record", allow_factor = TRUE)
  rows$field_name <- .verification_normalize_nonblank_character(rows$field_name, "field_name")
  project_id <- as.character(snapshot$project$project_id)
  if (length(project_id) != 1L || is.na(project_id) || !nzchar(project_id)) {
    .verification_signal_error("`rcon` does not provide one usable project ID.")
  }
  if (any(rows$project_id != project_id)) {
    .verification_signal_error("`verified$project_id` does not match the project represented by the plan.")
  }
  # These audit counts describe the supplied table, including out-of-plan rows.
  user_count <- if (is.character(rows$username)) {
    sum(rows$username == verified_user, na.rm = TRUE)
  } else 0L
  empty <- list(
    contexts = .verification_build_context_prototype(),
    audit = .verification_build_audit(
      enabled = TRUE, verified_user = verified_user,
      input_rows = input_rows, user_rows = user_count
    )
  )
  meta <- snapshot$metadata
  meta_index <- match(rows$field_name, as.character(meta$field_name))
  field_instrument <- as.character(meta$form_name[meta_index])
  targets <- plan$assessible_targets
  candidate <- rows$record %in% targets$record_id &
    !is.na(meta_index) & field_instrument %in% targets$instrument
  rows <- rows[candidate, , drop = FALSE]
  field_instrument <- field_instrument[candidate]
  if (!nrow(rows)) return(empty)

  rows$repeat_instrument <- .verification_normalize_nullable_character(rows$repeat_instrument, "repeat_instrument")
  rows$instance <- .verification_normalize_positive_integer(rows$instance, "instance", nullable = TRUE)
  event <- .verification_map_events(rows$event_id, snapshot)
  rows$event_id <- event$event_id
  rows$redcap_event_name <- event$redcap_event_name
  target_context <- tibble::tibble(
    record_id = targets$record_id,
    redcap_event_name = targets$redcap_event_name,
    repeat_instrument = targets$repeat_instrument,
    repeat_instance = targets$repeat_instance,
    instrument = targets$instrument
  )
  row_target_context <- tibble::tibble(
    record_id = rows$record,
    redcap_event_name = rows$redcap_event_name,
    repeat_instrument = rows$repeat_instrument,
    repeat_instance = rows$instance,
    instrument = field_instrument
  )
  target_count <- nrow(target_context)
  # A native instance of 1 is a placeholder only for a nonrepeating target.
  context_columns <- setdiff(names(target_context), "repeat_instance")
  context_groups <- .verification_build_group_id(dplyr::bind_rows(
    target_context[, context_columns], row_target_context[, context_columns]
  ))
  nonrepeat_groups <- context_groups[seq_len(target_count)][is.na(targets$repeat_instance)]
  row_groups <- context_groups[target_count + seq_len(nrow(rows))]
  placeholder <- row_groups %in% nonrepeat_groups & rows$instance %in% 1L
  rows$instance[placeholder] <- NA_integer_
  row_target_context$repeat_instance <- rows$instance
  target_groups <- .verification_build_group_id(dplyr::bind_rows(
    target_context,
    row_target_context
  ))
  plan_groups <- target_groups[seq_len(target_count)]
  verified_groups <- target_groups[target_count + seq_len(nrow(row_target_context))]
  rows <- rows[verified_groups %in% plan_groups, , drop = FALSE]
  if (!nrow(rows)) return(empty)

  # Issue-only and anonymous entries cannot identify reviewer evidence.
  rows$username <- .verification_normalize_nullable_character(rows$username, "username")
  rows <- rows[!is.na(rows$username), , drop = FALSE]
  if (!nrow(rows)) return(empty)
  rows$ts <- .verification_normalize_timestamp(rows$ts)
  rows$current_query_status <- .verification_normalize_nullable_character(rows$current_query_status, "current_query_status")
  rows$.field_group <- .verification_build_group_id(tibble::tibble(
    record_id = rows$record,
    redcap_event_name = rows$redcap_event_name,
    repeat_instrument = rows$repeat_instrument,
    repeat_instance = rows$instance,
    field_name = rows$field_name
  ))
  user_rows <- rows[rows$username == verified_user, , drop = FALSE]
  if (!nrow(user_rows)) return(empty)

  user_seconds <- as.numeric(user_rows$ts)
  ordering <- order(user_rows$.field_group, -user_seconds, method = "radix")
  ordered <- user_rows[ordering, , drop = FALSE]
  ordered_seconds <- user_seconds[ordering]
  first_group <- !duplicated(ordered$.field_group)
  first_position <- which(first_group)
  latest_seconds <- ordered_seconds[
    first_position[match(ordered$.field_group, ordered$.field_group[first_group])]
  ]
  latest <- ordered[ordered_seconds == latest_seconds, , drop = FALSE]

  comparable <- latest[, required, drop = FALSE]
  comparable$ts <- as.numeric(comparable$ts)
  latest <- latest[!duplicated(comparable), , drop = FALSE]
  if (anyDuplicated(latest$.field_group)) {
    .verification_signal_error("Conflicting verification rows share the latest timestamp for one field context.")
  }
  verified_latest <- latest$current_query_status %in% "VERIFIED"
  verified_contexts <- tibble::tibble(
    record_id = latest$record[verified_latest],
    redcap_event_name = latest$redcap_event_name[verified_latest],
    repeat_instrument = latest$repeat_instrument[verified_latest],
    repeat_instance = as.integer(latest$instance[verified_latest]),
    field_name = latest$field_name[verified_latest]
  )
  list(
    contexts = verified_contexts,
    audit = .verification_build_audit(
      enabled = TRUE,
      verified_user = verified_user,
      input_rows = input_rows,
      user_rows = user_count,
      latest_user_rows = nrow(latest),
      verified_rows = sum(verified_latest)
    )
  )
}


.verification_match_fields <- function(field_rows, targets, contexts) {
  matched <- rep.int(FALSE, nrow(field_rows))
  candidate <- which(field_rows$raw_disposition == "failed")
  if (!length(candidate) || !nrow(contexts)) return(matched)

  target_index <- field_rows$.target_row[candidate]
  candidate_context <- data.frame(
    record_id = targets$record_id[target_index],
    redcap_event_name = targets$redcap_event_name[target_index],
    repeat_instrument = targets$repeat_instrument[target_index],
    repeat_instance = targets$repeat_instance[target_index],
    field_name = field_rows$field_name[candidate],
    stringsAsFactors = FALSE
  )
  matched[candidate] <- !is.na(.assessible_target_match_record_rows(
    candidate_context,
    contexts,
    names(candidate_context)
  ))
  matched
}
