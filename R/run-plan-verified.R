## Verification evidence for plan execution --------------------------------

.rcm_verified_columns <- function() c(
  "project_id", "record", "event_id", "field_name", "repeat_instrument",
  "instance", "ts", "current_query_status", "username"
)

.rcm_verification_audit <- function(enabled = FALSE, verified_user = NA_character_,
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

.rcm_verified_abort <- function(message) {
  .rcm_plan_abort(message, subclass = "verification")
}

.rcm_check_verified_pair <- function(verified, verified_user) {
  supplied_verified <- !is.null(verified)
  supplied_user <- !is.null(verified_user)
  if (xor(supplied_verified, supplied_user)) {
    .rcm_verified_abort("`verified` and `verified_user` must be supplied together or both be NULL.")
  }
  if (!supplied_verified) return(invisible(NULL))
  if (!is.data.frame(verified)) {
    .rcm_verified_abort("`verified` must be a data frame or tibble.")
  }
  if (!is.character(verified_user) || length(verified_user) != 1L ||
      is.na(verified_user) || !nzchar(verified_user) ||
      !identical(verified_user, trimws(verified_user))) {
    .rcm_verified_abort("`verified_user` must be one nonblank, unpadded character value.")
  }
  invisible(NULL)
}

.rcm_verified_nonblank_character <- function(x, column) {
  if (!is.character(x)) {
    .rcm_verified_abort(paste0("`verified$", column, "` must be a character column."))
  }
  if (any(is.na(x) | !nzchar(x) | x != trimws(x))) {
    .rcm_verified_abort(paste0("`verified$", column, "` must contain nonblank, unpadded values."))
  }
  x
}

.rcm_verified_identifier <- function(x, column, allow_factor = FALSE,
                                     whole_numeric = FALSE) {
  if (is.factor(x) && isTRUE(allow_factor)) x <- as.character(x)
  if (!(is.character(x) || is.integer(x) || is.double(x))) {
    .rcm_verified_abort(paste0("`verified$", column, "` has an unsupported storage type."))
  }
  if (is.numeric(x)) {
    invalid <- is.na(x) | !is.finite(x)
    if (isTRUE(whole_numeric)) invalid <- invalid | x != floor(x)
    if (any(invalid)) {
      .rcm_verified_abort(paste0("`verified$", column, "` contains an invalid numeric value."))
    }
    x <- .rcm_numeric_character(x)
  }
  if (any(is.na(x) | !nzchar(x) | x != trimws(x))) {
    .rcm_verified_abort(paste0("`verified$", column, "` must contain nonblank, unpadded values."))
  }
  as.character(x)
}

.rcm_verified_nullable_character <- function(x, column) {
  if (is.factor(x)) {
    .rcm_verified_abort(paste0("`verified$", column, "` cannot use factor storage."))
  }
  if (is.logical(x) || is.integer(x) || is.double(x)) {
    if ((is.double(x) && any(is.nan(x))) || !all(is.na(x))) {
      .rcm_verified_abort(paste0("`verified$", column,
        "` may use a non-character type only when every value is a typed NA."))
    }
    return(rep(NA_character_, length(x)))
  }
  if (!is.character(x)) {
    .rcm_verified_abort(paste0("`verified$", column, "` has an unsupported storage type."))
  }
  blank <- is.na(x) | !nzchar(trimws(x))
  if (any(!blank & x != trimws(x))) {
    .rcm_verified_abort(paste0("`verified$", column, "` contains leading or trailing whitespace."))
  }
  x[blank] <- NA_character_
  x
}

.rcm_verified_positive_integer <- function(x, column, nullable = TRUE) {
  if (is.factor(x)) {
    .rcm_verified_abort(paste0("`verified$", column, "` cannot use factor storage."))
  }
  if (!(is.character(x) || is.integer(x) || is.double(x) || is.logical(x))) {
    .rcm_verified_abort(paste0("`verified$", column, "` has an unsupported storage type."))
  }
  if (is.logical(x) && !all(is.na(x))) {
    .rcm_verified_abort(paste0("`verified$", column,
      "` may be logical only when every value is missing."))
  }
  if (is.double(x) && any(is.nan(x))) {
    .rcm_verified_abort(paste0("`verified$", column, "` cannot contain NaN."))
  }
  missing_value <- is.na(x)
  if (is.character(x)) {
    blank <- !missing_value & !nzchar(trimws(x))
    if (any(!missing_value & !blank & x != trimws(x))) {
      .rcm_verified_abort(paste0("`verified$", column, "` contains leading or trailing whitespace."))
    }
    missing_value <- missing_value | blank
    if (any(!(missing_value | grepl("^[1-9][0-9]*$", x)))) {
      .rcm_verified_abort(paste0("`verified$", column,
        "` must contain canonical positive integers."))
    }
  }
  numeric_value <- suppressWarnings(as.numeric(x))
  invalid <- !missing_value & (is.na(numeric_value) | !is.finite(numeric_value) |
    numeric_value < 1 | numeric_value != floor(numeric_value) |
    numeric_value > .Machine$integer.max)
  if (any(invalid) || (!isTRUE(nullable) && any(missing_value))) {
    .rcm_verified_abort(paste0("`verified$", column, "` must contain ",
      if (isTRUE(nullable)) "missing values or " else "", "positive whole numbers."))
  }
  out <- as.integer(numeric_value)
  out[missing_value] <- NA_integer_
  out
}

.rcm_verified_timestamp <- function(x) {
  if (inherits(x, "POSIXct")) {
    seconds <- as.numeric(x)
    if (any(!is.finite(seconds))) {
      .rcm_verified_abort("`verified$ts` cannot contain missing or non-finite timestamps.")
    }
    return(as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC"))
  }
  if (!is.character(x)) .rcm_verified_abort("`verified$ts` must be character or POSIXct.")
  if (any(is.na(x) | !nzchar(x) | x != trimws(x))) {
    .rcm_verified_abort("`verified$ts` must contain nonblank, unpadded timestamps.")
  }
  parse_one <- function(value) {
    normalized <- sub("Z$", "+0000", value)
    normalized <- sub(
      "([+-][0-9]{2}):([0-9]{2})$",
      "\\1\\2",
      normalized,
      perl = TRUE
    )
    with_offset <- grepl("[+-][0-9]{4}$", normalized)
    shape <- if (with_offset) {
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?[+-][0-9]{4}$"
    } else {
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T][0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?$"
    }
    if (!grepl(shape, normalized, perl = TRUE)) return(NA_real_)
    separator <- substr(normalized, 11L, 11L)
    format <- paste0(
      "%Y-%m-%d",
      separator,
      "%H:%M:%OS",
      if (with_offset) "%z" else ""
    )
    parsed <- suppressWarnings(tryCatch(
      strptime(normalized, format = format, tz = "UTC"),
      error = function(e) NA
    ))
    if (length(parsed) != 1L || is.na(parsed)) return(NA_real_)
    as.numeric(as.POSIXct(parsed, tz = "UTC"))
  }
  seconds <- vapply(x, parse_one, numeric(1))
  if (any(!is.finite(seconds))) {
    .rcm_verified_abort("`verified$ts` contains an unparseable timestamp.")
  }
  as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC")
}

.rcm_verified_event_map <- function(event_id, snapshot) {
  longitudinal <- isTRUE(snapshot$project$longitudinal)
  normalized_id <- .rcm_verified_positive_integer(event_id, "event_id", nullable = !longitudinal)
  if (!longitudinal) {
    return(list(event_id = ifelse(is.na(normalized_id), NA_character_, as.character(normalized_id)),
                redcap_event_name = rep(NA_character_, length(normalized_id))))
  }
  events <- snapshot$events
  if (!all(c("event_id", "redcap_event_name") %in% names(events))) {
    .rcm_verified_abort("`rcon` does not provide event IDs and raw event names required by `verified`.")
  }
  source_id <- as.character(events$event_id)
  if (anyDuplicated(source_id)) .rcm_verified_abort("`rcon` maps at least one event ID more than once.")
  index <- match(as.character(normalized_id), source_id)
  if (anyNA(index)) .rcm_verified_abort("`verified$event_id` contains an unknown event ID.")
  list(event_id = as.character(normalized_id),
       redcap_event_name = as.character(events$redcap_event_name[index]))
}

.rcm_field_context_key <- function(record_id, redcap_event_name,
                                   repeat_instrument, repeat_instance, field_name) {
  encode <- function(x) { out <- as.character(x); out[is.na(x)] <- "<NA>"; out }
  paste(encode(record_id), encode(redcap_event_name), encode(repeat_instrument),
        encode(repeat_instance), encode(field_name), sep = "\r")
}

.rcm_prepare_verified <- function(verified, verified_user, snapshot, plan) {
  .rcm_check_verified_pair(verified, verified_user)
  if (is.null(verified)) return(list(keys = character(), audit = .rcm_verification_audit()))
  required <- .rcm_verified_columns()
  required_counts <- vapply(
    required,
    function(column) sum(names(verified) == column, na.rm = TRUE),
    integer(1)
  )
  invalid_required <- names(required_counts)[required_counts != 1L]
  if (length(invalid_required)) {
    .rcm_verified_abort(paste0(
      "`verified` must contain each required column exactly once; invalid column(s): ",
      paste(invalid_required, collapse = ", "),
      "."
    ))
  }
  rows <- verified[, required, drop = FALSE]
  input_rows <- nrow(rows)
  if (!input_rows) return(list(keys = character(), audit = .rcm_verification_audit(
    enabled = TRUE, verified_user = verified_user)))

  rows$project_id <- .rcm_verified_identifier(rows$project_id, "project_id", whole_numeric = TRUE)
  rows$record <- .rcm_verified_identifier(rows$record, "record", allow_factor = TRUE)
  rows$field_name <- .rcm_verified_nonblank_character(rows$field_name, "field_name")
  rows$current_query_status <- .rcm_verified_nonblank_character(rows$current_query_status, "current_query_status")
  rows$username <- .rcm_verified_nonblank_character(rows$username, "username")
  rows$repeat_instrument <- .rcm_verified_nullable_character(rows$repeat_instrument, "repeat_instrument")
  rows$instance <- .rcm_verified_positive_integer(rows$instance, "instance", nullable = TRUE)
  rows$ts <- .rcm_verified_timestamp(rows$ts)
  event <- .rcm_verified_event_map(rows$event_id, snapshot)
  rows$event_id <- event$event_id
  rows$redcap_event_name <- event$redcap_event_name

  project_id <- as.character(snapshot$project$project_id)
  if (length(project_id) != 1L || is.na(project_id) || !nzchar(project_id)) {
    .rcm_verified_abort("`rcon` does not provide one usable project ID.")
  }
  if (any(rows$project_id != project_id)) {
    .rcm_verified_abort("`verified$project_id` does not match the project represented by the plan.")
  }
  meta <- snapshot$metadata
  meta_index <- match(rows$field_name, as.character(meta$field_name))
  if (anyNA(meta_index)) .rcm_verified_abort("`verified$field_name` contains an unknown raw field name.")
  field_instrument <- as.character(meta$form_name[meta_index])
  targets <- plan$assessible_targets
  target_key <- .rcm_field_context_key(targets$record_id, targets$redcap_event_name,
    targets$repeat_instrument, targets$repeat_instance, targets$instrument)
  row_target_key <- .rcm_field_context_key(rows$record, rows$redcap_event_name,
    rows$repeat_instrument, rows$instance, field_instrument)
  if (any(!row_target_key %in% target_key)) {
    .rcm_verified_abort("`verified` contains a field context that is not an Assessible target.")
  }
  rows$.field_key <- .rcm_field_context_key(rows$record, rows$redcap_event_name,
    rows$repeat_instrument, rows$instance, rows$field_name)
  user_rows <- rows[rows$username == verified_user, , drop = FALSE]
  user_count <- nrow(user_rows)
  if (!user_count) return(list(keys = character(), audit = .rcm_verification_audit(
    enabled = TRUE, verified_user = verified_user, input_rows = input_rows)))

  latest_time <- stats::ave(as.numeric(user_rows$ts), user_rows$.field_key, FUN = max)
  latest <- user_rows[as.numeric(user_rows$ts) == latest_time, , drop = FALSE]
  split_latest <- split(seq_len(nrow(latest)), latest$.field_key)
  keep <- integer(length(split_latest)); i <- 0L
  for (indices in split_latest) {
    i <- i + 1L
    comparable <- latest[indices, required, drop = FALSE]
    comparable$ts <- as.numeric(comparable$ts)
    if (nrow(unique(comparable)) != 1L) {
      .rcm_verified_abort("Conflicting verification rows share the latest timestamp for one field context.")
    }
    keep[[i]] <- indices[[1L]]
  }
  latest <- latest[keep, , drop = FALSE]
  verified_latest <- latest$current_query_status == "VERIFIED"
  list(keys = unique(latest$.field_key[verified_latest]),
       audit = .rcm_verification_audit(enabled = TRUE, verified_user = verified_user,
         input_rows = input_rows, user_rows = user_count,
         latest_user_rows = nrow(latest), verified_rows = sum(verified_latest)))
}