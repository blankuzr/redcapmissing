## Internal helpers: verified REDCap data-quality exceptions ---------------

.miss_verified_columns <- function() {
  c(
    "status_id",
    "project_id",
    "record",
    "event_id",
    "field_name",
    "repeat_instrument",
    "instance",
    "query_status",
    "res_id",
    "ts",
    "current_query_status",
    "username"
  )
}

.miss_check_verified_arguments <- function(verified, verified_user) {
  supplied_verified <- !is.null(verified)
  supplied_user <- !is.null(verified_user)
  if (xor(supplied_verified, supplied_user)) {
    stop(
      "`verified` and `verified_user` must be supplied together.",
      call. = FALSE
    )
  }
  if (!supplied_verified) {
    return(invisible(NULL))
  }
  if (!is.data.frame(verified)) {
    stop("`verified` must be a data frame.", call. = FALSE)
  }
  if (
    !is.character(verified_user) ||
      length(verified_user) != 1L ||
      is.na(verified_user) ||
      !nzchar(trimws(verified_user))
  ) {
    stop(
      "`verified_user` must be one non-blank character value.",
      call. = FALSE
    )
  }

  invisible(NULL)
}

.miss_verification_diagnostics <- function(
  enabled = FALSE,
  verified_user = NA_character_,
  input_rows = 0L,
  user_rows = 0L,
  verified_rows = 0L,
  overrides_applied = 0L
) {
  list(
    enabled = isTRUE(enabled),
    verified_user = if (isTRUE(enabled)) {
      .miss_chr(verified_user)
    } else {
      NA_character_
    },
    input_rows = as.integer(input_rows),
    user_rows = as.integer(user_rows),
    verified_rows = as.integer(verified_rows),
    overrides_applied = as.integer(overrides_applied)
  )
}

.miss_prepare_verified <- function(
  verified,
  verified_user,
  meta,
  project_cache
) {
  if (is.null(verified)) {
    return(list(
      keys = character(),
      diagnostics = .miss_verification_diagnostics()
    ))
  }

  required <- .miss_verified_columns()
  missing_columns <- setdiff(required, names(verified))
  if (length(missing_columns) > 0L) {
    stop(
      "`verified` is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_data <- verified[, required, drop = FALSE]
  input_rows <- nrow(required_data)
  character_columns <- vapply(required_data, is.character, logical(1))
  nullable_columns <- c("repeat_instrument", "instance")
  nullable_is_all_missing <- vapply(
    required_data[nullable_columns],
    function(x) input_rows > 0L && all(is.na(x)),
    logical(1)
  )
  valid_columns <- character_columns
  valid_columns[nullable_columns] <-
    character_columns[nullable_columns] | nullable_is_all_missing
  logical_template <- nrow(required_data) == 0L &&
    all(vapply(required_data, is.logical, logical(1)))
  if (!all(valid_columns) && !logical_template) {
    invalid_columns <- names(valid_columns)[!valid_columns]
    stop(
      "`verified` column(s) must be character: ",
      paste(invalid_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  for (column in nullable_columns[nullable_is_all_missing]) {
    required_data[[column]] <- rep(NA_character_, input_rows)
  }

  if (input_rows == 0L) {
    warning(
      "No rows in `verified` match `verified_user` \"",
      verified_user,
      "\"; proceeding without verified exceptions.",
      call. = FALSE
    )
    return(list(
      keys = character(),
      diagnostics = .miss_verification_diagnostics(
        enabled = TRUE,
        verified_user = verified_user
      )
    ))
  }

  project_id <- .miss_get_project_information_value(
    project_cache$project_information,
    "project_id"
  )
  if (.miss_is_blank_scalar(project_id)) {
    stop(
      "`rcon$projectInformation()` must provide one non-blank `project_id` ",
      "when `verified` is used.",
      call. = FALSE
    )
  }
  invalid_project <- is.na(required_data$project_id) |
    required_data$project_id != project_id
  if (any(invalid_project)) {
    stop(
      "Every `verified$project_id` value must equal the REDCap project ID `",
      project_id,
      "`.",
      call. = FALSE
    )
  }

  .miss_check_verified_nonblank(required_data$status_id, "status_id")
  .miss_check_verified_nonblank(required_data$record, "record")
  .miss_check_verified_nonblank(required_data$field_name, "field_name")

  metadata_fields <- .miss_chr_vec(meta$field_name)
  field_counts <- table(metadata_fields, useNA = "no")
  supplied_fields <- unique(required_data$field_name)
  unknown_fields <- supplied_fields[!supplied_fields %in% names(field_counts)]
  if (length(unknown_fields) > 0L) {
    stop(
      "Unknown `verified$field_name` value(s): ",
      paste(unknown_fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  duplicate_fields <- supplied_fields[
    as.integer(field_counts[supplied_fields]) != 1L
  ]
  if (length(duplicate_fields) > 0L) {
    stop(
      "`rcon$metadata()` must contain exactly one row for each verified field; ",
      "duplicate field(s): ",
      paste(duplicate_fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  metadata_rows <- match(required_data$field_name, metadata_fields)
  field_forms <- .miss_chr_vec(meta$form_name[metadata_rows])
  if (any(.miss_is_blank_vec(field_forms))) {
    stop(
      "Every verified field must map to a non-blank metadata `form_name`.",
      call. = FALSE
    )
  }

  is_longitudinal <- .miss_project_is_longitudinal(
    .miss_get_project_information_value(
      project_cache$project_information,
      "is_longitudinal"
    )
  )
  if (is.na(is_longitudinal)) {
    stop(
      "`rcon$projectInformation()` must provide a valid `is_longitudinal` ",
      "value when `verified` is used.",
      call. = FALSE
    )
  }

  event_names <- if (isTRUE(is_longitudinal)) {
    .miss_verified_longitudinal_events(
      event_id = required_data$event_id,
      field_forms = field_forms,
      project_cache = project_cache
    )
  } else {
    .miss_verified_classic_events(required_data$event_id)
  }

  repeat_context <- .miss_normalize_verified_repeat_contexts(
    repeat_instrument = required_data$repeat_instrument,
    instance = required_data$instance,
    event_name = event_names,
    field_form = field_forms,
    repeat_structure = project_cache$repeat_instrument_event
  )

  issue_keys <- .miss_verified_key(
    record_id = required_data$record,
    event = event_names,
    repeat_instrument = repeat_context$repeat_instrument,
    repeat_instance = repeat_context$instance,
    field_name = required_data$field_name
  )
  latest_resolution <- .miss_verified_latest_resolution_rows(
    status_id = required_data$status_id,
    res_id = required_data$res_id,
    timestamp = required_data$ts,
    query_status = required_data$query_status,
    current_query_status = required_data$current_query_status,
    username = required_data$username,
    issue_key = issue_keys
  )

  user_match <- !is.na(required_data$username) &
    required_data$username == verified_user
  user_rows <- sum(user_match)
  if (user_rows == 0L) {
    warning(
      "No rows in `verified` match `verified_user` \"",
      verified_user,
      "\"; proceeding without verified exceptions.",
      call. = FALSE
    )
  }
  verified_match <- latest_resolution &
    user_match &
    !is.na(required_data$query_status) &
    required_data$query_status == "VERIFIED" &
    !is.na(required_data$current_query_status) &
    required_data$current_query_status == "VERIFIED"

  keys <- unique(issue_keys[verified_match])

  list(
    keys = keys,
    diagnostics = .miss_verification_diagnostics(
      enabled = TRUE,
      verified_user = verified_user,
      input_rows = input_rows,
      user_rows = user_rows,
      verified_rows = sum(verified_match)
    )
  )
}

.miss_verified_classic_events <- function(event_id) {
  supplied <- event_id[!is.na(event_id)]
  if (length(supplied) == 0L) {
    return(rep("", length(event_id)))
  }
  if (any(!nzchar(trimws(supplied))) ||
      any(!grepl("^[1-9][0-9]*$", supplied))) {
    stop(
      "`verified$event_id` must be missing or a canonical positive integer ",
      "string for a classic project.",
      call. = FALSE
    )
  }
  unique_ids <- unique(supplied)
  if (length(unique_ids) > 1L) {
    stop(
      "`verified$event_id` must contain at most one unique non-missing ",
      "internal event ID for a classic project.",
      call. = FALSE
    )
  }

  rep("", length(event_id))
}

.miss_verified_latest_resolution_rows <- function(
  status_id,
  res_id,
  timestamp,
  query_status,
  current_query_status,
  username,
  issue_key
) {
  canonical_status_id <- grepl("^[1-9][0-9]*$", status_id)
  canonical_status_id[is.na(canonical_status_id)] <- FALSE
  if (any(!canonical_status_id)) {
    stop(
      "`verified$status_id` must contain canonical positive integer strings.",
      call. = FALSE
    )
  }

  has_res_id <- !is.na(res_id)
  has_timestamp <- !is.na(timestamp)
  has_resolution_value <- has_res_id |
    has_timestamp |
    !is.na(current_query_status) |
    !is.na(username)
  complete_resolution <- has_res_id & has_timestamp
  if (any(has_resolution_value & !complete_resolution)) {
    stop(
      "Every supplied resolution must provide both `verified$res_id` and ",
      "`verified$ts`; rows without a resolution must leave all resolution ",
      "fields missing.",
      call. = FALSE
    )
  }

  resolution_rows <- which(complete_resolution)
  if (length(resolution_rows) > 0L) {
    canonical_res_id <- grepl("^[1-9][0-9]*$", res_id[resolution_rows])
    canonical_res_id[is.na(canonical_res_id)] <- FALSE
    if (any(!canonical_res_id)) {
      stop(
        "`verified$res_id` must contain canonical positive integer strings ",
        "for supplied resolutions.",
        call. = FALSE
      )
    }

    canonical_timestamp <- grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$",
      timestamp[resolution_rows]
    )
    parsed_timestamp <- as.POSIXct(
      timestamp[resolution_rows],
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
    canonical_timestamp <- canonical_timestamp &
      !is.na(parsed_timestamp) &
      format(parsed_timestamp, "%Y-%m-%d %H:%M:%S", tz = "UTC") ==
        timestamp[resolution_rows]
    canonical_timestamp[is.na(canonical_timestamp)] <- FALSE
    if (any(!canonical_timestamp)) {
      stop(
        "`verified$ts` must use valid REDCap `YYYY-MM-DD HH:MM:SS` ",
        "timestamp strings for supplied resolutions.",
        call. = FALSE
      )
    }
  }

  resolution_id_rows <- split(
    resolution_rows,
    res_id[resolution_rows],
    drop = TRUE
  )
  inconsistent_resolution <- vapply(
    resolution_id_rows,
    function(rows) {
      any(vapply(
        list(status_id, timestamp, current_query_status, username),
        function(x) length(unique(x[rows])) > 1L,
        logical(1)
      ))
    },
    logical(1)
  )
  if (any(inconsistent_resolution)) {
    stop(
      "Each `verified$res_id` must identify one consistent resolution row.",
      call. = FALSE
    )
  }

  issue_rows <- split(seq_along(status_id), status_id, drop = TRUE)
  inconsistent_issue <- vapply(
    issue_rows,
    function(rows) {
      length(unique(issue_key[rows])) > 1L ||
        length(unique(query_status[rows])) > 1L
    },
    logical(1)
  )
  if (any(inconsistent_issue)) {
    stop(
      "Each `verified$status_id` must map to one field context and one live ",
      "`query_status`.",
      call. = FALSE
    )
  }

  latest <- rep(FALSE, length(status_id))
  for (rows in issue_rows) {
    candidates <- rows[complete_resolution[rows]]
    if (length(candidates) == 0L) {
      next
    }
    latest_order <- order(
      timestamp[candidates],
      nchar(res_id[candidates]),
      res_id[candidates],
      decreasing = TRUE
    )
    latest[candidates[latest_order[[1L]]]] <- TRUE
  }

  latest
}

.miss_check_verified_nonblank <- function(x, column) {
  if (any(is.na(x) | !nzchar(trimws(x)))) {
    stop(
      "`verified$",
      column,
      "` must contain only non-blank, non-missing values.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

.miss_verified_longitudinal_events <- function(
  event_id,
  field_forms,
  project_cache
) {
  .miss_check_verified_nonblank(event_id, "event_id")

  events <- project_cache$events
  if (nrow(events) == 0L) {
    stop(
      "`rcon$events()` must provide event mappings when `verified` is used ",
      "with a longitudinal project.",
      call. = FALSE
    )
  }
  event_ids <- .miss_chr_vec(events$event_id)
  event_counts <- table(event_ids[!.miss_is_blank_vec(event_ids)])
  supplied_ids <- unique(event_id)
  unknown_ids <- supplied_ids[!supplied_ids %in% names(event_counts)]
  if (length(unknown_ids) > 0L) {
    stop(
      "Unknown `verified$event_id` value(s): ",
      paste(unknown_ids, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  ambiguous_ids <- supplied_ids[as.integer(event_counts[supplied_ids]) != 1L]
  if (length(ambiguous_ids) > 0L) {
    stop(
      "`rcon$events()` must map each verified `event_id` exactly once; ",
      "ambiguous ID(s): ",
      paste(ambiguous_ids, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  event_rows <- match(event_id, event_ids)
  event_names <- .miss_chr_vec(events$unique_event_name[event_rows])
  if (any(.miss_is_blank_vec(event_names))) {
    stop(
      "Every verified `event_id` must map to a non-blank ",
      "`unique_event_name`.",
      call. = FALSE
    )
  }

  mapping <- project_cache$mapping
  offered_keys <- paste(
    .miss_key_part(mapping$unique_event_name),
    .miss_key_part(mapping$form),
    sep = "\r"
  )
  verified_keys <- paste(
    .miss_key_part(event_names),
    .miss_key_part(field_forms),
    sep = "\r"
  )
  if (any(!verified_keys %in% offered_keys)) {
    stop(
      "A verified field's form is not offered at its mapped event.",
      call. = FALSE
    )
  }

  event_names
}

.miss_normalize_verified_repeat_contexts <- function(
  repeat_instrument,
  instance,
  event_name,
  field_form,
  repeat_structure
) {
  blank_repeat <- !is.na(repeat_instrument) &
    !nzchar(trimws(repeat_instrument))
  if (any(blank_repeat)) {
    stop(
      "Blank `verified$repeat_instrument` values are invalid; use a missing ",
      "value when not applicable.",
      call. = FALSE
    )
  }

  has_repeat <- !is.na(repeat_instrument)
  repeat_event <- .miss_key_part(repeat_structure$event_name)
  repeat_form <- .miss_chr_vec(repeat_structure$form_name)
  row_event <- .miss_key_part(event_name)
  row_form <- .miss_chr_vec(field_form)
  form_repeat <- vapply(
    seq_along(row_event),
    function(i) {
      any(
        repeat_event == row_event[[i]] &
          !.miss_is_blank_vec(repeat_form) &
          repeat_form == row_form[[i]]
      )
    },
    logical(1)
  )
  event_repeat <- vapply(
    seq_along(row_event),
    function(i) {
      any(
        repeat_event == row_event[[i]] &
          .miss_is_blank_vec(repeat_form)
      )
    },
    logical(1)
  )

  repeating_instrument_context <- has_repeat &
    repeat_instrument == row_form & form_repeat & !event_repeat
  repeating_event_context <- !has_repeat & event_repeat & !form_repeat
  regular_context <- !has_repeat & !form_repeat & !event_repeat
  valid_context <- repeating_instrument_context |
    repeating_event_context |
    regular_context
  valid_context[is.na(valid_context)] <- FALSE
  if (any(!valid_context)) {
    stop(
      "Invalid verified repeat context: use missing repeat values for regular ",
      "rows, the field's repeating form and a positive instance for repeating ",
      "instruments, or a missing repeat instrument and positive instance for ",
      "repeating events.",
      call. = FALSE
    )
  }

  requires_instance <- repeating_instrument_context | repeating_event_context
  valid_instance <- !is.na(instance) &
    nzchar(trimws(instance)) &
    grepl("^[1-9][0-9]*$", instance)
  valid_instance[is.na(valid_instance)] <- FALSE
  if (any(requires_instance & !valid_instance)) {
    stop(
      "`verified$instance` must be a canonical positive integer string for ",
      "repeating instruments and events.",
      call. = FALSE
    )
  }

  normalized_instance <- instance
  normalized_instance[regular_context] <- NA_character_
  list(
    repeat_instrument = repeat_instrument,
    instance = normalized_instance
  )
}

.miss_verified_key <- function(
  record_id,
  event,
  repeat_instrument,
  repeat_instance,
  field_name
) {
  paste(
    .miss_key_part(record_id),
    .miss_key_part(event),
    .miss_key_part(repeat_instrument),
    .miss_key_part(repeat_instance),
    .miss_key_part(field_name),
    sep = "\r"
  )
}

.miss_apply_verified_field_checks <- function(expected_result, verified_keys) {
  rows <- expected_result$rows
  if (nrow(rows) == 0L || length(verified_keys) == 0L) {
    return(list(
      expected_result = expected_result,
      overrides_applied = 0L
    ))
  }

  row_keys <- .miss_verified_key(
    record_id = rows$record_id,
    event = rows$redcap_event_name,
    repeat_instrument = rows$redcap_repeat_instrument,
    repeat_instance = rows$redcap_repeat_instance,
    field_name = rows$field_name
  )
  apply <- rows$validation_passed %in% FALSE & row_keys %in% verified_keys
  expected_result$rows$validation_passed[apply] <- TRUE
  expected_result$validation_passed[apply] <- TRUE

  list(
    expected_result = expected_result,
    overrides_applied = as.integer(sum(apply))
  )
}
