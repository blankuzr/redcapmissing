# REDCap record-export identity and physical-row contracts.

.record_list_system_fields <- function() {
  list(
    event_col = "redcap_event_name",
    repeat_instrument_col = "redcap_repeat_instrument",
    repeat_instance_col = "redcap_repeat_instance"
  )
}

.record_resolve_repeat_mode <- function(repeat_instrument, instance) {
  ifelse(
    !is.na(repeat_instrument),
    "repeating_instrument",
    ifelse(!is.na(instance), "repeating_event", "no_repeat")
  )
}

.record_detect_duplicate_rows <- function(data, columns) {
  anyDuplicated(as.data.frame(data[, columns, drop = FALSE])) > 0L
}

.record_validate_physical_contexts <- function(data, snapshot) {
  if (!nrow(data)) return(invisible(data))
  candidate <- tibble::tibble(
    redcap_event_name = data$redcap_event_name,
    repeat_mode = .record_resolve_repeat_mode(
      data$redcap_repeat_instrument,
      data$redcap_repeat_instance
    ),
    context_instrument = ifelse(
      is.na(data$redcap_repeat_instrument),
      NA_character_,
      data$redcap_repeat_instrument
    ),
    .input_row = seq_len(nrow(data))
  )
  allowed <- snapshot$physical_contexts
  allowed$.allowed <- TRUE
  matched <- merge(
    as.data.frame(candidate),
    as.data.frame(allowed),
    by = c("redcap_event_name", "repeat_mode", "context_instrument"),
    all.x = TRUE,
    sort = FALSE
  )
  invalid <- matched$.input_row[is.na(matched$.allowed)]
  if (length(invalid)) {
    .condition_signal_error(
      paste0(
        "Data row ",
        min(invalid),
        " has a combination of `redcap_event_name`, ",
        "`redcap_repeat_instrument`, and `redcap_repeat_instance` ",
        "that `rcon` does not allow."
      ),
      "schema"
    )
  }
  invisible(data)
}

.record_normalize_export <- function(
  data,
  snapshot,
  require_nonempty = FALSE,
  response_columns = NULL
) {
  if (!is.data.frame(data)) .condition_signal_error("`data` must be a data frame.", "argument")
  if (is.null(names(data)) || anyNA(names(data)) ||
      any(names(data) == "") || anyDuplicated(names(data))) {
    .condition_signal_error("`data` must have unique, nonblank column names.", "schema")
  }
  atomic_columns <- vapply(
    data,
    function(column) is.atomic(column) && is.null(dim(column)),
    logical(1)
  )
  if (any(!atomic_columns)) {
    .condition_signal_error(
      paste0(
        "`data` columns must use ordinary atomic vector storage; invalid column(s): ",
        paste(names(data)[!atomic_columns], collapse = ", "),
        "."
      ),
      "schema"
    )
  }
  if (!is.logical(require_nonempty) || length(require_nonempty) != 1 || is.na(require_nonempty)) {
    .condition_signal_error("`require_nonempty` must be TRUE or FALSE.", "argument")
  }
  if (isTRUE(require_nonempty) && !nrow(data)) .condition_signal_error("`data` must contain at least one row.", "schema")
  if (is.null(response_columns)) response_columns <- character()
  if (!is.character(response_columns) || anyNA(response_columns)) {
    .condition_signal_error("`response_columns` must be a character vector.", "argument")
  }
  .schema_require_columns(data, c(snapshot$project$record_id_field, response_columns), "data")
  repeat_columns <- c("redcap_repeat_instrument", "redcap_repeat_instance")
  repeat_present <- repeat_columns %in% names(data)
  if ((nrow(snapshot$repeat_configuration) > 0 && !all(repeat_present)) ||
      (any(repeat_present) && !all(repeat_present))) {
    .condition_signal_error(
      "`data` must provide `redcap_repeat_instrument` and `redcap_repeat_instance` together.",
      "schema"
    )
  }
  record_id <- .schema_normalize_required_id(
    data[[snapshot$project$record_id_field]],
    paste0("data$", snapshot$project$record_id_field)
  )
  if (isTRUE(snapshot$project$longitudinal)) {
    .schema_require_columns(data, "redcap_event_name", "data")
    event <- .schema_normalize_nullable_character(data$redcap_event_name, "data$redcap_event_name")
    if (anyNA(event)) .condition_signal_error("Longitudinal event names cannot be missing or blank.", "schema")
  } else if ("redcap_event_name" %in% names(data)) {
    event <- .schema_normalize_nullable_character(data$redcap_event_name, "data$redcap_event_name")
    if (any(!is.na(event))) .condition_signal_error("Classic project event values must be missing or blank.", "schema")
  } else {
    event <- rep(NA_character_, nrow(data))
  }
  repeat_instrument <- if (all(repeat_present)) {
    .schema_normalize_nullable_character(data$redcap_repeat_instrument, "data$redcap_repeat_instrument")
  } else rep(NA_character_, nrow(data))
  repeat_instance <- if (all(repeat_present)) {
    .schema_normalize_repeat_instance(data$redcap_repeat_instance, "data$redcap_repeat_instance")
  } else rep(NA_integer_, nrow(data))
  if (any(!is.na(repeat_instrument) & is.na(repeat_instance))) {
    .condition_signal_error("A repeating instrument requires a positive repeat instance.", "schema")
  }
  normalized <- tibble::as_tibble(data)
  normalized$.rcm_record_id <- record_id
  normalized$redcap_event_name <- event
  normalized$redcap_repeat_instrument <- repeat_instrument
  normalized$redcap_repeat_instance <- repeat_instance
  physical_key <- c(
    ".rcm_record_id", "redcap_event_name",
    "redcap_repeat_instrument", "redcap_repeat_instance"
  )
  if (.record_detect_duplicate_rows(normalized, physical_key)) {
    .condition_signal_error(
      "`data` contains duplicate normalized physical row keys.",
      "schema"
    )
  }
  .record_validate_physical_contexts(normalized, snapshot)
  normalized
}

.record_map_arms <- function(data, snapshot) {
  records <- unique(data$.rcm_record_id)
  if (!isTRUE(snapshot$project$longitudinal)) {
    return(tibble::tibble(record_id = records, arm_num = rep(NA_character_, length(records))))
  }
  if (!length(records)) {
    return(tibble::tibble(record_id = character(), arm_num = character()))
  }
  arms <- snapshot$event_arms$arm_num[
    match(data$redcap_event_name, snapshot$event_arms$redcap_event_name)
  ]
  record_arm_pairs <- unique(data.frame(
    record_id = data$.rcm_record_id,
    arm_num = arms,
    stringsAsFactors = FALSE
  ))
  duplicated_records <- unique(
    record_arm_pairs$record_id[duplicated(record_arm_pairs$record_id)]
  )
  if (length(duplicated_records)) {
    first_conflict <- records[records %in% duplicated_records][[1L]]
    .condition_signal_error(
      paste0("Record `", first_conflict, "` is observed in more than one arm."),
      "schema"
    )
  }
  tibble::tibble(
    record_id = records,
    arm_num = record_arm_pairs$arm_num[
      match(records, record_arm_pairs$record_id)
    ]
  )
}
