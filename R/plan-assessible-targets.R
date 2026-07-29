# Construction and ordering of plan assessible_targets.

.assessible_target_build_prototype <- function() {
  tibble::tibble(
    record_id = character(), instrument = character(),
    redcap_event_name = character(), repeat_instrument = character(),
    repeat_instance = integer(), target_source = character()
  )
}

.assessible_target_list_identity_columns <- function() {
  c("record_id", "instrument", "redcap_event_name", "repeat_instrument", "repeat_instance")
}

.assessible_target_build_observed <- function(data, snapshot, instruments) {
  allowable <- snapshot$allowable_crossings
  allowable <- allowable[
    allowable$instrument %in% instruments,
    c("instrument", "redcap_event_name", "repeat_mode"),
    drop = FALSE
  ]
  if (!nrow(data) || !nrow(allowable)) return(.assessible_target_build_prototype())
  contexts <- tibble::tibble(
    record_id = data$.rcm_record_id,
    redcap_event_name = data$redcap_event_name,
    context_instrument = data$redcap_repeat_instrument,
    repeat_instance = data$redcap_repeat_instance,
    repeat_mode = .record_resolve_repeat_mode(
      data$redcap_repeat_instrument,
      data$redcap_repeat_instance
    )
  )
  result <- list()
  rows_without_repeat_instrument <-
    contexts$repeat_mode != "repeating_instrument"
  crossings_without_repeat_instrument <-
    allowable$repeat_mode != "repeating_instrument"
  if (any(rows_without_repeat_instrument) &&
      any(crossings_without_repeat_instrument)) {
    joined <- merge(
      as.data.frame(contexts[
        rows_without_repeat_instrument,
        c("record_id", "redcap_event_name", "repeat_instance", "repeat_mode"),
        drop = FALSE
      ]),
      as.data.frame(allowable[
        crossings_without_repeat_instrument,
        c("instrument", "redcap_event_name", "repeat_mode"),
        drop = FALSE
      ]),
      by = c("redcap_event_name", "repeat_mode"),
      sort = FALSE
    )
    if (nrow(joined)) {
      result[[length(result) + 1L]] <- tibble::tibble(
        record_id = joined$record_id,
        instrument = joined$instrument,
        redcap_event_name = joined$redcap_event_name,
        repeat_instrument = rep(NA_character_, nrow(joined)),
        repeat_instance = joined$repeat_instance,
        target_source = rep("observed", nrow(joined))
      )
    }
  }
  repeating_instrument_rows <- contexts[
    contexts$repeat_mode == "repeating_instrument",
    ,
    drop = FALSE
  ]
  repeating_instrument_crossings <- allowable[
    allowable$repeat_mode == "repeating_instrument",
    ,
    drop = FALSE
  ]
  if (nrow(repeating_instrument_rows) &&
      nrow(repeating_instrument_crossings)) {
    repeating_instrument_rows$instrument <-
      repeating_instrument_rows$context_instrument
    repeating_instrument_rows$context_instrument <- NULL
    joined <- merge(
      as.data.frame(repeating_instrument_rows),
      as.data.frame(repeating_instrument_crossings),
      by = c("instrument", "redcap_event_name", "repeat_mode"),
      sort = FALSE
    )
    if (nrow(joined)) {
      result[[length(result) + 1L]] <- tibble::tibble(
        record_id = joined$record_id,
        instrument = joined$instrument,
        redcap_event_name = joined$redcap_event_name,
        repeat_instrument = joined$instrument,
        repeat_instance = joined$repeat_instance,
        target_source = rep("observed", nrow(joined))
      )
    }
  }
  if (!length(result)) return(.assessible_target_build_prototype())
  dplyr::bind_rows(result)
}

.assessible_target_build_scheduled <- function(schedule, snapshot, data, type) {
  if (!nrow(schedule)) return(.assessible_target_build_prototype())
  if (identical(type, "explicit")) {
    return(tibble::tibble(
      record_id = schedule$record_id,
      instrument = schedule$instrument,
      redcap_event_name = schedule$redcap_event_name,
      repeat_instrument = ifelse(
        schedule$repeat_mode == "repeating_instrument",
        schedule$instrument,
        NA_character_
      ),
      repeat_instance = schedule$repeat_instance,
      target_source = rep("explicit", nrow(schedule))
    ))
  }
  if (!isTRUE(snapshot$project$longitudinal)) {
    records <- unique(data$.rcm_record_id)
    if (!length(records)) {
      .schedule_warn_empty_arm("<classic>")
      return(.assessible_target_build_prototype())
    }
    schedule_index <- rep(seq_len(nrow(schedule)), each = length(records))
    record_index <- rep(seq_along(records), times = nrow(schedule))
    expanded <- schedule[schedule_index, , drop = FALSE]
    return(tibble::tibble(
      record_id = records[record_index],
      instrument = expanded$instrument,
      redcap_event_name = expanded$redcap_event_name,
      repeat_instrument = ifelse(
        expanded$repeat_mode == "repeating_instrument",
        expanded$instrument,
        NA_character_
      ),
      repeat_instance = expanded$repeat_instance,
      target_source = rep("extended", nrow(expanded))
    ))
  }
  record_arms <- .record_map_arms(data, snapshot)
  schedule_arm <- snapshot$event_arms$arm_num[
    match(schedule$redcap_event_name, snapshot$event_arms$redcap_event_name)
  ]
  empty_arm <- !schedule_arm %in% record_arms$arm_num
  if (any(empty_arm)) {
    .schedule_warn_empty_arm(schedule$redcap_event_name[empty_arm])
  }
  if (all(empty_arm)) return(.assessible_target_build_prototype())
  schedule_rows <- data.frame(
    arm_num = schedule_arm[!empty_arm],
    instrument = schedule$instrument[!empty_arm],
    redcap_event_name = schedule$redcap_event_name[!empty_arm],
    repeat_instance = schedule$repeat_instance[!empty_arm],
    repeat_mode = schedule$repeat_mode[!empty_arm],
    stringsAsFactors = FALSE
  )
  expanded <- merge(
    schedule_rows,
    as.data.frame(record_arms),
    by = "arm_num",
    sort = FALSE
  )
  tibble::tibble(
    record_id = expanded$record_id,
    instrument = expanded$instrument,
    redcap_event_name = expanded$redcap_event_name,
    repeat_instrument = ifelse(
      expanded$repeat_mode == "repeating_instrument",
      expanded$instrument,
      NA_character_
    ),
    repeat_instance = expanded$repeat_instance,
    target_source = rep("extended", nrow(expanded))
  )
}

.assessible_target_merge_sources <- function(observed, scheduled, construction) {
  if (!nrow(observed)) return(scheduled)
  if (!nrow(scheduled)) return(observed)
  identity_columns <- .assessible_target_list_identity_columns()
  output_columns <- c(identity_columns, "target_source")
  scheduled_keys <- unique(as.data.frame(
    scheduled[, identity_columns, drop = FALSE]
  ))
  scheduled_keys$.extended <- TRUE
  observed_marked <- merge(
    as.data.frame(observed),
    scheduled_keys,
    by = identity_columns,
    all.x = TRUE,
    sort = FALSE
  )
  overlap <- !is.na(observed_marked$.extended)
  if (identical(construction, "from_data")) {
    observed_marked$target_source[overlap] <- "observed+extended"
  }
  observed_result <- tibble::as_tibble(
    observed_marked[, output_columns, drop = FALSE]
  )
  observed_keys <- unique(as.data.frame(
    observed[, identity_columns, drop = FALSE]
  ))
  observed_keys$.observed <- TRUE
  scheduled_marked <- merge(
    as.data.frame(scheduled),
    observed_keys,
    by = identity_columns,
    all.x = TRUE,
    sort = FALSE
  )
  extension_only <- is.na(scheduled_marked$.observed)
  if (!any(extension_only)) return(observed_result)
  dplyr::bind_rows(
    observed_result,
    scheduled_marked[extension_only, output_columns, drop = FALSE]
  )
}

.assessible_target_compute_order <- function(targets, snapshot, instruments) {
  if (!nrow(targets)) return(integer())
  instrument_order <- match(targets$instrument, instruments)
  event_order <- ifelse(
    is.na(targets$redcap_event_name),
    0L,
    match(targets$redcap_event_name, snapshot$event_order)
  )
  repeat_kind <- ifelse(
    is.na(targets$repeat_instance),
    0L,
    ifelse(is.na(targets$repeat_instrument), 1L, 2L)
  )
  order(
    instrument_order, event_order, targets$record_id,
    repeat_kind, targets$repeat_instance, na.last = TRUE
  )
}

.assessible_target_order_rows <- function(targets, snapshot, instruments) {
  if (!nrow(targets)) return(.assessible_target_build_prototype())
  targets <- targets[
    .assessible_target_compute_order(targets, snapshot, instruments),
    , drop = FALSE
  ]
  rownames(targets) <- NULL
  tibble::as_tibble(targets)
}
