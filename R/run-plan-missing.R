# Construction of the run_plan() missing component and REDCap URLs.

.missing_format_validation_context <- function(event, repeat_instance) {
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

.missing_add_urls <- function(rows, rcon, snapshot) {
  rows$url <- rep(NA_character_, nrow(rows))
  if (!nrow(rows)) return(rows)

  instance_url <- tryCatch(rcon$url, error = function(e) NULL)
  if (is.function(instance_url)) {
    instance_url <- tryCatch(instance_url(), error = function(e) NULL)
  }
  version_method <- tryCatch(rcon$version, error = function(e) NULL)
  version <- if (is.function(version_method)) {
    tryCatch(version_method(), error = function(e) NULL)
  } else {
    version_method
  }
  if (length(instance_url) != 1L || length(version) != 1L) return(rows)
  instance_url <- as.character(instance_url)
  version <- as.character(version)
  if (
    is.na(instance_url) || !nzchar(trimws(instance_url)) ||
      is.na(version) || !nzchar(trimws(version))
  ) {
    return(rows)
  }

  event_id <- rep(NA_character_, nrow(rows))
  complete <- !is.na(rows$record_id) & nzchar(rows$record_id) &
    !is.na(rows$instrument) & nzchar(rows$instrument)
  if (isTRUE(snapshot$project$longitudinal)) {
    event_index <- match(
      rows$redcap_event_name,
      snapshot$events$redcap_event_name
    )
    event_id <- as.character(snapshot$events$event_id[event_index])
    complete <- complete & !is.na(event_id) & nzchar(event_id)
  }
  if (!any(complete)) return(rows)

  base_url <- sub("/api(/|)$", "", instance_url)
  rows$url[complete] <- sprintf(
    "%s/redcap_v%s/DataEntry/index.php?pid=%s&page=%s&id=%s",
    base_url,
    version,
    snapshot$project$project_id,
    rows$instrument[complete],
    rows$record_id[complete]
  )
  if (isTRUE(snapshot$project$longitudinal)) {
    rows$url[complete] <- paste0(
      rows$url[complete],
      "&event_id=",
      event_id[complete]
    )
  }
  rows
}

.missing_build_rows <- function(
  targets,
  target_results,
  field_rows,
  rcon,
  snapshot
) {
  empty <- tibble::tibble(
    record_id = character(), redcap_event_name = character(),
    repeat_instrument = character(), repeat_instance = integer(),
    validation_context = character(), instrument = character(),
    validation_check = character(), field_name = character(),
    field_label = character(), field_type = character(),
    branching_logic = character(), url = character()
  )
  target_failure <- function(check, disposition) {
    index <- which(disposition == "failed")
    if (!length(index)) return(NULL)
    tibble::tibble(
      record_id = targets$record_id[index],
      redcap_event_name = targets$redcap_event_name[index],
      repeat_instrument = targets$repeat_instrument[index],
      repeat_instance = targets$repeat_instance[index],
      instrument = targets$instrument[index],
      validation_check = rep.int(check, length(index)),
      field_name = rep.int(NA_character_, length(index)),
      field_label = rep.int(NA_character_, length(index)),
      field_type = rep.int(NA_character_, length(index)),
      branching_logic = rep.int(NA_character_, length(index))
    )
  }

  pieces <- list(
    target_failure("event-row-started", target_results$event_row_started),
    target_failure(
      "repeat-instance-row-started",
      target_results$repeat_instance_row_started
    ),
    target_failure("instrument-started", target_results$instrument_started)
  )
  effective_failure <- which(field_rows$effective_disposition == "failed")
  if (length(effective_failure)) {
    public_fields <- .field_complete_build_public_rows(
      field_rows[effective_failure, , drop = FALSE],
      targets
    )
    pieces[[4L]] <- tibble::tibble(
      record_id = public_fields$record_id,
      redcap_event_name = public_fields$redcap_event_name,
      repeat_instrument = public_fields$repeat_instrument,
      repeat_instance = public_fields$repeat_instance,
      instrument = public_fields$instrument,
      validation_check = rep.int("field-complete", nrow(public_fields)),
      field_name = public_fields$field_name,
      field_label = public_fields$field_label,
      field_type = public_fields$field_type,
      branching_logic = public_fields$branching_logic
    )
  }

  rows <- dplyr::bind_rows(pieces)
  if (!nrow(rows)) return(empty)
  rows <- .missing_add_urls(rows, rcon, snapshot)
  rows$validation_context <- .missing_format_validation_context(
    rows$redcap_event_name,
    rows$repeat_instance
  )
  rows <- rows[, c(
    "record_id", "redcap_event_name", "repeat_instrument", "repeat_instance",
    "validation_context", "instrument", "validation_check", "field_name",
    "field_label", "field_type", "branching_logic", "url"
  ), drop = FALSE]
  tibble::as_tibble(rows)
}
