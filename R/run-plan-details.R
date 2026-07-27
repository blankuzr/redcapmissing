# Construction of run_plan() details rows.

.details_resolve_validation_level <- function(repeat_instance) {
  ifelse(is.na(repeat_instance), "event:instrument", "event:instrument:instance")
}

.details_build_check_rows <- function(targets, target_results, field_rows) {
  add_target_checks <- function(check, disposition, reason = NA_character_) {
    n <- nrow(targets)
    tibble::tibble(
      record_id = targets$record_id, instrument = targets$instrument,
      redcap_event_name = targets$redcap_event_name,
      repeat_instrument = targets$repeat_instrument,
      repeat_instance = targets$repeat_instance, target_source = targets$target_source,
      validation_level = .details_resolve_validation_level(targets$repeat_instance),
      validation_check = rep(check, n), field_name = rep(NA_character_, n),
      field_label = rep(NA_character_, n), field_type = rep(NA_character_, n),
      branching_logic = rep(NA_character_, n), branch_satisfied = rep(NA, n),
      value_summary = rep(NA_character_, n),
      raw_disposition = as.character(disposition),
      verification_applied = rep(FALSE, n),
      effective_disposition = as.character(disposition),
      reason = rep(reason, length.out = n)
    )
  }
  event_reason <- rep(NA_character_, nrow(target_results))
  event_reason[target_results$event_row_started == "not applicable"] <-
    "not applicable for classic project"
  repeat_reason <- rep(NA_character_, nrow(target_results))
  repeat_reason[target_results$repeat_instance_row_started == "not applicable"] <-
    "not a repeating target"
  rows <- list(
    add_target_checks("event-row-started", target_results$event_row_started, event_reason),
    add_target_checks("repeat-instance-row-started", target_results$repeat_instance_row_started, repeat_reason),
    add_target_checks("instrument-started", target_results$instrument_started)
  )
  field_target_rows <- add_target_checks("field-complete", target_results$field_complete)
  field_target_rows$reason[target_results$field_complete == "not applicable"] <-
    target_results$field_applicability_reason[
      target_results$field_complete == "not applicable"
    ]
  rows[[4L]] <- field_target_rows[target_results$field_complete %in%
    c("not applicable", "not reached"), , drop = FALSE]
  if (nrow(field_rows)) {
    public_fields <- .field_complete_build_public_rows(field_rows, targets)
    rows[[5L]] <- tibble::tibble(
      record_id = public_fields$record_id,
      instrument = public_fields$instrument,
      redcap_event_name = public_fields$redcap_event_name,
      repeat_instrument = public_fields$repeat_instrument,
      repeat_instance = public_fields$repeat_instance,
      target_source = public_fields$target_source,
      validation_level = .details_resolve_validation_level(public_fields$repeat_instance),
      validation_check = "field-complete",
      field_name = public_fields$field_name,
      field_label = public_fields$field_label,
      field_type = public_fields$field_type,
      branching_logic = public_fields$branching_logic,
      branch_satisfied = public_fields$branch_satisfied,
      value_summary = public_fields$value_summary,
      raw_disposition = public_fields$raw_disposition,
      verification_applied = public_fields$verification_applied,
      effective_disposition = public_fields$effective_disposition,
      reason = NA_character_
    )
  }
  dplyr::bind_rows(rows)
}
