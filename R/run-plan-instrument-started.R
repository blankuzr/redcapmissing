# Execution of the instrument-started assessment.

.instrument_started_build_detection_plan <- function(
  metadata,
  instruments,
  id_field,
  field_dictionary = NULL
) {
  field_dictionary <- field_dictionary %||% .metadata_build_field_dictionary(metadata)
  rows <- metadata[metadata$form_name %in% instruments &
    metadata$field_name != id_field & !metadata$field_type %in% c("descriptive", "calc"),
    , drop = FALSE]
  result <- stats::setNames(vector("list", length(instruments)), instruments)
  for (instrument in instruments) {
    instrument_rows <- rows[rows$form_name == instrument, , drop = FALSE]
    fields <- unique(unlist(
      .run_plan_resolve_export_fields(
        metadata, instrument_rows$field_name, field_dictionary = field_dictionary
      ),
      use.names = FALSE
    ))
    if (!length(fields)) {
      .condition_signal_error(paste0("Instrument `", instrument,
        "` has no usable instrument start detection fields."), "project")
    }
    result[[instrument]] <- fields
  }
  list(export_fields = result)
}

.instrument_started_evaluate_targets <- function(
  target_indices_by_instrument,
  target_row,
  upstream_pass,
  response_masks,
  detection_fields,
  initial_status
) {
  status <- initial_status
  candidate <- !is.na(upstream_pass) & upstream_pass & !is.na(target_row)
  if (!any(candidate)) return(status)

  for (instrument in names(detection_fields)) {
    target_index <- target_indices_by_instrument[[instrument]]
    target_index <- target_index[candidate[target_index]]
    if (!length(target_index)) next

    data_index <- target_row[target_index]
    started <- rep.int(FALSE, length(target_index))
    for (field in detection_fields[[instrument]]) {
      present <- if (grepl("___", field, fixed = TRUE)) {
        .run_plan_response_masks_selected(response_masks, field)[data_index]
      } else {
        .run_plan_response_masks_present(response_masks, field)[data_index]
      }
      started <- started | present
      if (all(started)) break
    }
    status[target_index] <- ifelse(started, "passed", "failed")
  }
  status
}
