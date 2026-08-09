# Run-local lazy response masks over normalized physical rows.

.run_plan_response_masks_build <- function(records) {
  masks <- new.env(hash = FALSE, parent = emptyenv())
  masks$records <- records
  masks$present <- new.env(hash = TRUE, parent = emptyenv())
  masks$selected <- new.env(hash = TRUE, parent = emptyenv())
  masks
}

.run_plan_response_masks_present <- function(masks, field) {
  if (!exists(field, envir = masks$present, inherits = FALSE)) {
    assign(
      field,
      !.field_complete_detect_missing_values(masks$records[[field]]),
      envir = masks$present
    )
  }
  get(field, envir = masks$present, inherits = FALSE)
}

.run_plan_response_masks_selected <- function(masks, field) {
  if (!exists(field, envir = masks$selected, inherits = FALSE)) {
    assign(
      field,
      .branching_logic_detect_selected_checkbox(masks$records[[field]]),
      envir = masks$selected
    )
  }
  get(field, envir = masks$selected, inherits = FALSE)
}
