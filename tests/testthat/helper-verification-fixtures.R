run_plan_verified_row <- function(
  record = "1", field_name = "required_note",
  ts = "2026-07-25 12:00:00", status = "VERIFIED", username = "alice"
) {
  tibble::tibble(
    project_id = "77", record = record, event_id = NA_character_,
    field_name = field_name, repeat_instrument = NA_character_,
    instance = NA_integer_, ts = ts, current_query_status = status,
    username = username
  )
}
