empty_target_results_fixture <- function() {
  tibble::tibble(
    record_id = character(), instrument = character(),
    redcap_event_name = character(), repeat_instrument = character(),
    repeat_instance = integer(), target_source = character(),
    event_row_started = character(),
    repeat_instance_row_started = character(),
    instrument_started = character(), field_complete = character(),
    fields_assessed = integer(), fields_failed = integer(),
    field_applicability_reason = character()
  )
}

empty_diagnostics_fixture <- function() {
  tibble::tibble(
    stage = integer(), operation = character(), completed = logical(),
    elapsed_seconds = double()
  )
}

complete_report_fixture <- function(
  plan,
  target_results = empty_target_results_fixture(),
  summary = .summary_build_prototype(),
  missing = .missing_build_prototype(),
  verification = .verification_build_audit(),
  diagnostics = empty_diagnostics_fixture(),
  details = NULL
) {
  structure(
    list(
      plan = plan,
      target_results = target_results,
      summary = summary,
      missing = missing,
      verification = verification,
      diagnostics = diagnostics,
      details = details
    ),
    class = c("redcapmissing", "list")
  )
}
