#' Build a branching-aware REDCap missing-field report
#'
#' @description
#' `find_missing()` checks whether expected REDCap fields are present
#' for a single instrument/form. Expected fields are determined from REDCap
#' project metadata and project structure supplied through a
#' `redcapAPI::redcapConnection()` workflow.
#'
#' @details
#' A field is expected when it is on the requested form and, if it has REDCap
#' branching logic, that branching logic evaluates to `TRUE` for the record row.
#' Fields without branching logic are always expected on rows where the form is
#' offered. Blank values and `NA` values are considered missing.
#'
#' The function relies on `redcapAPI` project structure exposed through `rcon`
#' to avoid checking a form on events or repeating rows where that form is not
#' offered.
#' It inspects available connection methods such as `rcon$metadata()`,
#' `rcon$mapping()` / `rcon$mappings()`, `rcon$repeatInstrumentEvent()`, and
#' `rcon$projectInformation()` when present.
#'
#' Checkbox fields are treated as one REDCap root field. A checkbox root is
#' considered answered when at least one exported child column
#' (`field___choice`) is selected. Unselected sibling checkboxes are not flagged
#' as missing once the root question has at least one selected child.
#'
#' If an offered form has no data entered at all, the report emits one form-level
#' missing row instead of one missing row per field. Longitudinal event rows that
#' are absent from the export are also emitted as one form-level missing row for
#' that record and event.
#'
#' Validation is intentionally split between REDCap-specific R preprocessing and
#' `pointblank` reporting. The R code determines each row's `missing_scope`
#' before `pointblank` runs:
#' \describe{
#'   \item{`"form_blank"`}{An exported row exists for the form context, but every
#'     exported data-capturing field on that form is blank or unchecked. The row
#'     is emitted with `form_started = FALSE`, `event_row_present = TRUE`, and
#'     `value_present = TRUE`, so it fails only the whole-form pointblank check.}
#'   \item{`"event_absent"`}{The form is mapped to a longitudinal event for a
#'     record, but no exported row exists for that record/event. The row is
#'     emitted with `event_row_present = FALSE`, `form_started = TRUE`, and
#'     `value_present = TRUE`, so it fails only the missing-event pointblank
#'     check.}
#'   \item{`"repeat_absent"`}{The form is assessed in a REDCap repeating event
#'     or as a repeating instrument, and an expected repeat instance is absent
#'     for the record/event context. The row is emitted with
#'     `repeat_instance_present = FALSE`,
#'     `form_started = TRUE`, `event_row_present = TRUE`, and
#'     `value_present = TRUE`, so it fails only the missing-repeat pointblank
#'     check.}
#'   \item{`"any_field_missing"`}{The form context passed event, repeat, and
#'     whole-form checks, but at least one expected field in that context is
#'     blank. The row fails when `value_present` is `FALSE`.}
#'   \item{`"field"`}{The form context exists and is started. A field is expected
#'     after branching logic, required-field filtering, type exclusions, and
#'     ignored fields are applied. The row fails only when `value_present` is
#'     `FALSE`.}
#' }
#'
#' The whole-form checks are built before field-level checks. Contexts already
#' classified as `form_blank` are removed from field-level assessment so the
#' report returns one form-level row instead of one row per missing field.
#' `pointblank` does not rediscover REDCap branching, blank forms, or absent
#' events; it provides the auditable validation steps and failed-row extraction
#' after those REDCap-specific rules have been encoded.
#'
#' The pointblank checks are scoped with preconditions and stratified by a
#' `validation_context` so the agent summary denominator (`n`) is clinically
#' meaningful for each event or repeat instance. Context-level checks are
#' counted over expected record contexts. Granular field checks are counted over
#' expected field rows within each context. The `any_field_missing` check rolls
#' those field rows up to one row per evaluable record context.
#'
#' The returned `pointblank` agent is interrogated with failed-row extraction
#' enabled. The row-level extract is also returned as `missing` for easier
#' review and downstream correction workflows.
#'
#' @param data A data frame or tibble of REDCap records, usually produced by
#'   `redcapAPI::exportRecordsTyped()`. For longitudinal projects, this should
#'   include the REDCap system column `redcap_event_name`. For repeating
#'   instruments/events, it should include `redcap_repeat_instrument` and
#'   `redcap_repeat_instance` when those columns are present in the export.
#' @param rcon A `redcapAPI::redcapConnection()` object, or an offline or
#'   preserved connection-like object that mirrors the same methods, and that
#'   provides project metadata through `rcon$metadata()`. When available,
#'   form-event mapping, repeating instrument/event metadata, and project
#'   information are also read from the connection object.
#' @param form Required. A single REDCap form/instrument name to assess. No
#'   default is supplied so callers must choose the form deliberately.
#' @param desired_events Character vector of REDCap event names to assess for
#'   the requested form, or `NULL`. When a form is offered on multiple REDCap
#'   events, this argument can restrict the report to a selected subset of those
#'   events. If `NULL`, all offered events are assessed. If the form is offered
#'   on only one event, this argument is ignored. If the form is regular on
#'   some events and repeating on others, `desired_events` also determines
#'   whether repeat-instance logic is activated. Defaults to `NULL`.
#' @param required_fields Logical scalar. When `TRUE`, only fields marked as
#'   required in the REDCap metadata `required_field` column are assessed. When
#'   `FALSE`, all fields on the form are assessed after `exclude_types` and
#'   `ignore_fields` are applied. Defaults to `TRUE`.
#' @param ignore_fields Character vector of REDCap field names to skip. Values
#'   may be root field names such as `"checkbox_field"` or exported checkbox
#'   child names such as `"checkbox_field___1"`; checkbox child names are mapped
#'   back to their root field and the whole checkbox question is skipped.
#'   Defaults to `NULL`, which means no fields are ignored.
#' @param ignore_ids Character vector of REDCap record IDs to skip. Rows whose
#'   record identifier is in this vector are excluded before the missingness
#'   report is built. Defaults to `NULL`, which means no records are ignored.
#' @param exclude_types Character vector of REDCap field types to exclude from
#'   assessment. Defaults to `"descriptive"` because descriptive fields do not
#'   capture values. These types are excluded regardless of `required_fields`.
#' @param expected_repeats Positive whole-number scalar, or `NULL`. When `form`
#'   is assessed in a REDCap repeating event or as a repeating instrument, this
#'   is the minimum number of repeat instances expected for every assessed
#'   record/event context. For example, `expected_repeats = 2L` checks that
#'   repeat instances `1` and `2` exist. If the form is repeating and this
#'   argument is `NULL`, the function warns and assumes `1L` for the requested
#'   repeating contexts. If the requested events do not include any repeating
#'   contexts for the form, this argument is ignored.
#'
#' @return A list with class `"redcapmissing"` containing:
#' \describe{
#'   \item{`agent`}{An interrogated `pointblank` agent.}
#'   \item{`missing`}{A tibble of failed rows from the `pointblank` extract,
#'     keyed by record, event, repeat context, form, and field.}
#'   \item{`expected`}{The expected-field table checked by `pointblank`.}
#'   \item{`validation_rows`}{The full table supplied to `pointblank`, including
#'     field-level checks, any-field roll-up checks, blank-form checks,
#'     missing-event checks, and missing-repeat checks.}
#'   \item{`form_missing`}{One-row-per-context checks for exported form rows
#'     where every form field is blank or unchecked.}
#'   \item{`form_checks`}{All whole-form startedness checks, including passed
#'     and failed form contexts.}
#'   \item{`event_missing`}{One-row-per-context checks for longitudinal events
#'     where the form is offered but no event row exists in the export.}
#'   \item{`event_checks`}{All event-row presence checks, including passed and
#'     failed record/event contexts.}
#'   \item{`repeat_missing`}{One-row-per-context checks for expected repeat
#'     instances that are absent from the export.}
#'   \item{`repeat_checks`}{All expected repeat-instance checks, including
#'     passed and failed repeat contexts.}
#'   \item{`any_field_missing`}{One-row-per-context checks where at least one
#'     expected field is blank.}
#'   \item{`any_field_checks`}{All any-field roll-up checks, including passed
#'     and failed evaluable contexts.}
#'   \item{`field_plan`}{The metadata-derived field plan for the requested
#'     form, including checkbox child export columns.}
#'   \item{`required_fields`}{Whether the report was limited to REDCap-required
#'     fields.}
#'   \item{`desired_events`}{The REDCap events actually used for assessment, or
#'     `character(0)` when no event restriction applied.}
#'   \item{`expected_repeats`}{The repeat-instance count used for repeating
#'     forms, or `NULL` for non-repeating forms.}
#'   \item{`ignored_fields`}{Root field names skipped because of
#'     `ignore_fields`.}
#'   \item{`ignored_ids`}{Record IDs skipped because of `ignore_ids`.}
#'   \item{`project`}{A compact list of REDCap project structure inferred from
#'     `rcon`.}
#'   \item{`form`}{The assessed form name.}
#'   \item{`id_col`}{The REDCap record identifier field.}
#'   \item{`system_fields`}{The REDCap system field names used internally.}
#' }
#'
#' @seealso [redcapAPI::redcapConnection()], [redcapAPI::exportRecordsTyped()]
#' @references
#' Nutter B, Garbett S, Obregon S, Obadia T, Lehr M, High B, Lane S,
#' Beasley W, Gray W, Kennedy N, Hsi-Nien T, Horner J, Stephens J, Beck C,
#' Johnson B, Chase P, Tobias P (2026). *redcapAPI: Accessing data from REDCap
#' projects using the API*. R package version 2.12.0.
#' <https://doi.org/10.5281/zenodo.10564837>.
#'
#' @examples
#' \dontrun{
#' rcon <- redcapAPI::redcapConnection(
#'   url = Sys.getenv("REDCAP_API_URL"),
#'   token = Sys.getenv("REDCAP_API_TOKEN")
#' )
#'
#' records <- redcapAPI::exportRecordsTyped(
#'   rcon,
#'   cast = list(
#'     radio = redcapAPI::castCode,
#'     dropdown = redcapAPI::castCode,
#'     yesno = redcapAPI::castCode,
#'     checkbox = redcapAPI::castRaw,
#'     system = redcapAPI::castRaw
#'   )
#' )
#'
#' baseline_missing <- find_missing(
#'   data = records,
#'   rcon = rcon,
#'   form = "baseline_form",
#'   desired_events = c("baseline_event", "followup_event"),
#'   ignore_fields = c("status_flag", "screening_code")
#' )
#'
#' baseline_missing$agent
#' baseline_missing$missing
#'
#' repeat_missing <- find_missing(
#'   data = records,
#'   rcon = rcon,
#'   form = "repeat_form",
#'   expected_repeats = 2L
#' )
#' }
#'
#' @export
find_missing <- function(
  data,
  rcon,
  form,
  desired_events = NULL,
  required_fields = TRUE,
  ignore_fields = NULL,
  ignore_ids = NULL,
  exclude_types = "descriptive",
  expected_repeats = NULL
) {
  if (missing(form)) {
    stop("Provide `form`; this argument has no default.", call. = FALSE)
  }
  if (
    !is.logical(required_fields) ||
      length(required_fields) != 1 ||
      is.na(required_fields)
  ) {
    stop("`required_fields` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!requireNamespace("pointblank", quietly = TRUE)) {
    stop(
      "Install the pointblank package before building a missing-field report.",
      call. = FALSE
    )
  }

  # Normalize inputs and derive project context from the redcapAPI connection.
  records <- tibble::as_tibble(data)
  all_records <- records

  meta <- .miss_get_metadata(rcon)
  .miss_check_metadata(meta)

  project <- .miss_get_project(rcon = rcon, meta = meta, form = form)
  project <- .miss_resolve_desired_events(
    project = project,
    desired_events = desired_events,
    form = form
  )
  expected_repeats <- .miss_resolve_expected_repeats(
    expected_repeats = expected_repeats,
    project = project,
    form = form
  )
  if (!project$id_col %in% names(records)) {
    stop(
      "The REDCap record identifier column `",
      project$id_col,
      "` is not present in `data`.",
      call. = FALSE
    )
  }

  # Remove caller-specified records before branch evaluation or validation.
  ignore_ids <- unique(.miss_chr_vec(ignore_ids %||% character()))
  if (length(ignore_ids) > 0) {
    records <- records[
      !.miss_chr_vec(records[[project$id_col]]) %in% ignore_ids,
      ,
      drop = FALSE
    ]
    all_records <- all_records[
      !.miss_chr_vec(all_records[[project$id_col]]) %in% ignore_ids,
      ,
      drop = FALSE
    ]
  }

  # Whole-form startedness uses every metadata field on the form, regardless of
  # required/excluded/ignored status. The REDCap record ID is excluded because
  # it is exported even when the form itself has not been started.
  form_all_meta <- meta[meta$form_name == form, , drop = FALSE]

  # Build the assessable form metadata after caller-specified exclusions.
  form_meta <- meta[meta$form_name == form, , drop = FALSE]
  if (length(exclude_types) > 0) {
    form_meta <- form_meta[
      !form_meta$field_type %in% exclude_types,
      ,
      drop = FALSE
    ]
  }
  if (required_fields) {
    if (!"required_field" %in% names(form_meta)) {
      stop(
        "rcon$metadata() must include `required_field` when `required_fields = TRUE`.",
        call. = FALSE
      )
    }
    form_meta <- form_meta[
      .miss_required_vec(form_meta$required_field),
      ,
      drop = FALSE
    ]
  }

  # Map ignored checkbox child columns back to their REDCap root fields.
  field_names <- .miss_get_field_names(form_meta)
  ignore_fields <- unique(as.character(ignore_fields %||% character()))
  ignore_roots <- .miss_get_ignore_roots(
    form_meta = form_meta,
    field_names = field_names,
    ignore_fields = ignore_fields
  )
  if (length(ignore_roots) > 0) {
    form_meta <- form_meta[
      !form_meta$field_name %in% ignore_roots,
      ,
      drop = FALSE
    ]
    field_names <- .miss_get_field_names(form_meta)
  }

  if (nrow(form_meta) == 0) {
    stop(
      "No assessable metadata rows were found for form `",
      form,
      "`.",
      call. = FALSE
    )
  }

  # Keep only rows where the requested form is offered in REDCap.
  records <- .miss_filter_form_rows(
    records = records,
    form = form,
    project = project
  )

  event_checks <- .miss_build_event_check_rows(
    records = all_records,
    form_records = records,
    project = project,
    form = form
  )
  event_checks <- .miss_add_validation_context(event_checks)
  event_missing <- event_checks[event_checks$missing_scope == "event_absent", , drop = FALSE]

  repeat_checks <- .miss_build_repeat_check_rows(
    records = all_records,
    form_records = records,
    project = project,
    form = form,
    expected_repeats = expected_repeats
  )
  repeat_checks <- .miss_add_validation_context(repeat_checks)
  repeat_missing <- repeat_checks[repeat_checks$missing_scope == "repeat_absent", , drop = FALSE]

  expected_contexts <- .miss_build_expected_contexts(
    records = all_records,
    event_checks = event_checks,
    repeat_checks = repeat_checks,
    project = project
  )

  form_checks <- .miss_build_form_check_rows(
    records = records,
    form_meta = form_all_meta,
    project = project,
    form = form,
    expected_contexts = expected_contexts
  )
  form_checks <- .miss_add_validation_context(form_checks)
  form_missing <- form_checks[form_checks$missing_scope == "form_blank", , drop = FALSE]

  # Do not emit field-by-field failures when the whole form is unstarted.
  records_for_fields <- .miss_drop_form_missing_records(
    records = records,
    form_missing = form_missing,
    project = project
  )

  # Build the field plan and value-choice map needed for branching evaluation.
  choice_fields <- unique(c(
    form_meta$field_name,
    .miss_extract_logic_fields(form_meta$branching_logic)
  ))
  choice_map <- .miss_build_choice_map(meta[
    meta$field_name %in% choice_fields,
    ,
    drop = FALSE
  ])
  field_plan <- .miss_build_field_plan(
    form_meta = form_meta,
    field_names = field_names
  )

  # Create one expected-field row per record/form/field where the branch is open.
  expected <- .miss_build_expected(
    records = records_for_fields,
    lookup_records = all_records,
    field_plan = field_plan,
    meta = meta,
    choice_map = choice_map,
    project = project,
    form = form
  )
  expected <- .miss_add_validation_context(expected)
  any_field_checks <- .miss_build_any_field_check_rows(
    expected = expected,
    form = form
  )
  any_field_missing <- any_field_checks[
    any_field_checks$missing_scope == "any_field_missing",
    ,
    drop = FALSE
  ]
  validation_rows <- dplyr::bind_rows(
    event_checks,
    repeat_checks,
    form_checks,
    any_field_checks,
    expected
  )
  validation_rows <- .miss_add_validation_context(validation_rows)

  # Let pointblank own the validation object and failed-row extract.
  agent <- pointblank::create_agent(
    tbl = validation_rows,
    tbl_name = paste0(form, "_expected_fields"),
    label = paste0("Branch-aware missing fields for ", form)
  )

  agent <- .miss_add_validation_step(
    agent = agent,
    rows = event_checks,
    check_scope = "event_row_present",
    column = "event_row_present",
    step_id = paste0(form, "_event_row_missing"),
    label = "Offered REDCap event row exists for the form",
    keep_zero = TRUE
  )

  if (nrow(repeat_checks) > 0) {
    agent <- .miss_add_validation_step(
      agent = agent,
      rows = repeat_checks,
      check_scope = "repeat_instance_present",
      column = "repeat_instance_present",
      step_id = paste0(form, "_repeat_instance_missing"),
      label = "Expected REDCap repeat instances exist for the form"
    )
  }

  agent <- .miss_add_validation_step(
    agent = agent,
    rows = form_checks,
    check_scope = "form_started",
    column = "form_started",
    step_id = paste0(form, "_entire_form_blank"),
    label = "Offered REDCap form has at least one entered field",
    keep_zero = TRUE
  )

  if (nrow(any_field_checks) > 0) {
    agent <- .miss_add_validation_step(
      agent = agent,
      rows = any_field_checks,
      check_scope = "any_field_missing",
      column = "value_present",
      step_id = paste0(form, "_any_field_missing"),
      label = "Any expected REDCap field is missing"
    )
  }

  agent <- .miss_add_validation_step(
    agent = agent,
    rows = expected,
    check_scope = "field",
    column = "value_present",
    step_id = paste0(form, "_missing_fields"),
    label = "Expected REDCap fields are not blank",
    keep_zero = TRUE
  ) |>
    pointblank::interrogate(extract_failed = TRUE, progress = FALSE)
  agent <- .miss_annotate_agent_validation_set(
    agent = agent,
    validation_rows = validation_rows
  )

  missing <- .miss_get_failed_rows(agent)

  out <- list(
    agent = agent,
    missing = missing,
    expected = expected,
    validation_rows = validation_rows,
    form_checks = form_checks,
    form_missing = form_missing,
    event_checks = event_checks,
    event_missing = event_missing,
    repeat_checks = repeat_checks,
    repeat_missing = repeat_missing,
    any_field_checks = any_field_checks,
    any_field_missing = any_field_missing,
    field_plan = field_plan,
    required_fields = required_fields,
    desired_events = project$desired_events %||% character(),
    expected_repeats = expected_repeats,
    ignored_fields = ignore_roots,
    ignored_ids = ignore_ids,
    project = project,
    form = form,
    id_col = project$id_col,
    system_fields = project$system_fields
  )
  class(out) <- "redcapmissing"
  out
}
