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
#' `rcon$instruments()`, `rcon$mapping()` / `rcon$mappings()`,
#' `rcon$repeatInstrumentEvent()`, and `rcon$projectInformation()` when
#' present.
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
#' `pointblank` reporting. The R code assigns each row to a positive
#' `validation_scope` before `pointblank` runs:
#' \describe{
#'   \item{`"event_row_exists"`}{The form is mapped to a longitudinal event for
#'     a record. The row passes when an exported row exists for that
#'     record/event context.}
#'   \item{`"repeat_instance_row_exists"`}{The form is assessed in a REDCap
#'     repeating event or as a repeating instrument. The row passes when an
#'     exported row exists for the expected repeat instance.}
#'   \item{`"form_started"`}{An exported row exists for the form context. The
#'     row passes when at least one data-capturing field on that form is not
#'     blank or unchecked.}
#'   \item{`"form_complete"`}{The row context exists and the form is started.
#'     The row passes when all expected fields in that context are complete.}
#'   \item{`"fields_complete"`}{The row context exists and the form is started.
#'     A field is expected after branching logic, required-field filtering, type
#'     exclusions, and ignored fields are applied. The row passes when that
#'     field is complete.}
#' }
#'
#' The whole-form checks are built before field-level checks. Contexts already
#' failing `form_started` are removed from field-level assessment so the report
#' returns one form-level row instead of one row per missing field.
#' `pointblank` does not rediscover REDCap branching, blank forms, or absent
#' events; it provides the auditable validation steps and failed-row extraction
#' after those REDCap-specific rules have been encoded.
#'
#' The pointblank checks are scoped with preconditions and stratified by a
#' `validation_context` so the agent summary denominator (`n`) is clinically
#' meaningful for each event or repeat instance. Context-level checks are
#' counted over expected record contexts. Granular field checks are counted over
#' expected field rows within each context. The `form_complete` check rolls
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
#'   provides project metadata through `rcon$metadata()` and instrument labels
#'   through `rcon$instruments()`. When available, form-event mapping,
#'   repeating instrument/event metadata, and project information are also read
#'   from the connection object.
#' @param form Required. A single REDCap form/instrument name to assess. No
#'   default is supplied so callers must choose the form deliberately.
#' @param events Character vector of REDCap event names to assess for
#'   the requested form, or `NULL`. When a form is offered on multiple REDCap
#'   events, this argument can restrict the report to a selected subset of those
#'   events. If `NULL`, all offered events are assessed. If the form is offered
#'   on only one event, this argument is ignored. If the form is regular on
#'   some events and repeating on others, `events` also determines whether
#'   repeat-instance logic is activated. Defaults to `NULL`.
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
#' @param instances Positive whole-number scalar, or `NULL`. When `form`
#'   is assessed in a REDCap repeating event or as a repeating instrument, this
#'   is the minimum number of repeat instances expected for every assessed
#'   record/event context. For example, `instances = 2L` checks that repeat
#'   instances `1` and `2` exist. If the form is repeating and this argument is
#'   `NULL`, the function warns and assumes `1L` for the requested repeating
#'   contexts. If the requested events do not include any repeating
#'   contexts for the form, this argument is ignored.
#'
#' @return A list with class `"redcapmissing"` containing:
#' \describe{
#'   \item{`agent`}{An interrogated `pointblank` agent.}
#'   \item{`missing`}{A tibble of failed rows from the `pointblank` extract,
#'     keyed by record, event, repeat context, form, and field.}
#'   \item{`validation_rows`}{The full table supplied to `pointblank`, including
#'     event-row, repeat-instance, form-started, form-complete, and
#'     field-complete validation rows.}
#'   \item{`event_row_exists_checks`, `event_row_exists_failures`}{All
#'     event-row validation rows and their failed rows.}
#'   \item{`repeat_instance_row_exists_checks`,
#'     `repeat_instance_row_exists_failures`}{All repeat-instance validation
#'     rows and their failed rows.}
#'   \item{`form_started_checks`, `form_started_failures`}{All form-started
#'     validation rows and their failed rows.}
#'   \item{`form_complete_checks`, `form_complete_failures`}{All form-complete
#'     validation rows and their failed rows.}
#'   \item{`fields_complete_checks`, `fields_complete_failures`}{All
#'     field-complete validation rows and their failed rows.}
#'   \item{`field_plan`}{The metadata-derived field plan for the requested
#'     form, including checkbox child export columns.}
#'   \item{`required_fields`}{Whether the report was limited to REDCap-required
#'     fields.}
#'   \item{`events`}{The REDCap events actually used for assessment, or
#'     `character(0)` when no event restriction applied.}
#'   \item{`instances`}{The repeat-instance count used for repeating forms, or
#'     `NULL` for non-repeating forms.}
#'   \item{`ignored_fields`}{Root field names skipped because of
#'     `ignore_fields`.}
#'   \item{`ignored_ids`}{Record IDs skipped because of `ignore_ids`.}
#'   \item{`project`}{A compact list of REDCap project structure inferred from
#'     `rcon`.}
#'   \item{`form`}{The assessed form name.}
#'   \item{`form_label`}{The REDCap instrument label for `form`.}
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
#'   events = c("baseline_event", "followup_event"),
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
#'   instances = 2L
#' )
#' }
#'
#' @export
find_missing <- function(
  data,
  rcon,
  form,
  events = NULL,
  required_fields = TRUE,
  ignore_fields = NULL,
  ignore_ids = NULL,
  exclude_types = "descriptive",
  instances = NULL
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
  project <- .miss_resolve_events(
    project = project,
    events = events,
    form = form
  )
  instances <- .miss_resolve_instances(
    instances = instances,
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
  event_row_exists_failures <- event_checks[
    !event_checks$event_row_exists,
    ,
    drop = FALSE
  ]

  repeat_checks <- .miss_build_repeat_check_rows(
    records = all_records,
    form_records = records,
    project = project,
    form = form,
    instances = instances
  )
  repeat_checks <- .miss_add_validation_context(repeat_checks)
  repeat_instance_row_exists_failures <- repeat_checks[
    !repeat_checks$repeat_instance_row_exists,
    ,
    drop = FALSE
  ]

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
  form_started_failures <- form_checks[
    !form_checks$form_started,
    ,
    drop = FALSE
  ]

  # Do not emit field-by-field failures when the whole form is unstarted.
  records_for_fields <- .miss_drop_unstarted_form_records(
    records = records,
    form_started_failures = form_started_failures,
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
  form_complete_checks <- .miss_build_form_complete_check_rows(
    expected = expected,
    form = form
  )
  form_complete_failures <- form_complete_checks[
    !form_complete_checks$form_complete,
    ,
    drop = FALSE
  ]
  fields_complete_failures <- expected[
    !expected$field_complete,
    ,
    drop = FALSE
  ]
  validation_rows <- dplyr::bind_rows(
    event_checks,
    repeat_checks,
    form_checks,
    form_complete_checks,
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
    validation_scope = "event_row_exists",
    column = "event_row_exists",
    step_id = paste0(form, "_event_row_exists"),
    label = "Event row for record exists",
    keep_zero = TRUE
  )

  if (nrow(repeat_checks) > 0) {
    agent <- .miss_add_validation_step(
      agent = agent,
      rows = repeat_checks,
      validation_scope = "repeat_instance_row_exists",
      column = "repeat_instance_row_exists",
      step_id = paste0(form, "_repeat_instance_row_exists"),
      label = "Repeat instance row for record exists"
    )
  }

  agent <- .miss_add_validation_step(
    agent = agent,
    rows = form_checks,
    validation_scope = "form_started",
    column = "form_started",
    step_id = paste0(form, "_form_started"),
    label = "Form started",
    keep_zero = TRUE
  )

  if (nrow(form_complete_checks) > 0) {
    agent <- .miss_add_validation_step(
      agent = agent,
      rows = form_complete_checks,
      validation_scope = "form_complete",
      column = "form_complete",
      step_id = paste0(form, "_form_complete"),
      label = "Form complete"
    )
  }

  agent <- .miss_add_validation_step(
    agent = agent,
    rows = expected,
    validation_scope = "fields_complete",
    column = "field_complete",
    step_id = paste0(form, "_fields_complete"),
    label = "Fields complete",
    keep_zero = TRUE
  ) |>
    pointblank::interrogate(extract_failed = TRUE, progress = FALSE)
  agent <- .miss_annotate_agent_validation_set(
    agent = agent,
    validation_rows = validation_rows,
    form = form,
    form_label = project$form_label
  )

  missing <- .miss_get_failed_rows(agent)

  out <- list(
    agent = agent,
    missing = missing,
    validation_rows = validation_rows,
    event_row_exists_checks = event_checks,
    event_row_exists_failures = event_row_exists_failures,
    repeat_instance_row_exists_checks = repeat_checks,
    repeat_instance_row_exists_failures = repeat_instance_row_exists_failures,
    form_started_checks = form_checks,
    form_started_failures = form_started_failures,
    form_complete_checks = form_complete_checks,
    form_complete_failures = form_complete_failures,
    fields_complete_checks = expected,
    fields_complete_failures = fields_complete_failures,
    field_plan = field_plan,
    required_fields = required_fields,
    events = project$events %||% character(),
    instances = instances,
    ignored_fields = ignore_roots,
    ignored_ids = ignore_ids,
    project = project,
    form = form,
    form_label = project$form_label,
    id_col = project$id_col,
    system_fields = project$system_fields
  )
  class(out) <- "redcapmissing"
  out
}
