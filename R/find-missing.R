#' Build a branching-aware REDCap missingness report
#'
#' @description
#' `find_missing()` checks one or more REDCap instruments/forms for missingness
#' using the canonical validation model returned by [registry()]. Expected data
#' are derived from REDCap project metadata and structure supplied through a
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
#' offered. It inspects available connection methods such as `rcon$metadata()`,
#' `rcon$instruments()`, `rcon$events()`, `rcon$mapping()` /
#' `rcon$mappings()`, `rcon$repeatInstrumentEvent()`, and
#' `rcon$projectInformation()` when present.
#'
#' Checkbox fields are treated as one REDCap root field. A checkbox root is
#' considered answered when at least one exported child column
#' (`field___choice`) is selected. Unselected sibling checkboxes are not flagged
#' as missing once the root question has at least one selected child.
#'
#' Validation is performed by native `redcapmissing` code. The package builds
#' validation rows with:
#' \describe{
#'   \item{`validation_level`}{The emitted context label. Event/form checks use
#'     `"event:form"` for non-repeating contexts and
#'     `"event:form:instance"` for repeat-instance contexts; event rollups use
#'     `"event"`.}
#'   \item{`validation_check`}{One of `"event-row-started"`,
#'     `"instance-row-started"`, `"form-started"`, `"form-complete"`, or
#'     `"field-complete"`, or `"event-complete"`.}
#'   \item{`validation_check_type`}{`"on-route"` or `"detour"`.}
#'   \item{`validation_passed`}{Whether that row passed its validation check.}
#' }
#'
#' The checks are assessed in registry order. Failed `"on-route"` checks remove
#' the record/event/repeat/form context from every downstream assessment,
#' regardless of the downstream validation level or check type. `form-complete`
#' is a `"detour"` check: it reports whether all form fields are complete,
#' but a failed `form-complete` context still flows into `field-complete`.
#' `event-complete` is an event-level `"detour"` rollup with the display
#' meaning "all forms on event complete": it checks only downstream-gating
#' on-route results for each record/event context and does not count
#' `form-complete` because `form-complete` is also a detour.
#'
#' By default, the returned report keeps only compact summaries, failed rows,
#' project specification metadata, and diagnostics. Set `details = TRUE` when
#' row-level check tables are needed for debugging or regression tests.
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
#' @param forms Required character vector of REDCap form/instrument names to
#'   assess. No default is supplied so callers must choose the form or forms
#'   deliberately. Duplicate form names are not allowed.
#' @param events Character vector of REDCap unique event names to apply to all
#'   requested forms, a named list of event vectors by form, or `NULL`. When a
#'   form is offered on multiple REDCap events, this argument can restrict the
#'   report to a selected subset of those events. If `NULL`, all offered events
#'   are assessed. Named lists may omit forms or use `NULL` entries to request a
#'   form's default offered events. If a form is offered on only one event,
#'   supplied event values are ignored for that form. If a form is regular on
#'   some events and repeating on others, `events` also determines whether
#'   repeat-instance logic is activated. Defaults to `NULL`.
#' @param records Named list of REDCap record ID vectors by raw
#'   `redcap_event_name`, or `NULL`. Use it to restrict selected events to a
#'   specific set of record IDs. Events not named in `records` are not
#'   restricted by this argument: every applicable non-ignored record in `data`
#'   is considered for those events. A record with no exported row for an event
#'   fails `event-row-started` and is not assessed by downstream form or field
#'   checks for that event. Empty or blank-only entries are ignored and behave
#'   like omitted events. Supplied IDs are normalized to character values and
#'   are not checked through a live REDCap record export. IDs not present in
#'   `data` are allowed and can create upstream row-started failures. When both
#'   `events` and `records` are supplied, `events` selects the form-event scope
#'   first and `records` narrows record eligibility inside those events. For
#'   repeating contexts, event-level eligibility applies before `instances`
#'   expands the expected repeat-instance IDs. Defaults to `NULL`, which means
#'   `records` does not restrict any event.
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
#' @param instances Positive whole-number scalar count, vector of positive
#'   whole-number repeat-instance IDs, named list by form, or `NULL`. When a
#'   requested form is assessed in a REDCap repeating event or as a repeating
#'   instrument, a scalar such as `2L` checks that repeat instances `1` and `2`
#'   exist. A vector with length greater than one, such as `c(2L, 3L)`, checks
#'   exactly those repeat-instance IDs. Named lists may omit forms or use `NULL`
#'   entries to request defaults. If a requested form repeats and no instance
#'   setting is supplied for it, the function warns once and assumes instance
#'   `1`. Global `instances` values apply only to forms with requested repeating
#'   contexts; named non-`NULL` entries for non-repeating requested contexts
#'   error.
#' @param details Logical scalar. When `FALSE`, the report omits full
#'   validation-row tables and check-specific row tables. When `TRUE`, these
#'   row-level tables are stored under `report$details`. Defaults to `FALSE`.
#' @param progress Logical scalar. When `TRUE`, line-based progress messages
#'   are emitted with `cli::cat_line()` as each requested form starts and
#'   finishes, followed by a final overall completion line. Defaults to
#'   `interactive()`.
#'
#' @return A list with class `"redcapmissing"` containing:
#' \describe{
#'   \item{`summary`}{Compact validation summary rows used by `tidy()`,
#'     including assessed, passed, failed, pass-rate, and fail-rate columns.}
#'   \item{`missing`}{Failed validation rows keyed by generic native
#'     `validation_step` and `validation_row_id` identifiers plus REDCap record,
#'     event, repeat, form, and field context.}
#'   \item{`spec`}{Normalized report context, including requested forms,
#'     events, labels, eligible records, instances, ignored fields/IDs, REDCap
#'     ID column, system fields, project cache, and total eligible record count.}
#'   \item{`diagnostics`}{Timing and row-count metadata useful for benchmarking
#'     and troubleshooting.}
#'   \item{`details`}{`NULL` by default. When `details = TRUE`, a list with
#'     `validation_rows`, `checks`, and `failures` row tables.}
#' }
#'
#' @seealso [registry()], [redcapAPI::redcapConnection()],
#'   [redcapAPI::exportRecordsTyped()]
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
#'   forms = "baseline_form",
#'   events = c("baseline_event", "followup_event"),
#'   ignore_fields = c("status_flag", "screening_code")
#' )
#'
#' tidy(baseline_missing)
#' baseline_missing$missing
#'
#' detailed_missing <- find_missing(
#'   data = records,
#'   rcon = rcon,
#'   forms = "baseline_form",
#'   details = TRUE
#' )
#'
#' detailed_missing$details$checks[["field-complete"]]
#'
#' repeat_missing <- find_missing(
#'   data = records,
#'   rcon = rcon,
#'   forms = "repeat_form",
#'   instances = 2L
#' )
#'
#' multi_form_missing <- find_missing(
#'   data = records,
#'   rcon = rcon,
#'   forms = c("imaging", "demographics"),
#'   events = list(
#'     imaging = c("event_2_arm_1", "event_3_arm_1")
#'   )
#' )
#'
#' staged_missing <- find_missing(
#'   data = records,
#'   rcon = rcon,
#'   forms = c("surgery", "demographics"),
#'   records = list(
#'     event_1_arm_1 = c("record_a", "record_b"),
#'     event_2_arm_1 = c("record_a", "record_b"),
#'     event_3_arm_1 = "record_b"
#'   )
#' )
#' }
#'
#' @export
find_missing <- function(
  data,
  rcon,
  forms,
  events = NULL,
  records = NULL,
  required_fields = TRUE,
  ignore_fields = NULL,
  ignore_ids = NULL,
  exclude_types = "descriptive",
  instances = NULL,
  details = FALSE,
  progress = interactive()
) {
  call_arg_names <- names(as.list(sys.call()))[-1]
  if ("form" %in% call_arg_names) {
    stop("unused argument `form`; use `forms`.", call. = FALSE)
  }
  if (missing(forms)) {
    stop("Provide `forms`; this argument has no default.", call. = FALSE)
  }
  if (
    !is.logical(required_fields) ||
      length(required_fields) != 1 ||
      is.na(required_fields)
  ) {
    stop("`required_fields` must be TRUE or FALSE.", call. = FALSE)
  }
  if (
    !is.logical(details) ||
      length(details) != 1 ||
      is.na(details)
  ) {
    stop("`details` must be TRUE or FALSE.", call. = FALSE)
  }
  if (
    !is.logical(progress) ||
      length(progress) != 1 ||
      is.na(progress)
  ) {
    stop("`progress` must be TRUE or FALSE.", call. = FALSE)
  }

  report_started_at <- Sys.time()
  forms <- .miss_resolve_forms(forms)
  event_settings <- .miss_resolve_form_events_arg(
    events = events,
    forms = forms
  )
  instance_settings <- .miss_resolve_form_instances_arg(
    instances = instances,
    forms = forms
  )

  # Normalize inputs and derive project context from the redcapAPI connection.
  record_data <- tibble::as_tibble(data)
  all_records <- record_data

  meta <- .miss_get_metadata(rcon)
  .miss_check_metadata(meta)
  id_col <- meta$field_name[[1]]
  if (!id_col %in% names(record_data)) {
    stop(
      "The REDCap record identifier column `",
      id_col,
      "` is not present in `data`.",
      call. = FALSE
    )
  }
  project_cache <- .miss_get_project_cache(rcon = rcon)

  # Remove caller-specified records before branch evaluation or validation.
  ignore_ids <- unique(.miss_chr_vec(ignore_ids %||% character()))
  if (length(ignore_ids) > 0) {
    record_data <- record_data[
      !.miss_chr_vec(record_data[[id_col]]) %in% ignore_ids,
      ,
      drop = FALSE
    ]
    all_records <- all_records[
      !.miss_chr_vec(all_records[[id_col]]) %in% ignore_ids,
      ,
      drop = FALSE
    ]
  }
  eligible_records <- .miss_resolve_event_records_arg(
    records = records,
    valid_events = .miss_get_project_event_names(project_cache = project_cache),
    ignore_ids = ignore_ids
  )

  form_reports <- vector("list", length(forms))
  names(form_reports) <- forms
  for (form_i in seq_along(forms)) {
    form <- forms[[form_i]]
    .miss_cli_form_progress(
      form = form,
      form_percent = 0,
      overall_percent = .miss_percent((form_i - 1) / length(forms)),
      progress = progress
    )
    form_reports[[form]] <- .miss_build_form_report(
      records = record_data,
      all_records = all_records,
      meta = meta,
      rcon = rcon,
      project_cache = project_cache,
      form = form,
      events = event_settings$values[[form]],
      eligible_records = eligible_records,
      required_fields = required_fields,
      ignore_fields = ignore_fields,
      exclude_types = exclude_types,
      instances = instance_settings$values[[form]],
      instances_explicit = instance_settings$explicit[[form]],
      details = details
    )
    .miss_cli_form_progress(
      form = form,
      form_percent = 100,
      overall_percent = .miss_percent(form_i / length(forms)),
      progress = progress
    )
  }

  defaulted_instance_forms <- forms[vapply(
    form_reports,
    `[[`,
    logical(1),
    "instances_defaulted"
  )]
  if (length(defaulted_instance_forms) > 0) {
    warning(
      "`instances` was not provided for repeating form(s): ",
      paste(defaulted_instance_forms, collapse = ", "),
      ". Assuming instance 1 for the requested repeating contexts.",
      call. = FALSE
    )
  }

  form_labels <- stats::setNames(
    vapply(form_reports, `[[`, character(1), "form_label"),
    forms
  )
  event_labels <- .miss_combine_event_labels(form_reports)
  event_status <- .miss_bind_report_component(
    reports = form_reports,
    component = "event_status"
  )
  event_complete_checks <- .miss_build_event_complete_check_rows_from_status(
    event_status
  )
  summary <- .miss_build_validation_summary(
    form_reports = form_reports,
    event_complete_checks = event_complete_checks
  )
  form_validation_rows <- .miss_count_form_validation_rows(form_reports)
  validation_row_count <- form_validation_rows + nrow(event_complete_checks)
  missing <- .miss_build_report_missing_rows(
    form_reports = form_reports,
    event_complete_checks = event_complete_checks,
    form_validation_rows = form_validation_rows
  )
  ignored_fields <- unique(unlist(.miss_named_report_component(
    form_reports,
    "ignored_fields"
  ), use.names = FALSE))
  ignored_fields <- ignored_fields %||% character()
  report_finished_at <- Sys.time()
  .miss_cli_overall_progress(progress = progress)

  details_out <- if (isTRUE(details)) {
    validation_rows <- .miss_bind_report_component(
      reports = form_reports,
      component = "validation_rows"
    )
    validation_rows <- dplyr::bind_rows(
      validation_rows,
      event_complete_checks
    )
    .miss_build_report_details(
      validation_rows = validation_rows
    )
  } else {
    NULL
  }
  spec <- .miss_build_report_spec(
    form_reports = form_reports,
    required_fields = required_fields,
    eligible_records = eligible_records,
    ignored_fields = ignored_fields,
    ignored_ids = ignore_ids,
    forms = forms,
    form_labels = form_labels,
    event_labels = event_labels,
    id_col = id_col,
    total_n = .miss_count_form_report_records(form_reports)
  )
  diagnostics <- .miss_build_report_diagnostics(
    started_at = report_started_at,
    finished_at = report_finished_at,
    form_reports = form_reports,
    validation_row_count = validation_row_count,
    summary = summary,
    missing = missing
  )
  out <- list(
    summary = summary,
    missing = missing,
    spec = spec,
    diagnostics = diagnostics,
    details = details_out
  )
  class(out) <- "redcapmissing"
  out
}

# Internal helpers ---------------------------------------------------------

.miss_build_form_report <- function(
  records,
  all_records,
  meta,
  rcon,
  project_cache,
  form,
  events,
  eligible_records,
  required_fields,
  ignore_fields,
  exclude_types,
  instances,
  instances_explicit,
  details
) {

  project <- .miss_get_project(
    rcon = rcon,
    meta = meta,
    form = form,
    project_cache = project_cache
  )
  project <- .miss_resolve_events(
    project = project,
    events = events,
    form = form
  )
  resolved_instances <- .miss_resolve_instances(
    instances = instances,
    project = project,
    form = form,
    explicit = instances_explicit
  )
  instances <- resolved_instances$values

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
  records <- .miss_filter_eligible_event_records(
    records = records,
    project = project,
    eligible_records = eligible_records
  )

  event_checks <- .miss_build_event_check_rows(
    records = all_records,
    form_records = records,
    project = project,
    form = form,
    eligible_records = eligible_records
  )
  event_checks <- .miss_add_validation_context(event_checks)
  event_row_started_failures <- event_checks[
    !event_checks$validation_passed,
    ,
    drop = FALSE
  ]

  repeat_checks <- .miss_build_repeat_check_rows(
    records = all_records,
    form_records = records,
    project = project,
    form = form,
    instances = instances,
    eligible_records = eligible_records
  )
  repeat_checks <- .miss_add_validation_context(repeat_checks)
  instance_row_started_failures <- repeat_checks[
    !repeat_checks$validation_passed,
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
    !form_checks$validation_passed,
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
    !form_complete_checks$validation_passed,
    ,
    drop = FALSE
  ]
  field_complete_failures <- expected[
    !expected$validation_passed,
    ,
    drop = FALSE
  ]

  check_rows <- list(
    event_row_started_checks = event_checks,
    instance_row_started_checks = repeat_checks,
    form_started_checks = form_checks,
    form_complete_checks = form_complete_checks,
    field_complete_checks = expected
  )
  row_counts <- vapply(check_rows, nrow, integer(1))

  out <- list(
    summary = .miss_build_form_validation_summary(
      c(check_rows, list(form = form))
    ),
    missing = .miss_build_form_missing_rows(check_rows),
    row_counts = row_counts,
    event_status = .miss_build_on_route_event_status(check_rows),
    record_ids = .miss_validation_record_ids(check_rows),
    events = project$events %||% character(),
    event_labels = project$event_labels,
    instances = instances,
    instances_defaulted = resolved_instances$defaulted,
    ignored_fields = ignore_roots,
    project = project,
    form = form,
    form_label = project$form_label,
    id_col = project$id_col,
    system_fields = project$system_fields
  )

  if (isTRUE(details)) {
    validation_rows <- dplyr::bind_rows(check_rows)
    validation_rows <- .miss_add_validation_context(validation_rows)
    out <- c(out, list(
      validation_rows = validation_rows,
    event_row_started_checks = event_checks,
    event_row_started_failures = event_row_started_failures,
    instance_row_started_checks = repeat_checks,
    instance_row_started_failures = instance_row_started_failures,
    form_started_checks = form_checks,
    form_started_failures = form_started_failures,
    form_complete_checks = form_complete_checks,
    form_complete_failures = form_complete_failures,
    field_complete_checks = expected,
    field_complete_failures = field_complete_failures,
      field_plan = field_plan
    ))
  }

  out
}

.miss_build_validation_summary <- function(form_reports, event_complete_checks) {
  summary_rows <- lapply(form_reports, `[[`, "summary")

  if (nrow(event_complete_checks) > 0) {
    summary_rows[[length(summary_rows) + 1L]] <- .miss_summarise_validation_rows(
      rows = event_complete_checks,
      validation_check = "event-complete",
      form = "",
      keep_zero = FALSE
    )
  }

  out <- dplyr::bind_rows(summary_rows)
  if (nrow(out) == 0) {
    return(.miss_empty_validation_summary())
  }

  out
}

.miss_build_form_validation_summary <- function(form_report) {
  registry <- .redcapmissing_registry_data()
  summary_rows <- list()
  row_i <- 0L
  form_registry <- registry[
    registry$validation_check != "event-complete",
    ,
    drop = FALSE
  ]

  for (row in seq_len(nrow(form_registry))) {
    check <- form_registry[row, , drop = FALSE]
    component <- paste0(check$component_stem, "_checks")
    rows <- form_report[[component]] %||% .miss_empty_expected()
    keep_zero <- .miss_keep_zero_validation_step(
      validation_check = check$validation_check,
      form_report = form_report
    )
    if (nrow(rows) == 0 && !isTRUE(keep_zero)) {
      next
    }
    row_i <- row_i + 1L
    summary_rows[[row_i]] <- .miss_summarise_validation_rows(
      rows = rows,
      validation_check = check$validation_check,
      form = form_report$form,
      keep_zero = keep_zero
    )
  }

  out <- dplyr::bind_rows(summary_rows)
  if (nrow(out) == 0) {
    return(.miss_empty_validation_summary())
  }
  out
}

.miss_summarise_validation_rows <- function(
  rows,
  validation_check,
  form,
  keep_zero = FALSE
) {
  if (nrow(rows) == 0) {
    if (!isTRUE(keep_zero)) {
      return(.miss_empty_validation_summary())
    }
    return(.miss_zero_validation_summary(
      validation_check = validation_check,
      form = form
    ))
  }

  rows <- .miss_add_validation_context(rows)
  rows$form <- .miss_chr_vec(rows$form)
  rows$validation_check <- .miss_chr_vec(rows$validation_check)
  rows$redcap_event_name <- .miss_chr_vec(rows$redcap_event_name)
  rows$redcap_repeat_instrument <- .miss_chr_vec(rows$redcap_repeat_instrument)
  rows$redcap_repeat_instance <- .miss_chr_vec(rows$redcap_repeat_instance)
  rows$validation_context <- .miss_chr_vec(rows$validation_context)

  context_cols <- c(
    "redcap_event_name",
    "form",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "validation_context"
  )
  contexts <- rows[, context_cols, drop = FALSE]
  context_key <- paste(
    .miss_key_part(contexts$redcap_event_name),
    .miss_key_part(contexts$form),
    .miss_key_part(contexts$redcap_repeat_instrument),
    .miss_key_part(contexts$redcap_repeat_instance),
    .miss_key_part(contexts$validation_context),
    sep = "\r"
  )
  unique_key <- unique(context_key)
  group_id <- match(context_key, unique_key)
  contexts <- contexts[match(unique_key, context_key), , drop = FALSE]

  assessed <- tabulate(group_id, nbins = length(unique_key))
  passed <- as.integer(rowsum(
    as.integer(rows$validation_passed %in% TRUE),
    group_id,
    reorder = FALSE
  )[, 1])
  failed <- assessed - passed
  pass_rate <- ifelse(assessed > 0, passed / assessed, 0)
  fail_rate <- ifelse(assessed > 0, failed / assessed, 0)
  registry_row <- .redcapmissing_registry_row(validation_check)

  tibble::tibble(
    redcap_event_name = contexts$redcap_event_name,
    form = contexts$form,
    redcap_repeat_instrument = contexts$redcap_repeat_instrument,
    redcap_repeat_instance = contexts$redcap_repeat_instance,
    validation_level = .redcapmissing_context_validation_level(
      validation_check = validation_check,
      repeat_instance = contexts$redcap_repeat_instance
    ),
    validation_check = validation_check,
    validation_label = registry_row$validation_label,
    validation_context = contexts$validation_context,
    validation_step = .miss_step_id(contexts$form, validation_check),
    assessed = assessed,
    passed = passed,
    failed = failed,
    pass_rate = pass_rate,
    fail_rate = fail_rate,
    validation_check_type = registry_row$validation_check_type
  )
}

.miss_empty_validation_summary <- function() {
  tibble::tibble(
    redcap_event_name = character(),
    form = character(),
    redcap_repeat_instrument = character(),
    redcap_repeat_instance = character(),
    validation_level = character(),
    validation_check = character(),
    validation_label = character(),
    validation_context = character(),
    validation_step = character(),
    assessed = integer(),
    passed = integer(),
    failed = integer(),
    pass_rate = numeric(),
    fail_rate = numeric(),
    validation_check_type = character()
  )
}

.miss_zero_validation_summary <- function(validation_check, form) {
  registry_row <- .redcapmissing_registry_row(validation_check)
  tibble::tibble(
    redcap_event_name = "",
    form = form,
    redcap_repeat_instrument = "",
    redcap_repeat_instance = "",
    validation_level = .redcapmissing_context_validation_level(
      validation_check = validation_check,
      repeat_instance = ""
    ),
    validation_check = validation_check,
    validation_label = registry_row$validation_label,
    validation_context = "overall",
    validation_step = .miss_step_id(form, validation_check),
    assessed = 0L,
    passed = 0L,
    failed = 0L,
    pass_rate = 0,
    fail_rate = 0,
    validation_check_type = registry_row$validation_check_type
  )
}

.miss_build_missing_rows <- function(validation_rows) {
  .miss_build_component_missing_rows(validation_rows, validation_row_offset = 0L)
}

.miss_build_form_missing_rows <- function(check_rows) {
  pieces <- vector("list", length(check_rows))
  offset <- 0L
  for (i in seq_along(check_rows)) {
    rows <- check_rows[[i]]
    pieces[[i]] <- .miss_build_component_missing_rows(
      rows,
      validation_row_offset = offset
    )
    offset <- offset + nrow(rows)
  }

  out <- dplyr::bind_rows(pieces)
  if (nrow(out) == 0) {
    return(.miss_empty_missing_rows())
  }
  out
}

.miss_build_report_missing_rows <- function(
  form_reports,
  event_complete_checks,
  form_validation_rows
) {
  pieces <- vector("list", length(form_reports) + 1L)
  offset <- 0L
  for (i in seq_along(form_reports)) {
    form_missing <- form_reports[[i]]$missing %||% .miss_empty_missing_rows()
    pieces[[i]] <- .miss_offset_missing_rows(form_missing, offset)
    offset <- offset + sum(form_reports[[i]]$row_counts)
  }
  pieces[[length(pieces)]] <- .miss_build_component_missing_rows(
    event_complete_checks,
    validation_row_offset = form_validation_rows
  )

  out <- dplyr::bind_rows(pieces)
  if (nrow(out) == 0) {
    return(.miss_empty_missing_rows())
  }
  out
}

.miss_validation_record_ids <- function(check_rows) {
  rows <- dplyr::bind_rows(check_rows)
  if (nrow(rows) == 0 || !"record_id" %in% names(rows)) {
    return(character())
  }

  record_ids <- unique(.miss_chr_vec(rows$record_id))
  record_ids[!.miss_is_blank_vec(record_ids)]
}

.miss_build_component_missing_rows <- function(
  validation_rows,
  validation_row_offset = 0L
) {
  validation_rows <- .miss_add_validation_context(validation_rows)
  if (nrow(validation_rows) == 0) {
    return(.miss_empty_missing_rows())
  }

  validation_rows$validation_row_id <-
    validation_row_offset + seq_len(nrow(validation_rows))
  out <- validation_rows[
    !(validation_rows$validation_passed %in% TRUE),
    ,
    drop = FALSE
  ]
  if (nrow(out) == 0) {
    return(.miss_empty_missing_rows())
  }

  out$validation_step <- .miss_step_id(out$form, out$validation_check)
  .miss_select_missing_rows(out)
}

.miss_offset_missing_rows <- function(rows, offset) {
  if (nrow(rows) == 0) {
    return(rows)
  }
  rows$validation_row_id <- rows$validation_row_id + offset
  rows
}

.miss_select_missing_rows <- function(rows) {
  select_cols <- c(
    "validation_step",
    "validation_row_id",
    "record_id",
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "validation_context",
    "form",
    "validation_level",
    "validation_check_type",
    "validation_check",
    "validation_label",
    "validation_passed",
    "field_name",
    "field_label",
    "field_type",
    "branching_logic",
    "branch_satisfied",
    "value_summary",
    "export_fields"
  )

  rows |>
    dplyr::select(dplyr::all_of(select_cols), dplyr::everything())
}

.miss_empty_missing_rows <- function() {
  tibble::tibble(
    validation_step = character(),
    validation_row_id = integer(),
    record_id = character(),
    redcap_event_name = character(),
    redcap_repeat_instrument = character(),
    redcap_repeat_instance = character(),
    validation_context = character(),
    form = character(),
    validation_level = character(),
    validation_check_type = character(),
    validation_check = character(),
    validation_label = character(),
    validation_passed = logical(),
    field_name = character(),
    field_label = character(),
    field_type = character(),
    branching_logic = character(),
    branch_satisfied = logical(),
    value_summary = character(),
    export_fields = character()
  )
}

.miss_build_report_details <- function(validation_rows) {
  check_names <- .redcapmissing_validation_checks()
  checks <- stats::setNames(vector("list", length(check_names)), check_names)
  failures <- stats::setNames(vector("list", length(check_names)), check_names)
  for (validation_check in check_names) {
    rows <- validation_rows[
      validation_rows$validation_check == validation_check,
      ,
      drop = FALSE
    ]
    checks[[validation_check]] <- rows
    failures[[validation_check]] <- rows[
      !rows$validation_passed,
      ,
      drop = FALSE
    ]
  }

  list(
    validation_rows = validation_rows,
    checks = checks,
    failures = failures
  )
}

.miss_build_report_spec <- function(
  form_reports,
  required_fields,
  eligible_records,
  ignored_fields,
  ignored_ids,
  forms,
  form_labels,
  event_labels,
  id_col,
  total_n
) {
  list(
    required_fields = required_fields,
    forms = forms,
    form_labels = form_labels,
    events = .miss_named_report_component(form_reports, "events"),
    event_labels = event_labels,
    eligible_records = eligible_records,
    instances = .miss_named_report_component(form_reports, "instances"),
    ignored_fields = ignored_fields,
    ignored_ids = ignored_ids,
    project = .miss_named_report_component(form_reports, "project"),
    id_col = id_col,
    system_fields = form_reports[[1]]$system_fields,
    total_n = total_n
  )
}

.miss_count_form_report_records <- function(form_reports) {
  record_ids <- unique(unlist(
    lapply(form_reports, `[[`, "record_ids"),
    use.names = FALSE
  ))
  record_ids <- record_ids[!.miss_is_blank_vec(record_ids)]
  length(record_ids)
}

.miss_count_form_validation_rows <- function(form_reports) {
  sum(vapply(
    form_reports,
    function(form_report) sum(form_report$row_counts),
    integer(1)
  ))
}

.miss_build_report_diagnostics <- function(
  started_at,
  finished_at,
  form_reports,
  validation_row_count,
  summary,
  missing
) {
  list(
    started_at = started_at,
    finished_at = finished_at,
    elapsed_seconds = unname(as.numeric(
      difftime(finished_at, started_at, units = "secs")
    )),
    forms_processed = length(form_reports),
    validation_rows = validation_row_count,
    summary_rows = nrow(summary),
    missing_rows = nrow(missing)
  )
}

.miss_keep_zero_validation_step <- function(validation_check, form_report) {
  keep_zero_checks <- c(
    "event-row-started",
    "form-started",
    "field-complete"
  )
  if (!validation_check %in% keep_zero_checks) {
    return(FALSE)
  }

  if (
    identical(validation_check, "event-row-started") &&
      nrow(form_report$instance_row_started_checks) > 0
  ) {
    return(FALSE)
  }

  downstream_checks <- c("form-started", "field-complete")
  if (
    validation_check %in% downstream_checks &&
      .miss_form_report_has_row_context_gates(form_report)
  ) {
    return(FALSE)
  }

  TRUE
}

.miss_form_report_has_row_context_gates <- function(form_report) {
  nrow(form_report$event_row_started_checks) > 0 ||
    nrow(form_report$instance_row_started_checks) > 0
}

.miss_resolve_forms <- function(forms) {
  if (!is.character(forms)) {
    stop("`forms` must be a character vector of REDCap form names.", call. = FALSE)
  }

  forms <- .miss_chr_vec(forms)
  forms <- forms[!.miss_is_blank_vec(forms)]
  if (length(forms) == 0) {
    stop("`forms` must contain at least one non-blank REDCap form name.", call. = FALSE)
  }
  duplicated_forms <- unique(forms[duplicated(forms)])
  if (length(duplicated_forms) > 0) {
    stop(
      "`forms` must not contain duplicate form names. Duplicate form(s): ",
      paste(duplicated_forms, collapse = ", "),
      call. = FALSE
    )
  }

  forms
}

.miss_resolve_form_events_arg <- function(events, forms) {
  values <- stats::setNames(vector("list", length(forms)), forms)
  if (is.null(events)) {
    return(list(values = values))
  }

  if (is.list(events)) {
    .miss_check_named_setting_list(events, forms = forms, arg = "events")
    for (form in intersect(names(events), forms)) {
      value <- events[[form]]
      .miss_check_nonempty_setting_value(value, arg = "events", form = form)
      if (!is.null(value) && !is.character(value)) {
        stop(
          "`events` list entry `",
          form,
          "` must be NULL or a character vector of REDCap event names.",
          call. = FALSE
        )
      }
      values[[form]] <- value
    }
    return(list(values = values))
  }

  .miss_check_nonempty_setting_value(events, arg = "events")
  if (!is.character(events)) {
    stop(
      "`events` must be NULL, a character vector of REDCap event names, ",
      "or a named list by form.",
      call. = FALSE
    )
  }

  for (form in forms) {
    values[[form]] <- events
  }
  list(values = values)
}

.miss_resolve_form_instances_arg <- function(instances, forms) {
  values <- stats::setNames(vector("list", length(forms)), forms)
  explicit <- stats::setNames(as.list(rep(FALSE, length(forms))), forms)
  if (is.null(instances)) {
    return(list(values = values, explicit = explicit))
  }

  if (is.list(instances)) {
    .miss_check_named_setting_list(instances, forms = forms, arg = "instances")
    for (form in intersect(names(instances), forms)) {
      value <- instances[[form]]
      .miss_check_nonempty_setting_value(value, arg = "instances", form = form)
      if (!is.null(value)) {
        .miss_expand_instances(value)
      }
      values[[form]] <- value
      explicit[[form]] <- !is.null(value)
    }
    return(list(values = values, explicit = explicit))
  }

  .miss_check_nonempty_setting_value(instances, arg = "instances")
  .miss_expand_instances(instances)
  for (form in forms) {
    values[[form]] <- instances
  }
  list(values = values, explicit = explicit)
}

.miss_resolve_event_records_arg <- function(records, valid_events, ignore_ids) {
  if (is.null(records)) {
    return(list())
  }
  if (!is.list(records) || is.data.frame(records)) {
    stop(
      "`records` must be NULL or a named list of REDCap record ID vectors ",
      "by `redcap_event_name`.",
      call. = FALSE
    )
  }

  record_event_names <- names(records)
  if (
    is.null(record_event_names) ||
      length(record_event_names) != length(records) ||
      any(.miss_is_blank_vec(record_event_names))
  ) {
    stop(
      "`records` lists must be named by raw REDCap `redcap_event_name`.",
      call. = FALSE
    )
  }

  duplicated_events <- unique(record_event_names[duplicated(record_event_names)])
  if (length(duplicated_events) > 0) {
    stop(
      "`records` list names must not be duplicated. Duplicate event name(s): ",
      paste(duplicated_events, collapse = ", "),
      call. = FALSE
    )
  }

  valid_events <- unique(.miss_chr_vec(valid_events))
  valid_events <- valid_events[!.miss_is_blank_vec(valid_events)]
  if (length(valid_events) == 0) {
    stop(
      "`records` requires REDCap event metadata from `rcon`.",
      call. = FALSE
    )
  }

  unknown_events <- setdiff(record_event_names, valid_events)
  if (length(unknown_events) > 0) {
    stop(
      "`records` list names must be valid REDCap event names from `rcon`. ",
      "Unknown event(s): ",
      paste(unknown_events, collapse = ", "),
      call. = FALSE
    )
  }

  ignore_ids <- unique(.miss_chr_vec(ignore_ids %||% character()))
  out <- list()
  for (event in record_event_names) {
    value <- records[[event]]
    if (is.null(value)) {
      next
    }
    if (is.list(value) || is.data.frame(value)) {
      stop(
        "`records` list entry `",
        event,
        "` must be a vector of REDCap record IDs.",
        call. = FALSE
      )
    }

    record_ids <- unique(.miss_chr_vec(value))
    record_ids <- record_ids[!.miss_is_blank_vec(record_ids)]
    record_ids <- setdiff(record_ids, ignore_ids)
    if (length(record_ids) == 0) {
      next
    }

    out[[event]] <- record_ids
  }

  out
}

.miss_check_named_setting_list <- function(x, forms, arg) {
  setting_names <- names(x)
  if (
    is.null(setting_names) ||
      length(setting_names) != length(x) ||
      any(.miss_is_blank_vec(setting_names))
  ) {
    stop(
      "`",
      arg,
      "` lists must be named by requested form.",
      call. = FALSE
    )
  }

  duplicated_names <- unique(setting_names[duplicated(setting_names)])
  if (length(duplicated_names) > 0) {
    stop(
      "`",
      arg,
      "` list names must not be duplicated. Duplicate name(s): ",
      paste(duplicated_names, collapse = ", "),
      call. = FALSE
    )
  }

  unknown_names <- setdiff(setting_names, forms)
  if (length(unknown_names) > 0) {
    stop(
      "`",
      arg,
      "` list names must match requested `forms`. Unknown name(s): ",
      paste(unknown_names, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(x)
}

.miss_check_nonempty_setting_value <- function(value, arg, form = NULL) {
  if (is.null(value)) {
    return(invisible(value))
  }

  if (length(value) == 0) {
    target <- if (is.null(form)) {
      paste0("`", arg, "`")
    } else {
      paste0("`", arg, "` list entry `", form, "`")
    }
    stop(
      target,
      " must not be an empty vector. Use NULL to request the default.",
      call. = FALSE
    )
  }

  invisible(value)
}

.miss_bind_report_component <- function(reports, component) {
  dplyr::bind_rows(lapply(reports, `[[`, component))
}

.miss_named_report_component <- function(reports, component) {
  stats::setNames(lapply(reports, `[[`, component), names(reports))
}

.miss_combine_event_labels <- function(reports) {
  labels <- character()
  for (report in reports) {
    report_labels <- report$event_labels %||% character()
    if (length(report_labels) == 0) {
      next
    }

    report_label_names <- names(report_labels) %||% rep("", length(report_labels))
    keep <- !.miss_is_blank_vec(report_label_names) &
      !.miss_is_blank_vec(report_labels)
    report_labels <- report_labels[keep]
    names(report_labels) <- report_label_names[keep]
    labels <- c(labels, report_labels[!names(report_labels) %in% names(labels)])
  }

  labels
}

.miss_cli_form_progress <- function(
  form,
  form_percent,
  overall_percent,
  progress
) {
  if (!isTRUE(progress)) {
    return(invisible(NULL))
  }

  cli::cat_line(
    paste0(
      "find_missing: form ",
      form,
      " ",
      form_percent,
      "% processed; overall ",
      overall_percent,
      "% processed"
    )
  )
  invisible(NULL)
}

.miss_cli_overall_progress <- function(progress) {
  if (!isTRUE(progress)) {
    return(invisible(NULL))
  }

  cli::cat_line("find_missing: overall 100% processed")
  invisible(NULL)
}

.miss_percent <- function(x) {
  as.integer(round(x * 100))
}

.miss_step_id <- function(form, validation_check) {
  form <- .miss_chr_vec(form %||% character())
  validation_check <- .miss_chr_vec(validation_check)
  n <- max(length(form), length(validation_check))
  if (n == 0) {
    return(character())
  }

  form <- rep(form, length.out = n)
  validation_check <- rep(validation_check, length.out = n)
  ifelse(
    .miss_is_blank_vec(form),
    validation_check,
    paste0(form, "_", validation_check)
  )
}
