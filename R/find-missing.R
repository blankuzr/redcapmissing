#' Build a branching-aware REDCap missingness report
#'
#' @description
#' `find_missing()` checks expected REDCap event, repeat, form, and field
#' contexts for missingness. Expectations come from project metadata supplied
#' through `rcon` and the validation taxonomy returned by [registry()].
#'
#' @details
#' A field is expected on rows where its requested form is offered and its
#' REDCap branching logic, if any, evaluates to `TRUE`. Blank and `NA` values
#' are missing.
#'
#' Checkbox fields are treated as one REDCap root field. A checkbox root is
#' answered when at least one exported child column (`field___choice`) is
#' selected; unselected siblings are not separately flagged.
#'
#' A valid requested form remains in the report when `required_fields`,
#' `exclude_types`, and `ignore_fields` leave no fields to assess.
#' `form-started` is still evaluated from the form's data-capturing metadata,
#' and each event/repeat context that reaches field assessment receives an
#' explicit `field-complete` summary with zero assessed, passed, and failed
#' fields. Contexts stopped by an upstream row-started or `form-started` gate
#' have no downstream `field-complete` summary.
#'
#' The checks are assessed in registry order. Failed checks remove the
#' record/event/repeat/form context from downstream assessment. The function
#' stops if no record IDs remain after filtering, except that explicit `records`
#' entries can declare expected contexts for IDs absent from `data`; those
#' contexts can fail a row-started check.
#'
#' Set `details = TRUE` to retain row-level check tables for troubleshooting.
#' See `vignette("redcapmissing", package = "redcapmissing")` for synthetic
#' event, record-eligibility, and repeat-instance workflows.
#'
#' When `verified` is supplied, every non-empty input row is validated before
#' username or query-status filtering. Project mismatches, unknown or ambiguous
#' fields/events, forms not offered at the mapped event, non-canonical
#' instances in repeating contexts, and invalid regular/repeating context
#' combinations are errors. When a field's context is regular and
#' `repeat_instrument` is missing, an upstream `instance` placeholder is
#' ignored. Repeating events still require a valid instance even though their
#' repeat instrument is missing.
#' Matching rows never create an assessment or bypass upstream gates; they can
#' only change an exact, otherwise-failing `field-complete` result to a pass.
#'
#' @param data A data frame or tibble of REDCap records, usually produced by
#'   `redcapAPI::exportRecordsTyped()`. For longitudinal projects, this should
#'   include the REDCap system column `redcap_event_name`. For repeating
#'   instruments/events, it should include `redcap_repeat_instrument` and
#'   `redcap_repeat_instance` when those columns are present in the export.
#' @param rcon A `redcapAPI::redcapConnection()` object, or an offline or
#'   preserved connection-like object with the same methods. It must provide
#'   project metadata through `rcon$metadata()` and instrument labels through
#'   `rcon$instruments()`; form-event mapping, repeat structure, and project
#'   information are used when available.
#' @param forms Required character vector of REDCap form/instrument names to
#'   assess. No default is supplied so callers must choose the form or forms
#'   deliberately. Duplicate form names are not allowed.
#' @param events A character vector of raw REDCap event names applied to all
#'   requested forms, a named list of event vectors by form, or `NULL`. `NULL`
#'   and omitted or `NULL` list entries select every event where the form is
#'   offered. List names must be unique requested forms, and values cannot be
#'   empty vectors. Values for a form offered on only one event are ignored. For
#'   forms that are regular on some events and repeating on others, the selected
#'   events determine whether repeat-instance checks apply. Defaults to `NULL`.
#' @param records A fully named list of record eligibility by raw
#'   `redcap_event_name`, or `NULL`. An event-level vector applies to every
#'   requested form and selected instance; an event/form vector applies only to
#'   that form; and an event/form/instance list applies only to the named
#'   positive whole-number instance. Names must be unique; event names must be
#'   known project events, and nested names must identify requested forms and
#'   valid contexts. Omitted contexts use eligibility derived from `data` and
#'   the other selection arguments. Each supplied value must contain only
#'   non-blank record IDs; `NULL`, empty, `NA`, and blank values error. IDs are
#'   normalized to character and may be absent from `data`, in which case they
#'   can produce row-started failures. Defaults to `NULL`, which applies no
#'   record restriction.
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
#' @param instances A positive whole-number scalar count, vector of positive
#'   whole-number instance IDs, named list by form, or `NULL`. A scalar such as
#'   `2L` requests instances `1` and `2`; a longer vector such as `c(2L, 3L)`
#'   requests exactly those IDs. List names must be unique requested forms;
#'   omitted or `NULL` entries use the default. A repeating form without an
#'   explicit setting warns once and uses instance `1`. Global values affect
#'   only requested repeating contexts; a named non-`NULL` setting for a form
#'   with no requested repeating context errors. Defaults to `NULL`.
#' @param details Logical scalar. When `FALSE`, the report omits full
#'   validation-row tables and check-specific row tables. When `TRUE`, these
#'   row-level tables are stored under `report$details`. Defaults to `FALSE`.
#' @param progress Logical scalar. When `TRUE`, displays processing progress by
#'   form; percentages do not represent data completeness. Defaults to
#'   `interactive()`.
#' @param verified `NULL`, or a data frame of REDCap data-quality issues to
#'   cross-reference. A non-empty data frame must contain the character columns
#'   `project_id`, `record`, `event_id`, `field_name`, `instance`,
#'   `current_query_status`, and `username`, plus `repeat_instrument`.
#'   `repeat_instrument` must be character when it contains a value, but an
#'   entirely missing column is accepted regardless of its R storage type and
#'   normalized internally. Extra columns are ignored, and `event_name` is
#'   neither required nor used. A zero-row logical-column template returned by
#'   [redcapAPI::exportDataQuality()] is also accepted. Every non-empty row is
#'   validated against the cached project ID, metadata, events, form-event
#'   mapping, and repeat structure before any user or status filtering.
#'   `instance` placeholders are ignored for schema-confirmed regular contexts;
#'   repeating instruments and repeating events require canonical positive
#'   integer instance strings. Defaults to `NULL`.
#' @param verified_user `NULL`, or one non-blank character username paired with
#'   `verified`. Matching is exact and case-sensitive. Only rows for this user
#'   whose `current_query_status` is exactly `"VERIFIED"` can override an
#'   otherwise-failing, already-assessed `field-complete` check at the exact
#'   record, event, repeat instrument, instance, and field context. If no input
#'   rows match the username, the function warns and proceeds unchanged.
#'   Defaults to `NULL`.
#'
#' @return A list with class `"redcapmissing"` containing:
#' \describe{
#'   \item{`summary`}{Compact validation summaries used by [get_summary()].}
#'   \item{`missing`}{Compact failed-validation rows used by [get_missing()].}
#'   \item{`spec`}{Normalized report configuration and REDCap context used by
#'     report methods. `spec$record_eligibility` contains every final assessed
#'     record/event/form/repeat-instance context, including when `records` is
#'     omitted.}
#'   \item{`diagnostics`}{Timing and row-count metadata, preserving the report
#'     start/finish, elapsed time, form count, validation-row count, summary-row
#'     count, and missing-row count. `diagnostics$stage_timings` is a tibble
#'     with `scope`, `form`, `stage`, and `elapsed_seconds`; report stages
#'     separate connection metadata/project-context access, plan compilation,
#'     URL construction, and report assembly, while form stages separate
#'     context, eligibility, row checks, blankness, form startedness, the four
#'     field partitions, and form assembly. `diagnostics$form_workload` is a
#'     per-form tibble with record/context/started-row counts, total and
#'     assessable field counts, the four field-partition counts, and validation
#'     rows. `diagnostics$verification` contains only the stable verification
#'     fields `enabled`, `verified_user`, `input_rows`, `user_rows`,
#'     `verified_rows`, and `overrides_applied`; the supplied data are not
#'     retained.}
#'   \item{`details`}{`NULL` by default. When `details = TRUE`, a list with
#'     `validation_rows`, `checks`, and `failures` row tables.}
#' }
#'
#' @seealso [get_summary()], [get_missing()], [flexify()], [registry()],
#'   [redcapAPI::redcapConnection()], [redcapAPI::exportRecordsTyped()],
#'   [redcapAPI::exportDataQuality()]
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
#' report <- find_missing(
#'   data = records,
#'   rcon = rcon,
#'   forms = "baseline_form",
#'   events = c("baseline_event", "followup_event"),
#'   ignore_fields = c("status_flag", "screening_code")
#' )
#'
#' get_summary(report)
#' get_missing(report)
#'
#' repeat_missing <- find_missing(
#'   data = records,
#'   rcon = rcon,
#'   forms = "repeat_form",
#'   instances = 2L
#' )
#'
#' quality_issues <- redcapAPI::exportDataQuality(rcon)
#' verified_report <- find_missing(
#'   data = records,
#'   rcon = rcon,
#'   forms = "baseline_form",
#'   verified = quality_issues,
#'   verified_user = "reviewer_username"
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
  progress = interactive(),
  verified = NULL,
  verified_user = NULL
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
  .miss_check_verified_arguments(
    verified = verified,
    verified_user = verified_user
  )

  report_started_at <- Sys.time()
  report_timer <- .miss_new_timer()
  input_started_at <- .miss_timer_start(report_timer)
  forms <- .miss_resolve_forms(forms)
  event_settings <- .miss_resolve_form_events_arg(
    events = events,
    forms = forms
  )
  instance_settings <- .miss_resolve_form_instances_arg(
    instances = instances,
    forms = forms
  )

  progress_state <- .miss_cli_progress_start(
    forms = forms,
    progress = progress
  )
  on.exit(
    try(
      .miss_cli_progress_finish(progress_state, result = "failed"),
      silent = TRUE
    ),
    add = TRUE
  )

  .miss_timer_record(
    report_timer,
    scope = "report",
    stage = "input",
    started_at = input_started_at
  )

  # Normalize inputs and derive project context from the redcapAPI connection.
  record_data <- tibble::as_tibble(data)

  metadata_started_at <- .miss_timer_start(report_timer)
  meta <- .miss_get_metadata(rcon)
  .miss_check_metadata(meta)
  .miss_timer_record(
    report_timer,
    scope = "report",
    stage = "metadata_api",
    started_at = metadata_started_at
  )
  id_col <- meta$field_name[[1]]
  if (!id_col %in% names(record_data)) {
    stop(
      "The REDCap record identifier column `",
      id_col,
      "` is not present in `data`.",
      call. = FALSE
    )
  }
  project_started_at <- .miss_timer_start(report_timer)
  project_cache <- .miss_get_project_cache(rcon = rcon)
  .miss_timer_record(
    report_timer,
    scope = "report",
    stage = "project_context_api",
    started_at = project_started_at
  )
  verification <- .miss_prepare_verified(
    verified = verified,
    verified_user = verified_user,
    meta = meta,
    project_cache = project_cache
  )

  ignore_ids <- unique(.miss_chr_vec(ignore_ids %||% character()))
  record_specs <- .miss_resolve_records_arg(
    records = records,
    valid_events = .miss_get_project_event_names(project_cache = project_cache),
    forms = forms,
    ignore_ids = ignore_ids
  )

  store_started_at <- .miss_timer_start(report_timer)
  record_store <- .miss_new_record_store(
    data = record_data,
    id_col = id_col,
    ignore_ids = ignore_ids,
    system_fields = project_cache$system_fields
  )
  .miss_check_assessable_records(
    records = record_store$records,
    id_col = id_col,
    record_specs = record_specs
  )
  .miss_timer_record(
    report_timer,
    scope = "report",
    stage = "record_store",
    started_at = store_started_at
  )

  compile_started_at <- .miss_timer_start(report_timer)
  report_plan <- .miss_compile_report_plan(
    meta = meta,
    forms = forms,
    required_fields = required_fields,
    ignore_fields = ignore_fields,
    exclude_types = exclude_types,
    rcon = rcon,
    project_cache = project_cache
  )
  .miss_timer_record(
    report_timer,
    scope = "report",
    stage = "plan_compilation",
    started_at = compile_started_at
  )

  form_reports <- vector("list", length(forms))
  names(form_reports) <- forms
  forms_started_at <- .miss_timer_start(report_timer)
  for (form_i in seq_along(forms)) {
    form <- forms[[form_i]]
    .miss_cli_progress_update(
      state = progress_state,
      form_index = form_i,
      form_fraction = .miss_cli_form_fraction("start"),
      force = FALSE
    )
    progress_callback <- .miss_cli_progress_reporter(
      state = progress_state,
      form_index = form_i
    )
    form_reports[[form]] <- .miss_build_form_report(
      record_store = record_store,
      report_plan = report_plan,
      form_plan = report_plan$form_plans[[form]],
      form = form,
      events = event_settings$values[[form]],
      record_specs = record_specs,
      instances = instance_settings$values[[form]],
      instances_explicit = instance_settings$explicit[[form]],
      details = details,
      verified_keys = verification$keys,
      progress_callback = progress_callback,
      defer_assembly = TRUE
    )
  }
  .miss_timer_record(
    report_timer,
    scope = "report",
    stage = "forms",
    started_at = forms_started_at
  )
  .miss_cli_progress_finalize(
    state = progress_state,
    overall_fraction = 0.96,
    force = FALSE
  )

  preparation_started_at <- .miss_timer_start(report_timer)
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
  used_record_spec_keys <- unique(unlist(.miss_named_report_component(
    form_reports,
    "used_record_spec_keys"
  ), use.names = FALSE))
  unused_record_specs <- .miss_unused_record_specs(
    record_specs = record_specs,
    used_record_spec_keys = used_record_spec_keys
  )
  if (nrow(unused_record_specs) > 0) {
    warning("Unused records spec.", call. = FALSE)
  }
  record_eligibility <- .miss_bind_report_component(
    reports = form_reports,
    component = "record_eligibility"
  )
  flex_event_forms_field_counts <- .miss_bind_report_component(
    reports = form_reports,
    component = "flex_event_forms_field_counts"
  )
  report_validation_rows <- .miss_bind_report_check_rows(form_reports)
  summary <- .miss_build_validation_summary(
    form_reports = form_reports,
    validation_rows = report_validation_rows
  )
  form_validation_rows <- .miss_count_form_validation_rows(form_reports)
  validation_row_count <- form_validation_rows
  raw_missing <- .miss_build_report_missing_rows(
    form_reports = form_reports,
    validation_rows = report_validation_rows
  )
  .miss_timer_record(
    report_timer,
    scope = "report",
    stage = "result_preparation",
    started_at = preparation_started_at
  )

  url_started_at <- .miss_timer_start(report_timer)
  missing <- .miss_add_missing_urls(
    missing = raw_missing,
    rcon = rcon,
    project_cache = project_cache
  )
  .miss_timer_record(
    report_timer,
    scope = "report",
    stage = "url_construction",
    started_at = url_started_at
  )

  assembly_started_at <- .miss_timer_start(report_timer)
  ignored_fields <- unique(unlist(.miss_named_report_component(
    form_reports,
    "ignored_fields"
  ), use.names = FALSE))
  ignored_fields <- ignored_fields %||% character()
  .miss_cli_progress_finalize(
    state = progress_state,
    overall_fraction = 0.98,
    force = FALSE
  )

  details_out <- if (isTRUE(details)) {
    .miss_build_report_details(
      validation_rows = report_validation_rows
    )
  } else {
    NULL
  }
  spec <- .miss_build_report_spec(
    form_reports = form_reports,
    required_fields = required_fields,
    record_eligibility = record_eligibility,
    flex_event_forms_field_counts = flex_event_forms_field_counts,
    unused_record_specs = unused_record_specs,
    ignored_fields = ignored_fields,
    ignored_ids = ignore_ids,
    forms = forms,
    form_labels = form_labels,
    event_labels = event_labels,
    id_col = id_col,
    total_n = .miss_count_form_report_records(form_reports)
  )
  .miss_validate_flex_event_forms_denominators(
    summary = summary,
    spec = spec
  )
  .miss_cli_progress_finalize(
    state = progress_state,
    overall_fraction = 0.99,
    force = FALSE
  )
  .miss_timer_record(
    report_timer,
    scope = "report",
    stage = "report_assembly",
    started_at = assembly_started_at
  )
  report_finished_at <- Sys.time()
  verification$diagnostics$overrides_applied <- as.integer(sum(vapply(
    form_reports,
    `[[`,
    integer(1),
    "verification_overrides_applied"
  )))
  diagnostics <- .miss_build_report_diagnostics(
    started_at = report_started_at,
    finished_at = report_finished_at,
    form_reports = form_reports,
    validation_row_count = validation_row_count,
    summary = summary,
    missing = missing,
    stage_timings = dplyr::bind_rows(
      .miss_timer_rows(report_timer),
      .miss_bind_report_component(form_reports, "stage_timings")
    ),
    form_workload = .miss_bind_report_component(
      form_reports,
      "form_workload"
    ),
    verification = verification$diagnostics
  )
  out <- list(
    summary = summary,
    missing = missing,
    spec = spec,
    diagnostics = diagnostics,
    details = details_out
  )
  class(out) <- "redcapmissing"
  .miss_cli_progress_finish(progress_state, result = "done")
  out
}

# Internal helpers ---------------------------------------------------------

.miss_bind_report_check_rows <- function(form_reports) {
  check_rows <- unlist(
    lapply(form_reports, `[[`, "check_rows"),
    recursive = FALSE,
    use.names = FALSE
  )
  check_rows <- unname(check_rows[vapply(check_rows, nrow, integer(1)) > 0L])
  if (length(check_rows) == 0L) {
    return(.miss_empty_expected())
  }
  columns <- names(.miss_empty_expected())
  values <- lapply(columns, function(column) {
    do.call(c, lapply(check_rows, `[[`, column))
  })
  names(values) <- columns
  tibble::new_tibble(
    values,
    nrow = sum(vapply(check_rows, nrow, integer(1)))
  )
}

.miss_build_validation_summary <- function(
  form_reports,
  validation_rows = NULL
) {
  if (
    is.null(validation_rows) ||
      !all(vapply(form_reports, function(x) !is.null(x$check_rows), logical(1)))
  ) {
    out <- dplyr::bind_rows(lapply(form_reports, `[[`, "summary"))
    if (nrow(out) == 0) {
      return(.miss_empty_validation_summary())
    }
    return(out)
  }

  registry <- .redcapmissing_registry_data()
  zero_rows <- list()
  zero_i <- 0L
  for (form_report in form_reports) {
    counts <- form_report$row_counts
    event_zero <- counts[["event_row_started_checks"]] == 0L
    instance_zero <- counts[["instance_row_started_checks"]] == 0L
    no_row_gates <- event_zero && instance_zero
    zero_checks <- c(
      if (event_zero && instance_zero) "event-row-started",
      if (counts[["form_started_checks"]] == 0L && no_row_gates) {
        "form-started"
      },
      if (counts[["field_complete_checks"]] == 0L && no_row_gates) {
        "field-complete"
      }
    )
    if (length(zero_checks) > 0L) {
      zero_i <- zero_i + 1L
      zero_rows[[zero_i]] <- .miss_zero_validation_summaries(
        validation_checks = zero_checks,
        form = form_report$form,
        registry = registry
      )
    }
    if (
      !isTRUE(no_row_gates) &&
        !is.null(form_report$zero_field_complete_contexts) &&
        nrow(form_report$zero_field_complete_contexts) > 0L
    ) {
      zero_i <- zero_i + 1L
      zero_rows[[zero_i]] <- .miss_zero_validation_context_summaries(
        contexts = form_report$zero_field_complete_contexts,
        validation_check = "field-complete",
        registry = registry
      )
    }
  }

  out <- dplyr::bind_rows(
    .miss_summarise_validation_rows_multi(validation_rows, registry),
    zero_rows
  )
  if (nrow(out) == 0) {
    return(.miss_empty_validation_summary())
  }
  form_order <- match(out$form, names(form_reports))
  check_order <- match(out$validation_check, registry$validation_check)
  tibble::as_tibble(out[order(
    form_order,
    check_order,
    seq_len(nrow(out))
  ), , drop = FALSE])
}

.miss_summarise_validation_rows_multi <- function(rows, registry) {
  if (nrow(rows) == 0) {
    return(.miss_empty_validation_summary())
  }
  rows <- .miss_normalize_validation_context_columns(rows)
  event <- .miss_chr_vec(rows$redcap_event_name)
  form <- .miss_chr_vec(rows$form)
  repeat_instrument <- .miss_chr_vec(rows$redcap_repeat_instrument)
  repeat_instance <- .miss_chr_vec(rows$redcap_repeat_instance)
  validation_check <- .miss_chr_vec(rows$validation_check)
  context_key <- paste(
    .miss_key_part(event),
    .miss_key_part(form),
    .miss_key_part(repeat_instrument),
    .miss_key_part(repeat_instance),
    .miss_key_part(validation_check),
    sep = "\r"
  )
  unique_key <- unique(context_key)
  group_id <- match(context_key, unique_key)
  context_rows <- match(unique_key, context_key)
  assessed <- tabulate(group_id, nbins = length(unique_key))
  passed <- tabulate(
    group_id[rows$validation_passed %in% TRUE],
    nbins = length(unique_key)
  )
  failed <- assessed - passed
  output_check <- validation_check[context_rows]
  output_event <- event[context_rows]
  output_form <- form[context_rows]
  output_repeat_instrument <- repeat_instrument[context_rows]
  output_repeat_instance <- repeat_instance[context_rows]

  tibble::tibble(
    redcap_event_name = output_event,
    form = output_form,
    redcap_repeat_instrument = output_repeat_instrument,
    redcap_repeat_instance = output_repeat_instance,
    validation_level = .redcapmissing_context_validation_level(
      validation_check = output_check,
      repeat_instance = output_repeat_instance
    ),
    validation_check = output_check,
    validation_label = registry$validation_label[
      match(output_check, registry$validation_check)
    ],
    validation_context = .miss_validation_context_vec(
      event = output_event,
      repeat_instance = output_repeat_instance
    ),
    validation_step = .miss_step_id(output_form, output_check),
    assessed = assessed,
    passed = passed,
    failed = failed,
    pass_rate = ifelse(assessed > 0, passed / assessed, 0),
    fail_rate = ifelse(assessed > 0, failed / assessed, 0)
  )
}

.miss_build_form_validation_summary <- function(form_report) {
  registry <- .redcapmissing_registry_data()
  summary_rows <- list()
  row_i <- 0L
  form_registry <- registry

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
      keep_zero = keep_zero,
      validation_label = check$validation_label
    )
  }

  out <- dplyr::bind_rows(summary_rows)
  if (
    .miss_form_report_has_row_context_gates(form_report) &&
      !is.null(form_report$zero_field_complete_contexts) &&
      nrow(form_report$zero_field_complete_contexts) > 0L
  ) {
    out <- dplyr::bind_rows(
      out,
      .miss_zero_validation_context_summaries(
        contexts = form_report$zero_field_complete_contexts,
        validation_check = "field-complete",
        registry = registry
      )
    )
  }
  if (nrow(out) == 0) {
    return(.miss_empty_validation_summary())
  }
  out
}

.miss_summarise_validation_rows <- function(
  rows,
  validation_check,
  form,
  keep_zero = FALSE,
  validation_label = NULL
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

  rows <- .miss_normalize_validation_context_columns(rows)
  event <- .miss_chr_vec(rows$redcap_event_name)
  form_value <- .miss_chr_vec(rows$form)
  repeat_instrument <- .miss_chr_vec(rows$redcap_repeat_instrument)
  repeat_instance <- .miss_chr_vec(rows$redcap_repeat_instance)
  one_context <- all(event == event[[1]]) &&
    all(form_value == form_value[[1]]) &&
    all(repeat_instrument == repeat_instrument[[1]]) &&
    all(repeat_instance == repeat_instance[[1]])
  if (isTRUE(one_context)) {
    if (is.null(validation_label)) {
      validation_label <- .redcapmissing_registry_row(
        validation_check
      )$validation_label[[1]]
    }
    assessed <- nrow(rows)
    passed <- sum(rows$validation_passed %in% TRUE)
    failed <- assessed - passed
    return(tibble::tibble(
      redcap_event_name = event[[1]],
      form = form_value[[1]],
      redcap_repeat_instrument = repeat_instrument[[1]],
      redcap_repeat_instance = repeat_instance[[1]],
      validation_level = .redcapmissing_context_validation_level(
        validation_check = validation_check,
        repeat_instance = repeat_instance[[1]]
      ),
      validation_check = validation_check,
      validation_label = validation_label,
      validation_context = .miss_validation_context_vec(
        event = event[[1]],
        repeat_instance = repeat_instance[[1]]
      ),
      validation_step = .miss_step_id(form_value[[1]], validation_check),
      assessed = as.integer(assessed),
      passed = as.integer(passed),
      failed = as.integer(failed),
      pass_rate = passed / assessed,
      fail_rate = failed / assessed
    ))
  }
  context_key <- paste(
    .miss_key_part(event),
    .miss_key_part(repeat_instrument),
    .miss_key_part(repeat_instance),
    sep = "\r"
  )
  unique_key <- unique(context_key)
  group_id <- match(context_key, unique_key)
  context_rows <- match(unique_key, context_key)

  assessed <- tabulate(group_id, nbins = length(unique_key))
  passed <- tabulate(
    group_id[rows$validation_passed %in% TRUE],
    nbins = length(unique_key)
  )
  failed <- assessed - passed
  pass_rate <- ifelse(assessed > 0, passed / assessed, 0)
  fail_rate <- ifelse(assessed > 0, failed / assessed, 0)
  if (is.null(validation_label)) {
    validation_label <- .redcapmissing_registry_row(
      validation_check
    )$validation_label[[1]]
  }

  tibble::tibble(
    redcap_event_name = event[context_rows],
    form = form_value[context_rows],
    redcap_repeat_instrument = repeat_instrument[context_rows],
    redcap_repeat_instance = repeat_instance[context_rows],
    validation_level = .redcapmissing_context_validation_level(
      validation_check = validation_check,
      repeat_instance = repeat_instance[context_rows]
    ),
    validation_check = validation_check,
    validation_label = validation_label,
    validation_context = .miss_validation_context_vec(
      event = event[context_rows],
      repeat_instance = repeat_instance[context_rows]
    ),
    validation_step = .miss_step_id(form_value[context_rows], validation_check),
    assessed = assessed,
    passed = passed,
    failed = failed,
    pass_rate = pass_rate,
    fail_rate = fail_rate
  )
}

.miss_empty_validation_summary <- local({
  empty <- tibble::tibble(
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
    fail_rate = numeric()
  )
  function() empty
})

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
    fail_rate = 0
  )
}

.miss_build_missing_rows <- function(validation_rows) {
  .miss_build_component_missing_rows(validation_rows, validation_row_offset = 0L)
}

.miss_build_form_missing_rows <- function(check_rows) {
  .miss_build_component_missing_rows(dplyr::bind_rows(check_rows))
}

.miss_build_report_missing_rows <- function(
  form_reports,
  validation_rows = NULL
) {
  if (!is.null(validation_rows)) {
    return(.miss_build_component_missing_rows(validation_rows))
  }
  pieces <- vector("list", length(form_reports))
  offset <- 0L
  for (i in seq_along(form_reports)) {
    form_missing <- form_reports[[i]]$missing %||% .miss_empty_missing_rows()
    pieces[[i]] <- .miss_offset_missing_rows(form_missing, offset)
    offset <- offset + sum(form_reports[[i]]$row_counts)
  }

  out <- dplyr::bind_rows(pieces)
  if (nrow(out) == 0) {
    return(.miss_empty_missing_rows())
  }
  out
}

.miss_validation_record_ids <- function(check_rows) {
  record_ids <- unlist(lapply(check_rows, function(rows) {
    if (nrow(rows) == 0 || !"record_id" %in% names(rows)) {
      return(character())
    }
    .miss_chr_vec(rows$record_id)
  }), use.names = FALSE)
  if (length(record_ids) == 0) {
    return(character())
  }

  record_ids <- unique(record_ids)
  record_ids[!.miss_is_blank_vec(record_ids)]
}

.miss_build_component_missing_rows <- function(
  validation_rows,
  validation_row_offset = 0L
) {
  if (nrow(validation_rows) == 0) {
    return(.miss_empty_missing_rows())
  }

  failed_rows <- which(!(validation_rows$validation_passed %in% TRUE))
  out <- validation_rows[failed_rows, , drop = FALSE]
  if (nrow(out) == 0) {
    return(.miss_empty_missing_rows())
  }

  out <- .miss_add_validation_context(out)
  out$validation_row_id <- validation_row_offset + failed_rows
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
    "validation_check",
    "validation_passed",
    "field_name",
    "field_label",
    "field_type",
    "branching_logic",
    "branch_satisfied",
    "export_fields"
  )

  rows[, select_cols, drop = FALSE]
}

.miss_empty_missing_rows <- local({
  empty <- tibble::tibble(
    validation_step = character(),
    validation_row_id = integer(),
    record_id = character(),
    redcap_event_name = character(),
    redcap_repeat_instrument = character(),
    redcap_repeat_instance = character(),
    validation_context = character(),
    form = character(),
    validation_level = character(),
    validation_check = character(),
    validation_passed = logical(),
    field_name = character(),
    field_label = character(),
    field_type = character(),
    branching_logic = character(),
    branch_satisfied = logical(),
    export_fields = character(),
    url = character()
  )
  function() empty
})

.miss_add_missing_urls <- function(missing, rcon, project_cache) {
  if (nrow(missing) == 0) {
    return(missing)
  }

  missing$url <- .miss_build_missing_urls(
    missing = missing,
    rcon = rcon,
    project_cache = project_cache
  )
  missing
}

.miss_build_missing_urls <- function(missing, rcon, project_cache) {
  urls <- rep(NA_character_, nrow(missing))
  instance_url <- .miss_chr(rcon$url %||% NA_character_)
  redcap_version <- .miss_get_rcon_version(rcon)
  project_id <- .miss_get_project_information_value(
    project_information = project_cache$project_information,
    column = "project_id"
  )
  is_longitudinal <- .miss_project_is_longitudinal(
    .miss_get_project_information_value(
      project_information = project_cache$project_information,
      column = "is_longitudinal"
    )
  )

  if (
    .miss_is_blank_scalar(instance_url) ||
      .miss_is_blank_scalar(redcap_version) ||
      .miss_is_blank_scalar(project_id) ||
      is.na(is_longitudinal)
  ) {
    return(urls)
  }

  event_ids <- rep(NA_character_, nrow(missing))
  if (isTRUE(is_longitudinal)) {
    event_index <- match(
      .miss_chr_vec(missing$redcap_event_name),
      project_cache$events$unique_event_name
    )
    event_ids <- project_cache$events$event_id[event_index]
  }

  complete <-
    !.miss_is_blank_vec(missing$form) &
    !.miss_is_blank_vec(missing$record_id)
  if (isTRUE(is_longitudinal)) {
    complete <- complete & !.miss_is_blank_vec(event_ids)
  }
  if (!any(complete)) {
    return(urls)
  }

  base_url <- sub("/api(/|)$", "", instance_url)
  urls[complete] <- sprintf(
    "%s/redcap_v%s/DataEntry/index.php?pid=%s&page=%s&id=%s",
    base_url,
    redcap_version,
    project_id,
    missing$form[complete],
    missing$record_id[complete]
  )
  if (isTRUE(is_longitudinal)) {
    urls[complete] <- paste0(
      urls[complete],
      "&event_id=",
      event_ids[complete]
    )
  }

  urls
}

.miss_get_rcon_version <- function(rcon) {
  version_method <- rcon$version
  if (!is.function(version_method)) {
    return(NA_character_)
  }

  version <- tryCatch(version_method(), error = function(e) NULL)
  if (length(version) != 1) {
    return(NA_character_)
  }

  .miss_chr(version[[1]])
}

.miss_get_project_information_value <- function(project_information, column) {
  if (
    is.null(project_information) ||
      nrow(project_information) == 0 ||
      !column %in% names(project_information)
  ) {
    return(NA_character_)
  }

  .miss_chr(project_information[[column]][[1]])
}

.miss_project_is_longitudinal <- function(value) {
  value <- tolower(.miss_chr(value))
  if (value %in% c("1", "true")) {
    return(TRUE)
  }
  if (value %in% c("0", "false")) {
    return(FALSE)
  }

  NA
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
  record_eligibility,
  flex_event_forms_field_counts,
  unused_record_specs,
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
    record_eligibility = .miss_select_record_eligibility(record_eligibility),
    .flex_event_forms_field_counts = flex_event_forms_field_counts,
    unused_record_specs = .miss_select_unused_record_specs(unused_record_specs),
    instances = .miss_named_report_component(form_reports, "instances"),
    ignored_fields = ignored_fields,
    ignored_ids = ignored_ids,
    project = .miss_named_report_component(form_reports, "project"),
    id_col = id_col,
    system_fields = form_reports[[1]]$system_fields,
    total_n = total_n
  )
}

.miss_build_flex_event_forms_field_counts <- function(
  record_eligibility,
  field_complete_checks
) {
  context_columns <- c(
    "record_id",
    "redcap_event_name",
    "form",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  )
  out <- unique(record_eligibility[, context_columns, drop = FALSE])
  out <- tibble::as_tibble(out)
  if (nrow(out) == 0) {
    out$field_assessed <- integer()
    out$field_failed <- integer()
    return(out)
  }

  field_complete_checks <- tibble::as_tibble(field_complete_checks)
  if (nrow(field_complete_checks) == 0) {
    out$field_assessed <- rep.int(0L, nrow(out))
    out$field_failed <- rep.int(0L, nrow(out))
    return(out)
  }

  context_key <- do.call(
    paste,
    c(
      lapply(
        field_complete_checks[, context_columns, drop = FALSE],
        .miss_key_part
      ),
      list(sep = "\r")
    )
  )
  unique_key <- unique(context_key)
  group_id <- match(context_key, unique_key)
  assessed <- tabulate(group_id, nbins = length(unique_key))
  failed <- tabulate(
    group_id[!(field_complete_checks$validation_passed %in% TRUE)],
    nbins = length(unique_key)
  )
  out_key <- do.call(
    paste,
    c(lapply(out[, context_columns, drop = FALSE], .miss_key_part), list(sep = "\r"))
  )
  count_match <- match(out_key, unique_key)
  out$field_assessed <- rep.int(0L, nrow(out))
  out$field_failed <- rep.int(0L, nrow(out))
  matched <- !is.na(count_match)
  out$field_assessed[matched] <- assessed[count_match[matched]]
  out$field_failed[matched] <- failed[count_match[matched]]
  out
}

.miss_count_form_report_records <- function(form_reports) {
  record_ids <- unique(unlist(
    lapply(form_reports, `[[`, "record_ids"),
    use.names = FALSE
  ))
  record_ids <- record_ids[!.miss_is_blank_vec(record_ids)]
  length(record_ids)
}

.miss_check_assessable_records <- function(records, id_col, record_specs) {
  if (
    .miss_has_assessable_record_rows(records = records, id_col = id_col) ||
      nrow(record_specs) > 0
  ) {
    return(invisible(records))
  }

  stop(
    "`find_missing()` has no records to assess after filtering. ",
    "Provide at least one non-ignored record in `data`, or use `records` ",
    "to declare expected REDCap record IDs for selected contexts.",
    call. = FALSE
  )
}

.miss_has_assessable_record_rows <- function(records, id_col) {
  records <- tibble::as_tibble(records)
  if (nrow(records) == 0 || !id_col %in% names(records)) {
    return(FALSE)
  }

  any(!.miss_is_blank_vec(records[[id_col]]))
}

.miss_validate_flex_event_forms_denominators <- function(summary, spec) {
  contexts <- .miss_flex_event_forms_summary_contexts(summary)
  if (nrow(contexts) == 0) {
    stop(
      "`find_missing()` has no records to assess after filtering. ",
      "Provide at least one non-ignored record in `data`, or use `records` ",
      "to declare expected REDCap record IDs for selected events.",
      call. = FALSE
    )
  }

  all_events_blank <- all(.miss_is_blank_vec(contexts$redcap_event_name))
  for (row_i in seq_len(nrow(contexts))) {
    context <- contexts[row_i, , drop = FALSE]
    repeat_context <- .miss_summary_context_has_repeat(context)
    single_event_context <- all_events_blank &&
      .miss_is_blank_scalar(context$redcap_event_name) &&
      !isTRUE(repeat_context)
    if (isTRUE(single_event_context)) {
      .miss_validate_single_event_total_n(spec = spec)
      next
    }

    validation_check <- if (isTRUE(repeat_context)) {
      "instance-row-started"
    } else {
      "event-row-started"
    }
    summary_row <- .miss_exact_validation_summary_row(
      summary = summary,
      context = context,
      validation_check = validation_check
    )
    if (nrow(summary_row) == 0) {
      stop(
        "`find_missing()` built a report context without an exact `",
        validation_check,
        "` summary for ",
        .miss_validation_context_label(context),
        ".",
        call. = FALSE
      )
    }

    assessed <- summary_row$assessed[[1]]
    if (length(assessed) != 1 || is.na(assessed) || assessed <= 0) {
      stop(
        "`find_missing()` built a report context with invalid `",
        validation_check,
        "` assessed N for ",
        .miss_validation_context_label(context),
        ".",
        call. = FALSE
      )
    }
  }

  invisible(summary)
}

.miss_flex_event_forms_summary_contexts <- function(summary) {
  context_checks <- c(
    "event-row-started",
    "instance-row-started",
    "form-started",
    "field-complete"
  )
  context_cols <- c(
    "redcap_event_name",
    "form",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  )
  summary <- tibble::as_tibble(summary)
  summary <- .miss_normalize_validation_context_columns(summary)
  rows <- summary[
    summary$validation_check %in% context_checks &
      !.miss_is_blank_vec(summary$form),
    context_cols,
    drop = FALSE
  ]
  if (nrow(rows) == 0) {
    return(tibble::tibble(
      redcap_event_name = character(),
      form = character(),
      redcap_repeat_instrument = character(),
      redcap_repeat_instance = character()
    ))
  }

  rows <- unique(rows)
  tibble::as_tibble(rows)
}

.miss_summary_context_has_repeat <- function(context) {
  !.miss_is_blank_scalar(context$redcap_repeat_instrument) ||
    !.miss_is_blank_scalar(context$redcap_repeat_instance)
}

.miss_validate_single_event_total_n <- function(spec) {
  total_n <- spec$total_n %||% NA_integer_
  if (length(total_n) == 1 && !is.na(total_n) && total_n > 0) {
    return(invisible(total_n))
  }

  stop(
    "`find_missing()` built a non-longitudinal report without a positive ",
    "`spec$total_n` for `flex_event_forms()`.",
    call. = FALSE
  )
}

.miss_exact_validation_summary_row <- function(summary, context, validation_check) {
  summary <- tibble::as_tibble(summary)
  summary <- .miss_normalize_validation_context_columns(summary)
  rows <- summary[
    summary$validation_check == validation_check &
      summary$redcap_event_name == context$redcap_event_name &
      summary$form == context$form &
      summary$redcap_repeat_instrument == context$redcap_repeat_instrument &
      summary$redcap_repeat_instance == context$redcap_repeat_instance,
    ,
    drop = FALSE
  ]

  rows[seq_len(min(nrow(rows), 1L)), , drop = FALSE]
}

.miss_validation_context_label <- function(context) {
  parts <- c(
    paste0("event `", context$redcap_event_name[[1]], "`"),
    paste0("form `", context$form[[1]], "`")
  )
  if (!.miss_is_blank_scalar(context$redcap_repeat_instrument)) {
    parts <- c(
      parts,
      paste0("repeat instrument `", context$redcap_repeat_instrument[[1]], "`")
    )
  }
  if (!.miss_is_blank_scalar(context$redcap_repeat_instance)) {
    parts <- c(
      parts,
      paste0("repeat instance `", context$redcap_repeat_instance[[1]], "`")
    )
  }

  paste(parts, collapse = ", ")
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
  missing,
  stage_timings,
  form_workload,
  verification = .miss_verification_diagnostics()
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
    missing_rows = nrow(missing),
    stage_timings = tibble::as_tibble(stage_timings),
    form_workload = tibble::as_tibble(form_workload),
    verification = verification
  )
}

.miss_zero_validation_summaries <- function(
  validation_checks,
  form,
  registry = .redcapmissing_registry_data()
) {
  validation_checks <- .miss_chr_vec(validation_checks)
  n <- length(validation_checks)
  if (n == 0L) {
    return(.miss_empty_validation_summary())
  }
  tibble::new_tibble(list(
    redcap_event_name = rep("", n),
    form = rep(form, n),
    redcap_repeat_instrument = rep("", n),
    redcap_repeat_instance = rep("", n),
    validation_level = .redcapmissing_context_validation_level(
      validation_check = validation_checks,
      repeat_instance = rep("", n)
    ),
    validation_check = validation_checks,
    validation_label = registry$validation_label[
      match(validation_checks, registry$validation_check)
    ],
    validation_context = rep("overall", n),
    validation_step = .miss_step_id(form, validation_checks),
    assessed = rep(0L, n),
    passed = rep(0L, n),
    failed = rep(0L, n),
    pass_rate = rep(0, n),
    fail_rate = rep(0, n)
  ), nrow = n)
}

.miss_zero_validation_context_summaries <- function(
  contexts,
  validation_check,
  registry = .redcapmissing_registry_data()
) {
  contexts <- .miss_normalize_validation_context_columns(contexts)
  context_columns <- c(
    "redcap_event_name",
    "form",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  )
  contexts <- unique(contexts[, context_columns, drop = FALSE])
  n <- nrow(contexts)
  if (n == 0L) {
    return(.miss_empty_validation_summary())
  }

  validation_check <- rep(validation_check, n)
  tibble::new_tibble(list(
    redcap_event_name = contexts$redcap_event_name,
    form = contexts$form,
    redcap_repeat_instrument = contexts$redcap_repeat_instrument,
    redcap_repeat_instance = contexts$redcap_repeat_instance,
    validation_level = .redcapmissing_context_validation_level(
      validation_check = validation_check,
      repeat_instance = contexts$redcap_repeat_instance
    ),
    validation_check = validation_check,
    validation_label = registry$validation_label[
      match(validation_check, registry$validation_check)
    ],
    validation_context = .miss_validation_context_vec(
      event = contexts$redcap_event_name,
      repeat_instance = contexts$redcap_repeat_instance
    ),
    validation_step = .miss_step_id(contexts$form, validation_check),
    assessed = rep(0L, n),
    passed = rep(0L, n),
    failed = rep(0L, n),
    pass_rate = rep(0, n),
    fail_rate = rep(0, n)
  ), nrow = n)
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

.miss_resolve_records_arg <- function(records, valid_events, forms, ignore_ids) {
  if (is.null(records)) {
    return(.miss_empty_record_specs())
  }
  if (!is.list(records) || is.data.frame(records)) {
    stop(
      "`records` must be NULL or a named list by raw REDCap event name.",
      call. = FALSE
    )
  }

  record_event_names <- names(records)
  .miss_check_records_names(
    names = record_event_names,
    expected_length = length(records),
    context = "`records`"
  )
  .miss_check_duplicate_records_names(
    names = record_event_names,
    label = "event name"
  )

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

  pieces <- list()
  for (event in record_event_names) {
    value <- records[[event]]
    pieces <- c(pieces, list(.miss_parse_records_event_value(
      value = value,
      event = event,
      forms = forms
    )))
  }

  out <- dplyr::bind_rows(pieces)
  if (nrow(out) == 0) {
    return(.miss_empty_record_specs())
  }

  ignored <- intersect(unique(out$record_id), unique(.miss_chr_vec(ignore_ids)))
  ignored <- ignored[!.miss_is_blank_vec(ignored)]
  if (length(ignored) > 0) {
    stop(
      "`ignore_ids` includes ID(s) also listed in `records`: ",
      paste(ignored, collapse = ", "),
      call. = FALSE
    )
  }

  out$spec_key <- .miss_record_spec_key(out)
  out <- out[!duplicated(out), , drop = FALSE]
  tibble::as_tibble(out)
}

.miss_parse_records_event_value <- function(value, event, forms) {
  if (is.null(value)) {
    stop(
      "`records` entry `",
      event,
      "` must not be NULL.",
      call. = FALSE
    )
  }

  if (!is.list(value) || is.data.frame(value)) {
    record_ids <- .miss_validate_records_ids(value, context = paste0(
      "`records` entry `",
      event,
      "`"
    ))
    return(.miss_record_specs_rows(
      event = event,
      form = "",
      repeat_instance = "",
      record_ids = record_ids,
      source = "records_event"
    ))
  }

  form_names <- names(value)
  .miss_check_records_names(
    names = form_names,
    expected_length = length(value),
    context = paste0("`records` entry `", event, "`")
  )
  .miss_check_duplicate_records_names(
    names = form_names,
    label = "form name"
  )
  unknown_forms <- setdiff(form_names, forms)
  if (length(unknown_forms) > 0) {
    stop(
      "`records` form names must match requested `forms`. Unknown form(s): ",
      paste(unknown_forms, collapse = ", "),
      call. = FALSE
    )
  }

  pieces <- list()
  for (form in form_names) {
    pieces <- c(pieces, list(.miss_parse_records_form_value(
      value = value[[form]],
      event = event,
      form = form
    )))
  }
  dplyr::bind_rows(pieces)
}

.miss_parse_records_form_value <- function(value, event, form) {
  if (is.null(value)) {
    stop(
      "`records` entry `",
      event,
      "$",
      form,
      "` must not be NULL.",
      call. = FALSE
    )
  }

  if (!is.list(value) || is.data.frame(value)) {
    record_ids <- .miss_validate_records_ids(value, context = paste0(
      "`records` entry `",
      event,
      "$",
      form,
      "`"
    ))
    return(.miss_record_specs_rows(
      event = event,
      form = form,
      repeat_instance = "",
      record_ids = record_ids,
      source = "records_form"
    ))
  }

  instance_names <- names(value)
  .miss_check_records_names(
    names = instance_names,
    expected_length = length(value),
    context = paste0("`records` entry `", event, "$", form, "`")
  )
  .miss_check_duplicate_records_names(
    names = instance_names,
    label = "repeat instance"
  )
  repeat_instances <- .miss_validate_records_instance_names(instance_names)

  pieces <- list()
  for (i in seq_along(instance_names)) {
    record_ids <- .miss_validate_records_ids(value[[i]], context = paste0(
      "`records` entry `",
      event,
      "$",
      form,
      "$",
      instance_names[[i]],
      "`"
    ))
    pieces <- c(pieces, list(.miss_record_specs_rows(
      event = event,
      form = form,
      repeat_instance = repeat_instances[[i]],
      record_ids = record_ids,
      source = "records_instance"
    )))
  }
  dplyr::bind_rows(pieces)
}

.miss_check_records_names <- function(names, expected_length, context) {
  if (
    is.null(names) ||
      length(names) != expected_length ||
      any(.miss_is_blank_vec(names))
  ) {
    stop(
      context,
      " must be a fully named list.",
      call. = FALSE
    )
  }
  invisible(names)
}

.miss_check_duplicate_records_names <- function(names, label) {
  duplicated_names <- unique(names[duplicated(names)])
  if (length(duplicated_names) > 0) {
    stop(
      "`records` names must not be duplicated. Duplicate ",
      label,
      "(s): ",
      paste(duplicated_names, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(names)
}

.miss_validate_records_ids <- function(value, context) {
  if (is.null(value) || is.list(value) || is.data.frame(value)) {
    stop(context, " must be a vector of REDCap record IDs.", call. = FALSE)
  }
  record_ids <- .miss_chr_vec(value)
  if (
    length(record_ids) == 0 ||
      any(is.na(record_ids) | trimws(record_ids) == "")
  ) {
    stop(
      context,
      " must contain only non-blank record IDs.",
      call. = FALSE
    )
  }
  unique(record_ids)
}

.miss_validate_records_instance_names <- function(instance_names) {
  numeric_values <- suppressWarnings(as.numeric(instance_names))
  normalized_values <- as.character(as.integer(numeric_values))
  invalid <- is.na(numeric_values) |
    !is.finite(numeric_values) |
    numeric_values < 1 |
    numeric_values != floor(numeric_values) |
    normalized_values != instance_names
  if (any(invalid)) {
    stop(
      "`records` repeat-instance names must be positive whole numbers.",
      call. = FALSE
    )
  }
  normalized_values
}

.miss_record_specs_rows <- function(
  event,
  form,
  repeat_instance,
  record_ids,
  source
) {
  tibble::tibble(
    spec_key = NA_character_,
    redcap_event_name = rep(event, length(record_ids)),
    form = rep(form, length(record_ids)),
    redcap_repeat_instance = rep(repeat_instance, length(record_ids)),
    record_id = record_ids,
    eligibility_source = rep(source, length(record_ids))
  )
}

.miss_empty_record_specs <- local({
  empty <- tibble::tibble(
    spec_key = character(),
    redcap_event_name = character(),
    form = character(),
    redcap_repeat_instance = character(),
    record_id = character(),
    eligibility_source = character()
  )
  function() empty
})

.miss_record_spec_key <- function(record_specs) {
  paste(
    .miss_key_part(record_specs$redcap_event_name),
    .miss_key_part(record_specs$form),
    .miss_key_part(record_specs$redcap_repeat_instance),
    .miss_key_part(record_specs$eligibility_source),
    sep = "\r"
  )
}

.miss_check_record_specs_for_form <- function(record_specs, project, form) {
  if (nrow(record_specs) == 0) {
    return(invisible(record_specs))
  }

  form_specs <- record_specs[
    record_specs$form == form,
    ,
    drop = FALSE
  ]
  if (nrow(form_specs) == 0) {
    return(invisible(record_specs))
  }

  offered_events <- union(project$form_events, project$repeat_form_events)
  invalid_events <- setdiff(unique(form_specs$redcap_event_name), offered_events)
  if (length(invalid_events) > 0) {
    stop(
      "`records` includes event/form combination(s) not offered by REDCap: ",
      paste(paste(invalid_events, form, sep = "/"), collapse = ", "),
      call. = FALSE
    )
  }

  instance_specs <- form_specs[
    !.miss_is_blank_vec(form_specs$redcap_repeat_instance),
    ,
    drop = FALSE
  ]
  if (nrow(instance_specs) > 0) {
    repeat_events <- union(
      project$repeat_form_events,
      .miss_form_repeating_events(project)
    )
    nonrepeat_instance_events <- setdiff(
      unique(instance_specs$redcap_event_name),
      repeat_events
    )
    if (length(nonrepeat_instance_events) > 0) {
      stop(
        "`records` includes repeat instances for non-repeating context(s): ",
        paste(paste(nonrepeat_instance_events, form, sep = "/"), collapse = ", "),
        call. = FALSE
      )
    }
  }

  invisible(record_specs)
}

.miss_build_record_eligibility <- function(
  records,
  project,
  form,
  instances,
  record_specs
) {
  contexts <- .miss_build_record_eligibility_contexts(
    records = records,
    project = project,
    form = form,
    instances = instances
  )
  if (nrow(contexts) == 0) {
    return(.miss_empty_record_eligibility())
  }

  if (nrow(contexts) == 1 && nrow(record_specs) == 0) {
    out <- .miss_context_record_eligibility(
      context = contexts,
      record_ids = contexts$default_record_ids[[1]],
      source = "data",
      record_spec_key = ""
    )
    if (nrow(out) == 0) {
      return(.miss_empty_record_eligibility())
    }
    return(tibble::as_tibble(out[order(out$record_id), , drop = FALSE]))
  }

  pieces <- lapply(seq_len(nrow(contexts)), function(i) {
    context <- contexts[i, , drop = FALSE]
    selected <- .miss_record_specs_for_context(
      record_specs = record_specs,
      context = context
    )
    if (nrow(selected) > 0) {
      return(.miss_context_record_eligibility(
        context = context,
        record_ids = selected$record_id,
        source = selected$eligibility_source[[1]],
        record_spec_key = selected$spec_key[[1]]
      ))
    }

    .miss_context_record_eligibility(
      context = context,
      record_ids = context$default_record_ids[[1]],
      source = "data",
      record_spec_key = ""
    )
  })

  out <- dplyr::bind_rows(pieces)
  if (nrow(out) == 0) {
    return(.miss_empty_record_eligibility())
  }

  out <- unique(out)
  tibble::as_tibble(out[order(
    out$redcap_event_name,
    out$form,
    out$redcap_repeat_instrument,
    suppressWarnings(as.numeric(out$redcap_repeat_instance)),
    out$redcap_repeat_instance,
    out$record_id
  ), , drop = FALSE])
}

.miss_build_record_eligibility_contexts <- function(
  records,
  project,
  form,
  instances
) {
  pieces <- list()

  nonrepeat_events <- .miss_missing_event_form_events(project)
  if (length(nonrepeat_events) > 0) {
    event_records <- .miss_expected_record_events_from_data(
      records = records,
      project = project,
      events = nonrepeat_events
    )
    pieces <- c(pieces, list(.miss_record_contexts_from_record_events(
      record_events = event_records,
      events = nonrepeat_events,
      form = form,
      repeat_instrument = "",
      repeat_instance = ""
    )))
  }

  if (!is.null(instances) && isTRUE(project$form_repeats)) {
    repeat_contexts <- .miss_repeat_record_eligibility_contexts(
      records = records,
      project = project,
      form = form,
      instances = instances
    )
    pieces <- c(pieces, list(repeat_contexts))
  }

  fields <- project$system_fields
  if (
    length(nonrepeat_events) == 0 &&
      !isTRUE(project$form_repeats) &&
      !fields$event_col %in% names(records)
  ) {
    record_ids <- unique(.miss_chr_vec(records[[project$id_col]]))
    record_ids <- record_ids[!.miss_is_blank_vec(record_ids)]
    pieces <- c(pieces, list(tibble::tibble(
      redcap_event_name = "",
      form = form,
      redcap_repeat_instrument = "",
      redcap_repeat_instance = "",
      default_record_ids = list(record_ids)
    )))
  }

  out <- dplyr::bind_rows(pieces)
  if (nrow(out) == 0) {
    return(.miss_empty_record_eligibility_contexts())
  }
  out
}

.miss_repeat_record_eligibility_contexts <- function(
  records,
  project,
  form,
  instances
) {
  fields <- project$system_fields
  instance_values <- .miss_chr_vec(instances)
  pieces <- list()

  if (fields$event_col %in% names(records)) {
    if (length(project$repeat_form_events) > 0) {
      record_events <- .miss_expected_record_events_from_data(
        records = records,
        project = project,
        events = project$repeat_form_events
      )
      pieces <- c(pieces, list(.miss_record_contexts_from_record_events(
        record_events = record_events,
        events = project$repeat_form_events,
        form = form,
        repeat_instrument = form,
        repeat_instance = instance_values
      )))
    }

    repeating_events <- .miss_form_repeating_events(project)
    if (length(repeating_events) > 0) {
      record_events <- .miss_expected_record_events_from_data(
        records = records,
        project = project,
        events = repeating_events
      )
      pieces <- c(pieces, list(.miss_record_contexts_from_record_events(
        record_events = record_events,
        events = repeating_events,
        form = form,
        repeat_instrument = "",
        repeat_instance = instance_values
      )))
    }
  } else {
    record_ids <- unique(.miss_chr_vec(records[[project$id_col]]))
    record_ids <- record_ids[!.miss_is_blank_vec(record_ids)]
    pieces <- c(pieces, list(tibble::tibble(
      redcap_event_name = "",
      form = form,
      redcap_repeat_instrument = form,
      redcap_repeat_instance = instance_values,
      default_record_ids = rep(list(record_ids), length(instance_values))
    )))
  }

  out <- dplyr::bind_rows(pieces)
  if (nrow(out) == 0) {
    return(.miss_empty_record_eligibility_contexts())
  }
  out
}

.miss_record_contexts_from_record_events <- function(
  record_events,
  events,
  form,
  repeat_instrument,
  repeat_instance
) {
  context_events <- unique(.miss_chr_vec(events))
  context_events <- context_events[!.miss_is_blank_vec(context_events)]
  if (length(context_events) == 0) {
    return(.miss_empty_record_eligibility_contexts())
  }
  if (length(repeat_instance) == 1 && .miss_is_blank_scalar(repeat_instance)) {
    return(tibble::tibble(
      redcap_event_name = context_events,
      form = form,
      redcap_repeat_instrument = repeat_instrument,
      redcap_repeat_instance = "",
      default_record_ids = lapply(context_events, function(event) {
        unique(record_events$record_id[record_events$redcap_event_name == event])
      })
    ))
  }

  dplyr::bind_rows(lapply(context_events, function(event) {
    record_ids <- unique(record_events$record_id[
      record_events$redcap_event_name == event
    ])
    tibble::tibble(
      redcap_event_name = event,
      form = form,
      redcap_repeat_instrument = repeat_instrument,
      redcap_repeat_instance = repeat_instance,
      default_record_ids = rep(list(record_ids), length(repeat_instance))
    )
  }))
}

.miss_record_specs_for_context <- function(record_specs, context) {
  if (nrow(record_specs) == 0) {
    return(record_specs)
  }

  event <- context$redcap_event_name[[1]]
  form <- context$form[[1]]
  repeat_instance <- context$redcap_repeat_instance[[1]]

  exact_instance <- record_specs[
    record_specs$redcap_event_name == event &
      record_specs$form == form &
      record_specs$redcap_repeat_instance == repeat_instance &
      record_specs$eligibility_source == "records_instance",
    ,
    drop = FALSE
  ]
  if (nrow(exact_instance) > 0) {
    return(exact_instance)
  }

  exact_form <- record_specs[
    record_specs$redcap_event_name == event &
      record_specs$form == form &
      .miss_is_blank_vec(record_specs$redcap_repeat_instance) &
      record_specs$eligibility_source == "records_form",
    ,
    drop = FALSE
  ]
  if (nrow(exact_form) > 0) {
    return(exact_form)
  }

  event_specs <- record_specs[
    record_specs$redcap_event_name == event &
      .miss_is_blank_vec(record_specs$form) &
      .miss_is_blank_vec(record_specs$redcap_repeat_instance) &
      record_specs$eligibility_source == "records_event",
    ,
    drop = FALSE
  ]
  event_specs
}

.miss_context_record_eligibility <- function(
  context,
  record_ids,
  source,
  record_spec_key
) {
  record_ids <- unique(.miss_chr_vec(record_ids))
  record_ids <- record_ids[!.miss_is_blank_vec(record_ids)]
  if (length(record_ids) == 0) {
    return(.miss_empty_record_eligibility())
  }

  tibble::tibble(
    record_id = record_ids,
    redcap_event_name = rep(context$redcap_event_name[[1]], length(record_ids)),
    form = rep(context$form[[1]], length(record_ids)),
    redcap_repeat_instrument = rep(
      context$redcap_repeat_instrument[[1]],
      length(record_ids)
    ),
    redcap_repeat_instance = rep(
      context$redcap_repeat_instance[[1]],
      length(record_ids)
    ),
    eligibility_source = rep(source, length(record_ids)),
    record_spec_key = rep(record_spec_key, length(record_ids))
  )
}

.miss_empty_record_eligibility_contexts <- local({
  empty <- tibble::tibble(
    redcap_event_name = character(),
    form = character(),
    redcap_repeat_instrument = character(),
    redcap_repeat_instance = character(),
    default_record_ids = list()
  )
  function() empty
})

.miss_empty_record_eligibility <- local({
  empty <- tibble::tibble(
    record_id = character(),
    redcap_event_name = character(),
    form = character(),
    redcap_repeat_instrument = character(),
    redcap_repeat_instance = character(),
    eligibility_source = character(),
    record_spec_key = character()
  )
  function() empty
})

.miss_unused_record_specs <- function(record_specs, used_record_spec_keys) {
  if (nrow(record_specs) == 0) {
    return(.miss_empty_record_specs())
  }
  used_record_spec_keys <- unique(.miss_chr_vec(used_record_spec_keys))
  unused <- record_specs[
    !record_specs$spec_key %in% used_record_spec_keys,
    ,
    drop = FALSE
  ]
  if (nrow(unused) == 0) {
    return(.miss_empty_record_specs())
  }
  tibble::as_tibble(unused)
}

.miss_select_record_eligibility <- function(record_eligibility) {
  columns <- c(
    "record_id",
    "redcap_event_name",
    "form",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "eligibility_source"
  )
  record_eligibility[, columns, drop = FALSE]
}

.miss_select_unused_record_specs <- function(unused_record_specs) {
  columns <- c(
    "redcap_event_name",
    "form",
    "redcap_repeat_instance",
    "record_id",
    "eligibility_source"
  )
  unused_record_specs[, columns, drop = FALSE]
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

.miss_cli_progress_start <- function(forms, progress) {
  state <- new.env(parent = emptyenv())
  state$enabled <- isTRUE(progress)
  state$dynamic <- state$enabled && isTRUE(cli::is_dynamic_tty())
  state$forms <- .miss_chr_vec(forms)
  state$form_index <- 1L
  state$form_fraction <- 0
  state$overall_fraction <- 0
  state$phase <- "form"
  state$finished <- FALSE
  state$id <- NULL
  state$last_line <- NULL
  state$has_rendered <- FALSE
  state$last_render_elapsed <- unname(proc.time()[["elapsed"]])
  state$minimum_render_interval <- suppressWarnings(as.numeric(
    getOption("redcapmissing.progress_min_interval", 2)
  ))
  if (
    length(state$minimum_render_interval) != 1L ||
      is.na(state$minimum_render_interval) ||
      state$minimum_render_interval < 0
  ) {
    state$minimum_render_interval <- 2
  }
  state$envir <- parent.frame()
  state$theme <- if (state$enabled && !state$dynamic) {
    .miss_cli_progress_theme()
  } else {
    NULL
  }

  state
}

.miss_cli_progress_open <- function(state, line) {
  if (!isTRUE(state$dynamic) || !is.null(state$id)) {
    return(invisible(state$id))
  }
  state$id <- tryCatch(
    cli::cli_progress_bar(
      name = "find_missing",
      type = "custom",
      total = 100,
      format = "{cli::pb_extra$line}",
      format_done = "{cli::pb_extra$line}",
      format_failed = "{cli::pb_extra$line}",
      clear = FALSE,
      current = FALSE,
      auto_terminate = FALSE,
      extra = list(line = line),
      .auto_close = FALSE,
      .envir = state$envir
    ),
    error = function(error) {
      state$dynamic <- FALSE
      NULL
    }
  )
  invisible(state$id)
}

.miss_cli_progress_reporter <- function(state, form_index) {
  if (
    !is.environment(state) ||
      !isTRUE(state$enabled) ||
      !isTRUE(state$dynamic)
  ) {
    return(NULL)
  }

  force(form_index)
  function(form_fraction, force = FALSE) {
    .miss_cli_progress_update(
      state = state,
      form_index = form_index,
      form_fraction = form_fraction,
      force = force
    )
  }
}

.miss_cli_progress_update <- function(
  state,
  form_index,
  form_fraction,
  force = FALSE
) {
  if (!is.environment(state) || !isTRUE(state$enabled) || isTRUE(state$finished)) {
    return(invisible(NULL))
  }

  form_count <- length(state$forms)
  form_index <- max(1L, min(as.integer(form_index), form_count))
  form_fraction <- .miss_cli_clamp_fraction(form_fraction)
  if (identical(form_index, state$form_index)) {
    form_fraction <- max(state$form_fraction, form_fraction)
  }

  state$form_index <- form_index
  state$form_fraction <- form_fraction
  state$phase <- "form"
  state$overall_fraction <- max(
    state$overall_fraction,
    .miss_cli_overall_fraction(
      form_index = form_index,
      form_fraction = form_fraction,
      form_count = form_count
    )
  )
  if (!isTRUE(state$dynamic)) {
    return(invisible(NULL))
  }
  render_elapsed <- unname(proc.time()[["elapsed"]])
  if (
    !isTRUE(force) &&
      is.finite(state$last_render_elapsed) &&
      render_elapsed - state$last_render_elapsed < state$minimum_render_interval
  ) {
    return(invisible(NULL))
  }
  state$theme <- state$theme %||% .miss_cli_progress_theme()
  line <- .miss_cli_progress_line(
    forms = state$forms,
    form_index = state$form_index,
    form_fraction = state$form_fraction,
    overall_fraction = state$overall_fraction,
    phase = "form",
    theme = state$theme
  )
  .miss_cli_progress_open(state, line)
  if (is.null(state$id)) {
    return(invisible(NULL))
  }
  .miss_cli_progress_render(state, line, force = force)
}

.miss_cli_progress_finalize <- function(
  state,
  overall_fraction,
  force = TRUE
) {
  if (!is.environment(state) || !isTRUE(state$enabled) || isTRUE(state$finished)) {
    return(invisible(NULL))
  }

  state$form_index <- length(state$forms)
  state$form_fraction <- 1
  state$phase <- "finalizing"
  state$overall_fraction <- max(
    state$overall_fraction,
    .miss_cli_clamp_fraction(overall_fraction)
  )
  if (!isTRUE(state$dynamic)) {
    return(invisible(NULL))
  }
  render_elapsed <- unname(proc.time()[["elapsed"]])
  if (
    !isTRUE(force) &&
      is.finite(state$last_render_elapsed) &&
      render_elapsed - state$last_render_elapsed < state$minimum_render_interval
  ) {
    return(invisible(NULL))
  }
  state$theme <- state$theme %||% .miss_cli_progress_theme()
  line <- .miss_cli_progress_line(
    forms = state$forms,
    form_index = state$form_index,
    form_fraction = state$form_fraction,
    overall_fraction = state$overall_fraction,
    phase = "finalizing",
    theme = state$theme
  )
  .miss_cli_progress_open(state, line)
  if (is.null(state$id)) {
    return(invisible(NULL))
  }
  .miss_cli_progress_render(state, line, force = force)
}

.miss_cli_progress_finish <- function(
  state,
  result = c("done", "failed")
) {
  result <- match.arg(result)
  if (!is.environment(state) || !isTRUE(state$enabled) || isTRUE(state$finished)) {
    return(invisible(NULL))
  }

  state$finished <- TRUE
  if (identical(result, "done")) {
    state$form_index <- length(state$forms)
    state$form_fraction <- 1
    state$overall_fraction <- 1
  }
  line_phase <- if (
    identical(result, "failed") && identical(state$phase, "finalizing")
  ) {
    "finalizing_failed"
  } else {
    result
  }
  state$phase <- line_phase
  state$theme <- state$theme %||% .miss_cli_progress_theme()
  line <- .miss_cli_progress_line(
    forms = state$forms,
    form_index = state$form_index,
    form_fraction = state$form_fraction,
    overall_fraction = state$overall_fraction,
    phase = line_phase,
    theme = state$theme
  )

  opened_at_finish <- isTRUE(state$dynamic) && is.null(state$id)
  if (
    isTRUE(state$dynamic) &&
      is.null(state$id)
  ) {
    .miss_cli_progress_open(state, line)
  }
  if (isTRUE(state$dynamic)) {
    if (!is.null(state$id)) {
      if (
        isTRUE(opened_at_finish) ||
          isTRUE(state$has_rendered) ||
          identical(result, "failed")
      ) {
        invisible(try(
          cli::cli_progress_update(
            id = state$id,
            set = .miss_percent(state$overall_fraction),
            extra = list(line = line),
            force = isTRUE(opened_at_finish) || identical(result, "failed"),
            .envir = state$envir
          ),
          silent = TRUE
        ))
      }
      invisible(try(
        cli::cli_progress_done(
          id = state$id,
          result = result,
          .envir = state$envir
        ),
        silent = TRUE
      ))
    } else if (identical(result, "failed")) {
      invisible(try(cli::cat_line(line), silent = TRUE))
    }
  } else {
    invisible(try(cli::cat_line(line), silent = TRUE))
  }
  state$last_line <- line
  invisible(NULL)
}

.miss_cli_progress_render <- function(state, line, force = FALSE) {
  if (
    !isTRUE(state$dynamic) ||
      is.null(state$id) ||
      identical(line, state$last_line)
  ) {
    state$last_line <- line
    return(invisible(NULL))
  }

  render_elapsed <- unname(proc.time()[["elapsed"]])
  minimum_interval <- state$minimum_render_interval %||% 2
  if (
    !isTRUE(force) &&
      is.finite(state$last_render_elapsed) &&
      render_elapsed - state$last_render_elapsed < minimum_interval
  ) {
    return(invisible(NULL))
  }

  invisible(try(
    cli::cli_progress_update(
      id = state$id,
      set = .miss_percent(state$overall_fraction),
      extra = list(line = line),
      force = isTRUE(force),
      .envir = state$envir
    ),
    silent = TRUE
  ))
  state$last_line <- line
  state$last_render_elapsed <- render_elapsed
  state$has_rendered <- TRUE
  invisible(NULL)
}

.miss_cli_progress_line <- function(
  forms,
  form_index,
  form_fraction,
  overall_fraction,
  phase = c(
    "form",
    "finalizing",
    "done",
    "failed",
    "finalizing_failed"
  ),
  width = cli::console_width(),
  theme = NULL
) {
  phase <- match.arg(phase)
  forms <- .miss_chr_vec(forms)
  form_count <- length(forms)
  form_index <- max(1L, min(as.integer(form_index), form_count))
  form_fraction <- .miss_cli_clamp_fraction(form_fraction)
  overall_fraction <- .miss_cli_clamp_fraction(overall_fraction)
  width <- .miss_cli_progress_width(width)
  theme <- theme %||% .miss_cli_progress_theme()
  symbols <- theme$symbols
  completed_style <- theme$styles$completed
  active_style <- theme$styles$active
  overall_style <- theme$styles$overall
  pending_style <- theme$styles$pending
  failed_style <- theme$styles$failed
  overall_text <- overall_style(cli::style_bold(paste0(
    "OVERALL ",
    .miss_percent(overall_fraction),
    "%"
  )))
  overall_short <- overall_style(cli::style_bold(paste0(
    "O",
    .miss_percent(overall_fraction),
    "%"
  )))
  compact_separator <- pending_style(paste0(
    " ",
    symbols$separator,
    " "
  ))

  if (identical(phase, "done")) {
    full_line <- paste0(
      completed_style(symbols$tick),
      " ",
      completed_style(cli::style_bold("find_missing complete")),
      pending_style(paste0(" ", symbols$dot, " ")),
      form_count,
      "/",
      form_count,
      " forms",
      pending_style(paste0(" ", symbols$dot, " ")),
      overall_text
    )
    short_line <- paste0(
      completed_style(symbols$tick),
      " ",
      completed_style(cli::style_bold("complete")),
      pending_style(paste0(" ", symbols$separator, " ")),
      overall_text
    )
    narrow_line <- paste0(
      completed_style(symbols$tick),
      " ",
      completed_style(cli::style_bold("done")),
      compact_separator,
      overall_short
    )
    return(.miss_cli_progress_fit(
      list(full_line, short_line, narrow_line, overall_short),
      width = width
    ))
  }

  status <- if (identical(phase, "failed")) {
    "failed"
  } else if (identical(phase, "finalizing_failed")) {
    "finalizing_failed"
  } else if (identical(phase, "finalizing")) {
    "finalizing"
  } else {
    "active"
  }
  completed_count <- if (status %in% c("finalizing", "finalizing_failed")) {
    form_count
  } else if (identical(status, "failed")) {
    form_index - 1L
  } else if (form_fraction >= 1) {
    form_index
  } else {
    form_index - 1L
  }
  active_status <- if (identical(status, "failed")) {
    "failed"
  } else if (identical(status, "finalizing_failed")) {
    "failed"
  } else if (identical(status, "active") && form_fraction < 1) {
    "active"
  } else {
    "none"
  }
  pending_count <- max(
    0L,
    form_count - completed_count - as.integer(active_status != "none")
  )
  use_compact_constellation <- form_count > 8L || width < 72L
  constellation <- .miss_cli_progress_constellation(
    completed_count = completed_count,
    active_status = active_status,
    pending_count = pending_count,
    compact = use_compact_constellation,
    theme = theme
  )
  prefix <- if (width >= 54L) cli::style_bold("find_missing") else ""
  prefix_spacing <- if (nzchar(cli::ansi_strip(prefix))) "  " else ""
  separator <- pending_style(paste0("  ", symbols$separator, "  "))

  if (identical(status, "finalizing_failed")) {
    full_line <- paste0(
      prefix,
      prefix_spacing,
      constellation,
      "  ",
      failed_style(cli::style_bold("report assembly failed")),
      separator,
      overall_text
    )
    short_line <- paste0(
      .miss_cli_progress_constellation(
        completed_count = completed_count,
        active_status = "failed",
        pending_count = 0L,
        compact = TRUE,
        theme = theme
      ),
      "  ",
      failed_style(cli::style_bold("report failed")),
      separator,
      overall_text
    )
    narrow_line <- paste0(
      failed_style(symbols$failed),
      " ",
      failed_style(cli::style_bold("report")),
      compact_separator,
      overall_short
    )
    return(.miss_cli_progress_fit(
      list(full_line, short_line, narrow_line, overall_short),
      width = width
    ))
  }

  if (identical(status, "finalizing")) {
    label <- active_style(cli::style_bold(paste0(
      "forms 100% ",
      symbols$dot,
      " finalizing"
    )))
    full_line <- paste0(
      prefix,
      prefix_spacing,
      constellation,
      "  ",
      label,
      separator,
      overall_text
    )
    short_line <- paste0(
      .miss_cli_progress_constellation(
        completed_count = completed_count,
        active_status = "none",
        pending_count = 0L,
        compact = TRUE,
        theme = theme
      ),
      "  ",
      active_style(cli::style_bold("forms 100%")),
      separator,
      overall_text
    )
    narrow_line <- paste0(
      completed_style(symbols$tick),
      " ",
      active_style(cli::style_bold("F100%")),
      compact_separator,
      overall_short
    )
    return(.miss_cli_progress_fit(
      list(full_line, short_line, narrow_line, overall_short),
      width = width
    ))
  }

  form_name <- forms[[form_index]]
  form_suffix <- if (identical(status, "failed")) {
    paste0(" failed at ", .miss_percent(form_fraction), "%")
  } else {
    paste0(" ", .miss_percent(form_fraction), "%")
  }
  form_style <- if (identical(status, "failed")) failed_style else active_style
  marker <- switch(
    active_status,
    "active" = active_style(symbols$active),
    "failed" = failed_style(symbols$failed),
    completed_style(symbols$tick)
  )

  build_line <- function(prefix, constellation, form_suffix) {
    prefix_spacing <- if (nzchar(cli::ansi_strip(prefix))) "  " else ""
    fixed_width <- sum(c(
      cli::ansi_nchar(prefix, type = "width"),
      nchar(prefix_spacing, type = "width"),
      cli::ansi_nchar(constellation, type = "width"),
      2L,
      nchar(form_suffix, type = "width"),
      cli::ansi_nchar(separator, type = "width"),
      cli::ansi_nchar(overall_text, type = "width")
    ))
    form_width <- max(1L, min(24L, width - fixed_width))
    form_label <- cli::ansi_strtrim(form_name, form_width)
    paste0(
      prefix,
      prefix_spacing,
      constellation,
      "  ",
      form_style(cli::style_bold(paste0(form_label, form_suffix))),
      separator,
      overall_text
    )
  }

  line <- build_line(prefix, constellation, form_suffix)
  if (cli::ansi_nchar(line, type = "width") > width) {
    constellation <- .miss_cli_progress_constellation(
      completed_count = completed_count,
      active_status = active_status,
      pending_count = pending_count,
      compact = TRUE,
      theme = theme
    )
    line <- build_line("", constellation, form_suffix)
  }
  if (cli::ansi_nchar(line, type = "width") > width) {
    line <- build_line("", marker, form_suffix)
  }
  narrow_line <- paste0(
    marker,
    " ",
    form_style(cli::style_bold(paste0(
      "F",
      .miss_percent(form_fraction),
      "%"
    ))),
    compact_separator,
    overall_short
  )
  .miss_cli_progress_fit(
    list(line, narrow_line, overall_short),
    width = width
  )
}

.miss_cli_progress_constellation <- function(
  completed_count,
  active_status = c("active", "failed", "none"),
  pending_count,
  compact = FALSE,
  theme = NULL
) {
  active_status <- match.arg(active_status)
  completed_count <- max(0L, as.integer(completed_count))
  pending_count <- max(0L, as.integer(pending_count))
  theme <- theme %||% .miss_cli_progress_theme()
  symbols <- theme$symbols
  completed_style <- theme$styles$completed
  active_style <- theme$styles$active
  pending_style <- theme$styles$pending
  failed_style <- theme$styles$failed

  if (isTRUE(compact)) {
    pieces <- character()
    if (completed_count > 0L) {
      completed <- if (completed_count == 1L) {
        symbols$tick
      } else {
        paste0(symbols$tick, symbols$multiply, completed_count)
      }
      pieces <- c(pieces, completed_style(completed))
    }
    if (identical(active_status, "active")) {
      pieces <- c(pieces, active_style(symbols$active))
    } else if (identical(active_status, "failed")) {
      pieces <- c(pieces, failed_style(symbols$failed))
    }
    if (pending_count > 0L) {
      pending <- if (pending_count == 1L) {
        symbols$pending
      } else {
        paste0(symbols$pending, symbols$multiply, pending_count)
      }
      pieces <- c(pieces, pending_style(pending))
    }
    return(paste(pieces, collapse = " "))
  }

  pieces <- c(
    rep(completed_style(symbols$tick), completed_count),
    if (identical(active_status, "active")) active_style(symbols$active),
    if (identical(active_status, "failed")) failed_style(symbols$failed),
    rep(pending_style(symbols$pending), pending_count)
  )
  paste(pieces, collapse = " ")
}

.miss_cli_progress_symbols <- function(unicode = cli::is_utf8_output()) {
  if (isTRUE(unicode)) {
    return(list(
      tick = "\u2713",
      active = "\u25c9",
      pending = "\u25cb",
      failed = "\u2717",
      separator = "\u2502",
      multiply = "\u00d7",
      dot = "\u00b7"
    ))
  }

  list(
    tick = "v",
    active = "*",
    pending = ".",
    failed = "x",
    separator = "|",
    multiply = "x",
    dot = "-"
  )
}

.miss_cli_progress_palette <- function() {
  c(
    completed = "#22C55E",
    active = "#22D3EE",
    overall = "#3B82F6",
    pending = "#64748B",
    failed = "#EF4444"
  )
}

.miss_cli_progress_theme <- function() {
  palette <- .miss_cli_progress_palette()
  styles <- lapply(names(palette), .miss_cli_progress_style)
  names(styles) <- names(palette)
  list(
    symbols = .miss_cli_progress_symbols(),
    styles = styles
  )
}

.miss_cli_progress_style <- function(element) {
  palette <- .miss_cli_progress_palette()
  cli::make_ansi_style(palette[[element]])
}

.miss_cli_progress_width <- function(width) {
  if (length(width) == 0) {
    return(80L)
  }
  width <- suppressWarnings(as.integer(width[[1]]))
  if (is.na(width) || width < 1L) {
    return(80L)
  }
  width
}

.miss_cli_progress_fit <- function(lines, width) {
  for (line in lines) {
    if (cli::ansi_nchar(line, type = "width") <= width) {
      return(line)
    }
  }

  cli::ansi_strtrim(lines[[length(lines)]], width)
}

.miss_cli_report_form_progress <- function(
  progress_callback,
  stage,
  field_fraction = NULL,
  force = FALSE
) {
  if (is.null(progress_callback)) {
    return(invisible(NULL))
  }

  progress_callback(
    .miss_cli_form_fraction(
      stage = stage,
      field_fraction = field_fraction
    ),
    force = force
  )
  invisible(NULL)
}

.miss_cli_form_fraction <- function(stage, field_fraction = NULL) {
  switch(
    stage,
    "start" = 0,
    "context" = 0.10,
    "metadata" = 0.20,
    "eligibility" = 0.35,
    "row_checks" = 0.45,
    "form_checks" = 0.50,
    "field_checks" = 0.50 + 0.45 * .miss_cli_clamp_fraction(
      field_fraction %||% 0
    ),
    "field_complete" = 0.95,
    "complete" = 1,
    stop("Unknown progress stage `", stage, "`.", call. = FALSE)
  )
}

.miss_cli_overall_fraction <- function(
  form_index,
  form_fraction,
  form_count,
  forms_share = 0.95
) {
  if (form_count < 1) {
    return(0)
  }

  form_index <- max(1L, min(as.integer(form_index), as.integer(form_count)))
  form_fraction <- .miss_cli_clamp_fraction(form_fraction)
  forms_share <- .miss_cli_clamp_fraction(forms_share)
  forms_share * ((form_index - 1 + form_fraction) / form_count)
}

.miss_cli_clamp_fraction <- function(x) {
  if (length(x) == 0) {
    return(0)
  }
  x <- suppressWarnings(as.numeric(x[[1]]))
  if (is.na(x)) {
    return(0)
  }
  max(0, min(x, 1))
}

.miss_percent <- function(x) {
  as.integer(round(.miss_cli_clamp_fraction(x) * 100))
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
