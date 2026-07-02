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
#' `rcon$instruments()`, `rcon$mapping()` / `rcon$mappings()`,
#' `rcon$repeatInstrumentEvent()`, and `rcon$projectInformation()` when present.
#'
#' Checkbox fields are treated as one REDCap root field. A checkbox root is
#' considered answered when at least one exported child column
#' (`field___choice`) is selected. Unselected sibling checkboxes are not flagged
#' as missing once the root question has at least one selected child.
#'
#' Validation is split between REDCap-specific R preprocessing and `pointblank`
#' reporting. The package builds validation rows with:
#' \describe{
#'   \item{`validation_level`}{`"row"`, `"form"`, or `"field"`.}
#'   \item{`validation_check`}{One of `"event-row-started"`,
#'     `"instance-row-started"`, `"form-started"`, `"form-complete"`, or
#'     `"field-complete"`.}
#'   \item{`validation_check_type`}{`"on-route"` or `"detour"`.}
#'   \item{`validation_passed`}{Whether that row passed its validation check.}
#' }
#'
#' The checks are assessed in registry order. Failed `"on-route"` checks remove
#' the record/event/repeat/form context from every downstream assessment,
#' regardless of the downstream validation level or check type. `form-complete`
#' is a `"detour"` check: it reports whether all expected fields are complete,
#' but a failed `form-complete` context still flows into `field-complete`.
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
#'
#' @return A list with class `"redcapmissing"` containing:
#' \describe{
#'   \item{`agent`}{An interrogated `pointblank` agent.}
#'   \item{`missing`}{A tibble of failed rows from the `pointblank` extract,
#'     keyed by record, event, repeat context, form, and field.}
#'   \item{`validation_rows`}{The full table supplied to `pointblank`, including
#'     validation metadata and the `validation_passed` result column.}
#'   \item{`event_row_started_checks`, `event_row_started_failures`}{All
#'     event-row-started validation rows and their failed rows.}
#'   \item{`instance_row_started_checks`,
#'     `instance_row_started_failures`}{All instance-row-started validation
#'     rows and their failed rows.}
#'   \item{`form_started_checks`, `form_started_failures`}{All form-started
#'     validation rows and their failed rows.}
#'   \item{`form_complete_checks`, `form_complete_failures`}{All form-complete
#'     validation rows and their failed rows.}
#'   \item{`field_complete_checks`, `field_complete_failures`}{All
#'     field-complete validation rows and their failed rows.}
#'   \item{`field_plan`}{The metadata-derived field plan for the requested
#'     forms, including checkbox child export columns and a `form` column.}
#'   \item{`required_fields`}{Whether the report was limited to REDCap-required
#'     fields.}
#'   \item{`events`}{Named list of REDCap events actually used for assessment
#'     by form, or `character(0)` for forms where no event restriction applied.}
#'   \item{`instances`}{Named list of expanded repeat-instance IDs by form, or
#'     `NULL` for forms without requested repeating contexts.}
#'   \item{`ignored_fields`}{Root field names skipped because of
#'     `ignore_fields`.}
#'   \item{`ignored_ids`}{Record IDs skipped because of `ignore_ids`.}
#'   \item{`project`}{Named list of compact REDCap project structures inferred
#'     from `rcon`, one per requested form.}
#'   \item{`forms`}{The assessed form names.}
#'   \item{`form_labels`}{Named REDCap instrument labels for `forms`.}
#'   \item{`id_col`}{The REDCap record identifier field.}
#'   \item{`system_fields`}{The REDCap system field names used internally.}
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
#' baseline_missing$agent
#' baseline_missing$missing
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
#' }
#'
#' @export
find_missing <- function(
  data,
  rcon,
  forms,
  events = NULL,
  required_fields = TRUE,
  ignore_fields = NULL,
  ignore_ids = NULL,
  exclude_types = "descriptive",
  instances = NULL
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

  if (!requireNamespace("pointblank", quietly = TRUE)) {
    stop(
      "Install the pointblank package before building a missing-field report.",
      call. = FALSE
    )
  }

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
  records <- tibble::as_tibble(data)
  all_records <- records

  meta <- .miss_get_metadata(rcon)
  .miss_check_metadata(meta)
  id_col <- meta$field_name[[1]]
  if (!id_col %in% names(records)) {
    stop(
      "The REDCap record identifier column `",
      id_col,
      "` is not present in `data`.",
      call. = FALSE
    )
  }

  # Remove caller-specified records before branch evaluation or validation.
  ignore_ids <- unique(.miss_chr_vec(ignore_ids %||% character()))
  if (length(ignore_ids) > 0) {
    records <- records[
      !.miss_chr_vec(records[[id_col]]) %in% ignore_ids,
      ,
      drop = FALSE
    ]
    all_records <- all_records[
      !.miss_chr_vec(all_records[[id_col]]) %in% ignore_ids,
      ,
      drop = FALSE
    ]
  }

  form_reports <- lapply(forms, function(form) {
    .miss_build_form_report(
      records = records,
      all_records = all_records,
      meta = meta,
      rcon = rcon,
      form = form,
      events = event_settings$values[[form]],
      required_fields = required_fields,
      ignore_fields = ignore_fields,
      exclude_types = exclude_types,
      instances = instance_settings$values[[form]],
      instances_explicit = instance_settings$explicit[[form]]
    )
  })
  names(form_reports) <- forms

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

  validation_rows <- .miss_bind_report_component(
    reports = form_reports,
    component = "validation_rows"
  )
  form_labels <- stats::setNames(
    vapply(form_reports, `[[`, character(1), "form_label"),
    forms
  )

  # Let pointblank own the validation object and failed-row extract.
  agent <- pointblank::create_agent(
    tbl = validation_rows,
    tbl_name = "redcapmissing_expected_fields",
    label = "Branch-aware missing fields"
  )
  for (form_report in form_reports) {
    agent <- .miss_add_form_validation_steps(agent, form_report)
  }
  agent <- pointblank::interrogate(
    agent,
    extract_failed = TRUE,
    progress = FALSE
  )
  agent <- .miss_annotate_agent_validation_set(
    agent = agent,
    validation_rows = validation_rows,
    form_labels = form_labels
  )

  missing <- .miss_get_failed_rows(agent)
  ignored_fields <- unique(unlist(.miss_named_report_component(
    form_reports,
    "ignored_fields"
  ), use.names = FALSE))
  ignored_fields <- ignored_fields %||% character()

  out <- list(
    agent = agent,
    missing = missing,
    validation_rows = validation_rows,
    event_row_started_checks = .miss_bind_report_component(
      form_reports,
      "event_row_started_checks"
    ),
    event_row_started_failures = .miss_bind_report_component(
      form_reports,
      "event_row_started_failures"
    ),
    instance_row_started_checks = .miss_bind_report_component(
      form_reports,
      "instance_row_started_checks"
    ),
    instance_row_started_failures = .miss_bind_report_component(
      form_reports,
      "instance_row_started_failures"
    ),
    form_started_checks = .miss_bind_report_component(
      form_reports,
      "form_started_checks"
    ),
    form_started_failures = .miss_bind_report_component(
      form_reports,
      "form_started_failures"
    ),
    form_complete_checks = .miss_bind_report_component(
      form_reports,
      "form_complete_checks"
    ),
    form_complete_failures = .miss_bind_report_component(
      form_reports,
      "form_complete_failures"
    ),
    field_complete_checks = .miss_bind_report_component(
      form_reports,
      "field_complete_checks"
    ),
    field_complete_failures = .miss_bind_report_component(
      form_reports,
      "field_complete_failures"
    ),
    field_plan = .miss_bind_report_component(form_reports, "field_plan"),
    required_fields = required_fields,
    events = .miss_named_report_component(form_reports, "events"),
    instances = .miss_named_report_component(form_reports, "instances"),
    ignored_fields = ignored_fields,
    ignored_ids = ignore_ids,
    project = .miss_named_report_component(form_reports, "project"),
    forms = forms,
    form_labels = form_labels,
    id_col = id_col,
    system_fields = form_reports[[1]]$system_fields
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
  form,
  events,
  required_fields,
  ignore_fields,
  exclude_types,
  instances,
  instances_explicit
) {

  project <- .miss_get_project(rcon = rcon, meta = meta, form = form)
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

  event_checks <- .miss_build_event_check_rows(
    records = all_records,
    form_records = records,
    project = project,
    form = form
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
    instances = instances
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
  validation_rows <- dplyr::bind_rows(
    event_checks,
    repeat_checks,
    form_checks,
    form_complete_checks,
    expected
  )
  validation_rows <- .miss_add_validation_context(validation_rows)

  list(
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
    field_plan = field_plan,
    events = project$events %||% character(),
    instances = instances,
    instances_defaulted = resolved_instances$defaulted,
    ignored_fields = ignore_roots,
    project = project,
    form = form,
    form_label = project$form_label,
    id_col = project$id_col,
    system_fields = project$system_fields
  )
}

.miss_add_form_validation_steps <- function(agent, form_report) {
  form <- form_report$form
  registry <- .redcapmissing_registry_data()

  for (row in seq_len(nrow(registry))) {
    check <- registry[row, , drop = FALSE]
    component <- paste0(check$component_stem, "_checks")
    rows <- form_report[[component]] %||% .miss_empty_expected()
    keep_zero <- check$validation_check %in% c(
      "event-row-started",
      "form-started",
      "field-complete"
    )
    if (nrow(rows) > 0 || isTRUE(keep_zero)) {
      agent <- .miss_add_validation_step(
        agent = agent,
        rows = rows,
        validation_check = check$validation_check,
        keep_zero = keep_zero,
        form = form
      )
    }
  }

  agent
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

.miss_step_id <- function(form, validation_check) {
  paste0(form, "_", validation_check)
}
