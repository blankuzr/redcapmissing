#' Run a REDCap missingness assessment plan
#'
#' `run_plan()` evaluates the `assessible_targets` stored in a
#' [plan_from_data()] or [plan_explicit()] result against supplied REDCap data.
#' Target rows come from `plan$assessible_targets`.
#'
#' @param plan A validated `redcapmissing_plan`. Its project ID, schema version,
#'   structure fingerprint, stored instruments, target schema, target
#'   uniqueness, target provenance, and deterministic ordering are revalidated
#'   against `rcon`; malformed plans and plans edited by hand are rejected.
#' @param data A data frame containing physical REDCap rows and every response,
#'   checkbox child, and branching dependency column needed by the frozen
#'   targets. Instruments that occur in `plan$instruments` but have no target do
#'   not impose response-column requirements. Structural columns follow
#'   [plan_from_data()] normalization, and all columns must use ordinary atomic
#'   vector storage. A correctly structured empty data frame is allowed. Plan
#'   targets stay fixed when `data` contains additional rows; absent planned
#'   rows remain targets and follow the checks below.
#' @param rcon A `redcapAPI` connection inheriting from
#'   `redcapApiConnection`, as created by [redcapAPI::redcapConnection()], or
#'   `redcapOfflineConnection`, as created by [redcapAPI::offlineConnection()]
#'   or [redcapAPI::readPreservedProject()]. It must represent the same
#'   unchanged project structure as `plan` and expose project identity,
#'   longitudinal status, metadata, instruments, repeat configuration, and, for
#'   longitudinal projects, arms, events, and instrument to event mappings.
#' @param required_fields One nonmissing logical value. `TRUE` limits
#'   field-complete assessment to fields marked required in REDCap metadata;
#'   `FALSE` retains required and optional fields before the remaining policy
#'   exclusions.
#' @param ignore_fields `NULL`, `character(0)`, or a character vector of unique,
#'   nonmissing, nonblank, unpadded raw metadata field names on instruments
#'   represented by frozen targets. Supply checkbox root names. Exported
#'   checkbox child names are invalid. Unknown names, fields belonging only to
#'   zero-target instruments, and names unused after earlier policy steps are
#'   errors.
#' @param exclude_types `NULL`, `character(0)`, or a character vector of unique,
#'   nonmissing, nonblank, unpadded REDCap metadata field types. `NULL` and
#'   `character(0)` exclude no types. Explicitly supplied unknown or unused types
#'   are errors. The default removes `"descriptive"` fields when present.
#' @param verified `NULL`, or a data frame such as the result of
#'   [export_data_quality()], containing the nine required columns documented
#'   in **Verification**. Extra columns are ignored. It must be supplied
#'   together with `verified_user`; a complete empty table is valid.
#' @param verified_user `NULL`, or one exact, nonmissing, nonblank, unpadded
#'   character username. Matching is case sensitive. It must be supplied
#'   together with `verified`.
#' @param details One nonmissing logical value. `TRUE` retains one row per target
#'   for each of the first three checks, field level rows for each applicable
#'   field assessed by `field-complete`, and one target level `field-complete`
#'   row when that check is not applicable or not reached. Fields closed by
#'   branching logic have no detail row because they are not assessed. `FALSE`
#'   stores `NULL` in `details`. Both settings produce identical targets,
#'   summaries, missing rows, verification audit counts, and effective outcomes.
#' @param progress One nonmissing logical value controlling the twelve stage
#'   interactive progress bar. Progress cleanup is registered on exit.
#'
#' @section Execution stages:
#' `diagnostics` records these operations in fixed order:
#'
#' 1. Validate plan, data, and `rcon`.
#' 2. Validate and normalize verification.
#' 3. Resolve instrument-start fields.
#' 4. Resolve field-complete fields.
#' 5. Join assessible_targets to physical rows.
#' 6. Run `event-row-started`.
#' 7. Run `repeat-instance-row-started`.
#' 8. Run `instrument-started`.
#' 9. Run raw `field-complete`.
#' 10. Apply verification.
#' 11. Aggregate effective results.
#' 12. Construct the report.
#'
#' A newer record snapshot is permitted when the project identity and structure
#' fingerprint remain unchanged.
#'
#' @section Checks and gating:
#' Check statuses are `"passed"`, `"failed"`, `"not applicable"`, and
#' `"not reached"`. Validation levels are `"event:instrument"` and
#' `"event:instrument:instance"`.
#'
#' Every longitudinal target first receives `event-row-started`, based on any
#' physical row for its record and event. Classic targets mark that check not
#' applicable. A target with a positive `repeat_instance` receives
#' `repeat-instance-row-started` only after its event gate passes; a target with
#' `repeat_instance = NA_integer_` marks that check not applicable. A failed
#' event or repeat gate makes downstream checks not reached. An absent classic
#' target with `repeat_instance = NA_integer_` has both row checks marked not
#' applicable and fails `instrument-started`.
#'
#' `instrument-started` uses an independent detection set: every data entry
#' metadata field on an instrument represented by a frozen target except the
#' project record ID field and `descriptive` or `calc` fields. Checkbox roots
#' require a nonblank, unambiguous metadata choice definition and expand to
#' exported child columns.
#' An ordinary detection field starts the instrument when its response is
#' nonmissing. A checkbox root starts the instrument when at least one child is
#' selected. Child values `0`, `"unchecked"`, `"false"`, and `"no"`, matched in
#' any letter case, are unselected. A targeted instrument with no usable
#' detection fields, or a missing detection column, is an error. A selected
#' instrument with no frozen target is not evaluated and imposes no
#' response-column requirement. `field-complete` runs only after this check passes.
#'
#' @section Field-complete policy:
#' The field set is resolved in this exact order:
#'
#' ```text
#' all fields on instruments represented by frozen targets
#' -> retain required fields when required_fields = TRUE
#' -> exclude_types removal
#' -> ignore_fields removal
#' ```
#'
#' These three arguments determine the field set for `field-complete`. The plan
#' supplies targets, physical rows supply the two row checks, and the independent
#' detection set supplies `instrument-started`. An optional, ignored, or excluded
#' type field may establish that an instrument started. If no fields remain,
#' `field-complete` is `"not applicable"`, its reason is
#' `"no fields remain after field policy"`, its counts are zero, and its
#' rates are `NA_real_`.
#'
#' Branching logic is evaluated in the target's exact event/repeat context,
#' including cross event references. An unqualified cross event reference uses
#' the unique source row whose `redcap_repeat_instance` is missing when the
#' source event also contains repeated rows. When no such row exists, one
#' source row with a positive repeat instance is unambiguous and may be used;
#' multiple source rows with positive repeat instances are ambiguous and raise
#' a project error because no single instance can be selected.
#' REDCap `year(date)` expressions are supported for date and datetime values
#' in YMD, MDY, or DMY order with hyphen or slash separators. Blank or invalid
#' date values return a missing year and do not satisfy the branch.
#' Fields closed by branching logic are not assessed.
#' Checkbox roots are complete when at least one exported child choice is
#' selected. Every targeted response, checkbox child, and branching dependency
#' column needed by those target fields must be present; absence is an error. A
#' branching dependency remains required when its field belongs to a
#' non-targeted instrument.
#'
#' @section Response missingness:
#' One missingness predicate is used for ordinary instrument detection fields
#' and ordinary field completeness. Checkbox roots use the selection rules above
#' for both checks. Typed R missing values, factor `NA`, logical `NA`, numeric
#' `NA`/`NaN`, missing dates or timestamps, empty strings, and strings containing
#' only whitespace are missing.
#' `Inf` and `-Inf` are not automatically missing. Literal strings `"NA"`,
#' `"N/A"`, `"NULL"`, `"."`, and `"-999"` are ordinary nonmissing responses
#' unless converted upstream under a separate REDCap missing code policy.
#' Physical row presence is determined solely by structural identity and is
#' independent of response missingness.
#'
#' @section Verification:
#' This optional integration uses Data Resolution Workflow history from
#' [export_data_quality()]. Pass its returned table directly as `verified`,
#' together with the exact `verified_user`. Projects without this workflow can
#' leave both arguments `NULL`.
#'
#' Each of these column names must occur exactly once. Extra columns are
#' ignored, and a complete zero-row table is valid regardless of column storage.
#'
#' | Column | Accepted storage and values | Internal normalization |
#' |---|---|---|
#' | `project_id` | Character, integer, or finite whole number double; exact project match | Character |
#' | `record` | Character, factor, integer, or finite double; nonmissing, nonblank, unpadded | Character; leading zeros in character IDs are preserved |
#' | `event_id` | Positive character digits without leading zeros, integer, or whole number double; missing/blank also accepted in classic projects | Raw event name in longitudinal projects; a consistent classic internal event ID becomes `NA_character_` |
#' | `field_name` | Character; nonblank, unpadded raw field name | Character |
#' | `repeat_instrument` | Character raw instrument, or blank/typed missing where inapplicable | Character or `NA_character_` |
#' | `instance` | Positive character digits without leading zeros, integer, or whole number double; missing/blank where inapplicable | Integer; native placeholder `1` becomes `NA_integer_` for a nonrepeating target |
#' | `ts` | Nonmissing `POSIXct`, or character date and time through seconds, optional fractional seconds, and optional `Z` or numeric offset | `POSIXct` in UTC; text without a timezone is interpreted as UTC |
#' | `current_query_status` | Character, blank, or typed missing; nonblank values must be unpadded | Character, case preserved; blank becomes `NA_character_` |
#' | `username` | Character, blank, or typed missing; nonblank values must be unpadded | Character, case preserved; blank becomes `NA_character_` |
#'
#' Integer dimensions must be within R's integer range. Factors are rejected
#' except for `record`. Nullable text and integer dimensions accept an
#' all-missing logical, integer, or double column; `NaN` is invalid.
#' Timestamp text uses a space or `T` between date and time; numeric offsets
#' use `+HHMM`, `-HHMM`, `+HH:MM`, or `-HH:MM`.
#'
#' The table structure and project identity are checked first. Evidence may
#' include contexts outside `plan`: well-formed nonmatching records, events,
#' instruments, repeat contexts, and fields are silently discarded before
#' resolution contents are validated. They do not add assessment targets.
#' Malformed identifiers still raise errors. In a classic project, candidate
#' rows may use missing event IDs or one consistent positive internal event ID.
#' Actual repeating instances and their instrument/event relationships are
#' preserved.
#'
#' For matching contexts, issue-only rows and entries without a username are
#' quietly ignored. Entries with an identifiable reviewer require a valid
#' timestamp, including entries from other reviewers.
#'
#' Rows are then filtered to exact `verified_user`, grouped by normalized
#' record, event, repeat instrument, repeat instance, and field, and reduced to
#' the latest timestamp. Identical rows tied at the latest timestamp collapse;
#' conflicting latest ties error. A missing or blank latest status quietly
#' prevents an override, as does any status other than exact `"VERIFIED"`.
#' An older verification is never used instead. Issue-level `query_status`
#' and `assigned_username` do not affect this selection.
#'
#' An override is eligible only when the target has passed its upstream gates
#' and instrument start check, the field remains selected and applicable, and
#' its raw `field-complete` disposition is `"failed"`. Eligible evidence
#' changes the effective disposition to `"passed"`.
#'
#' The `verification` component contains seven audit fields:
#' `enabled`, `verified_user`, `input_rows`, `user_rows`,
#' `latest_user_rows`, `verified_rows`, and `overrides_applied`.
#' `input_rows` counts all supplied rows; `user_rows` counts all supplied
#' rows whose username exactly matches the selected reviewer, including
#' contexts outside the plan. `latest_user_rows` counts in-plan contexts
#' retained after latest-entry selection and duplicate collapse, including
#' missing latest statuses. `verified_rows` counts those retained entries
#' with exact `"VERIFIED"`; `overrides_applied` counts failed field results
#' actually changed to passes. Raw supplied history is excluded from the report.
#'
#' With both arguments `NULL`, verification is disabled: `enabled = FALSE`,
#' `verified_user = NA_character_`, and every count is `0L`.
#' A complete empty table with `verified_user` enables verification and
#' retains that username, with every count `0L`.
#'
#'
#' @section Return value:
#' A `redcapmissing` object with exactly `plan`, `target_results`, `summary`,
#' `missing`, `verification`, `diagnostics`, `details`, and `settings`. The stored components
#' exclude source `data`, raw `verified`, tokens, and the live connection.
#' `settings` contains integer `schema_version = 1L`, logical `required_fields`,
#' and character `ignore_fields` and `exclude_types` stored as sorted sets.
#' Empty selections use `character()`. These settings and `details = TRUE`
#' allow [compare_reports()] to check compatibility and compare stored outcomes.
#' Reports saved before settings were introduced remain readable through the
#' existing report accessors and formatters.
#'
#' `target_results` has one row per `assessible_targets` row and exactly:
#'
#' | Columns | Storage |
#' |---|---|
#' | `record_id`, `instrument`, `redcap_event_name`, `repeat_instrument`, `target_source` | Character |
#' | `repeat_instance`, `fields_assessed`, `fields_failed` | Integer |
#' | `event_row_started`, `repeat_instance_row_started`, `instrument_started`, `field_complete` | Character check status |
#' | `field_applicability_reason` | Character; reason that `field-complete` is not applicable after field policy or branching, otherwise typed missing |
#'
#' `summary` has exact columns `redcap_event_name`, `instrument`,
#' `repeat_instrument`, `repeat_instance`, `validation_level`,
#' `validation_check`, `status`, `reason`, `assessed`, `passed`, `failed`,
#' `pass_rate`, and `fail_rate`. Structural/context columns, codes, `status`, and
#' `reason` are character except integer `repeat_instance`; counts are integer
#' and rates are double. `status` is `"assessed"` or `"not applicable"`;
#' `reason` is typed missing unless the check is not applicable. Rates are
#' `NA_real_` when nothing was assessed.
#'
#' `missing` contains effective unresolved failures only, with exact columns
#' `record_id`, `redcap_event_name`, `repeat_instrument`, `repeat_instance`,
#' `validation_context`, `instrument`, `validation_check`, `field_name`,
#' `field_label`, `field_type`, `branching_logic`, and `url`. All are character
#' except integer `repeat_instance`; inapplicable structural values use
#' `NA_character_` or `NA_integer_`.
#'
#' `verification` is a named audit list containing logical `enabled`, character
#' `verified_user`, and integer `input_rows`, `user_rows`, `latest_user_rows`,
#' `verified_rows`, and `overrides_applied`. `diagnostics` is a tibble with
#' integer `stage`, character `operation`, logical `completed`, and double
#' `elapsed_seconds`.
#'
#' With `details = TRUE`, `details` is a tibble with exact columns
#' `record_id`, `instrument`, `redcap_event_name`, `repeat_instrument`,
#' `repeat_instance`, `target_source`, `validation_level`, `validation_check`,
#' `field_name`, `field_label`, `field_type`, `branching_logic`,
#' `branch_satisfied`, `value_summary`, `raw_disposition`,
#' `verification_applied`, `effective_disposition`, and `reason`. Repeat instance
#' is integer; branch and verification flags are logical; all other columns are
#' character. The first three checks contribute one row per target. An assessed
#' `field-complete` check contributes one row per applicable field. A check that
#' is not applicable or not reached contributes one target level row with typed
#' missing field columns. Fields closed by branching logic contribute no row.
#'
#' `branch_satisfied` is `TRUE` on every assessed field row and typed logical
#' `NA` on target level check rows. `value_summary` is the character
#' representation of an ordinary field value, including raw free text. For a
#' checkbox root, it is a comma separated list of selected exported child
#' column names; an assessed checkbox with no selected choices uses an empty
#' string. Target level check rows use typed missing `value_summary`.
#' `raw_disposition` and `effective_disposition` each accept `"passed"`,
#' `"failed"`, `"not applicable"`, or `"not reached"`. The raw value records
#' assessment before verification, while the effective value includes an
#' eligible verification override. `verification_applied` is `TRUE` exactly on
#' an overridden field row.
#'
#' `target_results$field_applicability_reason` is either typed missing,
#' `"no fields remain after field policy"`, or
#' `"no fields apply after branching logic"`. The `details$reason` column
#' explains a target level check that is not applicable, using
#' `"not applicable for classic project"`, `"not a repeating target"`, or one
#' of those two exact field reasons. `summary$reason` carries the corresponding
#' reason for an aggregated check row.
#'
#' With `details = TRUE`, the object stores raw free text and other sensitive
#' field values in `value_summary`. Protect it under the same access, retention,
#' and disclosure controls as the source REDCap export. With `details = FALSE`,
#' `details` is `NULL`. The remaining report components contain record
#' identifiers, unresolved failure contexts, and may contain direct REDCap data
#' entry URLs in `missing$url`; protect either mode according to project data
#' handling requirements.
#'
#' @section Conditions:
#' Public validation failures inherit from `redcapmissing_error`, with specific
#' subclasses `redcapmissing_error_argument`, `redcapmissing_error_schema`,
#' `redcapmissing_error_project`, `redcapmissing_error_schedule`,
#' `redcapmissing_error_plan`, and `redcapmissing_error_verification` according
#' to the failing boundary.
#'
#' @return A `redcapmissing` object as described in **Return value**.
#'
#' @examples
#' \dontrun{
#' # plan, records, and rcon are caller supplied.
#' report <- run_plan(plan, records, rcon, progress = FALSE)
#' get_summary(report)
#' }
#'
#' @seealso [plan_from_data()], [plan_explicit()], [registry()],
#'   [get_summary()], [get_missing()]
#' @export
run_plan <- function(
  plan, data, rcon, required_fields = TRUE, ignore_fields = NULL,
  exclude_types = "descriptive", verified = NULL, verified_user = NULL,
  details = FALSE, progress = interactive()
) {
  exclude_types_was_missing <- missing(exclude_types)
  .run_plan_normalize_logical_argument(required_fields, "required_fields")
  .run_plan_normalize_logical_argument(details, "details")
  .run_plan_normalize_logical_argument(progress, "progress")
  ignore_fields <- .run_plan_normalize_character_argument(ignore_fields, "ignore_fields")
  exclude_types <- .run_plan_normalize_character_argument(exclude_types, "exclude_types")

  stage_names <- c(
    "Validate plan, data, and rcon",
    "Validate and normalize verification",
    "Resolve instrument-start fields",
    "Resolve field-complete fields",
    "Join assessible_targets to physical rows",
    "Run event-row-started",
    "Run repeat-instance-row-started",
    "Run instrument-started",
    "Run raw field-complete",
    "Apply verification",
    "Aggregate effective results",
    "Construct the report"
  )
  elapsed <- rep(NA_real_, 12L); stage <- 0L; progress_id <- NULL
  if (isTRUE(progress)) {
    progress_id <- cli::cli_progress_bar("Running assessment plan", total = 12L)
    on.exit(try(cli::cli_progress_done(id = progress_id), silent = TRUE), add = TRUE)
  }
  complete_stage <- function(started_at) {
    stage <<- stage + 1L
    elapsed[[stage]] <<- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
    if (!is.null(progress_id)) cli::cli_progress_update(id = progress_id, set = stage)
  }

  started <- Sys.time()
  if (missing(plan) || is.null(plan)) .condition_signal_error("`plan` is required.", "plan")
  if (missing(data) || is.null(data) || !is.data.frame(data)) {
    .condition_signal_error("`data` must be a data frame or tibble.", "schema")
  }
  if (missing(rcon) || is.null(rcon)) .condition_signal_error("`rcon` is required.", "project")
  snapshot <- .project_structure_build_snapshot(rcon)
  plan <- .plan_validate_object(plan, snapshot = snapshot)
  targets <- tibble::as_tibble(plan$assessible_targets)
  target_instruments <- unique(targets$instrument)
  target_indices_by_instrument <- split(
    seq_len(nrow(targets)),
    factor(targets$instrument, levels = target_instruments),
    drop = FALSE
  )
  normalized_data <- .record_normalize_export(data, snapshot, require_nonempty = FALSE,
                                         response_columns = NULL)
  if (is.list(normalized_data) && "data" %in% names(normalized_data)) {
    normalized_data <- normalized_data$data
  }
  normalized_data <- tibble::as_tibble(normalized_data)
  complete_stage(started)

  started <- Sys.time()
  verification <- .verification_prepare_contexts(verified, verified_user, snapshot, plan)
  complete_stage(started)

  started <- Sys.time()
  metadata <- tibble::as_tibble(snapshot$metadata)
  id_field <- snapshot$project$record_id_field %||% as.character(metadata$field_name[[1L]])
  field_dictionary <- .metadata_build_field_dictionary(metadata)
  detection <- .instrument_started_build_detection_plan(
    metadata,
    target_instruments,
    id_field,
    field_dictionary = field_dictionary
  )
  detection_export_fields <- unique(unlist(
    detection$export_fields,
    use.names = FALSE
  ))
  .run_plan_require_response_columns(
    normalized_data, detection_export_fields, "instrument start detection"
  )
  complete_stage(started)

  started <- Sys.time()
  field_plan <- .field_complete_build_field_plan(
    metadata,
    target_instruments,
    required_fields,
    ignore_fields,
    exclude_types,
    strict_exclude_types = !exclude_types_was_missing,
    field_dictionary = field_dictionary
  )
  selected_export_fields <- unique(unlist(
    field_plan$export_fields,
    use.names = FALSE
  ))
  .run_plan_require_response_columns(
    normalized_data, selected_export_fields, "field-complete assessment"
  )
  branch_references <- .branching_logic_extract_references(
    field_plan$branching_logic
  )
  branch_dependencies <- .run_plan_build_branch_dependencies(
    metadata,
    field_plan$rows,
    references = branch_references,
    field_dictionary = field_dictionary
  )
  .run_plan_require_response_columns(
    normalized_data,
    branch_dependencies$global_export_fields,
    "branching logic evaluation"
  )
  structural_fields <- c(
    ".rcm_record_id",
    unname(unlist(.record_list_system_fields(), use.names = FALSE))
  )
  runtime_fields <- unique(c(
    structural_fields,
    detection_export_fields,
    selected_export_fields,
    branch_dependencies$global_export_fields
  ))
  normalized_data <- normalized_data[, runtime_fields, drop = FALSE]
  response_masks <- .run_plan_response_masks_build(normalized_data)
  complete_stage(started)

  started <- Sys.time()
  joins <- .assessible_target_join_records(targets, normalized_data,
                                 isTRUE(snapshot$project$longitudinal))
  complete_stage(started)

  started <- Sys.time()
  event_status <- if (isTRUE(snapshot$project$longitudinal)) {
    as.character(ifelse(joins$event_present, "passed", "failed"))
  } else rep("not applicable", nrow(targets))
  complete_stage(started)

  started <- Sys.time()
  has_repeat_instance <- !is.na(targets$repeat_instance)
  event_gate_pass <- if (isTRUE(snapshot$project$longitudinal)) {
    joins$event_present
  } else {
    rep(TRUE, nrow(targets))
  }
  repeat_status <- rep("not applicable", nrow(targets))
  repeat_status[has_repeat_instance & event_gate_pass] <- ifelse(
    joins$target_present[has_repeat_instance & event_gate_pass],
    "passed", "failed")
  repeat_status[has_repeat_instance & !event_gate_pass] <- "not reached"
  complete_stage(started)

  started <- Sys.time()
  upstream_pass <- event_gate_pass &
    (!has_repeat_instance | joins$target_present)
  instrument_status <- rep("not reached", nrow(targets))
  missing_target_without_repeat <- !has_repeat_instance & event_gate_pass &
    !joins$target_present
  instrument_status[missing_target_without_repeat] <- "failed"
  instrument_status <- .instrument_started_evaluate_targets(
    target_indices_by_instrument = target_indices_by_instrument,
    target_row = joins$target_row,
    upstream_pass = upstream_pass,
    response_masks = response_masks,
    detection_fields = detection$export_fields,
    initial_status = instrument_status
  )
  complete_stage(started)

  started <- Sys.time()
  assessment <- .field_complete_assess_targets(
    target_indices_by_instrument = target_indices_by_instrument,
    target_row = joins$target_row,
    instrument_status = instrument_status,
    normalized_data = normalized_data,
    response_masks = response_masks,
    metadata = metadata,
    field_plan = field_plan,
    field_dictionary = field_dictionary,
    branch_fields_by_instrument =
      branch_dependencies$export_fields_by_instrument,
    retain_passed_fields = isTRUE(details)
  )
  field_status <- assessment$field_status
  fields_assessed <- assessment$fields_assessed
  fields_failed <- assessment$fields_failed
  field_reason <- assessment$field_reason
  field_rows <- assessment$field_rows
  complete_stage(started)

  started <- Sys.time()
  overrides <- 0L
  if (nrow(field_rows) && nrow(verification$contexts)) {
    apply <- .verification_match_fields(
      field_rows,
      targets,
      verification$contexts
    )
    if (any(apply)) {
      field_rows$verification_applied[apply] <- TRUE
      field_rows$effective_disposition[apply] <- "passed"
      overrides <- sum(apply)

      overridden_targets <- field_rows$.target_row[apply]
      affected_targets <- unique(overridden_targets)
      overridden_counts <- tabulate(
        match(overridden_targets, affected_targets),
        nbins = length(affected_targets)
      )
      fields_failed[affected_targets] <-
        fields_failed[affected_targets] - overridden_counts
      field_status[affected_targets] <- ifelse(
        fields_failed[affected_targets] > 0L,
        "failed",
        "passed"
      )
    }
  }
  verification$audit$overrides_applied <- as.integer(overrides)
  complete_stage(started)

  started <- Sys.time()
  target_results <- tibble::tibble(
    record_id = targets$record_id, instrument = targets$instrument,
    redcap_event_name = targets$redcap_event_name,
    repeat_instrument = targets$repeat_instrument,
    repeat_instance = targets$repeat_instance, target_source = targets$target_source,
    event_row_started = event_status,
    repeat_instance_row_started = repeat_status,
    instrument_started = instrument_status, field_complete = field_status,
    fields_assessed = as.integer(fields_assessed),
    fields_failed = as.integer(fields_failed),
    field_applicability_reason = field_reason
  )
  summary <- .summary_build_rows(targets, target_results)
  missing_rows <- .missing_build_rows(
    targets,
    target_results,
    field_rows,
    rcon,
    snapshot
  )
  complete_stage(started)

  started <- Sys.time()
  diagnostics <- tibble::tibble(
    stage = seq_along(stage_names), operation = stage_names,
    completed = TRUE, elapsed_seconds = c(elapsed[seq_len(11L)], NA_real_)
  )
  out <- list(
    plan = plan, target_results = target_results, summary = summary,
    missing = missing_rows, verification = verification$audit,
    diagnostics = diagnostics,
    details = if (isTRUE(details)) {
      .details_build_check_rows(targets, target_results, field_rows)
    } else {
      NULL
    },
    settings = .comparison_normalize_settings(required_fields, ignore_fields, exclude_types)
  )
  class(out) <- c("redcapmissing", "list")
  complete_stage(started)
  out$diagnostics$elapsed_seconds[[12L]] <- elapsed[[12L]]
  out
}

.run_plan_normalize_logical_argument <- function(x, argument) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .condition_signal_error(paste0("`", argument, "` must be TRUE or FALSE."), "argument")
  }
  invisible(x)
}

.run_plan_normalize_character_argument <- function(x, argument) {
  if (is.null(x) || (is.character(x) && !length(x))) return(character())
  if (!is.character(x)) {
    .condition_signal_error(paste0("`", argument, "` must be NULL or a character vector."), "argument")
  }
  if (any(is.na(x) | !nzchar(x) | x != trimws(x))) {
    .condition_signal_error(paste0("`", argument, "` must contain nonblank, unpadded values."), "argument")
  }
  if (anyDuplicated(x)) .condition_signal_error(paste0("`", argument, "` cannot contain duplicates."), "argument")
  x
}

.run_plan_resolve_export_fields <- function(metadata, fields, field_dictionary = NULL) {
  fields <- as.character(fields)
  field_dictionary <- field_dictionary %||% .metadata_build_field_dictionary(metadata)
  unknown <- setdiff(fields, names(field_dictionary$export_fields))
  if (length(unknown)) {
    .condition_signal_error(
      paste0("Metadata must define field `", unknown[[1L]], "` exactly once."),
      "project"
    )
  }
  unname(field_dictionary$export_fields[fields])
}

.run_plan_require_response_columns <- function(data, export_fields, purpose) {
  missing_columns <- setdiff(unique(as.character(export_fields)), names(data))
  if (length(missing_columns)) {
    .condition_signal_error(paste0("`data` is missing column(s) required for ", purpose,
      ": ", paste(missing_columns, collapse = ", "), "."), "schema")
  }
  invisible(data)
}

.run_plan_build_branch_dependencies <- function(
  metadata,
  field_rows,
  references,
  field_dictionary = NULL
) {
  field_dictionary <- field_dictionary %||%
    .metadata_build_field_dictionary(metadata)
  empty_by_instrument <- lapply(field_rows, function(...) character())
  if (!nrow(references)) {
    return(list(
      global_export_fields = character(),
      export_fields_by_instrument = empty_by_instrument
    ))
  }

  roots <- unique(as.character(references$field))
  unknown <- setdiff(roots, as.character(metadata$field_name))
  if (length(unknown)) {
    .condition_signal_error(paste0("Branching logic references unknown field(s): ",
      paste(unknown, collapse = ", "), "."), "project")
  }
  .metadata_populate_field_dictionary_entries(
    field_dictionary,
    metadata,
    roots
  )
  export_fields_by_root <- .run_plan_resolve_export_fields(
    metadata,
    roots,
    field_dictionary = field_dictionary
  )
  reference_root_index <- match(as.character(references$field), roots)
  reference_logic <- unique(as.character(references$logic))
  reference_logic_index <- match(
    as.character(references$logic),
    reference_logic
  )
  root_index_by_logic <- unname(split(
    reference_root_index,
    factor(
      reference_logic_index,
      levels = seq_along(reference_logic)
    ),
    drop = FALSE
  ))
  export_fields_by_logic <- lapply(root_index_by_logic, function(root_index) {
    unique(unlist(
      export_fields_by_root[unique(root_index)],
      use.names = FALSE
    ))
  })
  logic_index_by_value <- list2env(
    stats::setNames(as.list(seq_along(reference_logic)), reference_logic),
    hash = TRUE,
    parent = emptyenv()
  )
  export_fields_by_instrument <- lapply(field_rows, function(instrument_fields) {
    logic <- .schema_normalize_character_vector(
      instrument_fields$branching_logic
    )
    logic <- unique(logic[!.schema_detect_blank_values(logic)])
    if (!length(logic)) return(character())

    known_logic <- vapply(
      logic,
      exists,
      logical(1),
      envir = logic_index_by_value,
      inherits = FALSE
    )
    if (!any(known_logic)) return(character())
    instrument_logic_index <- as.integer(unlist(
      mget(
        logic[known_logic],
        envir = logic_index_by_value,
        inherits = FALSE
      ),
      use.names = FALSE
    ))
    instrument_logic_index <- sort(
      unique(instrument_logic_index),
      method = "radix"
    )
    unique(unlist(
      export_fields_by_logic[instrument_logic_index],
      use.names = FALSE
    ))
  })

  list(
    global_export_fields = unique(unlist(
      export_fields_by_root,
      use.names = FALSE
    )),
    export_fields_by_instrument = export_fields_by_instrument
  )
}
