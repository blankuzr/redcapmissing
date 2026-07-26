#' Run a REDCap missingness assessment plan
#'
#' Evaluates the Assessible targets frozen in a [plan_from_data()] or
#' [plan_explicit()] result against supplied REDCap data. `run_plan()` never
#' infers, removes, or expands target scope.
#'
#' @param plan A validated `redcapmissing_plan`. Its project ID, schema version,
#'   structure fingerprint, selected instruments, target schema, target
#'   uniqueness, target provenance, and deterministic ordering are revalidated
#'   against `rcon`; malformed or hand-edited plans are rejected.
#' @param data A data frame containing physical REDCap rows and every response,
#'   checkbox-child, and branching-dependency column needed by the selected
#'   checks. Structural columns follow [plan_from_data()] normalization, and all
#'   columns must use ordinary atomic vector storage. A correctly structured
#'   zero-row data frame is allowed. Added unplanned rows
#'   cannot add targets; absent planned rows remain targets and can fail gates.
#' @param rcon The REDCap connection-like object for the same unchanged project
#'   structure as `plan`. It must expose project identity, longitudinal status,
#'   metadata, instruments, repeat configuration, and, for longitudinal
#'   projects, arms, events, and instrument-event mappings.
#' @param required_fields One nonmissing logical value. `TRUE` limits
#'   field-complete assessment to fields marked required in REDCap metadata;
#'   `FALSE` retains required and optional fields before the remaining policy
#'   exclusions.
#' @param ignore_fields `NULL`, `character(0)`, or a character vector of unique,
#'   nonmissing, nonblank, unpadded raw metadata field names on selected
#'   instruments. Use checkbox root names, not exported checkbox-child names.
#'   Unknown names and names unused after earlier policy steps are errors.
#' @param exclude_types `NULL`, `character(0)`, or a character vector of unique,
#'   nonmissing, nonblank, unpadded REDCap metadata field types. `NULL` and
#'   `character(0)` exclude no types. Explicitly supplied unknown or unused types
#'   are errors. The default removes `"descriptive"` fields when present.
#' @param verified `NULL`, or a data frame containing verification evidence with
#'   the nine required columns documented in **Verification**. Extra columns are
#'   ignored. It must be supplied together with `verified_user`; a complete
#'   zero-row table is valid.
#' @param verified_user `NULL`, or one exact, nonmissing, nonblank, unpadded
#'   character username. Matching is case-sensitive. It must be supplied
#'   together with `verified`.
#' @param details One nonmissing logical value. `TRUE` retains validation rows
#'   with raw and effective dispositions; `FALSE` stores `NULL` in `details`.
#'   This choice does not change targets, summaries, missing rows, or
#'   verification audit counts.
#' @param progress One nonmissing logical value controlling the twelve-stage
#'   interactive progress bar. Progress cleanup is registered on exit.
#'
#' @section Execution stages:
#' `diagnostics` records these operations in fixed order:
#'
#' 1. Validate plan, data, and `rcon`.
#' 2. Validate and normalize verification.
#' 3. Resolve instrument-start detection fields.
#' 4. Resolve field-complete fields.
#' 5. Join Assessible targets to physical rows.
#' 6. Run `event-row-started`.
#' 7. Run `repeat-instance-row-started`.
#' 8. Run `instrument-started`.
#' 9. Run raw `field-complete`.
#' 10. Apply verification.
#' 11. Aggregate effective results.
#' 12. Construct the report.
#'
#' Project surfaces are cached within the call. A newer record snapshot is
#' permitted only when the project identity and structure fingerprint remain
#' unchanged.
#'
#' @section Checks and gating:
#' Check statuses are `"passed"`, `"failed"`, `"not applicable"`, and
#' `"not reached"`. Validation levels are `"event:instrument"` and
#' `"event:instrument:instance"`.
#'
#' Every longitudinal target first receives `event-row-started`, based on any
#' physical row for its record and event. Classic targets mark that check not
#' applicable. A repeating target receives `repeat-instance-row-started` only
#' after its event gate passes; a regular target marks that check not applicable.
#' A failed event or repeat gate makes downstream checks not reached.
#'
#' `instrument-started` uses an independent detection set: every data-entry
#' metadata field on the selected instrument except the project record-ID field
#' and `descriptive` or `calc` fields. Checkbox roots require a nonblank,
#' unambiguous metadata choice definition and expand to exported child columns.
#' At least one nonmissing detection response starts the instrument. A
#' selected instrument with no usable detection fields, or a missing detection
#' column, is an error. `field-complete` runs only after this check passes.
#'
#' @section Field-complete policy:
#' The field set is resolved in this exact order:
#'
#' ```text
#' all fields on the selected instrument
#' -> required-only filter when required_fields = TRUE
#' -> exclude_types removal
#' -> ignore_fields removal
#' ```
#'
#' These three arguments affect only `field-complete`. They cannot change the
#' plan, targets, either physical-row check, or `instrument-started`; an optional,
#' ignored, or excluded-type field may still establish that an instrument
#' started. If no fields remain, `field-complete` is `"not applicable"`, its
#' reason is `"no assessible fields after field policy"`, its counts are zero,
#' and its rates are `NA_real_` rather than an automatic pass.
#'
#' Branching logic is evaluated in the target's exact event/repeat context,
#' including cross-event references. Branch-closed fields are not assessed.
#' Checkbox roots are complete when at least one exported child choice is
#' selected. Every selected response, checkbox-child, and branching-dependency
#' column must be present; an absent column is never interpreted as a blank.
#'
#' @section Response missingness:
#' One predicate is used for instrument-start detection and field completeness.
#' Typed R missing values, factor `NA`, logical `NA`, numeric `NA`/`NaN`, missing
#' dates or timestamps, empty strings, and whitespace-only strings are missing.
#' `Inf` and `-Inf` are not automatically missing. Literal strings `"NA"`,
#' `"N/A"`, `"NULL"`, `"."`, and `"-999"` are ordinary nonmissing responses
#' unless converted upstream under a separate REDCap missing-code policy.
#' Physical-row presence is determined solely by structural identity, never by
#' response missingness.
#'
#' @section Verification:
#' A non-`NULL` `verified` table requires these columns; validation occurs on
#' every row before username or status filtering:
#'
#' | Column | Accepted storage and values | Internal normalization |
#' |---|---|---|
#' | `project_id` | Character, integer, or whole-number finite double; exact project match | Character |
#' | `record` | Character, factor, integer, or finite double; nonmissing, nonblank, unpadded | Character |
#' | `event_id` | Canonical positive character digits, integer, or whole-number double; typed missing/blank only where the event dimension is inapplicable | Character event ID and mapped raw event name; `NA_character_` where inapplicable |
#' | `field_name` | Character; exact nonblank, unpadded raw metadata field name | Character |
#' | `repeat_instrument` | Character raw instrument, or character blank/typed missing only where inapplicable | Character; inapplicable values become `NA_character_` |
#' | `instance` | Canonical positive character digits, integer, or whole-number double; typed missing/blank only where inapplicable | Integer; inapplicable values become `NA_integer_` |
#' | `ts` | Nonmissing `POSIXct`, or a REDCap/ISO-8601 character timestamp with date, seconds, optional fractional seconds, and optional `Z` or numeric offset | `POSIXct` in UTC; timezone-less text is interpreted as UTC |
#' | `current_query_status` | Character; nonmissing, nonblank, unpadded, case-sensitive | Character |
#' | `username` | Character; nonmissing, nonblank, unpadded, case-sensitive | Character |
#'
#' Nonapplicable repeat/event dimensions must normalize to typed missing values;
#' applicable dimensions must exactly match an Assessible target. `NaN`,
#' infinity, padded identifiers, invalid timestamps, unknown project/event/field
#' contexts, illegal repeat shapes, and verification contexts outside the plan
#' are errors.
#'
#' Rows are filtered to exact `verified_user`, grouped by normalized field
#' context, and reduced to the latest timestamp. Identical rows tied at the
#' latest timestamp collapse; conflicting latest ties error. Only exact
#' `"VERIFIED"` changes an otherwise-failing, already-assessed
#' `field-complete` result. Verification cannot create targets, bypass gates,
#' start an instrument, change a passing field, or restore fields removed by
#' policy or branching. The report retains audit counts, not supplied rows.
#'
#' @section Return value:
#' A `redcapmissing` object with exactly `plan`, `target_results`, `summary`,
#' `missing`, `verification`, `diagnostics`, and `details`. It retains neither
#' source `data`, raw `verified`, tokens, nor a live connection.
#'
#' `target_results` has one row per Assessible target and exactly:
#'
#' | Columns | Storage |
#' |---|---|
#' | `record_id`, `instrument`, `redcap_event_name`, `repeat_instrument`, `target_source` | Character |
#' | `repeat_instance`, `fields_assessed`, `fields_failed` | Integer |
#' | `event_row_started`, `repeat_instance_row_started`, `instrument_started`, `field_complete` | Character check status |
#' | `field_applicability_reason` | Character; typed missing unless a reason applies |
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
#' except integer `repeat_instance`; inapplicable structural values are typed
#' missing rather than blank placeholders.
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
#' is integer; branch/verification flags are logical; all other columns are
#' character. With `details = FALSE`, `details` is `NULL`.
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
#' explicit_schedule <- data.frame(
#'   record_id = c("001", "002"),
#'   instrument = c("baseline", "baseline"),
#'   redcap_event_name = c("baseline_arm_1", "baseline_arm_1"),
#'   repeat_instance = c(NA_integer_, NA_integer_)
#' )
#' plan <- plan_explicit(records, rcon, "baseline", explicit_schedule)
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
  .rcm_run_logical(required_fields, "required_fields")
  .rcm_run_logical(details, "details")
  .rcm_run_logical(progress, "progress")
  ignore_fields <- .rcm_run_character_vector(ignore_fields, "ignore_fields")
  exclude_types <- .rcm_run_character_vector(exclude_types, "exclude_types")

  stage_names <- c(
    "Validate plan, data, and rcon",
    "Validate and normalize verification",
    "Resolve instrument-start fields",
    "Resolve field-complete fields",
    "Join Assessible targets to physical rows",
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
  if (missing(plan) || is.null(plan)) .rcm_plan_abort("`plan` is required.", "plan")
  if (missing(data) || is.null(data) || !is.data.frame(data)) {
    .rcm_plan_abort("`data` must be a data frame or tibble.", "schema")
  }
  if (missing(rcon) || is.null(rcon)) .rcm_plan_abort("`rcon` is required.", "project")
  snapshot <- .rcm_project_snapshot(rcon)
  plan <- .rcm_validate_plan(plan, snapshot = snapshot)
  normalized_data <- .rcm_normalize_data(data, snapshot, require_nonempty = FALSE,
                                         response_columns = NULL)
  if (is.list(normalized_data) && "data" %in% names(normalized_data)) {
    normalized_data <- normalized_data$data
  }
  normalized_data <- tibble::as_tibble(normalized_data)
  complete_stage(started)

  started <- Sys.time()
  verification <- .rcm_prepare_verified(verified, verified_user, snapshot, plan)
  complete_stage(started)

  started <- Sys.time()
  metadata <- tibble::as_tibble(snapshot$metadata)
  id_field <- snapshot$project$record_id_field %||% as.character(metadata$field_name[[1L]])
  detection <- .rcm_run_detection_plan(metadata, plan$instruments, id_field)
  .rcm_run_require_response_columns(normalized_data,
    unlist(detection$export_fields, use.names = FALSE), "instrument-start detection")
  complete_stage(started)

  started <- Sys.time()
  field_plan <- .rcm_run_field_plan(
    metadata,
    plan$instruments,
    required_fields,
    ignore_fields,
    exclude_types,
    strict_exclude_types = !exclude_types_was_missing
  )
  .rcm_run_require_response_columns(normalized_data,
    unlist(field_plan$export_fields, use.names = FALSE), "field-complete assessment")
  branch_fields <- .rcm_run_branch_export_fields(metadata, field_plan$branching_logic)
  .rcm_run_require_response_columns(normalized_data, branch_fields,
                                    "branching-logic evaluation")
  complete_stage(started)

  started <- Sys.time()
  targets <- tibble::as_tibble(plan$assessible_targets)
  joins <- .rcm_run_join_targets(targets, normalized_data,
                                 isTRUE(snapshot$project$longitudinal))
  complete_stage(started)

  started <- Sys.time()
  event_status <- if (isTRUE(snapshot$project$longitudinal)) {
    ifelse(joins$event_present, "passed", "failed")
  } else rep("not applicable", nrow(targets))
  complete_stage(started)

  started <- Sys.time()
  repeating <- !is.na(targets$repeat_instance)
  event_gate_pass <- if (isTRUE(snapshot$project$longitudinal)) {
    joins$event_present
  } else {
    rep(TRUE, nrow(targets))
  }
  repeat_status <- rep("not applicable", nrow(targets))
  repeat_status[repeating & event_gate_pass] <- ifelse(
    joins$target_present[repeating & event_gate_pass], "passed", "failed")
  repeat_status[repeating & !event_gate_pass] <- "not reached"
  complete_stage(started)

  started <- Sys.time()
  upstream_pass <- event_gate_pass & (!repeating | joins$target_present)
  instrument_status <- rep("not reached", nrow(targets))
  regular_missing <- !repeating & event_gate_pass & !joins$target_present
  instrument_status[regular_missing] <- "failed"
  for (i in which(upstream_pass)) {
    instrument <- targets$instrument[[i]]
    instrument_status[[i]] <- if (.rcm_run_any_present(
      normalized_data[joins$target_row[[i]], , drop = FALSE],
      detection$export_fields[[instrument]]
    )) "passed" else "failed"
  }
  complete_stage(started)

  started <- Sys.time()
  assessed <- vector("list", nrow(targets))
  field_status <- rep("not reached", nrow(targets))
  fields_assessed <- integer(nrow(targets)); fields_failed <- integer(nrow(targets))
  field_reason <- rep(NA_character_, nrow(targets))
  for (i in seq_len(nrow(targets))) {
    if (!identical(instrument_status[[i]], "passed")) {
      assessed[[i]] <- .rcm_run_empty_field_rows(); next
    }
    instrument <- targets$instrument[[i]]
    plan_rows <- field_plan$rows[[instrument]]
    if (!nrow(plan_rows)) {
      field_status[[i]] <- "not applicable"
      field_reason[[i]] <- "no assessible fields after field policy"
      assessed[[i]] <- .rcm_run_empty_field_rows(); next
    }
    assessed[[i]] <- .rcm_run_assess_fields(
      targets[i, , drop = FALSE],
      normalized_data[joins$target_row[[i]], , drop = FALSE],
      normalized_data, metadata, plan_rows
    )
    fields_assessed[[i]] <- nrow(assessed[[i]])
    if (!fields_assessed[[i]]) {
      field_status[[i]] <- "not applicable"
      field_reason[[i]] <- "no assessible fields after branching logic"
      next
    }
    fields_failed[[i]] <- sum(assessed[[i]]$raw_disposition == "failed")
    field_status[[i]] <- if (fields_failed[[i]]) "failed" else "passed"
  }
  complete_stage(started)

  started <- Sys.time()
  field_rows <- .rcm_run_bind_field_rows(assessed, targets)
  overrides <- 0L
  if (nrow(field_rows)) {
    verify_key <- .rcm_field_context_key(field_rows$record_id,
      field_rows$redcap_event_name, field_rows$repeat_instrument,
      field_rows$repeat_instance, field_rows$field_name)
    apply <- field_rows$raw_disposition == "failed" & verify_key %in% verification$keys
    field_rows$verification_applied <- apply
    field_rows$effective_disposition <- field_rows$raw_disposition
    field_rows$effective_disposition[apply] <- "passed"
    overrides <- sum(apply)
    split_fields <- split(field_rows, field_rows$.target_row)
    for (name in names(split_fields)) {
      i <- as.integer(name)
      fields_failed[[i]] <- sum(split_fields[[name]]$effective_disposition == "failed")
      field_status[[i]] <- if (fields_failed[[i]]) "failed" else "passed"
    }
    field_rows$.target_row <- NULL
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
  check_rows <- .rcm_run_check_rows(targets, target_results, field_rows)
  summary <- .rcm_run_summary(check_rows)
  missing_rows <- .rcm_run_missing(check_rows, rcon, snapshot)
  complete_stage(started)

  started <- Sys.time()
  diagnostics <- tibble::tibble(
    stage = seq_along(stage_names), operation = stage_names,
    completed = TRUE, elapsed_seconds = c(elapsed[seq_len(11L)], NA_real_)
  )
  out <- list(
    plan = plan, target_results = target_results, summary = summary,
    missing = missing_rows, verification = verification$audit,
    diagnostics = diagnostics, details = if (isTRUE(details)) check_rows else NULL
  )
  class(out) <- c("redcapmissing", "list")
  complete_stage(started)
  out$diagnostics$elapsed_seconds[[12L]] <- elapsed[[12L]]
  out
}

.rcm_run_logical <- function(x, argument) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .rcm_plan_abort(paste0("`", argument, "` must be TRUE or FALSE."), "argument")
  }
  invisible(x)
}

.rcm_run_character_vector <- function(x, argument) {
  if (is.null(x) || (is.character(x) && !length(x))) return(character())
  if (!is.character(x)) {
    .rcm_plan_abort(paste0("`", argument, "` must be NULL or a character vector."), "argument")
  }
  if (any(is.na(x) | !nzchar(x) | x != trimws(x))) {
    .rcm_plan_abort(paste0("`", argument, "` must contain nonblank, unpadded values."), "argument")
  }
  if (anyDuplicated(x)) .rcm_plan_abort(paste0("`", argument, "` cannot contain duplicates."), "argument")
  x
}

.rcm_run_detection_plan <- function(metadata, instruments, id_field) {
  rows <- metadata[metadata$form_name %in% instruments &
    metadata$field_name != id_field & !metadata$field_type %in% c("descriptive", "calc"),
    , drop = FALSE]
  result <- stats::setNames(vector("list", length(instruments)), instruments)
  for (instrument in instruments) {
    instrument_rows <- rows[rows$form_name == instrument, , drop = FALSE]
    fields <- unique(unlist(.rcm_run_export_fields(metadata, instrument_rows$field_name),
                            use.names = FALSE))
    if (!length(fields)) {
      .rcm_plan_abort(paste0("Instrument `", instrument,
        "` has no usable instrument-start detection fields."), "project")
    }
    result[[instrument]] <- fields
  }
  list(export_fields = result)
}

.rcm_run_field_plan <- function(
  metadata,
  instruments,
  required_fields,
  ignore_fields,
  exclude_types,
  strict_exclude_types = TRUE
) {
  selected <- metadata[metadata$form_name %in% instruments, , drop = FALSE]
  unknown_ignore <- setdiff(ignore_fields, unique(as.character(selected$field_name)))
  if (length(unknown_ignore)) {
    .rcm_plan_abort(paste0("`ignore_fields` contains fields outside the selected instruments: ",
      paste(unknown_ignore, collapse = ", "), "."), "argument")
  }
  if (isTRUE(required_fields) && !"required_field" %in% names(metadata)) {
    .rcm_plan_abort("`rcon` metadata must contain `required_field` when `required_fields = TRUE`.", "project")
  }
  if (isTRUE(required_fields)) selected <- selected[.miss_required_vec(selected$required_field), , drop = FALSE]
  unknown_types <- setdiff(exclude_types, unique(as.character(selected$field_type)))
  if (length(unknown_types) && isTRUE(strict_exclude_types)) {
    .rcm_plan_abort(paste0("`exclude_types` contains types unused by the resolved field policy: ",
      paste(unknown_types, collapse = ", "), "."), "argument")
  }
  selected <- selected[!selected$field_type %in% exclude_types, , drop = FALSE]
  unused_ignore <- setdiff(ignore_fields, as.character(selected$field_name))
  if (length(unused_ignore)) {
    .rcm_plan_abort(paste0("`ignore_fields` contains fields not used by the resolved field policy: ",
      paste(unused_ignore, collapse = ", "), "."), "argument")
  }
  selected <- selected[!selected$field_name %in% ignore_fields, , drop = FALSE]
  selected$branching_logic <- if ("branching_logic" %in% names(selected))
    as.character(selected$branching_logic) else rep("", nrow(selected))
  selected$field_label <- if ("field_label" %in% names(selected))
    as.character(selected$field_label) else as.character(selected$field_name)
  selected$export_fields <- .rcm_run_export_fields(metadata, selected$field_name)
  split_rows <- split(selected, selected$form_name)
  result_rows <- result_exports <- stats::setNames(vector("list", length(instruments)), instruments)
  for (instrument in instruments) {
    value <- split_rows[[instrument]]
    if (is.null(value)) value <- selected[0, , drop = FALSE]
    result_rows[[instrument]] <- tibble::as_tibble(value)
    result_exports[[instrument]] <- unique(unlist(value$export_fields, use.names = FALSE))
  }
  list(rows = result_rows, export_fields = result_exports,
       branching_logic = selected$branching_logic)
}
.rcm_run_export_fields <- function(metadata, fields) {
  fields <- as.character(fields)
  result <- vector("list", length(fields))
  for (i in seq_along(fields)) {
    field <- fields[[i]]
    row <- metadata[metadata$field_name == field, , drop = FALSE]
    if (nrow(row) != 1L) {
      .rcm_plan_abort(paste0("Metadata must define field `", field, "` exactly once."), "project")
    }
    result[[i]] <- if (identical(as.character(row$field_type[[1L]]), "checkbox")) {
      as.character(.miss_derive_field_names(row)$export_field_name)
    } else field
  }
  result
}

.rcm_run_require_response_columns <- function(data, export_fields, purpose) {
  missing_columns <- setdiff(unique(as.character(export_fields)), names(data))
  if (length(missing_columns)) {
    .rcm_plan_abort(paste0("`data` is missing column(s) required for ", purpose,
      ": ", paste(missing_columns, collapse = ", "), "."), "schema")
  }
  invisible(data)
}

.rcm_run_branch_export_fields <- function(metadata, logic) {
  refs <- .miss_extract_logic_references(logic)
  if (!nrow(refs)) return(character())
  unknown <- setdiff(refs$field, as.character(metadata$field_name))
  if (length(unknown)) {
    .rcm_plan_abort(paste0("Branching logic references unknown field(s): ",
      paste(unknown, collapse = ", "), "."), "project")
  }
  unique(unlist(.rcm_run_export_fields(metadata, unique(refs$field)), use.names = FALSE))
}

.rcm_run_context_key <- function(record_id, event, repeat_instrument, repeat_instance) {
  encode <- function(x) { out <- as.character(x); out[is.na(x)] <- "<NA>"; out }
  paste(encode(record_id), encode(event), encode(repeat_instrument),
        encode(repeat_instance), sep = "\r")
}

.rcm_run_join_targets <- function(targets, data, longitudinal) {
  data_key <- .rcm_run_context_key(data$.rcm_record_id, data$redcap_event_name,
    data$redcap_repeat_instrument, data$redcap_repeat_instance)
  target_key <- .rcm_run_context_key(targets$record_id, targets$redcap_event_name,
    targets$repeat_instrument, targets$repeat_instance)
  target_row <- match(target_key, data_key)
  event_data_key <- .rcm_run_context_key(data$.rcm_record_id,
    if (isTRUE(longitudinal)) data$redcap_event_name else NA_character_,
    NA_character_, NA_integer_)
  event_target_key <- .rcm_run_context_key(targets$record_id,
    if (isTRUE(longitudinal)) targets$redcap_event_name else NA_character_,
    NA_character_, NA_integer_)
  list(target_row = as.list(target_row), target_present = !is.na(target_row),
       event_present = event_target_key %in% event_data_key)
}

.rcm_run_is_missing <- function(x) {
  missing <- is.na(x)
  if (is.character(x) || is.factor(x)) missing <- missing | !nzchar(trimws(as.character(x)))
  if (is.numeric(x)) missing <- missing | is.nan(x)
  missing
}

.rcm_run_any_present <- function(record, fields) {
  any(vapply(fields, function(field) {
    if (grepl("___", field, fixed = TRUE)) {
      return(any(.miss_checkbox_selected_vec(record[[field]])))
    }
    any(!.rcm_run_is_missing(record[[field]]))
  }, logical(1)))
}

.rcm_run_empty_field_rows <- function() {
  tibble::tibble(
    field_name = character(), field_label = character(), field_type = character(),
    branching_logic = character(), branch_satisfied = logical(),
    value_summary = character(), raw_disposition = character(),
    verification_applied = logical(), effective_disposition = character()
  )
}

.rcm_run_assess_fields <- function(target, record, all_data, metadata, field_plan) {
  project <- list(id_col = ".rcm_record_id", system_fields = .miss_system_fields())
  choice_map <- .miss_build_choice_map(metadata)
  rows <- vector("list", nrow(field_plan)); keep <- logical(nrow(field_plan))
  for (i in seq_len(nrow(field_plan))) {
    logic <- field_plan$branching_logic[[i]]
    branch_satisfied <- tryCatch(
      .miss_branch_satisfied(
        logic = logic, records = record, lookup_records = all_data,
        meta = metadata, choice_map = choice_map, project = project
      )[[1L]],
      error = function(error) {
        .rcm_plan_abort(
          paste0(
            "Could not evaluate REDCap branching logic `",
            logic,
            "`: ",
            conditionMessage(error)
          ),
          "project"
        )
      }
    )
    if (!isTRUE(branch_satisfied)) next
    keep[[i]] <- TRUE
    export_fields <- field_plan$export_fields[[i]]
    field_type <- as.character(field_plan$field_type[[i]])
    if (identical(field_type, "checkbox")) {
      selected <- vapply(export_fields, function(field) {
        .miss_checkbox_selected_vec(record[[field]])[[1L]]
      }, logical(1))
      passed <- any(selected)
      value <- if (passed) paste(export_fields[selected], collapse = ", ") else ""
    } else {
      value_object <- record[[export_fields[[1L]]]]
      passed <- !.rcm_run_is_missing(value_object)[[1L]]
      value <- .miss_chr(value_object[[1L]])
    }
    rows[[i]] <- tibble::tibble(
      field_name = as.character(field_plan$field_name[[i]]),
      field_label = as.character(field_plan$field_label[[i]]),
      field_type = field_type, branching_logic = as.character(logic),
      branch_satisfied = TRUE, value_summary = value,
      raw_disposition = if (passed) "passed" else "failed",
      verification_applied = FALSE,
      effective_disposition = if (passed) "passed" else "failed"
    )
  }
  if (!any(keep)) return(.rcm_run_empty_field_rows())
  dplyr::bind_rows(rows[keep])
}

.rcm_run_bind_field_rows <- function(rows, targets) {
  pieces <- vector("list", length(rows))
  for (i in seq_along(rows)) {
    if (!nrow(rows[[i]])) next
    pieces[[i]] <- dplyr::mutate(
      rows[[i]], .target_row = i,
      record_id = targets$record_id[[i]], instrument = targets$instrument[[i]],
      redcap_event_name = targets$redcap_event_name[[i]],
      repeat_instrument = targets$repeat_instrument[[i]],
      repeat_instance = targets$repeat_instance[[i]], .before = 1L
    )
  }
  dplyr::bind_rows(pieces)
}

.rcm_run_validation_level <- function(repeat_instance) {
  ifelse(is.na(repeat_instance), "event:instrument", "event:instrument:instance")
}

.rcm_run_check_rows <- function(targets, target_results, field_rows) {
  add_target_checks <- function(check, disposition, reason = NA_character_) {
    n <- nrow(targets)
    tibble::tibble(
      record_id = targets$record_id, instrument = targets$instrument,
      redcap_event_name = targets$redcap_event_name,
      repeat_instrument = targets$repeat_instrument,
      repeat_instance = targets$repeat_instance, target_source = targets$target_source,
      validation_level = .rcm_run_validation_level(targets$repeat_instance),
      validation_check = rep(check, n), field_name = rep(NA_character_, n),
      field_label = rep(NA_character_, n), field_type = rep(NA_character_, n),
      branching_logic = rep(NA_character_, n), branch_satisfied = rep(NA, n),
      value_summary = rep(NA_character_, n), raw_disposition = disposition,
      verification_applied = rep(FALSE, n), effective_disposition = disposition,
      reason = rep(reason, length.out = n)
    )
  }
  event_reason <- rep(NA_character_, nrow(target_results))
  event_reason[target_results$event_row_started == "not applicable"] <-
    "not applicable for classic project"
  repeat_reason <- rep(NA_character_, nrow(target_results))
  repeat_reason[target_results$repeat_instance_row_started == "not applicable"] <-
    "not a repeating target"
  rows <- list(
    add_target_checks("event-row-started", target_results$event_row_started, event_reason),
    add_target_checks("repeat-instance-row-started", target_results$repeat_instance_row_started, repeat_reason),
    add_target_checks("instrument-started", target_results$instrument_started)
  )
  field_target_rows <- add_target_checks("field-complete", target_results$field_complete)
  field_target_rows$reason[target_results$field_complete == "not applicable"] <-
    target_results$field_applicability_reason[
      target_results$field_complete == "not applicable"
    ]
  rows[[4L]] <- field_target_rows[target_results$field_complete %in%
    c("not applicable", "not reached"), , drop = FALSE]
  if (nrow(field_rows)) {
    field_target_key <- paste(
      .rcm_run_context_key(
        field_rows$record_id,
        field_rows$redcap_event_name,
        field_rows$repeat_instrument,
        field_rows$repeat_instance
      ),
      field_rows$instrument,
      sep = "\r"
    )
    target_key <- paste(
      .rcm_run_context_key(
        targets$record_id,
        targets$redcap_event_name,
        targets$repeat_instrument,
        targets$repeat_instance
      ),
      targets$instrument,
      sep = "\r"
    )
    target_index <- match(field_target_key, target_key)
    if (anyNA(target_index)) {
      .rcm_plan_abort("Detailed field rows could not be matched to their Assessible targets.", "schema")
    }
    rows[[5L]] <- tibble::tibble(
      record_id = field_rows$record_id, instrument = field_rows$instrument,
      redcap_event_name = field_rows$redcap_event_name,
      repeat_instrument = field_rows$repeat_instrument,
      repeat_instance = field_rows$repeat_instance,
      target_source = targets$target_source[target_index],
      validation_level = .rcm_run_validation_level(field_rows$repeat_instance),
      validation_check = "field-complete", field_name = field_rows$field_name,
      field_label = field_rows$field_label, field_type = field_rows$field_type,
      branching_logic = field_rows$branching_logic,
      branch_satisfied = field_rows$branch_satisfied,
      value_summary = field_rows$value_summary,
      raw_disposition = field_rows$raw_disposition,
      verification_applied = field_rows$verification_applied,
      effective_disposition = field_rows$effective_disposition,
      reason = NA_character_
    )
  }
  dplyr::bind_rows(rows)
}

.rcm_run_summary <- function(check_rows) {
  empty <- tibble::tibble(
    redcap_event_name = character(), instrument = character(),
    repeat_instrument = character(), repeat_instance = integer(),
    validation_level = character(), validation_check = character(),
    status = character(), reason = character(), assessed = integer(),
    passed = integer(), failed = integer(), pass_rate = double(),
    fail_rate = double()
  )
  if (!nrow(check_rows)) return(empty)
  group_columns <- c("redcap_event_name", "instrument", "repeat_instrument",
                     "repeat_instance", "validation_level", "validation_check")
  key <- do.call(paste, c(lapply(check_rows[group_columns], function(x) {
    value <- as.character(x); value[is.na(x)] <- "<NA>"; value
  }), sep = "\r"))
  result <- lapply(split(seq_len(nrow(check_rows)), key), function(index) {
    disposition <- check_rows$effective_disposition[index]
    assessed <- sum(disposition %in% c("passed", "failed"))
    passed <- sum(disposition == "passed"); failed <- sum(disposition == "failed")
    all_na <- all(disposition == "not applicable"); first <- index[[1L]]
    reason <- unique(stats::na.omit(check_rows$reason[index]))
    tibble::tibble(
      redcap_event_name = check_rows$redcap_event_name[[first]],
      instrument = check_rows$instrument[[first]],
      repeat_instrument = check_rows$repeat_instrument[[first]],
      repeat_instance = check_rows$repeat_instance[[first]],
      validation_level = check_rows$validation_level[[first]],
      validation_check = check_rows$validation_check[[first]],
      status = if (all_na) "not applicable" else "assessed",
      reason = if (all_na && length(reason)) reason[[1L]] else NA_character_,
      assessed = as.integer(assessed), passed = as.integer(passed),
      failed = as.integer(failed),
      pass_rate = if (!assessed) NA_real_ else passed / assessed,
      fail_rate = if (!assessed) NA_real_ else failed / assessed
    )
  })
  result <- dplyr::bind_rows(result)
  source_context <- unique(.rcm_run_context_key(
    check_rows$instrument,
    check_rows$redcap_event_name,
    check_rows$repeat_instrument,
    check_rows$repeat_instance
  ))
  result_context <- .rcm_run_context_key(
    result$instrument,
    result$redcap_event_name,
    result$repeat_instrument,
    result$repeat_instance
  )
  check_order <- match(
    result$validation_check,
    .redcapmissing_validation_checks()
  )
  result[
    order(match(result_context, source_context), check_order),
    ,
    drop = FALSE
  ]
}

.rcm_run_add_urls <- function(rows, rcon, snapshot) {
  rows$url <- rep(NA_character_, nrow(rows))
  if (!nrow(rows)) return(rows)

  instance_url <- tryCatch(rcon$url, error = function(e) NULL)
  if (is.function(instance_url)) {
    instance_url <- tryCatch(instance_url(), error = function(e) NULL)
  }
  version_method <- tryCatch(rcon$version, error = function(e) NULL)
  version <- if (is.function(version_method)) {
    tryCatch(version_method(), error = function(e) NULL)
  } else {
    version_method
  }
  if (length(instance_url) != 1L || length(version) != 1L) return(rows)
  instance_url <- as.character(instance_url)
  version <- as.character(version)
  if (
    is.na(instance_url) || !nzchar(trimws(instance_url)) ||
      is.na(version) || !nzchar(trimws(version))
  ) {
    return(rows)
  }

  event_id <- rep(NA_character_, nrow(rows))
  complete <- !is.na(rows$record_id) & nzchar(rows$record_id) &
    !is.na(rows$instrument) & nzchar(rows$instrument)
  if (isTRUE(snapshot$project$longitudinal)) {
    event_index <- match(
      rows$redcap_event_name,
      snapshot$events$redcap_event_name
    )
    event_id <- as.character(snapshot$events$event_id[event_index])
    complete <- complete & !is.na(event_id) & nzchar(event_id)
  }
  if (!any(complete)) return(rows)

  base_url <- sub("/api(/|)$", "", instance_url)
  rows$url[complete] <- sprintf(
    "%s/redcap_v%s/DataEntry/index.php?pid=%s&page=%s&id=%s",
    base_url,
    version,
    snapshot$project$project_id,
    rows$instrument[complete],
    rows$record_id[complete]
  )
  if (isTRUE(snapshot$project$longitudinal)) {
    rows$url[complete] <- paste0(
      rows$url[complete],
      "&event_id=",
      event_id[complete]
    )
  }
  rows
}
.rcm_run_missing <- function(check_rows, rcon, snapshot) {
  rows <- check_rows[check_rows$effective_disposition == "failed", , drop = FALSE]
  rows <- .rcm_run_add_urls(rows, rcon, snapshot)
  rows$validation_context <- .miss_validation_context_vec(
    rows$redcap_event_name, rows$repeat_instance
  )
  rows <- rows[, c(
    "record_id", "redcap_event_name", "repeat_instrument", "repeat_instance",
    "validation_context", "instrument", "validation_check", "field_name",
    "field_label", "field_type", "branching_logic", "url"
  ), drop = FALSE]
  tibble::as_tibble(rows)
}