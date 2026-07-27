#' Run a REDCap missingness assessment plan
#'
#' `run_plan()` evaluates the `assessible_targets` stored in a
#' [plan_from_data()] or [plan_explicit()] result against supplied REDCap data.
#' Target rows come from `plan$assessible_targets`.
#'
#' @param plan A validated `redcapmissing_plan`. Its project ID, schema version,
#'   structure fingerprint, selected instruments, target schema, target
#'   uniqueness, target provenance, and deterministic ordering are revalidated
#'   against `rcon`; malformed plans and plans edited by hand are rejected.
#' @param data A data frame containing physical REDCap rows and every response,
#'   checkbox child, and branching dependency column needed by the selected
#'   checks. Structural columns follow [plan_from_data()] normalization, and all
#'   columns must use ordinary atomic vector storage. A correctly structured
#'   empty data frame is allowed. Plan targets stay fixed when `data` contains
#'   additional rows; absent planned rows remain targets and follow the checks below.
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
#'   nonmissing, nonblank, unpadded raw metadata field names on selected
#'   instruments. Supply checkbox root names. Exported checkbox child names are
#'   invalid. Unknown names and names unused after earlier policy steps are
#'   errors.
#' @param exclude_types `NULL`, `character(0)`, or a character vector of unique,
#'   nonmissing, nonblank, unpadded REDCap metadata field types. `NULL` and
#'   `character(0)` exclude no types. Explicitly supplied unknown or unused types
#'   are errors. The default removes `"descriptive"` fields when present.
#' @param verified `NULL`, or a data frame containing verification evidence with
#'   the nine required columns documented in **Verification**. Extra columns are
#'   ignored. It must be supplied together with `verified_user`; a complete
#'   empty table is valid.
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
#' metadata field on the selected instrument except the project record ID field
#' and `descriptive` or `calc` fields. Checkbox roots require a nonblank,
#' unambiguous metadata choice definition and expand to exported child columns.
#' An ordinary detection field starts the instrument when its response is
#' nonmissing. A checkbox root starts the instrument when at least one child is
#' selected. Child values `0`, `"unchecked"`, `"false"`, and `"no"`, matched in
#' any letter case, are unselected. A selected instrument with no
#' usable detection fields, or a missing detection column, is an error.
#' `field-complete` runs only after this check passes.
#'
#' @section Field-complete policy:
#' The field set is resolved in this exact order:
#'
#' ```text
#' all fields on the selected instrument
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
#' Fields closed by branching logic are not assessed.
#' Checkbox roots are complete when at least one exported child choice is
#' selected. Every selected response, checkbox child, and branching dependency
#' column must be present; absence is an error.
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
#' When `verified` is supplied, each of these column names must occur exactly
#' once. A table with zero rows is accepted after that name check. The storage
#' rules below are checked when rows exist, and every row is validated before
#' username or status filtering:
#'
#' | Column | Accepted storage and values | Internal normalization |
#' |---|---|---|
#' | `project_id` | Character, integer, or whole number finite double; exact project match | Character |
#' | `record` | Character, factor, integer, or finite double; nonmissing, nonblank, unpadded | Character |
#' | `event_id` | Positive character digits without leading zeros, integer, or whole number double; typed missing/blank only where the event dimension is inapplicable | Character event ID and mapped raw event name; `NA_character_` where inapplicable |
#' | `field_name` | Character; exact nonblank, unpadded raw metadata field name | Character |
#' | `repeat_instrument` | Character raw instrument, or character blank/typed missing only where inapplicable | Character; inapplicable values become `NA_character_` |
#' | `instance` | Positive character digits without leading zeros, integer, or whole number double; typed missing/blank only where inapplicable | Integer; inapplicable values become `NA_integer_` |
#' | `ts` | Nonmissing `POSIXct`, or a REDCap/ISO-8601 character timestamp with date, seconds, optional fractional seconds, and optional `Z` or numeric offset | `POSIXct` in UTC; text without a timezone is interpreted as UTC |
#' | `current_query_status` | Character; nonmissing, nonblank, unpadded, case sensitive | Character |
#' | `username` | Character; nonmissing, nonblank, unpadded, case sensitive | Character |
#'
#' Inapplicable repeat and event dimensions must normalize to typed missing values;
#' applicable dimensions must exactly match an `assessible_targets` row. `NaN`,
#' infinity, padded identifiers, invalid timestamps, unknown project/event/field
#' contexts, illegal repeat shapes, and verification contexts outside the plan
#' are errors.
#'
#' Rows are filtered to exact `verified_user`, grouped by normalized field
#' context, and reduced to the latest timestamp. Identical rows tied at the
#' latest timestamp collapse; conflicting latest ties error. A verification
#' override is eligible only when the latest row for the exact selected user
#' and normalized field context has status `"VERIFIED"`, the target has passed
#' its upstream gates and instrument start check, the field remains selected
#' and applicable, and its raw `field-complete` disposition is `"failed"`.
#' Eligible rows change the effective disposition to `"passed"`. The stored
#' verification component contains audit counts. Supplied rows are excluded.
#'
#' @section Return value:
#' A `redcapmissing` object with exactly `plan`, `target_results`, `summary`,
#' `missing`, `verification`, `diagnostics`, and `details`. The stored components
#' exclude source `data`, raw `verified`, tokens, and the live connection.
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
  field_dictionary <- .rcm_run_field_dictionary(metadata)
  detection <- .rcm_run_detection_plan(
    metadata,
    plan$instruments,
    id_field,
    field_dictionary = field_dictionary
  )
  .rcm_run_require_response_columns(normalized_data,
    unlist(detection$export_fields, use.names = FALSE), "instrument start detection")
  complete_stage(started)

  started <- Sys.time()
  field_plan <- .rcm_run_field_plan(
    metadata,
    plan$instruments,
    required_fields,
    ignore_fields,
    exclude_types,
    strict_exclude_types = !exclude_types_was_missing,
    field_dictionary = field_dictionary
  )
  .rcm_run_require_response_columns(normalized_data,
    unlist(field_plan$export_fields, use.names = FALSE), "field-complete assessment")
  branch_fields <- .rcm_run_branch_export_fields(
    metadata,
    field_plan$branching_logic,
    field_dictionary = field_dictionary
  )
  .rcm_run_require_response_columns(normalized_data, branch_fields,
                                    "branching logic evaluation")
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
  instrument_status <- .rcm_run_instrument_started(
    targets = targets,
    target_row = joins$target_row,
    upstream_pass = upstream_pass,
    normalized_data = normalized_data,
    detection_fields = detection$export_fields,
    initial_status = instrument_status
  )
  complete_stage(started)

  started <- Sys.time()
  assessment <- .rcm_run_assess_targets(
    targets = targets,
    target_row = joins$target_row,
    instrument_status = instrument_status,
    normalized_data = normalized_data,
    metadata = metadata,
    field_plan = field_plan,
    field_dictionary = field_dictionary,
    branch_fields = branch_fields,
    instruments = plan$instruments,
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
  if (nrow(field_rows)) {
    apply <- .rcm_run_verification_matches(
      field_rows,
      targets,
      verification$contexts
    )
    field_rows$verification_applied <- apply
    field_rows$effective_disposition <- field_rows$raw_disposition
    field_rows$effective_disposition[apply] <- "passed"
    overrides <- sum(apply)
    fields_failed <- tabulate(
      field_rows$.target_row[field_rows$effective_disposition == "failed"],
      nbins = nrow(targets)
    )
    assessed_targets <- fields_assessed > 0L
    field_status[assessed_targets] <- ifelse(
      fields_failed[assessed_targets] > 0L,
      "failed",
      "passed"
    )
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
  summary <- .rcm_run_summary(targets, target_results)
  missing_rows <- .rcm_run_missing(
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
      .rcm_run_check_rows(targets, target_results, field_rows)
    } else {
      NULL
    }
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

.rcm_run_field_dictionary <- function(metadata) {
  field_names <- as.character(metadata$field_name)
  duplicated_fields <- unique(field_names[duplicated(field_names)])
  if (length(duplicated_fields)) {
    .rcm_plan_abort(
      paste0(
        "Metadata must define every field exactly once; duplicated field(s): ",
        paste(duplicated_fields, collapse = ", "),
        "."
      ),
      "project"
    )
  }

  derived <- .miss_derive_field_names(metadata)
  export_fields <- split(
    as.character(derived$export_field_name),
    factor(derived$original_field_name, levels = field_names),
    drop = FALSE
  )

  list(
    export_fields = export_fields,
    choice_map = .miss_build_choice_map(metadata)
  )
}

.rcm_run_detection_plan <- function(
  metadata,
  instruments,
  id_field,
  field_dictionary = NULL
) {
  field_dictionary <- field_dictionary %||% .rcm_run_field_dictionary(metadata)
  rows <- metadata[metadata$form_name %in% instruments &
    metadata$field_name != id_field & !metadata$field_type %in% c("descriptive", "calc"),
    , drop = FALSE]
  result <- stats::setNames(vector("list", length(instruments)), instruments)
  for (instrument in instruments) {
    instrument_rows <- rows[rows$form_name == instrument, , drop = FALSE]
    fields <- unique(unlist(
      .rcm_run_export_fields(
        metadata, instrument_rows$field_name, field_dictionary = field_dictionary
      ),
      use.names = FALSE
    ))
    if (!length(fields)) {
      .rcm_plan_abort(paste0("Instrument `", instrument,
        "` has no usable instrument start detection fields."), "project")
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
  strict_exclude_types = TRUE,
  field_dictionary = NULL
) {
  field_dictionary <- field_dictionary %||% .rcm_run_field_dictionary(metadata)
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
  selected$export_fields <- .rcm_run_export_fields(
    metadata,
    selected$field_name,
    field_dictionary = field_dictionary
  )
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
.rcm_run_export_fields <- function(metadata, fields, field_dictionary = NULL) {
  fields <- as.character(fields)
  field_dictionary <- field_dictionary %||% .rcm_run_field_dictionary(metadata)
  unknown <- setdiff(fields, names(field_dictionary$export_fields))
  if (length(unknown)) {
    .rcm_plan_abort(
      paste0("Metadata must define field `", unknown[[1L]], "` exactly once."),
      "project"
    )
  }
  unname(field_dictionary$export_fields[fields])
}

.rcm_run_require_response_columns <- function(data, export_fields, purpose) {
  missing_columns <- setdiff(unique(as.character(export_fields)), names(data))
  if (length(missing_columns)) {
    .rcm_plan_abort(paste0("`data` is missing column(s) required for ", purpose,
      ": ", paste(missing_columns, collapse = ", "), "."), "schema")
  }
  invisible(data)
}

.rcm_run_branch_export_fields <- function(
  metadata,
  logic,
  field_dictionary = NULL
) {
  field_dictionary <- field_dictionary %||% .rcm_run_field_dictionary(metadata)
  refs <- .miss_extract_logic_references(logic)
  if (!nrow(refs)) return(character())
  unknown <- setdiff(refs$field, as.character(metadata$field_name))
  if (length(unknown)) {
    .rcm_plan_abort(paste0("Branching logic references unknown field(s): ",
      paste(unknown, collapse = ", "), "."), "project")
  }
  unique(unlist(.rcm_run_export_fields(
    metadata, unique(refs$field), field_dictionary = field_dictionary
  ), use.names = FALSE))
}

.rcm_run_row_groups <- function(data, columns) {
  n <- nrow(data)
  if (!n) return(integer())
  if (!length(columns)) return(rep.int(1L, n))

  values <- unname(as.list(data[, columns, drop = FALSE]))
  ordering <- do.call(order, c(values, list(na.last = TRUE, method = "radix")))
  same_as_previous <- rep.int(TRUE, max(0L, n - 1L))
  if (n > 1L) {
    previous <- ordering[-n]
    current <- ordering[-1L]
    for (value in values) {
      left <- value[previous]
      right <- value[current]
      both_missing <- is.na(left) & is.na(right)
      both_present <- !is.na(left) & !is.na(right)
      equal <- both_missing | (both_present & left == right)
      equal[is.na(equal)] <- FALSE
      same_as_previous <- same_as_previous & equal
    }
  }

  sorted_group <- cumsum(c(TRUE, !same_as_previous))
  group <- integer(n)
  group[ordering] <- sorted_group
  group
}

.rcm_run_match_rows <- function(needles, haystack, columns) {
  needle_n <- nrow(needles)
  haystack_n <- nrow(haystack)
  if (!needle_n) return(integer())
  if (!haystack_n) return(rep.int(NA_integer_, needle_n))

  combined <- rbind(
    as.data.frame(haystack[, columns, drop = FALSE]),
    as.data.frame(needles[, columns, drop = FALSE])
  )
  groups <- .rcm_run_row_groups(combined, columns)
  match(
    groups[haystack_n + seq_len(needle_n)],
    groups[seq_len(haystack_n)]
  )
}

.rcm_run_join_targets <- function(targets, data, longitudinal) {
  target_context <- data.frame(
    record_id = targets$record_id,
    event = targets$redcap_event_name,
    repeat_instrument = targets$repeat_instrument,
    repeat_instance = targets$repeat_instance,
    stringsAsFactors = FALSE
  )
  data_context <- data.frame(
    record_id = data$.rcm_record_id,
    event = data$redcap_event_name,
    repeat_instrument = data$redcap_repeat_instrument,
    repeat_instance = data$redcap_repeat_instance,
    stringsAsFactors = FALSE
  )
  target_row <- .rcm_run_match_rows(
    target_context,
    data_context,
    names(target_context)
  )
  event_present <- if (isTRUE(longitudinal)) {
    !is.na(.rcm_run_match_rows(
      target_context,
      data_context,
      c("record_id", "event")
    ))
  } else {
    rep.int(TRUE, nrow(targets))
  }
  list(
    target_row = as.integer(target_row),
    target_present = !is.na(target_row),
    event_present = event_present
  )
}

.rcm_run_is_missing <- function(x) {
  missing <- is.na(x)
  if (is.character(x) || is.factor(x)) missing <- missing | !nzchar(trimws(as.character(x)))
  if (is.numeric(x)) missing <- missing | is.nan(x)
  missing
}

.rcm_run_instrument_started <- function(
  targets,
  target_row,
  upstream_pass,
  normalized_data,
  detection_fields,
  initial_status
) {
  status <- initial_status
  candidates <- which(upstream_pass & !is.na(target_row))
  if (!length(candidates)) return(status)

  for (instrument in names(detection_fields)) {
    target_index <- candidates[targets$instrument[candidates] == instrument]
    if (!length(target_index)) next

    data_index <- target_row[target_index]
    started <- rep.int(FALSE, length(target_index))
    for (field in detection_fields[[instrument]]) {
      value <- normalized_data[[field]][data_index]
      present <- if (grepl("___", field, fixed = TRUE)) {
        .miss_checkbox_selected_vec(value)
      } else {
        !.rcm_run_is_missing(value)
      }
      started <- started | present
    }
    status[target_index] <- ifelse(started, "passed", "failed")
  }
  status
}

.rcm_run_empty_field_rows <- function() {
  tibble::tibble(
    .target_row = integer(), .field_order = integer(),
    field_name = character(), field_label = character(), field_type = character(),
    branching_logic = character(), branch_satisfied = logical(),
    value_summary = character(), raw_disposition = character(),
    verification_applied = logical(), effective_disposition = character()
  )
}

.rcm_run_checkbox_result <- function(
  records,
  row_index,
  fields,
  include_summary
) {
  n <- length(row_index)
  selected <- vapply(fields, function(field) {
    .miss_checkbox_selected_vec(records[[field]][row_index])
  }, logical(n))
  if (is.null(dim(selected))) {
    selected <- matrix(selected, nrow = n, ncol = length(fields))
  }
  passed <- rowSums(selected, na.rm = TRUE) > 0L
  if (!isTRUE(include_summary)) {
    return(list(passed = passed, value_summary = rep.int(NA_character_, n)))
  }

  value_summary <- rep.int("", n)
  for (i in seq_along(fields)) {
    hit <- selected[, i]
    if (!any(hit)) next
    existing <- nzchar(value_summary[hit])
    value_summary[hit] <- ifelse(
      existing,
      paste0(value_summary[hit], ", ", fields[[i]]),
      fields[[i]]
    )
  }
  list(passed = passed, value_summary = value_summary)
}

.rcm_run_assess_targets <- function(
  targets,
  target_row,
  instrument_status,
  normalized_data,
  metadata,
  field_plan,
  field_dictionary,
  branch_fields,
  instruments,
  retain_passed_fields
) {
  target_n <- nrow(targets)
  field_status <- rep.int("not reached", target_n)
  fields_assessed <- integer(target_n)
  fields_failed <- integer(target_n)
  field_reason <- rep.int(NA_character_, target_n)
  pieces <- list()
  piece_n <- 0L
  compiled_branches <- new.env(hash = TRUE, parent = emptyenv())
  project <- list(id_col = ".rcm_record_id", system_fields = .miss_system_fields())

  for (instrument in instruments) {
    active_targets <- which(
      targets$instrument == instrument & instrument_status == "passed"
    )
    if (!length(active_targets)) next

    instrument_fields <- field_plan$rows[[instrument]]
    if (!nrow(instrument_fields)) {
      field_status[active_targets] <- "not applicable"
      field_reason[active_targets] <- "no fields remain after field policy"
      next
    }

    source_rows <- target_row[active_targets]
    if (anyNA(source_rows)) {
      .rcm_plan_abort(
        "A target that passed instrument start could not be matched to its physical row.",
        "schema"
      )
    }
    needed_columns <- unique(c(
      ".rcm_record_id",
      unname(unlist(project$system_fields, use.names = FALSE)),
      branch_fields,
      unlist(instrument_fields$export_fields, use.names = FALSE)
    ))
    records <- normalized_data[source_rows, needed_columns, drop = FALSE]
    branch_cache <- .miss_new_branch_cache(
      records = records,
      lookup_records = normalized_data,
      project = project
    )

    for (field_index in seq_len(nrow(instrument_fields))) {
      logic <- as.character(instrument_fields$branching_logic[[field_index]])
      branch_satisfied <- tryCatch({
        branch_plan <- NULL
        if (!.miss_is_blank_scalar(logic)) {
          if (!exists(logic, envir = compiled_branches, inherits = FALSE)) {
            assign(
              logic,
              .miss_compile_branch_logic(logic),
              envir = compiled_branches
            )
          }
          branch_plan <- get(logic, envir = compiled_branches, inherits = FALSE)
        }
        .miss_branch_satisfied(
          logic = logic,
          branch_plan = branch_plan,
          branch_cache = branch_cache,
          records = records,
          lookup_records = normalized_data,
          meta = metadata,
          choice_map = field_dictionary$choice_map,
          project = project
        )
      }, error = function(error) {
        .rcm_plan_abort(
          paste0(
            "Could not evaluate REDCap branching logic `",
            logic,
            "`: ",
            conditionMessage(error)
          ),
          "project"
        )
      })
      applicable_rows <- which(branch_satisfied)
      if (!length(applicable_rows)) next

      target_index <- active_targets[applicable_rows]
      export_fields <- instrument_fields$export_fields[[field_index]]
      field_type <- as.character(instrument_fields$field_type[[field_index]])
      if (identical(field_type, "checkbox")) {
        outcome <- .rcm_run_checkbox_result(
          records,
          applicable_rows,
          export_fields,
          include_summary = retain_passed_fields
        )
        passed <- outcome$passed
        value_summary <- outcome$value_summary
      } else {
        value <- records[[export_fields[[1L]]]][applicable_rows]
        passed <- !.rcm_run_is_missing(value)
        value_summary <- if (isTRUE(retain_passed_fields)) {
          .miss_chr_vec(value)
        } else {
          rep.int(NA_character_, length(value))
        }
      }

      fields_assessed[target_index] <- fields_assessed[target_index] + 1L
      failed_targets <- target_index[!passed]
      fields_failed[failed_targets] <- fields_failed[failed_targets] + 1L

      retain <- if (isTRUE(retain_passed_fields)) {
        rep.int(TRUE, length(passed))
      } else {
        !passed
      }
      if (!any(retain)) next

      raw_disposition <- ifelse(passed[retain], "passed", "failed")
      piece_n <- piece_n + 1L
      pieces[[piece_n]] <- tibble::tibble(
        .target_row = as.integer(target_index[retain]),
        .field_order = rep.int(as.integer(field_index), sum(retain)),
        field_name = rep.int(
          as.character(instrument_fields$field_name[[field_index]]), sum(retain)
        ),
        field_label = rep.int(
          as.character(instrument_fields$field_label[[field_index]]), sum(retain)
        ),
        field_type = rep.int(field_type, sum(retain)),
        branching_logic = rep.int(logic, sum(retain)),
        branch_satisfied = rep.int(TRUE, sum(retain)),
        value_summary = value_summary[retain],
        raw_disposition = raw_disposition,
        verification_applied = rep.int(FALSE, sum(retain)),
        effective_disposition = raw_disposition
      )
    }

    assessed <- fields_assessed[active_targets] > 0L
    assessed_targets <- active_targets[assessed]
    field_status[assessed_targets] <- ifelse(
      fields_failed[assessed_targets] > 0L,
      "failed",
      "passed"
    )
    not_applicable <- active_targets[!assessed]
    field_status[not_applicable] <- "not applicable"
    field_reason[not_applicable] <- "no fields apply after branching logic"
  }

  field_rows <- if (!piece_n) {
    .rcm_run_empty_field_rows()
  } else {
    dplyr::bind_rows(pieces)
  }
  if (nrow(field_rows)) {
    field_rows <- field_rows[
      order(field_rows$.target_row, field_rows$.field_order, method = "radix"),
      ,
      drop = FALSE
    ]
    row.names(field_rows) <- NULL
  }

  list(
    field_status = field_status,
    fields_assessed = as.integer(fields_assessed),
    fields_failed = as.integer(fields_failed),
    field_reason = field_reason,
    field_rows = tibble::as_tibble(field_rows)
  )
}

.rcm_run_verification_matches <- function(field_rows, targets, contexts) {
  matched <- rep.int(FALSE, nrow(field_rows))
  candidate <- which(field_rows$raw_disposition == "failed")
  if (!length(candidate) || !nrow(contexts)) return(matched)

  target_index <- field_rows$.target_row[candidate]
  candidate_context <- data.frame(
    record_id = targets$record_id[target_index],
    redcap_event_name = targets$redcap_event_name[target_index],
    repeat_instrument = targets$repeat_instrument[target_index],
    repeat_instance = targets$repeat_instance[target_index],
    field_name = field_rows$field_name[candidate],
    stringsAsFactors = FALSE
  )
  matched[candidate] <- !is.na(.rcm_run_match_rows(
    candidate_context,
    contexts,
    names(candidate_context)
  ))
  matched
}

.rcm_run_public_field_rows <- function(field_rows, targets) {
  if (!nrow(field_rows)) {
    return(tibble::tibble(
      record_id = character(), instrument = character(),
      redcap_event_name = character(), repeat_instrument = character(),
      repeat_instance = integer(), target_source = character(),
      field_name = character(), field_label = character(), field_type = character(),
      branching_logic = character(), branch_satisfied = logical(),
      value_summary = character(), raw_disposition = character(),
      verification_applied = logical(), effective_disposition = character()
    ))
  }

  target_index <- field_rows$.target_row
  tibble::tibble(
    record_id = targets$record_id[target_index],
    instrument = targets$instrument[target_index],
    redcap_event_name = targets$redcap_event_name[target_index],
    repeat_instrument = targets$repeat_instrument[target_index],
    repeat_instance = targets$repeat_instance[target_index],
    target_source = targets$target_source[target_index],
    field_name = field_rows$field_name,
    field_label = field_rows$field_label,
    field_type = field_rows$field_type,
    branching_logic = field_rows$branching_logic,
    branch_satisfied = field_rows$branch_satisfied,
    value_summary = field_rows$value_summary,
    raw_disposition = field_rows$raw_disposition,
    verification_applied = field_rows$verification_applied,
    effective_disposition = field_rows$effective_disposition
  )
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
      value_summary = rep(NA_character_, n),
      raw_disposition = as.character(disposition),
      verification_applied = rep(FALSE, n),
      effective_disposition = as.character(disposition),
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
    public_fields <- .rcm_run_public_field_rows(field_rows, targets)
    rows[[5L]] <- tibble::tibble(
      record_id = public_fields$record_id,
      instrument = public_fields$instrument,
      redcap_event_name = public_fields$redcap_event_name,
      repeat_instrument = public_fields$repeat_instrument,
      repeat_instance = public_fields$repeat_instance,
      target_source = public_fields$target_source,
      validation_level = .rcm_run_validation_level(public_fields$repeat_instance),
      validation_check = "field-complete",
      field_name = public_fields$field_name,
      field_label = public_fields$field_label,
      field_type = public_fields$field_type,
      branching_logic = public_fields$branching_logic,
      branch_satisfied = public_fields$branch_satisfied,
      value_summary = public_fields$value_summary,
      raw_disposition = public_fields$raw_disposition,
      verification_applied = public_fields$verification_applied,
      effective_disposition = public_fields$effective_disposition,
      reason = NA_character_
    )
  }
  dplyr::bind_rows(rows)
}

.rcm_run_summary <- function(targets, target_results) {
  empty <- tibble::tibble(
    redcap_event_name = character(), instrument = character(),
    repeat_instrument = character(), repeat_instance = integer(),
    validation_level = character(), validation_check = character(),
    status = character(), reason = character(), assessed = integer(),
    passed = integer(), failed = integer(), pass_rate = double(),
    fail_rate = double()
  )
  if (!nrow(targets)) return(empty)

  context_columns <- c(
    "redcap_event_name", "instrument", "repeat_instrument", "repeat_instance"
  )
  raw_group <- .rcm_run_row_groups(targets, context_columns)
  context_group <- match(raw_group, unique(raw_group))
  group_n <- length(unique(context_group))
  first <- match(seq_len(group_n), context_group)
  group_size <- tabulate(context_group, nbins = group_n)

  first_reason <- function(reason) {
    result <- rep.int(NA_character_, group_n)
    available <- !is.na(reason)
    if (!any(available)) return(result)
    position <- match(seq_len(group_n), context_group[available])
    hit <- !is.na(position)
    result[hit] <- reason[available][position[hit]]
    result
  }
  sum_by_context <- function(value) {
    as.integer(rowsum(
      as.integer(value),
      context_group,
      reorder = FALSE
    )[, 1L])
  }
  build_summary <- function(
    check,
    disposition,
    reason,
    assessed = NULL,
    passed = NULL,
    failed = NULL
  ) {
    if (is.null(assessed)) {
      passed <- tabulate(
        context_group[disposition == "passed"],
        nbins = group_n
      )
      failed <- tabulate(
        context_group[disposition == "failed"],
        nbins = group_n
      )
      assessed <- passed + failed
    }
    not_applicable <- tabulate(
      context_group[disposition == "not applicable"],
      nbins = group_n
    ) == group_size
    group_reason <- first_reason(reason)
    group_reason[!not_applicable] <- NA_character_

    tibble::tibble(
      redcap_event_name = targets$redcap_event_name[first],
      instrument = targets$instrument[first],
      repeat_instrument = targets$repeat_instrument[first],
      repeat_instance = targets$repeat_instance[first],
      validation_level = .rcm_run_validation_level(
        targets$repeat_instance[first]
      ),
      validation_check = rep.int(check, group_n),
      status = ifelse(not_applicable, "not applicable", "assessed"),
      reason = group_reason,
      assessed = as.integer(assessed),
      passed = as.integer(passed),
      failed = as.integer(failed),
      pass_rate = ifelse(assessed == 0L, NA_real_, passed / assessed),
      fail_rate = ifelse(assessed == 0L, NA_real_, failed / assessed),
      .context_group = seq_len(group_n)
    )
  }

  event_reason <- ifelse(
    target_results$event_row_started == "not applicable",
    "not applicable for classic project",
    NA_character_
  )
  repeat_reason <- ifelse(
    target_results$repeat_instance_row_started == "not applicable",
    "not a repeating target",
    NA_character_
  )
  field_assessed <- sum_by_context(target_results$fields_assessed)
  field_failed <- sum_by_context(target_results$fields_failed)

  result <- dplyr::bind_rows(
    build_summary(
      "event-row-started",
      target_results$event_row_started,
      event_reason
    ),
    build_summary(
      "repeat-instance-row-started",
      target_results$repeat_instance_row_started,
      repeat_reason
    ),
    build_summary(
      "instrument-started",
      target_results$instrument_started,
      rep.int(NA_character_, nrow(targets))
    ),
    build_summary(
      "field-complete",
      target_results$field_complete,
      target_results$field_applicability_reason,
      assessed = field_assessed,
      passed = field_assessed - field_failed,
      failed = field_failed
    )
  )
  result <- result[order(
    result$.context_group,
    match(
      result$validation_check,
      .redcapmissing_validation_checks()
    ),
    method = "radix"
  ),
    setdiff(names(result), ".context_group"),
    drop = FALSE
  ]
  row.names(result) <- NULL
  tibble::as_tibble(result)
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
.rcm_run_missing <- function(
  targets,
  target_results,
  field_rows,
  rcon,
  snapshot
) {
  empty <- tibble::tibble(
    record_id = character(), redcap_event_name = character(),
    repeat_instrument = character(), repeat_instance = integer(),
    validation_context = character(), instrument = character(),
    validation_check = character(), field_name = character(),
    field_label = character(), field_type = character(),
    branching_logic = character(), url = character()
  )
  target_failure <- function(check, disposition) {
    index <- which(disposition == "failed")
    if (!length(index)) return(NULL)
    tibble::tibble(
      record_id = targets$record_id[index],
      redcap_event_name = targets$redcap_event_name[index],
      repeat_instrument = targets$repeat_instrument[index],
      repeat_instance = targets$repeat_instance[index],
      instrument = targets$instrument[index],
      validation_check = rep.int(check, length(index)),
      field_name = rep.int(NA_character_, length(index)),
      field_label = rep.int(NA_character_, length(index)),
      field_type = rep.int(NA_character_, length(index)),
      branching_logic = rep.int(NA_character_, length(index))
    )
  }

  pieces <- list(
    target_failure("event-row-started", target_results$event_row_started),
    target_failure(
      "repeat-instance-row-started",
      target_results$repeat_instance_row_started
    ),
    target_failure("instrument-started", target_results$instrument_started)
  )
  effective_failure <- which(field_rows$effective_disposition == "failed")
  if (length(effective_failure)) {
    public_fields <- .rcm_run_public_field_rows(
      field_rows[effective_failure, , drop = FALSE],
      targets
    )
    pieces[[4L]] <- tibble::tibble(
      record_id = public_fields$record_id,
      redcap_event_name = public_fields$redcap_event_name,
      repeat_instrument = public_fields$repeat_instrument,
      repeat_instance = public_fields$repeat_instance,
      instrument = public_fields$instrument,
      validation_check = rep.int("field-complete", nrow(public_fields)),
      field_name = public_fields$field_name,
      field_label = public_fields$field_label,
      field_type = public_fields$field_type,
      branching_logic = public_fields$branching_logic
    )
  }

  rows <- dplyr::bind_rows(pieces)
  if (!nrow(rows)) return(empty)
  rows <- .rcm_run_add_urls(rows, rcon, snapshot)
  rows$validation_context <- .miss_validation_context_vec(
    rows$redcap_event_name,
    rows$repeat_instance
  )
  rows <- rows[, c(
    "record_id", "redcap_event_name", "repeat_instrument", "repeat_instance",
    "validation_context", "instrument", "validation_check", "field_name",
    "field_label", "field_type", "branching_logic", "url"
  ), drop = FALSE]
  tibble::as_tibble(rows)
}