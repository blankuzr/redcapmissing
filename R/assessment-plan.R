#' Construct an assessment plan from observed REDCap rows
#'
#' `plan_from_data()` identifies `assessible_targets` by intersecting REDCap
#' instrument/event/repeat crossings allowed by `rcon` with crossings observed
#' in `data` or added through `extended_schedule`. An absent extension row
#' leaves observed targets unchanged.
#'
#' @param data A data frame exported from the REDCap project represented by
#'   `rcon`. It must contain at least one physical REDCap row. Planning uses
#'   structural row identity. Instrument response fields may be absent, and a
#'   response row containing only blanks remains observed. Every supplied column
#'   must use ordinary atomic vector storage; list or matrix columns are
#'   rejected. `rcon` supplies the name of the record ID column.
#' @param rcon A `redcapAPI` connection inheriting from
#'   `redcapApiConnection`, as created by [redcapAPI::redcapConnection()], or
#'   `redcapOfflineConnection`, as created by [redcapAPI::offlineConnection()]
#'   or [redcapAPI::readPreservedProject()]. It must expose project information,
#'   metadata, instruments, and applicable arms, events, mappings, and repeat
#'   configuration. A missing repeat configuration surface is an error; an
#'   explicit empty surface represents a project with no repeats. Constructors
#'   inspect project structure and use records supplied in `data`.
#' @param instruments A nonempty character vector of unique, nonmissing,
#'   nonblank, unpadded raw REDCap instrument names. Values must exist in the
#'   project. Their order determines target ordering.
#' @param extended_schedule `NULL`, or a data frame with exactly the columns
#'   `instrument`, `redcap_event_name`, and `repeat_instance`, in that order.
#'   Each row adds its exact crossing for every record observed in the applicable
#'   arm; in a classic project it adds the crossing for every observed record.
#'   `NULL` and a correctly typed empty table mean observed only planning.
#'
#' @section `assessible_targets` rule:
#' `plan_from_data()` implements:
#'
#' ```text
#' assessible_targets =
#'   crossings allowed by rcon
#'   INTERSECT
#'   (crossings observed in data UNION extended_schedule crossings)
#' ```
#'
#' Schedule rows are validated before intersection. An extension into an arm
#' with no observed records emits one classed warning and adds no targets.
#'
#' @section Extended schedule schema:
#' The function accepts only these column names, order, and storage:
#'
#' | Column | Accepted storage and values | Normalized storage |
#' |---|---|---|
#' | `instrument` | Character or factor; one selected raw instrument name per row; no missing, blank, padded, or unknown values | Character |
#' | `redcap_event_name` | Character or factor raw event names in longitudinal projects; character/factor blanks or typed missing values only in classic projects | Character; `NA_character_` in classic projects |
#' | `repeat_instance` | Positive integer, whole number double, or digit string without leading zeros when the scheduled event or instrument repeats; typed missing or blank only when neither repeats | Integer; `NA_integer_` when neither the scheduled event nor instrument repeats |
#'
#' Extra columns, incorrect column order, incomplete empty schemas, duplicate
#' normalized rows, unselected instruments, unknown events, and event,
#' instrument, or repeat combinations disallowed by `rcon` are errors. An
#' exact repeat instance, such as `2`, adds instance 2 exclusively.
#'
#' @section Structural data normalization:
#' Record IDs accept character, factor, integer, or finite double storage and
#' normalize to character. Character leading zeros are preserved. Missing,
#' blank, padded, `NaN`, and infinite identifiers produce schema errors.
#'
#' Longitudinal rows require a nonblank raw `redcap_event_name`. In classic
#' projects that column may be absent, or contain only character/factor blanks
#' or typed missing values, and normalizes to `NA_character_`; a nonblank value
#' is an error.
#'
#' When the project configures repeating events or instruments, `data` must
#' contain both `redcap_repeat_instrument` and `redcap_repeat_instance`. A row
#' where neither its event nor its instrument repeats uses `NA_character_` and
#' `NA_integer_`, respectively. A row at a repeating event uses
#' `NA_character_` and a positive instance. A row for a repeating instrument
#' uses its raw instrument name and a positive instance. Repeat instances
#' accept positive integers, whole number doubles, or digit strings without
#' leading zeros, and normalize to integer. Strings with leading zeros, zero,
#' negative, decimal, missing instances where an event or instrument repeats,
#' other text, `NaN`, infinity, and integer overflow are errors.
#'
#' The normalized physical row key is record ID, event, repeat instrument, and
#' repeat instance. Every row must match one of the three repeat structures
#' described above as allowed by `rcon`, and keys must be unique. Normalization
#' collisions and duplicate keys are errors.
#'
#' @section Returned plan:
#' The returned object has class `redcapmissing_plan` and these components:
#'
#' | Component | Storage and meaning |
#' |---|---|
#' | `schema_version` | Integer scalar `1L` |
#' | `construction` | Character scalar `"from_data"` or `"explicit"` |
#' | `instruments` | Character vector of selected raw instrument names in target order |
#' | `assessible_targets` | Tibble described below |
#' | `project` | Named project identity and label list described below |
#' | `structure_fingerprint` | One lowercase SHA-256 value containing 64 characters |
#'
#' `construction` is `"from_data"` for `plan_from_data()` and `"explicit"` for
#' `plan_explicit()`.
#'
#' `assessible_targets` has exactly these columns:
#'
#' | Column | Storage | Meaning |
#' |---|---|---|
#' | `record_id` | Character | Normalized project record ID |
#' | `instrument` | Character | Selected raw REDCap instrument name |
#' | `redcap_event_name` | Character | Raw event name, or `NA_character_` in a classic project |
#' | `repeat_instrument` | Character | Raw instrument for repeating instrument targets; otherwise `NA_character_` |
#' | `repeat_instance` | Integer | Exact positive repeat instance; otherwise `NA_integer_` |
#' | `target_source` | Character | `"observed"`, `"extended"`, `"observed+extended"`, or `"explicit"`, as allowed by the construction type |
#'
#' Targets are unique on the first five columns and deterministically ordered by
#' selected instrument order, REDCap event order, record ID, repeat kind, and
#' instance.
#'
#' `project` is a named list with exactly these fields:
#'
#' | Field | Storage and meaning |
#' |---|---|
#' | `project_id` | Nonblank character scalar |
#' | `record_id_field` | Nonblank character scalar naming the REDCap record field |
#' | `longitudinal` | One nonmissing logical value |
#' | `event_labels` | Named character vector ordered by raw event name; empty for classic projects |
#' | `instrument_labels` | Named character vector ordered by raw instrument name |
#'
#' The SHA-256 `structure_fingerprint` represents normalized project identity,
#' metadata, instruments, arms, events, mappings, and repeat configuration. Its
#' value is independent of source table row order. The stored plan excludes
#' `data` and the live connection.
#'
#' @section Conditions:
#' Validation failures inherit from `redcapmissing_error`, with the more
#' specific classes `redcapmissing_error_argument`,
#' `redcapmissing_error_schema`, `redcapmissing_error_project`,
#' `redcapmissing_error_schedule`, or `redcapmissing_error_plan` as applicable.
#' An extension into an arm with no observed records emits
#' `redcapmissing_warning_empty_arm_extension`, which also inherits from
#' `redcapmissing_warning`.
#'
#' @return A validated `redcapmissing_plan` as described in **Returned plan**.
#'
#' @examples
#' \dontrun{
#' # records, rcon, instruments, and extended_schedule are caller supplied.
#' observed_plan <- plan_from_data(records, rcon, instruments)
#' extended_plan <- plan_from_data(
#'   records, rcon, instruments, extended_schedule
#' )
#' }
#'
#' @seealso [plan_explicit()], [run_plan()]
#' @export
plan_from_data <- function(data, rcon, instruments, extended_schedule = NULL) {
  if (missing(data) || is.null(data)) {
    .condition_signal_error("`data` is required and cannot be `NULL`.", "argument")
  }
  if (missing(rcon) || is.null(rcon)) {
    .condition_signal_error("`rcon` is required and cannot be `NULL`.", "argument")
  }
  if (missing(instruments) || is.null(instruments)) {
    .condition_signal_error("`instruments` is required and cannot be `NULL`.", "argument")
  }
  snapshot <- .project_structure_build_snapshot(rcon)
  instruments <- .schedule_validate_instruments(instruments, snapshot)
  data <- .record_normalize_export(data, snapshot, require_nonempty = TRUE)
  observed <- .assessible_target_build_observed(data, snapshot, instruments)
  extended <- if (is.null(extended_schedule)) {
    .assessible_target_build_prototype()
  } else {
    schedule <- .schedule_normalize_rows(
      extended_schedule, "extended", snapshot, instruments, data
    )
    .assessible_target_build_scheduled(schedule, snapshot, data, "extended")
  }
  targets <- .assessible_target_merge_sources(observed, extended, "from_data")
  targets <- .assessible_target_order_rows(targets, snapshot, instruments)
  .plan_build_object("from_data", instruments, targets, snapshot)
}

#' Construct an explicit REDCap assessment plan
#'
#' `plan_explicit()` assesses only the exact record, instrument, event, and
#' repeat instance crossings declared by `explicit_schedule`, after validating
#' that each crossing is allowed by `rcon`. A schedule row may identify a record
#' absent from `data`; that absence is evaluated later by [run_plan()].
#'
#' @inheritParams plan_from_data
#' @param data A data frame exported from the REDCap project represented by
#'   `rcon`. Only structural columns are required, and a correctly structured
#'   empty data frame is allowed. Every supplied column must use ordinary
#'   atomic vector storage. Structural normalization follows [plan_from_data()].
#' @param explicit_schedule A data frame with exactly the columns `record_id`,
#'   `instrument`, `redcap_event_name`, and `repeat_instance`, in that order.
#'   An absent row means do not assess that crossing. A correctly typed empty
#'   schedule creates a valid plan with no `assessible_targets` rows. `NULL` and an
#'   omitted argument are errors.
#'
#' @section `assessible_targets` rule:
#' `plan_explicit()` implements:
#'
#' ```text
#' assessible_targets =
#'   crossings allowed by rcon
#'   INTERSECT
#'   explicit_schedule crossings
#' ```
#'
#' Every schedule row identifies exactly one target. Scheduled rows are the
#' complete target set; a selected instrument with no schedule rows has no
#' targets. A record ID absent from `data` is allowed, and [run_plan()] applies
#' its checks according to the target context. An observed record may be
#' scheduled only within its observed arm.
#'
#' @section Explicit schedule schema:
#' The function accepts only these column names, order, and storage:
#'
#' | Column | Accepted storage and values | Normalized storage |
#' |---|---|---|
#' | `record_id` | Character, factor, integer, or finite double; nonmissing, nonblank, and unpadded | Character; character leading zeros preserved |
#' | `instrument` | Character or factor; one selected raw instrument name per row; no missing, blank, padded, or unknown values | Character |
#' | `redcap_event_name` | Character or factor raw event names in longitudinal projects; character/factor blanks or typed missing values only in classic projects | Character; `NA_character_` in classic projects |
#' | `repeat_instance` | Positive integer, whole number double, or digit string without leading zeros when the scheduled event or instrument repeats; typed missing or blank only when neither repeats | Integer; `NA_integer_` when neither the scheduled event nor instrument repeats |
#'
#' Extra columns, incorrect column order, incomplete empty schemas, duplicate
#' normalized rows, and invalid or disallowed crossings are errors. The
#' structural identifier and repeat instance rejection rules documented for
#' [plan_from_data()] apply here as well.
#'
#' @inheritSection plan_from_data Returned plan
#' @inheritSection plan_from_data Conditions
#'
#' @return A validated `redcapmissing_plan` as described in **Returned plan**,
#'   with `construction = "explicit"` and `target_source = "explicit"` for
#'   every target.
#'
#' @examples
#' \dontrun{
#' # records, rcon, instruments, and explicit_schedule are caller supplied.
#' explicit_plan <- plan_explicit(
#'   records, rcon, instruments, explicit_schedule
#' )
#' }
#'
#' @seealso [plan_from_data()], [run_plan()]
#' @export
plan_explicit <- function(data, rcon, instruments, explicit_schedule) {
  if (missing(data) || is.null(data)) {
    .condition_signal_error("`data` is required and cannot be `NULL`.", "argument")
  }
  if (missing(rcon) || is.null(rcon)) {
    .condition_signal_error("`rcon` is required and cannot be `NULL`.", "argument")
  }
  if (missing(instruments) || is.null(instruments)) {
    .condition_signal_error("`instruments` is required and cannot be `NULL`.", "argument")
  }
  if (missing(explicit_schedule) || is.null(explicit_schedule)) {
    .condition_signal_error("`explicit_schedule` is required and cannot be `NULL`.", "argument")
  }
  snapshot <- .project_structure_build_snapshot(rcon)
  instruments <- .schedule_validate_instruments(instruments, snapshot)
  data <- .record_normalize_export(data, snapshot, require_nonempty = FALSE)
  schedule <- .schedule_normalize_rows(
    explicit_schedule, "explicit", snapshot, instruments, data
  )
  targets <- .assessible_target_build_scheduled(schedule, snapshot, data, "explicit")
  targets <- .assessible_target_order_rows(targets, snapshot, instruments)
  .plan_build_object("explicit", instruments, targets, snapshot)
}

#' Print a REDCap missingness assessment plan
#'
#' The display contains the construction type, number of selected instruments,
#' and number of `assessible_targets` rows. Record IDs and individual targets
#' remain in `x$assessible_targets`.
#'
#' @param x A `redcapmissing_plan` object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#'
#' @seealso [plan_from_data()], [plan_explicit()]
#' @export
print.redcapmissing_plan <- function(x, ...) {
  x <- .plan_validate_object(x)
  cat(
    "<redcapmissing_plan>\n",
    "  Construction: ", x$construction, "\n",
    "  Instruments:  ", length(x$instruments), "\n",
    "  Targets:      ", nrow(x$assessible_targets), "\n",
    sep = ""
  )
  invisible(x)
}


.plan_build_object <- function(construction, instruments, targets, snapshot) {
  plan <- structure(
    list(
      schema_version = 1L,
      construction = construction,
      instruments = instruments,
      assessible_targets = targets,
      project = snapshot$project,
      structure_fingerprint = snapshot$structure_fingerprint
    ),
    class = "redcapmissing_plan"
  )
  .plan_validate_object(plan, snapshot)
}

.plan_validate_object <- function(plan, snapshot = NULL) {
  expected_names <- c(
    "schema_version", "construction", "instruments",
    "assessible_targets", "project", "structure_fingerprint"
  )
  if (!inherits(plan, "redcapmissing_plan") || !is.list(plan) || !identical(names(plan), expected_names)) {
    .condition_signal_error("`plan` is not a valid `redcapmissing_plan` representation.", "plan")
  }
  if (!identical(plan$schema_version, 1L) ||
      !is.character(plan$construction) ||
      length(plan$construction) != 1 ||
      !plan$construction %in% c("from_data", "explicit")) {
    .condition_signal_error("`plan` has an unsupported schema version or construction type.", "plan")
  }
  if (!is.character(plan$instruments) || !length(plan$instruments) || anyNA(plan$instruments) ||
      any(plan$instruments == "") || any(trimws(plan$instruments) != plan$instruments) ||
      anyDuplicated(plan$instruments)) {
    .condition_signal_error("`plan$instruments` is malformed.", "plan")
  }
  valid_label_map <- function(x) {
    is.character(x) && !anyNA(x) &&
      (!length(x) || (
        !is.null(names(x)) && !anyNA(names(x)) &&
          all(names(x) != "") &&
          all(trimws(names(x)) == names(x)) &&
          !anyDuplicated(names(x)) &&
          identical(names(x), sort(names(x)))
      ))
  }
  if (!is.list(plan$project) ||
      !identical(
        names(plan$project),
        c("project_id", "record_id_field", "longitudinal", "event_labels", "instrument_labels")
      ) ||
      !is.character(plan$project$project_id) || length(plan$project$project_id) != 1 ||
      is.na(plan$project$project_id) || plan$project$project_id == "" ||
      trimws(plan$project$project_id) != plan$project$project_id ||
      !is.character(plan$project$record_id_field) || length(plan$project$record_id_field) != 1 ||
      is.na(plan$project$record_id_field) || plan$project$record_id_field == "" ||
      trimws(plan$project$record_id_field) != plan$project$record_id_field ||
      !is.logical(plan$project$longitudinal) || length(plan$project$longitudinal) != 1 ||
      is.na(plan$project$longitudinal) ||
      !valid_label_map(plan$project$event_labels) ||
      !valid_label_map(plan$project$instrument_labels) ||
      (!plan$project$longitudinal && length(plan$project$event_labels)) ||
      (plan$project$longitudinal && !length(plan$project$event_labels)) ||
      length(setdiff(plan$instruments, names(plan$project$instrument_labels)))) {
    .condition_signal_error("`plan$project` is malformed.", "plan")
  }
  if (!is.character(plan$structure_fingerprint) || length(plan$structure_fingerprint) != 1 ||
      is.na(plan$structure_fingerprint) ||
      !grepl("^[0-9a-f]{64}$", plan$structure_fingerprint)) {
    .condition_signal_error("`plan$structure_fingerprint` is malformed.", "plan")
  }
  targets <- plan$assessible_targets
  target_names <- c(
    "record_id", "instrument", "redcap_event_name",
    "repeat_instrument", "repeat_instance", "target_source"
  )
  valid_schema <- is.data.frame(targets) && identical(names(targets), target_names) &&
    is.character(targets$record_id) && is.character(targets$instrument) &&
    is.character(targets$redcap_event_name) && is.character(targets$repeat_instrument) &&
    is.integer(targets$repeat_instance) && is.character(targets$target_source)
  if (!valid_schema) .condition_signal_error("`plan$assessible_targets` has an invalid schema.", "plan")
  if (nrow(targets)) {
    valid_sources <- if (identical(plan$construction, "explicit")) "explicit" else c("observed", "extended", "observed+extended")
    event_invalid <- if (isTRUE(plan$project$longitudinal)) {
      is.na(targets$redcap_event_name) |
        targets$redcap_event_name == "" |
        trimws(targets$redcap_event_name) != targets$redcap_event_name |
        !targets$redcap_event_name %in% names(plan$project$event_labels)
    } else {
      !is.na(targets$redcap_event_name)
    }
    repeat_name_invalid <- !is.na(targets$repeat_instrument) & (
      targets$repeat_instrument == "" |
        trimws(targets$repeat_instrument) != targets$repeat_instrument |
        targets$repeat_instrument != targets$instrument
    )
    repeat_shape_invalid <-
      (is.na(targets$repeat_instance) & !is.na(targets$repeat_instrument)) |
      (!is.na(targets$repeat_instance) & targets$repeat_instance < 1L)
    bad <- is.na(targets$record_id) | targets$record_id == "" |
      trimws(targets$record_id) != targets$record_id |
      is.na(targets$instrument) | !targets$instrument %in% plan$instruments |
      event_invalid | repeat_name_invalid | repeat_shape_invalid |
      is.na(targets$target_source) | !targets$target_source %in% valid_sources
    if (any(bad) || .record_detect_duplicate_rows(
      targets, .assessible_target_list_identity_columns()
    )) {
      .condition_signal_error("`plan$assessible_targets` contains invalid or duplicate targets.", "plan")
    }
  }
  if (!is.null(snapshot)) {
    if (!identical(plan$project, snapshot$project) ||
        !identical(plan$structure_fingerprint, snapshot$structure_fingerprint)) {
      .condition_signal_error("`plan` and `rcon` do not represent the same unchanged project structure.", "plan")
    }
    if (length(setdiff(plan$instruments, snapshot$instrument_order))) {
      .condition_signal_error("`plan` contains unavailable instruments.", "plan")
    }
    target_order <- .assessible_target_compute_order(
      targets,
      snapshot,
      plan$instruments
    )
    if (!identical(target_order, seq_len(nrow(targets)))) {
      .condition_signal_error("`plan$assessible_targets` is not in deterministic target order.", "plan")
    }
    if (nrow(targets)) {
      candidate <- tibble::tibble(
        instrument = targets$instrument,
        redcap_event_name = targets$redcap_event_name,
        repeat_mode = .record_resolve_repeat_mode(
          targets$repeat_instrument,
          targets$repeat_instance
        )
      )
      allowed <- snapshot$allowable_crossings[
        , c("instrument", "redcap_event_name", "repeat_mode"),
        drop = FALSE
      ]
      allowed$.allowed <- TRUE
      matched <- merge(
        as.data.frame(candidate),
        as.data.frame(allowed),
        by = c("instrument", "redcap_event_name", "repeat_mode"),
        all.x = TRUE,
        sort = FALSE
      )
      if (anyNA(matched$.allowed)) {
        .condition_signal_error("`plan` contains a target disallowed by current project structure.", "plan")
      }
    }
  }
  plan
}
