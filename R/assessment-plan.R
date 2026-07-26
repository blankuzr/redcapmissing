#' Construct an assessment plan from observed REDCap rows
#'
#' `plan_from_data()` identifies Assessible targets by intersecting REDCap
#' instrument/event/repeat crossings allowed by `rcon` with crossings observed
#' in `data` or added through `extended_schedule`. An absent extension row never
#' removes an observed target.
#'
#' @param data A data frame exported from the REDCap project represented by
#'   `rcon`. It must contain at least one physical REDCap row. Only structural
#'   row identity is used; instrument response fields are not required and an
#'   all-blank response row is still observed. Every supplied column must use
#'   ordinary atomic vector storage; list or matrix columns are rejected. The
#'   record-ID column is discovered from `rcon` rather than assumed to have a
#'   particular name.
#' @param rcon A REDCap connection-like object exposing project information,
#'   metadata, instruments, and applicable arms, events, mappings, and repeat
#'   configuration. A missing repeat-configuration surface is an error; an
#'   explicit zero-row surface represents a project with no repeats. Constructors
#'   inspect project structure but never export records from `rcon`.
#' @param instruments A nonempty character vector of unique, nonmissing,
#'   nonblank, unpadded raw REDCap instrument names. Values must exist in the
#'   project. Their order determines target ordering.
#' @param extended_schedule `NULL`, or a data frame with exactly the columns
#'   `instrument`, `redcap_event_name`, and `repeat_instance`, in that order.
#'   Each row adds its exact crossing for every record observed in the applicable
#'   arm; in a classic project it adds the crossing for every observed record.
#'   `NULL` and a correctly typed zero-row table mean observed-only planning.
#'
#' @section Assessible-target rule:
#' `plan_from_data()` implements:
#'
#' ```text
#' Assessible targets =
#'   rcon-allowable crossings
#'   INTERSECT
#'   (crossings observed in data UNION extended_schedule crossings)
#' ```
#'
#' Schedule rows are validated before intersection. An extension into an arm
#' with no observed records emits one classed warning and adds no targets.
#'
#' @section Extended-schedule schema:
#' Column names, order, and storage are strict:
#'
#' | Column | Accepted storage and values | Normalized storage |
#' |---|---|---|
#' | `instrument` | Character or factor; one selected raw instrument name per row; no missing, blank, padded, or unknown values | Character |
#' | `redcap_event_name` | Character or factor raw event names in longitudinal projects; character/factor blanks or typed missing values only in classic projects | Character; `NA_character_` in classic projects |
#' | `repeat_instance` | Positive integer, whole-number double, or canonical character digits for a repeating crossing; typed missing or blank only for a regular crossing | Integer; `NA_integer_` for regular crossings |
#'
#' Extra columns, incorrect column order, incomplete zero-row schemas, duplicate
#' normalized rows, unselected instruments, unknown events, and regular/repeat
#' combinations disallowed by `rcon` are errors. An exact repeat instance, such
#' as `2`, adds only instance 2; it does not imply earlier instances.
#'
#' @section Structural data normalization:
#' Record IDs accept character, factor, integer, or finite double storage and
#' normalize to character. Character leading zeros are preserved. Missing,
#' blank, whitespace-padded, `NaN`, and infinite identifiers are errors; values
#' are never silently trimmed.
#'
#' Longitudinal rows require a nonblank raw `redcap_event_name`. In classic
#' projects that column may be absent, or contain only character/factor blanks
#' or typed missing values, and normalizes to `NA_character_`; a nonblank value
#' is an error.
#'
#' When the project configures repeating events or instruments, `data` must
#' contain both `redcap_repeat_instrument` and `redcap_repeat_instance`. A
#' repeating-instrument row uses its raw instrument name; regular and
#' repeating-event rows use `NA_character_`. Repeat instances accept positive
#' integers, whole-number doubles, or canonical character digits and normalize
#' to integer. Leading-zero strings, zero, negative, decimal, missing instances
#' on repeating rows, noncanonical text, `NaN`, infinity, and integer overflow
#' are errors. Regular rows normalize to `NA_integer_`.
#'
#' The normalized physical-row key is record ID, event, repeat instrument, and
#' repeat instance. Every row must describe a regular, repeating-event, or
#' repeating-instrument context allowed by `rcon`, and keys must be unique.
#' Normalization collisions error; no row is silently discarded.
#'
#' @section Returned plan:
#' The returned object has class `redcapmissing_plan` and exactly these
#' components: `schema_version`, `construction`, `instruments`,
#' `assessible_targets`, `project`, and `structure_fingerprint`.
#' `construction` is `"from_data"` here and `"explicit"` for
#' [plan_explicit()].
#'
#' `assessible_targets` has exactly these columns:
#'
#' | Column | Storage | Meaning |
#' |---|---|---|
#' | `record_id` | Character | Canonical project record ID |
#' | `instrument` | Character | Selected raw REDCap instrument name |
#' | `redcap_event_name` | Character | Raw event name, or `NA_character_` in a classic project |
#' | `repeat_instrument` | Character | Raw instrument for repeating-instrument targets; otherwise `NA_character_` |
#' | `repeat_instance` | Integer | Exact positive repeat instance; otherwise `NA_integer_` |
#' | `target_source` | Character | `"observed"`, `"extended"`, `"observed+extended"`, or `"explicit"`, as allowed by the construction type |
#'
#' Targets are unique on the first five columns and deterministically ordered by
#' selected-instrument order, REDCap event order, record ID, repeat kind, and
#' instance. `project` stores canonical project ID, record-ID field name,
#' longitudinal status, and named event/instrument label maps. The SHA-256
#' `structure_fingerprint` represents canonicalized project identity, metadata,
#' instruments, arms, events, mappings, and repeat configuration, independent
#' of source-table row ordering. A plan retains neither `data` nor a live
#' connection.
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
#' @seealso [plan_explicit()], [run_plan()]
#' @export
plan_from_data <- function(data, rcon, instruments, extended_schedule = NULL) {
  if (missing(data) || is.null(data)) {
    .rcm_plan_abort("`data` is required and cannot be `NULL`.", "argument")
  }
  if (missing(rcon) || is.null(rcon)) {
    .rcm_plan_abort("`rcon` is required and cannot be `NULL`.", "argument")
  }
  if (missing(instruments) || is.null(instruments)) {
    .rcm_plan_abort("`instruments` is required and cannot be `NULL`.", "argument")
  }
  snapshot <- .rcm_project_snapshot(rcon)
  instruments <- .rcm_selected_instruments(instruments, snapshot)
  data <- .rcm_normalize_data(data, snapshot, require_nonempty = TRUE)
  observed <- .rcm_observed_targets(data, snapshot, instruments)
  extended <- if (is.null(extended_schedule)) {
    .rcm_empty_targets()
  } else {
    schedule <- .rcm_normalize_schedule(
      extended_schedule, "extended", snapshot, instruments, data
    )
    .rcm_scheduled_targets(schedule, snapshot, data, "extended")
  }
  targets <- .rcm_merge_targets(observed, extended, "from_data")
  targets <- .rcm_order_targets(targets, snapshot, instruments)
  .rcm_new_plan("from_data", instruments, targets, snapshot)
}

#' Construct an explicit REDCap assessment plan
#'
#' `plan_explicit()` assesses only the exact record, instrument, event, and
#' repeat-instance crossings declared by `explicit_schedule`, after validating
#' that each crossing is allowed by `rcon`. A schedule row may identify a record
#' absent from `data`; that absence is evaluated later by [run_plan()].
#'
#' @inheritParams plan_from_data
#' @param data A data frame exported from the REDCap project represented by
#'   `rcon`. Only structural columns are required, and a correctly structured
#'   zero-row data frame is allowed. Every supplied column must use ordinary
#'   atomic vector storage. Structural normalization follows [plan_from_data()].
#' @param explicit_schedule A data frame with exactly the columns `record_id`,
#'   `instrument`, `redcap_event_name`, and `repeat_instance`, in that order.
#'   An absent row means do not assess that crossing. A correctly typed zero-row
#'   schedule creates a valid plan with no Assessible targets. `NULL` and an
#'   omitted argument are errors.
#'
#' @section Assessible-target rule:
#' `plan_explicit()` implements:
#'
#' ```text
#' Assessible targets =
#'   rcon-allowable crossings
#'   INTERSECT
#'   explicit_schedule crossings
#' ```
#'
#' No expansion occurs: every schedule row identifies exactly one target.
#' Observed rows absent from the schedule, including all targets for a selected
#' instrument absent from the schedule, are not assessed. A record ID absent
#' from `data` is allowed and remains capable of failing a physical-row gate.
#' A record observed in one arm cannot be scheduled into a contradictory arm.
#'
#' @section Explicit-schedule schema:
#' Column names, order, and storage are strict:
#'
#' | Column | Accepted storage and values | Normalized storage |
#' |---|---|---|
#' | `record_id` | Character, factor, integer, or finite double; nonmissing, nonblank, and unpadded | Character; character leading zeros preserved |
#' | `instrument` | Character or factor; one selected raw instrument name per row; no missing, blank, padded, or unknown values | Character |
#' | `redcap_event_name` | Character or factor raw event names in longitudinal projects; character/factor blanks or typed missing values only in classic projects | Character; `NA_character_` in classic projects |
#' | `repeat_instance` | Positive integer, whole-number double, or canonical character digits for a repeating crossing; typed missing or blank only for a regular crossing | Integer; `NA_integer_` for regular crossings |
#'
#' Extra columns, incorrect column order, incomplete zero-row schemas, duplicate
#' normalized rows, and invalid or disallowed crossings are errors. The
#' structural identifier and repeat-instance rejection rules documented for
#' [plan_from_data()] apply here as well.
#'
#' @inheritSection plan_from_data Returned plan
#' @inheritSection plan_from_data Conditions
#'
#' @return A validated `redcapmissing_plan` as described in **Returned plan**,
#'   with `construction = "explicit"` and `target_source = "explicit"` for
#'   every target.
#'
#' @seealso [plan_from_data()], [run_plan()]
#' @export
plan_explicit <- function(data, rcon, instruments, explicit_schedule) {
  if (missing(data) || is.null(data)) {
    .rcm_plan_abort("`data` is required and cannot be `NULL`.", "argument")
  }
  if (missing(rcon) || is.null(rcon)) {
    .rcm_plan_abort("`rcon` is required and cannot be `NULL`.", "argument")
  }
  if (missing(instruments) || is.null(instruments)) {
    .rcm_plan_abort("`instruments` is required and cannot be `NULL`.", "argument")
  }
  if (missing(explicit_schedule) || is.null(explicit_schedule)) {
    .rcm_plan_abort("`explicit_schedule` is required and cannot be `NULL`.", "argument")
  }
  snapshot <- .rcm_project_snapshot(rcon)
  instruments <- .rcm_selected_instruments(instruments, snapshot)
  data <- .rcm_normalize_data(data, snapshot, require_nonempty = FALSE)
  schedule <- .rcm_normalize_schedule(
    explicit_schedule, "explicit", snapshot, instruments, data
  )
  targets <- .rcm_scheduled_targets(schedule, snapshot, data, "explicit")
  targets <- .rcm_order_targets(targets, snapshot, instruments)
  .rcm_new_plan("explicit", instruments, targets, snapshot)
}

#' Print a REDCap missingness assessment plan
#'
#' Prints only the construction type, number of selected instruments, and number
#' of Assessible targets. Record IDs and individual targets are not printed.
#'
#' @param x A `redcapmissing_plan` object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#'
#' @seealso [plan_from_data()], [plan_explicit()]
#' @export
print.redcapmissing_plan <- function(x, ...) {
  x <- .rcm_validate_plan(x)
  cat(
    "<redcapmissing_plan>\n",
    "  Construction: ", x$construction, "\n",
    "  Instruments:  ", length(x$instruments), "\n",
    "  Targets:      ", nrow(x$assessible_targets), "\n",
    sep = ""
  )
  invisible(x)
}
