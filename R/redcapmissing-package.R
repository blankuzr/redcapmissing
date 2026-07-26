#' Plan and Run Missingness Assessment for REDCap Exports
#'
#' Construct a set of Assessible REDCap targets, then evaluate physical rows,
#' instrument start, branching logic, and field completeness against a record
#' export. Exported functions use *instrument* for raw REDCap instrument names.
#'
#' @details
#' The package workflow has three steps:
#'
#' 1. [plan_from_data()] combines allowable crossings observed in the export
#'    with an optional extension. [plan_explicit()] uses an exact schedule that
#'    includes a record ID for every target.
#' 2. [run_plan()] evaluates the targets stored in the plan.
#' 3. [get_summary()] and [get_missing()] inspect results, while [flexify()] and
#'    [flex_event_instruments()] format them.
#'
#' Constructors discover the record ID field and project structure from `rcon`.
#' A plan contains `schema_version`, `construction`, `instruments`,
#' `assessible_targets`, `project`, and `structure_fingerprint`. Source records
#' and the live connection remain outside the plan.
#'
#' A report contains the plan, target results, summaries, unresolved failures,
#' verification counts, diagnostics, and optional field details. With
#' `details = TRUE`, `details$value_summary` stores assessed ordinary field
#' values as character. For checkbox fields, it stores selected exported
#' checkbox child column names. Reports contain record IDs and may
#' contain REDCap data entry URLs. Store each report under the same access,
#' retention, and sharing rules as its source REDCap export.
#'
#' Structural dimensions use typed missing values when they do not apply. In
#' `plan$assessible_targets`, `redcap_event_name` is `NA_character_` for classic
#' projects. `repeat_instrument` contains the raw instrument name for a
#' repeating instrument target; it is `NA_character_` for a repeating event
#' target or a target where neither the event nor instrument repeats.
#' `repeat_instance` contains a positive integer for repeating event and
#' repeating instrument targets; it is `NA_integer_` when neither repeats. See
#' [plan_from_data()] for structural normalization and [run_plan()] for field
#' and verification missing values.
#'
#' Validation failures inherit from `redcapmissing_error` and use subclasses for
#' argument, schema, project, schedule, plan, and verification failures.
#' Extensions into arms with zero observed records use the
#' `redcapmissing_warning_empty_arm_extension` warning.
#'
#' An online workflow uses `redcapAPI::redcapConnection()` and
#' `redcapAPI::exportRecordsTyped()`. Keep REDCap API tokens outside source,
#' logs, reports, and saved R objects.
#'
#' @seealso [plan_from_data()], [plan_explicit()], [run_plan()],
#'   [get_summary()], [get_missing()], [registry()], [flexify()],
#'   [flex_event_instruments()], [flex_html()],
#'   [redcapAPI::redcapConnection()], [redcapAPI::exportRecordsTyped()]
#' @importFrom redcapAPI isNAorBlank
#' @keywords internal
"_PACKAGE"