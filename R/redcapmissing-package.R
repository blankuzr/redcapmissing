#' Plan and Run Missingness Assessment for REDCap Exports
#'
#' Construct `assessible_targets` for REDCap, then evaluate physical rows,
#' instrument start, branching logic, and field completeness against a record
#' export. Exported functions use *instrument* for raw REDCap instrument names.
#'
#' @details
#' The package workflow has three steps:
#'
#' 1. Choose raw instrument names directly or retrieve the project inventory
#'    with [all_instruments()]. [plan_from_data()] combines allowable crossings
#'    observed in the export with an optional extension, which
#'    [build_extended_schedule()] can construct from project structure.
#'    [plan_explicit()] uses an exact schedule that includes a record ID for
#'    every target; [build_explicit_schedule()] can cross project-shaped cohort
#'    data with selected allowable instrument-event rows. The explicit schedule
#'    is also the complete instrument scope for that plan.
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
#' [build_extended_schedule()] uses
#' `redcapmissing_warning_undesignated_extension` when a valid requested
#' instrument in a longitudinal project is designated to no event and therefore
#' contributes no schedule row. Classic project crossings use
#' `redcap_event_name = NA_character_` natively and do not produce this warning.
#'
#' Supported `rcon` objects inherit from `redcapApiConnection`, created by
#' [redcapAPI::redcapConnection()], or `redcapOfflineConnection`, created by
#' [redcapAPI::offlineConnection()] or [redcapAPI::readPreservedProject()].
#' Online workflows commonly use [redcapAPI::exportRecordsTyped()]. Keep REDCap
#' API tokens outside source, logs, reports, and saved R objects.
#'
#' @seealso [all_instruments()], [build_explicit_schedule()],
#'   [build_extended_schedule()], [plan_from_data()], [plan_explicit()],
#'   [run_plan()], [get_summary()],
#'   [get_missing()], [registry()], [flexify()],
#'   [flex_event_instruments()], [flex_html()],
#'   [redcapAPI::redcapConnection()], [redcapAPI::offlineConnection()],
#'   [redcapAPI::exportRecordsTyped()]
#' @importFrom redcapAPI isNAorBlank
#' @keywords internal
"_PACKAGE"
