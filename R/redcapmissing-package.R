#' redcapmissing: Plan-and-Run Completeness Assessment for REDCap Exports
#'
#' Construct a frozen set of Assessible REDCap targets, then evaluate physical
#' rows, instrument start, and branching-aware field completeness against a
#' record export. Public API terminology consistently uses *instrument*, matching
#' raw REDCap instrument names.
#'
#' @details
#' The workflow deliberately separates target construction from assessment:
#'
#' 1. [plan_from_data()] combines allowable crossings observed in the export
#'    with an optional sparse extension; or [plan_explicit()] accepts an
#'    authoritative record-level schedule.
#' 2. [run_plan()] evaluates the frozen plan without allowing runtime rows to add
#'    targets.
#' 3. [get_summary()] and [get_missing()] inspect results, while [flexify()] and
#'    [flex_event_instruments()] format them.
#'
#' Constructors discover the record-ID field and project structure from `rcon`.
#' Plans retain canonical target identity and a deterministic SHA-256 structure
#' fingerprint, but neither source records nor a live connection. Reports retain
#' the plan, target-level results, aggregates, unresolved failures, verification
#' audit counts, diagnostics, and optional validation detail; they do not retain
#' source data, raw verification rows, tokens, or a live connection.
#'
#' Structural absence is represented by typed missing values: for example,
#' `NA_character_` for classic event context and non-repeating instrument context,
#' and `NA_integer_` for a regular target's repeat instance. Response missingness
#' is separate from physical-row presence. See [plan_from_data()] for structural
#' normalization and [run_plan()] for response and verification missingness.
#'
#' Public validation failures inherit from `redcapmissing_error` and use boundary
#' subclasses for argument, schema, project, schedule, plan, and verification
#' failures. Sparse extensions into arms without observed records use the classed
#' `redcapmissing_warning_empty_arm_extension` warning.
#'
#' A typical online workflow uses `redcapAPI::redcapConnection()` and a typed
#' export from `redcapAPI::exportRecordsTyped()`. Keep REDCap API tokens outside
#' source, logs, reports, and serialized objects.
#'
#' @seealso [plan_from_data()], [plan_explicit()], [run_plan()],
#'   [get_summary()], [get_missing()], [registry()], [flexify()],
#'   [flex_event_instruments()], [flex_html()],
#'   [redcapAPI::redcapConnection()], [redcapAPI::exportRecordsTyped()]
#' @importFrom redcapAPI isNAorBlank
#' @keywords internal
"_PACKAGE"