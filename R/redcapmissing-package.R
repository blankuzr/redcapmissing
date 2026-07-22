#' redcapmissing: Branching-Aware Missingness Reports for REDCap Exports
#'
#' Build missingness reports for REDCap record exports while respecting
#' branching logic, form-event mappings, and repeat contexts. Start with
#' [find_missing()], inspect results with [get_summary()] or [get_missing()],
#' and format them with [flexify()] or [flex_event_forms()].
#'
#' @details
#' A typical workflow creates a `redcapAPI::redcapConnection()`, exports typed
#' records with `redcapAPI::exportRecordsTyped()`, and passes both objects to
#' [find_missing()]. The package uses `redcapAPI` project metadata and REDCap
#' blank-value semantics to determine which contexts and fields are expected.
#'
#' @seealso [find_missing()], [get_summary()], [get_missing()], [flexify()],
#'   [registry()], [flex_event_forms()], [flex_html()],
#'   [redcapAPI::redcapConnection()], [redcapAPI::exportRecordsTyped()]
#' @importFrom redcapAPI isNAorBlank
#' @keywords internal
"_PACKAGE"
