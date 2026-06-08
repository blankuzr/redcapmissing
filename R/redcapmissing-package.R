#' redcapmissing: Branching-Aware Missingness Reports for REDCap Exports
#'
#' Build branching-aware missingness reports for REDCap record exports,
#' including form-level, event-level, repeat-instance, and field-level
#' validation surfaces.
#'
#' @details
#' `redcapmissing` is built on top of the `redcapAPI` package. In routine use,
#' callers create a `redcapAPI::redcapConnection()` object, export typed
#' records with `redcapAPI::exportRecordsTyped()`, and then pass both the
#' exported data and the connection object into [redcap_missing_report()].
#'
#' This package depends on `redcapAPI` for REDCap-aware project metadata, form
#' mapping, repeating-instrument structure, and REDCap-style blank-value
#' semantics.
#'
#' @seealso [redcap_missing_report()], [redcap_missing_summary()],
#'   [redcapAPI::redcapConnection()], [redcapAPI::exportRecordsTyped()]
#' @references
#' Nutter B, Garbett S, Obregon S, Obadia T, Lehr M, High B, Lane S,
#' Beasley W, Gray W, Kennedy N, Hsi-Nien T, Horner J, Stephens J, Beck C,
#' Johnson B, Chase P, Tobias P (2026). *redcapAPI: Accessing data from REDCap
#' projects using the API*. R package version 2.12.0.
#' <https://doi.org/10.5281/zenodo.10564837>.
#'
#' @importFrom redcapAPI isNAorBlank
#' @keywords internal
"_PACKAGE"
