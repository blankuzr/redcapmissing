#' redcapmissing: Branching-Aware Missingness Reports for REDCap Exports
#'
#' Build branching-aware missingness reports for REDCap record exports,
#' organized by contextual validation levels and validation checks.
#'
#' @details
#' `redcapmissing` is built on top of the `redcapAPI` package. In routine use,
#' callers create a `redcapAPI::redcapConnection()` object, export typed
#' records with `redcapAPI::exportRecordsTyped()`, and then pass both the
#' exported data and the connection object into [find_missing()].
#'
#' This package depends on `redcapAPI` for REDCap-aware project metadata, form
#' mapping, repeating event/instrument structure, and REDCap-style blank-value
#' semantics. The original package lineage is still important to acknowledge,
#' but current public stewardship and development resources now live under the
#' VUMC Biostatistics `vubiostat/redcapAPI` project.
#'
#' @seealso [find_missing()], [registry()], [tidy.redcapmissing()], [flex()],
#'   [flex_event_forms()], [flex_html()],
#'   [redcapAPI::redcapConnection()], [redcapAPI::exportRecordsTyped()]
#' @references
#' Nutter B, Garbett S, Obregon S, Obadia T, Lehr M, High B, Lane S,
#' Beasley W, Gray W, Kennedy N, Hsi-Nien T, Horner J, Stephens J, Beck C,
#' Johnson B, Chase P, Tobias P (2026). *redcapAPI: Accessing data from REDCap
#' projects using the API*. R package version 2.12.0.
#' <https://doi.org/10.5281/zenodo.10564837>.
#'
#' Vanderbilt University Medical Center Department of Biostatistics.
#' *redcapAPI: Analysis-ready data retrieval from REDCap with advanced
#' processing capabilities in R*. Public project page and abstract by
#' Savannah Obregon, Shawn Garbett, and Benjamin Nutter.
#' <https://www.vumc.org/biostatistics/node/565>. Current source repository:
#' <https://github.com/vubiostat/redcapAPI>.
#'
#' @importFrom redcapAPI isNAorBlank
#' @keywords internal
"_PACKAGE"
