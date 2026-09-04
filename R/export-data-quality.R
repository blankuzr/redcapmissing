#' Export Data Resolution Workflow history
#'
#' Retrieve and flatten the history provided by Vanderbilt's Data Quality API
#' external module. The result can be passed directly to [run_plan()] as
#' `verified`; reviewer and latest-status selection happen during assessment.
#'
#' @param rcon A live `redcapApiConnection`, as created by
#'   [redcapAPI::redcapConnection()]. Its project information supplies the
#'   project ID. Offline connections cannot retrieve history.
#' @param records `NULL` to retrieve all records, or a character vector of
#'   nonmissing, nonblank, unpadded record IDs. Character leading zeros are
#'   preserved. `character(0)` returns an empty table without requesting history.
#' @param prefix One nonmissing, nonblank, unpadded module prefix. The default
#'   is `"data_quality_api"`.
#'
#' @section Prerequisites and retrieval:
#' The project must use REDCap's Data Resolution Workflow. The Data Quality API
#' external module must be enabled in both the REDCap Control Center and the
#' project. Find its prefix in the module's configuration/link, or obtain it
#' from the REDCap administrator. Use a connection authorized to export the
#' project's workflow history.
#'
#' `records = NULL` can be expensive for large projects. Restrict records when
#' useful, export once, and reuse the table across assessments. [run_plan()]
#' never retrieves history itself. The connection's existing retry settings
#' apply. No status or user filter is sent: every resolution for the selected
#' issues is retained, including later entries that revoke verification.
#'
#' @return A plain `data.frame` with one row per resolution. Issue fields are
#'   repeated on each row. An issue without resolutions contributes one row
#'   with missing resolution values. All columns are character; JSON nulls and
#'   absent optional fields become `NA_character_`. Timestamps retain their
#'   source text. No reviewer, status, or assessment-context filtering occurs.
#'
#'   The following columns are always present, including in empty results:
#'
#'   - Issue fields: `status_id`, `rule_id`, `pd_rule_id`, `non_rule`,
#'     `project_id`, `record`, `event_id`, `field_name`, `repeat_instrument`,
#'     `instance`, `status`, `exclude`, `query_status`, `assigned_username`,
#'     `group_id`.
#'   - Resolution fields: `res_id`, `ts`, `response_requested`, `response`,
#'     `comment`, `current_query_status`, `upload_doc_id`,
#'     `field_comment_edited`, `username`.
#'
#'   Additional scalar module fields are retained. No deployment-specific
#'   additions, such as `event_name`, are required. Row order follows the
#'   response and does not imply chronological order. The export can contain
#'   comments and responses; store it according to project data-handling rules.
#'
#' @section Conditions:
#' Invalid arguments, failed requests, malformed responses, inconsistent
#' issue/resolution identities, and responses outside the requested project or
#' record scope raise `redcapmissing_error_export`, inheriting from
#' `redcapmissing_error`. Error messages exclude response contents. For request
#' or JSON failures, check module enablement, prefix, and connection permissions.
#'
#' @seealso [run_plan()],
#'   [Vanderbilt module documentation](https://github.com/vanderbilt-redcap/data_quality_api/blob/master/README.md)
#' @examples
#' \dontrun{
#' history <- export_data_quality(rcon, records = c("1", "2"))
#' report <- run_plan(plan, data, rcon,
#'   verified = history, verified_user = "reviewer"
#' )
#' }
#' @export
export_data_quality <- function(rcon, records = NULL, prefix = "data_quality_api") {
  if (!inherits(rcon, "redcapApiConnection") ||
      inherits(rcon, "redcapOfflineConnection")) {
    .dqr_signal_error("`rcon` must be a live redcapApiConnection.")
  }
  valid_text <- function(x) is.character(x) && is.null(dim(x)) && !anyNA(x) &&
    all(nzchar(x) & x == trimws(x))
  if (!valid_text(prefix) || length(prefix) != 1L) {
    .dqr_signal_error("`prefix` must be one nonblank, unpadded character value.")
  }
  if (!is.null(records) && !valid_text(records)) {
    .dqr_signal_error("`records` must be NULL or nonblank, unpadded character IDs.")
  }
  if (!is.null(records) && !length(records)) return(.dqr_build_empty_table())

  project_id <- tryCatch(
    as.character(rcon$projectInformation()$project_id),
    error = function(e) .dqr_signal_error("Could not retrieve the connection's project ID.")
  )
  if (length(project_id) != 1L || is.na(project_id) ||
      !grepl("^[1-9][0-9]*$", project_id)) {
    .dqr_signal_error("`rcon` must provide one positive project ID.")
  }
  if (!valid_text(rcon$url) || length(rcon$url) != 1L) {
    .dqr_signal_error("`rcon` must provide one API URL.")
  }
  url <- paste0(
    rcon$url, if (grepl("?", rcon$url, fixed = TRUE)) "&" else "?",
    "prefix=", utils::URLencode(prefix, reserved = TRUE),
    "&page=export&type=module&NOAUTH&pid=", project_id
  )
  body <- list(format = "json", returnFormat = "json")
  if (!is.null(records)) {
    body <- c(body, redcapAPI::vectorToApiBodyList(unique(records), "record"))
  }
  response <- tryCatch(
    redcapAPI::makeApiCall(rcon, body = body, url = url, log = FALSE),
    error = function(e) .dqr_signal_error(paste0(
      "Data Quality API export failed. Check the connection's permissions, ",
      "module enablement, and prefix."
    ))
  )
  .dqr_parse_history(response$content, project_id, records)
}

.dqr_signal_error <- function(message) {
  .condition_signal_error(message, subclass = "export")
}

.dqr_build_empty_table <- function() {
  columns <- c(
    "status_id", "rule_id", "pd_rule_id", "non_rule", "project_id", "record",
    "event_id", "field_name", "repeat_instrument", "instance", "status",
    "exclude", "query_status", "assigned_username", "group_id", "res_id",
    "ts", "response_requested", "response", "comment", "current_query_status",
    "upload_doc_id", "field_comment_edited", "username"
  )
  as.data.frame(stats::setNames(rep(list(character()), length(columns)), columns))
}

.dqr_validate_object <- function(x, required = character()) {
  if (!is.list(x) || (length(x) && (is.null(names(x)) ||
      anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x)))) ||
      !all(required %in% names(x))) {
    .dqr_signal_error("The export must contain uniquely named issue and resolution objects with the required fields.")
  }
  invisible(NULL)
}

.dqr_flatten_fields <- function(x) {
  lapply(x, function(value) {
    if (is.null(value)) return(NA_character_)
    if (!is.atomic(value) || length(value) != 1L ||
        (is.numeric(value) && !is.finite(value))) {
      .dqr_signal_error("Issue and resolution fields must be scalar JSON values or null.")
    }
    as.character(value)
  })
}

# Keep the module's parent/child identities intact while expanding its history.
# Missing resolution objects describe issues, not reviewer evidence.
.dqr_parse_history <- function(content, project_id, records) {
  history <- tryCatch({
    if (is.raw(content)) content <- rawToChar(content)
    if (!is.character(content) || length(content) != 1L || is.na(content) ||
        !grepl("^\\s*[\\[{]", content)) stop("invalid response")
    jsonlite::fromJSON(content, simplifyVector = FALSE)
  }, error = function(e) .dqr_signal_error(paste0(
    "The Data Quality API response is not valid history JSON. ",
    "Check module enablement and prefix."
  )))
  .dqr_validate_object(history)
  if (!length(history)) return(.dqr_build_empty_table())

  rows <- list()
  for (issue_key in names(history)) {
    issue <- history[[issue_key]]
    .dqr_validate_object(issue, c(
      "status_id", "project_id", "record", "event_id", "field_name",
      "repeat_instrument", "instance"
    ))
    resolutions <- issue[["resolutions"]]
    .dqr_validate_object(if (is.null(resolutions)) list() else resolutions)
    issue[["resolutions"]] <- NULL
    issue <- .dqr_flatten_fields(issue)
    if (is.na(issue$status_id) || !grepl("^[1-9][0-9]*$", issue_key) ||
        issue$status_id != issue_key) {
      .dqr_signal_error("An issue ID does not match its export key.")
    }
    if (is.na(issue$project_id) || issue$project_id != project_id) {
      .dqr_signal_error("The export contains an issue from a different project.")
    }
    if (is.na(issue$record) || !nzchar(issue$record) ||
        issue$record != trimws(issue$record) ||
        (!is.null(records) && !issue$record %in% records)) {
      .dqr_signal_error("The export contains a missing or out-of-scope record ID.")
    }
    if (!length(resolutions)) {
      rows[[length(rows) + 1L]] <- issue
      next
    }
    for (resolution_key in names(resolutions)) {
      resolution <- resolutions[[resolution_key]]
      .dqr_validate_object(resolution, c(
        "res_id", "status_id", "ts", "current_query_status", "username"
      ))
      resolution <- .dqr_flatten_fields(resolution)
      if (is.na(resolution$res_id) ||
          !grepl("^[1-9][0-9]*$", resolution_key) ||
          resolution$res_id != resolution_key || is.na(resolution$status_id) ||
          resolution$status_id != issue$status_id) {
        .dqr_signal_error("A resolution ID or parent issue ID is inconsistent.")
      }
      resolution$status_id <- NULL
      if (length(intersect(names(issue), names(resolution)))) {
        .dqr_signal_error("Issue and resolution fields have ambiguous overlapping names.")
      }
      rows[[length(rows) + 1L]] <- c(issue, resolution)
    }
  }
  result <- as.data.frame(dplyr::bind_rows(c(list(.dqr_build_empty_table()), rows)))
  if (anyDuplicated(result$res_id[!is.na(result$res_id)])) {
    .dqr_signal_error("The export repeats a resolution ID.")
  }
  result
}
