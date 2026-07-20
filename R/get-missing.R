#' Get focused missing rows from a REDCap missingness report
#'
#' @description
#' `get_missing()` returns the user-facing missing-row columns from a
#' [find_missing()] report. By default, it returns failures from every
#' validation check. Use `validation_check` to keep one or more canonical
#' checks from [registry()].
#'
#' @details
#' Row-started and form-started failures do not describe an individual field,
#' so their field metadata columns contain `NA`. The `url` column remains a raw
#' REDCap Data Entry URL when one is available and otherwise contains `NA`.
#'
#' `get_missing()` preserves the row order and values stored in
#' `report$missing`. Valid validation checks that have no failed rows in a
#' report return a zero-row tibble with the documented columns.
#'
#' @param report A `redcapmissing` object created by [find_missing()].
#' @param validation_check `NULL`, or a non-empty character vector containing
#'   canonical validation-check codes from [registry()]. `NULL` returns all
#'   failed rows. Duplicate values are treated as a set.
#'
#' @return A tibble containing failed validation rows with these columns:
#' \describe{
#'   \item{`record_id`}{The REDCap record identifier.}
#'   \item{`validation_context`}{The overall, event, or repeat-instance
#'     context for the failed validation row.}
#'   \item{`form`}{The REDCap instrument/form name.}
#'   \item{`validation_check`}{The canonical validation-check code.}
#'   \item{`field_name`}{The REDCap field name, or `NA` when the check is
#'     not field-specific.}
#'   \item{`field_label`}{The REDCap field label, or `NA` when the check is
#'     not field-specific.}
#'   \item{`field_type`}{The REDCap field type, or `NA` when the check is
#'     not field-specific.}
#'   \item{`branching_logic`}{The field branching logic, or `NA` when the
#'     check is not field-specific.}
#'   \item{`url`}{A raw REDCap Data Entry URL for the failed record/form
#'     context when available; otherwise `NA`.}
#' }
#'
#' @examples
#' \dontrun{
#' report <- find_missing(
#'   data = records,
#'   rcon = rcon,
#'   forms = "baseline_form"
#' )
#'
#' get_missing(report)
#' get_missing(report, validation_check = "field-complete")
#' }
#'
#' @seealso [find_missing()], [registry()], [tidy.redcapmissing()]
#'
#' @export
get_missing <- function(report, validation_check = NULL) {
  .redcapmissing_check_report(report, arg = "report")
  .redcapmissing_check_missing_rows(report$missing)

  validation_check <- .redcapmissing_resolve_missing_validation_check(
    validation_check
  )
  missing_rows <- report$missing
  if (!is.null(validation_check)) {
    missing_rows <- missing_rows[
      missing_rows$validation_check %in% validation_check,
      ,
      drop = FALSE
    ]
  }

  tibble::as_tibble(missing_rows[
    ,
    .redcapmissing_get_missing_columns(),
    drop = FALSE
  ])
}

# Internal helpers ---------------------------------------------------------

.redcapmissing_get_missing_columns <- function() {
  c(
    "record_id",
    "validation_context",
    "form",
    "validation_check",
    "field_name",
    "field_label",
    "field_type",
    "branching_logic",
    "url"
  )
}

.redcapmissing_check_missing_rows <- function(missing_rows) {
  expected <- .miss_empty_missing_rows()
  expected_names <- names(expected)

  if (!identical(names(missing_rows), expected_names)) {
    stop(
      "`report$missing` must use the current missing-row column names and ",
      "order: ",
      paste(expected_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  column_types <- vapply(missing_rows, typeof, character(1))
  expected_types <- vapply(expected, typeof, character(1))
  if (!identical(column_types, expected_types)) {
    mismatched <- names(expected_types)[column_types != expected_types]
    expected_description <- paste0(
      mismatched,
      " (`",
      expected_types[mismatched],
      "`)"
    )
    stop(
      "`report$missing` must use the current missing-row column types. ",
      "Expected ",
      paste(expected_description, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(missing_rows)
}

.redcapmissing_resolve_missing_validation_check <- function(validation_check) {
  if (is.null(validation_check)) {
    return(NULL)
  }
  if (!is.character(validation_check) || length(validation_check) == 0) {
    stop(
      "`validation_check` must be `NULL` or a non-empty character vector.",
      call. = FALSE
    )
  }
  if (anyNA(validation_check) || any(trimws(validation_check) == "")) {
    stop(
      "`validation_check` may not contain `NA` or blank values.",
      call. = FALSE
    )
  }

  validation_check <- unique(validation_check)
  valid_checks <- .redcapmissing_validation_checks()
  unknown_checks <- setdiff(validation_check, valid_checks)
  if (length(unknown_checks) > 0) {
    stop(
      "Unknown `validation_check` value(s): ",
      paste0("`", unknown_checks, "`", collapse = ", "),
      ". Valid values are: ",
      paste0("`", valid_checks, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  validation_check
}
