#' Tidy a REDCap missingness report
#'
#' @description
#' `tidy()` returns a focused validation-summary tibble from a
#' [find_missing()] report. Each row represents one native validation
#' step and REDCap context from the report's `summary` table.
#'
#' @param x A `redcapmissing` object created by [find_missing()].
#' @param ... Unused.
#'
#' @return A tibble with one row per validation step/context and columns:
#' \describe{
#'   \item{`redcap_event_name`}{The REDCap event name for the validation
#'     context, or `""` when not applicable.}
#'   \item{`form`}{The REDCap instrument/form name assessed by the report.}
#'   \item{`redcap_repeat_instrument`}{The REDCap repeat instrument for the
#'     validation context, or `""` when not applicable. Omitted when the
#'     report contains no repeat context.}
#'   \item{`redcap_repeat_instance`}{The REDCap repeat instance for the
#'     validation context, or `""` when not applicable. Omitted when the
#'     report contains no repeat context.}
#'   \item{`validation_level`}{The emitted context label. Event/form checks use
#'     `"event:form"` for non-repeating contexts and
#'     `"event:form:instance"` for repeat-instance contexts.}
#'   \item{`validation_check`}{The canonical validation-check code.}
#'   \item{`assessed`}{The number of rows assessed.}
#'   \item{`passed`}{The number of rows that passed.}
#'   \item{`failed`}{The number of rows that failed.}
#'   \item{`pass_rate`}{The numeric pass fraction.}
#'   \item{`fail_rate`}{The numeric failure fraction.}
#' }
#'
#' @seealso [find_missing()], [flex()], [flex_html()]
#'
#' @export
tidy.redcapmissing <- function(x, ...) {
  .redcapmissing_check_report(x)

  validation_set <- x$summary
  .redcapmissing_check_tidy_validation_set(validation_set)

  out <- tibble::tibble(
    redcap_event_name = validation_set$redcap_event_name,
    form = validation_set$form,
    redcap_repeat_instrument = validation_set$redcap_repeat_instrument,
    redcap_repeat_instance = validation_set$redcap_repeat_instance,
    validation_level = validation_set$validation_level,
    validation_check = validation_set$validation_check,
    assessed = validation_set$assessed,
    passed = validation_set$passed,
    failed = validation_set$failed,
    pass_rate = validation_set$pass_rate,
    fail_rate = validation_set$fail_rate
  )

  .redcapmissing_tidy_drop_repeat_columns(out)
}

#' @importFrom generics tidy
#' @export
generics::tidy

# Internal helpers ---------------------------------------------------------

.redcapmissing_tidy_validation_set_columns <- function() {
  c(
    "redcap_event_name",
    "form",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "validation_level",
    "validation_check",
    "assessed",
    "passed",
    "failed",
    "pass_rate",
    "fail_rate"
  )
}

.redcapmissing_check_tidy_validation_set <- function(validation_set) {
  required_columns <- .redcapmissing_tidy_validation_set_columns()
  missing_columns <- setdiff(required_columns, names(validation_set))

  if (length(missing_columns) > 0) {
    stop(
      "`x$summary` must include the current validation summary ",
      "columns: ",
      paste(required_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(validation_set)
}

.redcapmissing_tidy_drop_repeat_columns <- function(x) {
  repeat_columns <- c("redcap_repeat_instrument", "redcap_repeat_instance")
  if (!all(repeat_columns %in% names(x))) {
    return(x)
  }

  has_repeat <- any(
    !.miss_is_blank_vec(x$redcap_repeat_instrument) |
      !.miss_is_blank_vec(x$redcap_repeat_instance)
  )
  if (isTRUE(has_repeat)) {
    return(x)
  }

  x[, setdiff(names(x), repeat_columns), drop = FALSE]
}
