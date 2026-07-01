#' Tidy a REDCap missingness report
#'
#' @description
#' `tidy()` returns a focused validation-summary tibble from a
#' [find_missing()] report. Each row represents one pointblank validation
#' step and REDCap context from the report's `agent$validation_set`.
#'
#' @param x A `redcapmissing` object created by [find_missing()].
#' @param ... Unused.
#'
#' @return A tibble with one row per validation step/context and columns:
#' \describe{
#'   \item{`validation_id`}{The validation step identifier.}
#'   \item{`validation`}{The validation label.}
#'   \item{`validation_context`}{The REDCap context used for stratified
#'     denominators.}
#'   \item{`redcap_event_name`}{The REDCap event name for the validation
#'     context, or `""` when not applicable.}
#'   \item{`redcap_repeat_instrument`}{The REDCap repeat instrument for the
#'     validation context, or `""` when not applicable.}
#'   \item{`redcap_repeat_instance`}{The REDCap repeat instance for the
#'     validation context, or `""` when not applicable.}
#'   \item{`all_passed`}{Whether all assessed rows passed.}
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

  validation_set <- x$agent$validation_set
  .redcapmissing_check_tidy_validation_set(validation_set)

  tibble::tibble(
    validation_id = validation_set$step_id,
    validation = validation_set$label,
    validation_context = validation_set$validation_context,
    redcap_event_name = validation_set$redcap_event_name,
    redcap_repeat_instrument = validation_set$redcap_repeat_instrument,
    redcap_repeat_instance = validation_set$redcap_repeat_instance,
    all_passed = validation_set$all_passed,
    assessed = validation_set$n,
    passed = validation_set$n_passed,
    failed = validation_set$n_failed,
    pass_rate = validation_set$f_passed,
    fail_rate = validation_set$f_failed
  )
}

#' @importFrom generics tidy
#' @export
generics::tidy

# Internal helpers ---------------------------------------------------------

.redcapmissing_tidy_validation_set_columns <- function() {
  c(
    "step_id",
    "label",
    "validation_context",
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "all_passed",
    "n",
    "n_passed",
    "n_failed",
    "f_passed",
    "f_failed"
  )
}

.redcapmissing_check_tidy_validation_set <- function(validation_set) {
  required_columns <- .redcapmissing_tidy_validation_set_columns()
  missing_columns <- setdiff(required_columns, names(validation_set))

  if (length(missing_columns) > 0) {
    stop(
      "`x$agent$validation_set` must include the current validation summary ",
      "columns: ",
      paste(required_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(validation_set)
}
