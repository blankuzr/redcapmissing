#' Summarize a REDCap missingness report
#'
#' @description
#' `summary()` returns the validation set stored inside a `find_missing()`
#' result. No columns are selected, renamed, rounded, or otherwise changed.
#' The table includes the `validation_context` and REDCap event/repeat context
#' columns added by `find_missing()`.
#'
#' @param object A `redcapmissing` object created by
#'   [find_missing()].
#' @param ... Unused.
#'
#' @return The report's context-stratified `agent$validation_set` tibble with
#'   an additional `"summary.redcapmissing"` S3 class for printing.
#'
#' @export
summary.redcapmissing <- function(object, ...) {
  .redcapmissing_check_report(object, arg = "object")

  validation_set <- object$agent$validation_set
  class(validation_set) <- c(
    "summary.redcapmissing",
    setdiff(class(validation_set), "summary.redcapmissing")
  )
  validation_set
}

#' @export
print.summary.redcapmissing <- function(x, ...) {
  printed_tbl <- x
  class(printed_tbl) <- setdiff(class(printed_tbl), "summary.redcapmissing")
  print(printed_tbl, ...)
  invisible(x)
}
