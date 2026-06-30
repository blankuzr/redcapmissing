#' Summarize a REDCap missingness report
#'
#' @description
#' `summary()` returns the unmodified `pointblank` validation set stored inside
#' a `redcap_missing_report()` result. No columns are selected, renamed,
#' rounded, or otherwise changed.
#'
#' @param object A `redcapmissing` object created by
#'   [redcap_missing_report()].
#' @param ... Unused.
#'
#' @return The report's `agent$validation_set` tibble with an additional
#'   `"summary.redcapmissing"` S3 class for printing.
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

#' Format a REDCap missingness report as a flextable
#'
#' @description
#' `flex()` formats the validation summary from a `redcapmissing` report as a
#' `flextable` for reporting workflows. Unlike `summary()`, this function is
#' presentation-focused and formats pass/fail counts for display.
#'
#' @param x An object to format.
#' @param ... Unused.
#'
#' @return A `flextable` object.
#'
#' @export
flex <- function(x, ...) {
  UseMethod("flex")
}

#' @export
flex.redcapmissing <- function(x, ...) {
  .redcapmissing_check_report(x)
  .redcapmissing_check_packages(c("flextable", "glue"), "flex()")

  x$agent$validation_set |>
    dplyr::mutate(
      n_passed = glue::glue("{n_passed} ({round(f_passed * 100, 1)}%)"),
      n_failed = glue::glue("{n_failed} ({round(f_failed * 100, 1)}%)")
    ) |>
    dplyr::select(dplyr::all_of(c("label", "n", "n_passed", "n_failed"))) |>
    stats::setNames(c("Evaluation", "Assessed", "Passed", "Failed")) |>
    flextable::flextable() |>
    flextable::align(align = "left", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::padding(padding = 4, part = "all") |>
    flextable::autofit()
}

#' Render a flextable as an HTML string
#'
#' @description
#' `flex_html()` renders a `flextable` object, such as one returned by
#' [flex()], to the HTML string used by email and report insertion workflows.
#'
#' @param x A `flextable` object.
#'
#' @return A character scalar containing rendered HTML.
#'
#' @export
flex_html <- function(x) {
  if (!inherits(x, "flextable")) {
    stop("`x` must be a `flextable` object created by `flex()`.", call. = FALSE)
  }
  .redcapmissing_check_packages(c("flextable", "htmltools"), "flex_html()")

  rendered_tags <- flextable::htmltools_value(x) |>
    htmltools::renderTags()
  rendered_tags[["html"]]
}

# Internal helpers ---------------------------------------------------------

.redcapmissing_check_report <- function(x, arg = "x") {
  if (!inherits(x, "redcapmissing")) {
    stop(
      "`",
      arg,
      "` must be a `redcapmissing` object created by `redcap_missing_report()`.",
      call. = FALSE
    )
  }
  if (is.null(x$agent) || is.null(x$agent$validation_set)) {
    stop(
      "`",
      arg,
      "` must contain `agent$validation_set`.",
      call. = FALSE
    )
  }
  invisible(x)
}

.redcapmissing_check_packages <- function(packages, context) {
  missing_packages <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages) > 0) {
    stop(
      "Install the following package(s) before using `",
      context,
      "`: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(packages)
}
