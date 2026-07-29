#' Render a flextable as an HTML string
#'
#' @description
#' `flex_html()` renders a `flextable` object, such as one returned by
#' [flexify()] or [flex_event_instruments()], to the HTML string used by email and
#' report insertion workflows. The optional packages `flextable` and
#' `htmltools` are required when this function is called; the error lists
#' each missing package.
#'
#' @param x A `flextable` object.
#'
#' @return A character scalar containing rendered HTML.
#'
#' @examples
#' \dontrun{
#' # report is caller supplied.
#' summary_html <- flex_html(flexify(get_summary(report)))
#' missing_html <- flex_html(flexify(get_missing(report)))
#' event_instrument_html <- flex_html(flex_event_instruments(report))
#' }
#'
#' @export
flex_html <- function(x) {
  if (!inherits(x, "flextable")) {
    stop(
      "`x` must be a `flextable` object, such as one created by `flexify()` or ",
      "`flex_event_instruments()`.",
      call. = FALSE
    )
  }
  .flex_require_packages(c("flextable", "htmltools"), "flex_html()")

  rendered_tags <- flextable::htmltools_value(x) |>
    htmltools::renderTags()
  rendered_tags[["html"]]
}
