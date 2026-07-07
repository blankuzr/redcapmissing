#' Render a flextable as an HTML string
#'
#' @description
#' `flex_html()` renders a `flextable` object, such as one returned by
#' [flex()] or [flex_event_forms()], to the HTML string used by email and
#' report insertion workflows.
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
