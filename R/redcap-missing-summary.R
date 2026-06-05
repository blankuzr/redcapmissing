#' Summarize a REDCap missingness report with a formatted table
#'
#' @description
#' `redcap_missing_summary()` formats the `pointblank` validation summary from a
#' `redcap_missing_report()` result as a `flextable` and also returns an HTML
#' representation of that table.
#'
#' @param x A `redcap_missing_report` object created by
#'   `redcap_missing_report()`.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`agent_summary`}{A `flextable` summary of the `pointblank`
#'   validation set.}
#'   \item{`agent_summary_html`}{An HTML string representation of the same
#'   summary table.}
#' }
#'
#' @export
redcap_missing_summary <- function(x) {
  if (!inherits(x, "redcap_missing_report")) {
    stop("`x` must be a `redcap_missing_report` object.", call. = FALSE)
  }
  needed <- c("flextable", "glue", "htmltools")
  missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop(
      "Install the following package(s) before using `redcap_missing_summary()`: ",
      paste(missing_pkgs, collapse = ", "),
      call. = FALSE
    )
  }

  agent_summary <- x$agent$validation_set |>
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

  agent_summary_html <- htmltools::renderTags(
    flextable::htmltools_value(agent_summary)
  )$html

  list(
    agent_summary = agent_summary,
    agent_summary_html = agent_summary_html
  )
}
