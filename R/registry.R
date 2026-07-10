#' Inspect the redcapmissing validation registry
#'
#' @description
#' `registry()` returns the public validation taxonomy used by
#' [find_missing()]. The registry is the package source of truth for validation
#' levels, validation checks, display labels, downstream order, and the internal
#' R-safe stems used to build report components.
#'
#' @return A tibble with class `"redcapmissing_registry"` and one row per
#'   validation check. The returned columns include:
#' \describe{
#'   \item{`validation_order`}{The canonical assessment order.}
#'   \item{`downstream_order`}{The order used when applying downstream gating.}
#'   \item{`validation_level`}{The registry validation level:
#'     `"event:form / event:form:instance"`. Report outputs resolve the
#'     contextual event/form level to `"event:form"` or
#'     `"event:form:instance"` from the assessed REDCap context.}
#'   \item{`validation_check`}{The canonical validation-check code.}
#'   \item{`validation_label`}{The canonical validation label.}
#'   \item{`flex_label`}{The display label used by [flex()].}
#'   \item{`description`}{A short user-facing description of the check.}
#'   \item{`r_identifier`, `component_stem`, `step_suffix`}{Internal R-safe
#'     stems and native validation-step metadata.}
#'   \item{`gates_downstream`}{Whether a failed check removes that context from
#'     downstream assessment.}
#' }
#'
#' @examples
#' registry()
#'
#' @export
registry <- function() {
  .redcapmissing_new_registry(.redcapmissing_registry_data())
}

#' @export
print.redcapmissing_registry <- function(x, ...) {
  x <- tibble::as_tibble(x)
  required_cols <- c(
    "validation_order",
    "validation_level",
    "validation_check",
    "description"
  )
  if (!all(required_cols %in% names(x))) {
    class(x) <- setdiff(class(x), "redcapmissing_registry")
    print(x, ...)
    return(invisible(x))
  }

  n_checks <- length(unique(x$validation_check))
  n_levels <- length(unique(x$validation_level))

  stream <- stdout()
  cli::cat_line(
    .redcapmissing_registry_style("header")(
      cli::style_bold("validation registry")
    ),
    file = stream
  )
  cli::cat_line(
    .redcapmissing_registry_style("muted")(
      paste0(n_checks, " checks; ", n_levels, " levels")
    ),
    file = stream
  )
  .redcapmissing_registry_print_table(x, stream = stream)
  invisible(x)
}

# Internal helpers ---------------------------------------------------------

.redcapmissing_registry_data <- function() {
  tibble::tibble(
    validation_order = 1:4,
    downstream_order = 1:4,
    validation_level = rep("event:form / event:form:instance", 4),
    validation_check = c(
      "event-row-started",
      "instance-row-started",
      "form-started",
      "field-complete"
    ),
    validation_label = c(
      "event-row-started",
      "instance-row-started",
      "form-started",
      "field-complete"
    ),
    flex_label = c(
      "Event row started",
      "Instance row started",
      "Form started",
      "Field complete"
    ),
    description = c(
      "The expected REDCap event row exists in the export.",
      "The expected REDCap repeat instance row exists in the export.",
      "The exported form context has at least one entered data-capturing field.",
      "field complete"
    ),
    r_identifier = c(
      "event_row_started",
      "instance_row_started",
      "form_started",
      "field_complete"
    ),
    component_stem = c(
      "event_row_started",
      "instance_row_started",
      "form_started",
      "field_complete"
    ),
    step_suffix = c(
      "event-row-started",
      "instance-row-started",
      "form-started",
      "field-complete"
    ),
    gates_downstream = rep(TRUE, 4)
  )
}

.redcapmissing_new_registry <- function(x) {
  class(x) <- c("redcapmissing_registry", class(x))
  x
}

.redcapmissing_registry_row <- function(validation_check) {
  registry <- .redcapmissing_registry_data()
  out <- registry[registry$validation_check == validation_check, , drop = FALSE]
  if (nrow(out) != 1) {
    stop(
      "Unknown validation check `",
      validation_check,
      "`.",
      call. = FALSE
    )
  }
  out
}

.redcapmissing_validation_checks <- function() {
  .redcapmissing_registry_data()$validation_check
}

.redcapmissing_on_route_checks <- function() {
  registry <- .redcapmissing_registry_data()
  registry$validation_check[registry$gates_downstream]
}

.redcapmissing_validation_metadata <- function(
  validation_check,
  n,
  repeat_instance = NULL
) {
  check <- .redcapmissing_registry_row(validation_check)
  tibble::tibble(
    validation_level = .redcapmissing_context_validation_level(
      validation_check = rep(check$validation_check, n),
      repeat_instance = repeat_instance %||% rep("", n)
    ),
    validation_check = rep(check$validation_check, n),
    validation_label = rep(check$validation_label, n)
  )
}

.redcapmissing_context_validation_level <- function(
  validation_check,
  repeat_instance = NULL
) {
  validation_check <- .miss_chr_vec(validation_check)
  repeat_instance <- .miss_chr_vec(repeat_instance %||% character())
  n <- max(length(validation_check), length(repeat_instance))
  if (n == 0) {
    return(character())
  }

  validation_check <- rep(validation_check, length.out = n)
  repeat_instance <- rep(repeat_instance, length.out = n)

  level <- rep("event:form", n)
  has_repeat <- !.miss_is_blank_vec(repeat_instance)
  level[has_repeat] <- "event:form:instance"
  level
}

.redcapmissing_flex_labels <- function(validation_check) {
  registry <- .redcapmissing_registry_data()
  flex_label <- registry$flex_label[
    match(validation_check, registry$validation_check)
  ]
  flex_label[is.na(flex_label)] <- validation_check[is.na(flex_label)]
  flex_label
}

.redcapmissing_registry_print_table <- function(x, stream = stdout()) {
  x <- x[order(x$validation_order, x$validation_level), , drop = FALSE]
  widths <- c(34, 21, 31)
  names(widths) <- c("level", "check", "meaning")

  cli::cat_line(
    .redcapmissing_registry_style("border")(.redcapmissing_registry_rule(widths)),
    file = stream
  )
  cli::cat_line(
    .redcapmissing_registry_print_row(names(widths), widths, header = TRUE),
    file = stream
  )
  cli::cat_line(
    .redcapmissing_registry_style("border")(.redcapmissing_registry_rule(widths)),
    file = stream
  )

  for (row in seq_len(nrow(x))) {
    values <- c(
      x$validation_level[[row]],
      x$validation_check[[row]],
      .redcapmissing_registry_print_meaning(x$validation_check[[row]])
    )
    cli::cat_line(
      .redcapmissing_registry_print_row(values, widths, record = x[row, , drop = FALSE]),
      file = stream
    )
  }

  cli::cat_line(
    .redcapmissing_registry_style("border")(.redcapmissing_registry_rule(widths)),
    file = stream
  )
}

.redcapmissing_registry_rule <- function(widths) {
  paste0(
    "+",
    paste(vapply(widths, function(width) {
      paste(rep("-", width + 2), collapse = "")
    }, character(1)), collapse = "+"),
    "+"
  )
}

.redcapmissing_registry_print_row <- function(values, widths, header = FALSE, record = NULL) {
  cells <- mapply(
    .redcapmissing_registry_cell,
    values,
    widths,
    names(widths),
    MoreArgs = list(header = header, record = record),
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  row <- paste0(
    .redcapmissing_registry_style("border")("| "),
    paste(cells, collapse = .redcapmissing_registry_style("border")(" | ")),
    .redcapmissing_registry_style("border")(" |")
  )
  row
}

.redcapmissing_registry_cell <- function(value, width, column, header = FALSE, record = NULL) {
  value <- .redcapmissing_pad(value, width)
  if (isTRUE(header)) {
    return(cli::style_bold(cli::col_white(value)))
  }

  switch(
    column,
    "level" = .redcapmissing_registry_level_color(record$validation_level, value),
    "check" = cli::col_cyan(value),
    "meaning" = .redcapmissing_registry_style("text")(value),
    value
  )
}

.redcapmissing_registry_level_color <- function(level, value) {
  switch(
    level,
    "event:form / event:form:instance" = .redcapmissing_registry_style("level")(value),
    "event:form" = .redcapmissing_registry_style("level")(value),
    "event:form:instance" = cli::col_magenta(value),
    value
  )
}

.redcapmissing_registry_gate_color <- function(gates_downstream, value) {
  if (isTRUE(gates_downstream)) {
    return(cli::col_green(value))
  }
  cli::col_yellow(value)
}

.redcapmissing_registry_gate_label <- function(gates_downstream) {
  if (isTRUE(gates_downstream)) {
    return(paste(cli::symbol$tick, "gates"))
  }
  paste(cli::symbol$bullet, "report")
}

.redcapmissing_registry_style <- function(element) {
  switch(
    element,
    "header" = cli::make_ansi_style("#1d4ed8"),
    "border" = cli::make_ansi_style("#334155"),
    "muted" = cli::make_ansi_style("#94a3b8"),
    "text" = cli::make_ansi_style("#e5e7eb"),
    "level" = cli::make_ansi_style("#60a5fa"),
    "on_route" = cli::make_ansi_style("#22c55e"),
    identity
  )
}

.redcapmissing_registry_print_meaning <- function(validation_check) {
  switch(
    validation_check,
    "event-row-started" = "event row exists",
    "instance-row-started" = "repeat row exists",
    "form-started" = "form has data",
    "field-complete" = "field complete",
    validation_check
  )
}

.redcapmissing_pad <- function(value, width) {
  value <- as.character(value)
  value[is.na(value)] <- ""
  too_wide <- nchar(value, type = "width") > width
  value[too_wide] <- paste0(substr(value[too_wide], 1, width - 1), "~")
  padding <- pmax(width - nchar(value, type = "width"), 0)
  paste0(value, vapply(padding, function(n) {
    paste(rep(" ", n), collapse = "")
  }, character(1)))
}
