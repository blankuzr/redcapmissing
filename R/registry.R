#' Inspect validation checks and report metadata
#'
#' `registry()` returns the four validation checks used by [run_plan()] and the
#' report metadata associated with each check. The returned columns identify
#' assessment order, validation level, validation check code, [flexify()] label,
#' and concise pass condition.
#'
#' @details
#' The four `validation_check` values, in `validation_order`, are:
#'
#' 1. `"event-row-started"`;
#' 2. `"repeat-instance-row-started"`;
#' 3. `"instrument-started"`; and
#' 4. `"field-complete"`.
#'
#' Report rows use `validation_level = "event:instrument"` when a target has no
#' repeat instance and `"event:instrument:instance"` when it has a repeat
#' instance. See [run_plan()] for when each check is assessed, marked
#' `"not applicable"`, or marked `"not reached"`, and for field selection,
#' branching logic, missing responses, and verification overrides.
#'
#' @return A tibble with class `redcapmissing_registry`, eight rows, and exactly
#'   these columns:
#'
#' | Column | Storage | What it refers to |
#' |---|---|---|
#' | `validation_order` | Integer | The order in which [run_plan()] evaluates the check |
#' | `validation_level` | Character | The value stored in report rows: `"event:instrument"` or `"event:instrument:instance"` |
#' | `validation_check` | Character | The check code stored in report rows and accepted by the [get_summary()] and [get_missing()] filters |
#' | `flex_label` | Character | The label used by [flexify()] |
#' | `description` | Character | For an assessed check, the condition that makes the check pass; this is also the condition shown by `print(registry())` |
#'
#' Each validation check has one row for each validation level.
#'
#' @examples
#' registry()
#'
#' @seealso [run_plan()], [get_summary()], [get_missing()], [flexify()]
#'
#' @export
registry <- function() {
  .registry_build_object(.registry_get_table())
}

#' Print the validation registry
#'
#' @param x A `redcapmissing_registry` returned by [registry()].
#' @param ... Additional arguments passed to the fallback tibble print method
#'   when `x` is malformed.
#'
#' @return `x`, invisibly.
#'
#' @rdname registry
#' @export
print.redcapmissing_registry <- function(x, ...) {
  display <- tibble::as_tibble(x)
  required_cols <- c(
    "validation_order",
    "validation_level",
    "validation_check",
    "description"
  )
  if (!all(required_cols %in% names(display))) {
    class(display) <- setdiff(class(display), "redcapmissing_registry")
    print(display, ...)
    return(invisible(x))
  }

  n_checks <- length(unique(display$validation_check))
  n_levels <- length(unique(display$validation_level))

  stream <- stdout()
  cli::cat_line(
    .registry_resolve_style("header")(
      cli::style_bold("validation registry")
    ),
    file = stream
  )
  cli::cat_line(
    .registry_resolve_style("muted")(
      paste0(n_checks, " checks; ", n_levels, " levels")
    ),
    file = stream
  )
  .registry_print_table(display, stream = stream)
  invisible(x)
}

# Internal helpers ---------------------------------------------------------

.registry_get_table <- local({
  checks <- c(
    "event-row-started",
    "repeat-instance-row-started",
    "instrument-started",
    "field-complete"
  )
  data <- tibble::tibble(
    validation_order = rep(1:4, each = 2L),
    validation_level = rep(
      c("event:instrument", "event:instrument:instance"),
      times = 4L
    ),
    validation_check = rep(checks, each = 2L),
    flex_label = rep(c(
      "Event row started",
      "Repeat instance row started",
      "Instrument started",
      "Field complete"
    ), each = 2L),
    description = rep(c(
      paste("At least one physical row exists in the export for the target",
            "record and event."),
      paste("The exact physical row exists in the export for the target record,",
            "event, repeat instrument, and repeat instance."),
      paste("The exact target row exists, and at least one ordinary detection",
            "field has a present response or one checkbox detection field has",
            "a selected child."),
      paste("Every field selected by field policy and branching logic is",
            "complete: each ordinary field has a present response or eligible",
            "verification override, and each checkbox has a selected child or",
            "eligible verification override.")
    ), each = 2L)
  )
  function() data
})

.registry_build_object <- function(x) {
  class(x) <- c("redcapmissing_registry", class(x))
  x
}

.registry_build_row <- function(validation_check, validation_level = NULL) {
  registry <- .registry_get_table()
  out <- registry[registry$validation_check == validation_check, , drop = FALSE]
  if (!is.null(validation_level)) {
    out <- out[out$validation_level == validation_level, , drop = FALSE]
  } else if (nrow(out)) {
    out <- out[1L, , drop = FALSE]
  }
  if (nrow(out) != 1L) {
    stop(
      "Unknown validation check/level combination `",
      validation_check,
      "`.",
      call. = FALSE
    )
  }
  out
}

.registry_list_validation_checks <- function() {
  unique(.registry_get_table()$validation_check)
}


.registry_resolve_validation_level <- function(
  validation_check,
  repeat_instance = NULL
) {
  repeat_instance <- .schema_normalize_character_vector(repeat_instance %||% character())
  n <- max(length(validation_check), length(repeat_instance))
  if (n == 0) {
    return(character())
  }
  level <- rep("event:instrument", n)
  if (length(repeat_instance) > 0) {
    repeat_instance <- rep(repeat_instance, length.out = n)
    has_repeat <- !.schema_detect_blank_values(repeat_instance)
    level[has_repeat] <- "event:instrument:instance"
  }
  level
}

.registry_build_flex_labels <- function(validation_check) {
  registry <- .registry_get_table()
  registry <- registry[!duplicated(registry$validation_check), , drop = FALSE]
  flex_label <- registry$flex_label[
    match(validation_check, registry$validation_check)
  ]
  flex_label[is.na(flex_label)] <- validation_check[is.na(flex_label)]
  flex_label
}

.registry_print_table <- function(x, stream = stdout()) {
  x <- x[order(x$validation_order, x$validation_level), , drop = FALSE]
  display_width <- function(header, values) {
    max(nchar(c(header, as.character(values)), type = "width"), na.rm = TRUE)
  }
  fixed_widths <- c(
    display_width("level", x$validation_level),
    display_width("check", x$validation_check)
  )
  condition_width <- max(
    30L,
    min(72L, getOption("width", 80L) - sum(fixed_widths) - 10L)
  )
  widths <- c(fixed_widths, condition_width)
  names(widths) <- c("level", "check", "condition")

  cli::cat_line(
    .registry_resolve_style("border")(.registry_build_rule(widths)),
    file = stream
  )
  cli::cat_line(
    .registry_print_row(names(widths), widths, header = TRUE),
    file = stream
  )
  cli::cat_line(
    .registry_resolve_style("border")(.registry_build_rule(widths)),
    file = stream
  )

  for (row in seq_len(nrow(x))) {
    condition_lines <- strwrap(
      as.character(x$description[[row]]),
      width = widths[["condition"]]
    )
    if (!length(condition_lines)) {
      condition_lines <- ""
    }
    for (line in seq_along(condition_lines)) {
      values <- c(
        if (line == 1L) x$validation_level[[row]] else "",
        if (line == 1L) x$validation_check[[row]] else "",
        condition_lines[[line]]
      )
      cli::cat_line(
        .registry_print_row(
          values,
          widths,
          record = x[row, , drop = FALSE]
        ),
        file = stream
      )
    }
  }

  cli::cat_line(
    .registry_resolve_style("border")(.registry_build_rule(widths)),
    file = stream
  )
}

.registry_build_rule <- function(widths) {
  paste0(
    "+",
    paste(vapply(widths, function(width) {
      paste(rep("-", width + 2), collapse = "")
    }, character(1)), collapse = "+"),
    "+"
  )
}

.registry_print_row <- function(values, widths, header = FALSE, record = NULL) {
  cells <- mapply(
    .registry_format_cell,
    values,
    widths,
    names(widths),
    MoreArgs = list(header = header, record = record),
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  row <- paste0(
    .registry_resolve_style("border")("| "),
    paste(cells, collapse = .registry_resolve_style("border")(" | ")),
    .registry_resolve_style("border")(" |")
  )
  row
}

.registry_format_cell <- function(value, width, column, header = FALSE, record = NULL) {
  value <- .registry_pad_value(value, width)
  if (isTRUE(header)) {
    return(cli::style_bold(cli::col_white(value)))
  }

  switch(
    column,
    "level" = .registry_resolve_level_color(record$validation_level, value),
    "check" = cli::col_cyan(value),
    "condition" = .registry_resolve_style("text")(value),
    value
  )
}

.registry_resolve_level_color <- function(level, value) {
  switch(
    level,

    "event:instrument" = .registry_resolve_style("level")(value),
    "event:instrument:instance" = cli::col_magenta(value),
    value
  )
}


.registry_resolve_style <- function(element) {
  switch(
    element,
    "header" = cli::make_ansi_style("#1d4ed8"),
    "border" = cli::make_ansi_style("#334155"),
    "muted" = cli::make_ansi_style("#94a3b8"),
    "text" = cli::make_ansi_style("#e5e7eb"),
    "level" = cli::make_ansi_style("#60a5fa"),
    identity
  )
}


.registry_pad_value <- function(value, width) {
  value <- as.character(value)
  value[is.na(value)] <- ""
  too_wide <- nchar(value, type = "width") > width
  value[too_wide] <- paste0(substr(value[too_wide], 1, width - 1), "~")
  padding <- pmax(width - nchar(value, type = "width"), 0)
  paste0(value, vapply(padding, function(n) {
    paste(rep(" ", n), collapse = "")
  }, character(1)))
}
