#' Inspect the redcapmissing validation registry
#'
#' `registry()` returns the public validation taxonomy used by [run_plan()]. It
#' defines the canonical check codes, display labels, assessment order, and
#' downstream-gating metadata used by accessors and formatters.
#'
#' @details
#' The four canonical validation-check codes, in assessment order, are:
#'
#' 1. `"event-row-started"`: a physical row exists for the record and event;
#' 2. `"repeat-instance-row-started"`: the exact repeating physical row exists;
#' 3. `"instrument-started"`: an independent data-entry detection field has a
#'    response; and
#' 4. `"field-complete"`: every applicable field selected by policy is complete.
#'
#' Report rows use validation levels `"event:instrument"` and
#' `"event:instrument:instance"`, selected from the target's repeat context.
#' Applicable failed upstream checks gate downstream assessment. See
#' [run_plan()] for classic/regular not-applicable behavior and the exact field
#' policy.
#'
#' @return A tibble with class `redcapmissing_registry` and one row per
#'   validation-check and validation-level combination. It contains integer
#'   `validation_order` and
#'   `downstream_order`; character `validation_level`, `validation_check`,
#'   `validation_label`, `flex_label`, `description`, `r_identifier`,
#'   `component_stem`, and `step_suffix`; and logical `gates_downstream`.
#'   `validation_check` is the stable public code used by [get_summary()] and
#'   [get_missing()]; R-safe stems are implementation metadata rather than
#'   alternate public vocabulary.
#'
#' @examples
#' registry()
#'
#' @seealso [run_plan()], [get_summary()], [get_missing()]
#'
#' @export
registry <- function() {
  .redcapmissing_new_registry(.redcapmissing_registry_data())
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

.redcapmissing_registry_data <- local({
  checks <- c(
    "event-row-started",
    "repeat-instance-row-started",
    "instrument-started",
    "field-complete"
  )
  data <- tibble::tibble(
    validation_order = rep(1:4, each = 2L),
    downstream_order = rep(1:4, each = 2L),
    validation_level = rep(
      c("event:instrument", "event:instrument:instance"),
      times = 4L
    ),
    validation_check = rep(checks, each = 2L),
    validation_label = rep(checks, each = 2L),
    flex_label = rep(c(
      "Event row started",
      "Repeat instance row started",
      "Instrument started",
      "Field complete"
    ), each = 2L),
    description = rep(c(
      "The expected REDCap event row exists in the export.",
      "The expected REDCap repeat instance row exists in the export.",
      "The exported instrument has at least one entered data-capturing field.",
      "field complete"
    ), each = 2L),
    r_identifier = rep(c(
      "event_row_started",
      "repeat_instance_row_started",
      "instrument_started",
      "field_complete"
    ), each = 2L),
    component_stem = rep(c(
      "event_row_started",
      "repeat_instance_row_started",
      "instrument_started",
      "field_complete"
    ), each = 2L),
    step_suffix = rep(checks, each = 2L),
    gates_downstream = rep(TRUE, 8L)
  )
  function() data
})

.redcapmissing_new_registry <- function(x) {
  class(x) <- c("redcapmissing_registry", class(x))
  x
}

.redcapmissing_registry_row <- function(validation_check, validation_level = NULL) {
  registry <- .redcapmissing_registry_data()
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

.redcapmissing_validation_checks <- function() {
  unique(.redcapmissing_registry_data()$validation_check)
}

.redcapmissing_on_route_checks <- function() {
  registry <- .redcapmissing_registry_data()
  unique(registry$validation_check[registry$gates_downstream])
}

.redcapmissing_validation_metadata <- function(
  validation_check,
  n,
  repeat_instance = NULL
) {
  registry <- .redcapmissing_registry_data()
  registry <- registry[!duplicated(registry$validation_check), , drop = FALSE]
  check_i <- match(validation_check, registry$validation_check)
  if (length(check_i) != 1L || is.na(check_i)) {
    stop(
      "Unknown validation check `",
      validation_check,
      "`.",
      call. = FALSE
    )
  }
  check <- registry$validation_check[[check_i]]
  label <- registry$validation_label[[check_i]]
  tibble::new_tibble(list(
    validation_level = .redcapmissing_context_validation_level(
      validation_check = rep(check, n),
      repeat_instance = repeat_instance %||% rep("", n)
    ),
    validation_check = rep(check, n),
    validation_label = rep(label, n)
  ), nrow = n)
}

.redcapmissing_context_validation_level <- function(
  validation_check,
  repeat_instance = NULL
) {
  repeat_instance <- .miss_chr_vec(repeat_instance %||% character())
  n <- max(length(validation_check), length(repeat_instance))
  if (n == 0) {
    return(character())
  }
  level <- rep("event:instrument", n)
  if (length(repeat_instance) > 0) {
    repeat_instance <- rep(repeat_instance, length.out = n)
    has_repeat <- !.miss_is_blank_vec(repeat_instance)
    level[has_repeat] <- "event:instrument:instance"
  }
  level
}

.redcapmissing_flex_labels <- function(validation_check) {
  registry <- .redcapmissing_registry_data()
  registry <- registry[!duplicated(registry$validation_check), , drop = FALSE]
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

    "event:instrument" = .redcapmissing_registry_style("level")(value),
    "event:instrument:instance" = cli::col_magenta(value),
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
    "repeat-instance-row-started" = "repeat instance row exists",
    "instrument-started" = "instrument has data",
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
