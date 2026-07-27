#' Format an accessor tibble as a flextable
#'
#' @description
#' `flexify()` turns a tibble returned by [get_summary()] or [get_missing()]
#' into a presentation ready `flextable`.
#'
#' @details
#' Column names and storage types must match either the `get_summary()` or
#' `get_missing()` return schema. Columns may appear in any
#' order, but added or renamed columns and combinations of summary columns and
#' missing row columns are rejected.
#'
#' Event, instrument, and repeat instrument values use the package display label
#' metadata carried by the accessors. Raw REDCap values are used when that
#' metadata is unavailable. Validation checks use the `flex_label` values from
#' [registry()], rates are displayed as percentages with one decimal place,
#' missing values are displayed as blank cells, and available `url` values are
#' formatted as
#' hyperlinks.
#'
#' Input row and column order are preserved. If both repeat context columns are
#' present and entirely blank, they are omitted together when at least one other
#' display column remains. `flexify()` formats an ungrouped copy; `x` retains its
#' original rows, columns, groups, values, and attributes.
#'
#' The optional `flextable` package is required when this function is called;
#' the error lists it when unavailable.
#'
#' @param x A tibble returned by [get_summary()] or [get_missing()], optionally
#'   filtered, grouped, reordered, or reduced to a nonempty subset of its
#'   documented columns.
#'
#' @return A `flextable` object with one display column per retained input
#'   column.
#'
#' @examples
#' \dontrun{
#' # report is caller supplied.
#' summary_table <- flexify(get_summary(report))
#' missing_table <- flexify(get_missing(report))
#' }
#'
#' @seealso [get_summary()], [get_missing()], [run_plan()], [flex_html()]
#'
#' @export
flexify <- function(x) {
  .flexify_validate_input(x)
  .flex_require_packages("flextable", "flexify()")

  labels <- attr(x, "redcapmissing_labels", exact = TRUE)
  if (!is.list(labels)) {
    labels <- list()
  }

  flex_data <- x |>
    dplyr::ungroup() |>
    tibble::as_tibble()
  flex_data <- .flexify_drop_blank_repeat_columns(flex_data)
  flex_data <- .flexify_apply_labels(flex_data, labels)

  out <- flextable::flextable(
    data = flex_data,
    col_keys = names(flex_data)
  )
  out <- flextable::set_header_labels(
    out,
    values = as.list(
      .flexify_build_header_labels()[names(flex_data)]
    )
  )
  out <- .flexify_format_cells(out, flex_data)
  out <- .flexify_format_urls(out, flex_data)

  out |>
    flextable::align(align = "left", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::padding(padding = 4, part = "all") |>
    flextable::autofit()
}

# Internal helpers ---------------------------------------------------------

.flexify_validate_input <- function(x) {
  if (!inherits(x, "tbl_df")) {
    stop(
      "`x` must be a tibble returned by `get_summary()` or `get_missing()`.",
      call. = FALSE
    )
  }
  if (ncol(x) == 0) {
    stop("`x` must contain at least one column.", call. = FALSE)
  }

  column_names <- names(x)
  if (
    length(column_names) != ncol(x) ||
      anyNA(column_names) ||
      any(trimws(column_names) == "")
  ) {
    stop("Every column in `x` must have a nonblank name.", call. = FALSE)
  }
  if (anyDuplicated(column_names)) {
    stop("Column names in `x` must be unique.", call. = FALSE)
  }

  summary_columns <- .summary_list_columns()
  missing_columns <- .missing_list_columns()
  allowed_columns <- union(summary_columns, missing_columns)
  unknown_columns <- setdiff(column_names, allowed_columns)
  if (length(unknown_columns) > 0) {
    stop(
      "`x` contains unsupported column(s): ",
      .flexify_detect_list_columns(unknown_columns),
      ". Columns must come from `get_summary()` or `get_missing()`.",
      call. = FALSE
    )
  }

  summary_compatible <- all(column_names %in% summary_columns)
  missing_compatible <- all(column_names %in% missing_columns)
  if (!summary_compatible && !missing_compatible) {
    stop(
      "`x` must use columns from one accessor schema.",
      call. = FALSE
    )
  }

  expected_types <- .flexify_list_column_types()[column_names]
  actual_types <- vapply(x, typeof, character(1))
  wrong_type <- actual_types != expected_types
  if (any(wrong_type)) {
    details <- paste0(
      "`",
      column_names[wrong_type],
      "` must be `",
      expected_types[wrong_type],
      "` (not `",
      actual_types[wrong_type],
      "`)"
    )
    stop(
      "`x` must retain the accessor column storage types: ",
      paste(details, collapse = "; "),
      ".",
      call. = FALSE
    )
  }

  invisible(x)
}

.flexify_list_column_types <- function() {
  summary_types <- vapply(
    .summary_build_prototype(),
    typeof,
    character(1)
  )
  missing_types <- vapply(
    .missing_build_prototype(),
    typeof,
    character(1)
  )

  c(
    summary_types,
    missing_types[setdiff(names(missing_types), names(summary_types))]
  )
}

.flexify_detect_list_columns <- function(x) {
  paste0("`", x, "`", collapse = ", ")
}

.flexify_drop_blank_repeat_columns <- function(x) {
  repeat_columns <- c("repeat_instrument", "repeat_instance")
  if (!all(repeat_columns %in% names(x)) || ncol(x) <= length(repeat_columns)) {
    return(x)
  }

  repeat_blank <- all(.schema_detect_blank_values(x$repeat_instrument)) &&
    all(.schema_detect_blank_values(x$repeat_instance))
  if (!repeat_blank) {
    return(x)
  }

  x[, setdiff(names(x), repeat_columns), drop = FALSE]
}

.flexify_apply_labels <- function(x, labels) {
  event_labels <- labels$events %||% character()
  instrument_labels <- labels$instruments %||% character()
  if (!is.character(event_labels)) {
    event_labels <- character()
  }
  if (!is.character(instrument_labels)) {
    instrument_labels <- character()
  }

  if ("redcap_event_name" %in% names(x)) {
    x$redcap_event_name <- .flex_apply_labels(
      x$redcap_event_name,
      event_labels
    )
  }
  if ("instrument" %in% names(x)) {
    x$instrument <- .flex_apply_labels(x$instrument, instrument_labels)
  }
  if ("repeat_instrument" %in% names(x)) {
    x$repeat_instrument <- .flex_apply_labels(
      x$repeat_instrument,
      instrument_labels
    )
  }
  if ("validation_check" %in% names(x)) {
    x$validation_check <- .registry_build_flex_labels(x$validation_check)
  }

  x
}

.flexify_build_header_labels <- function() {
  c(
    record_id = "Record ID",
    redcap_event_name = "Event",
    instrument = "Instrument",
    repeat_instrument = "Repeat Instrument",
    repeat_instance = "Repeat Instance",
    validation_context = "Validation Context",
    validation_level = "Validation Level",
    validation_check = "Validation Check",
    status = "Status",
    reason = "Reason",
    field_name = "Field Name",
    field_label = "Field Label",
    field_type = "Field Type",
    branching_logic = "Branching Logic",
    url = "URL",
    assessed = "Assessed",
    passed = "Passed",
    failed = "Failed",
    pass_rate = "Pass Rate",
    fail_rate = "Fail Rate"
  )
}

.flexify_format_cells <- function(x, data) {
  character_columns <- names(data)[
    vapply(data, typeof, character(1)) == "character"
  ]
  integer_columns <- names(data)[
    vapply(data, typeof, character(1)) == "integer"
  ]

  if (length(character_columns) > 0) {
    x <- flextable::colformat_char(x, j = character_columns, na_str = "")
  }
  if (length(integer_columns) > 0) {
    x <- flextable::colformat_int(x, j = integer_columns, na_str = "")
  }

  rate_columns <- intersect(c("pass_rate", "fail_rate"), names(data))
  if (length(rate_columns) > 0) {
    rate_formatters <- rep(
      list(.flexify_format_rate),
      length(rate_columns)
    )
    names(rate_formatters) <- rate_columns
    x <- flextable::set_formatter(x, values = rate_formatters)
  }

  x
}

.flexify_format_rate <- function(x) {
  out <- rep("", length(x))
  present <- !is.na(x)
  out[present] <- paste0(
    formatC(x[present] * 100, format = "f", digits = 1),
    "%"
  )
  out
}

.flexify_format_urls <- function(x, data) {
  if (!"url" %in% names(data) || nrow(data) == 0) {
    return(x)
  }

  has_url <- !.schema_detect_blank_values(data$url)
  if (!any(has_url)) {
    return(x)
  }

  url_values <- data$url[has_url]
  flextable::compose(
    x,
    i = which(has_url),
    j = "url",
    value = flextable::as_paragraph(
      flextable::hyperlink_text(x = url_values, url = url_values)
    )
  )
}


.flex_require_packages <- function(packages, context) {
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

.flex_apply_labels <- function(values, labels) {
  values <- .schema_normalize_character_vector(values)
  labels <- labels %||% character()
  if (is.null(names(labels))) {
    names(labels) <- rep("", length(labels))
  }

  out <- unname(labels[values])
  use_raw <- is.na(out) | .schema_detect_blank_values(out)
  out[use_raw] <- values[use_raw]
  out[.schema_detect_blank_values(values)] <- ""
  out
}
