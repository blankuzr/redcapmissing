#' Format an accessor tibble as a flextable
#'
#' @description
#' `flexify()` turns a tibble returned by [get_summary()] or [get_missing()]
#' into a presentation-ready `flextable`. The tibble may be filtered, reordered,
#' grouped, or reduced to a non-empty subset of its documented columns before
#' formatting.
#'
#' @details
#' Column names and storage types must remain compatible with either the
#' `get_summary()` or `get_missing()` return schema. Columns may appear in any
#' order, but added or renamed columns and combinations of summary-only and
#' missing-row-only columns are rejected.
#'
#' Event, form, and repeat-instrument values use the package-owned display-label
#' metadata carried by the accessors. Raw REDCap values are used when that
#' metadata is unavailable. Validation checks use the `flex_label` values from
#' [registry()], rates are displayed as one-decimal percentages, missing values
#' are displayed as blank cells, and available `url` values are formatted as
#' hyperlinks.
#'
#' Input row and column order are preserved. If both repeat-context columns are
#' present and entirely blank, they are omitted together when at least one other
#' display column remains. `flexify()` formats an ungrouped copy and does not
#' modify `x`.
#'
#' @param x A tibble returned by [get_summary()] or [get_missing()], optionally
#'   filtered, grouped, reordered, or reduced to a non-empty subset of its
#'   documented columns.
#'
#' @return A `flextable` object with one display column per retained input
#'   column. This function requires the optional `flextable` package.
#'
#' @examplesIf requireNamespace("flextable", quietly = TRUE)
#' summary_rows <- tibble::tibble(
#'   form = "baseline_form",
#'   validation_check = "field-complete",
#'   assessed = 10L,
#'   pass_rate = 0.9
#' )
#' flexify(summary_rows)
#'
#' @seealso [get_summary()], [get_missing()], [find_missing()], [flex_html()]
#'
#' @export
flexify <- function(x) {
  .redcapmissing_check_flexify_input(x)
  .redcapmissing_check_packages("flextable", "flexify()")

  labels <- attr(x, "redcapmissing_labels", exact = TRUE)
  if (!is.list(labels)) {
    labels <- list()
  }

  flex_data <- x |>
    dplyr::ungroup() |>
    tibble::as_tibble()
  flex_data <- .redcapmissing_flexify_drop_blank_repeat_columns(flex_data)
  flex_data <- .redcapmissing_flexify_label_values(flex_data, labels)

  out <- flextable::flextable(
    data = flex_data,
    col_keys = names(flex_data)
  )
  out <- flextable::set_header_labels(
    out,
    values = as.list(
      .redcapmissing_flexify_header_labels()[names(flex_data)]
    )
  )
  out <- .redcapmissing_flexify_format_cells(out, flex_data)
  out <- .redcapmissing_flexify_format_urls(out, flex_data)

  out |>
    flextable::align(align = "left", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::padding(padding = 4, part = "all") |>
    flextable::autofit()
}

# Internal helpers ---------------------------------------------------------

.redcapmissing_check_flexify_input <- function(x) {
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
    stop("Every column in `x` must have a non-blank name.", call. = FALSE)
  }
  if (anyDuplicated(column_names)) {
    stop("Column names in `x` must be unique.", call. = FALSE)
  }

  summary_columns <- .redcapmissing_get_summary_columns()
  missing_columns <- .redcapmissing_get_missing_columns()
  allowed_columns <- union(summary_columns, missing_columns)
  unknown_columns <- setdiff(column_names, allowed_columns)
  if (length(unknown_columns) > 0) {
    stop(
      "`x` contains unsupported column(s): ",
      .redcapmissing_flexify_list_columns(unknown_columns),
      ". Columns must come from `get_summary()` or `get_missing()`.",
      call. = FALSE
    )
  }

  summary_compatible <- all(column_names %in% summary_columns)
  missing_compatible <- all(column_names %in% missing_columns)
  if (!summary_compatible && !missing_compatible) {
    stop(
      "`x` may not combine summary-only and missing-row-only columns.",
      call. = FALSE
    )
  }

  expected_types <- .redcapmissing_flexify_column_types()[column_names]
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

.redcapmissing_flexify_column_types <- function() {
  summary_types <- vapply(
    .redcapmissing_get_summary_prototype(),
    typeof,
    character(1)
  )
  missing_types <- vapply(
    .redcapmissing_get_missing_prototype(),
    typeof,
    character(1)
  )

  c(
    summary_types,
    missing_types[setdiff(names(missing_types), names(summary_types))]
  )
}

.redcapmissing_flexify_list_columns <- function(x) {
  paste0("`", x, "`", collapse = ", ")
}

.redcapmissing_flexify_drop_blank_repeat_columns <- function(x) {
  repeat_columns <- c("redcap_repeat_instrument", "redcap_repeat_instance")
  if (!all(repeat_columns %in% names(x)) || ncol(x) <= length(repeat_columns)) {
    return(x)
  }

  repeat_blank <- all(.miss_is_blank_vec(x$redcap_repeat_instrument)) &&
    all(.miss_is_blank_vec(x$redcap_repeat_instance))
  if (!repeat_blank) {
    return(x)
  }

  x[, setdiff(names(x), repeat_columns), drop = FALSE]
}

.redcapmissing_flexify_label_values <- function(x, labels) {
  event_labels <- labels$events %||% character()
  form_labels <- labels$forms %||% character()
  if (!is.character(event_labels)) {
    event_labels <- character()
  }
  if (!is.character(form_labels)) {
    form_labels <- character()
  }

  if ("redcap_event_name" %in% names(x)) {
    x$redcap_event_name <- .redcapmissing_flex_label_values(
      x$redcap_event_name,
      event_labels
    )
  }
  if ("form" %in% names(x)) {
    x$form <- .redcapmissing_flex_label_values(x$form, form_labels)
  }
  if ("redcap_repeat_instrument" %in% names(x)) {
    x$redcap_repeat_instrument <- .redcapmissing_flex_label_values(
      x$redcap_repeat_instrument,
      form_labels
    )
  }
  if ("validation_check" %in% names(x)) {
    x$validation_check <- .redcapmissing_flex_labels(x$validation_check)
  }

  x
}

.redcapmissing_flexify_header_labels <- function() {
  c(
    record_id = "Record ID",
    redcap_event_name = "Event",
    form = "Form",
    redcap_repeat_instrument = "Repeat Instrument",
    redcap_repeat_instance = "Repeat Instance",
    validation_context = "Validation Context",
    validation_level = "Validation Level",
    validation_check = "Validation Check",
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

.redcapmissing_flexify_format_cells <- function(x, data) {
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
      list(.redcapmissing_flexify_format_rate),
      length(rate_columns)
    )
    names(rate_formatters) <- rate_columns
    x <- flextable::set_formatter(x, values = rate_formatters)
  }

  x
}

.redcapmissing_flexify_format_rate <- function(x) {
  out <- rep("", length(x))
  present <- !is.na(x)
  out[present] <- paste0(
    formatC(x[present] * 100, format = "f", digits = 1),
    "%"
  )
  out
}

.redcapmissing_flexify_format_urls <- function(x, data) {
  if (!"url" %in% names(data) || nrow(data) == 0) {
    return(x)
  }

  has_url <- !.miss_is_blank_vec(data$url)
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
