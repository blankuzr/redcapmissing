# Shared storage and schema normalization contracts.

.schema_require_columns <- function(data, columns, source) {
  absent <- setdiff(columns, names(data))
  if (length(absent)) {
    .condition_signal_error(
      paste0("`", source, "` is missing required column(s): ", paste(absent, collapse = ", "), "."),
      "schema"
    )
  }
  invisible(data)
}

.schema_normalize_character <- function(x, source) {
  if (is.character(x)) return(x)
  if (is.factor(x)) return(as.character(x))
  .condition_signal_error(paste0("`", source, "` must use character or factor storage."), "schema")
}

.schema_format_numeric <- function(x) {
  if (!length(x)) return(character())
  out <- character(length(x))
  missing <- is.na(x)
  finite <- is.finite(x)
  whole <- finite & x == trunc(x)
  if (any(whole)) {
    out[whole] <- sprintf("%.0f", x[whole])
  }
  fractional <- finite & !whole
  if (any(fractional)) {
    out[fractional] <- trimws(formatC(
      x[fractional],
      digits = 17L,
      format = "fg",
      drop0trailing = TRUE,
      decimal.mark = "."
    ))
  }
  out[is.infinite(x) & x > 0] <- "Inf"
  out[is.infinite(x) & x < 0] <- "-Inf"
  out[missing & !is.nan(x)] <- "NA"
  out[is.nan(x)] <- "NaN"
  out[out == "-0"] <- "0"
  out
}

.schema_normalize_required_id <- function(x, source) {
  valid <- is.character(x) || is.factor(x) || (is.numeric(x) && !is.logical(x))
  if (!valid || inherits(x, c("Date", "POSIXt"))) {
    .condition_signal_error(
      paste0("`", source, "` must use character, factor, integer, or numeric storage."),
      "schema"
    )
  }
  if (is.numeric(x) && any(is.na(x) | is.nan(x) | !is.finite(x))) {
    .condition_signal_error(paste0("`", source, "` requires present, finite values."), "schema")
  }
  value <- if (is.factor(x)) {
    as.character(x)
  } else if (is.numeric(x)) {
    .schema_format_numeric(x)
  } else {
    x
  }
  invalid <- is.na(value) | value == "" | grepl("^\\s+$", value) | trimws(value) != value
  if (any(invalid)) {
    .condition_signal_error(
      paste0("`", source, "` requires present, nonblank, unpadded identifiers."),
      "schema"
    )
  }
  value
}

.schema_normalize_nullable_character <- function(x, source) {
  if (!is.character(x) && !is.factor(x)) {
    all_typed_missing <- is.atomic(x) &&
      !inherits(x, c("Date", "POSIXt")) &&
      (length(x) > 0 && all(is.na(x)) && !(is.numeric(x) && any(is.nan(x))))
    if (all_typed_missing) return(rep(NA_character_, length(x)))
    .condition_signal_error(
      paste0("`", source, "` must use character/factor storage or contain only typed NA values."),
      "schema"
    )
  }
  value <- if (is.factor(x)) as.character(x) else x
  blank <- is.na(value) | grepl("^\\s*$", value)
  if (any(!blank & trimws(value) != value)) {
    .condition_signal_error(paste0("`", source, "` cannot contain surrounding whitespace."), "schema")
  }
  value[blank] <- NA_character_
  value
}

.schema_normalize_repeat_instance <- function(x, source) {
  if (is.logical(x) && length(x) > 0 && all(is.na(x))) {
    return(rep(NA_integer_, length(x)))
  }
  if (is.factor(x)) {
    value <- as.character(x)
    if (length(value) > 0 && all(is.na(value))) {
      return(rep(NA_integer_, length(value)))
    }
    .condition_signal_error(
      paste0("`", source, "` may use factor storage only when every value is missing."),
      "schema"
    )
  }
  valid <- is.character(x) || (is.numeric(x) && !is.logical(x))
  if (!valid || inherits(x, c("Date", "POSIXt"))) {
    .condition_signal_error(
      paste0("`", source, "` must use character, factor, integer, or numeric storage."),
      "schema"
    )
  }
  if (!length(x)) return(integer())
  if (is.numeric(x)) {
    if (any(is.nan(x))) {
      .condition_signal_error(paste0("`", source, "` cannot contain NaN."), "schema")
    }
    missing <- is.na(x)
    bad <- !missing & (!is.finite(x) | x < 1 | x != floor(x) | x > .Machine$integer.max)
    if (any(bad)) {
      .condition_signal_error(paste0("`", source, "` requires positive whole number IDs within integer range."), "schema")
    }
    out <- rep(NA_integer_, length(x))
    out[!missing] <- as.integer(x[!missing])
    return(out)
  }
  value <- if (is.factor(x)) as.character(x) else x
  blank <- is.na(value) | grepl("^\\s*$", value)
  digit_string <- blank | grepl("^[1-9][0-9]*$", value)
  number <- suppressWarnings(as.numeric(value))
  bad <- (!blank & trimws(value) != value) | !digit_string | (!blank & (!is.finite(number) | number > .Machine$integer.max))
  if (any(bad)) {
    .condition_signal_error(paste0("`", source, "` requires positive integer digit strings or missing values."), "schema")
  }
  out <- rep(NA_integer_, length(value))
  out[!blank] <- as.integer(number[!blank])
  out
}

.schema_resolve_column <- function(data, choices, source, required = TRUE) {
  found <- choices[choices %in% names(data)]
  if (length(found)) return(found[[1]])
  if (isTRUE(required)) {
    .condition_signal_error(
      paste0("`", source, "` must provide one of: ", paste0("`", choices, "`", collapse = ", "), "."),
      "project"
    )
  }
  NULL
}

.schema_extract_column_vector <- function(records, column) {
  if (is.null(column) || !column %in% names(records)) {
    return(rep(NA_character_, nrow(records)))
  }
  records[[column]]
}

.schema_normalize_character_vector <- function(x) {
  if (length(x) == 0) {
    return(character())
  }
  if (is.character(x)) {
    return(x)
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return(as.character(x))
  }
  if (is.list(x)) {
    return(vapply(x, .schema_normalize_character_scalar, character(1)))
  }
  out <- as.character(x)
  if (anyNA(x)) {
    out[is.na(x)] <- NA_character_
  }
  out
}

.schema_detect_blank_values <- function(x) {
  if (length(x) == 0) {
    return(logical())
  }
  isNAorBlank(.schema_normalize_character_vector(x))
}

.schema_detect_blank_value <- function(x) {
  if (length(x) == 0) {
    return(TRUE)
  }
  .schema_detect_blank_values(x)[[1]]
}

.schema_require_values <- function(x) {
  required <- tolower(trimws(.schema_normalize_character_vector(x)))
  !.schema_detect_blank_values(required) & required %in% c("y", "yes", "true", "1")
}

.schema_normalize_character_scalar <- function(x) {
  if (length(x) == 0 || is.null(x) || is.na(x[[1]])) {
    return(NA_character_)
  }
  as.character(x[[1]])
}
