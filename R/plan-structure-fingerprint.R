# Canonical serialization for the documented structure_fingerprint.

.structure_fingerprint_prefix_value <- function(value) {
  value <- enc2utf8(value)
  paste0(nchar(value, type = "bytes"), ":", value)
}

.structure_fingerprint_encode_atomic <- function(value) {
  if (!length(value)) return(character())
  if (is.factor(value)) {
    type <- "character"
    text <- as.character(value)
  } else if (is.numeric(value) && !inherits(value, c("Date", "POSIXt"))) {
    type <- "number"
    text <- .schema_format_numeric(value)
  } else if (inherits(value, "Date")) {
    type <- "date"
    text <- as.character(value)
  } else if (inherits(value, "POSIXt")) {
    type <- "datetime"
    text <- format(
      as.POSIXct(value, tz = "UTC"),
      "%Y-%m-%dT%H:%M:%OS6Z",
      tz = "UTC"
    )
  } else {
    type <- typeof(value)
    text <- as.character(value)
  }
  type <- .structure_fingerprint_prefix_value(type)
  missing <- is.na(value)
  text <- enc2utf8(text)
  out <- paste0("v", type, .structure_fingerprint_prefix_value(text))
  out[missing] <- paste0("m", type)
  out
}

.structure_fingerprint_encode_cell <- function(value) {
  if (is.null(value)) return("null")
  if (is.list(value)) {
    parts <- vapply(value, .structure_fingerprint_encode_cell, character(1))
  } else if (is.atomic(value)) {
    parts <- .structure_fingerprint_encode_atomic(value)
  } else {
    bytes <- serialize(value, NULL, version = 2L)
    return(paste0("serialized", paste0(format(bytes), collapse = "")))
  }
  names_encoded <- if (is.null(names(value))) {
    .structure_fingerprint_encode_atomic(rep(NA_character_, length(parts)))
  } else {
    .structure_fingerprint_encode_atomic(as.character(names(value)))
  }
  payload <- as.vector(rbind(names_encoded, parts))
  paste0(
    "sequence", length(parts), ":",
    paste0(.structure_fingerprint_prefix_value(payload), collapse = "")
  )
}

.structure_fingerprint_encode_table <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data <- data[, sort(names(data)), drop = FALSE]
  for (name in names(data)) {
    value <- data[[name]]
    data[[name]] <- if (is.list(value)) {
      vapply(value, .structure_fingerprint_encode_cell, character(1))
    } else {
      .structure_fingerprint_encode_atomic(value)
    }
  }
  if (nrow(data) > 1 && ncol(data)) {
    data <- data[do.call(order, unname(data)), , drop = FALSE]
  }
  rownames(data) <- NULL
  data
}

.structure_fingerprint_compute_digest <- function(
  project,
  metadata,
  instruments,
  arms,
  events,
  mapping,
  repeat_configuration
) {
  fingerprint_input <- list(
    project = project,
    metadata = .structure_fingerprint_encode_table(metadata),
    instruments = .structure_fingerprint_encode_table(instruments),
    arms = .structure_fingerprint_encode_table(arms),
    events = .structure_fingerprint_encode_table(events),
    mapping = .structure_fingerprint_encode_table(mapping),
    repeat_configuration = .structure_fingerprint_encode_table(repeat_configuration)
  )

  digest::digest(fingerprint_input, algo = "sha256", serialize = TRUE)
}
