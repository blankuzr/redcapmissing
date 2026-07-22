# Internal helpers shared by package reporting functions.

.redcapmissing_flex_label_values <- function(values, labels) {
  values <- .miss_chr_vec(values)
  labels <- labels %||% character()
  if (is.null(names(labels))) {
    names(labels) <- rep("", length(labels))
  }

  out <- unname(labels[values])
  use_raw <- is.na(out) | .miss_is_blank_vec(out)
  out[use_raw] <- values[use_raw]
  out[.miss_is_blank_vec(values)] <- ""
  out
}
