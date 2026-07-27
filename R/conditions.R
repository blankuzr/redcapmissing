# Classed conditions shared across planning and assessment.

.condition_signal_error <- function(message, subclass = "argument") {
  condition <- structure(
    list(message = as.character(message), call = NULL),
    class = c(
      paste0("redcapmissing_error_", subclass),
      "redcapmissing_error", "error", "condition"
    )
  )
  stop(condition)
}
