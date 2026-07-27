# Shared internal infix operators only.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
