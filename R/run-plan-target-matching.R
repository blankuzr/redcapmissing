# Matching assessible targets to physical REDCap record rows.

.assessible_target_group_record_rows <- function(data, columns) {
  n <- nrow(data)
  if (!n) return(integer())
  if (!length(columns)) return(rep.int(1L, n))

  values <- unname(as.list(data[, columns, drop = FALSE]))
  ordering <- do.call(order, c(values, list(na.last = TRUE, method = "radix")))
  same_as_previous <- rep.int(TRUE, max(0L, n - 1L))
  if (n > 1L) {
    previous <- ordering[-n]
    current <- ordering[-1L]
    for (value in values) {
      left <- value[previous]
      right <- value[current]
      both_missing <- is.na(left) & is.na(right)
      both_present <- !is.na(left) & !is.na(right)
      equal <- both_missing | (both_present & left == right)
      equal[is.na(equal)] <- FALSE
      same_as_previous <- same_as_previous & equal
    }
  }

  sorted_group <- cumsum(c(TRUE, !same_as_previous))
  group <- integer(n)
  group[ordering] <- sorted_group
  group
}

.assessible_target_match_record_rows <- function(needles, haystack, columns) {
  needle_n <- nrow(needles)
  haystack_n <- nrow(haystack)
  if (!needle_n) return(integer())
  if (!haystack_n) return(rep.int(NA_integer_, needle_n))

  combined <- rbind(
    as.data.frame(haystack[, columns, drop = FALSE]),
    as.data.frame(needles[, columns, drop = FALSE])
  )
  groups <- .assessible_target_group_record_rows(combined, columns)
  match(
    groups[haystack_n + seq_len(needle_n)],
    groups[seq_len(haystack_n)]
  )
}

.assessible_target_join_records <- function(targets, data, longitudinal) {
  target_context <- data.frame(
    record_id = targets$record_id,
    event = targets$redcap_event_name,
    repeat_instrument = targets$repeat_instrument,
    repeat_instance = targets$repeat_instance,
    stringsAsFactors = FALSE
  )
  data_context <- data.frame(
    record_id = data$.rcm_record_id,
    event = data$redcap_event_name,
    repeat_instrument = data$redcap_repeat_instrument,
    repeat_instance = data$redcap_repeat_instance,
    stringsAsFactors = FALSE
  )
  target_row <- .assessible_target_match_record_rows(
    target_context,
    data_context,
    names(target_context)
  )
  event_present <- if (isTRUE(longitudinal)) {
    !is.na(.assessible_target_match_record_rows(
      target_context,
      data_context,
      c("record_id", "event")
    ))
  } else {
    rep.int(TRUE, nrow(targets))
  }
  list(
    target_row = as.integer(target_row),
    target_present = !is.na(target_row),
    event_present = event_present
  )
}
