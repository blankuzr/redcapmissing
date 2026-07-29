# Construction of the run_plan() summary component.

.summary_build_rows <- function(targets, target_results) {
  empty <- tibble::tibble(
    redcap_event_name = character(), instrument = character(),
    repeat_instrument = character(), repeat_instance = integer(),
    validation_level = character(), validation_check = character(),
    status = character(), reason = character(), assessed = integer(),
    passed = integer(), failed = integer(), pass_rate = double(),
    fail_rate = double()
  )
  if (!nrow(targets)) return(empty)

  context_columns <- c(
    "redcap_event_name", "instrument", "repeat_instrument", "repeat_instance"
  )
  raw_group <- .assessible_target_group_record_rows(targets, context_columns)
  context_group <- match(raw_group, unique(raw_group))
  group_n <- length(unique(context_group))
  first <- match(seq_len(group_n), context_group)
  group_size <- tabulate(context_group, nbins = group_n)

  first_reason <- function(reason) {
    result <- rep.int(NA_character_, group_n)
    available <- !is.na(reason)
    if (!any(available)) return(result)
    position <- match(seq_len(group_n), context_group[available])
    hit <- !is.na(position)
    result[hit] <- reason[available][position[hit]]
    result
  }
  sum_by_context <- function(value) {
    as.integer(rowsum(
      as.integer(value),
      context_group,
      reorder = FALSE
    )[, 1L])
  }
  build_summary <- function(
    check,
    disposition,
    reason,
    assessed = NULL,
    passed = NULL,
    failed = NULL
  ) {
    if (is.null(assessed)) {
      passed <- tabulate(
        context_group[disposition == "passed"],
        nbins = group_n
      )
      failed <- tabulate(
        context_group[disposition == "failed"],
        nbins = group_n
      )
      assessed <- passed + failed
    }
    not_applicable <- tabulate(
      context_group[disposition == "not applicable"],
      nbins = group_n
    ) == group_size
    group_reason <- first_reason(reason)
    group_reason[!not_applicable] <- NA_character_

    tibble::tibble(
      redcap_event_name = targets$redcap_event_name[first],
      instrument = targets$instrument[first],
      repeat_instrument = targets$repeat_instrument[first],
      repeat_instance = targets$repeat_instance[first],
      validation_level = .details_resolve_validation_level(
        targets$repeat_instance[first]
      ),
      validation_check = rep.int(check, group_n),
      status = ifelse(not_applicable, "not applicable", "assessed"),
      reason = group_reason,
      assessed = as.integer(assessed),
      passed = as.integer(passed),
      failed = as.integer(failed),
      pass_rate = ifelse(assessed == 0L, NA_real_, passed / assessed),
      fail_rate = ifelse(assessed == 0L, NA_real_, failed / assessed),
      .context_group = seq_len(group_n)
    )
  }

  event_reason <- ifelse(
    target_results$event_row_started == "not applicable",
    "not applicable for classic project",
    NA_character_
  )
  repeat_reason <- ifelse(
    target_results$repeat_instance_row_started == "not applicable",
    "not a repeating target",
    NA_character_
  )
  field_assessed <- sum_by_context(target_results$fields_assessed)
  field_failed <- sum_by_context(target_results$fields_failed)

  result <- dplyr::bind_rows(
    build_summary(
      "event-row-started",
      target_results$event_row_started,
      event_reason
    ),
    build_summary(
      "repeat-instance-row-started",
      target_results$repeat_instance_row_started,
      repeat_reason
    ),
    build_summary(
      "instrument-started",
      target_results$instrument_started,
      rep.int(NA_character_, nrow(targets))
    ),
    build_summary(
      "field-complete",
      target_results$field_complete,
      target_results$field_applicability_reason,
      assessed = field_assessed,
      passed = field_assessed - field_failed,
      failed = field_failed
    )
  )
  result <- result[order(
    result$.context_group,
    match(
      result$validation_check,
      .registry_list_validation_checks()
    ),
    method = "radix"
  ),
    setdiff(names(result), ".context_group"),
    drop = FALSE
  ]
  row.names(result) <- NULL
  tibble::as_tibble(result)
}
