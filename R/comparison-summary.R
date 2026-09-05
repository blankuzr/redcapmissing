# Stratified aggregation and public access to stored comparison outcomes.

#' @rdname get_summary
#' @param population For comparisons, `"full"`, `"shared"`, or both (the
#'   default). Full scope uses each complete plan; shared targets use only
#'   exact target keys present in both plans. See [compare_reports()] for the
#'   comparison summary schema. Filters never recompute denominators.
#' @export
get_summary.redcapmissing_comparison <- function(
  report,
  validation_check = NULL,
  events = NULL,
  instruments = NULL,
  population = c("full", "shared"),
  ...
) {
  if (length(list(...))) {
    .condition_signal_error("Unused arguments in `...`.")
  }
  .comparison_validate_object(report)
  .report_validate_component_rows(
    report$summary,
    .comparison_summary_prototype(),
    "summary"
  )
  population <- .comparison_resolve_population(population)
  rows <- .comparison_filter_rows(
    report$summary,
    report,
    validation_check,
    events,
    instruments
  )
  rows <- rows[rows$population %in% population, ]
  .comparison_attach_labels(rows, report)
}

#' Get record and field changes from an assessment comparison
#'
#' Return failures present in either report, organized by validation context
#' and registry check order. This is the drill-down for [get_summary()].
#' @param comparison A `redcapmissing_comparison` from [compare_reports()].
#' @inheritParams get_summary
#' @param change `NULL` or a nonempty vector of exact transition names from
#'   **Transitions** below. Values within a filter are alternatives; different
#'   filters intersect. Check, event, and instrument filters follow
#'   [get_summary()] rules against the union of both plan scopes.
#' @return A tibble retaining all [get_missing()] columns and their types, plus
#'   character `validation_level`, `target_scope`, `change`, and `reason`.
#'   Each side adds character `previous_raw_disposition` /
#'   `current_raw_disposition` and `previous_effective_disposition` /
#'   `current_effective_disposition`, and logical
#'   `previous_verification_applied` / `current_verification_applied`.
#'   Absent-side outcomes are typed missing. Unassessed fields on present
#'   targets use `"not reached"` for upstream gating and `"not applicable"`
#'   for closed branching. The `redcapmissing_labels` attribute retains labels
#'   for the union of both scopes, including after filtering to zero rows.
#' @section Transitions:
#' * `newly_detected`: currently fails and previously did not fail. `reason`
#'   distinguishes newly assessed checks and verification that no longer applies.
#' * `still_missing`: fails in both reports.
#' * `completed`: previously failed and now passes directly.
#' * `verified`: previously failed and now passes through verification.
#' * `no_longer_assessed`: previously failed and is now blocked by branching or
#'   an upstream gate; this is not completed data entry.
#' * `added_to_scope` / `removed_from_scope`: a failure whose target entered or
#'   left the plan. Scope membership takes precedence over outcome transitions.
#' @examples
#' \dontrun{
#' get_changes(comparison, validation_check = "field-complete")
#' get_changes(comparison, change = c("newly_detected", "added_to_scope"))
#' }
#' @seealso [compare_reports()], [get_summary()], [flexify()]
#' @export
get_changes <- function(
  comparison,
  validation_check = NULL,
  events = NULL,
  instruments = NULL,
  change = NULL
) {
  .comparison_validate_object(comparison)
  .report_validate_component_rows(
    comparison$changes,
    .comparison_changes_prototype(),
    "changes"
  )
  change <- .report_resolve_filter(change, "change", .comparison_list_changes())
  rows <- .comparison_filter_rows(
    comparison$changes,
    comparison,
    validation_check,
    events,
    instruments
  )
  if (!is.null(change)) {
    rows <- rows[rows$change %in% change, ]
  }
  .comparison_attach_labels(rows, comparison)
}

.comparison_summary_prototype <- function() {
  out <- tibble::tibble(population = character())
  for (key in .comparison_context_keys()) {
    out[[key]] <- .summary_build_prototype()[[key]]
  }
  for (side in c("previous", "current")) {
    out[[paste0(side, "_in_scope")]] <- logical()
    for (column in setdiff(
      .summary_list_columns(),
      .comparison_context_keys()
    )) {
      out[[paste0(side, "_", column)]] <- .summary_build_prototype()[[column]]
    }
  }
  out$delta_assessed <- integer()
  out$delta_failed <- integer()
  out$delta_fail_rate <- double()
  for (change in .comparison_list_changes()) {
    out[[change]] <- integer()
  }
  out
}

.comparison_build_summary <- function(previous, current, targets, changes) {
  results <- list()
  keys <- .comparison_context_keys()
  shared <- targets[targets$target_scope == "shared", .comparison_target_keys()]
  for (population in c("full", "shared")) {
    before <- previous$target_results
    after <- current$target_results
    transitions <- changes
    if (population == "shared") {
      before <- dplyr::semi_join(before, shared, by = .comparison_target_keys())
      after <- dplyr::semi_join(after, shared, by = .comparison_target_keys())
      transitions <- transitions[transitions$target_scope == "shared", ]
    }
    paired <- .comparison_pair_rows(
      .summary_build_rows(before, before),
      .summary_build_rows(after, after),
      keys
    )
    for (side in c("previous", "current")) {
      absent <- !paired[[paste0(side, "_in_scope")]]
      for (column in c("assessed", "passed", "failed")) {
        paired[[paste0(side, "_", column)]][absent] <- 0L
      }
    }
    paired$population <- rep(population, nrow(paired))
    paired$delta_assessed <- paired$current_assessed - paired$previous_assessed
    paired$delta_failed <- paired$current_failed - paired$previous_failed
    paired$delta_fail_rate <- paired$current_fail_rate -
      paired$previous_fail_rate
    transition_index <- .comparison_match_rows(transitions, paired, keys)
    for (change in .comparison_list_changes()) {
      paired[[change]] <- as.integer(tabulate(
        transition_index[transitions$change == change],
        nbins = nrow(paired)
      ))
    }
    if (
      any(
        paired$previous_failed !=
          paired$still_missing +
            paired$completed +
            paired$verified +
            paired$no_longer_assessed +
            paired$removed_from_scope
      ) ||
        any(
          paired$current_failed !=
            paired$still_missing + paired$newly_detected + paired$added_to_scope
        )
    ) {
      .comparison_signal_error(
        "Failure transitions do not reconcile within validation strata."
      )
    }
    results[[population]] <- .comparison_order_rows(
      paired[, names(.comparison_summary_prototype())],
      targets
    )
  }
  dplyr::bind_rows(results)
}

.comparison_resolve_population <- function(population) {
  if (is.null(population)) {
    .condition_signal_error("`population` must select full, shared, or both.")
  }
  .report_resolve_filter(
    population,
    "population",
    names(.comparison_population_labels())
  )
}

.comparison_scope_values <- function(comparison, scope) {
  unique(unlist(
    lapply(comparison$plans, function(plan) {
      .report_list_scope_values(list(plan = plan), scope)
    }),
    use.names = FALSE
  ))
}

.comparison_filter_rows <- function(
  rows,
  comparison,
  validation_check,
  events,
  instruments
) {
  filters <- list(
    validation_check = .report_resolve_filter(
      validation_check,
      "validation_check",
      .registry_list_validation_checks()
    ),
    events = .report_resolve_filter(
      events,
      "events",
      .comparison_scope_values(comparison, "events")
    ),
    instruments = .report_resolve_filter(
      instruments,
      "instruments",
      .comparison_scope_values(comparison, "instruments")
    )
  )
  .report_filter_rows(rows, filters)
}

.comparison_attach_labels <- function(rows, comparison) {
  project <- comparison$plans$current$project
  attr(rows, "redcapmissing_labels") <- list(
    events = .report_resolve_labels(
      project$event_labels,
      .comparison_scope_values(comparison, "events")
    ),
    instruments = .report_resolve_labels(
      project$instrument_labels,
      .comparison_scope_values(comparison, "instruments")
    )
  )
  rows
}

.comparison_format_summary_side <- function(rows, side) {
  assessed <- rows[[paste0(side, "_assessed")]]
  failed <- rows[[paste0(side, "_failed")]]
  out <- paste0(failed, "/", assessed)
  out[rows[[paste0(side, "_status")]] %in% "not applicable"] <- "Not applicable"
  out[!rows[[paste0(side, "_in_scope")]]] <- "Out of scope"
  out
}

.comparison_extract_targets <- function(comparison, side, population) {
  rows <- comparison$target_results
  keep <- if (population == "shared") {
    rows$target_scope == "shared"
  } else {
    rows$target_scope != if (side == "previous") "added" else "removed"
  }
  rows <- rows[keep, ]
  prefix <- paste0(side, "_")
  columns <- names(rows)[startsWith(names(rows), prefix)]
  out <- rows[, c(.comparison_target_keys(), columns)]
  names(out) <- sub(paste0("^", prefix), "", names(out))
  out
}
