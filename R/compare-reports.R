#' Compare REDCap assessments within their validation contexts
#'
#' Compare stored outcomes from two [run_plan()] reports. Full-scope results
#' describe each assessment as run; shared-target results restrict both sides
#' to the exact targets present in both plans, without rerunning branching logic.
#'
#' @param previous,current Reports created with `details = TRUE` and versioned
#'   field-selection settings. Project identity, structure fingerprints and
#'   normalized field-selection settings must match. Targets and verification
#'   setups may differ. Older reports must be regenerated before comparison.
#'
#' @return A `redcapmissing_comparison` list with these components:
#' * `plans`: the previous and current assessment plans.
#' * `settings`: common `schema_version`, `required_fields`, `ignore_fields`,
#'   and `exclude_types`. Selection vectors are sorted sets; empty selections
#'   use `character()`.
#' * `target_results`: the union of target keys (`record_id`, `instrument`,
#'   `redcap_event_name`, `repeat_instrument`, `repeat_instance`), `target_scope`
#'   (`"shared"`, `"added"`, `"removed"`), and every remaining target-result
#'   column prefixed with `previous_` or `current_`. Absent-side values are
#'   typed missing. Provenance does not determine target identity.
#' * `summary`: see **Validation summary** below.
#' * `changes`: see [get_changes()].
#' * `scope_changes`: added and removed rows of `target_results`, including
#'   targets with no failures.
#' * `verification`: `previous` and `current` lists with `enabled` and
#'   `verified_user`, and a logical `setup_changed` flag.
#'
#' @section Validation summary:
#' Each row is keyed by `population` (`"full"` or `"shared"`),
#' `redcap_event_name`, `instrument`, `repeat_instrument`, `repeat_instance`,
#' `validation_level`, and `validation_check`. Counts for the first three checks
#' count target outcomes; `field-complete` counts assessed field occurrences,
#' with each checkbox root counted once. Checks retain [registry()] order.
#'
#' `previous_in_scope` and `current_in_scope` are logical flags. Each side has
#' prefixed `status`, `reason`, `assessed`, `passed`, `failed`, `pass_rate`, and
#' `fail_rate`, using [get_summary()] types and definitions. An absent context
#' has zero counts and missing status, reason and rates. Zero assessed counts
#' retain missing rates. `delta_assessed` and `delta_failed` are integer
#' current-minus-previous counts; `delta_fail_rate` is a double difference in
#' proportions, displayed as percentage points by [flexify()].
#'
#' Seven integer columns count `newly_detected`, `still_missing`, `completed`,
#' `verified`, `no_longer_assessed`, `added_to_scope`, and `removed_from_scope`
#' transitions in that same stratum and unit. Shared-target rows have zero
#' scope-transition counts. Previously failed checks reconcile to still
#' missing, completed, verified, no longer assessed, and removed from scope;
#' current failures reconcile to still missing, newly detected, and added to
#' scope. Different checks must not be summed into one issue count.
#'
#' Both populations retain the original assessment rules. Even shared targets
#' can have different field denominators when instrument start or branching
#' changes. Verification setup changes are recorded without assigning causation.
#'
#' @section Ordering and storage:
#' Contexts follow first appearance in the previous plan, then current-only
#' contexts; checks follow registry order. Within each context/check, existing
#' issue order is retained and current-only issues follow. Record identifiers
#' remain character, including leading zeros, and nullable keys match by their
#' typed missing values. Tables carry union-scope `redcapmissing_labels`.
#' Response values, source reports, connections, and raw verification history
#' are not copied into the comparison. Record identifiers and available REDCap
#' links remain sensitive in the same way as the source reports.
#'
#' @section Conditions:
#' Invalid, inconsistent, incompatible, legacy, or nondetailed inputs raise
#' `redcapmissing_error_comparison` with a recovery message.
#'
#' @examples
#' \dontrun{
#' comparison <- compare_reports(previous, current)
#' get_summary(comparison, population = "shared")
#' get_changes(comparison, validation_check = "field-complete")
#' flex_event_instruments(comparison)
#' }
#' @seealso [get_summary()], [get_changes()], [flex_event_instruments()]
#' @export
compare_reports <- function(previous, current) {
  if (missing(previous) || missing(current)) {
    .comparison_signal_error("Supply both `previous` and `current` reports.")
  }
  .comparison_validate_report(previous, "previous")
  .comparison_validate_report(current, "current")
  if (
    !identical(previous$plan$project, current$plan$project) ||
      !identical(
        previous$plan$structure_fingerprint,
        current$plan$structure_fingerprint
      )
  ) {
    .comparison_signal_error(
      "Reports must have the same project identity and structure fingerprint."
    )
  }
  if (!identical(previous$settings, current$settings)) {
    .comparison_signal_error(
      "Reports must use the same normalized field-selection settings."
    )
  }
  targets <- .comparison_pair_rows(
    previous$target_results,
    current$target_results,
    .comparison_target_keys()
  )
  targets$target_scope <- as.character(ifelse(
    !targets$previous_in_scope,
    "added",
    ifelse(!targets$current_in_scope, "removed", "shared")
  ))
  targets <- targets[, c(
    .comparison_target_keys(),
    "target_scope",
    setdiff(
      names(targets),
      c(
        .comparison_target_keys(),
        "target_scope",
        "previous_in_scope",
        "current_in_scope"
      )
    )
  )]
  changes <- .comparison_build_changes(previous, current, targets)
  summary <- .comparison_build_summary(previous, current, targets, changes)
  verification <- lapply(
    list(previous = previous, current = current),
    function(report) {
      report$verification[c("enabled", "verified_user")]
    }
  )
  verification$setup_changed <- !identical(
    verification$previous,
    verification$current
  )
  out <- structure(
    list(
      plans = list(previous = previous$plan, current = current$plan),
      settings = previous$settings,
      target_results = targets,
      summary = summary,
      changes = changes,
      scope_changes = targets[targets$target_scope != "shared", ],
      verification = verification
    ),
    class = c("redcapmissing_comparison", "list")
  )
  for (component in c(
    "target_results",
    "summary",
    "changes",
    "scope_changes"
  )) {
    out[[component]] <- .comparison_attach_labels(out[[component]], out)
  }
  out
}

#' Print a comparison by population and validation context
#' @param x A `redcapmissing_comparison` from [compare_reports()].
#' @param ... Passed to the tibble print method.
#' @return `x`, invisibly.
#' @rdname compare_reports
#' @export
print.redcapmissing_comparison <- function(x, ...) {
  .comparison_validate_object(x)
  cat("<redcapmissing_comparison>\n")
  for (population in c("full", "shared")) {
    cat("\n", .comparison_population_labels()[[population]], "\n", sep = "")
    rows <- get_summary(x, population = population)
    if (!nrow(rows)) {
      cat(
        if (population == "shared") {
          "No shared targets.\n"
        } else {
          "No assessment targets.\n"
        }
      )
      next
    }
    display <- rows[, .comparison_context_keys()]
    display$previous_failed_assessed <- .comparison_format_summary_side(
      rows,
      "previous"
    )
    display$current_failed_assessed <- .comparison_format_summary_side(
      rows,
      "current"
    )
    print(display, ...)
  }
  if (x$verification$setup_changed) {
    cat("\nVerification setup changed; see $verification.\n")
  }
  invisible(x)
}

.comparison_signal_error <- function(message) {
  .condition_signal_error(message, "comparison")
}

.comparison_target_keys <- function() {
  c(
    "record_id",
    "instrument",
    "redcap_event_name",
    "repeat_instrument",
    "repeat_instance"
  )
}

.comparison_context_keys <- function() {
  c(
    "redcap_event_name",
    "instrument",
    "repeat_instrument",
    "repeat_instance",
    "validation_level",
    "validation_check"
  )
}

.comparison_list_changes <- function() {
  c(
    "newly_detected",
    "still_missing",
    "completed",
    "verified",
    "no_longer_assessed",
    "added_to_scope",
    "removed_from_scope"
  )
}

.comparison_population_labels <- function() {
  c(full = "Full scope", shared = "Shared targets")
}

.comparison_normalize_settings <- function(
  required_fields,
  ignore_fields,
  exclude_types
) {
  list(
    schema_version = 1L,
    required_fields = required_fields,
    ignore_fields = sort(unique(as.character(ignore_fields)), method = "radix"),
    exclude_types = sort(unique(as.character(exclude_types)), method = "radix")
  )
}

#' Match nullable structural keys without string concatenation or coercion.
#' @param rows,reference Tables whose key columns have already been validated.
#' @param keys Key columns; `reference` must be unique on these columns.
#' @return Integer reference positions, including typed missing for absent keys.
#' @noRd
.comparison_match_rows <- function(rows, reference, keys) {
  lookup <- reference[, keys, drop = FALSE]
  lookup$.comparison_row <- seq_len(nrow(reference))
  dplyr::left_join(
    rows[, keys, drop = FALSE],
    lookup,
    by = keys,
    na_matches = "na"
  )$.comparison_row
}

.comparison_pair_rows <- function(previous, current, keys) {
  out <- unique(dplyr::bind_rows(previous[, keys], current[, keys]))
  for (side in c("previous", "current")) {
    rows <- if (side == "previous") previous else current
    index <- .comparison_match_rows(out, rows, keys)
    out[[paste0(side, "_in_scope")]] <- !is.na(index)
    for (column in setdiff(names(rows), keys)) {
      out[[paste0(side, "_", column)]] <- rows[[column]][index]
    }
  }
  tibble::as_tibble(out)
}

.comparison_order_rows <- function(rows, targets) {
  context_keys <- .comparison_context_keys()[1:4]
  contexts <- unique(targets[, context_keys])
  context_index <- .comparison_match_rows(rows, contexts, context_keys)
  check_index <- match(
    rows$validation_check,
    .registry_list_validation_checks()
  )
  rows[
    order(context_index, check_index, seq_len(nrow(rows)), method = "radix"),
  ]
}

.comparison_build_changes <- function(previous, current, targets) {
  keys <- c(.comparison_target_keys(), "validation_check", "field_name")
  issues <- unique(dplyr::bind_rows(
    previous$missing[, keys],
    current$missing[, keys]
  ))
  out <- .comparison_changes_prototype()[rep(NA_integer_, nrow(issues)), ]
  for (key in keys) {
    out[[key]] <- issues[[key]]
  }
  for (column in setdiff(.missing_list_columns(), keys)) {
    before <- previous$missing[[column]][.comparison_match_rows(
      issues,
      previous$missing,
      keys
    )]
    after <- current$missing[[column]][.comparison_match_rows(
      issues,
      current$missing,
      keys
    )]
    use_before <- .schema_detect_blank_values(after)
    after[use_before] <- before[use_before]
    out[[column]] <- after
  }
  out$validation_level <- .details_resolve_validation_level(out$repeat_instance)
  # Entry links belong to targets, so a current link from another failing
  # field is also available for an issue that has now passed.
  current_links <- current$missing[
    !.schema_detect_blank_values(current$missing$url),
  ]
  current_links <- current_links[
    !duplicated(current_links[, .comparison_target_keys()]),
  ]
  urls <- current_links$url[.comparison_match_rows(
    out,
    current_links,
    .comparison_target_keys()
  )]
  has_url <- !.schema_detect_blank_values(urls)
  out$url[has_url] <- urls[has_url]
  out$target_scope <- targets$target_scope[.comparison_match_rows(
    out,
    targets,
    .comparison_target_keys()
  )]
  for (side in c("previous", "current")) {
    report <- if (side == "previous") previous else current
    detail_index <- .comparison_match_rows(issues, report$details, keys)
    target_index <- .comparison_match_rows(
      issues,
      report$target_results,
      .comparison_target_keys()
    )
    for (column in c(
      "raw_disposition",
      "effective_disposition",
      "verification_applied"
    )) {
      value <- report$details[[column]][detail_index]
      closed <- !is.na(target_index) & is.na(detail_index)
      if (column == "verification_applied") {
        value[closed] <- FALSE
      } else {
        value[closed] <- ifelse(
          report$target_results$field_complete[target_index[closed]] ==
            "not reached",
          "not reached",
          "not applicable"
        )
      }
      out[[paste0(side, "_", column)]] <- value
    }
  }
  before_failed <- out$previous_effective_disposition %in% "failed"
  after_failed <- out$current_effective_disposition %in% "failed"
  out$change <- as.character(ifelse(
    out$target_scope == "added",
    "added_to_scope",
    ifelse(
      out$target_scope == "removed",
      "removed_from_scope",
      ifelse(
        before_failed & after_failed,
        "still_missing",
        ifelse(
          after_failed,
          "newly_detected",
          ifelse(
            out$current_verification_applied %in% TRUE,
            "verified",
            ifelse(
              out$current_effective_disposition %in% "passed",
              "completed",
              "no_longer_assessed"
            )
          )
        )
      )
    )
  ))
  reasons <- c(
    newly_detected = "check now fails",
    still_missing = "fails in both reports",
    completed = "check now passes directly",
    verified = "verification applied",
    no_longer_assessed = "branching logic not satisfied",
    added_to_scope = "target added to the plan",
    removed_from_scope = "target removed from the plan"
  )
  out$reason <- unname(reasons[out$change])
  newly <- out$change == "newly_detected"
  out$reason[
    newly &
      out$previous_effective_disposition %in% c("not reached", "not applicable")
  ] <- "newly assessed"
  out$reason[
    newly & out$previous_verification_applied %in% TRUE
  ] <- "verification no longer applies"
  out$reason[
    out$change == "no_longer_assessed" &
      out$current_effective_disposition %in% "not reached"
  ] <- "upstream check failed"
  .comparison_order_rows(out, targets)
}

.comparison_changes_prototype <- function() {
  out <- .missing_build_prototype()
  out$validation_level <- character()
  out$target_scope <- character()
  out$change <- character()
  out$reason <- character()
  for (side in c("previous", "current")) {
    out[[paste0(side, "_raw_disposition")]] <- character()
    out[[paste0(side, "_effective_disposition")]] <- character()
    out[[paste0(side, "_verification_applied")]] <- logical()
  }
  out
}
