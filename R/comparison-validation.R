# Validation at the stored-report boundary; no source responses are reevaluated.

.comparison_validate_report <- function(report, argument) {
  tryCatch(
    {
      .report_validate_object(report, argument)
      .comparison_validate_settings(report$settings)
      if (is.null(report$details)) {
        .comparison_signal_error(
          "Regenerate the report with `details = TRUE` before comparison."
        )
      }
      results <- .flex_event_instruments_build_targets(report)
      .comparison_validate_gates(results, report$plan$project$longitudinal)
      targets <- report$plan$assessible_targets
      if (
        !identical(
          as.data.frame(results[, names(targets)]),
          as.data.frame(targets)
        )
      ) {
        .comparison_signal_error(
          "Target results must match the plan's ordered targets and provenance."
        )
      }
      if (
        anyNA(results$fields_assessed) ||
          anyNA(results$fields_failed) ||
          any(
            results$fields_failed < 0L |
              results$fields_assessed < results$fields_failed
          )
      ) {
        .comparison_signal_error(
          "Target field counts must be nonnegative and failed <= assessed."
        )
      }
      empty_details <- .details_build_check_rows(
        targets[FALSE, ],
        results[FALSE, ],
        .field_complete_build_empty_rows()
      )
      .report_validate_component_rows(report$details, empty_details, "details")
      details <- report$details
      keys <- c(.comparison_target_keys(), "validation_check", "field_name")
      .comparison_require_unique(details, keys, "details")
      target_index <- .comparison_match_rows(
        details,
        targets,
        .comparison_target_keys()
      )
      if (anyNA(target_index)) {
        .comparison_signal_error("Details contain a target outside the plan.")
      }
      if (
        !identical(
          details$target_source,
          targets$target_source[target_index]
        ) ||
          !identical(
            details$validation_level,
            .details_resolve_validation_level(details$repeat_instance)
          )
      ) {
        .comparison_signal_error(
          "Detail provenance and validation levels must match their targets."
        )
      }
      valid <- c("passed", "failed", "not applicable", "not reached")
      if (
        any(!details$raw_disposition %in% valid) ||
          any(!details$effective_disposition %in% valid) ||
          any(
            !details$validation_check %in% .registry_list_validation_checks()
          ) ||
          anyNA(details$verification_applied)
      ) {
        .comparison_signal_error(
          "Details contain invalid check dispositions or verification flags."
        )
      }
      field <- !is.na(details$field_name)
      if (
        any(!nzchar(trimws(details$field_name[field]))) ||
          any(trimws(details$field_name[field]) != details$field_name[field])
      ) {
        .comparison_signal_error(
          "Field detail keys must be nonblank, unpadded field names."
        )
      }
      if (
        any(details$validation_check[field] != "field-complete") ||
          any(!details$raw_disposition[field] %in% c("passed", "failed")) ||
          any(!details$branch_satisfied[field] %in% TRUE)
      ) {
        .comparison_signal_error(
          "Field details must represent assessed field-complete outcomes."
        )
      }
      override <- field &
        details$raw_disposition == "failed" &
        details$effective_disposition == "passed"
      if (
        any(details$verification_applied != override) ||
          any(
            details$raw_disposition[!override] !=
              details$effective_disposition[!override]
          )
      ) {
        .comparison_signal_error(
          "Raw and effective details disagree with verification flags."
        )
      }
      expected_targets <- .details_build_check_rows(
        targets,
        results,
        .field_complete_build_empty_rows()
      )
      actual_targets <- details[!field, ]
      .comparison_require_equal_rows(
        actual_targets,
        expected_targets,
        keys,
        names(expected_targets),
        "Target check details"
      )
      field_counts <- tabulate(target_index[field], nbins = nrow(targets))
      failed_counts <- tabulate(
        target_index[field & details$effective_disposition == "failed"],
        nbins = nrow(targets)
      )
      if (
        !identical(as.integer(field_counts), results$fields_assessed) ||
          !identical(as.integer(failed_counts), results$fields_failed)
      ) {
        .comparison_signal_error(
          "Field details do not reconcile with target field counts."
        )
      }
      if (
        any(
          field &
            !results$field_complete[target_index] %in% c("passed", "failed")
        ) ||
          any(
            results$field_complete == "passed" &
              (results$fields_failed != 0L | results$fields_assessed == 0L)
          ) ||
          any(results$field_complete == "failed" & results$fields_failed == 0L)
      ) {
        .comparison_signal_error(
          "Target field dispositions do not reconcile with detailed outcomes."
        )
      }
      .missing_validate_rows(report$missing)
      .comparison_require_unique(report$missing, keys, "missing")
      failures <- details[details$effective_disposition == "failed", ]
      .comparison_require_equal_rows(
        report$missing,
        failures,
        keys,
        keys,
        "Missing rows"
      )
      .summary_validate_rows(report$summary)
      expected_summary <- .summary_build_rows(targets, results)
      .comparison_require_unique(
        report$summary,
        .comparison_context_keys(),
        "summary"
      )
      .comparison_require_equal_rows(
        report$summary,
        expected_summary,
        .comparison_context_keys(),
        names(expected_summary),
        "Validation summary"
      )
      audit <- report$verification
      if (
        !is.list(audit) ||
          !identical(names(audit), names(.verification_build_audit())) ||
          !is.logical(audit$enabled) ||
          length(audit$enabled) != 1L ||
          is.na(audit$enabled) ||
          !is.character(audit$verified_user) ||
          length(audit$verified_user) != 1L ||
          (audit$enabled &&
            (is.na(audit$verified_user) ||
              !nzchar(trimws(audit$verified_user)))) ||
          (!audit$enabled && !is.na(audit$verified_user)) ||
          !identical(audit$overrides_applied, as.integer(sum(override))) ||
          (!audit$enabled && any(override))
      ) {
        .comparison_signal_error(
          "Verification audit does not reconcile with the report details."
        )
      }
      invisible(report)
    },
    error = function(error) {
      .comparison_signal_error(paste0(
        "Invalid `",
        argument,
        "`: ",
        conditionMessage(error)
      ))
    }
  )
}

.comparison_validate_gates <- function(rows, longitudinal) {
  event_keys <- c("record_id", "redcap_event_name")
  event_outcomes <- unique(rows[, c(event_keys, "event_row_started")])
  if (anyDuplicated(event_outcomes[, event_keys])) {
    .comparison_signal_error(
      "Event gates conflict for the same record and event context."
    )
  }
  repeat_keys <- c(event_keys, "repeat_instrument", "repeat_instance")
  repeated <- rows[!is.na(rows$repeat_instance), ]
  repeat_outcomes <- unique(repeated[, c(
    repeat_keys,
    "repeat_instance_row_started"
  )])
  if (anyDuplicated(repeat_outcomes[, repeat_keys])) {
    .comparison_signal_error(
      "Repeat gates conflict for the same physical repeat context."
    )
  }
  event_valid <- if (longitudinal) {
    rows$event_row_started %in% c("passed", "failed")
  } else {
    rows$event_row_started == "not applicable"
  }
  event_blocked <- rows$event_row_started == "failed"
  repeat_valid <- ifelse(
    event_blocked,
    rows$repeat_instance_row_started == "not reached",
    ifelse(
      is.na(rows$repeat_instance),
      rows$repeat_instance_row_started == "not applicable",
      rows$repeat_instance_row_started %in% c("passed", "failed")
    )
  )
  upstream_blocked <- event_blocked |
    rows$repeat_instance_row_started == "failed"
  instrument_valid <- ifelse(
    upstream_blocked,
    rows$instrument_started == "not reached",
    rows$instrument_started %in% c("passed", "failed")
  )
  field_valid <- ifelse(
    rows$instrument_started == "passed",
    rows$field_complete %in% c("passed", "failed", "not applicable"),
    rows$field_complete == "not reached"
  )
  if (any(!event_valid | !repeat_valid | !instrument_valid | !field_valid)) {
    .comparison_signal_error(
      "Target dispositions violate the event, repeat, instrument, and field gate sequence."
    )
  }
}

.comparison_validate_settings <- function(settings) {
  if (is.null(settings)) {
    .comparison_signal_error(
      "Regenerate this legacy report with the current `run_plan()` and `details = TRUE` to retain settings."
    )
  }
  if (
    !is.list(settings) ||
      !identical(
        names(settings),
        c("schema_version", "required_fields", "ignore_fields", "exclude_types")
      ) ||
      !identical(settings$schema_version, 1L)
  ) {
    .comparison_signal_error(
      "Unsupported report settings schema; regenerate the report."
    )
  }
  .run_plan_normalize_logical_argument(
    settings$required_fields,
    "settings$required_fields"
  )
  for (name in c("ignore_fields", "exclude_types")) {
    if (!is.character(settings[[name]])) {
      .comparison_signal_error("Settings selections must be character vectors.")
    }
    .run_plan_normalize_character_argument(
      settings[[name]],
      paste0("settings$", name)
    )
  }
  normalized <- .comparison_normalize_settings(
    settings$required_fields,
    settings$ignore_fields,
    settings$exclude_types
  )
  if (!identical(settings, normalized)) {
    .comparison_signal_error(
      "Report settings must use normalized selection sets."
    )
  }
  invisible(settings)
}

.comparison_require_unique <- function(rows, keys, component) {
  if (anyDuplicated(rows[, keys, drop = FALSE])) {
    .comparison_signal_error(paste0(
      "`",
      component,
      "` contains duplicate keys."
    ))
  }
}

.comparison_require_equal_rows <- function(
  actual,
  expected,
  keys,
  columns,
  component
) {
  index <- .comparison_match_rows(expected, actual, keys)
  same <- nrow(actual) == nrow(expected) && !anyNA(index)
  if (same) {
    same <- all(vapply(
      columns,
      function(column) identical(actual[[column]][index], expected[[column]]),
      logical(1)
    ))
  }
  if (!same) {
    .comparison_signal_error(paste0(
      component,
      " do not reconcile with stored assessment outcomes."
    ))
  }
}

.comparison_validate_object <- function(x) {
  expected <- c(
    "plans",
    "settings",
    "target_results",
    "summary",
    "changes",
    "scope_changes",
    "verification"
  )
  if (
    !inherits(x, "redcapmissing_comparison") ||
      !is.list(x) ||
      !identical(names(x), expected)
  ) {
    .comparison_signal_error(
      "Supply a `redcapmissing_comparison` created by `compare_reports()`."
    )
  }
  if (
    !is.list(x$plans) || !identical(names(x$plans), c("previous", "current"))
  ) {
    .comparison_signal_error(
      "Comparison plans must contain previous and current plans."
    )
  }
  .plan_validate_object(x$plans$previous)
  .plan_validate_object(x$plans$current)
  .comparison_validate_settings(x$settings)
  invisible(x)
}
