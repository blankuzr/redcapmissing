# Execution of the field-complete assessment.

.field_complete_build_field_plan <- function(
  metadata,
  instruments,
  required_fields,
  ignore_fields,
  exclude_types,
  strict_exclude_types = TRUE,
  field_dictionary = NULL
) {
  field_dictionary <- field_dictionary %||% .metadata_build_field_dictionary(metadata)
  selected <- metadata[metadata$form_name %in% instruments, , drop = FALSE]
  unknown_ignore <- setdiff(ignore_fields, unique(as.character(selected$field_name)))
  if (length(unknown_ignore)) {
    .condition_signal_error(paste0(
      "`ignore_fields` contains fields outside instruments represented by ",
      "frozen targets: ",
      paste(unknown_ignore, collapse = ", "), "."), "argument")
  }
  if (isTRUE(required_fields) && length(instruments)) {
    if (!"required_field" %in% names(metadata)) {
      .condition_signal_error(
        "`rcon` metadata must contain `required_field` when `required_fields = TRUE`.",
        "project"
      )
    }
    selected <- selected[
      .schema_require_values(selected$required_field),
      ,
      drop = FALSE
    ]
  }
  unknown_types <- setdiff(exclude_types, unique(as.character(selected$field_type)))
  if (length(unknown_types) && isTRUE(strict_exclude_types)) {
    .condition_signal_error(paste0("`exclude_types` contains types unused by the resolved field policy: ",
      paste(unknown_types, collapse = ", "), "."), "argument")
  }
  selected <- selected[!selected$field_type %in% exclude_types, , drop = FALSE]
  unused_ignore <- setdiff(ignore_fields, as.character(selected$field_name))
  if (length(unused_ignore)) {
    .condition_signal_error(paste0("`ignore_fields` contains fields not used by the resolved field policy: ",
      paste(unused_ignore, collapse = ", "), "."), "argument")
  }
  selected <- selected[!selected$field_name %in% ignore_fields, , drop = FALSE]
  selected$branching_logic <- if ("branching_logic" %in% names(selected))
    as.character(selected$branching_logic) else rep("", nrow(selected))
  selected$field_label <- if ("field_label" %in% names(selected))
    as.character(selected$field_label) else as.character(selected$field_name)
  selected$export_fields <- .run_plan_resolve_export_fields(
    metadata,
    selected$field_name,
    field_dictionary = field_dictionary
  )
  split_rows <- split(selected, selected$form_name)
  result_rows <- result_exports <- stats::setNames(vector("list", length(instruments)), instruments)
  for (instrument in instruments) {
    value <- split_rows[[instrument]]
    if (is.null(value)) value <- selected[0, , drop = FALSE]
    result_rows[[instrument]] <- tibble::as_tibble(value)
    result_exports[[instrument]] <- unique(unlist(value$export_fields, use.names = FALSE))
  }
  list(rows = result_rows, export_fields = result_exports,
       branching_logic = selected$branching_logic)
}

.field_complete_detect_missing_values <- function(x) {
  missing <- is.na(x)
  if (is.character(x) || is.factor(x)) missing <- missing | !nzchar(trimws(as.character(x)))
  if (is.numeric(x)) missing <- missing | is.nan(x)
  missing
}

.field_complete_build_empty_rows <- function() {
  tibble::tibble(
    .target_row = integer(), .field_order = integer(),
    field_name = character(), field_label = character(), field_type = character(),
    branching_logic = character(), branch_satisfied = logical(),
    value_summary = character(), raw_disposition = character(),
    verification_applied = logical(), effective_disposition = character()
  )
}

.field_complete_evaluate_checkbox <- function(
  records,
  row_index,
  fields,
  include_summary
) {
  n <- length(row_index)
  selected <- vapply(fields, function(field) {
    .branching_logic_detect_selected_checkbox(records[[field]][row_index])
  }, logical(n))
  if (is.null(dim(selected))) {
    selected <- matrix(selected, nrow = n, ncol = length(fields))
  }
  passed <- rowSums(selected, na.rm = TRUE) > 0L
  if (!isTRUE(include_summary)) {
    return(list(passed = passed, value_summary = rep.int(NA_character_, n)))
  }

  value_summary <- rep.int("", n)
  for (i in seq_along(fields)) {
    hit <- selected[, i]
    if (!any(hit)) next
    existing <- nzchar(value_summary[hit])
    value_summary[hit] <- ifelse(
      existing,
      paste0(value_summary[hit], ", ", fields[[i]]),
      fields[[i]]
    )
  }
  list(passed = passed, value_summary = value_summary)
}

.field_complete_assess_targets <- function(
  targets,
  target_row,
  instrument_status,
  normalized_data,
  metadata,
  field_plan,
  field_dictionary,
  branch_fields,
  instruments,
  retain_passed_fields
) {
  target_n <- nrow(targets)
  field_status <- rep.int("not reached", target_n)
  fields_assessed <- integer(target_n)
  fields_failed <- integer(target_n)
  field_reason <- rep.int(NA_character_, target_n)
  pieces <- list()
  piece_n <- 0L
  compiled_branches <- new.env(hash = TRUE, parent = emptyenv())
  project <- list(id_col = ".rcm_record_id", system_fields = .record_list_system_fields())

  for (instrument in instruments) {
    active_targets <- which(
      targets$instrument == instrument & instrument_status == "passed"
    )
    if (!length(active_targets)) next

    instrument_fields <- field_plan$rows[[instrument]]
    if (!nrow(instrument_fields)) {
      field_status[active_targets] <- "not applicable"
      field_reason[active_targets] <- "no fields remain after field policy"
      next
    }

    source_rows <- target_row[active_targets]
    if (anyNA(source_rows)) {
      .condition_signal_error(
        "A target that passed instrument start could not be matched to its physical row.",
        "schema"
      )
    }
    needed_columns <- unique(c(
      ".rcm_record_id",
      unname(unlist(project$system_fields, use.names = FALSE)),
      branch_fields,
      unlist(instrument_fields$export_fields, use.names = FALSE)
    ))
    records <- normalized_data[source_rows, needed_columns, drop = FALSE]
    branch_cache <- .branching_logic_build_cache(
      records = records,
      lookup_records = normalized_data,
      project = project
    )

    for (field_index in seq_len(nrow(instrument_fields))) {
      logic <- as.character(instrument_fields$branching_logic[[field_index]])
      branch_satisfied <- tryCatch({
        branch_plan <- NULL
        if (!.schema_detect_blank_value(logic)) {
          if (!exists(logic, envir = compiled_branches, inherits = FALSE)) {
            assign(
              logic,
              .branching_logic_compile_expression(logic),
              envir = compiled_branches
            )
          }
          branch_plan <- get(logic, envir = compiled_branches, inherits = FALSE)
        }
        .branching_logic_evaluate_rows(
          logic = logic,
          branch_plan = branch_plan,
          branch_cache = branch_cache,
          records = records,
          lookup_records = normalized_data,
          meta = metadata,
          choice_map = field_dictionary$choice_map,
          project = project
        )
      }, error = function(error) {
        .condition_signal_error(
          paste0(
            "Could not evaluate REDCap branching logic `",
            logic,
            "`: ",
            conditionMessage(error)
          ),
          "project"
        )
      })
      applicable_rows <- which(branch_satisfied)
      if (!length(applicable_rows)) next

      target_index <- active_targets[applicable_rows]
      export_fields <- instrument_fields$export_fields[[field_index]]
      field_type <- as.character(instrument_fields$field_type[[field_index]])
      if (identical(field_type, "checkbox")) {
        outcome <- .field_complete_evaluate_checkbox(
          records,
          applicable_rows,
          export_fields,
          include_summary = retain_passed_fields
        )
        passed <- outcome$passed
        value_summary <- outcome$value_summary
      } else {
        value <- records[[export_fields[[1L]]]][applicable_rows]
        passed <- !.field_complete_detect_missing_values(value)
        value_summary <- if (isTRUE(retain_passed_fields)) {
          .schema_normalize_character_vector(value)
        } else {
          rep.int(NA_character_, length(value))
        }
      }

      fields_assessed[target_index] <- fields_assessed[target_index] + 1L
      failed_targets <- target_index[!passed]
      fields_failed[failed_targets] <- fields_failed[failed_targets] + 1L

      retain <- if (isTRUE(retain_passed_fields)) {
        rep.int(TRUE, length(passed))
      } else {
        !passed
      }
      if (!any(retain)) next

      raw_disposition <- ifelse(passed[retain], "passed", "failed")
      piece_n <- piece_n + 1L
      pieces[[piece_n]] <- tibble::tibble(
        .target_row = as.integer(target_index[retain]),
        .field_order = rep.int(as.integer(field_index), sum(retain)),
        field_name = rep.int(
          as.character(instrument_fields$field_name[[field_index]]), sum(retain)
        ),
        field_label = rep.int(
          as.character(instrument_fields$field_label[[field_index]]), sum(retain)
        ),
        field_type = rep.int(field_type, sum(retain)),
        branching_logic = rep.int(logic, sum(retain)),
        branch_satisfied = rep.int(TRUE, sum(retain)),
        value_summary = value_summary[retain],
        raw_disposition = raw_disposition,
        verification_applied = rep.int(FALSE, sum(retain)),
        effective_disposition = raw_disposition
      )
    }

    assessed <- fields_assessed[active_targets] > 0L
    assessed_targets <- active_targets[assessed]
    field_status[assessed_targets] <- ifelse(
      fields_failed[assessed_targets] > 0L,
      "failed",
      "passed"
    )
    not_applicable <- active_targets[!assessed]
    field_status[not_applicable] <- "not applicable"
    field_reason[not_applicable] <- "no fields apply after branching logic"
  }

  field_rows <- if (!piece_n) {
    .field_complete_build_empty_rows()
  } else {
    dplyr::bind_rows(pieces)
  }
  if (nrow(field_rows)) {
    field_rows <- field_rows[
      order(field_rows$.target_row, field_rows$.field_order, method = "radix"),
      ,
      drop = FALSE
    ]
    row.names(field_rows) <- NULL
  }

  list(
    field_status = field_status,
    fields_assessed = as.integer(fields_assessed),
    fields_failed = as.integer(fields_failed),
    field_reason = field_reason,
    field_rows = tibble::as_tibble(field_rows)
  )
}

.field_complete_build_public_rows <- function(field_rows, targets) {
  if (!nrow(field_rows)) {
    return(tibble::tibble(
      record_id = character(), instrument = character(),
      redcap_event_name = character(), repeat_instrument = character(),
      repeat_instance = integer(), target_source = character(),
      field_name = character(), field_label = character(), field_type = character(),
      branching_logic = character(), branch_satisfied = logical(),
      value_summary = character(), raw_disposition = character(),
      verification_applied = logical(), effective_disposition = character()
    ))
  }

  target_index <- field_rows$.target_row
  tibble::tibble(
    record_id = targets$record_id[target_index],
    instrument = targets$instrument[target_index],
    redcap_event_name = targets$redcap_event_name[target_index],
    repeat_instrument = targets$repeat_instrument[target_index],
    repeat_instance = targets$repeat_instance[target_index],
    target_source = targets$target_source[target_index],
    field_name = field_rows$field_name,
    field_label = field_rows$field_label,
    field_type = field_rows$field_type,
    branching_logic = field_rows$branching_logic,
    branch_satisfied = field_rows$branch_satisfied,
    value_summary = field_rows$value_summary,
    raw_disposition = field_rows$raw_disposition,
    verification_applied = field_rows$verification_applied,
    effective_disposition = field_rows$effective_disposition
  )
}
