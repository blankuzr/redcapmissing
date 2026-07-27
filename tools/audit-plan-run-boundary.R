# Static review of the planning and assessment source boundary.
#
# Run from the package root:
#   Rscript tools/audit-plan-run-boundary.R

if (!file.exists("DESCRIPTION") || !dir.exists("R")) {
  stop("Run this audit from the redcapmissing package root.", call. = FALSE)
}

source_files <- sort(list.files(
  "R",
  pattern = "\\.[Rr]$",
  full.names = TRUE
))
source_expressions <- stats::setNames(lapply(
  source_files,
  function(path) {
    tryCatch(
      parse(file = path, keep.source = TRUE),
      error = function(error) {
        stop(
          "Unable to parse ", path, ": ", conditionMessage(error),
          call. = FALSE
        )
      }
    )
  }
), source_files)
namespace_expressions <- parse(file = "NAMESPACE", keep.source = FALSE)

failures <- character()
add_failure <- function(message) {
  failures <<- c(failures, message)
}

.audit_call_name <- function(expression) {
  if (!is.call(expression)) return("")
  head <- expression[[1L]]
  if (is.symbol(head)) return(as.character(head))
  if (is.call(head) && length(head) == 3L &&
      as.character(head[[1L]]) %in% c("::", ":::")) {
    return(as.character(head[[3L]]))
  }
  ""
}

.audit_function_literal <- function(expression) {
  is.call(expression) && identical(expression[[1L]], as.name("function"))
}

.audit_source_line <- function(expression) {
  reference <- attr(expression, "srcref")
  if (is.null(reference)) return(NA_integer_)
  as.integer(reference[[1L]])
}

definitions <- list()
assignment_names <- character()
.audit_add_definition <- function(name, function_expression, path, expression) {
  definitions[[length(definitions) + 1L]] <<- list(
    name = name,
    function_expression = function_expression,
    path = path,
    line = .audit_source_line(expression)
  )
}

.audit_collect_definitions <- function(expression, path) {
  if (!is.call(expression)) return(invisible(NULL))
  call_name <- .audit_call_name(expression)

  if (call_name %in% c("{", "if")) {
    branches <- if (identical(call_name, "{")) {
      as.list(expression)[-1L]
    } else {
      as.list(expression)[seq.int(3L, length(expression))]
    }
    for (branch in branches) .audit_collect_definitions(branch, path)
    return(invisible(NULL))
  }

  if (call_name %in% c("<-", "=") && length(expression) == 3L &&
      is.symbol(expression[[2L]])) {
    name <- as.character(expression[[2L]])
    assignment_names <<- c(assignment_names, name)
    if (.audit_function_literal(expression[[3L]])) {
      .audit_add_definition(name, expression[[3L]], path, expression)
    }
    return(invisible(NULL))
  }

  if (identical(call_name, "->") && length(expression) == 3L &&
      is.symbol(expression[[3L]])) {
    name <- as.character(expression[[3L]])
    assignment_names <<- c(assignment_names, name)
    if (.audit_function_literal(expression[[2L]])) {
      .audit_add_definition(name, expression[[2L]], path, expression)
    }
    return(invisible(NULL))
  }

  if (identical(call_name, "assign") && length(expression) >= 3L &&
      is.character(expression[[2L]]) && length(expression[[2L]]) == 1L) {
    name <- expression[[2L]]
    assignment_names <<- c(assignment_names, name)
    if (.audit_function_literal(expression[[3L]])) {
      .audit_add_definition(name, expression[[3L]], path, expression)
    }
  }
  invisible(NULL)
}

for (path in source_files) {
  for (expression in source_expressions[[path]]) {
    .audit_collect_definitions(expression, path)
  }
}

definition_names <- vapply(definitions, `[[`, character(1), "name")
assignment_names <- unique(assignment_names)
.audit_definition_locations <- function(index) {
  vapply(index, function(i) {
    line <- definitions[[i]]$line
    paste0(
      basename(definitions[[i]]$path),
      if (!is.na(line)) paste0(":", line) else ""
    )
  }, character(1))
}

.audit_call_heads <- function(expression) {
  found <- character()
  walk <- function(node) {
    if (!is.call(node) && !is.expression(node) && !is.pairlist(node)) {
      return(invisible(NULL))
    }
    if (is.call(node)) {
      call_name <- .audit_call_name(node)
      if (nzchar(call_name)) found <<- c(found, call_name)
    }
    for (i in seq_along(node)) {
      if (!identical(node[[i]], quote(expr = ))) walk(node[[i]])
    }
    invisible(NULL)
  }
  walk(expression)
  unique(found)
}

called_functions <- unique(unlist(lapply(
  unname(source_expressions),
  function(expressions) unlist(lapply(expressions, .audit_call_heads))
), use.names = FALSE))

.audit_namespace_exports <- function(expressions) {
  unlist(lapply(expressions, function(expression) {
    if (!is.call(expression) ||
        !identical(.audit_call_name(expression), "export")) {
      return(character())
    }
    vapply(as.list(expression)[-1L], function(argument) {
      if (is.symbol(argument)) return(as.character(argument))
      if (is.character(argument) && length(argument) == 1L) return(argument)
      ""
    }, character(1))
  }), use.names = FALSE)
}

namespace_exports <- .audit_namespace_exports(namespace_expressions)

retired_entries <- c("find_missing", "flex_event_forms")
retired_definition_indexes <- which(definition_names %in% retired_entries)
if (length(retired_definition_indexes)) {
  add_failure(paste0(
    "Retired entry point definition found: ",
    paste(
      paste0(
        definition_names[retired_definition_indexes], " in ",
        .audit_definition_locations(retired_definition_indexes)
      ),
      collapse = ", "
    )
  ))
}

retired_calls <- intersect(retired_entries, called_functions)
if (length(retired_calls)) {
  add_failure(paste0(
    "Retired callable entry point invoked in package source: ",
    paste(retired_calls, collapse = ", ")
  ))
}

retired_exports <- intersect(retired_entries, namespace_exports)
if (length(retired_exports)) {
  add_failure(paste0(
    "Retired export found: ", paste(retired_exports, collapse = ", ")
  ))
}

retired_pipeline_helpers <- c(
  ".miss_compile_report_plan",
  ".miss_build_form_report",
  ".miss_build_expected_cached",
  ".miss_resolve_forms",
  ".miss_build_report_spec",
  ".miss_build_record_eligibility"
)
retired_helper_definitions <- which(definition_names %in% retired_pipeline_helpers)
if (length(retired_helper_definitions)) {
  add_failure(paste0(
    "Former operational pipeline helper defined: ",
    paste(
      paste0(
        definition_names[retired_helper_definitions], " in ",
        .audit_definition_locations(retired_helper_definitions)
      ),
      collapse = ", "
    )
  ))
}
retired_helper_calls <- intersect(retired_pipeline_helpers, called_functions)
if (length(retired_helper_calls)) {
  add_failure(paste0(
    "Former operational pipeline helper invoked: ",
    paste(retired_helper_calls, collapse = ", ")
  ))
}

legacy_engine_paths <- source_files[
  basename(source_files) %in% c(
    "missing-engine.R",
    "find-missing.R",
    "plan-structure.R",
    "missing-branching.R",
    "redcap-evaluation-helpers.R",
    "redcapmissing-checks.R",
    "reporting-helpers.R",
    "get-missing.R",
    "run-plan-verified.R",
    "run-plan-results.R",
    "run-plan-target-results.R",
    "run-plan-diagnostics.R"
  )
]
if (length(legacy_engine_paths)) {
  add_failure(paste0(
    "Retired or prohibited assessment source file found: ",
    paste(basename(legacy_engine_paths), collapse = ", ")
  ))
}

required_contract_files <- c(
  "assessment-plan.R",
  "conditions.R",
  "internal-operators.R",
  "plan-assessible-targets.R",
  "plan-schedules.R",
  "plan-structure-fingerprint.R",
  "redcap-branching-logic.R",
  "redcap-metadata.R",
  "redcap-project-structure.R",
  "redcap-records.R",
  "report-accessors.R",
  "run-plan.R",
  "run-plan-details.R",
  "run-plan-field-complete.R",
  "run-plan-instrument-started.R",
  "run-plan-missing.R",
  "run-plan-summary.R",
  "run-plan-target-matching.R",
  "run-plan-verification.R",
  "schema-normalization.R"
)
missing_contract_files <- setdiff(
  required_contract_files,
  basename(source_files)
)
if (length(missing_contract_files)) {
  add_failure(paste0(
    "Required cohesive contract file missing: ",
    paste(missing_contract_files, collapse = ", ")
  ))
}

invalid_filenames <- basename(source_files)[
  !grepl("^[a-z0-9]+(?:-[a-z0-9]+)*\\.R$", basename(source_files))
]
if (length(invalid_filenames)) {
  add_failure(paste0(
    "R source filename is not lowercase hyphenated: ",
    paste(invalid_filenames, collapse = ", ")
  ))
}

vague_filenames <- basename(source_files)[
  grepl(
    "(?:^|-)(helpers?|utils?|results?)(?:-|\\.)|^fingerprint\\.R$|^evaluation\\.R$",
    basename(source_files),
    perl = TRUE
  )
]
if (length(vague_filenames)) {
  add_failure(paste0(
    "Vague or prohibited R source filename found: ",
    paste(vague_filenames, collapse = ", ")
  ))
}

core_text <- paste(vapply(
  source_files,
  function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
  character(1)
), collapse = "\n")

retired_core_patterns <- c(
  "\\bignore_ids\\b" = "legacy `ignore_ids` scope translation",
  "form-started" = "retired `form-started` validation code",
  "instance-started" = "retired `instance-started` validation code",
  "event:form" = "retired validation level that uses form"
)
for (pattern in names(retired_core_patterns)) {
  if (grepl(pattern, core_text, perl = TRUE)) {
    add_failure(retired_core_patterns[[pattern]])
  }
}

legacy_internal_assignments <- grep(
  "^\\.(rcm|miss|redcapmissing)_",
  assignment_names,
  value = TRUE
)
if (length(legacy_internal_assignments)) {
  add_failure(paste0(
    "Legacy internal assignment name found: ",
    paste(legacy_internal_assignments, collapse = ", ")
  ))
}

legacy_internal_calls <- grep(
  "^\\.(rcm|miss|redcapmissing)_",
  called_functions,
  value = TRUE
)
if (length(legacy_internal_calls)) {
  add_failure(paste0(
    "Legacy internal helper invoked: ",
    paste(legacy_internal_calls, collapse = ", ")
  ))
}

approved_domains <- c(
  "condition",
  "schema",
  "rcon",
  "project_structure",
  "structure_fingerprint",
  "record",
  "metadata",
  "branching_logic",
  "schedule",
  "assessible_target",
  "plan",
  "run_plan",
  "instrument_started",
  "field_complete",
  "verification",
  "details",
  "summary",
  "missing",
  "report",
  "registry",
  "flex",
  "flexify",
  "flex_event_instruments",
  "startup"
)
approved_domains <- approved_domains[
  order(nchar(approved_domains), decreasing = TRUE)
]
internal_assignment_names <- assignment_names[
  startsWith(assignment_names, ".") &
    !assignment_names %in% c(".onLoad", ".onAttach", ".onDetach", ".onUnload")
]

for (name in internal_assignment_names) {
  domain <- approved_domains[vapply(
    approved_domains,
    function(candidate) startsWith(name, paste0(".", candidate, "_")),
    logical(1)
  )]
  if (!length(domain)) {
    add_failure(paste0(
      "Internal assignment does not use an approved domain: ", name
    ))
    next
  }

  prefix <- paste0(".", domain[[1L]], "_")
  suffix <- substring(name, nchar(prefix) + 1L)
  if (!grepl("^[a-z][a-z0-9]*(?:_[a-z0-9]+)+$", suffix, perl = TRUE)) {
    add_failure(paste0(
      "Internal assignment must use .<domain>_<verb>_<object>: ", name
    ))
  }
}

expected_exports <- c("plan_from_data", "plan_explicit", "run_plan")
missing_exports <- setdiff(expected_exports, namespace_exports)
if (length(missing_exports)) {
  add_failure(paste0(
    "Required planning or assessment export missing: ",
    paste(missing_exports, collapse = ", ")
  ))
}

expected_formals <- list(
  plan_from_data = formals(function(
    data, rcon, instruments, extended_schedule = NULL
  ) NULL),
  plan_explicit = formals(function(
    data, rcon, instruments, explicit_schedule
  ) NULL),
  run_plan = formals(function(
    plan, data, rcon, required_fields = TRUE, ignore_fields = NULL,
    exclude_types = "descriptive", verified = NULL, verified_user = NULL,
    details = FALSE, progress = interactive()
  ) NULL)
)

public_functions <- list()
for (entry in names(expected_formals)) {
  indexes <- which(definition_names == entry)
  if (length(indexes) != 1L) {
    add_failure(paste0(
      "Expected exactly one file scope definition of `", entry,
      "`; found ", length(indexes), "."
    ))
    next
  }
  public_functions[[entry]] <- eval(
    definitions[[indexes]]$function_expression,
    envir = baseenv()
  )
  if (!identical(formals(public_functions[[entry]]), expected_formals[[entry]])) {
    add_failure(paste0("Public signature changed for `", entry, "`."))
  }
}

.audit_ast_identifiers <- function(expression) {
  found <- character()
  walk <- function(node) {
    if (is.symbol(node)) {
      found <<- c(found, as.character(node))
      return(invisible(NULL))
    }
    if (!is.call(node) && !is.expression(node) && !is.pairlist(node)) {
      return(invisible(NULL))
    }
    tags <- names(node)
    if (!is.null(tags)) found <<- c(found, tags[nzchar(tags)])
    for (i in seq_along(node)) {
      if (!identical(node[[i]], quote(expr = ))) walk(node[[i]])
    }
    invisible(NULL)
  }
  walk(expression)
  unique(found)
}

legacy_scope_symbols <- c(
  "forms", "events", "records", "instances", "ignore_ids",
  "find_missing", "flex_event_forms"
)
for (entry in names(public_functions)) {
  identifiers <- .audit_ast_identifiers(body(public_functions[[entry]]))
  legacy_identifiers <- intersect(legacy_scope_symbols, identifiers)
  if (length(legacy_identifiers)) {
    add_failure(paste0(
      "Legacy scope symbol in `", entry, "()` body: ",
      paste(legacy_identifiers, collapse = ", ")
    ))
  }
}

if (length(failures)) {
  stop(paste(failures, collapse = "\n"), call. = FALSE)
}

cat("Planning and assessment source boundary review passed.\n")
