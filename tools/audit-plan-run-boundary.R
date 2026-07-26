# Static review check for the plan-and-run architecture boundary.
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
      is.symbol(expression[[2L]]) &&
      .audit_function_literal(expression[[3L]])) {
    .audit_add_definition(
      as.character(expression[[2L]]), expression[[3L]], path, expression
    )
    return(invisible(NULL))
  }

  if (identical(call_name, "->") && length(expression) == 3L &&
      .audit_function_literal(expression[[2L]]) &&
      is.symbol(expression[[3L]])) {
    .audit_add_definition(
      as.character(expression[[3L]]), expression[[2L]], path, expression
    )
    return(invisible(NULL))
  }

  if (identical(call_name, "assign") && length(expression) >= 3L &&
      is.character(expression[[2L]]) && length(expression[[2L]]) == 1L &&
      .audit_function_literal(expression[[3L]])) {
    .audit_add_definition(
      expression[[2L]], expression[[3L]], path, expression
    )
  }
  invisible(NULL)
}

for (path in source_files) {
  for (expression in source_expressions[[path]]) {
    .audit_collect_definitions(expression, path)
  }
}

definition_names <- vapply(definitions, `[[`, character(1), "name")
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
  basename(source_files) %in% c("missing-engine.R", "find-missing.R")
]
if (length(legacy_engine_paths)) {
  add_failure(paste0(
    "Former assessment pipeline file restored: ",
    paste(basename(legacy_engine_paths), collapse = ", ")
  ))
}

core_files <- source_files[basename(source_files) %in% c(
  "plan-structure.R",
  "run-plan.R",
  "run-plan-verified.R",
  "missing-branching.R"
)]
core_text <- paste(vapply(
  core_files,
  function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
  character(1)
), collapse = "\n")

retired_core_patterns <- c(
  "\\bignore_ids\\b" = "legacy `ignore_ids` scope translation",
  "form-started" = "retired `form-started` validation code",
  "instance-started" = "retired `instance-started` validation code",
  "event:form" = "retired form-based validation level"
)
for (pattern in names(retired_core_patterns)) {
  if (grepl(pattern, core_text, perl = TRUE)) {
    add_failure(retired_core_patterns[[pattern]])
  }
}

expected_exports <- c("plan_from_data", "plan_explicit", "run_plan")
missing_exports <- setdiff(expected_exports, namespace_exports)
if (length(missing_exports)) {
  add_failure(paste0(
    "Required plan-and-run export missing: ",
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
      "Expected exactly one top-level definition of `", entry,
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

cat("Plan-and-run architecture boundary audit passed.\n")
