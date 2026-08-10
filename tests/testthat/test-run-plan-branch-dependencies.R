branch_dependency_metadata <- function() {
  dplyr::bind_rows(
    meta_row("plain", "form_a"),
    meta_row(
      "checkbox_field",
      "form_a",
      field_type = "checkbox",
      choices = "a-b, Alpha | 2, Second"
    ),
    meta_row("trigger", "form_b")
  )
}

test_that("branch dependencies preserve global and per-instrument order", {
  metadata <- branch_dependency_metadata()
  field_dictionary <- .metadata_build_field_dictionary(metadata)
  form_a_logic <-
    "[checkbox_field(2)] = '1' and [plain] = 'yes'"
  form_b_logic <-
    "[baseline_arm_1][trigger] = '1' and [plain] = 'yes'"
  references <- .branching_logic_extract_references(c(
    form_a_logic,
    form_b_logic
  ))
  field_rows <- list(
    form_b = tibble::tibble(branching_logic = c(form_b_logic, "")),
    form_a = tibble::tibble(branching_logic = form_a_logic),
    no_branch = tibble::tibble(
      branching_logic = c(NA_character_, " ", "1 = 1")
    )
  )

  original_populator <- .metadata_populate_field_dictionary_entries
  original_resolver <- .run_plan_resolve_export_fields
  populated_roots <- list()
  resolved_roots <- list()
  testthat::local_mocked_bindings(
    .metadata_populate_field_dictionary_entries = function(
      field_dictionary,
      metadata,
      fields
    ) {
      populated_roots[[length(populated_roots) + 1L]] <<- fields
      original_populator(field_dictionary, metadata, fields)
    },
    .run_plan_resolve_export_fields = function(
      metadata,
      fields,
      field_dictionary = NULL
    ) {
      resolved_roots[[length(resolved_roots) + 1L]] <<- fields
      original_resolver(
        metadata,
        fields,
        field_dictionary = field_dictionary
      )
    },
    .package = "redcapmissing"
  )

  dependencies <- .run_plan_build_branch_dependencies(
    metadata,
    field_rows,
    references,
    field_dictionary = field_dictionary
  )

  expect_identical(populated_roots, list(c(
    "checkbox_field",
    "plain",
    "trigger"
  )))
  expect_identical(resolved_roots, list(c(
    "checkbox_field",
    "plain",
    "trigger"
  )))
  expect_identical(
    dependencies$global_export_fields,
    c("checkbox_field___a_b", "checkbox_field___2", "plain", "trigger")
  )
  expect_identical(
    dependencies$export_fields_by_instrument,
    list(
      form_b = c("trigger", "plain"),
      form_a = c("checkbox_field___a_b", "checkbox_field___2", "plain"),
      no_branch = character()
    )
  )
  expect_identical(
    references$event[references$field == "trigger"],
    "baseline_arm_1"
  )
})

test_that("shared branch logic reuses one resolved dependency vector", {
  metadata <- branch_dependency_metadata()
  field_dictionary <- .metadata_build_field_dictionary(metadata)
  shared_logic <-
    "[checkbox_field(2)] = '1' and [plain] = 'yes'"
  references <- .branching_logic_extract_references(shared_logic)
  instrument_names <- sprintf("form_%03d", seq_len(80L))
  field_rows <- stats::setNames(
    rep(
      list(tibble::tibble(branching_logic = c(shared_logic, shared_logic))),
      length(instrument_names)
    ),
    instrument_names
  )

  original_populator <- .metadata_populate_field_dictionary_entries
  original_resolver <- .run_plan_resolve_export_fields
  population_count <- 0L
  resolver_count <- 0L
  testthat::local_mocked_bindings(
    .metadata_populate_field_dictionary_entries = function(
      field_dictionary,
      metadata,
      fields
    ) {
      population_count <<- population_count + 1L
      original_populator(field_dictionary, metadata, fields)
    },
    .run_plan_resolve_export_fields = function(
      metadata,
      fields,
      field_dictionary = NULL
    ) {
      resolver_count <<- resolver_count + 1L
      original_resolver(
        metadata,
        fields,
        field_dictionary = field_dictionary
      )
    },
    .package = "redcapmissing"
  )

  dependencies <- .run_plan_build_branch_dependencies(
    metadata,
    field_rows,
    references,
    field_dictionary = field_dictionary
  )
  expected <- c(
    "checkbox_field___a_b",
    "checkbox_field___2",
    "plain"
  )

  expect_identical(population_count, 1L)
  expect_identical(resolver_count, 1L)
  expect_named(
    dependencies$export_fields_by_instrument,
    instrument_names
  )
  expect_true(all(vapply(
    dependencies$export_fields_by_instrument,
    identical,
    logical(1),
    expected
  )))
})

test_that("branch dependencies preserve typed empty logic and instruments", {
  metadata <- branch_dependency_metadata()
  field_dictionary <- .metadata_build_field_dictionary(metadata)
  empty_references <- .branching_logic_extract_references(
    c(NA_character_, "", " ")
  )
  field_rows <- list(
    form_a = tibble::tibble(branching_logic = c(NA_character_, "")),
    form_b = tibble::tibble(branching_logic = character())
  )

  empty_logic <- .run_plan_build_branch_dependencies(
    metadata,
    field_rows,
    empty_references,
    field_dictionary = field_dictionary
  )
  expect_identical(
    empty_logic,
    list(
      global_export_fields = character(),
      export_fields_by_instrument = list(
        form_a = character(),
        form_b = character()
      )
    )
  )

  no_instruments <- .run_plan_build_branch_dependencies(
    metadata,
    stats::setNames(vector("list", 0L), character()),
    .branching_logic_extract_references("[plain] = 'yes'"),
    field_dictionary = field_dictionary
  )
  expect_identical(no_instruments$global_export_fields, "plain")
  expect_identical(
    no_instruments$export_fields_by_instrument,
    stats::setNames(vector("list", 0L), character())
  )
})

test_that("branch dependencies preserve unknown-field project conditions", {
  metadata <- branch_dependency_metadata()
  logic <- "[missing_first] = '1' or [missing_second] = '1'"

  expect_error(
    .run_plan_build_branch_dependencies(
      metadata,
      list(form_a = tibble::tibble(branching_logic = logic)),
      .branching_logic_extract_references(logic),
      field_dictionary = .metadata_build_field_dictionary(metadata)
    ),
    paste0(
      "Branching logic references unknown field\\(s\\): ",
      "missing_first, missing_second\\."
    ),
    class = "redcapmissing_error_project"
  )
})
