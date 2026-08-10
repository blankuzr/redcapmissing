branching_dictionary_metadata <- function() {
  dplyr::bind_rows(
    meta_row("plain", "form"),
    meta_row("numeric_text", "form", validation = "number"),
    meta_row("calculated", "form", field_type = "calc"),
    meta_row(
      "radio_field",
      "form",
      field_type = "radio",
      choices = "1, One | 2, Two"
    ),
    meta_row("yesno_field", "form", field_type = "yesno"),
    meta_row(
      "checkbox_field",
      "form",
      field_type = "checkbox",
      choices = "a-b, Alpha | 2, Second"
    )
  )
}

test_that("field dictionary lazily populates hashed branching entries", {
  metadata <- branching_dictionary_metadata()
  dictionary <- .metadata_build_field_dictionary(metadata)

  expect_named(
    dictionary,
    c("export_fields", "choice_map", "field_entries")
  )
  expect_true(is.environment(dictionary$field_entries))
  expect_identical(
    ls(dictionary$field_entries, all.names = TRUE),
    character()
  )
  referenced_fields <- c(
    "radio_field", "numeric_text", "checkbox_field"
  )
  .metadata_populate_field_dictionary_entries(
    dictionary,
    metadata,
    c(referenced_fields, "radio_field")
  )
  field_entries <- mget(
    referenced_fields,
    envir = dictionary$field_entries,
    inherits = FALSE
  )
  expect_named(field_entries, referenced_fields)
  expect_identical(
    sort(ls(dictionary$field_entries, all.names = TRUE)),
    sort(referenced_fields)
  )
  for (field_entry in field_entries) {
    expect_named(
      field_entry,
      c("export_fields", "field_type", "numeric_field", "choices")
    )
    expect_named(field_entry$choices, c("code", "label"))
  }
  expect_identical(
    vapply(
      field_entries,
      function(field_entry) field_entry$field_type,
      character(1)
    ),
    stats::setNames(
      as.character(metadata$field_type[match(
        referenced_fields,
        metadata$field_name
      )]),
      referenced_fields
    )
  )
  expect_identical(
    vapply(
      field_entries,
      function(field_entry) field_entry$numeric_field,
      logical(1)
    ),
    stats::setNames(
      c(FALSE, TRUE, FALSE),
      referenced_fields
    )
  )
  expect_identical(
    field_entries$radio_field$choices$code,
    c("1", "2")
  )
  expect_identical(
    field_entries$radio_field$choices$label,
    c("One", "Two")
  )
  expect_identical(
    field_entries$checkbox_field$export_fields,
    c("checkbox_field___a_b", "checkbox_field___2")
  )
  expect_identical(
    dictionary$export_fields$checkbox_field,
    c("checkbox_field___a_b", "checkbox_field___2")
  )

  metadata$text_validation_type_or_show_slider_number <- NULL
  without_validation <- .metadata_build_field_dictionary(metadata)
  without_validation_fields <- c("numeric_text", "calculated")
  .metadata_populate_field_dictionary_entries(
    without_validation,
    metadata,
    without_validation_fields
  )
  without_validation_entries <- mget(
    without_validation_fields,
    envir = without_validation$field_entries,
    inherits = FALSE
  )
  expect_identical(
    vapply(
      without_validation_entries,
      function(field_entry) field_entry$numeric_field,
      logical(1)
    ),
    stats::setNames(
      c(FALSE, TRUE),
      without_validation_fields
    )
  )
})

test_that("branch dependencies populate only referenced root entries", {
  metadata <- branching_dictionary_metadata()
  dictionary <- .metadata_build_field_dictionary(metadata)
  blank_references <- .branching_logic_extract_references(
    c(NA_character_, "", " ")
  )
  blank_rows <- list(
    form = tibble::tibble(branching_logic = c(NA_character_, ""))
  )

  .run_plan_build_branch_dependencies(
    metadata,
    blank_rows,
    blank_references,
    field_dictionary = dictionary
  )
  expect_identical(
    ls(dictionary$field_entries, all.names = TRUE),
    character()
  )

  logic <- paste0(
    "[radio_field] = '1' and ",
    "[numeric_text] > 0 and ",
    "[radio_field] = '2'"
  )
  .run_plan_build_branch_dependencies(
    metadata,
    list(form = tibble::tibble(branching_logic = logic)),
    .branching_logic_extract_references(logic),
    field_dictionary = dictionary
  )
  expect_identical(
    sort(ls(dictionary$field_entries, all.names = TRUE)),
    c("numeric_text", "radio_field")
  )
})

test_that("dictionary-backed branching helpers preserve direct behavior", {
  metadata <- branching_dictionary_metadata()
  dictionary <- .metadata_build_field_dictionary(metadata)
  .metadata_populate_field_dictionary_entries(
    dictionary,
    metadata,
    c("radio_field", "numeric_text")
  )

  for (field in c(as.character(metadata$field_name), "unknown_field")) {
    expect_identical(
      .branching_logic_resolve_field_type(metadata, field),
      .branching_logic_resolve_field_type(
        metadata,
        field,
        field_dictionary = dictionary
      )
    )
    expect_identical(
      .branching_logic_detect_numeric_field(metadata, field),
      .branching_logic_detect_numeric_field(
        metadata,
        field,
        field_dictionary = dictionary
      )
    )
  }

  for (field in list(
    NULL,
    character(),
    "",
    NA_character_,
    c("plain", "numeric_text")
  )) {
    expect_identical(
      .branching_logic_resolve_field_type(metadata, field),
      .branching_logic_resolve_field_type(
        metadata,
        field,
        field_dictionary = dictionary
      )
    )
    expect_identical(
      .branching_logic_detect_numeric_field(metadata, field),
      .branching_logic_detect_numeric_field(
        metadata,
        field,
        field_dictionary = dictionary
      )
    )
  }

  cases <- list(
    list(field = "radio_field", value = c("One", "2", "", NA_character_)),
    list(field = "yesno_field", value = c("Yes", "No", "1", NA_character_)),
    list(field = "numeric_text", value = c("1.5", "", NA_character_)),
    list(field = "plain", value = c("entered", "", NA_character_)),
    list(field = "unknown_field", value = c("entered", "", NA_character_))
  )
  for (case in cases) {
    expect_identical(
      .branching_logic_normalize_value(
        case$value,
        case$field,
        metadata,
        dictionary$choice_map
      ),
      .branching_logic_normalize_value(
        case$value,
        case$field,
        metadata,
        dictionary$choice_map,
        field_dictionary = dictionary
      )
    )
  }
})

test_that("dictionary-backed evaluation preserves branch field semantics", {
  metadata <- branching_dictionary_metadata()
  dictionary <- .metadata_build_field_dictionary(metadata)
  .metadata_populate_field_dictionary_entries(
    dictionary,
    metadata,
    c(
      "radio_field", "numeric_text", "yesno_field", "checkbox_field"
    )
  )
  project <- list(
    id_col = ".rcm_record_id",
    system_fields = .record_list_system_fields()
  )
  records <- tibble::tibble(
    radio_field = c("One", "2"),
    numeric_text = c("6", "4"),
    yesno_field = c("Yes", "1"),
    checkbox_field___a_b = c("0", "1"),
    checkbox_field___2 = c("unchecked", "yes")
  )
  logic <- paste0(
    "[radio_field] = '1' and ",
    "[numeric_text] > 5 and ",
    "[yesno_field] = '1'"
  )

  legacy <- .branching_logic_evaluate_rows(
    logic = logic,
    records = records,
    lookup_records = records,
    meta = metadata,
    choice_map = dictionary$choice_map,
    project = project
  )
  original_resolver <- .branching_logic_resolve_field_entry
  lookup_count <- 0L
  testthat::local_mocked_bindings(
    .branching_logic_resolve_field_entry = function(...) {
      lookup_count <<- lookup_count + 1L
      original_resolver(...)
    },
    .package = "redcapmissing"
  )
  mapped <- .branching_logic_evaluate_rows(
    logic = logic,
    records = records,
    lookup_records = records,
    meta = metadata,
    choice_map = dictionary$choice_map,
    project = project,
    field_dictionary = dictionary
  )
  expect_identical(mapped, legacy)
  expect_identical(mapped, c(TRUE, FALSE))
  expect_identical(lookup_count, 3L)

  legacy_root <- .branching_logic_resolve_value(
    records = records,
    lookup_records = records,
    event = NULL,
    field = "checkbox_field",
    choice = NULL,
    meta = metadata,
    choice_map = dictionary$choice_map,
    project = project
  )
  root_lookup_count <- lookup_count
  mapped_root <- .branching_logic_resolve_value(
    records = records,
    lookup_records = records,
    event = NULL,
    field = "checkbox_field",
    choice = NULL,
    meta = metadata,
    choice_map = dictionary$choice_map,
    project = project,
    field_dictionary = dictionary
  )
  expect_identical(mapped_root, legacy_root)
  expect_identical(mapped_root, c(0L, 1L))
  expect_identical(lookup_count, root_lookup_count + 1L)

  choice_lookup_count <- lookup_count
  mapped_choice <- .branching_logic_resolve_value(
    records = records,
    lookup_records = records,
    event = NULL,
    field = "checkbox_field",
    choice = "a-b",
    meta = metadata,
    choice_map = dictionary$choice_map,
    project = project,
    field_dictionary = dictionary
  )
  expect_identical(mapped_choice, c(0L, 1L))
  expect_identical(lookup_count, choice_lookup_count + 1L)
})
