skip_flexify_package <- function() {
  testthat::skip_if_not_installed("flextable")
}

flexify_test_report <- function() {
  records <- tibble::tibble(
    record_id = c("r1", "r2"),
    branch_flag = c("0", "0"),
    required_note = c("", "entered"),
    optional_note = c("", ""),
    checkbox_field___1 = c("1", "0"),
    checkbox_field___2 = c("0", "0"),
    checkbox_other = c("", ""),
    conditional_note = c("", "")
  )

  find_missing(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    forms = "baseline_form"
  )
}

flexify_repeat_report <- function() {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "repeat_form", field_label = "Record ID", required = "y"),
    meta_row("repeat_value", "repeat_form", field_label = "Repeat value", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("regular_event", "repeat_event"),
    form = c("repeat_form", "repeat_form")
  )
  repeat_instrument_event <- tibble::tibble(
    event_name = "repeat_event",
    form_name = "repeat_form",
    custom_form_label = ""
  )
  records <- tibble::tibble(
    record_id = c("r1", "r1"),
    redcap_event_name = c("regular_event", "repeat_event"),
    redcap_repeat_instrument = c("", "repeat_form"),
    redcap_repeat_instance = c("", "1"),
    repeat_value = c("", "")
  )

  find_missing(
    data = records,
    rcon = fake_rcon(
      metadata,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    forms = "repeat_form",
    instances = 1L
  )
}

flexify_render_html <- function(x) {
  testthat::skip_if_not_installed("htmltools")
  rendered <- flextable::htmltools_value(x) |>
    htmltools::renderTags()
  rendered[["html"]]
}

test_that("flexify is a one-argument ordinary function", {
  expect_true("flexify" %in% getNamespaceExports("redcapmissing"))
  expect_identical(as.list(formals(flexify)), alist(x = ))
  expect_false(utils::isS3stdGeneric("flexify"))
})

test_that("flexify accepts complete accessor outputs without changing them", {
  skip_flexify_package()

  report <- flexify_test_report()
  summary_rows <- get_summary(report)
  missing_rows <- get_missing(report)
  original_summary <- summary_rows
  original_missing <- missing_rows

  summary_flex <- flexify(summary_rows)
  missing_flex <- flexify(missing_rows)
  filtered_missing <- missing_rows[
    missing_rows$validation_check == "field-complete",
    c("record_id", "form", "validation_check", "field_name"),
    drop = FALSE
  ]
  filtered_missing_flex <- flexify(filtered_missing)

  expect_s3_class(summary_flex, "flextable")
  expect_s3_class(missing_flex, "flextable")
  expect_identical(
    names(summary_flex$body$dataset),
    setdiff(
      names(summary_rows),
      c("redcap_repeat_instrument", "redcap_repeat_instance")
    )
  )
  expect_identical(
    names(missing_flex$body$dataset),
    setdiff(
      names(missing_rows),
      c("redcap_repeat_instrument", "redcap_repeat_instance")
    )
  )
  expect_identical(
    names(filtered_missing_flex$body$dataset),
    c("record_id", "form", "validation_check", "field_name")
  )
  expect_identical(
    filtered_missing_flex$body$dataset$record_id,
    filtered_missing$record_id
  )
  expect_identical(summary_rows, original_summary)
  expect_identical(missing_rows, original_missing)
})

test_that("flexify accepts subsets, reordered columns, groups, and zero rows", {
  skip_flexify_package()

  summary_rows <- get_summary(flexify_test_report())
  selected <- summary_rows[
    rev(seq_len(nrow(summary_rows))),
    c("passed", "form", "validation_check", "pass_rate"),
    drop = FALSE
  ]
  grouped <- dplyr::group_by(selected, form)
  original_grouped <- grouped

  selected_flex <- flexify(grouped)
  one_column_flex <- flexify(summary_rows[, "passed", drop = FALSE])
  shared_flex <- flexify(
    summary_rows[, c("validation_check", "form"), drop = FALSE]
  )
  single_repeat_flex <- flexify(
    summary_rows[, "redcap_repeat_instrument", drop = FALSE]
  )
  repeat_only_flex <- flexify(summary_rows[, c(
    "redcap_repeat_instance",
    "redcap_repeat_instrument"
  ), drop = FALSE])
  zero_row_flex <- flexify(summary_rows[0, c(
    "form",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  ), drop = FALSE])

  expect_identical(
    names(selected_flex$body$dataset),
    c("passed", "form", "validation_check", "pass_rate")
  )
  expect_identical(selected_flex$body$dataset$passed, selected$passed)
  expect_identical(names(one_column_flex$body$dataset), "passed")
  expect_identical(
    names(shared_flex$body$dataset),
    c("validation_check", "form")
  )
  expect_identical(
    names(single_repeat_flex$body$dataset),
    "redcap_repeat_instrument"
  )
  expect_identical(
    names(repeat_only_flex$body$dataset),
    c("redcap_repeat_instance", "redcap_repeat_instrument")
  )
  expect_identical(names(zero_row_flex$body$dataset), "form")
  expect_equal(nrow(zero_row_flex$body$dataset), 0)
  expect_identical(grouped, original_grouped)
})

test_that("flexify retains repeat columns only for displayed repeat context", {
  skip_flexify_package()

  report <- flexify_repeat_report()
  summary_rows <- get_summary(report)
  missing_rows <- get_missing(report)
  full_flex <- flexify(summary_rows)
  missing_flex <- flexify(missing_rows)
  regular_rows <- summary_rows[
    summary_rows$redcap_event_name == "regular_event",
    ,
    drop = FALSE
  ]
  repeat_rows <- summary_rows[
    summary_rows$redcap_event_name == "repeat_event",
    ,
    drop = FALSE
  ]
  regular_flex <- flexify(regular_rows)
  repeat_flex <- flexify(repeat_rows)

  expect_true(all(c(
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  ) %in% names(full_flex$body$dataset)))
  expect_true(all(c(
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  ) %in% names(missing_flex$body$dataset)))
  expect_false(any(c(
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  ) %in% names(regular_flex$body$dataset)))
  expect_true(all(c(
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  ) %in% names(repeat_flex$body$dataset)))
  expect_true(any(
    repeat_flex$body$dataset$redcap_repeat_instrument == "repeat_form label"
  ))
  expect_true(any(
    missing_flex$body$dataset$redcap_repeat_instrument == "repeat_form label"
  ))
})

test_that("flexify applies metadata labels with raw-value fallback", {
  skip_flexify_package()

  input <- tibble::tibble(
    redcap_event_name = c("event_a", "event_b"),
    form = c("form_a", "form_b"),
    redcap_repeat_instrument = c("form_a", "form_b"),
    redcap_repeat_instance = c("1", "2")
  )
  attr(input, "redcapmissing_labels") <- list(
    events = c(event_a = "Baseline"),
    forms = c(form_a = "Enrollment")
  )
  original <- input

  labeled <- flexify(input)
  expect_identical(input, original)
  attr(input, "redcapmissing_labels") <- NULL
  raw <- flexify(input)

  expect_identical(
    labeled$body$dataset$redcap_event_name,
    c("Baseline", "event_b")
  )
  expect_identical(labeled$body$dataset$form, c("Enrollment", "form_b"))
  expect_identical(
    labeled$body$dataset$redcap_repeat_instrument,
    c("Enrollment", "form_b")
  )
  expect_identical(raw$body$dataset$redcap_event_name, c("event_a", "event_b"))
  expect_identical(raw$body$dataset$form, c("form_a", "form_b"))
  expect_identical(original$redcap_event_name, c("event_a", "event_b"))
  expect_identical(
    attr(original, "redcapmissing_labels", exact = TRUE),
    list(
      events = c(event_a = "Baseline"),
      forms = c(form_a = "Enrollment")
    )
  )
})

test_that("flexify applies headers, registry labels, rates, and blank NA cells", {
  skip_flexify_package()

  input <- tibble::tibble(
    pass_rate = c(0.625, NA_real_),
    validation_check = c("field-complete", "form-started"),
    form = c("form_a", NA_character_),
    assessed = c(1L, NA_integer_)
  )
  out <- flexify(input)
  html <- flexify_render_html(out)

  expect_match(html, "Pass Rate", fixed = TRUE)
  expect_match(html, "Validation Check", fixed = TRUE)
  expect_match(html, "Form", fixed = TRUE)
  expect_match(html, "Assessed", fixed = TRUE)
  expect_identical(
    out$body$dataset$validation_check,
    c("Field complete", "Form started")
  )
  expect_match(html, "62.5%", fixed = TRUE)
  expect_false(grepl(">NA<", html, fixed = TRUE))
})

test_that("flexify renders available URLs as hyperlinks", {
  skip_flexify_package()

  input <- tibble::tibble(
    record_id = c("r1", "r2", "r3"),
    url = c(
      "https://example.test/records/1",
      NA_character_,
      "https://example.test/records/3"
    ),
    field_label = c("Required value", NA_character_, "Follow-up value")
  )
  out <- flexify(input)
  html <- flexify_render_html(out)

  expect_identical(out$body$dataset$url, input$url)
  expect_match(
    html,
    'href="https://example.test/records/1"',
    fixed = TRUE
  )
  expect_match(
    html,
    'href="https://example.test/records/3"',
    fixed = TRUE
  )
  expect_false(grepl(">NA<", html, fixed = TRUE))
})

test_that("flexify rejects unsupported input shapes and schemas", {
  expect_error(flexify(list(form = "a")), "must be a tibble")
  expect_error(flexify(matrix("a")), "must be a tibble")
  expect_error(flexify(data.frame(form = "a")), "must be a tibble")
  expect_error(flexify(tibble::tibble()), "at least one column")

  duplicate_names <- tibble::as_tibble(
    list("form_a", "form_b"),
    .name_repair = "minimal"
  )
  names(duplicate_names) <- c("form", "form")
  expect_error(flexify(duplicate_names), "must be unique")

  blank_name <- tibble::as_tibble(
    setNames(list("form_a"), ""),
    .name_repair = "minimal"
  )
  expect_error(flexify(blank_name), "non-blank name")
  expect_error(
    flexify(tibble::tibble(form = "form_a", added = "value")),
    "unsupported column.*`added`"
  )
  expect_error(
    flexify(tibble::tibble(record_id = "r1", assessed = 1L)),
    "may not combine summary-only and missing-row-only"
  )
})

test_that("flexify rejects changed accessor column types", {
  expect_error(
    flexify(tibble::tibble(passed = 1)),
    "`passed` must be `integer`.*`double`"
  )
  expect_error(
    flexify(tibble::tibble(record_id = 1L)),
    "`record_id` must be `character`.*`integer`"
  )
  expect_error(
    flexify(tibble::tibble(pass_rate = 1L)),
    "`pass_rate` must be `double`.*`integer`"
  )
  expect_error(
    flexify(tibble::tibble(form = factor("form_a"))),
    "`form` must be `character`.*`integer`"
  )
})

test_that("flexify names its optional dependency in errors", {
  testthat::local_mocked_bindings(
    .redcapmissing_check_packages = function(packages, context) {
      stop("missing dependency for `", context, "`", call. = FALSE)
    },
    .package = "redcapmissing"
  )

  expect_error(
    flexify(tibble::tibble(form = "form_a")),
    "missing dependency for `flexify\\(\\)`"
  )
})
