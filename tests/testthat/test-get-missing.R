get_missing_fixture <- function() {
  missing_rows <- tibble::tibble(
    validation_step = c(
      "status_form/event-row-started",
      "repeat_form/instance-row-started",
      "status_form/form-started",
      "status_form/field-complete"
    ),
    validation_row_id = 1:4,
    record_id = c("r3", "r1", "r2", "r1"),
    redcap_event_name = c(
      "followup_event",
      "baseline_event",
      "baseline_event",
      "baseline_event"
    ),
    redcap_repeat_instrument = c("", "repeat_form", "", ""),
    redcap_repeat_instance = c("", "2", "", ""),
    validation_context = c(
      "event: followup_event",
      "event: baseline_event; repeat: 2",
      "event: baseline_event",
      "event: baseline_event"
    ),
    form = c("status_form", "repeat_form", "status_form", "status_form"),
    validation_level = c(
      "event:form",
      "event:form:instance",
      "event:form",
      "event:form"
    ),
    validation_check = c(
      "event-row-started",
      "instance-row-started",
      "form-started",
      "field-complete"
    ),
    validation_passed = rep(FALSE, 4),
    field_name = c(NA_character_, NA_character_, NA_character_, "status_value"),
    field_label = c(
      NA_character_,
      NA_character_,
      NA_character_,
      "Status value"
    ),
    field_type = c(NA_character_, NA_character_, NA_character_, "text"),
    branching_logic = c(
      NA_character_,
      NA_character_,
      NA_character_,
      "[status_started] = '1'"
    ),
    branch_satisfied = c(NA, NA, NA, TRUE),
    export_fields = c(
      NA_character_,
      NA_character_,
      NA_character_,
      "status_value"
    ),
    url = c(
      NA_character_,
      "https://redcap.example.edu/data-entry/repeat/r1",
      NA_character_,
      "https://redcap.example.edu/data-entry/status/r1"
    )
  )

  structure(
    list(
      summary = tibble::tibble(),
      missing = missing_rows,
      spec = list(
        forms = c("status_form", "repeat_form", "empty_form"),
        events = list(
          status_form = c("baseline_event", "followup_event"),
          repeat_form = "baseline_event",
          empty_form = "unused_event"
        ),
        event_labels = c(
          baseline_event = "Baseline",
          followup_event = "Follow-up",
          unused_event = "Unused"
        ),
        form_labels = c(
          status_form = "Status",
          repeat_form = "Repeat",
          empty_form = "Empty"
        )
      )
    ),
    class = "redcapmissing"
  )
}

get_missing_expected_columns <- function() {
  c(
    "record_id",
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "validation_context",
    "form",
    "validation_check",
    "field_name",
    "field_label",
    "field_type",
    "branching_logic",
    "url"
  )
}

test_that("get_missing exposes its stable raw-context contract", {
  report <- get_missing_fixture()
  original_report <- report

  missing_rows <- get_missing(report)

  expect_true("get_missing" %in% getNamespaceExports("redcapmissing"))
  expect_identical(
    as.list(formals(get_missing)),
    alist(
      report = ,
      validation_check = NULL,
      events = NULL,
      forms = NULL
    )
  )
  expect_identical(class(missing_rows), c("tbl_df", "tbl", "data.frame"))
  expect_identical(names(missing_rows), get_missing_expected_columns())
  expect_identical(
    unname(vapply(missing_rows, typeof, character(1))),
    rep("character", length(get_missing_expected_columns()))
  )
  expect_identical(
    lapply(get_missing_expected_columns(), function(column) {
      missing_rows[[column]]
    }),
    lapply(get_missing_expected_columns(), function(column) {
      report$missing[[column]]
    })
  )
  expect_identical(
    attr(missing_rows, "redcapmissing_labels"),
    list(
      events = report$spec$event_labels,
      forms = report$spec$form_labels
    )
  )
  expect_identical(missing_rows$redcap_repeat_instance, c("", "2", "", ""))
  expect_identical(missing_rows$url, report$missing$url)
  expect_identical(report, original_report)
})

test_that("get_missing preserves field metadata missingness", {
  missing_rows <- get_missing(get_missing_fixture())
  non_field_columns <- c(
    "field_name",
    "field_label",
    "field_type",
    "branching_logic"
  )

  expect_true(all(vapply(
    missing_rows[1:3, non_field_columns],
    function(column) all(is.na(column)),
    logical(1)
  )))
  expect_identical(missing_rows$field_name[[4]], "status_value")
})

test_that("get_missing applies shared filters by intersection without reordering", {
  report <- get_missing_fixture()

  field_rows <- get_missing(report, validation_check = "field-complete")
  expect_identical(field_rows$record_id, "r1")

  selected_rows <- get_missing(
    report,
    validation_check = c("form-started", "event-row-started")
  )
  expect_identical(
    selected_rows$validation_check,
    c("event-row-started", "form-started")
  )

  event_rows <- get_missing(report, events = "followup_event")
  expect_identical(event_rows$record_id, "r3")

  form_rows <- get_missing(report, forms = "status_form")
  expect_identical(form_rows$record_id, c("r3", "r2", "r1"))

  joint_rows <- get_missing(
    report,
    validation_check = "field-complete",
    events = "baseline_event",
    forms = "status_form"
  )
  expect_identical(joint_rows$record_id, "r1")

  duplicate_filter <- get_missing(
    report,
    validation_check = c("field-complete", "field-complete"),
    events = c("baseline_event", "baseline_event"),
    forms = c("status_form", "status_form")
  )
  expect_identical(duplicate_filter, joint_rows)
})

test_that("get_missing accepts configured scopes with no failures", {
  report <- get_missing_fixture()

  for (result in list(
    get_missing(report, events = "unused_event"),
    get_missing(report, forms = "empty_form"),
    get_missing(report, events = "followup_event", forms = "repeat_form")
  )) {
    expect_equal(nrow(result), 0)
    expect_identical(names(result), get_missing_expected_columns())
    expect_identical(
      unname(vapply(result, typeof, character(1))),
      rep("character", length(get_missing_expected_columns()))
    )
    expect_identical(
      attr(result, "redcapmissing_labels"),
      list(
        events = report$spec$event_labels,
        forms = report$spec$form_labels
      )
    )
  }

  no_instance_failure <- report
  no_instance_failure$missing <- no_instance_failure$missing[
    no_instance_failure$missing$validation_check != "instance-row-started",
    ,
    drop = FALSE
  ]
  expect_equal(
    nrow(get_missing(
      no_instance_failure,
      validation_check = "instance-row-started"
    )),
    0
  )
})

test_that("get_missing rejects malformed and unknown filter values", {
  report <- get_missing_fixture()

  for (arg in c("validation_check", "events", "forms")) {
    for (bad_value in list(1, character())) {
      args <- list(report = report)
      args[[arg]] <- bad_value
      expect_error(
        do.call(get_missing, args),
        paste0("`", arg, "` must be `NULL` or a non-empty character vector"),
        fixed = TRUE
      )
    }
    for (bad_value in list(NA_character_, "", " ", c("valid", "\t"))) {
      args <- list(report = report)
      args[[arg]] <- bad_value
      expect_error(
        do.call(get_missing, args),
        paste0("`", arg, "` may not contain `NA` or blank values"),
        fixed = TRUE
      )
    }
  }

  expect_error(
    get_missing(report, validation_check = "FIELD-COMPLETE"),
    "Unknown `validation_check` value(s): `FIELD-COMPLETE`",
    fixed = TRUE
  )
  expect_error(
    get_missing(report, events = "BASELINE_EVENT"),
    "Unknown `events` value(s): `BASELINE_EVENT`",
    fixed = TRUE
  )
  expect_error(
    get_missing(report, forms = "STATUS_FORM"),
    "Unknown `forms` value(s): `STATUS_FORM`",
    fixed = TRUE
  )
})

test_that("get_missing requires the current full report schema", {
  expect_error(
    get_missing(list()),
    "`report` must be a `redcapmissing` object",
    fixed = TRUE
  )

  missing_summary <- structure(
    list(missing = get_missing_fixture()$missing),
    class = "redcapmissing"
  )
  expect_error(
    get_missing(missing_summary),
    "`report` must contain `summary`",
    fixed = TRUE
  )

  missing_component <- structure(
    list(summary = tibble::tibble()),
    class = "redcapmissing"
  )
  expect_error(
    get_missing(missing_component),
    "current missing-row column names and order",
    fixed = TRUE
  )

  missing_column <- get_missing_fixture()
  missing_column$missing$field_type <- NULL
  expect_error(
    get_missing(missing_column),
    "current missing-row column names and order",
    fixed = TRUE
  )

  extra_column <- get_missing_fixture()
  extra_column$missing$extra <- "unexpected"
  expect_error(
    get_missing(extra_column),
    "current missing-row column names and order",
    fixed = TRUE
  )

  reordered_columns <- get_missing_fixture()
  reordered_columns$missing <- reordered_columns$missing[
    rev(names(reordered_columns$missing))
  ]
  expect_error(
    get_missing(reordered_columns),
    "current missing-row column names and order",
    fixed = TRUE
  )

  wrong_type <- get_missing_fixture()
  wrong_type$missing$validation_row_id <- as.numeric(
    wrong_type$missing$validation_row_id
  )
  expect_error(
    get_missing(wrong_type),
    "Expected validation_row_id (`integer`)",
    fixed = TRUE
  )
})

test_that("get_missing preserves a find_missing zero-row schema and labels", {
  records <- tibble::tibble(
    record_id = "r1",
    branch_flag = "0",
    required_note = "entered",
    optional_note = "",
    checkbox_field___1 = "1",
    checkbox_field___2 = "0",
    checkbox_other = "",
    conditional_note = ""
  )
  report <- find_missing(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    forms = "baseline_form"
  )

  missing_rows <- get_missing(report)

  expect_equal(nrow(report$missing), 0)
  expect_equal(nrow(missing_rows), 0)
  expect_identical(names(missing_rows), get_missing_expected_columns())
  expect_true(all(c(
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  ) %in% names(missing_rows)))
  expect_identical(
    attr(missing_rows, "redcapmissing_labels")$events,
    report$spec$event_labels
  )
})

test_that("get_missing uses safe empty label vectors for legacy report objects", {
  report <- get_missing_fixture()
  report$spec <- NULL

  out <- get_missing(report)

  expect_identical(
    attr(out, "redcapmissing_labels"),
    list(
      events = structure(character(), names = character()),
      forms = structure(character(), names = character())
    )
  )
})

test_that("get_missing output supports downstream record and repeat filtering", {
  missing_rows <- get_missing(get_missing_fixture())
  selected <- missing_rows[
    missing_rows$record_id == "r1" &
      missing_rows$redcap_repeat_instance == "2",
    c("record_id", "form", "validation_check", "redcap_repeat_instance"),
    drop = FALSE
  ]

  expect_identical(nrow(selected), 1L)
  expect_identical(selected$record_id, "r1")
  expect_identical(selected$form, "repeat_form")
  expect_identical(selected$validation_check, "instance-row-started")
  expect_true(all(vapply(selected, is.character, logical(1))))
})

test_that("get_missing preserves available and unavailable REDCap URLs", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "status_form", required = "y"),
    meta_row("status_started", "status_form", required = "y"),
    meta_row("status_value", "status_form", required = "y")
  )
  records <- tibble::tibble(
    record_id = "r1",
    status_started = "yes",
    status_value = ""
  )
  available_report <- find_missing(
    data = records,
    rcon = fake_rcon(
      metadata,
      project_information = tibble::tibble(
        project_id = 71,
        is_longitudinal = 0
      ),
      url = "https://redcap.example.edu/api/",
      version = "14.2.0"
    ),
    forms = "status_form"
  )
  unavailable_report <- find_missing(
    data = records,
    rcon = fake_rcon(metadata),
    forms = "status_form"
  )

  available_rows <- get_missing(
    available_report,
    validation_check = "field-complete"
  )
  unavailable_rows <- get_missing(
    unavailable_report,
    validation_check = "field-complete"
  )

  expect_identical(
    available_rows$url,
    available_report$missing$url[
      available_report$missing$validation_check == "field-complete"
    ]
  )
  expect_match(available_rows$url, "DataEntry/index.php", fixed = TRUE)
  expect_true(all(is.na(unavailable_rows$url)))
})
