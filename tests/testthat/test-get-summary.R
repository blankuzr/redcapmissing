get_summary_expected_columns <- function() {
  c(
    "redcap_event_name",
    "form",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "validation_level",
    "validation_check",
    "assessed",
    "passed",
    "failed",
    "pass_rate",
    "fail_rate"
  )
}

get_summary_fixture <- function() {
  summary_rows <- tibble::tibble(
    redcap_event_name = c(
      "baseline_event",
      "followup_event",
      "baseline_event",
      "followup_event",
      "baseline_event",
      "repeat_event"
    ),
    form = c(
      "alpha_form",
      "alpha_form",
      "beta_form",
      "beta_form",
      "alpha_form",
      "alpha_form"
    ),
    redcap_repeat_instrument = c("", "", "", "", "alpha_form", ""),
    redcap_repeat_instance = c("", "", "", "", "2", "1"),
    validation_level = c(
      "event:form",
      "event:form",
      "event:form",
      "event:form",
      "event:form:instance",
      "event:form:instance"
    ),
    validation_check = c(
      "event-row-started",
      "field-complete",
      "form-started",
      "field-complete",
      "instance-row-started",
      "instance-row-started"
    ),
    validation_label = c(
      "event-row-started",
      "field-complete",
      "form-started",
      "field-complete",
      "instance-row-started",
      "instance-row-started"
    ),
    validation_context = c(
      "event: baseline_event",
      "event: followup_event",
      "event: baseline_event",
      "event: followup_event",
      "event: baseline_event; repeat: 2",
      "event: repeat_event; repeat: 1"
    ),
    validation_step = c(
      "alpha_form/event-row-started",
      "alpha_form/field-complete",
      "beta_form/form-started",
      "beta_form/field-complete",
      "alpha_form/instance-row-started",
      "alpha_form/instance-row-started"
    ),
    assessed = c(2L, 4L, 2L, 4L, 2L, 2L),
    passed = c(2L, 3L, 1L, 2L, 1L, 2L),
    failed = c(0L, 1L, 1L, 2L, 1L, 0L),
    pass_rate = c(1, 0.75, 0.5, 0.5, 0.5, 1),
    fail_rate = c(0, 0.25, 0.5, 0.5, 0.5, 0)
  )

  structure(
    list(
      summary = summary_rows,
      spec = list(
        forms = c("alpha_form", "beta_form", "empty_form"),
        events = list(
          alpha_form = c(
            "baseline_event",
            "followup_event",
            "repeat_event",
            "unused_event"
          ),
          beta_form = c("baseline_event", "followup_event"),
          empty_form = "unused_event"
        ),
        event_labels = c(
          baseline_event = "Baseline",
          followup_event = "Follow-up",
          repeat_event = "Repeating visit",
          unused_event = "Unused"
        ),
        form_labels = c(
          alpha_form = "Alpha",
          beta_form = "Beta",
          empty_form = "Empty"
        )
      )
    ),
    class = "redcapmissing"
  )
}

get_summary_baseline_report <- function() {
  records <- tibble::tibble(
    record_id = "r1",
    branch_flag = "0",
    required_note = "",
    optional_note = "",
    checkbox_field___1 = "1",
    checkbox_field___2 = "0",
    checkbox_other = "",
    conditional_note = ""
  )

  find_missing(
    data = records,
    rcon = fake_rcon(baseline_form_meta()),
    forms = "baseline_form"
  )
}

test_that("get_summary exposes a stable typed schema with raw contexts", {
  report <- get_summary_fixture()
  original_report <- report

  summary_rows <- get_summary(report)

  expect_true("get_summary" %in% getNamespaceExports("redcapmissing"))
  expect_identical(
    as.list(formals(get_summary)),
    alist(
      report = ,
      validation_check = NULL,
      events = NULL,
      forms = NULL
    )
  )
  expect_identical(class(summary_rows), c("tbl_df", "tbl", "data.frame"))
  expect_identical(names(summary_rows), get_summary_expected_columns())
  expect_identical(
    unname(vapply(summary_rows, typeof, character(1))),
    c(
      rep("character", 6),
      rep("integer", 3),
      rep("double", 2)
    )
  )
  expect_identical(
    lapply(get_summary_expected_columns(), function(column) {
      summary_rows[[column]]
    }),
    lapply(get_summary_expected_columns(), function(column) {
      report$summary[[column]]
    })
  )
  expect_identical(
    attr(summary_rows, "redcapmissing_labels"),
    list(
      events = report$spec$event_labels,
      forms = report$spec$form_labels
    )
  )
  expect_true(any(summary_rows$redcap_repeat_instrument == "alpha_form"))
  expect_true(any(
    summary_rows$redcap_repeat_instrument == "" &
      summary_rows$redcap_repeat_instance == "1"
  ))
  expect_identical(report, original_report)
})

test_that("get_summary applies shared filters by intersection without reordering", {
  report <- get_summary_fixture()

  check_rows <- get_summary(
    report,
    validation_check = c("form-started", "field-complete")
  )
  expect_identical(check_rows$validation_check, c(
    "field-complete",
    "form-started",
    "field-complete"
  ))

  event_rows <- get_summary(report, events = "baseline_event")
  expect_identical(event_rows$passed, c(2L, 1L, 1L))

  form_rows <- get_summary(report, forms = "beta_form")
  expect_identical(form_rows$failed, c(1L, 2L))

  joint_rows <- get_summary(
    report,
    validation_check = "field-complete",
    events = "followup_event",
    forms = "beta_form"
  )
  expect_identical(joint_rows$failed, 2L)

  duplicate_filter <- get_summary(
    report,
    validation_check = c("field-complete", "field-complete"),
    events = c("followup_event", "followup_event"),
    forms = c("beta_form", "beta_form")
  )
  expect_identical(duplicate_filter, joint_rows)
})

test_that("get_summary accepts configured scopes and intersections with no rows", {
  report <- get_summary_fixture()

  for (result in list(
    get_summary(report, events = "unused_event"),
    get_summary(report, forms = "empty_form"),
    get_summary(
      report,
      validation_check = "field-complete",
      events = "baseline_event",
      forms = "beta_form"
    )
  )) {
    expect_equal(nrow(result), 0)
    expect_identical(names(result), get_summary_expected_columns())
    expect_identical(
      unname(vapply(result, typeof, character(1))),
      c(rep("character", 6), rep("integer", 3), rep("double", 2))
    )
    expect_identical(
      attr(result, "redcapmissing_labels"),
      list(
        events = report$spec$event_labels,
        forms = report$spec$form_labels
      )
    )
  }

  no_form_check <- report
  no_form_check$summary <- no_form_check$summary[
    no_form_check$summary$validation_check != "form-started",
    ,
    drop = FALSE
  ]
  expect_equal(
    nrow(get_summary(no_form_check, validation_check = "form-started")),
    0
  )
})

test_that("get_summary rejects malformed and unknown filter values", {
  report <- get_summary_fixture()

  for (arg in c("validation_check", "events", "forms")) {
    for (bad_value in list(1, character())) {
      args <- list(report = report)
      args[[arg]] <- bad_value
      expect_error(
        do.call(get_summary, args),
        paste0("`", arg, "` must be `NULL` or a non-empty character vector"),
        fixed = TRUE
      )
    }
    for (bad_value in list(NA_character_, "", " ", c("valid", "\t"))) {
      args <- list(report = report)
      args[[arg]] <- bad_value
      expect_error(
        do.call(get_summary, args),
        paste0("`", arg, "` may not contain `NA` or blank values"),
        fixed = TRUE
      )
    }
  }

  expect_error(
    get_summary(report, validation_check = "FIELD-COMPLETE"),
    "Unknown `validation_check` value(s): `FIELD-COMPLETE`",
    fixed = TRUE
  )
  expect_error(
    get_summary(report, events = "BASELINE_EVENT"),
    "Unknown `events` value(s): `BASELINE_EVENT`",
    fixed = TRUE
  )
  expect_error(
    get_summary(report, forms = "ALPHA_FORM"),
    "Unknown `forms` value(s): `ALPHA_FORM`",
    fixed = TRUE
  )
})

test_that("get_summary validates the current summary schema", {
  expect_error(
    get_summary(list()),
    "`report` must be a `redcapmissing` object",
    fixed = TRUE
  )

  no_summary <- structure(list(), class = "redcapmissing")
  expect_error(
    get_summary(no_summary),
    "`report` must contain `summary`",
    fixed = TRUE
  )

  for (broken_report in list(
    {
      x <- get_summary_fixture()
      x$summary$validation_label <- NULL
      x
    },
    {
      x <- get_summary_fixture()
      x$summary$extra <- "unexpected"
      x
    },
    {
      x <- get_summary_fixture()
      x$summary <- x$summary[rev(names(x$summary))]
      x
    }
  )) {
    expect_error(
      get_summary(broken_report),
      "current validation summary column names and order",
      fixed = TRUE
    )
  }

  wrong_type <- get_summary_fixture()
  wrong_type$summary$assessed <- as.numeric(wrong_type$summary$assessed)
  expect_error(
    get_summary(wrong_type),
    "Expected assessed (`integer`)",
    fixed = TRUE
  )
})

test_that("get_summary preserves typed zero rows and optional metadata fallback", {
  report <- get_summary_fixture()
  report$summary <- report$summary[0, , drop = FALSE]

  summary_rows <- get_summary(report)

  expect_equal(nrow(summary_rows), 0)
  expect_identical(names(summary_rows), get_summary_expected_columns())
  expect_identical(
    attr(summary_rows, "redcapmissing_labels"),
    list(
      events = report$spec$event_labels,
      forms = report$spec$form_labels
    )
  )

  legacy_report <- get_summary_fixture()
  legacy_report$spec <- NULL
  legacy_rows <- get_summary(legacy_report)
  expect_identical(
    attr(legacy_rows, "redcapmissing_labels"),
    list(
      events = structure(character(), names = character()),
      forms = structure(character(), names = character())
    )
  )
})

test_that("get_summary preserves non-longitudinal calculations and blank contexts", {
  report <- get_summary_baseline_report()

  summary_rows <- get_summary(report)
  event_summary <- summary_rows[
    summary_rows$validation_check == "event-row-started",
    ,
    drop = FALSE
  ]

  expect_identical(
    lapply(get_summary_expected_columns(), function(column) {
      summary_rows[[column]]
    }),
    lapply(get_summary_expected_columns(), function(column) {
      report$summary[[column]]
    })
  )
  expect_identical(event_summary$assessed, 0L)
  expect_identical(event_summary$passed, 0L)
  expect_identical(event_summary$failed, 0L)
  expect_identical(event_summary$pass_rate, 0)
  expect_identical(event_summary$fail_rate, 0)
  expect_true(all(summary_rows$redcap_event_name == ""))
  expect_true(all(summary_rows$redcap_repeat_instrument == ""))
  expect_true(all(summary_rows$redcap_repeat_instance == ""))
  expect_identical(
    attr(summary_rows, "redcapmissing_labels")$events,
    report$spec$event_labels
  )
})

test_that("get_summary preserves multi-form and multi-event calculations", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "alpha_form", required = "y"),
    meta_row("alpha_value", "alpha_form", required = "y"),
    meta_row("beta_value", "beta_form", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = rep(1, 4),
    unique_event_name = rep(c("event_1", "event_2"), each = 2),
    form = rep(c("alpha_form", "beta_form"), 2)
  )
  events <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("event_1", "event_2"),
    event_name = c("Event one", "Event two")
  )
  records <- tibble::tibble(
    record_id = c("r1", "r1", "r2", "r2"),
    redcap_event_name = rep(c("event_1", "event_2"), 2),
    alpha_value = c("entered", "", "entered", "entered"),
    beta_value = c("", "entered", "entered", "entered")
  )
  report <- find_missing(
    data = records,
    rcon = fake_rcon(metadata, events = events, mapping = mapping),
    forms = c("alpha_form", "beta_form")
  )

  summary_rows <- get_summary(report)

  expect_identical(
    lapply(get_summary_expected_columns(), function(column) {
      summary_rows[[column]]
    }),
    lapply(get_summary_expected_columns(), function(column) {
      report$summary[[column]]
    })
  )
  expect_setequal(unique(summary_rows$form), c("alpha_form", "beta_form"))
  expect_setequal(unique(summary_rows$redcap_event_name), c("event_1", "event_2"))
  expect_equal(
    nrow(get_summary(report, events = "event_1", forms = "alpha_form")),
    sum(
      report$summary$redcap_event_name == "event_1" &
        report$summary$form == "alpha_form"
    )
  )
})

test_that("get_summary preserves repeating-instrument calculations", {
  repeat_meta <- dplyr::bind_rows(
    meta_row("record_id", "screen_form", required = "y"),
    meta_row("repeat_started", "repeat_form", required = "y"),
    meta_row("repeat_value", "repeat_form", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = c(1, 1),
    unique_event_name = c("baseline_event", "baseline_event"),
    form = c("screen_form", "repeat_form")
  )
  repeat_instrument_event <- tibble::tibble(
    event_name = "baseline_event",
    form_name = "repeat_form",
    custom_form_label = ""
  )
  records <- tibble::tibble(
    record_id = c("r1", "r2", "r2"),
    redcap_event_name = rep("baseline_event", 3),
    redcap_repeat_instrument = rep("repeat_form", 3),
    redcap_repeat_instance = c("1", "1", "2"),
    repeat_started = rep("yes", 3),
    repeat_value = c("10", "20", "30")
  )
  report <- find_missing(
    data = records,
    rcon = fake_rcon(
      repeat_meta,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    forms = "repeat_form",
    instances = 2L
  )

  summary_rows <- get_summary(report)

  expect_identical(
    lapply(get_summary_expected_columns(), function(column) {
      summary_rows[[column]]
    }),
    lapply(get_summary_expected_columns(), function(column) {
      report$summary[[column]]
    })
  )
  expect_true(all(summary_rows$redcap_repeat_instrument == "repeat_form"))
  expect_setequal(summary_rows$redcap_repeat_instance, c("1", "2"))
})

test_that("get_summary preserves repeating-event calculations", {
  metadata <- dplyr::bind_rows(
    meta_row("record_id", "visit_form", required = "y"),
    meta_row("visit_started", "visit_form", required = "y"),
    meta_row("visit_value", "visit_form", required = "y")
  )
  mapping <- tibble::tibble(
    arm_num = 1,
    unique_event_name = "repeat_event",
    form = "visit_form"
  )
  repeat_instrument_event <- tibble::tibble(
    event_name = "repeat_event",
    form_name = "",
    custom_form_label = ""
  )
  records <- tibble::tibble(
    record_id = c("r1", "r1"),
    redcap_event_name = c("repeat_event", "repeat_event"),
    redcap_repeat_instrument = c("", ""),
    redcap_repeat_instance = c("1", "2"),
    visit_started = c("yes", "yes"),
    visit_value = c("entered", "")
  )
  report <- find_missing(
    data = records,
    rcon = fake_rcon(
      metadata,
      mapping = mapping,
      repeat_instrument_event = repeat_instrument_event
    ),
    forms = "visit_form",
    instances = 2L
  )

  summary_rows <- get_summary(report)

  expect_identical(
    lapply(get_summary_expected_columns(), function(column) {
      summary_rows[[column]]
    }),
    lapply(get_summary_expected_columns(), function(column) {
      report$summary[[column]]
    })
  )
  expect_true(all(summary_rows$redcap_event_name == "repeat_event"))
  expect_true(all(summary_rows$redcap_repeat_instrument == ""))
  expect_setequal(summary_rows$redcap_repeat_instance, c("1", "2"))
})

test_that("accessor outputs remain directly filterable without type drift", {
  summary_rows <- get_summary(get_summary_fixture())
  selected <- summary_rows[
    summary_rows$redcap_event_name == "baseline_event" &
      summary_rows$redcap_repeat_instance == "2",
    c("form", "validation_check", "assessed"),
    drop = FALSE
  ]

  expect_identical(nrow(selected), 1L)
  expect_identical(selected$form, "alpha_form")
  expect_identical(selected$validation_check, "instance-row-started")
  expect_identical(typeof(selected$assessed), "integer")
})

test_that("legacy tidy and flex APIs are absent", {
  namespace <- asNamespace("redcapmissing")
  exports <- getNamespaceExports("redcapmissing")

  expect_false("tidy" %in% exports)
  expect_false("flex" %in% exports)
  expect_false(exists("tidy.redcapmissing", envir = namespace, inherits = FALSE))
  expect_false(exists("flex", envir = namespace, inherits = FALSE))
  expect_false(exists("flex.redcapmissing", envir = namespace, inherits = FALSE))
  expect_null(utils::getS3method("tidy", "redcapmissing", optional = TRUE))
  expect_null(utils::getS3method("flex", "redcapmissing", optional = TRUE))
})
